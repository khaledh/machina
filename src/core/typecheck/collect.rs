//! Pass 1 of the type checker: collect global typing environment.
//!
//! This module scans the resolved module and builds:
//! - nominal type definitions,
//! - callable signatures (functions and methods),
//! - property accessor signatures, and
//! - generic type-parameter environments keyed by definition id.
//!
//! No local/body-level inference happens here. This is intentionally a
//! declaration-only pass so downstream phases can reason over a stable symbol
//! environment.

use std::collections::{HashMap, HashSet};

use crate::core::ast::ParamMode;
use crate::core::ast::{
    Attribute, EnumDefVariant, FunctionSig, MethodBlock, MethodItem, MethodSig, Param,
    StructDefField, TypeDef, TypeDefKind, TypeExpr, TypeExprKind, TypeParam,
};
use crate::core::context::ResolvedContext;
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefTable, ImportedFacts};
use crate::core::typecheck::engine::{
    CollectedCallableSig, CollectedParamSig, CollectedPropertySig, CollectedTraitMethodSig,
    CollectedTraitPropertySig, CollectedTraitSig, TypecheckEngine,
};
use crate::core::typecheck::errors::{TEK, TypeCheckError};
use crate::core::typecheck::imported::{extend_imported_function_sigs, extend_imported_trait_sigs};
use crate::core::typecheck::tc_push_error;
use crate::core::typecheck::type_map::{
    TypeDefLookup, resolve_param_type_expr_with_params, resolve_return_type_expr_with_params,
    resolve_type_expr, resolve_type_expr_with_params,
};
use crate::core::types::{EnumVariant, StructField, TyVarId, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PropertyAccessorKind {
    Get,
    Set,
}

struct ResolvedTypeLookup<'a> {
    context: &'a ResolvedContext,
    imported_facts: &'a ImportedFacts,
}

impl<'a> ResolvedTypeLookup<'a> {
    fn new(context: &'a ResolvedContext, imported_facts: &'a ImportedFacts) -> Self {
        Self {
            context,
            imported_facts,
        }
    }
}

impl TypeDefLookup for ResolvedTypeLookup<'_> {
    fn type_def_by_id(&self, _def_table: &DefTable, def_id: DefId) -> Option<&TypeDef> {
        self.context
            .module
            .type_def_by_id(&self.context.def_table, def_id)
    }

    fn imported_type_by_id(&self, def_id: DefId) -> Option<&Type> {
        self.imported_facts.imported_type(def_id)
    }
}

/// Pass 1: collect global symbols/signatures into the engine environment.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let ctx = engine.context().clone();
    let imported_facts = engine.env().imported_facts.clone();

    let mut type_symbols = HashMap::new();
    let mut type_defs = HashMap::new();
    let mut trait_sigs = HashMap::new();
    let mut trait_impls = HashMap::new();
    let mut func_sigs = HashMap::new();
    let mut method_sigs = HashMap::new();
    let mut property_sigs = HashMap::new();
    let mut generic_envs = HashMap::new();
    let mut property_conflicts = HashSet::new();
    let mut errors = Vec::new();

    // 1) Collect nominal types and type symbols.
    collect_type_defs(
        &ctx,
        &imported_facts,
        &mut type_symbols,
        &mut type_defs,
        &mut generic_envs,
        &mut errors,
    );

    // 2) Collect trait method contracts.
    collect_trait_sigs(&ctx, &imported_facts, &mut trait_sigs, &mut errors);

    // 3) Collect function overloads.
    collect_function_sigs(
        &ctx,
        &imported_facts,
        &mut func_sigs,
        &mut generic_envs,
        &mut errors,
    );

    // 4) Collect method overloads and synthesized property signatures.
    collect_method_sigs(
        &ctx,
        &imported_facts,
        &type_defs,
        &trait_sigs,
        &mut trait_impls,
        &mut method_sigs,
        &mut property_sigs,
        &mut property_conflicts,
        &mut generic_envs,
        &mut errors,
    );

    // Publish the collected environment as the shared immutable phase input.
    let env = engine.env_mut();
    env.type_symbols = type_symbols;
    env.type_defs = type_defs;
    env.trait_sigs = trait_sigs;
    env.trait_impls = trait_impls;
    env.func_sigs = func_sigs;
    env.method_sigs = method_sigs;
    env.property_sigs = property_sigs;
    env.generic_envs = generic_envs;

    if errors.is_empty() {
        Ok(())
    } else {
        engine.state_mut().diags.extend(errors.clone());
        Err(errors)
    }
}

fn collect_type_defs(
    ctx: &ResolvedContext,
    imported_facts: &ImportedFacts,
    type_symbols: &mut HashMap<String, DefId>,
    type_defs: &mut HashMap<String, Type>,
    generic_envs: &mut HashMap<DefId, HashMap<DefId, TyVarId>>,
    errors: &mut Vec<TypeCheckError>,
) {
    let type_lookup = ResolvedTypeLookup::new(ctx, imported_facts);
    for type_def in ctx.module.type_defs() {
        let Some(type_def_id) = ctx.def_table.lookup_node_def_id(type_def.id) else {
            continue;
        };
        type_symbols.insert(type_def.name.clone(), type_def_id);
        record_generic_env(
            type_def_id,
            &ctx.def_table,
            &type_def.type_params,
            generic_envs,
        );

        if !type_def.type_params.is_empty() {
            // Generic type definitions are instantiated in later passes.
            continue;
        }

        let resolved =
            match &type_def.kind {
                TypeDefKind::Alias { aliased_ty } => {
                    resolve_type_expr(&ctx.def_table, &type_lookup, aliased_ty)
                }
                TypeDefKind::Struct { fields } => resolve_struct_type(ctx, &type_lookup, fields)
                    .map(|fields| Type::Struct {
                        name: type_def.name.clone(),
                        type_args: Vec::new(),
                        fields,
                    }),
                TypeDefKind::Enum { variants } => resolve_enum_type(ctx, &type_lookup, variants)
                    .map(|variants| Type::Enum {
                        name: type_def.name.clone(),
                        type_args: Vec::new(),
                        variants,
                    }),
                TypeDefKind::Linear { .. } => {
                    unimplemented!("linear types are not collected yet")
                }
            };

        match resolved {
            Ok(ty) => {
                type_defs.insert(type_def.name.clone(), ty);
            }
            Err(err) => errors.push(err),
        }
    }
}

fn resolve_struct_type(
    ctx: &ResolvedContext,
    type_lookup: &ResolvedTypeLookup<'_>,
    fields: &[StructDefField],
) -> Result<Vec<StructField>, TypeCheckError> {
    let mut out = Vec::with_capacity(fields.len());
    for field in fields {
        let field_ty = resolve_type_expr(&ctx.def_table, type_lookup, &field.ty)?;
        out.push(StructField {
            name: field.name.clone(),
            ty: field_ty,
        });
    }
    Ok(out)
}

fn resolve_enum_type(
    ctx: &ResolvedContext,
    type_lookup: &ResolvedTypeLookup<'_>,
    variants: &[EnumDefVariant],
) -> Result<Vec<EnumVariant>, TypeCheckError> {
    let mut out = Vec::with_capacity(variants.len());
    for variant in variants {
        let payload = variant
            .payload
            .iter()
            .map(|payload_ty| resolve_type_expr(&ctx.def_table, type_lookup, payload_ty))
            .collect::<Result<Vec<_>, _>>()?;
        out.push(EnumVariant {
            name: variant.name.clone(),
            payload,
        });
    }
    Ok(out)
}

fn collect_trait_sigs(
    ctx: &ResolvedContext,
    imported_facts: &ImportedFacts,
    trait_sigs: &mut HashMap<String, CollectedTraitSig>,
    errors: &mut Vec<TypeCheckError>,
) {
    let type_lookup = ResolvedTypeLookup::new(ctx, imported_facts);
    for trait_def in ctx.module.trait_defs() {
        let mut methods = HashMap::new();
        let mut properties = HashMap::new();

        for method in &trait_def.methods {
            let Some(collected) =
                collect_trait_method_sig(ctx, imported_facts, &method.sig, errors)
            else {
                continue;
            };

            if methods.insert(method.sig.name.clone(), collected).is_some() {
                tc_push_error!(
                    errors,
                    method.span,
                    TEK::TraitMethodDuplicate(trait_def.name.clone(), method.sig.name.clone(),)
                );
            }
        }

        for property in &trait_def.properties {
            let ty = match resolve_type_expr(&ctx.def_table, &type_lookup, &property.ty) {
                Ok(ty) => ty,
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            };

            if properties
                .insert(
                    property.name.clone(),
                    CollectedTraitPropertySig {
                        name: property.name.clone(),
                        ty,
                        has_get: property.has_get,
                        has_set: property.has_set,
                        span: property.span,
                    },
                )
                .is_some()
            {
                tc_push_error!(
                    errors,
                    property.span,
                    TEK::TraitPropertyDuplicate(trait_def.name.clone(), property.name.clone(),)
                );
            }
        }

        trait_sigs.insert(
            trait_def.name.clone(),
            CollectedTraitSig {
                def_id: match ctx.def_table.lookup_node_def_id(trait_def.id) {
                    Some(def_id) => def_id,
                    None => continue,
                },
                methods,
                properties,
                span: trait_def.span,
            },
        );
    }
    extend_imported_trait_sigs(ctx, imported_facts, trait_sigs);
}

fn collect_function_sigs(
    ctx: &ResolvedContext,
    imported_facts: &ImportedFacts,
    func_sigs: &mut HashMap<String, Vec<CollectedCallableSig>>,
    generic_envs: &mut HashMap<DefId, HashMap<DefId, TyVarId>>,
    errors: &mut Vec<TypeCheckError>,
) {
    let mut overloads = Vec::new();
    for func_decl in ctx.module.func_decls() {
        if let Some(def_id) = ctx.def_table.lookup_node_def_id(func_decl.id) {
            overloads.push((def_id, func_decl.sig.clone()));
        }
    }
    for func_def in ctx.module.func_defs() {
        if let Some(def_id) = ctx.def_table.lookup_node_def_id(func_def.id) {
            overloads.push((def_id, func_def.sig.clone()));
        }
    }

    for (def_id, sig) in overloads {
        collect_callable_sig(
            ctx,
            imported_facts,
            def_id,
            &sig,
            None,
            None,
            None,
            &[],
            func_sigs.entry(sig.name.clone()).or_default(),
            generic_envs,
            errors,
        );
    }
    extend_imported_function_sigs(ctx, imported_facts, func_sigs);
}

fn collect_method_sigs(
    ctx: &ResolvedContext,
    imported_facts: &ImportedFacts,
    type_defs: &HashMap<String, Type>,
    trait_sigs: &HashMap<String, CollectedTraitSig>,
    trait_impls: &mut HashMap<String, HashSet<String>>,
    method_sigs: &mut HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
    property_sigs: &mut HashMap<String, HashMap<String, CollectedPropertySig>>,
    property_conflicts: &mut HashSet<(String, String)>,
    generic_envs: &mut HashMap<DefId, HashMap<DefId, TyVarId>>,
    errors: &mut Vec<TypeCheckError>,
) {
    let mut seen_trait_impls = HashSet::new();

    for method_block in ctx.module.method_blocks() {
        let mut implemented_trait_methods = HashSet::new();
        let mut implemented_trait_properties = HashSet::new();
        let trait_contract = method_block
            .trait_name
            .as_ref()
            .and_then(|trait_name| trait_sigs.get(trait_name));

        if let Some(trait_name) = &method_block.trait_name {
            let impl_key = (method_block.type_name.clone(), trait_name.clone());
            if !seen_trait_impls.insert(impl_key) {
                tc_push_error!(
                    errors,
                    method_block.span,
                    TEK::TraitImplDuplicate(method_block.type_name.clone(), trait_name.clone(),)
                );
            }
            trait_impls
                .entry(method_block.type_name.clone())
                .or_default()
                .insert(trait_name.clone());
        }

        for method_item in &method_block.method_items {
            let (def_id, sig, attrs, span) = match method_item {
                MethodItem::Decl(method_decl) => (
                    ctx.def_table.lookup_node_def_id(method_decl.id),
                    method_decl.sig.clone(),
                    method_decl.attrs.clone(),
                    method_decl.span,
                ),
                MethodItem::Def(method_def) => (
                    ctx.def_table.lookup_node_def_id(method_def.id),
                    method_def.sig.clone(),
                    method_def.attrs.clone(),
                    method_def.span,
                ),
            };
            let Some(def_id) = def_id else {
                continue;
            };
            let accessor_kind = property_accessor_kind(&attrs);

            let mut collected = Vec::new();
            let receiver_type_params = method_block_type_params(ctx, method_block);
            let self_ty = resolve_method_block_self_type(
                ctx,
                imported_facts,
                method_block,
                &receiver_type_params,
            );
            collect_callable_sig(
                ctx,
                imported_facts,
                def_id,
                &function_sig_from_method(&sig),
                self_ty,
                Some(sig.self_param.mode.clone()),
                method_block.trait_name.clone(),
                &receiver_type_params,
                &mut collected,
                generic_envs,
                errors,
            );
            let Some(collected) = collected.pop() else {
                continue;
            };

            if let (Some(trait_name), Some(contract)) =
                (method_block.trait_name.as_ref(), trait_contract)
            {
                // Trait `prop` items are synthesized into getter/setter methods.
                // Validate those against the trait property contract, not the method contract.
                if accessor_kind.is_none() {
                    validate_trait_method_impl(
                        &method_block.type_name,
                        trait_name,
                        contract,
                        &sig.name,
                        &collected,
                        span,
                        &mut implemented_trait_methods,
                        errors,
                    );
                }
            }

            method_sigs
                .entry(method_block.type_name.clone())
                .or_default()
                .entry(sig.name.clone())
                .or_default()
                .push(collected.clone());

            if let Some(kind) = accessor_kind {
                if method_block.trait_name.is_some() {
                    implemented_trait_properties.insert(sig.name.clone());
                }
                record_property_sig(
                    type_defs,
                    property_sigs,
                    property_conflicts,
                    &method_block.type_name,
                    &sig.name,
                    kind,
                    def_id,
                    &collected.params,
                    &collected.ret_ty,
                    span,
                    method_block.trait_name.is_some(),
                    errors,
                );
            }
        }

        if let (Some(trait_name), Some(contract)) =
            (method_block.trait_name.as_ref(), trait_contract)
        {
            for prop_name in &implemented_trait_properties {
                if !contract.properties.contains_key(prop_name) {
                    tc_push_error!(
                        errors,
                        method_block.span,
                        TEK::TraitPropertyNotInTrait(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            prop_name.clone(),
                        )
                    );
                }
            }

            for required in contract.methods.keys() {
                if !implemented_trait_methods.contains(required) {
                    tc_push_error!(
                        errors,
                        method_block.span,
                        TEK::TraitMethodMissingImpl(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            required.clone(),
                        )
                    );
                }
            }

            for (prop_name, required) in &contract.properties {
                let impl_prop = property_sigs
                    .get(&method_block.type_name)
                    .and_then(|by_name| by_name.get(prop_name));

                let Some(impl_prop) = impl_prop else {
                    tc_push_error!(
                        errors,
                        method_block.span,
                        TEK::TraitPropertyMissingImpl(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            prop_name.clone(),
                        )
                    );
                    continue;
                };

                if impl_prop.ty != required.ty {
                    tc_push_error!(
                        errors,
                        method_block.span,
                        TEK::TraitPropertyTypeMismatch(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            prop_name.clone(),
                            required.ty.clone(),
                            impl_prop.ty.clone(),
                        )
                    );
                }
                if required.has_get && impl_prop.getter.is_none() {
                    tc_push_error!(
                        errors,
                        method_block.span,
                        TEK::TraitPropertyMissingGetter(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            prop_name.clone(),
                        )
                    );
                }
                if required.has_set && impl_prop.setter.is_none() {
                    tc_push_error!(
                        errors,
                        method_block.span,
                        TEK::TraitPropertyMissingSetter(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            prop_name.clone(),
                        )
                    );
                }
            }
        }
    }
}

fn collect_callable_sig(
    ctx: &ResolvedContext,
    imported_facts: &ImportedFacts,
    def_id: DefId,
    sig: &FunctionSig,
    self_ty: Option<Type>,
    self_mode: Option<ParamMode>,
    impl_trait: Option<String>,
    receiver_type_params: &[TypeParam],
    out: &mut Vec<CollectedCallableSig>,
    generic_envs: &mut HashMap<DefId, HashMap<DefId, TyVarId>>,
    errors: &mut Vec<TypeCheckError>,
) {
    let all_type_params = receiver_type_params
        .iter()
        .cloned()
        .chain(sig.type_params.iter().cloned())
        .collect::<Vec<_>>();
    let type_param_map = if all_type_params.is_empty() {
        None
    } else {
        let map = type_param_map(&ctx.def_table, &all_type_params);
        generic_envs.insert(def_id, map.clone());
        Some(map)
    };

    let params = match build_param_sigs(ctx, imported_facts, &sig.params, type_param_map.as_ref()) {
        Ok(params) => params,
        Err(err) => {
            errors.push(err);
            return;
        }
    };

    let type_lookup = ResolvedTypeLookup::new(ctx, imported_facts);
    let ret_ty = match resolve_return_type_expr_with_params(
        &ctx.def_table,
        &type_lookup,
        &sig.ret_ty_expr,
        type_param_map.as_ref(),
    ) {
        Ok(ret_ty) => ret_ty,
        Err(err) => {
            errors.push(err);
            return;
        }
    };

    out.push(CollectedCallableSig {
        def_id,
        self_ty,
        params,
        ret_ty,
        type_param_count: all_type_params.len(),
        type_param_var_names: all_type_params
            .iter()
            .filter_map(|type_param| {
                let var = type_param_map
                    .as_ref()
                    .and_then(|map| {
                        ctx.def_table
                            .lookup_node_def_id(type_param.id)
                            .and_then(|def_id| map.get(&def_id))
                    })
                    .copied()?;
                Some((var.index(), type_param.ident.clone()))
            })
            .collect(),
        type_param_bounds: type_param_bounds(&all_type_params),
        self_mode,
        impl_trait,
    });
}

fn resolve_method_block_self_type(
    ctx: &ResolvedContext,
    imported_facts: &ImportedFacts,
    method_block: &MethodBlock,
    receiver_type_params: &[TypeParam],
) -> Option<Type> {
    let type_lookup = ResolvedTypeLookup::new(ctx, imported_facts);
    let type_param_map = if receiver_type_params.is_empty() {
        None
    } else {
        Some(type_param_map(&ctx.def_table, receiver_type_params))
    };
    let self_ty_expr = TypeExpr {
        id: method_block.id,
        kind: TypeExprKind::Named {
            ident: method_block.type_name.clone(),
            type_args: method_block.type_args.clone(),
        },
        span: method_block.span,
    };
    resolve_type_expr_with_params(
        &ctx.def_table,
        &type_lookup,
        &self_ty_expr,
        type_param_map.as_ref(),
    )
    .ok()
}

fn method_block_type_params(ctx: &ResolvedContext, method_block: &MethodBlock) -> Vec<TypeParam> {
    method_block
        .type_args
        .iter()
        .filter_map(|type_arg| {
            let TypeExprKind::Named { ident, type_args } = &type_arg.kind else {
                return None;
            };
            if !type_args.is_empty() {
                return None;
            }
            let def_id = ctx.def_table.lookup_node_def_id(type_arg.id)?;
            let def = ctx.def_table.lookup_def(def_id)?;
            if def.kind != crate::core::resolve::DefKind::TypeParam {
                return None;
            }
            Some(TypeParam {
                id: type_arg.id,
                ident: ident.clone(),
                bound: None,
                span: type_arg.span,
            })
        })
        .collect()
}

fn collect_trait_method_sig(
    ctx: &ResolvedContext,
    imported_facts: &ImportedFacts,
    sig: &MethodSig,
    errors: &mut Vec<TypeCheckError>,
) -> Option<CollectedTraitMethodSig> {
    let type_param_map = if sig.type_params.is_empty() {
        None
    } else {
        Some(type_param_map(&ctx.def_table, &sig.type_params))
    };

    let params = match build_param_sigs(ctx, imported_facts, &sig.params, type_param_map.as_ref()) {
        Ok(params) => params,
        Err(err) => {
            errors.push(err);
            return None;
        }
    };

    let type_lookup = ResolvedTypeLookup::new(ctx, imported_facts);
    let ret_ty = match resolve_return_type_expr_with_params(
        &ctx.def_table,
        &type_lookup,
        &sig.ret_ty_expr,
        type_param_map.as_ref(),
    ) {
        Ok(ret_ty) => ret_ty,
        Err(err) => {
            errors.push(err);
            return None;
        }
    };

    Some(CollectedTraitMethodSig {
        name: sig.name.clone(),
        params,
        ret_ty,
        type_param_count: sig.type_params.len(),
        type_param_bounds: type_param_bounds(&sig.type_params),
        self_mode: sig.self_param.mode.clone(),
        span: sig.span,
    })
}

fn validate_trait_method_impl(
    type_name: &str,
    trait_name: &str,
    contract: &CollectedTraitSig,
    method_name: &str,
    method: &CollectedCallableSig,
    span: Span,
    seen_methods: &mut HashSet<String>,
    errors: &mut Vec<TypeCheckError>,
) {
    let Some(expected) = contract.methods.get(method_name) else {
        tc_push_error!(
            errors,
            span,
            TEK::TraitMethodNotInTrait(
                type_name.to_string(),
                trait_name.to_string(),
                method_name.to_string(),
            )
        );
        return;
    };

    if !seen_methods.insert(method_name.to_string()) {
        tc_push_error!(
            errors,
            span,
            TEK::TraitMethodImplDuplicate(
                type_name.to_string(),
                trait_name.to_string(),
                method_name.to_string(),
            )
        );
        return;
    }

    let matches = method.self_mode == Some(expected.self_mode.clone())
        && method.type_param_count == expected.type_param_count
        && method.type_param_bounds == expected.type_param_bounds
        && method.params.len() == expected.params.len()
        && method.ret_ty == expected.ret_ty
        && method
            .params
            .iter()
            .zip(expected.params.iter())
            .all(|(actual, expected)| actual.mode == expected.mode && actual.ty == expected.ty);

    if !matches {
        tc_push_error!(
            errors,
            span,
            TEK::TraitMethodSignatureMismatch(
                type_name.to_string(),
                trait_name.to_string(),
                method_name.to_string(),
            )
        );
    }
}

fn build_param_sigs(
    ctx: &ResolvedContext,
    imported_facts: &ImportedFacts,
    params: &[Param],
    type_params: Option<&HashMap<DefId, TyVarId>>,
) -> Result<Vec<CollectedParamSig>, TypeCheckError> {
    let type_lookup = ResolvedTypeLookup::new(ctx, imported_facts);
    let mut out = Vec::with_capacity(params.len());
    for param in params {
        let ty = resolve_param_type_expr_with_params(
            &ctx.def_table,
            &type_lookup,
            &param.typ,
            type_params,
        )?;
        let name = ctx
            .def_table
            .lookup_node_def_id(param.id)
            .and_then(|def_id| ctx.def_table.lookup_def(def_id))
            .map(|def| def.name.clone())
            .unwrap_or_else(|| param.ident.clone());
        out.push(CollectedParamSig {
            name,
            ty,
            mode: param.mode.clone(),
        });
    }
    Ok(out)
}

fn record_generic_env(
    owner: DefId,
    def_table: &DefTable,
    type_params: &[TypeParam],
    generic_envs: &mut HashMap<DefId, HashMap<DefId, TyVarId>>,
) {
    if type_params.is_empty() {
        return;
    }
    generic_envs.insert(owner, type_param_map(def_table, type_params));
}

fn type_param_map(def_table: &DefTable, type_params: &[TypeParam]) -> HashMap<DefId, TyVarId> {
    type_params
        .iter()
        .enumerate()
        .filter_map(|(index, param)| {
            def_table
                .lookup_node_def_id(param.id)
                .map(|def_id| (def_id, TyVarId::new(index as u32)))
        })
        .collect()
}

fn type_param_bounds(type_params: &[TypeParam]) -> Vec<Option<String>> {
    type_params
        .iter()
        .map(|param| param.bound.as_ref().map(|bound| bound.name.clone()))
        .collect()
}

fn property_accessor_kind(attrs: &[Attribute]) -> Option<PropertyAccessorKind> {
    attrs.iter().find_map(|attr| match attr.name.as_str() {
        "__property_get" => Some(PropertyAccessorKind::Get),
        "__property_set" => Some(PropertyAccessorKind::Set),
        _ => None,
    })
}

fn record_property_sig(
    type_defs: &HashMap<String, Type>,
    property_sigs: &mut HashMap<String, HashMap<String, CollectedPropertySig>>,
    property_conflicts: &mut HashSet<(String, String)>,
    type_name: &str,
    prop_name: &str,
    kind: PropertyAccessorKind,
    def_id: DefId,
    params: &[CollectedParamSig],
    ret_ty: &Type,
    span: Span,
    allow_field_overlap: bool,
    errors: &mut Vec<TypeCheckError>,
) {
    let prop_ty = match kind {
        PropertyAccessorKind::Get => {
            if !params.is_empty() {
                tc_push_error!(
                    errors,
                    span,
                    TEK::PropertyGetterHasParams(prop_name.to_string())
                );
                return;
            }
            ret_ty.clone()
        }
        PropertyAccessorKind::Set => {
            if params.len() != 1 {
                tc_push_error!(
                    errors,
                    span,
                    TEK::PropertySetterParamCount(prop_name.to_string(), params.len(),)
                );
                return;
            }
            if *ret_ty != Type::Unit {
                tc_push_error!(
                    errors,
                    span,
                    TEK::PropertySetterReturnType(prop_name.to_string(), ret_ty.clone(),)
                );
                return;
            }
            params[0].ty.clone()
        }
    };

    if !allow_field_overlap
        && !property_sigs
            .get(type_name)
            .is_some_and(|props| props.contains_key(prop_name))
        && let Some(Type::Struct { fields, .. }) = type_defs.get(type_name)
        && let Some(field) = fields.iter().find(|field| field.name == prop_name)
    {
        if property_conflicts.insert((type_name.to_string(), prop_name.to_string())) {
            tc_push_error!(
                errors,
                span,
                TEK::PropertyConflictsWithField(prop_name.to_string(), field.name.clone(),)
            );
        }
        return;
    }

    let props = property_sigs.entry(type_name.to_string()).or_default();
    let entry = props
        .entry(prop_name.to_string())
        .or_insert_with(|| CollectedPropertySig {
            ty: prop_ty.clone(),
            getter: None,
            setter: None,
        });

    if entry.ty != prop_ty {
        tc_push_error!(
            errors,
            span,
            TEK::PropertyAccessorTypeMismatch(prop_name.to_string(), entry.ty.clone(), prop_ty,)
        );
        return;
    }

    match kind {
        PropertyAccessorKind::Get => {
            if entry.getter.is_some() {
                tc_push_error!(
                    errors,
                    span,
                    TEK::PropertyAccessorDuplicate(prop_name.to_string())
                );
            } else {
                entry.getter = Some(def_id);
            }
        }
        PropertyAccessorKind::Set => {
            if entry.setter.is_some() {
                tc_push_error!(
                    errors,
                    span,
                    TEK::PropertyAccessorDuplicate(prop_name.to_string())
                );
            } else {
                entry.setter = Some(def_id);
            }
        }
    }
}

fn function_sig_from_method(sig: &MethodSig) -> FunctionSig {
    FunctionSig {
        name: sig.name.clone(),
        type_params: sig.type_params.clone(),
        params: sig.params.clone(),
        ret_ty_expr: sig.ret_ty_expr.clone(),
        span: sig.span,
    }
}

#[cfg(test)]
#[path = "../../tests/core/typecheck/t_collect.rs"]
mod tests;
