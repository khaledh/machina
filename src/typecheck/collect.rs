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

use crate::diag::Span;
use crate::resolve::DefId;
use crate::tree::ParamMode;
use crate::tree::resolved::{
    Attribute, EnumDefVariant, FunctionSig, MethodItem, MethodSig, Param, StructDefField,
    TypeDefKind, TypeParam,
};
use crate::typecheck::engine::{
    CollectedCallableSig, CollectedParamSig, CollectedPropertySig, CollectedTraitMethodSig,
    CollectedTraitPropertySig, CollectedTraitSig, TypecheckEngine,
};
use crate::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::typecheck::type_map::{
    resolve_return_type_expr_with_params, resolve_type_expr, resolve_type_expr_with_params,
};
use crate::types::{EnumVariant, StructField, TyVarId, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PropertyAccessorKind {
    Get,
    Set,
}

/// Pass 1: collect global symbols/signatures into the engine environment.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let ctx = engine.context().clone();

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
        &mut type_symbols,
        &mut type_defs,
        &mut generic_envs,
        &mut errors,
    );

    // 2) Collect trait method contracts.
    collect_trait_sigs(&ctx, &mut trait_sigs, &mut errors);

    // 3) Collect function overloads.
    collect_function_sigs(&ctx, &mut func_sigs, &mut generic_envs, &mut errors);

    // 4) Collect method overloads and synthesized property signatures.
    collect_method_sigs(
        &ctx,
        &type_defs,
        &trait_sigs,
        &mut trait_impls,
        &mut method_sigs,
        &mut property_sigs,
        &mut property_conflicts,
        &mut generic_envs,
        &mut errors,
    );

    if !errors.is_empty() {
        return Err(errors);
    }

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

    Ok(())
}

fn collect_type_defs(
    ctx: &crate::context::ResolvedContext,
    type_symbols: &mut HashMap<String, DefId>,
    type_defs: &mut HashMap<String, Type>,
    generic_envs: &mut HashMap<DefId, HashMap<DefId, TyVarId>>,
    errors: &mut Vec<TypeCheckError>,
) {
    for type_def in ctx.module.type_defs() {
        type_symbols.insert(type_def.name.clone(), type_def.def_id);
        record_generic_env(type_def.def_id, &type_def.type_params, generic_envs);

        if !type_def.type_params.is_empty() {
            // Generic type definitions are instantiated in later passes.
            continue;
        }

        let resolved = match &type_def.kind {
            TypeDefKind::Alias { aliased_ty } => {
                resolve_type_expr(&ctx.def_table, &ctx.module, aliased_ty)
            }
            TypeDefKind::Struct { fields } => {
                resolve_struct_type(ctx, fields).map(|fields| Type::Struct {
                    name: type_def.name.clone(),
                    fields,
                })
            }
            TypeDefKind::Enum { variants } => {
                resolve_enum_type(ctx, variants).map(|variants| Type::Enum {
                    name: type_def.name.clone(),
                    variants,
                })
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
    ctx: &crate::context::ResolvedContext,
    fields: &[StructDefField],
) -> Result<Vec<StructField>, TypeCheckError> {
    let mut out = Vec::with_capacity(fields.len());
    for field in fields {
        let field_ty = resolve_type_expr(&ctx.def_table, &ctx.module, &field.ty)?;
        out.push(StructField {
            name: field.name.clone(),
            ty: field_ty,
        });
    }
    Ok(out)
}

fn resolve_enum_type(
    ctx: &crate::context::ResolvedContext,
    variants: &[EnumDefVariant],
) -> Result<Vec<EnumVariant>, TypeCheckError> {
    let mut out = Vec::with_capacity(variants.len());
    for variant in variants {
        let payload = variant
            .payload
            .iter()
            .map(|payload_ty| resolve_type_expr(&ctx.def_table, &ctx.module, payload_ty))
            .collect::<Result<Vec<_>, _>>()?;
        out.push(EnumVariant {
            name: variant.name.clone(),
            payload,
        });
    }
    Ok(out)
}

fn collect_trait_sigs(
    ctx: &crate::context::ResolvedContext,
    trait_sigs: &mut HashMap<String, CollectedTraitSig>,
    errors: &mut Vec<TypeCheckError>,
) {
    for trait_def in ctx.module.trait_defs() {
        let mut methods = HashMap::new();
        let mut properties = HashMap::new();

        for method in &trait_def.methods {
            let Some(collected) = collect_trait_method_sig(ctx, &method.sig, errors) else {
                continue;
            };

            if methods.insert(method.sig.name.clone(), collected).is_some() {
                errors.push(
                    TypeCheckErrorKind::TraitMethodDuplicate(
                        trait_def.name.clone(),
                        method.sig.name.clone(),
                        method.span,
                    )
                    .into(),
                );
            }
        }

        for property in &trait_def.properties {
            let ty = match resolve_type_expr(&ctx.def_table, &ctx.module, &property.ty) {
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
                errors.push(
                    TypeCheckErrorKind::TraitPropertyDuplicate(
                        trait_def.name.clone(),
                        property.name.clone(),
                        property.span,
                    )
                    .into(),
                );
            }
        }

        trait_sigs.insert(
            trait_def.name.clone(),
            CollectedTraitSig {
                def_id: trait_def.def_id,
                methods,
                properties,
                span: trait_def.span,
            },
        );
    }
}

fn collect_function_sigs(
    ctx: &crate::context::ResolvedContext,
    func_sigs: &mut HashMap<String, Vec<CollectedCallableSig>>,
    generic_envs: &mut HashMap<DefId, HashMap<DefId, TyVarId>>,
    errors: &mut Vec<TypeCheckError>,
) {
    let mut overloads = Vec::new();
    for func_decl in ctx.module.func_decls() {
        overloads.push((func_decl.def_id, func_decl.sig.clone()));
    }
    for func_def in ctx.module.func_defs() {
        overloads.push((func_def.def_id, func_def.sig.clone()));
    }

    for (def_id, sig) in overloads {
        collect_callable_sig(
            ctx,
            def_id,
            &sig,
            None,
            None,
            func_sigs.entry(sig.name.clone()).or_default(),
            generic_envs,
            errors,
        );
    }
}

fn collect_method_sigs(
    ctx: &crate::context::ResolvedContext,
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
                errors.push(
                    TypeCheckErrorKind::TraitImplDuplicate(
                        method_block.type_name.clone(),
                        trait_name.clone(),
                        method_block.span,
                    )
                    .into(),
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
                    method_decl.def_id,
                    method_decl.sig.clone(),
                    method_decl.attrs.clone(),
                    method_decl.span,
                ),
                MethodItem::Def(method_def) => (
                    method_def.def_id,
                    method_def.sig.clone(),
                    method_def.attrs.clone(),
                    method_def.span,
                ),
            };
            let accessor_kind = property_accessor_kind(&attrs);

            let mut collected = Vec::new();
            collect_callable_sig(
                ctx,
                def_id,
                &function_sig_from_method(&sig),
                Some(sig.self_param.mode.clone()),
                method_block.trait_name.clone(),
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
                    errors.push(
                        TypeCheckErrorKind::TraitPropertyNotInTrait(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            prop_name.clone(),
                            method_block.span,
                        )
                        .into(),
                    );
                }
            }

            for required in contract.methods.keys() {
                if !implemented_trait_methods.contains(required) {
                    errors.push(
                        TypeCheckErrorKind::TraitMethodMissingImpl(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            required.clone(),
                            method_block.span,
                        )
                        .into(),
                    );
                }
            }

            for (prop_name, required) in &contract.properties {
                let impl_prop = property_sigs
                    .get(&method_block.type_name)
                    .and_then(|by_name| by_name.get(prop_name));

                let Some(impl_prop) = impl_prop else {
                    errors.push(
                        TypeCheckErrorKind::TraitPropertyMissingImpl(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            prop_name.clone(),
                            method_block.span,
                        )
                        .into(),
                    );
                    continue;
                };

                if impl_prop.ty != required.ty {
                    errors.push(
                        TypeCheckErrorKind::TraitPropertyTypeMismatch(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            prop_name.clone(),
                            required.ty.clone(),
                            impl_prop.ty.clone(),
                            method_block.span,
                        )
                        .into(),
                    );
                }
                if required.has_get && impl_prop.getter.is_none() {
                    errors.push(
                        TypeCheckErrorKind::TraitPropertyMissingGetter(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            prop_name.clone(),
                            method_block.span,
                        )
                        .into(),
                    );
                }
                if required.has_set && impl_prop.setter.is_none() {
                    errors.push(
                        TypeCheckErrorKind::TraitPropertyMissingSetter(
                            method_block.type_name.clone(),
                            trait_name.clone(),
                            prop_name.clone(),
                            method_block.span,
                        )
                        .into(),
                    );
                }
            }
        }
    }
}

fn collect_callable_sig(
    ctx: &crate::context::ResolvedContext,
    def_id: DefId,
    sig: &FunctionSig,
    self_mode: Option<ParamMode>,
    impl_trait: Option<String>,
    out: &mut Vec<CollectedCallableSig>,
    generic_envs: &mut HashMap<DefId, HashMap<DefId, TyVarId>>,
    errors: &mut Vec<TypeCheckError>,
) {
    let type_param_map = if sig.type_params.is_empty() {
        None
    } else {
        let map = type_param_map(&sig.type_params);
        generic_envs.insert(def_id, map.clone());
        Some(map)
    };

    let params = match build_param_sigs(ctx, &sig.params, type_param_map.as_ref()) {
        Ok(params) => params,
        Err(err) => {
            errors.push(err);
            return;
        }
    };

    let ret_ty = match resolve_return_type_expr_with_params(
        &ctx.def_table,
        &ctx.module,
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
        params,
        ret_ty,
        type_param_count: sig.type_params.len(),
        type_param_bounds: type_param_bounds(&sig.type_params),
        self_mode,
        impl_trait,
    });
}

fn collect_trait_method_sig(
    ctx: &crate::context::ResolvedContext,
    sig: &MethodSig,
    errors: &mut Vec<TypeCheckError>,
) -> Option<CollectedTraitMethodSig> {
    let type_param_map = if sig.type_params.is_empty() {
        None
    } else {
        Some(type_param_map(&sig.type_params))
    };

    let params = match build_param_sigs(ctx, &sig.params, type_param_map.as_ref()) {
        Ok(params) => params,
        Err(err) => {
            errors.push(err);
            return None;
        }
    };

    let ret_ty = match resolve_return_type_expr_with_params(
        &ctx.def_table,
        &ctx.module,
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
        errors.push(
            TypeCheckErrorKind::TraitMethodNotInTrait(
                type_name.to_string(),
                trait_name.to_string(),
                method_name.to_string(),
                span,
            )
            .into(),
        );
        return;
    };

    if !seen_methods.insert(method_name.to_string()) {
        errors.push(
            TypeCheckErrorKind::TraitMethodImplDuplicate(
                type_name.to_string(),
                trait_name.to_string(),
                method_name.to_string(),
                span,
            )
            .into(),
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
        errors.push(
            TypeCheckErrorKind::TraitMethodSignatureMismatch(
                type_name.to_string(),
                trait_name.to_string(),
                method_name.to_string(),
                span,
            )
            .into(),
        );
    }
}

fn build_param_sigs(
    ctx: &crate::context::ResolvedContext,
    params: &[Param],
    type_params: Option<&HashMap<DefId, TyVarId>>,
) -> Result<Vec<CollectedParamSig>, TypeCheckError> {
    let mut out = Vec::with_capacity(params.len());
    for param in params {
        let ty =
            resolve_type_expr_with_params(&ctx.def_table, &ctx.module, &param.typ, type_params)?;
        let name = ctx
            .def_table
            .lookup_def(param.def_id)
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
    type_params: &[TypeParam],
    generic_envs: &mut HashMap<DefId, HashMap<DefId, TyVarId>>,
) {
    if type_params.is_empty() {
        return;
    }
    generic_envs.insert(owner, type_param_map(type_params));
}

fn type_param_map(type_params: &[TypeParam]) -> HashMap<DefId, TyVarId> {
    type_params
        .iter()
        .enumerate()
        .map(|(index, param)| (param.def_id, TyVarId::new(index as u32)))
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
                errors.push(
                    TypeCheckErrorKind::PropertyGetterHasParams(prop_name.to_string(), span).into(),
                );
                return;
            }
            ret_ty.clone()
        }
        PropertyAccessorKind::Set => {
            if params.len() != 1 {
                errors.push(
                    TypeCheckErrorKind::PropertySetterParamCount(
                        prop_name.to_string(),
                        params.len(),
                        span,
                    )
                    .into(),
                );
                return;
            }
            if *ret_ty != Type::Unit {
                errors.push(
                    TypeCheckErrorKind::PropertySetterReturnType(
                        prop_name.to_string(),
                        ret_ty.clone(),
                        span,
                    )
                    .into(),
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
    {
        if let Some(Type::Struct { fields, .. }) = type_defs.get(type_name) {
            if let Some(field) = fields.iter().find(|field| field.name == prop_name) {
                if property_conflicts.insert((type_name.to_string(), prop_name.to_string())) {
                    errors.push(
                        TypeCheckErrorKind::PropertyConflictsWithField(
                            prop_name.to_string(),
                            field.name.clone(),
                            span,
                        )
                        .into(),
                    );
                }
                return;
            }
        }
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
        errors.push(
            TypeCheckErrorKind::PropertyAccessorTypeMismatch(
                prop_name.to_string(),
                entry.ty.clone(),
                prop_ty,
                span,
            )
            .into(),
        );
        return;
    }

    match kind {
        PropertyAccessorKind::Get => {
            if entry.getter.is_some() {
                errors.push(
                    TypeCheckErrorKind::PropertyAccessorDuplicate(prop_name.to_string(), span)
                        .into(),
                );
            } else {
                entry.getter = Some(def_id);
            }
        }
        PropertyAccessorKind::Set => {
            if entry.setter.is_some() {
                errors.push(
                    TypeCheckErrorKind::PropertyAccessorDuplicate(prop_name.to_string(), span)
                        .into(),
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
#[path = "../tests/typecheck/t_collect.rs"]
mod tests;
