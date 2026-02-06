use std::collections::{HashMap, HashSet};

use crate::diag::Span;
use crate::resolve::DefId;
use crate::tree::ParamMode;
use crate::tree::resolved::{
    Attribute, EnumDefVariant, FunctionSig, MethodItem, MethodSig, Param, StructDefField,
    TypeDefKind, TypeParam,
};
use crate::typecheck::engine::{
    CollectedCallableSig, CollectedParamSig, CollectedPropertySig, TypecheckEngine,
};
use crate::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::typeck::type_map::{resolve_type_expr, resolve_type_expr_with_params};
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
    let mut func_sigs = HashMap::new();
    let mut method_sigs = HashMap::new();
    let mut property_sigs = HashMap::new();
    let mut generic_envs = HashMap::new();
    let mut property_conflicts = HashSet::new();
    let mut errors = Vec::new();

    collect_type_defs(
        &ctx,
        &mut type_symbols,
        &mut type_defs,
        &mut generic_envs,
        &mut errors,
    );
    collect_function_sigs(&ctx, &mut func_sigs, &mut generic_envs, &mut errors);
    collect_method_sigs(
        &ctx,
        &type_defs,
        &mut method_sigs,
        &mut property_sigs,
        &mut property_conflicts,
        &mut generic_envs,
        &mut errors,
    );

    if !errors.is_empty() {
        return Err(errors);
    }

    let env = engine.env_mut();
    env.type_symbols = type_symbols;
    env.type_defs = type_defs;
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
            func_sigs.entry(sig.name.clone()).or_default(),
            generic_envs,
            errors,
        );
    }
}

fn collect_method_sigs(
    ctx: &crate::context::ResolvedContext,
    type_defs: &HashMap<String, Type>,
    method_sigs: &mut HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
    property_sigs: &mut HashMap<String, HashMap<String, CollectedPropertySig>>,
    property_conflicts: &mut HashSet<(String, String)>,
    generic_envs: &mut HashMap<DefId, HashMap<DefId, TyVarId>>,
    errors: &mut Vec<TypeCheckError>,
) {
    let mut items = Vec::new();
    for method_block in ctx.module.method_blocks() {
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
            items.push((method_block.type_name.clone(), def_id, sig, attrs, span));
        }
    }

    for (type_name, def_id, sig, attrs, span) in items {
        let mut collected = Vec::new();
        collect_callable_sig(
            ctx,
            def_id,
            &function_sig_from_method(&sig),
            Some(sig.self_param.mode.clone()),
            &mut collected,
            generic_envs,
            errors,
        );
        let Some(collected) = collected.pop() else {
            continue;
        };

        method_sigs
            .entry(type_name.clone())
            .or_default()
            .entry(sig.name.clone())
            .or_default()
            .push(collected.clone());

        if let Some(kind) = property_accessor_kind(&attrs) {
            record_property_sig(
                type_defs,
                property_sigs,
                property_conflicts,
                &type_name,
                &sig.name,
                kind,
                def_id,
                &collected.params,
                &collected.ret_ty,
                span,
                errors,
            );
        }
    }
}

fn collect_callable_sig(
    ctx: &crate::context::ResolvedContext,
    def_id: DefId,
    sig: &FunctionSig,
    self_mode: Option<ParamMode>,
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

    let ret_ty = match resolve_type_expr_with_params(
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
        self_mode,
    });
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

    if !property_sigs
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
mod tests {
    use super::*;
    use crate::context::ParsedContext;
    use crate::lexer::{LexError, Lexer, Token};
    use crate::parse::Parser;
    use crate::resolve::resolve;
    use crate::typecheck::engine::TypecheckEngine;

    fn resolve_source(source: &str) -> crate::context::ResolvedContext {
        let lexer = Lexer::new(source);
        let tokens = lexer
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .expect("Failed to tokenize");
        let mut parser = Parser::new(&tokens);
        let module = parser.parse().expect("Failed to parse");
        let id_gen = parser.into_id_gen();
        let ast_context = ParsedContext::new(module, id_gen);
        resolve(ast_context).expect("Failed to resolve")
    }

    #[test]
    fn test_collect_type_and_function_signatures() {
        let source = r#"
            type Pair<T> = { left: T, right: T }
            type Point = { x: u64, y: u64 }

            fn id<T>(x: T) -> T { x }
            fn add(x: u64, y: u64) -> u64 { x + y }
        "#;

        let resolved = resolve_source(source);
        let mut engine = TypecheckEngine::new(resolved);
        run(&mut engine).expect("collect pass failed");

        let env = engine.env();
        assert!(env.type_symbols.contains_key("Pair"));
        assert!(env.type_symbols.contains_key("Point"));
        assert!(env.type_defs.contains_key("Point"));
        assert!(!env.type_defs.contains_key("Pair"));

        let id_sigs = env.func_sigs.get("id").expect("missing id");
        assert_eq!(id_sigs.len(), 1);
        assert_eq!(id_sigs[0].type_param_count, 1);

        let add_sigs = env.func_sigs.get("add").expect("missing add");
        assert_eq!(add_sigs.len(), 1);
        assert_eq!(add_sigs[0].params.len(), 2);
        assert_eq!(add_sigs[0].ret_ty, Type::uint(64));
    }

    #[test]
    fn test_collect_method_and_property_signatures() {
        let source = r#"
            type Point = { x: u64, y: u64 }

            Point::{
                fn sum(self) -> u64 {
                    self.x + self.y
                }

                prop y_val: u64 {
                    get { self.y }
                    set(v) { self.y = v; }
                }
            }
        "#;

        let resolved = resolve_source(source);
        let mut engine = TypecheckEngine::new(resolved);
        run(&mut engine).expect("collect pass failed");

        let env = engine.env();
        let point_methods = env.method_sigs.get("Point").expect("missing Point methods");
        assert!(point_methods.contains_key("sum"));

        let props = env
            .property_sigs
            .get("Point")
            .expect("missing Point properties");
        let y_val = props.get("y_val").expect("missing y_val property");
        assert_eq!(y_val.ty, Type::uint(64));
        assert!(y_val.getter.is_some());
        assert!(y_val.setter.is_some());
    }
}
