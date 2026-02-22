//! Type-map construction and type-expression resolution helpers.
//!
//! This module is shared by type-check phases and downstream stages. It
//! resolves type expressions, records node/def/call typing side tables, and
//! materializes final `TypeMap` outputs.

use crate::core::context::{ResolvedContext, TypeCheckedContext};
use crate::core::diag::Span;
use crate::core::resolve::{Def, DefId, DefKind, DefTable};
use crate::core::tree as ast;
use crate::core::tree::semantic as sem;
use crate::core::tree::{NodeId, ParamMode, RefinementKind};
use crate::core::typecheck::errors::{TEK, TypeCheckError};
use crate::core::typecheck::nominal::NominalKey;
use crate::core::typecheck::utils::{fn_param_mode, nominal_key_concreteness};
use crate::core::types::{
    EnumVariant, FnParam, IntBounds, StructField, TyVarId, Type, TypeCache, TypeId,
};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;

type TypeParamMap = HashMap<DefId, TyVarId>;
type TypeArgMap = HashMap<DefId, Type>;

fn builtin_type(name: &str) -> Option<Type> {
    match name {
        "()" => Some(Type::Unit),
        "u8" => Some(Type::uint(8)),
        "u16" => Some(Type::uint(16)),
        "u32" => Some(Type::uint(32)),
        "u64" => Some(Type::uint(64)),
        "i8" => Some(Type::sint(8)),
        "i16" => Some(Type::sint(16)),
        "i32" => Some(Type::sint(32)),
        "i64" => Some(Type::sint(64)),
        "bool" => Some(Type::Bool),
        "char" => Some(Type::Char),
        "string" => Some(Type::String),
        _ => None,
    }
}

pub(crate) fn resolve_type_expr(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &ast::TypeExpr,
) -> Result<Type, TypeCheckError> {
    resolve_type_expr_with_params_and_args(def_table, module, type_expr, None, None, false)
}

pub(crate) fn resolve_type_expr_with_params(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &ast::TypeExpr,
    type_params: Option<&TypeParamMap>,
) -> Result<Type, TypeCheckError> {
    resolve_type_expr_with_params_and_args(def_table, module, type_expr, type_params, None, false)
}

pub(crate) fn resolve_return_type_expr_with_params(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &ast::TypeExpr,
    type_params: Option<&TypeParamMap>,
) -> Result<Type, TypeCheckError> {
    resolve_type_expr_with_params_and_args(def_table, module, type_expr, type_params, None, true)
}

#[allow(dead_code)]
pub(crate) fn resolve_type_def_with_args(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    def_id: DefId,
    type_args: &[Type],
) -> Result<Type, TypeCheckError> {
    let def = def_table
        .lookup_def(def_id)
        .ok_or(TEK::UnknownType.at(Span::default()))?;
    if let Some(imported_ty) = module.imported_type_by_id(def_id) {
        if !type_args.is_empty() {
            return Err(
                TEK::TypeArgCountMismatch(def.name.clone(), 0, type_args.len())
                    .at(Span::default())
                    .into(),
            );
        }
        return Ok(imported_ty.clone());
    }
    let type_def = module
        .type_def_by_id(def_table, def_id)
        .ok_or(TEK::UnknownType.at(Span::default()))?;

    if type_def.type_params.len() != type_args.len() {
        return Err(TEK::TypeArgCountMismatch(
            def.name.clone(),
            type_def.type_params.len(),
            type_args.len(),
        )
        .at(type_def.span)
        .into());
    }

    let mut arg_map = TypeArgMap::new();
    for (param, arg_ty) in type_def.type_params.iter().zip(type_args.iter()) {
        arg_map.insert(def_table.def_id(param.id), arg_ty.clone());
    }

    let type_name = mangle_type_name(&def.name, type_args);
    let mut in_progress = HashSet::new();
    match &type_def.kind {
        ast::TypeDefKind::Alias { aliased_ty } => resolve_type_alias(
            def_table,
            module,
            def,
            aliased_ty,
            None,
            Some(&arg_map),
            &mut in_progress,
            false,
        ),
        ast::TypeDefKind::Struct { fields } => resolve_struct_type(
            def_table,
            module,
            def.id,
            &type_name,
            fields,
            None,
            Some(&arg_map),
            &mut in_progress,
            false,
        ),
        ast::TypeDefKind::Enum { variants } => resolve_enum_type(
            def_table,
            module,
            def.id,
            &type_name,
            variants,
            None,
            Some(&arg_map),
            &mut in_progress,
            false,
        ),
    }
}

fn resolve_type_expr_with_params_and_args(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &ast::TypeExpr,
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    allow_error_union: bool,
) -> Result<Type, TypeCheckError> {
    let mut in_progress = HashSet::new();
    resolve_type_expr_impl(
        def_table,
        module,
        type_expr,
        type_params,
        type_args,
        &mut in_progress,
        allow_error_union,
    )
}

pub(crate) trait TypeDefLookup {
    fn type_def_by_id(&self, def_table: &DefTable, def_id: DefId) -> Option<&ast::TypeDef>;

    fn imported_type_by_id(&self, _def_id: DefId) -> Option<&Type> {
        None
    }
}

impl TypeDefLookup for ast::Module {
    fn type_def_by_id(&self, def_table: &DefTable, def_id: DefId) -> Option<&ast::TypeDef> {
        ast::Module::type_def_by_id(self, def_table, def_id)
    }
}

impl TypeDefLookup for sem::Module {
    fn type_def_by_id(&self, def_table: &DefTable, def_id: DefId) -> Option<&ast::TypeDef> {
        sem::Module::type_def_by_id(self, def_table, def_id)
    }
}

impl TypeDefLookup for ResolvedContext {
    fn type_def_by_id(&self, _def_table: &DefTable, def_id: DefId) -> Option<&ast::TypeDef> {
        self.module.type_def_by_id(&self.def_table, def_id)
    }
}

impl TypeDefLookup for TypeCheckedContext {
    fn type_def_by_id(&self, _def_table: &DefTable, def_id: DefId) -> Option<&ast::TypeDef> {
        self.module.type_def_by_id(&self.def_table, def_id)
    }
}

fn resolve_type_expr_impl(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &ast::TypeExpr,
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
    allow_error_union: bool,
) -> Result<Type, TypeCheckError> {
    match &type_expr.kind {
        ast::TypeExprKind::Infer => Err(TEK::UnknownType.at(type_expr.span).into()),
        ast::TypeExprKind::Union { variants } => {
            if !allow_error_union {
                return Err(TEK::UnionNotAllowedHere.at(type_expr.span).into());
            }
            let resolved = variants
                .iter()
                .map(|variant| {
                    resolve_type_expr_impl(
                        def_table,
                        module,
                        variant,
                        type_params,
                        type_args,
                        in_progress,
                        false,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            let (ok_ty, err_tys) = resolved
                .split_first()
                .expect("parser guarantees at least one union variant");
            Ok(Type::ErrorUnion {
                ok_ty: Box::new(ok_ty.clone()),
                err_tys: err_tys.to_vec(),
            })
        }
        ast::TypeExprKind::Named {
            ident,
            type_args: type_arg_exprs,
        } => {
            let def_id = if let Some(def_id) = def_table.lookup_node_def_id(type_expr.id) {
                def_id
            } else if let Some(type_params) = type_params {
                type_params
                    .keys()
                    .copied()
                    .find(|def_id| {
                        def_table
                            .lookup_def(*def_id)
                            .is_some_and(|def| def.name == *ident)
                    })
                    .or_else(|| def_table.lookup_type_def_id(ident))
                    .ok_or_else(|| TEK::UnknownType.at(type_expr.span))?
            } else {
                def_table
                    .lookup_type_def_id(ident)
                    .ok_or_else(|| TEK::UnknownType.at(type_expr.span))?
            };
            resolve_named_type(
                def_table,
                module,
                type_expr,
                &def_id,
                type_arg_exprs,
                type_params,
                type_args,
                in_progress,
                allow_error_union,
            )
        }
        ast::TypeExprKind::Array { elem_ty_expr, dims } => {
            let elem_ty = resolve_type_expr_impl(
                def_table,
                module,
                elem_ty_expr,
                type_params,
                type_args,
                in_progress,
                allow_error_union,
            )?;
            Ok(Type::Array {
                elem_ty: Box::new(elem_ty),
                dims: dims.clone(),
            })
        }
        ast::TypeExprKind::DynArray { elem_ty_expr } => {
            let elem_ty = resolve_type_expr_impl(
                def_table,
                module,
                elem_ty_expr,
                type_params,
                type_args,
                in_progress,
                allow_error_union,
            )?;
            Ok(Type::DynArray {
                elem_ty: Box::new(elem_ty),
            })
        }
        ast::TypeExprKind::Tuple { field_ty_exprs } => {
            let field_tys = field_ty_exprs
                .iter()
                .map(|f| {
                    resolve_type_expr_impl(
                        def_table,
                        module,
                        f,
                        type_params,
                        type_args,
                        in_progress,
                        allow_error_union,
                    )
                })
                .collect::<Result<Vec<Type>, _>>()?;
            Ok(Type::Tuple { field_tys })
        }
        ast::TypeExprKind::Refined {
            base_ty_expr,
            refinements,
        } => {
            let base_ty = resolve_type_expr_impl(
                def_table,
                module,
                base_ty_expr,
                type_params,
                type_args,
                in_progress,
                allow_error_union,
            )?;
            apply_refinements(base_ty, refinements, type_expr.span)
        }
        ast::TypeExprKind::Slice { elem_ty_expr } => {
            let elem_ty = resolve_type_expr_impl(
                def_table,
                module,
                elem_ty_expr,
                type_params,
                type_args,
                in_progress,
                allow_error_union,
            )?;
            Ok(Type::Slice {
                elem_ty: Box::new(elem_ty),
            })
        }
        ast::TypeExprKind::Heap { elem_ty_expr } => {
            let elem_ty = resolve_type_expr_impl(
                def_table,
                module,
                elem_ty_expr,
                type_params,
                type_args,
                in_progress,
                allow_error_union,
            )?;
            Ok(Type::Heap {
                elem_ty: Box::new(elem_ty),
            })
        }
        ast::TypeExprKind::Ref {
            mutable,
            elem_ty_expr,
        } => {
            let elem_ty = resolve_type_expr_impl(
                def_table,
                module,
                elem_ty_expr,
                type_params,
                type_args,
                in_progress,
                allow_error_union,
            )?;
            Ok(Type::Ref {
                mutable: *mutable,
                elem_ty: Box::new(elem_ty),
            })
        }
        ast::TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => {
            let param_tys = params
                .iter()
                .map(|param| {
                    let ty = resolve_type_expr_impl(
                        def_table,
                        module,
                        &param.ty_expr,
                        type_params,
                        type_args,
                        in_progress,
                        allow_error_union,
                    )?;
                    Ok(FnParam {
                        mode: fn_param_mode(param.mode.clone()),
                        ty,
                    })
                })
                .collect::<Result<Vec<_>, TypeCheckError>>()?;
            let ret_ty = resolve_type_expr_impl(
                def_table,
                module,
                ret_ty_expr,
                type_params,
                type_args,
                in_progress,
                allow_error_union,
            )?;
            Ok(Type::Fn {
                params: param_tys,
                ret_ty: Box::new(ret_ty),
            })
        }
    }
}

fn int_full_range(signed: bool, bits: u8) -> (i128, i128) {
    if signed {
        let max = 1i128 << (bits as u32 - 1);
        (-max, max)
    } else {
        (0, 1i128 << (bits as u32))
    }
}

fn apply_refinements(
    base_ty: Type,
    refinements: &[RefinementKind],
    span: Span,
) -> Result<Type, TypeCheckError> {
    let mut ty = base_ty;
    for refinement in refinements {
        match refinement {
            RefinementKind::Bounds { min, max } => {
                let Type::Int {
                    signed,
                    bits,
                    bounds,
                    nonzero,
                } = ty
                else {
                    return Err(TEK::RefinementBaseNotInt(ty).at(span).into());
                };
                let (min_bound, max_bound) = if let Some(bounds) = bounds {
                    (bounds.min, bounds.max_excl)
                } else {
                    int_full_range(signed, bits)
                };
                if *min < min_bound || *max > max_bound {
                    return Err(TEK::BoundsOutOfRange(*min, *max, min_bound, max_bound)
                        .at(span)
                        .into());
                }
                ty = Type::Int {
                    signed,
                    bits,
                    bounds: Some(IntBounds {
                        min: *min,
                        max_excl: *max,
                    }),
                    nonzero,
                };
            }
            RefinementKind::NonZero => {
                let Type::Int {
                    signed,
                    bits,
                    bounds,
                    ..
                } = ty
                else {
                    return Err(TEK::RefinementBaseNotInt(ty).at(span).into());
                };
                ty = Type::Int {
                    signed,
                    bits,
                    bounds,
                    nonzero: true,
                };
            }
        }
    }
    if let Type::Int {
        bounds: Some(bounds),
        nonzero: true,
        ..
    } = ty
        && (bounds.min > 0 || bounds.max_excl <= 0)
    {
        return Err(TEK::RedundantNonZero(bounds.min, bounds.max_excl)
            .at(span)
            .into());
    }

    Ok(ty)
}

fn type_arg_name(ty: &Type) -> String {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name.clone(),
        Type::Var(var) => format!("T{}", var.index()),
        _ => ty.to_string(),
    }
}

fn mangle_type_name(base: &str, type_args: &[Type]) -> String {
    if type_args.is_empty() {
        base.to_string()
    } else {
        let args = type_args
            .iter()
            .map(type_arg_name)
            .collect::<Vec<_>>()
            .join(", ");
        format!("{base}<{args}>")
    }
}

fn resolve_named_type(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    type_expr: &ast::TypeExpr,
    def_id: &DefId,
    type_arg_exprs: &[ast::TypeExpr],
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
    allow_error_union: bool,
) -> Result<Type, TypeCheckError> {
    let def = def_table
        .lookup_def(*def_id)
        .ok_or(TEK::UnknownType.at(type_expr.span))?;

    if def.name == "set" {
        if type_arg_exprs.len() != 1 {
            return Err(
                TEK::TypeArgCountMismatch(def.name.clone(), 1, type_arg_exprs.len())
                    .at(type_expr.span)
                    .into(),
            );
        }
        let elem_ty = resolve_type_expr_impl(
            def_table,
            module,
            &type_arg_exprs[0],
            type_params,
            type_args,
            in_progress,
            allow_error_union,
        )?;
        return Ok(Type::Set {
            elem_ty: Box::new(elem_ty),
        });
    }
    if def.name == "map" {
        if type_arg_exprs.len() != 2 {
            return Err(
                TEK::TypeArgCountMismatch(def.name.clone(), 2, type_arg_exprs.len())
                    .at(type_expr.span)
                    .into(),
            );
        }
        let key_ty = resolve_type_expr_impl(
            def_table,
            module,
            &type_arg_exprs[0],
            type_params,
            type_args,
            in_progress,
            allow_error_union,
        )?;
        let value_ty = resolve_type_expr_impl(
            def_table,
            module,
            &type_arg_exprs[1],
            type_params,
            type_args,
            in_progress,
            allow_error_union,
        )?;
        return Ok(Type::Map {
            key_ty: Box::new(key_ty),
            value_ty: Box::new(value_ty),
        });
    }
    if def.name == "Pending" || def.name == "ReplyCap" {
        if type_arg_exprs.len() != 1 {
            return Err(
                TEK::TypeArgCountMismatch(def.name.clone(), 1, type_arg_exprs.len())
                    .at(type_expr.span)
                    .into(),
            );
        }
        let response_set_ty = resolve_type_expr_impl(
            def_table,
            module,
            &type_arg_exprs[0],
            type_params,
            type_args,
            in_progress,
            true,
        )?;
        let response_tys = match response_set_ty {
            Type::ErrorUnion { ok_ty, err_tys } => std::iter::once(*ok_ty).chain(err_tys).collect(),
            other => vec![other],
        };
        return if def.name == "Pending" {
            Ok(Type::Pending { response_tys })
        } else {
            Ok(Type::ReplyCap { response_tys })
        };
    }

    if let Some(ty) = builtin_type(&def.name) {
        if !type_arg_exprs.is_empty() {
            return Err(
                TEK::TypeArgCountMismatch(def.name.clone(), 0, type_arg_exprs.len())
                    .at(type_expr.span)
                    .into(),
            );
        }
        return Ok(ty);
    }

    match &def.kind {
        DefKind::TypeParam => {
            if let Some(map) = type_args
                && let Some(ty) = map.get(def_id)
            {
                return Ok(ty.clone());
            }
            if let Some(map) = type_params
                && let Some(var) = map.get(def_id)
            {
                return Ok(Type::Var(*var));
            }
            Err(TEK::UnknownType.at(type_expr.span).into())
        }
        DefKind::TypeDef { .. } => {
            let Some(type_def) = module.type_def_by_id(def_table, *def_id) else {
                if let Some(imported_ty) = module.imported_type_by_id(*def_id) {
                    if !type_arg_exprs.is_empty() {
                        return Err(TEK::TypeArgCountMismatch(
                            def.name.clone(),
                            0,
                            type_arg_exprs.len(),
                        )
                        .at(type_expr.span)
                        .into());
                    }
                    return Ok(imported_ty.clone());
                }
                return Err(TEK::UnknownType.at(type_expr.span).into());
            };
            if type_def.type_params.is_empty() {
                if !type_arg_exprs.is_empty() {
                    return Err(TEK::TypeArgCountMismatch(
                        def.name.clone(),
                        0,
                        type_arg_exprs.len(),
                    )
                    .at(type_expr.span)
                    .into());
                }
                let type_name = def.name.as_str();
                return match &type_def.kind {
                    ast::TypeDefKind::Alias { aliased_ty } => resolve_type_alias(
                        def_table,
                        module,
                        def,
                        aliased_ty,
                        type_params,
                        type_args,
                        in_progress,
                        allow_error_union,
                    ),
                    ast::TypeDefKind::Struct { fields } => resolve_struct_type(
                        def_table,
                        module,
                        def.id,
                        type_name,
                        fields,
                        type_params,
                        type_args,
                        in_progress,
                        allow_error_union,
                    ),
                    ast::TypeDefKind::Enum { variants } => resolve_enum_type(
                        def_table,
                        module,
                        def.id,
                        type_name,
                        variants,
                        type_params,
                        type_args,
                        in_progress,
                        allow_error_union,
                    ),
                };
            }

            if type_def.type_params.len() != type_arg_exprs.len() {
                return Err(TEK::TypeArgCountMismatch(
                    def.name.clone(),
                    type_def.type_params.len(),
                    type_arg_exprs.len(),
                )
                .at(type_expr.span)
                .into());
            }

            let resolved_args = type_arg_exprs
                .iter()
                .map(|arg| {
                    resolve_type_expr_impl(
                        def_table,
                        module,
                        arg,
                        type_params,
                        type_args,
                        in_progress,
                        allow_error_union,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            let mut arg_map = TypeArgMap::new();
            for (param, arg_ty) in type_def.type_params.iter().zip(resolved_args.iter()) {
                arg_map.insert(def_table.def_id(param.id), arg_ty.clone());
            }

            let type_name = mangle_type_name(&def.name, &resolved_args);
            match &type_def.kind {
                ast::TypeDefKind::Alias { aliased_ty } => resolve_type_alias(
                    def_table,
                    module,
                    def,
                    aliased_ty,
                    type_params,
                    Some(&arg_map),
                    in_progress,
                    allow_error_union,
                ),
                ast::TypeDefKind::Struct { fields } => resolve_struct_type(
                    def_table,
                    module,
                    def.id,
                    &type_name,
                    fields,
                    type_params,
                    Some(&arg_map),
                    in_progress,
                    allow_error_union,
                ),
                ast::TypeDefKind::Enum { variants } => resolve_enum_type(
                    def_table,
                    module,
                    def.id,
                    &type_name,
                    variants,
                    type_params,
                    Some(&arg_map),
                    in_progress,
                    allow_error_union,
                ),
            }
        }
        _ => Err(TEK::UnknownType.at(type_expr.span).into()),
    }
}

fn resolve_type_alias(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    def: &Def,
    ty_expr: &ast::TypeExpr,
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
    allow_error_union: bool,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def.id) {
        return Ok(Type::Unknown);
    }
    in_progress.insert(def.id);
    let ty = resolve_type_expr_impl(
        def_table,
        module,
        ty_expr,
        type_params,
        type_args,
        in_progress,
        allow_error_union,
    );
    in_progress.remove(&def.id);
    ty
}

fn resolve_struct_type(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    def_id: DefId,
    type_name: &str,
    fields: &[ast::StructDefField],
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
    allow_error_union: bool,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def_id) {
        return Ok(Type::Struct {
            name: type_name.to_string(),
            // Recursive nominal references must stay shallow so later passes
            // can route recursion through nominal glue instead of truncating
            // structural drop/visit depth.
            fields: Vec::new(),
        });
    }
    in_progress.insert(def_id);
    let struct_fields = resolve_struct_fields(
        def_table,
        module,
        fields,
        type_params,
        type_args,
        in_progress,
        allow_error_union,
    )?;
    in_progress.remove(&def_id);
    Ok(Type::Struct {
        name: type_name.to_string(),
        fields: struct_fields,
    })
}

fn resolve_struct_fields(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    fields: &[ast::StructDefField],
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
    allow_error_union: bool,
) -> Result<Vec<StructField>, TypeCheckError> {
    fields
        .iter()
        .map(|field| {
            let field_ty = resolve_type_expr_impl(
                def_table,
                module,
                &field.ty,
                type_params,
                type_args,
                in_progress,
                allow_error_union,
            )?;
            Ok(StructField {
                name: field.name.clone(),
                ty: field_ty,
            })
        })
        .collect()
}

fn resolve_enum_type(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    def_id: DefId,
    type_name: &str,
    variants: &[ast::EnumDefVariant],
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
    allow_error_union: bool,
) -> Result<Type, TypeCheckError> {
    if in_progress.contains(&def_id) {
        return Ok(Type::Enum {
            name: type_name.to_string(),
            // Recursive nominal references must stay shallow so later passes
            // can route recursion through nominal glue instead of truncating
            // structural drop/visit depth.
            variants: Vec::new(),
        });
    }
    in_progress.insert(def_id);
    let enum_variants = resolve_enum_variants(
        def_table,
        module,
        variants,
        type_params,
        type_args,
        in_progress,
        allow_error_union,
    )?;
    in_progress.remove(&def_id);
    Ok(Type::Enum {
        name: type_name.to_string(),
        variants: enum_variants,
    })
}

fn resolve_enum_variants(
    def_table: &DefTable,
    module: &impl TypeDefLookup,
    variants: &[ast::EnumDefVariant],
    type_params: Option<&TypeParamMap>,
    type_args: Option<&TypeArgMap>,
    in_progress: &mut HashSet<DefId>,
    allow_error_union: bool,
) -> Result<Vec<EnumVariant>, TypeCheckError> {
    let mut enum_variants = Vec::new();
    for variant in variants {
        let payload = variant
            .payload
            .iter()
            .map(|payload_ty| {
                resolve_type_expr_impl(
                    def_table,
                    module,
                    payload_ty,
                    type_params,
                    type_args,
                    in_progress,
                    allow_error_union,
                )
            })
            .collect::<Result<Vec<Type>, _>>()?;
        enum_variants.push(EnumVariant {
            name: variant.name.clone(),
            payload,
        });
    }
    Ok(enum_variants)
}

pub struct TypeMapBuilder {
    type_table: TypeCache,
    node_type: HashMap<NodeId, TypeId>, // maps node to its type
    def_type: HashMap<Def, TypeId>,     // maps def to its type
    def_type_param_names: HashMap<DefId, BTreeMap<u32, String>>,
    nominal_keys: HashMap<TypeId, NominalKey>,
    call_sigs: HashMap<NodeId, CallSig>,
    generic_insts: HashMap<NodeId, GenericInst>,
}

impl Default for TypeMapBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeMapBuilder {
    pub fn new() -> Self {
        Self {
            type_table: TypeCache::new(),
            node_type: HashMap::new(),
            def_type: HashMap::new(),
            def_type_param_names: HashMap::new(),
            nominal_keys: HashMap::new(),
            call_sigs: HashMap::new(),
            generic_insts: HashMap::new(),
        }
    }

    pub fn record_def_type(&mut self, def: Def, typ: Type) {
        self.record_def_type_with_nominal(def, typ, None);
    }

    pub(crate) fn record_def_type_with_nominal(
        &mut self,
        def: Def,
        typ: Type,
        nominal_key: Option<NominalKey>,
    ) {
        let id = self.type_table.intern(typ);
        self.def_type.insert(def, id);
        self.record_nominal_key(id, nominal_key);
    }

    pub fn record_node_type(&mut self, node_id: NodeId, typ: Type) {
        self.record_node_type_with_nominal(node_id, typ, None);
    }

    pub(crate) fn record_node_type_with_nominal(
        &mut self,
        node_id: NodeId,
        typ: Type,
        nominal_key: Option<NominalKey>,
    ) {
        let id = self.type_table.intern(typ);
        self.node_type.insert(node_id, id);
        self.record_nominal_key(id, nominal_key);
    }

    pub fn record_call_sig(&mut self, node_id: NodeId, sig: CallSig) {
        self.call_sigs.insert(node_id, sig);
    }

    pub fn record_generic_inst(&mut self, node_id: NodeId, inst: GenericInst) {
        self.generic_insts.insert(node_id, inst);
    }

    pub fn record_def_type_param_names(&mut self, def_id: DefId, names: BTreeMap<u32, String>) {
        if names.is_empty() {
            return;
        }
        self.def_type_param_names.insert(def_id, names);
    }

    pub fn apply_inference<F>(&mut self, mut apply: F)
    where
        F: FnMut(&Type) -> Type,
    {
        for type_id in self.node_type.values_mut() {
            let ty = self.type_table.get(*type_id).clone();
            let new_ty = apply(&ty);
            *type_id = self.type_table.intern(new_ty);
        }

        for type_id in self.def_type.values_mut() {
            let ty = self.type_table.get(*type_id).clone();
            let new_ty = apply(&ty);
            *type_id = self.type_table.intern(new_ty);
        }

        for sig in self.call_sigs.values_mut() {
            if let Some(receiver) = &mut sig.receiver {
                receiver.ty = apply(&receiver.ty);
            }
            for param in sig.params.iter_mut() {
                param.ty = apply(&param.ty);
            }
        }

        for inst in self.generic_insts.values_mut() {
            inst.type_args = inst.type_args.iter().map(|ty| apply(ty)).collect();
        }
    }

    pub fn lookup_def_type(&self, def: &Def) -> Option<Type> {
        self.def_type
            .get(def)
            .map(|id| self.type_table.get(*id).clone())
    }

    pub fn lookup_def_type_id(&self, def: &Def) -> Option<TypeId> {
        self.def_type.get(def).copied()
    }

    pub fn finish(self) -> (TypeMap, CallSigMap, GenericInstMap) {
        let call_sigs = self.call_sigs;
        let generic_insts = self.generic_insts;
        (
            TypeMap {
                type_table: self.type_table,
                def_type: self.def_type,
                def_type_param_names: self.def_type_param_names,
                node_type: self.node_type,
                nominal_keys: self.nominal_keys,
            },
            call_sigs,
            generic_insts,
        )
    }

    fn record_nominal_key(&mut self, type_id: TypeId, nominal_key: Option<NominalKey>) {
        let Some(key) = nominal_key else {
            return;
        };
        if let Some(existing) = self.nominal_keys.get_mut(&type_id) {
            if let Some(selected) = select_more_concrete_nominal_key(existing, &key) {
                *existing = selected;
            }
            return;
        }
        self.nominal_keys.insert(type_id, key);
    }
}

#[derive(Debug, Clone)]
pub struct CallSig {
    pub def_id: Option<DefId>,
    pub receiver: Option<CallParam>,
    pub params: Vec<CallParam>,
}

pub type CallSigMap = HashMap<NodeId, CallSig>;

#[derive(Debug, Clone)]
pub struct GenericInst {
    pub def_id: DefId,
    pub type_args: Vec<Type>,
    pub call_span: Span,
}

pub type GenericInstMap = HashMap<NodeId, GenericInst>;

#[derive(Debug, Clone)]
pub struct CallParam {
    pub mode: ParamMode,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct TypeMap {
    type_table: TypeCache,
    def_type: HashMap<Def, TypeId>,
    def_type_param_names: HashMap<DefId, BTreeMap<u32, String>>,
    node_type: HashMap<NodeId, TypeId>,
    nominal_keys: HashMap<TypeId, NominalKey>,
}

impl TypeMap {
    pub fn lookup_node_type(&self, node: NodeId) -> Option<Type> {
        self.node_type
            .get(&node)
            .map(|id| self.type_table.get(*id).clone())
    }

    pub fn lookup_node_type_id(&self, node: NodeId) -> Option<TypeId> {
        self.node_type.get(&node).copied()
    }

    pub fn type_of(&self, node: NodeId) -> TypeId {
        self.node_type
            .get(&node)
            .copied()
            .unwrap_or_else(|| panic!("missing type for node {node}"))
    }

    pub fn lookup_def_type(&self, def: &Def) -> Option<Type> {
        self.def_type
            .get(def)
            .map(|id| self.type_table.get(*id).clone())
    }

    pub fn lookup_def_type_id(&self, def: &Def) -> Option<TypeId> {
        self.def_type.get(def).copied()
    }

    pub fn lookup_def_type_param_names(&self, def_id: DefId) -> Option<&BTreeMap<u32, String>> {
        self.def_type_param_names.get(&def_id)
    }

    pub(crate) fn lookup_nominal_key_for_type_id(&self, type_id: TypeId) -> Option<&NominalKey> {
        self.nominal_keys.get(&type_id)
    }

    pub(crate) fn record_nominal_key_for_type_id(&mut self, type_id: TypeId, key: NominalKey) {
        if let Some(existing) = self.nominal_keys.get_mut(&type_id) {
            if let Some(selected) = select_more_concrete_nominal_key(existing, &key) {
                *existing = selected;
            }
            return;
        }
        self.nominal_keys.insert(type_id, key);
    }

    pub fn insert_def_type(&mut self, def: Def, typ: Type) -> TypeId {
        let id = self.type_table.intern(typ);
        self.def_type.insert(def, id);
        id
    }

    pub fn insert_node_type(&mut self, node: NodeId, typ: Type) -> TypeId {
        let id = self.type_table.intern(typ);
        self.node_type.insert(node, id);
        id
    }

    pub fn type_table(&self) -> &TypeCache {
        &self.type_table
    }

    pub(crate) fn iter_node_type_ids(&self) -> impl Iterator<Item = (NodeId, TypeId)> + '_ {
        self.node_type
            .iter()
            .map(|(node_id, type_id)| (*node_id, *type_id))
    }

    pub(crate) fn iter_def_type_ids(&self) -> impl Iterator<Item = (&Def, TypeId)> + '_ {
        self.def_type.iter().map(|(def, type_id)| (def, *type_id))
    }
}

impl<'a> IntoIterator for &'a TypeMap {
    type Item = (&'a Def, &'a Type);
    type IntoIter = std::vec::IntoIter<(&'a Def, &'a Type)>;

    fn into_iter(self) -> Self::IntoIter {
        let mut items: Vec<_> = self
            .def_type
            .iter()
            .map(|(def, ty_id)| (def, self.type_table.get(*ty_id)))
            .collect();
        items.sort_by_key(|(def, _)| def.id);
        items.into_iter()
    }
}

impl fmt::Display for TypeMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // sort by node id
        let mut node_type = self.node_type.iter().collect::<Vec<(&NodeId, &TypeId)>>();
        node_type.sort_by_key(|(node, _)| node.0);
        for (node, ty_id) in node_type {
            let typ = self.type_table.get(*ty_id);
            writeln!(f, "Node [{}] -> Type [{}]", node, typ)?;
        }
        Ok(())
    }
}

fn select_more_concrete_nominal_key(
    existing: &NominalKey,
    candidate: &NominalKey,
) -> Option<NominalKey> {
    if existing == candidate {
        return None;
    }
    if existing.def_id != candidate.def_id || existing.type_args.len() != candidate.type_args.len()
    {
        // Conflicting identities should be rare; keep first-wins to preserve
        // deterministic behavior.
        return None;
    }
    let existing_score = nominal_key_concreteness(existing);
    let candidate_score = nominal_key_concreteness(candidate);
    (candidate_score > existing_score).then(|| candidate.clone())
}
