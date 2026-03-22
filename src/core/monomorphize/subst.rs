//! Generic substitution and `Type -> TypeExpr` rebuilding for specialized clones.

use std::collections::HashMap;

use crate::core::ast::visit_mut::{VisitorMut, walk_type_expr};
use crate::core::ast::*;
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefKind, DefTable};
use crate::core::typecheck::type_map::GenericInst;
use crate::core::types::{FnParamMode, Type};

use super::{MonomorphizeError, MonomorphizeErrorKind};

pub(super) fn apply_inst_to_func_def(
    func_def: &mut FuncDef,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let subst = build_subst(&func_def.sig.type_params, inst, def_table)?;
    func_def.sig.type_params.clear();
    {
        let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
        substituter.visit_func_def(func_def);
        substituter.finish()?;
    }
    apply_iterable_param_inst(
        &mut func_def.sig.params,
        &inst.iterable_param_tys,
        def_table,
        node_id_gen,
    )
}

pub(super) fn apply_inst_to_func_decl(
    func_decl: &mut FuncDecl,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let subst = build_subst(&func_decl.sig.type_params, inst, def_table)?;
    func_decl.sig.type_params.clear();
    {
        let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
        substituter.visit_func_decl(func_decl);
        substituter.finish()?;
    }
    apply_iterable_param_inst(
        &mut func_decl.sig.params,
        &inst.iterable_param_tys,
        def_table,
        node_id_gen,
    )
}

pub(super) fn apply_inst_to_method_def(
    receiver_type_args: &[TypeExpr],
    method_def: &mut MethodDef,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let type_params = method_block_type_params(def_table, receiver_type_args)
        .into_iter()
        .chain(method_def.sig.type_params.iter().cloned())
        .collect::<Vec<_>>();
    let subst = build_subst(&type_params, inst, def_table)?;
    method_def.sig.type_params.clear();
    {
        let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
        substituter.visit_method_def(method_def);
        substituter.finish()?;
    }
    apply_iterable_param_inst(
        &mut method_def.sig.params,
        &inst.iterable_param_tys,
        def_table,
        node_id_gen,
    )
}

pub(super) fn apply_inst_to_method_decl(
    receiver_type_args: &[TypeExpr],
    method_decl: &mut MethodDecl,
    inst: &GenericInst,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let type_params = method_block_type_params(def_table, receiver_type_args)
        .into_iter()
        .chain(method_decl.sig.type_params.iter().cloned())
        .collect::<Vec<_>>();
    let subst = build_subst(&type_params, inst, def_table)?;
    method_decl.sig.type_params.clear();
    {
        let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
        substituter.visit_method_decl(method_decl);
        substituter.finish()?;
    }
    apply_iterable_param_inst(
        &mut method_decl.sig.params,
        &inst.iterable_param_tys,
        def_table,
        node_id_gen,
    )
}

pub(super) fn apply_inst_to_method_block(
    method_block: &mut MethodBlock,
    receiver_inst_args: &[Type],
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    let type_params = method_block_type_params(def_table, &method_block.type_args);
    if type_params.is_empty() {
        return Ok(());
    }

    if type_params.len() != receiver_inst_args.len() {
        return Err(MonomorphizeErrorKind::ArityMismatch {
            name: method_block.type_name.clone(),
            expected: type_params.len(),
            got: receiver_inst_args.len(),
        }
        .at(method_block.span));
    }
    let subst = type_params
        .iter()
        .zip(receiver_inst_args.iter().cloned())
        .map(|(param, ty)| (def_table.def_id(param.id), ty))
        .collect::<HashMap<_, _>>();
    let mut substituter = TypeExprSubstitutor::new(&subst, def_table, node_id_gen);
    for type_arg in &mut method_block.type_args {
        substituter.visit_type_expr(type_arg);
    }
    substituter.finish()
}

pub(super) fn method_block_type_param_count(
    def_table: &DefTable,
    receiver_type_args: &[TypeExpr],
) -> usize {
    method_block_type_params(def_table, receiver_type_args).len()
}

fn method_block_type_params(
    def_table: &DefTable,
    receiver_type_args: &[TypeExpr],
) -> Vec<TypeParam> {
    receiver_type_args
        .iter()
        .filter_map(|type_arg| {
            let TypeExprKind::Named { ident, type_args } = &type_arg.kind else {
                return None;
            };
            if !type_args.is_empty() {
                return None;
            }
            let def_id = def_table.lookup_node_def_id(type_arg.id)?;
            let def = def_table.lookup_def(def_id)?;
            if !matches!(def.kind, DefKind::TypeParam) {
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

fn build_subst(
    type_params: &[TypeParam],
    inst: &GenericInst,
    def_table: &DefTable,
) -> Result<HashMap<DefId, Type>, MonomorphizeError> {
    if type_params.len() != inst.type_args.len() {
        let name = def_name(def_table, inst.def_id);
        return Err(MonomorphizeErrorKind::ArityMismatch {
            name,
            expected: type_params.len(),
            got: inst.type_args.len(),
        }
        .at(inst.call_span));
    }

    Ok(type_params
        .iter()
        .zip(inst.type_args.iter().cloned())
        .map(|(param, ty)| (def_table.def_id(param.id), ty))
        .collect())
}

fn apply_iterable_param_inst(
    params: &mut [Param],
    concrete_tys: &[Type],
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
) -> Result<(), MonomorphizeError> {
    if concrete_tys.is_empty() {
        return Ok(());
    }

    let iterable_params = params
        .iter_mut()
        .filter(|param| type_expr_is_iterable(&param.typ))
        .collect::<Vec<_>>();
    if iterable_params.len() != concrete_tys.len() {
        return Err(MonomorphizeErrorKind::ArityMismatch {
            name: "Iterable".to_string(),
            expected: iterable_params.len(),
            got: concrete_tys.len(),
        }
        .at(params.first().map(|p| p.span).unwrap_or_default()));
    }

    for (param, ty) in iterable_params.into_iter().zip(concrete_tys.iter()) {
        param.typ = type_expr_from_type(ty, def_table, node_id_gen, param.typ.span)?;
    }

    Ok(())
}

fn type_expr_is_iterable(ty_expr: &TypeExpr) -> bool {
    matches!(&ty_expr.kind, TypeExprKind::Named { ident, type_args } if ident == "Iterable" && type_args.len() == 1)
}

struct TypeExprSubstitutor<'a> {
    subst: &'a HashMap<DefId, Type>,
    def_table: &'a DefTable,
    node_id_gen: &'a mut NodeIdGen,
    error: Option<MonomorphizeError>,
}

impl<'a> TypeExprSubstitutor<'a> {
    fn new(
        subst: &'a HashMap<DefId, Type>,
        def_table: &'a DefTable,
        node_id_gen: &'a mut NodeIdGen,
    ) -> Self {
        Self {
            subst,
            def_table,
            node_id_gen,
            error: None,
        }
    }

    fn finish(self) -> Result<(), MonomorphizeError> {
        if let Some(err) = self.error {
            Err(err)
        } else {
            Ok(())
        }
    }
}

impl<'a> VisitorMut for TypeExprSubstitutor<'a> {
    fn visit_type_expr(&mut self, type_expr: &mut TypeExpr) {
        if self.error.is_some() {
            return;
        }

        if let TypeExprKind::Named { .. } = &type_expr.kind
            && let Some(def_id) = self.def_table.lookup_node_def_id(type_expr.id)
            && let Some(ty) = self.subst.get(&def_id)
        {
            match type_expr_from_type(ty, self.def_table, self.node_id_gen, type_expr.span) {
                Ok(new_expr) => {
                    *type_expr = new_expr;
                    return;
                }
                Err(err) => {
                    self.error = Some(err);
                    return;
                }
            }
        }

        walk_type_expr(self, type_expr);
    }
}

fn type_expr_from_type(
    ty: &Type,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Result<TypeExpr, MonomorphizeError> {
    let id = node_id_gen.new_id();
    let kind = match ty {
        Type::Unit => {
            return named_type_expr("()", def_table, node_id_gen, span);
        }
        Type::Int {
            signed,
            bits,
            bounds,
            nonzero,
        } => {
            let name = match (*signed, *bits) {
                (false, 8) => "u8",
                (false, 16) => "u16",
                (false, 32) => "u32",
                (false, 64) => "u64",
                (true, 8) => "i8",
                (true, 16) => "i16",
                (true, 32) => "i32",
                (true, 64) => "i64",
                _ => return Err(MonomorphizeErrorKind::UnsupportedType.at(span)),
            };
            let mut refinements = Vec::new();
            if let Some(bounds) = bounds {
                refinements.push(RefinementKind::Bounds {
                    min: bounds.min,
                    max: bounds.max_excl,
                });
            }
            if *nonzero {
                refinements.push(RefinementKind::NonZero);
            }
            if refinements.is_empty() {
                return named_type_expr(name, def_table, node_id_gen, span);
            }
            let base_expr = named_type_expr(name, def_table, node_id_gen, span)?;
            TypeExprKind::Refined {
                base_ty_expr: Box::new(base_expr),
                refinements,
            }
        }
        Type::Bool => {
            return named_type_expr("bool", def_table, node_id_gen, span);
        }
        Type::Char => {
            return named_type_expr("char", def_table, node_id_gen, span);
        }
        Type::String => {
            return named_type_expr("string", def_table, node_id_gen, span);
        }
        Type::Array { elem_ty, dims } => TypeExprKind::Array {
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
            dims: dims.clone(),
        },
        Type::DynArray { elem_ty } => TypeExprKind::DynArray {
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
        },
        Type::Pending { response_tys } => TypeExprKind::Named {
            ident: "Pending".to_string(),
            type_args: vec![response_set_type_arg_expr(
                response_tys,
                def_table,
                node_id_gen,
                span,
            )?],
        },
        Type::ReplyCap { response_tys } => TypeExprKind::Named {
            ident: "ReplyCap".to_string(),
            type_args: vec![response_set_type_arg_expr(
                response_tys,
                def_table,
                node_id_gen,
                span,
            )?],
        },
        Type::Set { elem_ty } => TypeExprKind::Named {
            ident: "set".to_string(),
            type_args: vec![type_expr_from_type(elem_ty, def_table, node_id_gen, span)?],
        },
        Type::Iterable { item_ty } => TypeExprKind::Named {
            ident: "Iterable".to_string(),
            type_args: vec![type_expr_from_type(item_ty, def_table, node_id_gen, span)?],
        },
        Type::Map { key_ty, value_ty } => TypeExprKind::Named {
            ident: "map".to_string(),
            type_args: vec![
                type_expr_from_type(key_ty, def_table, node_id_gen, span)?,
                type_expr_from_type(value_ty, def_table, node_id_gen, span)?,
            ],
        },
        Type::Tuple { field_tys } => TypeExprKind::Tuple {
            field_ty_exprs: field_tys
                .iter()
                .map(|ty| type_expr_from_type(ty, def_table, node_id_gen, span))
                .collect::<Result<Vec<_>, _>>()?,
        },
        Type::Slice { elem_ty } => TypeExprKind::Slice {
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
        },
        Type::Heap { elem_ty } => TypeExprKind::Heap {
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
        },
        Type::Ref { mutable, elem_ty } => TypeExprKind::Ref {
            mutable: *mutable,
            elem_ty_expr: Box::new(type_expr_from_type(elem_ty, def_table, node_id_gen, span)?),
        },
        Type::Fn { params, ret_ty } => {
            let params = params
                .iter()
                .map(|param| {
                    let mode = match param.mode {
                        FnParamMode::In => ParamMode::In,
                        FnParamMode::InOut => ParamMode::InOut,
                        FnParamMode::Out => ParamMode::Out,
                        FnParamMode::Sink => ParamMode::Sink,
                    };
                    Ok(FnTypeParam {
                        mode,
                        ty_expr: type_expr_from_type(&param.ty, def_table, node_id_gen, span)?,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            TypeExprKind::Fn {
                params,
                ret_ty_expr: Box::new(type_expr_from_type(ret_ty, def_table, node_id_gen, span)?),
            }
        }
        Type::ErrorUnion { ok_ty, err_tys } => TypeExprKind::Union {
            variants: std::iter::once(ok_ty.as_ref())
                .chain(err_tys.iter())
                .map(|ty| type_expr_from_type(ty, def_table, node_id_gen, span))
                .collect::<Result<Vec<_>, _>>()?,
        },
        Type::Struct { name, .. } | Type::Enum { name, .. } => {
            return named_type_expr(name, def_table, node_id_gen, span);
        }
        Type::Range { .. } => return Err(MonomorphizeErrorKind::UnsupportedType.at(span)),
        Type::Unknown | Type::Var(_) => {
            return Err(MonomorphizeErrorKind::UnsupportedType.at(span));
        }
    };

    Ok(TypeExpr { id, kind, span })
}

fn response_set_type_arg_expr(
    response_tys: &[Type],
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Result<TypeExpr, MonomorphizeError> {
    if response_tys.len() <= 1 {
        return type_expr_from_type(
            response_tys
                .first()
                .ok_or(MonomorphizeErrorKind::UnsupportedType.at(span))?,
            def_table,
            node_id_gen,
            span,
        );
    }
    Ok(TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Union {
            variants: response_tys
                .iter()
                .map(|ty| type_expr_from_type(ty, def_table, node_id_gen, span))
                .collect::<Result<Vec<_>, _>>()?,
        },
        span,
    })
}

fn named_type_expr(
    name: &str,
    def_table: &DefTable,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Result<TypeExpr, MonomorphizeError> {
    if def_table.lookup_type_def_id(name).is_none() {
        return Err(MonomorphizeErrorKind::UnknownType {
            name: name.to_string(),
        }
        .at(span));
    }
    Ok(TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Named {
            ident: name.to_string(),
            type_args: Vec::new(),
        },
        span,
    })
}

pub(super) fn def_name(def_table: &DefTable, def_id: DefId) -> String {
    def_table
        .lookup_def(def_id)
        .map(|def| def.name.clone())
        .unwrap_or_else(|| format!("def_{def_id:?}"))
}
