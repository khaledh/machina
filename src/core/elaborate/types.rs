//! Type expression synthesis.
//!
//! When elaboration generates new definitions (e.g., closure struct fields),
//! it needs to produce `TypeExpr` AST nodes that represent those types.
//! This module provides the machinery to convert internal `Type` values
//! back into syntactic `TypeExpr` nodes that can appear in the AST.
//!
//! This is essentially the inverse of type checking: given a resolved type,
//! produce the AST representation that would parse to that type.

use crate::core::ast::{FnTypeParam, ParamMode, RefinementKind, TypeExpr, TypeExprKind};
use crate::core::diag::Span;
use crate::core::types::{FnParamMode, Type};

use super::elaborator::Elaborator;

impl<'a> Elaborator<'a> {
    /// Convert an internal `Type` value into a `TypeExpr` AST node.
    ///
    /// Used when synthesizing new definitions that need type annotations,
    /// such as closure struct fields and method signatures.
    pub(super) fn type_expr_from_type(&mut self, ty: &Type, span: Span) -> TypeExpr {
        let id = self.node_id_gen.new_id();
        let kind = match ty {
            Type::Unknown => {
                panic!("compiler bug: unknown type in closure capture at {}", span)
            }
            Type::Var(_) => {
                panic!(
                    "compiler bug: type variable in type expression synthesis at {}",
                    span
                )
            }
            Type::Unit => self.named_type_expr("()"),
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
                    _ => {
                        panic!("compiler bug: unsupported int type signed={signed} bits={bits}")
                    }
                };
                let mut refinements = Vec::new();
                if let Some(bounds) = bounds {
                    let min = bounds.min;
                    let max = bounds.max_excl;
                    refinements.push(RefinementKind::Bounds { min, max });
                }
                if *nonzero {
                    refinements.push(RefinementKind::NonZero);
                }
                if refinements.is_empty() {
                    self.named_type_expr(name)
                } else {
                    let base_expr = TypeExpr {
                        id: self.node_id_gen.new_id(),
                        kind: self.named_type_expr(name),
                        span,
                    };
                    TypeExprKind::Refined {
                        base_ty_expr: Box::new(base_expr),
                        refinements,
                    }
                }
            }
            Type::Bool => self.named_type_expr("bool"),
            Type::Char => self.named_type_expr("char"),
            Type::String => self.named_type_expr("string"),
            Type::ErrorUnion { ok_ty, err_tys } => TypeExprKind::Union {
                variants: std::iter::once(ok_ty.as_ref())
                    .chain(err_tys.iter())
                    .map(|ty| self.type_expr_from_type(ty, span))
                    .collect(),
            },
            Type::Range { .. } => {
                panic!("compiler bug: range value type not representable in type expressions");
            }
            Type::Array { elem_ty, dims } => TypeExprKind::Array {
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
                dims: dims.clone(),
            },
            Type::DynArray { elem_ty } => TypeExprKind::DynArray {
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
            },
            Type::Pending { response_tys } => TypeExprKind::Named {
                ident: "Pending".to_string(),
                type_args: vec![self.response_set_type_arg_expr(response_tys, span)],
            },
            Type::ReplyCap { response_tys } => TypeExprKind::Named {
                ident: "ReplyCap".to_string(),
                type_args: vec![self.response_set_type_arg_expr(response_tys, span)],
            },
            Type::Set { elem_ty } => TypeExprKind::Named {
                ident: "set".to_string(),
                type_args: vec![self.type_expr_from_type(elem_ty, span)],
            },
            Type::Iterable { item_ty } => TypeExprKind::Named {
                ident: "Iterable".to_string(),
                type_args: vec![self.type_expr_from_type(item_ty, span)],
            },
            Type::Map { key_ty, value_ty } => TypeExprKind::Named {
                ident: "map".to_string(),
                type_args: vec![
                    self.type_expr_from_type(key_ty, span),
                    self.type_expr_from_type(value_ty, span),
                ],
            },
            Type::Tuple { field_tys } => TypeExprKind::Tuple {
                field_ty_exprs: field_tys
                    .iter()
                    .map(|field| self.type_expr_from_type(field, span))
                    .collect(),
            },
            Type::Struct { name, .. } | Type::Enum { name, .. } => self.named_type_expr(name),
            Type::Slice { elem_ty } => TypeExprKind::Slice {
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
            },
            Type::Heap { elem_ty } => TypeExprKind::Heap {
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
            },
            Type::Ref { mutable, elem_ty } => TypeExprKind::Ref {
                mutable: *mutable,
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
            },
            Type::Fn { params, ret_ty } => TypeExprKind::Fn {
                params: params
                    .iter()
                    .map(|param| FnTypeParam {
                        mode: match param.mode {
                            FnParamMode::In => ParamMode::In,
                            FnParamMode::InOut => ParamMode::InOut,
                            FnParamMode::Out => ParamMode::Out,
                            FnParamMode::Sink => ParamMode::Sink,
                        },
                        ty_expr: self.type_expr_from_type(&param.ty, span),
                    })
                    .collect(),
                ret_ty_expr: Box::new(self.type_expr_from_type(ret_ty, span)),
            },
        };

        TypeExpr { id, kind, span }
    }

    fn named_type_expr(&self, name: &str) -> TypeExprKind {
        TypeExprKind::Named {
            ident: name.to_string(),
            type_args: Vec::new(),
        }
    }

    fn response_set_type_arg_expr(&mut self, response_tys: &[Type], span: Span) -> TypeExpr {
        if response_tys.len() <= 1 {
            return self.type_expr_from_type(
                response_tys
                    .first()
                    .expect("response set type arg requires at least one variant"),
                span,
            );
        }
        TypeExpr {
            id: self.node_id_gen.new_id(),
            kind: TypeExprKind::Union {
                variants: response_tys
                    .iter()
                    .map(|ty| self.type_expr_from_type(ty, span))
                    .collect(),
            },
            span,
        }
    }
}
