//! Type expression synthesis.
//!
//! When elaboration generates new definitions (e.g., closure struct fields),
//! it needs to produce `TypeExpr` AST nodes that represent those types.
//! This module provides the machinery to convert internal `Type` values
//! back into syntactic `TypeExpr` nodes that can appear in the semantic tree.
//!
//! This is essentially the inverse of type checking: given a resolved type,
//! produce the AST representation that would parse to that type.

use crate::diag::Span;
use crate::tree::semantic as sem;
use crate::tree::{ParamMode, RefinementKind};
use crate::types::{FnParamMode, Type};

use super::elaborator::Elaborator;

impl<'a> Elaborator<'a> {
    /// Convert an internal `Type` value into a `TypeExpr` AST node.
    ///
    /// Used when synthesizing new definitions that need type annotations,
    /// such as closure struct fields and method signatures.
    pub(super) fn type_expr_from_type(&mut self, ty: &Type, span: Span) -> sem::TypeExpr {
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
            Type::Unit => self.named_type_expr("()", span),
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
                    self.named_type_expr(name, span)
                } else {
                    let base_expr = sem::TypeExpr {
                        id: self.node_id_gen.new_id(),
                        kind: self.named_type_expr(name, span),
                        span,
                    };
                    sem::TypeExprKind::Refined {
                        base_ty_expr: Box::new(base_expr),
                        refinements,
                    }
                }
            }
            Type::Bool => self.named_type_expr("bool", span),
            Type::Char => self.named_type_expr("char", span),
            Type::String => self.named_type_expr("string", span),
            Type::ErrorUnion { ok_ty, err_tys } => sem::TypeExprKind::Union {
                variants: std::iter::once(ok_ty.as_ref())
                    .chain(err_tys.iter())
                    .map(|ty| self.type_expr_from_type(ty, span))
                    .collect(),
            },
            Type::Range { .. } => {
                panic!("compiler bug: range value type not representable in type expressions");
            }
            Type::Array { elem_ty, dims } => sem::TypeExprKind::Array {
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
                dims: dims.clone(),
            },
            Type::DynArray { elem_ty } => sem::TypeExprKind::DynArray {
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
            },
            Type::Set { elem_ty } => sem::TypeExprKind::Named {
                ident: "set".to_string(),
                def_id: self.def_table.lookup_type_def_id("set").unwrap_or_else(|| {
                    panic!("compiler bug: missing def id for type set at {}", span)
                }),
                type_args: vec![self.type_expr_from_type(elem_ty, span)],
            },
            Type::Map { key_ty, value_ty } => sem::TypeExprKind::Named {
                ident: "map".to_string(),
                def_id: self.def_table.lookup_type_def_id("map").unwrap_or_else(|| {
                    panic!("compiler bug: missing def id for type map at {}", span)
                }),
                type_args: vec![
                    self.type_expr_from_type(key_ty, span),
                    self.type_expr_from_type(value_ty, span),
                ],
            },
            Type::Tuple { field_tys } => sem::TypeExprKind::Tuple {
                field_ty_exprs: field_tys
                    .iter()
                    .map(|field| self.type_expr_from_type(field, span))
                    .collect(),
            },
            Type::Struct { name, .. } | Type::Enum { name, .. } => self.named_type_expr(name, span),
            Type::Slice { elem_ty } => sem::TypeExprKind::Slice {
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
            },
            Type::Heap { elem_ty } => sem::TypeExprKind::Heap {
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
            },
            Type::Ref { mutable, elem_ty } => sem::TypeExprKind::Ref {
                mutable: *mutable,
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
            },
            Type::Fn { params, ret_ty } => sem::TypeExprKind::Fn {
                params: params
                    .iter()
                    .map(|param| sem::FnTypeParam {
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

        sem::TypeExpr { id, kind, span }
    }

    fn named_type_expr(&self, name: &str, span: Span) -> sem::TypeExprKind {
        // Resolve a builtin/nominal type name into its DefId.
        let def_id = self
            .def_table
            .lookup_type_def_id(name)
            .unwrap_or_else(|| panic!("compiler bug: missing def id for type {name} at {}", span));
        sem::TypeExprKind::Named {
            ident: name.to_string(),
            def_id,
            type_args: Vec::new(),
        }
    }
}
