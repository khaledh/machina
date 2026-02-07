//! Place expression elaboration.
//!
//! Place expressions (lvalues) represent memory locations that can be read
//! from or written to. This module transforms normalized expressions that
//! appear in lvalue position into explicit `PlaceExpr` nodes.
//!
//! Key transformation: Inside closure bodies, references to captured
//! variables are rewritten to access the closure's environment struct
//! (`env.<field>` for move captures, `*env.<field>` for borrow captures).

use crate::tree::normalized as norm;
use crate::tree::semantic as sem;

use super::elaborator::Elaborator;

impl<'a> Elaborator<'a> {
    fn peel_place_base_type(&self, ty: &crate::types::Type) -> crate::types::Type {
        let mut curr = ty.clone();
        while let crate::types::Type::Heap { elem_ty } | crate::types::Type::Ref { elem_ty, .. } =
            curr
        {
            curr = (*elem_ty).clone();
        }
        curr
    }

    /// Elaborate a normalized expression into a place expression.
    ///
    /// When inside a closure body, captured variable references are
    /// rewritten to access the `env` parameter's struct fields.
    pub(super) fn elab_place(&mut self, expr: &norm::Expr) -> sem::PlaceExpr {
        if let norm::ExprKind::Var { def_id, .. } = &expr.kind
            && let Some(place) = self.capture_place_for(*def_id, expr.id, expr.span)
        {
            return place;
        }
        let kind = match &expr.kind {
            norm::ExprKind::Var { ident, def_id } => sem::PlaceExprKind::Var {
                ident: ident.clone(),
                def_id: *def_id,
            },
            norm::ExprKind::Deref { expr } => sem::PlaceExprKind::Deref {
                value: Box::new(self.elab_value(expr)),
            },
            norm::ExprKind::ArrayIndex { target, indices } => {
                let target_place = self.elab_place(target);
                let target_ty = self.type_map.type_table().get(target_place.ty).clone();
                let plan = self.build_index_plan(&target_ty);
                self.type_map.insert_index_plan(expr.id, plan);
                sem::PlaceExprKind::ArrayIndex {
                    target: Box::new(target_place),
                    indices: indices.iter().map(|index| self.elab_value(index)).collect(),
                }
            }
            norm::ExprKind::TupleField { target, index } => sem::PlaceExprKind::TupleField {
                target: Box::new(self.elab_place(target)),
                index: *index,
            },
            norm::ExprKind::StructField { target, field } => sem::PlaceExprKind::StructField {
                target: Box::new(self.elab_place(target)),
                field: field.clone(),
            },
            norm::ExprKind::Move { expr } | norm::ExprKind::ImplicitMove { expr } => {
                return self.elab_place(expr);
            }
            _ => {
                panic!(
                    "compiler bug: non-place expression in elab_place {:?}",
                    expr.kind
                )
            }
        };

        let ty = match &expr.kind {
            norm::ExprKind::Var { def_id, .. } => {
                // If this var is a closure binding, its type is the generated closure struct.
                self.closure_type_id_for_def(*def_id).unwrap_or(expr.ty)
            }
            norm::ExprKind::Deref { expr: inner } => {
                let inner_ty = self.type_map.type_table().get(inner.ty).clone();
                match inner_ty {
                    crate::types::Type::Heap { elem_ty }
                    | crate::types::Type::Ref { elem_ty, .. } => {
                        self.type_map.insert_node_type(expr.id, (*elem_ty).clone())
                    }
                    _ => expr.ty,
                }
            }
            norm::ExprKind::TupleField { target, index } => {
                let target_ty =
                    self.peel_place_base_type(self.type_map.type_table().get(target.ty));
                match target_ty {
                    crate::types::Type::Tuple { field_tys } => field_tys
                        .get(*index)
                        .cloned()
                        .map(|ty| self.type_map.insert_node_type(expr.id, ty))
                        .unwrap_or(expr.ty),
                    _ => expr.ty,
                }
            }
            norm::ExprKind::StructField { target, field } => {
                let target_ty =
                    self.peel_place_base_type(self.type_map.type_table().get(target.ty));
                match target_ty {
                    crate::types::Type::Struct { fields, .. } => fields
                        .iter()
                        .find(|f| f.name == *field)
                        .map(|f| self.type_map.insert_node_type(expr.id, f.ty.clone()))
                        .unwrap_or(expr.ty),
                    _ => expr.ty,
                }
            }
            _ => expr.ty,
        };

        sem::PlaceExpr {
            id: expr.id,
            kind,
            ty,
            span: expr.span,
        }
    }
}
