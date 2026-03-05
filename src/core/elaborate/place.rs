//! Place expression elaboration.
//!
//! Place expressions (lvalues) represent memory locations that can be read
//! from or written to. This module transforms normalized expressions that
//! appear in lvalue position into place-enriched AST nodes.
//!
//! Key transformation: Inside closure bodies, references to captured
//! variables are rewritten to access the closure's environment struct
//! (`env.<field>` for move captures, `*env.<field>` for borrow captures).

use crate::core::ast::{Expr, ExprKind};

use super::elaborator::Elaborator;

impl<'a> Elaborator<'a> {
    fn peel_place_base_type(&self, ty: &crate::core::types::Type) -> crate::core::types::Type {
        let mut curr = ty.clone();
        while let crate::core::types::Type::Heap { elem_ty }
        | crate::core::types::Type::Ref { elem_ty, .. } = curr
        {
            curr = (*elem_ty).clone();
        }
        curr
    }

    /// Elaborate a normalized expression into a place expression.
    ///
    /// When inside a closure body, captured variable references are
    /// rewritten to access the `env` parameter's struct fields.
    pub(super) fn elab_place(&mut self, expr: &Expr) -> Expr {
        if let ExprKind::Var { .. } = &expr.kind
            && let Some(place) =
                self.capture_place_for(self.def_table.def_id(expr.id), expr.id, expr.span)
        {
            return place;
        }
        let kind = match &expr.kind {
            ExprKind::Var { ident } => ExprKind::Var {
                ident: ident.clone(),
            },
            ExprKind::Deref { expr } => ExprKind::Deref {
                expr: Box::new(self.elab_value(expr)),
            },
            ExprKind::ArrayIndex { target, indices } => {
                let target_place = self.elab_place(target);
                let target_ty = self
                    .type_map
                    .type_table()
                    .get(self.type_id_for(target_place.id))
                    .clone();
                let plan = self.build_index_plan(&target_ty);
                self.record_index_plan(expr.id, plan);
                ExprKind::ArrayIndex {
                    target: Box::new(target_place),
                    indices: indices.iter().map(|index| self.elab_value(index)).collect(),
                }
            }
            ExprKind::TupleField { target, index } => ExprKind::TupleField {
                target: Box::new(self.elab_place(target)),
                index: *index,
            },
            ExprKind::StructField { target, field } => ExprKind::StructField {
                target: Box::new(self.elab_place(target)),
                field: field.clone(),
            },
            ExprKind::Move { expr } | ExprKind::ImplicitMove { expr } => {
                return self.elab_place(expr);
            }
            _ => {
                panic!(
                    "compiler bug: non-place expression in elab_place {:?}",
                    expr.kind
                )
            }
        };

        // Register the type for this place expression node.
        let _ty = match &expr.kind {
            ExprKind::Var { .. } => {
                // If this var is a closure binding, its type is the generated closure struct.
                self.closure_type_id_for_def(self.def_table.def_id(expr.id))
                    .unwrap_or_else(|| self.type_map.type_of(expr.id))
            }
            ExprKind::Deref { expr: inner } => {
                let inner_ty = self
                    .type_map
                    .type_table()
                    .get(self.type_map.type_of(inner.id))
                    .clone();
                match inner_ty {
                    crate::core::types::Type::Heap { elem_ty }
                    | crate::core::types::Type::Ref { elem_ty, .. } => {
                        self.insert_synth_node_type(expr.id, (*elem_ty).clone())
                    }
                    _ => self.type_map.type_of(expr.id),
                }
            }
            ExprKind::TupleField { target, index } => {
                let target_ty = self.peel_place_base_type(
                    self.type_map
                        .type_table()
                        .get(self.type_map.type_of(target.id)),
                );
                match target_ty {
                    crate::core::types::Type::Tuple { field_tys } => field_tys
                        .get(*index)
                        .cloned()
                        .map(|ty| self.insert_synth_node_type(expr.id, ty))
                        .unwrap_or_else(|| self.type_map.type_of(expr.id)),
                    _ => self.type_map.type_of(expr.id),
                }
            }
            ExprKind::StructField { target, field } => {
                let target_ty = self.peel_place_base_type(
                    self.type_map
                        .type_table()
                        .get(self.type_map.type_of(target.id)),
                );
                match target_ty {
                    crate::core::types::Type::Struct { fields, .. } => fields
                        .iter()
                        .find(|f| f.name == *field)
                        .map(|f| self.insert_synth_node_type(expr.id, f.ty.clone()))
                        .unwrap_or_else(|| self.type_map.type_of(expr.id)),
                    _ => self.type_map.type_of(expr.id),
                }
            }
            _ => self.type_map.type_of(expr.id),
        };

        Expr {
            id: expr.id,
            kind,
            span: expr.span,
        }
    }
}
