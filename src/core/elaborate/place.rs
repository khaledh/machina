//! Place expression elaboration.
//!
//! Place expressions (lvalues) represent memory locations that can be read
//! from or written to. This module transforms normalized expressions that
//! appear in lvalue position into explicit `PlaceExpr` nodes.
//!
//! Key transformation: Inside closure bodies, references to captured
//! variables are rewritten to access the closure's environment struct
//! (`env.<field>` for move captures, `*env.<field>` for borrow captures).

use crate::core::tree as ast;
use crate::core::tree::semantic as sem;

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
    pub(super) fn elab_place(&mut self, expr: &ast::Expr) -> sem::PlaceExpr {
        if let ast::ExprKind::Var { .. } = &expr.kind
            && let Some(place) =
                self.capture_place_for(self.def_table.def_id(expr.id), expr.id, expr.span)
        {
            return place;
        }
        let kind = match &expr.kind {
            ast::ExprKind::Var { ident } => sem::PlaceExprKind::Var {
                ident: ident.clone(),
                def_id: self.def_table.def_id(expr.id),
            },
            ast::ExprKind::Deref { expr } => sem::PlaceExprKind::Deref {
                value: Box::new(self.elab_value(expr)),
            },
            ast::ExprKind::ArrayIndex { target, indices } => {
                let target_place = self.elab_place(target);
                let target_ty = self.type_map.type_table().get(target_place.ty).clone();
                let plan = self.build_index_plan(&target_ty);
                self.record_index_plan(expr.id, plan);
                sem::PlaceExprKind::ArrayIndex {
                    target: Box::new(target_place),
                    indices: indices.iter().map(|index| self.elab_value(index)).collect(),
                }
            }
            ast::ExprKind::TupleField { target, index } => sem::PlaceExprKind::TupleField {
                target: Box::new(self.elab_place(target)),
                index: *index,
            },
            ast::ExprKind::StructField { target, field } => sem::PlaceExprKind::StructField {
                target: Box::new(self.elab_place(target)),
                field: field.clone(),
            },
            ast::ExprKind::Move { expr } | ast::ExprKind::ImplicitMove { expr } => {
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
            ast::ExprKind::Var { .. } => {
                // If this var is a closure binding, its type is the generated closure struct.
                self.closure_type_id_for_def(self.def_table.def_id(expr.id))
                    .unwrap_or_else(|| self.type_map.type_of(expr.id))
            }
            ast::ExprKind::Deref { expr: inner } => {
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
            ast::ExprKind::TupleField { target, index } => {
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
            ast::ExprKind::StructField { target, field } => {
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

        sem::PlaceExpr {
            id: expr.id,
            kind,
            ty,
            span: expr.span,
        }
    }
}
