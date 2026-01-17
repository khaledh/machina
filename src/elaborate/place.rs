use crate::tree::normalized as norm;
use crate::tree::semantic as sem;

use super::elaborator::Elaborator;

impl<'a> Elaborator<'a> {
    pub(super) fn elab_place(&mut self, expr: &norm::Expr) -> sem::PlaceExpr {
        if let norm::ExprKind::Var { def_id, .. } = &expr.kind {
            // In closure bodies, rewrite captured vars to env.<field>.
            if let Some(place) = self.capture_place_for(*def_id, expr.id, expr.span) {
                return place;
            }
        }
        let kind = match &expr.kind {
            norm::ExprKind::Var { ident, def_id } => sem::PlaceExprKind::Var {
                ident: ident.clone(),
                def_id: *def_id,
            },
            norm::ExprKind::Deref { expr } => sem::PlaceExprKind::Deref {
                value: Box::new(self.elab_value(expr)),
            },
            norm::ExprKind::ArrayIndex { target, indices } => sem::PlaceExprKind::ArrayIndex {
                target: Box::new(self.elab_place(target)),
                indices: indices.iter().map(|index| self.elab_value(index)).collect(),
            },
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
