//! Block item and statement elaboration.
//!
//! Handles the contents of block expressions: statements and trailing
//! expressions.
//!
//! This stage preserves `for` statements as semantic `StmtExprKind::For`.
//! The dedicated `syntax_desugar` pass rewrites them into lower-level `while`
//! form later in the elaborate pipeline.

use crate::core::ast::{BindPattern, BlockItem, Expr, ExprKind, StmtExpr, StmtExprKind};
use crate::core::elaborate::elaborator::Elaborator;
use crate::core::types::Type;

impl<'a> Elaborator<'a> {
    /// Elaborate a single block item (statement or expression).
    pub(in crate::core::elaborate::value) fn elab_block_item(
        &mut self,
        item: &BlockItem,
    ) -> BlockItem {
        match item {
            BlockItem::Stmt(stmt) => BlockItem::Stmt(self.elab_stmt_expr(stmt)),
            BlockItem::Expr(expr) => BlockItem::Expr(self.elab_value(expr)),
        }
    }

    fn elab_stmt_expr(&mut self, stmt: &StmtExpr) -> StmtExpr {
        let is_let = matches!(stmt.kind, StmtExprKind::LetBind { .. });
        let kind = match &stmt.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            }
            | StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                let value = self.elab_bind_value(pattern, value);
                let bind_ty = self
                    .type_map
                    .type_table()
                    .get(self.type_id_for(value.id))
                    .clone();
                let pattern = self.elab_bind_pattern(pattern, &bind_ty);
                if is_let {
                    StmtExprKind::LetBind {
                        pattern,
                        decl_ty: decl_ty.clone(),
                        value,
                    }
                } else {
                    StmtExprKind::VarBind {
                        pattern,
                        decl_ty: decl_ty.clone(),
                        value,
                    }
                }
            }
            StmtExprKind::VarDecl { ident, decl_ty } => StmtExprKind::VarDecl {
                ident: ident.clone(),
                decl_ty: decl_ty.clone(),
            },
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                let place = self.elab_place(assignee);
                StmtExprKind::Assign {
                    assignee: Box::new(place.clone()),
                    value: Box::new(self.elab_value(value)),
                    init: self.init_info_for_id(place.id),
                }
            }
            StmtExprKind::CompoundAssign { .. } => {
                panic!("normalize must desugar compound assignment before elaborate");
            }
            StmtExprKind::While { cond, body } => StmtExprKind::While {
                cond: Box::new(self.elab_value(cond)),
                body: Box::new(self.elab_value(body)),
            },
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                let iter_value = self.elab_value(iter);
                let elem_ty = self.for_iter_elem_type(iter);
                let pattern = self.elab_bind_pattern(pattern, &elem_ty);
                StmtExprKind::For {
                    pattern,
                    iter: Box::new(iter_value),
                    body: Box::new(self.elab_value(body)),
                }
            }
            StmtExprKind::Defer { value } => StmtExprKind::Defer {
                value: Box::new(self.elab_value(value)),
            },
            StmtExprKind::Using {
                binding,
                value,
                body,
            } => StmtExprKind::Using {
                binding: binding.clone(),
                value: Box::new(self.elab_value(value)),
                body: Box::new(self.elab_value(body)),
            },
            StmtExprKind::Break => StmtExprKind::Break,
            StmtExprKind::Continue => StmtExprKind::Continue,
            StmtExprKind::Return { value } => StmtExprKind::Return {
                value: value.as_ref().map(|expr| Box::new(self.elab_value(expr))),
            },
        };

        StmtExpr {
            id: stmt.id,
            kind,
            span: stmt.span,
        }
    }

    fn for_iter_elem_type(&self, iter: &Expr) -> Type {
        let iter_ty = self
            .type_map
            .type_table()
            .get(self.type_id_for(iter.id))
            .clone();
        self.iter_elem_type_or_panic(&iter_ty, "semantic for statement elaboration")
    }

    /// Elaborate the value in a let/var binding, with special handling for closures.
    ///
    /// When binding a closure, records the mapping from the bound variable(s)
    /// to the closure's DefId so that later references can be resolved to the
    /// correct closure type.
    fn elab_bind_value(&mut self, pattern: &BindPattern, value: &Expr) -> Box<Expr> {
        if let ExprKind::Closure {
            ident,
            params,
            return_ty,
            body,
            captures: _,
        } = &value.kind
        {
            let def_id = self.def_id_for(value.id);
            if self.is_captureless_closure(def_id) {
                // Emit a top-level function for captureless closures.
                self.ensure_closure_func(
                    ident, def_id, params, return_ty, body, value.span, value.id,
                );
            } else {
                let info = self.ensure_closure_info(
                    ident, def_id, params, return_ty, body, value.span, value.id,
                );
                self.record_closure_binding(pattern, def_id, &info);
            }
        }
        Box::new(self.elab_value(value))
    }
}
