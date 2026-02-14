//! Block item and statement elaboration.
//!
//! Handles the contents of block expressions: statements and trailing
//! expressions.
//!
//! This stage preserves `for` statements as semantic `StmtExprKind::For`.
//! The dedicated `syntax_desugar` pass rewrites them into lower-level `while`
//! form later in the elaborate pipeline.

use crate::core::elaborate::elaborator::Elaborator;
use crate::core::tree::normalized as norm;
use crate::core::tree::semantic as sem;
use crate::core::types::Type;

impl<'a> Elaborator<'a> {
    /// Elaborate a single block item (statement or expression).
    pub(in crate::core::elaborate::value) fn elab_block_item(
        &mut self,
        item: &norm::BlockItem,
    ) -> sem::BlockItem {
        match item {
            norm::BlockItem::Stmt(stmt) => sem::BlockItem::Stmt(self.elab_stmt_expr(stmt)),
            norm::BlockItem::Expr(expr) => sem::BlockItem::Expr(self.elab_value(expr)),
        }
    }

    fn elab_stmt_expr(&mut self, stmt: &norm::StmtExpr) -> sem::StmtExpr {
        let is_let = matches!(stmt.kind, norm::StmtExprKind::LetBind { .. });
        let kind = match &stmt.kind {
            norm::StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            }
            | norm::StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                let value = self.elab_bind_value(pattern, value);
                let bind_ty = self.type_map.type_table().get(value.ty).clone();
                let pattern = self.elab_bind_pattern(pattern, &bind_ty);
                if is_let {
                    sem::StmtExprKind::LetBind {
                        pattern,
                        decl_ty: decl_ty.clone(),
                        value,
                    }
                } else {
                    sem::StmtExprKind::VarBind {
                        pattern,
                        decl_ty: decl_ty.clone(),
                        value,
                    }
                }
            }
            norm::StmtExprKind::VarDecl {
                ident,
                def_id,
                decl_ty,
            } => sem::StmtExprKind::VarDecl {
                ident: ident.clone(),
                def_id: *def_id,
                decl_ty: decl_ty.clone(),
            },
            norm::StmtExprKind::Assign {
                assignee, value, ..
            } => {
                let place = self.elab_place(assignee);
                sem::StmtExprKind::Assign {
                    assignee: Box::new(place.clone()),
                    value: Box::new(self.elab_value(value)),
                    init: self.init_info_for_id(place.id),
                }
            }
            norm::StmtExprKind::While { cond, body } => sem::StmtExprKind::While {
                cond: Box::new(self.elab_value(cond)),
                body: Box::new(self.elab_value(body)),
            },
            norm::StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                let iter_value = self.elab_value(iter);
                let elem_ty = self.for_iter_elem_type(iter);
                let pattern = self.elab_bind_pattern(pattern, &elem_ty);
                sem::StmtExprKind::For {
                    pattern,
                    iter: Box::new(iter_value),
                    body: Box::new(self.elab_value(body)),
                }
            }
            norm::StmtExprKind::Break => sem::StmtExprKind::Break,
            norm::StmtExprKind::Continue => sem::StmtExprKind::Continue,
            norm::StmtExprKind::Return { value } => sem::StmtExprKind::Return {
                value: value.as_ref().map(|expr| Box::new(self.elab_value(expr))),
            },
        };

        sem::StmtExpr {
            id: stmt.id,
            kind,
            span: stmt.span,
        }
    }

    fn for_iter_elem_type(&self, iter: &norm::Expr) -> Type {
        let iter_ty = self.type_map.type_table().get(iter.ty).clone();
        self.iter_elem_type_or_panic(&iter_ty, "semantic for statement elaboration")
    }

    /// Elaborate the value in a let/var binding, with special handling for closures.
    ///
    /// When binding a closure, records the mapping from the bound variable(s)
    /// to the closure's DefId so that later references can be resolved to the
    /// correct closure type.
    fn elab_bind_value(
        &mut self,
        pattern: &norm::BindPattern,
        value: &norm::Expr,
    ) -> Box<sem::ValueExpr> {
        if let norm::ExprKind::Closure {
            ident,
            def_id,
            params,
            return_ty,
            body,
            captures: _,
        } = &value.kind
        {
            if self.is_captureless_closure(*def_id) {
                // Emit a top-level function for captureless closures.
                self.ensure_closure_func(
                    ident, *def_id, params, return_ty, body, value.span, value.id,
                );
            } else {
                let info = self.ensure_closure_info(
                    ident, *def_id, params, return_ty, body, value.span, value.id,
                );
                self.record_closure_binding(pattern, *def_id, &info);
            }
        }
        Box::new(self.elab_value(value))
    }
}
