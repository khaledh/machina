//! Statement and block-item constraint collection.

use super::*;

impl<'a> ConstraintCollector<'a> {
    pub(super) fn collect_block_item(&mut self, item: &BlockItem) {
        match item {
            BlockItem::Stmt(stmt) => self.collect_stmt(stmt),
            BlockItem::Expr(expr) => {
                let _ = self.collect_expr(expr, None);
            }
        }
    }

    pub(super) fn collect_stmt(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
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
                // For explicitly typed declarations, bind pattern variables to
                // the declared type (not the raw value type) so refinements are
                // preserved and runtime checks can be emitted downstream.
                let expected_decl_ty = decl_ty
                    .as_ref()
                    .and_then(|decl_ty| self.resolve_type_in_scope(decl_ty).ok());
                let value_ty = self.collect_expr(value, expected_decl_ty.clone());
                if let Some(decl_ty) = expected_decl_ty.clone() {
                    self.push_assignable(
                        value_ty.clone(),
                        decl_ty.clone(),
                        ConstraintReason::Stmt(stmt.id, stmt.span),
                    );
                    self.collect_bind_pattern(pattern, decl_ty);
                } else {
                    self.collect_bind_pattern(pattern, value_ty);
                }
            }
            StmtExprKind::VarDecl { decl_ty, .. } => {
                let def_id = self.ctx.def_table.def_id(stmt.id);
                let def_term = self.def_term(def_id);
                if let Ok(ty) = self.resolve_type_in_scope(decl_ty) {
                    self.push_eq(def_term, ty, ConstraintReason::Decl(def_id, stmt.span));
                }
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                // Property/field assignment has dedicated obligations so setter
                // accessibility and field-level typing can be checked precisely.
                if let ExprKind::StructField { target, field } = &assignee.kind {
                    let target_ty = self.collect_expr(target, None);
                    let assignee_ty = self.node_term(assignee.id);
                    let value_ty = self.collect_expr(value, None);
                    self.out
                        .expr_obligations
                        .push(ExprObligation::StructFieldAssign {
                            stmt_id: stmt.id,
                            assignee_expr_id: assignee.id,
                            target: target_ty,
                            field: field.clone(),
                            assignee: assignee_ty,
                            value: value_ty,
                            caller_def_id: self.current_callable_def_id(),
                            span: stmt.span,
                        });
                } else if let ExprKind::ArrayIndex { target, .. } = &assignee.kind {
                    let target_ty = self.collect_expr(target, None);
                    self.out
                        .expr_obligations
                        .push(ExprObligation::MapIndexAssign {
                            stmt_id: stmt.id,
                            target: target_ty,
                            span: stmt.span,
                        });

                    let lhs_ty = self.collect_expr(assignee, None);
                    let rhs_ty = self.collect_expr(value, Some(lhs_ty.clone()));
                    self.push_assignable(
                        rhs_ty,
                        lhs_ty,
                        ConstraintReason::Stmt(stmt.id, stmt.span),
                    );
                } else {
                    let lhs_ty = self.collect_expr(assignee, None);
                    let rhs_ty = self.collect_expr(value, Some(lhs_ty.clone()));
                    self.push_assignable(
                        rhs_ty,
                        lhs_ty,
                        ConstraintReason::Stmt(stmt.id, stmt.span),
                    );
                }
            }
            StmtExprKind::CompoundAssign {
                assignee,
                op,
                value,
                ..
            } => {
                // Compound assignment is typed as a read-modify-write:
                // 1) read assignee,
                // 2) apply binary op with rhs,
                // 3) assign op result back into assignee.
                let lhs_ty = self.collect_expr(assignee, None);
                let rhs_ty = self.collect_expr(value, Some(lhs_ty.clone()));
                let result_ty = self.fresh_var_term();
                self.out.expr_obligations.push(ExprObligation::BinOp {
                    expr_id: stmt.id,
                    op: *op,
                    left: lhs_ty.clone(),
                    right: rhs_ty,
                    result: result_ty.clone(),
                    span: stmt.span,
                });

                if let ExprKind::StructField { target, field } = &assignee.kind {
                    let target_ty = self.collect_expr(target, None);
                    let assignee_ty = self.node_term(assignee.id);
                    self.out
                        .expr_obligations
                        .push(ExprObligation::StructFieldAssign {
                            stmt_id: stmt.id,
                            assignee_expr_id: assignee.id,
                            target: target_ty,
                            field: field.clone(),
                            assignee: assignee_ty,
                            value: result_ty.clone(),
                            caller_def_id: self.current_callable_def_id(),
                            span: stmt.span,
                        });
                } else if let ExprKind::ArrayIndex { target, .. } = &assignee.kind {
                    let target_ty = self.collect_expr(target, None);
                    self.out
                        .expr_obligations
                        .push(ExprObligation::MapIndexAssign {
                            stmt_id: stmt.id,
                            target: target_ty,
                            span: stmt.span,
                        });
                }

                self.push_assignable(
                    result_ty,
                    lhs_ty,
                    ConstraintReason::Stmt(stmt.id, stmt.span),
                );
            }
            StmtExprKind::While { cond, body } => {
                self.collect_expr(cond, Some(Type::Bool));
                self.enter_loop();
                self.collect_expr(body, Some(Type::Unit));
                self.exit_loop();
            }
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                let iter_ty = self.collect_expr(iter, None);
                let pattern_ty = self.fresh_var_term();
                self.collect_bind_pattern(pattern, pattern_ty.clone());
                self.out.expr_obligations.push(ExprObligation::ForIter {
                    stmt_id: stmt.id,
                    iter: iter_ty,
                    pattern: pattern_ty,
                    span: stmt.span,
                });
                self.enter_loop();
                self.collect_expr(body, Some(Type::Unit));
                self.exit_loop();
            }
            StmtExprKind::Defer { value } => {
                let _ = self.collect_expr(value, None);
            }
            StmtExprKind::Using { value, body, .. } => {
                // `using name = expr { ... }` introduces a scoped immutable
                // binding whose type matches the initializer.
                let value_ty = self.collect_expr(value, None);
                let def_id = self.ctx.def_table.def_id(stmt.id);
                let def_term = self.def_term(def_id);
                self.push_eq(
                    def_term,
                    value_ty,
                    ConstraintReason::Stmt(stmt.id, stmt.span),
                );
                self.collect_expr(body, Some(Type::Unit));
            }
            StmtExprKind::Break => {
                self.out.control_facts.push(ControlFact::Break {
                    stmt_id: stmt.id,
                    loop_depth: self.current_loop_depth(),
                    span: stmt.span,
                });
            }
            StmtExprKind::Continue => {
                self.out.control_facts.push(ControlFact::Continue {
                    stmt_id: stmt.id,
                    loop_depth: self.current_loop_depth(),
                    span: stmt.span,
                });
            }
            StmtExprKind::Return { value } => {
                let expected_return = self.current_return_ty();
                if let Some(value) = value {
                    let value_expected = if matches!(expected_return, Some(Type::Unit)) {
                        None
                    } else {
                        expected_return.clone()
                    };
                    let value_ty = self.collect_expr(value, value_expected);
                    if let Some(expected_return) = expected_return.clone()
                        && !matches!(expected_return, Type::Unit)
                    {
                        self.push_assignable(
                            value_ty,
                            expected_return,
                            ConstraintReason::Return(stmt.id, stmt.span),
                        );
                    }
                }
                self.out.control_facts.push(ControlFact::Return {
                    stmt_id: stmt.id,
                    has_value: value.is_some(),
                    expected_return_ty: expected_return,
                    loop_depth: self.current_loop_depth(),
                    span: stmt.span,
                });
            }
        }
    }

    pub(super) fn block_has_explicit_return(&self, items: &[BlockItem]) -> bool {
        items.iter().any(|item| match item {
            BlockItem::Stmt(stmt) => self.stmt_has_return(stmt),
            BlockItem::Expr(expr) => self.expr_has_return(expr),
        })
    }

    pub(super) fn stmt_has_return(&self, stmt: &StmtExpr) -> bool {
        match &stmt.kind {
            StmtExprKind::Return { .. } => true,
            StmtExprKind::While { body, .. } | StmtExprKind::For { body, .. } => {
                self.expr_has_return(body)
            }
            StmtExprKind::LetBind { value, .. }
            | StmtExprKind::VarBind { value, .. }
            | StmtExprKind::Assign { value, .. }
            | StmtExprKind::CompoundAssign { value, .. }
            | StmtExprKind::Defer { value, .. } => self.expr_has_return(value),
            StmtExprKind::Using { value, body, .. } => {
                self.expr_has_return(value) || self.expr_has_return(body)
            }
            _ => false,
        }
    }

    pub(super) fn expr_has_return(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Block { items, tail } => {
                self.block_has_explicit_return(items)
                    || tail
                        .as_ref()
                        .is_some_and(|tail_expr| self.expr_has_return(tail_expr))
            }
            ExprKind::If {
                then_body,
                else_body,
                ..
            } => self.expr_has_return(then_body) || self.expr_has_return(else_body),
            ExprKind::Match { arms, .. } => arms.iter().any(|arm| self.expr_has_return(&arm.body)),
            // Do not recurse into nested closures; their returns are local to them.
            ExprKind::Closure { .. } => false,
            _ => false,
        }
    }
}
