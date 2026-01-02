use crate::ast::{BlockItem, Expr, ExprKind, NodeId, StmtExpr, StmtExprKind};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{ExprValue, FuncLowerer};
use crate::mcir::types::*;

impl<'a> FuncLowerer<'a> {
    // --- Block Lowering ---

    /// Lower a block used as an expression.
    pub(super) fn lower_block_expr(
        &mut self,
        items: &[BlockItem],
        tail: Option<&Expr>,
    ) -> Result<ExprValue, LowerError> {
        // Each block introduces a new drop scope for owned locals.
        self.enter_drop_scope();

        // Evaluate all but the last expression for side effects.
        for item in items {
            self.lower_block_item(item)?;
        }

        let result = match tail {
            Some(expr) => self.lower_expr_value(expr),
            None => Ok(ExprValue::Scalar(Operand::Const(Const::Unit))),
        };

        // Drop owned locals before leaving the block.
        self.exit_drop_scope()?;
        result
    }

    /// Lower a block and write its final aggregate value into dst.
    pub(super) fn lower_block_into(
        &mut self,
        dst: Place<Aggregate>,
        items: &[BlockItem],
        tail: Option<&Expr>,
        block_id: NodeId,
    ) -> Result<(), LowerError> {
        // Each block introduces a new drop scope for owned locals.
        self.enter_drop_scope();

        // Evaluate prefix, then write the tail into dst.
        for item in items {
            self.lower_block_item(item)?;
        }

        let result = match tail {
            Some(expr) => self.lower_agg_value_into(dst, expr),
            None => Err(LowerError::UnsupportedAggregateRhs(block_id)),
        };

        // Drop owned locals before leaving the block.
        self.exit_drop_scope()?;
        result
    }

    /// Lower a block item (stmt-like expression) for side effects.
    pub(super) fn lower_block_item(&mut self, item: &BlockItem) -> Result<(), LowerError> {
        match item {
            BlockItem::Stmt(stmt) => self.lower_stmt_expr(stmt),
            BlockItem::Expr(expr) => self.lower_expr_value(expr).map(|_| ()),
        }
    }

    pub(super) fn lower_stmt_expr(&mut self, stmt: &StmtExpr) -> Result<(), LowerError> {
        match &stmt.kind {
            StmtExprKind::LetBind { pattern, value, .. } => self.lower_binding(pattern, value),
            StmtExprKind::VarBind { pattern, value, .. } => self.lower_binding(pattern, value),
            StmtExprKind::Assign { assignee, value } => self.lower_assign(assignee, value),
            StmtExprKind::While { cond, body } => self.lower_while_expr(cond, body).map(|_| ()),
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => self.lower_for_expr(pattern, iter, body).map(|_| ()),
        }
    }

    /// Lower an assignment expression.
    pub(super) fn lower_assign(&mut self, assignee: &Expr, value: &Expr) -> Result<(), LowerError> {
        let value_ty = self.ty_for_node(value.id)?;

        if value_ty.is_scalar() {
            // Scalar assignment.
            let assignee_place = self.lower_place_scalar(assignee)?;
            let value_operand = self.lower_scalar_expr(value)?;
            let target_ty = self.ty_for_node(assignee.id)?;
            self.emit_conversion_check(&value_ty, &target_ty, &value_operand);

            if target_ty.needs_drop() {
                // Overwrite semantics: free the old heap pointer first.
                self.emit_drop_place(PlaceAny::Scalar(assignee_place.clone()), &target_ty);
                if let ExprKind::Var(_) = assignee.kind {
                    if let Ok(def) = self.def_for_node(assignee.id) {
                        self.clear_moved(def.id);
                    }
                }
            }

            self.emit_copy_scalar(assignee_place, Rvalue::Use(value_operand));
        } else {
            // Aggregate assignment (value written directly into place).
            let assignee_place = self.lower_place_agg(assignee)?;

            // Overwrite semantics: free the old heap pointer first.
            let target_ty = self.ty_for_node(assignee.id)?;
            if target_ty.needs_drop() {
                self.emit_drop_place(PlaceAny::Aggregate(assignee_place.clone()), &target_ty);
            }

            // Write the new value into the assignee place.
            self.lower_agg_value_into(assignee_place, value)?;
        }

        Ok(())
    }
}
