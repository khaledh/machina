use crate::ast::{BlockItem, Expr, NodeId, StmtExpr, StmtExprKind};
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
        // Evaluate all but the last expression for side effects.
        for item in items {
            self.lower_block_item(item)?;
        }

        match tail {
            Some(expr) => self.lower_expr_value(expr),
            None => Ok(ExprValue::Scalar(Operand::Const(Const::Unit))),
        }
    }

    /// Lower a block and write its final aggregate value into dst.
    pub(super) fn lower_block_into(
        &mut self,
        dst: Place<Aggregate>,
        items: &[BlockItem],
        tail: Option<&Expr>,
        block_id: NodeId,
    ) -> Result<(), LowerError> {
        // Evaluate prefix, then write the tail into dst.
        for item in items {
            self.lower_block_item(item)?;
        }

        match tail {
            Some(expr) => self.lower_agg_value_into(dst, expr),
            None => Err(LowerError::UnsupportedAggregateRhs(block_id)),
        }
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
}
