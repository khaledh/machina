use crate::ast::{Expr, ExprKind as EK, NodeId};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{ExprValue, FuncLowerer};
use crate::mcir::types::*;

impl<'a> FuncLowerer<'a> {
    // --- Block Lowering ---

    /// Lower a block used as an expression.
    pub(super) fn lower_block_expr(&mut self, exprs: &[Expr]) -> Result<ExprValue, LowerError> {
        // Evaluate all but the last expression for side effects.
        self.lower_block_side_effects(exprs)?;

        match exprs.last() {
            None => {
                // Empty block: return unit
                let c = Const::Unit;
                Ok(ExprValue::Scalar(Operand::Const(c)))
            }
            Some(last_expr) => self.lower_expr_value(last_expr),
        }
    }

    /// Lower all but the last expression in a block (side effects only).
    pub(super) fn lower_block_side_effects(&mut self, exprs: &[Expr]) -> Result<(), LowerError> {
        // Block prefix: execute in order, discard results.
        for e in exprs.iter().take(exprs.len().saturating_sub(1)) {
            self.lower_block_item(e)?;
        }
        Ok(())
    }

    /// Lower a block item (stmt-like expression) for side effects.
    pub(super) fn lower_block_item(&mut self, expr: &Expr) -> Result<(), LowerError> {
        match &expr.kind {
            // Block-item-only forms.
            EK::LetBind { pattern, value, .. } => self.lower_binding(pattern, value),
            EK::VarBind { pattern, value, .. } => self.lower_binding(pattern, value),
            EK::Assign { assignee, value } => self.lower_assign(assignee, value),
            EK::While { cond, body } => {
                // while in block‑item position: just lower and discard
                self.lower_while_expr(cond, body)?;
                Ok(())
            }
            EK::For {
                pattern,
                iter,
                body,
            } => {
                // for in block‑item position: just lower and discard
                self.lower_for_expr(pattern, iter, body)?;
                Ok(())
            }
            EK::If {
                cond,
                then_body,
                else_body,
            } => {
                // if in block‑item position: just lower and discard
                let _ = self.lower_if_expr(cond, then_body, else_body)?;
                Ok(())
            }
            _ => {
                // Regular expression used for side effects only.
                let _ = self.lower_expr_value(expr)?;
                Ok(())
            }
        }
    }

    /// Lower a block and write its final aggregate value into dst.
    pub(super) fn lower_block_into(
        &mut self,
        dst: Place<Aggregate>,
        exprs: &[Expr],
        block_id: NodeId,
    ) -> Result<(), LowerError> {
        // Evaluate prefix, then write the tail into dst.
        self.lower_block_side_effects(exprs)?;

        match exprs.last() {
            Some(last_expr) => self.lower_agg_value_into(dst, last_expr),
            None => Err(LowerError::UnsupportedAggregateRhs(block_id)),
        }
    }
}
