//! Straight-line (single-block) lowering routines.

use crate::ssa::lower::lowerer::{BranchResult, FuncLowerer, LinearValue, LocalValue, StmtOutcome};
use crate::ssa::lower::mapping::{map_binop, map_cmp};
use crate::ssa::lower::{LoweringError, LoweringErrorKind};
use crate::ssa::model::ir::{Terminator, UnOp};
use crate::tree::UnaryOp;
use crate::tree::semantic as sem;

impl<'a> FuncLowerer<'a> {
    /// Lowers a linear value expression directly from the semantic tree.
    ///
    /// This avoids constructing a parallel linear AST for the common cases.
    pub(super) fn lower_value_expr_linear(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<LinearValue, LoweringError> {
        match &expr.kind {
            sem::ValueExprKind::Block { items, tail } => {
                for item in items {
                    match item {
                        sem::BlockItem::Stmt(stmt) => match self.lower_stmt_expr_linear(stmt)? {
                            StmtOutcome::Continue => {}
                            StmtOutcome::Return => {
                                return Err(self.err_stmt(stmt, LoweringErrorKind::UnsupportedStmt));
                            }
                        },
                        sem::BlockItem::Expr(expr) => {
                            let _ = self.lower_value_expr_linear(expr)?;
                        }
                    }
                }

                if let Some(tail) = tail {
                    return self.lower_value_expr_linear(tail);
                }

                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_unit(ty))
            }

            sem::ValueExprKind::UnitLit => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_unit(ty))
            }
            sem::ValueExprKind::IntLit(value) => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                let (signed, bits) = self.type_lowerer.int_info(expr.ty);
                Ok(self.builder.const_int(*value as i128, signed, bits, ty))
            }
            sem::ValueExprKind::BoolLit(value) => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_bool(*value, ty))
            }

            sem::ValueExprKind::UnaryOp { op, expr: inner } => {
                let value = self.lower_value_expr_linear(inner)?;
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                let result = match op {
                    UnaryOp::Neg => self.builder.unop(UnOp::Neg, value, ty),
                    UnaryOp::LogicalNot => self.builder.unop(UnOp::Not, value, ty),
                    UnaryOp::BitNot => self.builder.unop(UnOp::BitNot, value, ty),
                };
                Ok(result)
            }

            sem::ValueExprKind::BinOp { left, op, right } => {
                let lhs = self.lower_value_expr_linear(left)?;
                let rhs = self.lower_value_expr_linear(right)?;
                let ty = self.type_lowerer.lower_type_id(expr.ty);

                if let Some(binop) = map_binop(*op) {
                    return Ok(self.builder.binop(binop, lhs, rhs, ty));
                }
                if let Some(cmp) = map_cmp(*op) {
                    return Ok(self.builder.cmp(cmp, lhs, rhs, ty));
                }
                Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr))
            }

            sem::ValueExprKind::Load { place } => match &place.kind {
                sem::PlaceExprKind::Var { def_id, .. } => Ok(self.lookup_local(*def_id).value),
                _ => Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr)),
            },

            sem::ValueExprKind::Call { callee: _, args } => self.lower_call_expr(expr, args),

            sem::ValueExprKind::MethodCall { receiver, args, .. } => {
                self.lower_method_call_expr(expr, receiver, args)
            }

            sem::ValueExprKind::ClosureRef { .. } => {
                Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr))
            }

            _ => Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr)),
        }
    }

    /// Lowers a linear statement directly from the semantic tree.
    pub(super) fn lower_stmt_expr_linear(
        &mut self,
        stmt: &sem::StmtExpr,
    ) -> Result<StmtOutcome, LoweringError> {
        match &stmt.kind {
            sem::StmtExprKind::LetBind { pattern, value, .. }
            | sem::StmtExprKind::VarBind { pattern, value, .. } => {
                let value_expr = value;
                let value = self.lower_value_expr_linear(value_expr)?;
                let ty = self.type_lowerer.lower_type_id(value_expr.ty);
                self.bind_pattern(pattern, LocalValue { value, ty })?;
                Ok(StmtOutcome::Continue)
            }

            sem::StmtExprKind::Assign {
                assignee, value, ..
            } => {
                let value_expr = value;
                let value = self.lower_value_expr_linear(value_expr)?;
                match &assignee.kind {
                    sem::PlaceExprKind::Var { def_id, .. } => {
                        let ty = self.type_lowerer.lower_type_id(value_expr.ty);
                        self.locals.insert(*def_id, LocalValue { value, ty });
                        Ok(StmtOutcome::Continue)
                    }
                    _ => Err(self.err_stmt(stmt, LoweringErrorKind::UnsupportedStmt)),
                }
            }

            sem::StmtExprKind::Return { value } => {
                let value = match value {
                    Some(expr) => Some(self.lower_value_expr_linear(expr)?),
                    None => None,
                };
                self.builder.terminate(Terminator::Return { value });
                Ok(StmtOutcome::Return)
            }

            sem::StmtExprKind::VarDecl { .. }
            | sem::StmtExprKind::While { .. }
            | sem::StmtExprKind::For { .. }
            | sem::StmtExprKind::Break
            | sem::StmtExprKind::Continue => {
                Err(self.err_stmt(stmt, LoweringErrorKind::UnsupportedStmt))
            }
        }
    }

    /// Convenience: lower a ValueExpr and return its value in one step.
    pub(super) fn lower_linear_expr_value(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<LinearValue, LoweringError> {
        match self.lower_value_expr(expr)? {
            BranchResult::Value(value) => Ok(value),
            BranchResult::Return => {
                Err(self.err_span(expr.span, LoweringErrorKind::UnsupportedExpr))
            }
        }
    }

    /// Binds a pattern to a value, updating the locals map.
    /// Only simple name patterns are supported.
    pub(super) fn bind_pattern(
        &mut self,
        pattern: &sem::BindPattern,
        value: LocalValue,
    ) -> Result<(), LoweringError> {
        match &pattern.kind {
            sem::BindPatternKind::Name { def_id, .. } => {
                self.locals.insert(*def_id, value);
                Ok(())
            }
            _ => Err(LoweringError {
                kind: LoweringErrorKind::UnsupportedStmt,
                span: pattern.span,
            }),
        }
    }
}
