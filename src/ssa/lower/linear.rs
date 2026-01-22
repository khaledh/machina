//! Straight-line (single-block) lowering routines.

use crate::ssa::lower::linearize::linearize_expr;
use crate::ssa::lower::lowerer::{FuncLowerer, LinearValue, LocalValue, StmtOutcome};
use crate::ssa::lower::mapping::{map_binop, map_cmp};
use crate::ssa::model::ir::{Terminator, UnOp};
use crate::tree::UnaryOp;
use crate::tree::semantic as sem;

impl<'a> FuncLowerer<'a> {
    /// Lowers a linear expression at the current block cursor.
    ///
    /// Linear expressions don't introduce control flow, so all instructions
    /// are emitted to the current block without changing the cursor.
    pub(super) fn lower_linear_expr(
        &mut self,
        expr: &sem::LinearExpr,
    ) -> Result<LinearValue, sem::LinearizeError> {
        match &expr.kind {
            // Block: process items sequentially, return tail value or unit.
            sem::LinearExprKind::Block { items, tail } => {
                for item in items {
                    match item {
                        sem::LinearBlockItem::Stmt(stmt) => match self.lower_linear_stmt(stmt)? {
                            StmtOutcome::Continue => {}
                            StmtOutcome::Return => {
                                return Err(
                                    self.err_stmt(stmt, sem::LinearizeErrorKind::UnsupportedStmt)
                                );
                            }
                        },
                        sem::LinearBlockItem::Expr(expr) => {
                            // Side-effect only; discard the value.
                            let _ = self.lower_linear_expr(expr)?;
                        }
                    }
                }
                if let Some(tail) = tail {
                    return self.lower_linear_expr(tail);
                }
                // No tail: produce unit.
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_unit(ty))
            }

            // Literals: emit constant instructions.
            sem::LinearExprKind::UnitLit => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_unit(ty))
            }
            sem::LinearExprKind::IntLit(value) => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                let (signed, bits) = self.type_lowerer.int_info(expr.ty);
                Ok(self.builder.const_int(*value as i128, signed, bits, ty))
            }
            sem::LinearExprKind::BoolLit(value) => {
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.const_bool(*value, ty))
            }

            // Unary operators: lower operand and emit unary instruction.
            sem::LinearExprKind::UnaryOp { op, expr: inner } => {
                let value = self.lower_linear_expr(inner)?;
                let ty = self.type_lowerer.lower_type_id(inner.ty);
                let result = match op {
                    UnaryOp::Neg => self.builder.unop(UnOp::Neg, value, ty),
                    UnaryOp::LogicalNot => self.builder.unop(UnOp::Not, value, ty),
                    UnaryOp::BitNot => self.builder.unop(UnOp::BitNot, value, ty),
                };
                Ok(result)
            }

            // Binary operators: lower operands and emit binop/cmp instruction.
            sem::LinearExprKind::BinOp { left, op, right } => {
                let lhs = self.lower_linear_expr(left)?;
                let rhs = self.lower_linear_expr(right)?;
                let ty = self.type_lowerer.lower_type_id(expr.ty);

                // Try arithmetic/logical binary operation.
                if let Some(binop) = map_binop(*op) {
                    return Ok(self.builder.binop(binop, lhs, rhs, ty));
                }
                // Try comparison operation.
                if let Some(cmp) = map_cmp(*op) {
                    return Ok(self.builder.cmp(cmp, lhs, rhs, ty));
                }
                Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr))
            }

            // Load from variable: look up current SSA value.
            sem::LinearExprKind::Load { place } => match &place.kind {
                sem::PlaceExprKind::Var { def_id, .. } => Ok(self.lookup_local(*def_id).value),
                _ => Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr)),
            },

            sem::LinearExprKind::Call { callee: _, args } => self.lower_call_expr(expr, args),

            sem::LinearExprKind::MethodCall { receiver, args, .. } => {
                self.lower_method_call_expr(expr, receiver, args)
            }

            sem::LinearExprKind::CharLit(_) | sem::LinearExprKind::ClosureRef { .. } => {
                Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr))
            }
        }
    }

    /// Convenience: linearize a ValueExpr and lower it in one step.
    pub(super) fn lower_linear_expr_value(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<LinearValue, sem::LinearizeError> {
        let linear = linearize_expr(expr)?;
        self.lower_linear_expr(&linear)
    }

    /// Lowers a linear statement, emitting instructions and updating locals.
    pub(super) fn lower_linear_stmt(
        &mut self,
        stmt: &sem::LinearStmt,
    ) -> Result<StmtOutcome, sem::LinearizeError> {
        match &stmt.kind {
            // Let/var binding: lower the value and bind the pattern.
            sem::LinearStmtKind::LetBind { pattern, value, .. }
            | sem::LinearStmtKind::VarBind { pattern, value, .. } => {
                let value_expr = value;
                let value = self.lower_linear_expr(value_expr)?;
                let ty = self.type_lowerer.lower_type_id(value_expr.ty);
                self.bind_pattern(pattern, LocalValue { value, ty })?;
                Ok(StmtOutcome::Continue)
            }

            // Assignment: lower the value and update the local's SSA value.
            sem::LinearStmtKind::Assign { assignee, value } => {
                let value_expr = value;
                let value = self.lower_linear_expr(value_expr)?;
                match &assignee.kind {
                    sem::PlaceExprKind::Var { def_id, .. } => {
                        // Update the local to the new SSA value.
                        let ty = self.type_lowerer.lower_type_id(value_expr.ty);
                        self.locals.insert(*def_id, LocalValue { value, ty });
                        Ok(StmtOutcome::Continue)
                    }
                    _ => Err(self.err_stmt(stmt, sem::LinearizeErrorKind::UnsupportedStmt)),
                }
            }

            // Return: emit return terminator.
            sem::LinearStmtKind::Return { value } => {
                let value = match value {
                    Some(expr) => Some(self.lower_linear_expr(expr)?),
                    None => None,
                };
                self.builder.terminate(Terminator::Return { value });
                Ok(StmtOutcome::Return)
            }

            // Variable declaration without initializer: not yet supported.
            sem::LinearStmtKind::VarDecl { .. } => {
                Err(self.err_stmt(stmt, sem::LinearizeErrorKind::UnsupportedStmt))
            }
        }
    }

    /// Binds a pattern to a value, updating the locals map.
    /// Only simple name patterns are supported.
    pub(super) fn bind_pattern(
        &mut self,
        pattern: &sem::BindPattern,
        value: LocalValue,
    ) -> Result<(), sem::LinearizeError> {
        match &pattern.kind {
            sem::BindPatternKind::Name { def_id, .. } => {
                self.locals.insert(*def_id, value);
                Ok(())
            }
            _ => Err(sem::LinearizeError {
                kind: sem::LinearizeErrorKind::UnsupportedStmt,
                span: pattern.span,
            }),
        }
    }
}
