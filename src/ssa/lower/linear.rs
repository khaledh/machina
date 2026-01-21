//! Straight-line (single-block) lowering routines.

use crate::ssa::lower::linearize::linearize_expr;
use crate::ssa::lower::lowerer::{FuncLowerer, LinearValue, LocalValue, StmtOutcome};
use crate::ssa::lower::mapping::{map_binop, map_cmp};
use crate::ssa::model::ir::{BlockId, Callee, Terminator, UnOp};
use crate::tree::UnaryOp;
use crate::tree::semantic as sem;

impl<'a> FuncLowerer<'a> {
    pub(super) fn lower_linear_expr(
        &mut self,
        block: BlockId,
        expr: &sem::LinearExpr,
    ) -> Result<LinearValue, sem::LinearizeError> {
        match &expr.kind {
            sem::LinearExprKind::Block { items, tail } => {
                let mut cur_block = block;
                for item in items {
                    match item {
                        sem::LinearBlockItem::Stmt(stmt) => {
                            match self.lower_linear_stmt(cur_block, stmt)? {
                                StmtOutcome::Continue(next_block) => {
                                    cur_block = next_block;
                                }
                                StmtOutcome::Return => {
                                    return Err(self
                                        .err_stmt(stmt, sem::LinearizeErrorKind::UnsupportedStmt));
                                }
                            }
                        }
                        sem::LinearBlockItem::Expr(expr) => {
                            let _ = self.lower_linear_expr(cur_block, expr)?;
                        }
                    }
                }
                if let Some(tail) = tail {
                    return self.lower_linear_expr(cur_block, tail);
                }
                // Linear blocks without a tail evaluate to unit.
                let value = self
                    .builder
                    .const_unit(cur_block, self.ctx.ssa_type_for_type_id(expr.ty));
                Ok(value)
            }
            sem::LinearExprKind::UnitLit => Ok(self
                .builder
                .const_unit(block, self.ctx.ssa_type_for_type_id(expr.ty))),
            sem::LinearExprKind::IntLit(value) => {
                let ty = self.ctx.ssa_type_for_type_id(expr.ty);
                let (signed, bits) = self.ctx.int_info_for_type_id(expr.ty);
                Ok(self
                    .builder
                    .const_int(block, *value as i128, signed, bits, ty))
            }
            sem::LinearExprKind::BoolLit(value) => {
                Ok(self
                    .builder
                    .const_bool(block, *value, self.ctx.ssa_type_for_type_id(expr.ty)))
            }
            sem::LinearExprKind::UnaryOp { op, expr } => {
                let value = self.lower_linear_expr(block, expr)?;
                let ty = self.ctx.ssa_type_for_type_id(expr.ty);
                let value = match op {
                    UnaryOp::Neg => self.builder.unop(block, UnOp::Neg, value, ty),
                    UnaryOp::LogicalNot => self.builder.unop(block, UnOp::Not, value, ty),
                    UnaryOp::BitNot => self.builder.unop(block, UnOp::BitNot, value, ty),
                };
                Ok(value)
            }
            sem::LinearExprKind::BinOp { left, op, right } => {
                if let Some(binop) = map_binop(*op) {
                    let lhs = self.lower_linear_expr(block, left)?;
                    let rhs = self.lower_linear_expr(block, right)?;
                    let ty = self.ctx.ssa_type_for_type_id(expr.ty);
                    return Ok(self.builder.binop(block, binop, lhs, rhs, ty));
                }
                if let Some(cmp) = map_cmp(*op) {
                    let lhs = self.lower_linear_expr(block, left)?;
                    let rhs = self.lower_linear_expr(block, right)?;
                    let ty = self.ctx.ssa_type_for_type_id(expr.ty);
                    return Ok(self.builder.cmp(block, cmp, lhs, rhs, ty));
                }
                Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr))
            }
            sem::LinearExprKind::Load { place } => match &place.kind {
                sem::PlaceExprKind::Var { def_id, .. } => Ok(self.lookup_local(*def_id).value),
                _ => Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr)),
            },
            sem::LinearExprKind::Call { callee, args } => {
                // Resolve the callee to a direct target or reject unsupported forms.
                let callee = match &callee.kind {
                    sem::LinearExprKind::Load { place } => match &place.kind {
                        sem::PlaceExprKind::Var { def_id, .. } => Callee::Direct(*def_id),
                        _ => {
                            return Err(
                                self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr)
                            );
                        }
                    },
                    sem::LinearExprKind::ClosureRef { def_id } => Callee::Direct(*def_id),
                    _ => {
                        return Err(
                            self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr)
                        );
                    }
                };

                // Lower arguments in order, then emit the call instruction.
                let mut arg_values = Vec::with_capacity(args.len());
                for arg in args {
                    arg_values.push(self.lower_linear_expr(block, arg)?);
                }

                let ty = self.ctx.ssa_type_for_type_id(expr.ty);
                Ok(self.builder.call(block, callee, arg_values, ty))
            }
            sem::LinearExprKind::CharLit(_) | sem::LinearExprKind::ClosureRef { .. } => {
                Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr))
            }
        }
    }

    pub(super) fn lower_linear_expr_value(
        &mut self,
        block: BlockId,
        expr: &sem::ValueExpr,
    ) -> Result<LinearValue, sem::LinearizeError> {
        let linear = linearize_expr(expr)?;
        self.lower_linear_expr(block, &linear)
    }

    pub(super) fn lower_linear_stmt(
        &mut self,
        block: BlockId,
        stmt: &sem::LinearStmt,
    ) -> Result<StmtOutcome, sem::LinearizeError> {
        match &stmt.kind {
            sem::LinearStmtKind::LetBind { pattern, value, .. }
            | sem::LinearStmtKind::VarBind { pattern, value, .. } => {
                let value_expr = value;
                let value = self.lower_linear_expr(block, value_expr)?;
                let ty = self.ctx.ssa_type_for_type_id(value_expr.ty);
                self.bind_pattern(pattern, LocalValue { value, ty })?;
                Ok(StmtOutcome::Continue(block))
            }
            sem::LinearStmtKind::Assign { assignee, value } => {
                let value_expr = value;
                let value = self.lower_linear_expr(block, value_expr)?;
                match &assignee.kind {
                    sem::PlaceExprKind::Var { def_id, .. } => {
                        // Assignments currently model locals as SSA values.
                        let ty = self.ctx.ssa_type_for_type_id(value_expr.ty);
                        self.locals.insert(*def_id, LocalValue { value, ty });
                        Ok(StmtOutcome::Continue(block))
                    }
                    _ => Err(self.err_stmt(stmt, sem::LinearizeErrorKind::UnsupportedStmt)),
                }
            }
            sem::LinearStmtKind::Return { value } => {
                let value = match value {
                    Some(expr) => Some(self.lower_linear_expr(block, expr)?),
                    None => None,
                };
                self.builder
                    .set_terminator(block, Terminator::Return { value });
                Ok(StmtOutcome::Return)
            }
            sem::LinearStmtKind::VarDecl { .. } => {
                Err(self.err_stmt(stmt, sem::LinearizeErrorKind::UnsupportedStmt))
            }
        }
    }

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
