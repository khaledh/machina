//! Straight-line (single-block) lowering routines.

use crate::ssa::lower::linearize::linearize_expr;
use crate::ssa::lower::lowerer::{FuncLowerer, LinearValue, LocalValue, StmtOutcome};
use crate::ssa::lower::mapping::{map_binop, map_cmp};
use crate::ssa::model::ir::{Callee, Terminator, UnOp, ValueId};
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
                // Try arithmetic/logical binary operation.
                if let Some(binop) = map_binop(*op) {
                    let lhs = self.lower_linear_expr(left)?;
                    let rhs = self.lower_linear_expr(right)?;
                    let ty = self.type_lowerer.lower_type_id(expr.ty);
                    return Ok(self.builder.binop(binop, lhs, rhs, ty));
                }
                // Try comparison operation.
                if let Some(cmp) = map_cmp(*op) {
                    let lhs = self.lower_linear_expr(left)?;
                    let rhs = self.lower_linear_expr(right)?;
                    let ty = self.type_lowerer.lower_type_id(expr.ty);
                    return Ok(self.builder.cmp(cmp, lhs, rhs, ty));
                }
                Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr))
            }

            // Load from variable: look up current SSA value.
            sem::LinearExprKind::Load { place } => match &place.kind {
                sem::PlaceExprKind::Var { def_id, .. } => Ok(self.lookup_local(*def_id).value),
                _ => Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr)),
            },

            // Function call: resolve callee from call plan and emit call instruction.
            sem::LinearExprKind::Call { callee: _, args } => {
                // Retrieve the call plan from type-checking (contains resolved target).
                let call_plan = self.type_map.lookup_call_plan(expr.id).ok_or_else(|| {
                    self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr)
                })?;

                // Direct calls (no receiver) only for now.
                if call_plan.has_receiver {
                    return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
                }

                // Resolve the callee (only direct calls supported).
                let callee = match &call_plan.target {
                    sem::CallTarget::Direct(def_id) => Callee::Direct(*def_id),
                    sem::CallTarget::Indirect | sem::CallTarget::Intrinsic(_) => {
                        return Err(
                            self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr)
                        );
                    }
                };

                // Lower argument expressions.
                let mut arg_values = Vec::with_capacity(args.len());
                for arg in args {
                    arg_values.push(self.lower_linear_expr(arg)?);
                }

                // Apply the call plan to reorder/transform arguments.
                let call_args =
                    self.lower_call_args_from_plan(expr, &call_plan, None, &arg_values)?;
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.call(callee, call_args, ty))
            }

            // Method call: similar to function call but with a receiver.
            sem::LinearExprKind::MethodCall { receiver, args, .. } => {
                // Retrieve the call plan (contains resolved method target).
                let call_plan = self.type_map.lookup_call_plan(expr.id).ok_or_else(|| {
                    self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr)
                })?;

                // Method calls must have a receiver.
                if !call_plan.has_receiver {
                    return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
                }

                // Resolve the callee (only direct calls supported).
                let callee = match &call_plan.target {
                    sem::CallTarget::Direct(def_id) => Callee::Direct(*def_id),
                    sem::CallTarget::Indirect | sem::CallTarget::Intrinsic(_) => {
                        return Err(
                            self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr)
                        );
                    }
                };

                // Lower the receiver (value receivers only).
                let receiver_value = match receiver {
                    sem::LinearMethodReceiver::Value(expr) => self.lower_linear_expr(expr)?,
                    sem::LinearMethodReceiver::Place(_) => {
                        return Err(
                            self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr)
                        );
                    }
                };

                // Lower argument expressions.
                let mut arg_values = Vec::with_capacity(args.len());
                for arg in args {
                    arg_values.push(self.lower_linear_expr(arg)?);
                }

                // Apply the call plan to build final argument list.
                let call_args = self.lower_call_args_from_plan(
                    expr,
                    &call_plan,
                    Some(receiver_value),
                    &arg_values,
                )?;
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                Ok(self.builder.call(callee, call_args, ty))
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

    /// Builds the final argument list for a call using the call plan.
    ///
    /// The call plan specifies how source arguments map to the callee's
    /// parameter list (e.g., receiver position, argument reordering).
    fn lower_call_args_from_plan(
        &self,
        expr: &sem::LinearExpr,
        call_plan: &sem::CallPlan,
        receiver_value: Option<ValueId>,
        arg_values: &[ValueId],
    ) -> Result<Vec<ValueId>, sem::LinearizeError> {
        // Verify receiver presence matches the plan.
        if call_plan.has_receiver != receiver_value.is_some() {
            return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
        }

        // Build arguments according to the plan.
        let mut call_args = Vec::with_capacity(call_plan.args.len());
        for lowering in &call_plan.args {
            match lowering {
                sem::ArgLowering::Direct(input) => {
                    // Direct pass-through of receiver or argument.
                    let value = match input {
                        sem::CallInput::Receiver => receiver_value.unwrap_or_else(|| {
                            panic!("ssa call plan missing receiver value for {:?}", expr.id)
                        }),
                        sem::CallInput::Arg(index) => *arg_values
                            .get(*index)
                            .unwrap_or_else(|| panic!("ssa call arg index out of range: {index}")),
                    };
                    call_args.push(value);
                }
                // PtrLen lowering (for slices) not yet supported.
                sem::ArgLowering::PtrLen { .. } => {
                    return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
                }
            }
        }
        Ok(call_args)
    }
}
