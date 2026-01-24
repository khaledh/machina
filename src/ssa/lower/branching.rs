//! Branching (multi-block) lowering routines.

use crate::resolve::DefId;
use crate::ssa::IrTypeId;
use crate::ssa::lower::lowerer::{BranchResult, FuncLowerer, LoopContext, StmtOutcome};
use crate::ssa::lower::r#match::MatchLowerer;
use crate::ssa::lower::{LoweringError, LoweringErrorKind};
use crate::ssa::model::ir::{Terminator, ValueId};
use crate::tree::{BinaryOp, semantic as sem};

impl<'a, 'g> FuncLowerer<'a, 'g> {
    /// Lowers a branching expression, potentially creating multiple basic blocks.
    ///
    /// Emits instructions starting at the current block cursor. May create new blocks
    /// and switch the cursor. On return, the cursor is at the "ending block" where
    /// execution continues after this expression.
    pub(super) fn lower_branching_value_expr(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<BranchResult, LoweringError> {
        match &expr.kind {
            // Block expression: process items sequentially.
            sem::ValueExprKind::Block { items, tail } => {
                for item in items {
                    match item {
                        sem::BlockItem::Stmt(stmt) => {
                            if let Some(result) = self.lower_stmt_expr_branching(stmt)? {
                                return Ok(result);
                            }
                        }
                        sem::BlockItem::Expr(expr) => {
                            // Statement-position expression: lower using the plan.
                            match self.lower_value_expr(expr)? {
                                BranchResult::Value(_) => {}
                                BranchResult::Return => {
                                    return Ok(BranchResult::Return);
                                }
                            }
                        }
                    }
                }

                // Lower the tail expression if present.
                if let Some(tail) = tail {
                    return self.lower_value_expr(tail);
                }

                // Blocks without a tail produce unit.
                let ty = self.type_lowerer.lower_type_id(expr.ty);
                let value = self.builder.const_unit(ty);
                Ok(BranchResult::Value(value))
            }

            // If expression: creates then/else blocks and a join block.
            sem::ValueExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                // Lower the condition in the current block.
                let cond_value = self.lower_linear_expr_value(cond)?;

                // Create the control-flow structure: then block, else block, join block.
                let then_bb = self.builder.add_block();
                let else_bb = self.builder.add_block();
                let join = self.begin_join(expr);

                // Emit the conditional branch from current block.
                self.builder.terminate(Terminator::CondBr {
                    cond: cond_value,
                    then_bb,
                    then_args: Vec::new(),
                    else_bb,
                    else_args: Vec::new(),
                });

                // Lower the then branch.
                self.builder.select_block(then_bb);
                let mut then_returned = false;
                match self.lower_branching_value_expr(then_body)? {
                    BranchResult::Value(value) => {
                        // Cursor is at end of then branch; emit branch to join.
                        join.emit_branch(self, value, expr.span)?;
                    }
                    BranchResult::Return => {
                        then_returned = true;
                    }
                }

                // Lower the else branch (start from saved locals snapshot).
                join.restore_locals(self);
                self.builder.select_block(else_bb);
                let mut else_returned = false;
                match self.lower_branching_value_expr(else_body)? {
                    BranchResult::Value(value) => {
                        // Cursor is at end of else branch; emit branch to join.
                        join.emit_branch(self, value, expr.span)?;
                    }
                    BranchResult::Return => {
                        else_returned = true;
                    }
                }

                // If both branches return, the if expression never produces a value.
                if then_returned && else_returned {
                    return Ok(BranchResult::Return);
                }

                // Select join block and install its parameters as the new local values.
                let join_value = join.join_value();
                join.finalize(self);
                Ok(BranchResult::Value(join_value))
            }

            sem::ValueExprKind::BinOp { left, op, right }
                if matches!(op, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) =>
            {
                // Short-circuit lowering: emit a conditional branch based on the LHS.
                let lhs = match self.lower_value_expr(left)? {
                    BranchResult::Value(value) => value,
                    BranchResult::Return => return Ok(BranchResult::Return),
                };

                // Create control-flow structure for RHS evaluation vs. short-circuit.
                let rhs_bb = self.builder.add_block();
                let short_bb = self.builder.add_block();
                let join = self.begin_join(expr);

                // AND: true evaluates RHS, false short-circuits.
                // OR: false evaluates RHS, true short-circuits.
                let (then_bb, else_bb, short_val) = match op {
                    BinaryOp::LogicalAnd => (rhs_bb, short_bb, false),
                    BinaryOp::LogicalOr => (short_bb, rhs_bb, true),
                    _ => unreachable!("ssa logical op dispatch missed: {op:?}"),
                };

                // Branch on the LHS value.
                self.builder.terminate(Terminator::CondBr {
                    cond: lhs,
                    then_bb,
                    then_args: Vec::new(),
                    else_bb,
                    else_args: Vec::new(),
                });

                // Evaluate the RHS on the branch that continues execution.
                self.builder.select_block(rhs_bb);
                match self.lower_value_expr(right)? {
                    BranchResult::Value(value) => {
                        join.emit_branch(self, value, expr.span)?;
                    }
                    BranchResult::Return => {
                        return Ok(BranchResult::Return);
                    }
                }

                // Emit the short-circuit value on the opposite branch.
                join.restore_locals(self);
                self.builder.select_block(short_bb);
                let bool_ty = self.type_lowerer.lower_type_id(expr.ty);
                let short_value = self.builder.const_bool(short_val, bool_ty);
                join.emit_branch(self, short_value, expr.span)?;

                let join_value = join.join_value();
                join.finalize(self);
                Ok(BranchResult::Value(join_value))
            }

            // Match expression: switch on enum tags (decision tree not yet supported).
            sem::ValueExprKind::Match { scrutinee, arms } => {
                MatchLowerer::lower(self, expr, scrutinee, arms)
            }

            // Other expressions: lower using their plan.
            _ => {
                let plan = self.lowering_plans.get(&expr.id).unwrap_or_else(|| {
                    panic!("ssa lower_func missing lowering plan {:?}", expr.id)
                });
                match plan {
                    sem::LoweringPlan::Linear => {
                        let value = self.lower_linear_value_expr(expr)?;
                        Ok(BranchResult::Value(value))
                    }
                    sem::LoweringPlan::Branching => {
                        Err(self.err_span(expr.span, LoweringErrorKind::UnimplementedBranching))
                    }
                }
            }
        }
    }

    /// Lowers a statement inside a branching block.
    fn lower_stmt_expr_branching(
        &mut self,
        stmt: &sem::StmtExpr,
    ) -> Result<Option<BranchResult>, LoweringError> {
        match &stmt.kind {
            sem::StmtExprKind::While { cond, body } => {
                self.lower_while_stmt(cond, body)?;
                Ok(None)
            }
            sem::StmtExprKind::Break => Ok(Some(self.lower_break_stmt()?)),
            sem::StmtExprKind::Continue => Ok(Some(self.lower_continue_stmt()?)),
            _ => match self.lower_stmt_expr_linear(stmt)? {
                StmtOutcome::Continue => Ok(None),
                StmtOutcome::Return => Ok(Some(BranchResult::Return)),
            },
        }
    }

    /// Lowers a while loop to SSA IR.
    ///
    /// Creates three blocks:
    /// 1. Header block: evaluates the condition, branches to body or exit
    /// 2. Body block: executes the loop body, branches back to header
    /// 3. Exit block: continuation after the loop
    ///
    /// Local variables are threaded through as block parameters to maintain
    /// SSA form across the loop back-edge.
    pub(super) fn lower_while_stmt(
        &mut self,
        cond: &sem::ValueExpr,
        body: &sem::ValueExpr,
    ) -> Result<(), LoweringError> {
        // Snapshot current locals for threading through the loop.
        let locals_snapshot = self.locals.ordered();
        let defs: Vec<DefId> = locals_snapshot.iter().map(|(def_id, _)| *def_id).collect();
        let locals: Vec<_> = locals_snapshot.iter().map(|(_, local)| *local).collect();
        let tys: Vec<IrTypeId> = locals
            .iter()
            .map(|local| self.local_storage_ty(*local))
            .collect();
        let args: Vec<ValueId> = locals.iter().map(|local| local.storage_value()).collect();

        // Create the loop structure: header, body, and exit blocks.
        let header_bb = self.builder.add_block();
        let body_bb = self.builder.add_block();
        let exit_bb = self.builder.add_block();

        // Add parameters to header and exit blocks for threading locals.
        let header_params: Vec<ValueId> = tys
            .iter()
            .map(|ty| self.builder.add_block_param(header_bb, *ty))
            .collect();
        let exit_params: Vec<ValueId> = tys
            .iter()
            .map(|ty| self.builder.add_block_param(exit_bb, *ty))
            .collect();

        // Branch from the current block to the loop header.
        self.builder.terminate(Terminator::Br {
            target: header_bb,
            args,
        });

        // Lower the condition in the header block.
        self.builder.select_block(header_bb);
        self.locals
            .set_from_params_like(&defs, &locals, &header_params);
        let cond_value = self.lower_linear_expr_value(cond)?;
        let Some(exit_args) = self.locals.args_for(&defs) else {
            panic!("ssa lower_while_stmt missing locals args for loop exit");
        };

        // Conditional branch: true goes to body, false exits the loop.
        self.builder.terminate(Terminator::CondBr {
            cond: cond_value,
            then_bb: body_bb,
            then_args: Vec::new(),
            else_bb: exit_bb,
            else_args: exit_args,
        });

        // Lower the body and add back-edge to header (unless the body terminates).
        self.builder.select_block(body_bb);
        self.loop_stack.push(LoopContext {
            header_bb,
            exit_bb,
            defs: defs.clone(),
        });
        let body_result = self.lower_branching_value_expr(body);
        self.loop_stack.pop();
        let body_result = body_result?;
        if let BranchResult::Value(_) = body_result {
            // Collect updated locals and branch back to header.
            let Some(loop_args) = self.locals.args_for(&defs) else {
                panic!("ssa lower_while_stmt missing locals args for loop back-edge");
            };
            self.builder.terminate(Terminator::Br {
                target: header_bb,
                args: loop_args,
            });
        }

        // Set up locals for code after the loop and move cursor to exit block.
        self.builder.select_block(exit_bb);
        self.locals
            .set_from_params_like(&defs, &locals, &exit_params);
        Ok(())
    }

    /// Lowers a `break` statement by branching to the loop exit block.
    fn lower_break_stmt(&mut self) -> Result<BranchResult, LoweringError> {
        let ctx = self.current_loop();
        let Some(exit_args) = self.locals.args_for(&ctx.defs) else {
            panic!("ssa break missing locals args for loop exit");
        };
        self.builder.terminate(Terminator::Br {
            target: ctx.exit_bb,
            args: exit_args,
        });
        Ok(BranchResult::Return)
    }

    /// Lowers a `continue` statement by branching to the loop header block.
    fn lower_continue_stmt(&mut self) -> Result<BranchResult, LoweringError> {
        let ctx = self.current_loop();
        let Some(loop_args) = self.locals.args_for(&ctx.defs) else {
            panic!("ssa continue missing locals args for loop header");
        };
        self.builder.terminate(Terminator::Br {
            target: ctx.header_bb,
            args: loop_args,
        });
        Ok(BranchResult::Return)
    }
}
