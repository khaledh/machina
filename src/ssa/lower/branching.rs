//! Branching (multi-block) lowering routines.

use crate::resolve::DefId;
use crate::ssa::lower::linearize::{linearize_expr, linearize_stmt};
use crate::ssa::lower::lowerer::{BranchResult, BranchingValue, FuncLowerer, StmtOutcome};
use crate::ssa::model::ir::{BlockId, Terminator, TypeId, ValueId};
use crate::tree::semantic as sem;

impl<'a> FuncLowerer<'a> {
    /// Lowers a branching expression, potentially creating multiple basic blocks.
    ///
    /// Handles control flow constructs (if/else, blocks with branches) and falls
    /// back to linear lowering for expressions that fit in a single block.
    pub(super) fn lower_branching_expr(
        &mut self,
        block: BlockId,
        expr: &sem::ValueExpr,
    ) -> Result<BranchResult, sem::LinearizeError> {
        match &expr.kind {
            // Block expression: process items sequentially, tracking the current block.
            sem::ValueExprKind::Block { items, tail } => {
                let mut cur_block = block;
                for item in items {
                    // While loops are handled specially at the statement level.
                    if let sem::BlockItem::Stmt(stmt) = item {
                        if let sem::StmtExprKind::While { cond, body } = &stmt.kind {
                            cur_block = self.lower_while_stmt(cur_block, cond, body)?;
                            continue;
                        }
                    }

                    // Dispatch based on precomputed plan (linear vs branching).
                    match item {
                        sem::BlockItem::Stmt(stmt) => {
                            // Statements are linearized and lowered.
                            let stmt = linearize_stmt(stmt)?;
                            match self.lower_linear_stmt(cur_block, &stmt)? {
                                StmtOutcome::Continue(next_block) => {
                                    cur_block = next_block;
                                }
                                StmtOutcome::Return => {
                                    return Ok(BranchResult::Return);
                                }
                            }
                        }
                        sem::BlockItem::Expr(expr) => {
                            // Look up the precomputed plan for this expression.
                            let plan = self.block_expr_plans.get(&expr.id).unwrap_or_else(|| {
                                panic!("ssa lower_func missing block expr plan {:?}", expr.id)
                            });
                            match plan {
                                sem::BlockExprPlan::Linear => {
                                    // Linear: can be lowered in the current block.
                                    let linear = linearize_expr(expr).unwrap_or_else(|err| {
                                        panic!(
                                            "ssa lower_func block expr plan mismatch {:?}: {:?}",
                                            expr.id, err
                                        )
                                    });
                                    // Side-effect only; discard the value.
                                    let _ = self.lower_linear_expr(cur_block, &linear)?;
                                }
                                sem::BlockExprPlan::Branching => {
                                    // Branching: may create new blocks.
                                    let result = self.lower_branching_expr(cur_block, expr)?;
                                    match result {
                                        BranchResult::Value(value) => {
                                            cur_block = value.block;
                                        }
                                        BranchResult::Return => {
                                            return Ok(BranchResult::Return);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Lower the tail expression if present.
                if let Some(tail) = tail {
                    return self.lower_branching_expr(cur_block, tail);
                }

                // Blocks without a tail produce unit.
                let ty = self.ctx.ssa_type_for_expr(expr);
                let value = self.builder.const_unit(cur_block, ty);
                return Ok(BranchResult::Value(BranchingValue {
                    value,
                    block: cur_block,
                }));
            }
            // If expression: creates then/else blocks and a join block.
            sem::ValueExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                // Lower the condition in the current block.
                let cond_value = self.lower_linear_expr_value(block, cond)?;

                // Create the control-flow structure: then block, else block, join block.
                let then_bb = self.builder.add_block();
                let else_bb = self.builder.add_block();
                let join_plan = self.build_join_plan(expr);

                // Emit the conditional branch.
                self.builder.set_terminator(
                    block,
                    Terminator::CondBr {
                        cond: cond_value,
                        then_bb,
                        then_args: Vec::new(),
                        else_bb,
                        else_args: Vec::new(),
                    },
                );

                // Save locals before lowering branches (each branch starts fresh).
                let saved_locals = self.locals.clone();

                // Lower the then branch.
                let mut then_value = None;
                match self.lower_branching_expr(then_bb, then_body)? {
                    BranchResult::Value(value) => {
                        then_value = Some(value.value);
                        // Branch to join with the result value and current locals.
                        self.emit_join_branch(then_bb, &join_plan, value.value, expr.span)?;
                    }
                    BranchResult::Return => {
                        // Then branch returns early; restore locals for else branch.
                        self.locals = saved_locals.clone();
                    }
                }

                // Lower the else branch (start from saved locals snapshot).
                self.locals = saved_locals.clone();
                let mut else_value = None;
                match self.lower_branching_expr(else_bb, else_body)? {
                    BranchResult::Value(value) => {
                        else_value = Some(value.value);
                        // Branch to join with the result value and current locals.
                        self.emit_join_branch(else_bb, &join_plan, value.value, expr.span)?;
                    }
                    BranchResult::Return => {
                        self.locals = saved_locals;
                    }
                }

                // If both branches return, the if expression never produces a value.
                if then_value.is_none() && else_value.is_none() {
                    return Ok(BranchResult::Return);
                }

                // Install the join block's parameters as the new local values.
                self.set_locals_from_params(
                    &join_plan.defs,
                    &join_plan.tys,
                    &join_plan.join_local_params,
                );
                Ok(BranchResult::Value(BranchingValue {
                    value: join_plan.join_value,
                    block: join_plan.join_bb,
                }))
            }
            // Other expressions: try linear lowering.
            _ => {
                let linear = linearize_expr(expr)?;
                let value = self.lower_linear_expr(block, &linear)?;
                Ok(BranchResult::Value(BranchingValue { value, block }))
            }
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
        block: BlockId,
        cond: &sem::ValueExpr,
        body: &sem::ValueExpr,
    ) -> Result<BlockId, sem::LinearizeError> {
        // Snapshot current locals for threading through the loop.
        let locals_snapshot = self.ordered_locals();
        let defs: Vec<DefId> = locals_snapshot.iter().map(|(def_id, _)| *def_id).collect();
        let tys: Vec<TypeId> = locals_snapshot.iter().map(|(_, local)| local.ty).collect();
        let args: Vec<ValueId> = locals_snapshot
            .iter()
            .map(|(_, local)| local.value)
            .collect();

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
        self.builder.set_terminator(
            block,
            Terminator::Br {
                target: header_bb,
                args,
            },
        );

        // Lower the condition in the header block.
        self.set_locals_from_params(&defs, &tys, &header_params);
        let cond_value = self.lower_linear_expr_value(header_bb, cond)?;
        let exit_args = self.locals_args(&defs, cond.span)?;

        // Conditional branch: true goes to body, false exits the loop.
        self.builder.set_terminator(
            header_bb,
            Terminator::CondBr {
                cond: cond_value,
                then_bb: body_bb,
                then_args: Vec::new(),
                else_bb: exit_bb,
                else_args: exit_args,
            },
        );

        // Lower the body and add back-edge to header (unless body returns).
        let body_result = self.lower_branching_expr(body_bb, body)?;
        if let BranchResult::Value(_) = body_result {
            // Collect updated locals and branch back to header.
            let loop_args = self.locals_args(&defs, body.span)?;
            self.builder.set_terminator(
                body_bb,
                Terminator::Br {
                    target: header_bb,
                    args: loop_args,
                },
            );
        }

        // Set up locals for code after the loop.
        self.set_locals_from_params(&defs, &tys, &exit_params);
        Ok(exit_bb)
    }
}
