//! Branching (multi-block) lowering routines.

use crate::resolve::DefId;
use crate::ssa::lower::linearize::{linearize_expr, linearize_stmt};
use crate::ssa::lower::lowerer::{BranchResult, BranchingValue, FuncLowerer, StmtOutcome};
use crate::ssa::model::ir::{BlockId, Terminator, TypeId, ValueId};
use crate::tree::semantic as sem;

impl<'a> FuncLowerer<'a> {
    pub(super) fn lower_branching_expr(
        &mut self,
        block: BlockId,
        expr: &sem::ValueExpr,
    ) -> Result<BranchResult, sem::LinearizeError> {
        match &expr.kind {
            sem::ValueExprKind::Block { items, tail } => {
                let mut cur_block = block;
                for item in items {
                    if let sem::BlockItem::Stmt(stmt) = item {
                        if let sem::StmtExprKind::While { cond, body } = &stmt.kind {
                            cur_block = self.lower_while_stmt(cur_block, cond, body)?;
                            continue;
                        }
                    }

                    // Use the precomputed plan to decide linear vs branching lowering.
                    match item {
                        sem::BlockItem::Stmt(stmt) => {
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
                            let plan = self.block_expr_plans.get(&expr.id).unwrap_or_else(|| {
                                panic!("ssa lower_func missing block expr plan {:?}", expr.id)
                            });
                            match plan {
                                sem::BlockExprPlan::Linear => {
                                    let linear = linearize_expr(expr).unwrap_or_else(|err| {
                                        panic!(
                                            "ssa lower_func block expr plan mismatch {:?}: {:?}",
                                            expr.id, err
                                        )
                                    });
                                    // Drop the value; we only care about side effects in statements.
                                    let _ = self.lower_linear_expr(cur_block, &linear)?;
                                }
                                sem::BlockExprPlan::Branching => {
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
                if let Some(tail) = tail {
                    return self.lower_branching_expr(cur_block, tail);
                }
                // Empty blocks produce unit.
                let ty = self.ctx.ssa_type_for_expr(expr);
                let value = self.builder.const_unit(cur_block, ty);
                return Ok(BranchResult::Value(BranchingValue {
                    value,
                    block: cur_block,
                }));
            }
            sem::ValueExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                let cond_value = self.lower_linear_expr_value(block, cond)?;

                // Build the control-flow skeleton and join parameters up front.
                let then_bb = self.builder.add_block();
                let else_bb = self.builder.add_block();
                let join_bb = self.builder.add_block();
                let join_ty = self.ctx.ssa_type_for_expr(expr);
                let join_value = self.builder.add_block_param(join_bb, join_ty);
                let locals_snapshot = self.ordered_locals();
                let defs: Vec<DefId> = locals_snapshot.iter().map(|(def_id, _)| *def_id).collect();
                let tys: Vec<TypeId> = locals_snapshot.iter().map(|(_, local)| local.ty).collect();
                let join_local_params: Vec<ValueId> = tys
                    .iter()
                    .map(|ty| self.builder.add_block_param(join_bb, *ty))
                    .collect();

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

                // Lower the then branch and pass its value + locals to the join.
                let saved_locals = self.locals.clone();
                let mut then_value = None;
                match self.lower_branching_expr(then_bb, then_body)? {
                    BranchResult::Value(value) => {
                        then_value = Some(value.value);
                        let local_args = self.locals_args(&defs, expr.span)?;
                        let mut args = Vec::with_capacity(1 + local_args.len());
                        args.push(value.value);
                        args.extend(local_args);
                        self.builder.set_terminator(
                            then_bb,
                            Terminator::Br {
                                target: join_bb,
                                args,
                            },
                        );
                    }
                    BranchResult::Return => {
                        self.locals = saved_locals.clone();
                    }
                }

                // Reset locals so the else branch starts from the same snapshot.
                self.locals = saved_locals.clone();
                let mut else_value = None;
                match self.lower_branching_expr(else_bb, else_body)? {
                    BranchResult::Value(value) => {
                        else_value = Some(value.value);
                        let local_args = self.locals_args(&defs, expr.span)?;
                        let mut args = Vec::with_capacity(1 + local_args.len());
                        args.push(value.value);
                        args.extend(local_args);
                        self.builder.set_terminator(
                            else_bb,
                            Terminator::Br {
                                target: join_bb,
                                args,
                            },
                        );
                    }
                    BranchResult::Return => {
                        self.locals = saved_locals;
                    }
                }

                // If both branches return, there is no join to emit.
                if then_value.is_none() && else_value.is_none() {
                    return Ok(BranchResult::Return);
                }

                // Install the join locals so subsequent lowering sees merged values.
                self.set_locals_from_params(&defs, &tys, &join_local_params);
                Ok(BranchResult::Value(BranchingValue {
                    value: join_value,
                    block: join_bb,
                }))
            }
            _ => {
                // Fall back to the linear subset for everything else.
                let linear = linearize_expr(expr)?;
                let value = self.lower_linear_expr(block, &linear)?;
                Ok(BranchResult::Value(BranchingValue { value, block }))
            }
        }
    }

    pub(super) fn lower_while_stmt(
        &mut self,
        block: BlockId,
        cond: &sem::ValueExpr,
        body: &sem::ValueExpr,
    ) -> Result<BlockId, sem::LinearizeError> {
        let locals_snapshot = self.ordered_locals();
        let defs: Vec<DefId> = locals_snapshot.iter().map(|(def_id, _)| *def_id).collect();
        let tys: Vec<TypeId> = locals_snapshot.iter().map(|(_, local)| local.ty).collect();
        let args: Vec<ValueId> = locals_snapshot
            .iter()
            .map(|(_, local)| local.value)
            .collect();

        // Allocate the loop blocks and their parameter lists.
        let header_bb = self.builder.add_block();
        let body_bb = self.builder.add_block();
        let exit_bb = self.builder.add_block();
        let header_params: Vec<ValueId> = tys
            .iter()
            .map(|ty| self.builder.add_block_param(header_bb, *ty))
            .collect();
        let exit_params: Vec<ValueId> = tys
            .iter()
            .map(|ty| self.builder.add_block_param(exit_bb, *ty))
            .collect();

        self.builder.set_terminator(
            block,
            Terminator::Br {
                target: header_bb,
                args,
            },
        );

        // Thread locals through the header and use them when lowering the condition.
        self.set_locals_from_params(&defs, &tys, &header_params);
        let cond_value = self.lower_linear_expr_value(header_bb, cond)?;
        let exit_args = self.locals_args(&defs, cond.span)?;

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

        // Only re-loop when the body produces a value (return short-circuits).
        let body_result = self.lower_branching_expr(body_bb, body)?;
        if let BranchResult::Value(_) = body_result {
            let loop_args = self.locals_args(&defs, body.span)?;
            self.builder.set_terminator(
                body_bb,
                Terminator::Br {
                    target: header_bb,
                    args: loop_args,
                },
            );
        }

        self.set_locals_from_params(&defs, &tys, &exit_params);
        Ok(exit_bb)
    }
}
