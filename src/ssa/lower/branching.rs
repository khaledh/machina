//! Branching (multi-block) lowering routines.

use crate::resolve::DefId;
use crate::ssa::lower::linearize::{linearize_block_item, linearize_expr};
use crate::ssa::lower::lowerer::{BranchingValue, FuncLowerer};
use crate::ssa::model::ir::{BlockId, Terminator, TypeId, ValueId};
use crate::tree::semantic as sem;

impl<'a> FuncLowerer<'a> {
    pub(super) fn lower_branching_expr(
        &mut self,
        block: BlockId,
        expr: &sem::ValueExpr,
    ) -> Result<BranchingValue, sem::LinearizeError> {
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
                    // Linearize block items so we can guarantee straight-line lowering.
                    let item = linearize_block_item(item)?;
                    match item {
                        sem::LinearBlockItem::Stmt(stmt) => {
                            cur_block = self.lower_linear_stmt(cur_block, &stmt)?;
                        }
                        sem::LinearBlockItem::Expr(expr) => {
                            // Drop the value; we only care about side effects in statements.
                            let _ = self.lower_linear_expr(cur_block, &expr)?;
                        }
                    }
                }
                if let Some(tail) = tail {
                    return self.lower_branching_expr(cur_block, tail);
                }
                // Empty blocks produce unit.
                let ty = self.ctx.ssa_type_for_expr(expr);
                let value = self.builder.const_unit(cur_block, ty);
                return Ok(BranchingValue {
                    value,
                    block: cur_block,
                });
            }
            sem::ValueExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                let cond_value = self.lower_linear_expr_value(block, cond)?;

                let then_bb = self.builder.add_block();
                let else_bb = self.builder.add_block();
                let join_bb = self.builder.add_block();
                let join_ty = self.ctx.ssa_type_for_expr(expr);
                let join_value = self.builder.add_block_param(join_bb, join_ty);

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

                // For now, require that locals don't change across branches.
                let saved_locals = self.locals.clone();
                let then_value = self.lower_branching_expr(then_bb, then_body)?;
                if self.locals != saved_locals {
                    return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
                }
                self.locals = saved_locals.clone();
                self.builder.set_terminator(
                    then_bb,
                    Terminator::Br {
                        target: join_bb,
                        args: vec![then_value.value],
                    },
                );

                let else_value = self.lower_branching_expr(else_bb, else_body)?;
                if self.locals != saved_locals {
                    return Err(self.err_span(expr.span, sem::LinearizeErrorKind::UnsupportedExpr));
                }
                self.locals = saved_locals;
                self.builder.set_terminator(
                    else_bb,
                    Terminator::Br {
                        target: join_bb,
                        args: vec![else_value.value],
                    },
                );

                Ok(BranchingValue {
                    value: join_value,
                    block: join_bb,
                })
            }
            _ => {
                // Fall back to the linear subset for everything else.
                let linear = linearize_expr(expr)?;
                let value = self.lower_linear_expr(block, &linear)?;
                Ok(BranchingValue { value, block })
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

        let _body_value = self.lower_branching_expr(body_bb, body)?;
        let loop_args = self.locals_args(&defs, body.span)?;
        self.builder.set_terminator(
            body_bb,
            Terminator::Br {
                target: header_bb,
                args: loop_args,
            },
        );

        self.set_locals_from_params(&defs, &tys, &exit_params);
        Ok(exit_bb)
    }
}
