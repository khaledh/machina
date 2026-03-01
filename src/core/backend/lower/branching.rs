//! Branching (multi-block) lowering routines.

use crate::core::backend::lower::LowerToIrError;
use crate::core::backend::lower::join::JoinSession;
use crate::core::backend::lower::lowerer::{BranchResult, FuncLowerer, LoopContext, StmtOutcome};
use crate::core::backend::lower::r#match::MatchLowerer;
use crate::core::ir::IrTypeId;
use crate::core::ir::{
    BlockId, Callee, CastKind, CmpOp, ConstValue, SwitchCase, Terminator, ValueId,
};
use crate::core::resolve::DefId;
use crate::core::tree::{BinaryOp, NodeId, semantic as sem};
use crate::core::types::{Type, TypeId};

struct TryLoweringSetup {
    union_value: ValueId,
    union_ty: Type,
    ok_ty: Type,
    join_expr_ty: TypeId,
    union_addr: ValueId,
    tag: ValueId,
    blob_ty: IrTypeId,
    ok_payload_ty: IrTypeId,
    ok_payload_offset: u64,
    ok_bb: BlockId,
    err_bb: BlockId,
    join: JoinSession,
}

impl<'a, 'g> FuncLowerer<'a, 'g> {
    fn lower_expr_to_join(
        &mut self,
        join: &JoinSession,
        expr: &sem::ValueExpr,
        span: crate::core::diag::Span,
        join_sem_ty: Option<&Type>,
    ) -> Result<bool, LowerToIrError> {
        match self.lower_branching_value_expr(expr)? {
            BranchResult::Value(value) => {
                let branch_value = if let Some(join_sem_ty) = join_sem_ty {
                    let branch_sem_ty = self.type_map.type_table().get(expr.ty).clone();
                    self.coerce_value(value, &branch_sem_ty, join_sem_ty)
                } else {
                    value
                };
                join.emit_branch(self, branch_value, span)?;
                Ok(false)
            }
            BranchResult::Return => Ok(true),
        }
    }

    fn finalize_join_value(&mut self, join: JoinSession) -> BranchResult {
        let join_value = join.join_value();
        join.finalize(self);
        BranchResult::Value(join_value)
    }

    fn lower_loop_jump(
        &mut self,
        stmt_id: NodeId,
        target: BlockId,
        defs: &[DefId],
        locals: &[crate::core::backend::lower::locals::LocalValue],
    ) -> Result<BranchResult, LowerToIrError> {
        self.emit_drops_for_stmt(stmt_id)?;
        let args = self.local_args_for_like(defs, locals);
        self.builder.terminate(Terminator::Br { target, args });
        Ok(BranchResult::Return)
    }

    /// Lowers a branching expression, potentially creating multiple basic blocks.
    ///
    /// Emits instructions starting at the current block cursor. May create new blocks
    /// and switch the cursor. On return, the cursor is at the "ending block" where
    /// execution continues after this expression.
    pub(super) fn lower_branching_value_expr(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        match &expr.kind {
            // Block expression: process items sequentially.
            sem::ValueExprKind::Block { items, tail } => {
                self.with_drop_scope(expr.id, |lowerer| {
                    for item in items {
                        match item {
                            sem::BlockItem::Stmt(stmt) => {
                                if let Some(result) = lowerer.lower_stmt_expr_branching(stmt)? {
                                    return Ok(result);
                                }
                            }
                            sem::BlockItem::Expr(expr) => {
                                lowerer.annotate_expr(expr);
                                // Statement-position expression: lower using the plan.
                                if let BranchResult::Return = lowerer.lower_value_expr(expr)? {
                                    return Ok(BranchResult::Return);
                                }
                            }
                        }
                    }

                    if let Some(tail) = tail {
                        lowerer.annotate_expr(tail);
                        return match lowerer.lower_value_expr(tail)? {
                            BranchResult::Value(value) => Ok(BranchResult::Value(value)),
                            BranchResult::Return => Ok(BranchResult::Return),
                        };
                    }

                    // Blocks without a tail produce unit.
                    let ty = lowerer.type_lowerer.lower_type_id(expr.ty);
                    let value = lowerer.builder.const_unit(ty);
                    Ok(BranchResult::Value(value))
                })
            }

            // If expression: creates then/else blocks and a join block.
            sem::ValueExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                let join_sem_ty = self.type_map.type_table().get(expr.ty).clone();
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
                let then_returned =
                    self.lower_expr_to_join(&join, then_body, expr.span, Some(&join_sem_ty))?;

                // Lower the else branch (start from saved locals snapshot).
                join.restore_locals(self);
                self.builder.select_block(else_bb);
                let else_returned =
                    self.lower_expr_to_join(&join, else_body, expr.span, Some(&join_sem_ty))?;

                // If both branches return, the if expression never produces a value.
                if then_returned && else_returned {
                    return Ok(BranchResult::Return);
                }

                // Select join block and install its parameters as the new local values.
                Ok(self.finalize_join_value(join))
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
                if self.lower_expr_to_join(&join, right, expr.span, None)? {
                    return Ok(BranchResult::Return);
                }

                // Emit the short-circuit value on the opposite branch.
                join.restore_locals(self);
                self.builder.select_block(short_bb);
                let bool_ty = self.type_lowerer.lower_type_id(expr.ty);
                let short_value = self.builder.const_bool(short_val, bool_ty);
                join.emit_branch(self, short_value, expr.span)?;

                Ok(self.finalize_join_value(join))
            }

            sem::ValueExprKind::Try {
                fallible_expr,
                on_error,
            } => {
                if let Some(handler) = on_error {
                    self.lower_try_handle(expr, fallible_expr, handler)
                } else {
                    self.lower_try_propagate(expr, fallible_expr)
                }
            }

            // Match expression: switch on enum tags (decision tree not yet supported).
            sem::ValueExprKind::Match { scrutinee, arms } => {
                MatchLowerer::lower(self, expr, scrutinee, arms)
            }

            // Other expressions: delegate to unified value lowering to avoid duplication.
            _ => match self.lowering_plan(expr.id) {
                sem::LoweringPlan::Linear | sem::LoweringPlan::Branching => {
                    self.lower_value_expr_value(expr)
                }
            },
        }
    }

    pub(super) fn lower_try_propagate(
        &mut self,
        expr: &sem::ValueExpr,
        inner: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        let union_value = match self.lower_value_expr(inner)? {
            BranchResult::Value(value) => value,
            BranchResult::Return => return Ok(BranchResult::Return),
        };

        let setup = self.lower_try_setup(expr, inner, union_value)?;
        let Type::ErrorUnion { err_tys, .. } = &setup.union_ty else {
            panic!(
                "backend try operator expects error union operand, found {:?}",
                setup.union_ty
            );
        };

        // Fast path: when operand and function return unions are identical,
        // propagation can return the original union value directly.
        let return_union_matches_operand = setup.union_ty == self.ret_ty;
        let err_variants = self.try_err_variants(inner.ty, &setup.union_ty);

        if err_variants.len() != err_tys.len() {
            panic!(
                "backend try mismatch between error variants ({}) and source error types ({})",
                err_variants.len(),
                err_tys.len()
            );
        }

        self.lower_try_ok_path(&setup, expr.span)?;
        self.enter_try_error_path(&setup);
        if let Some(cleanup) = self.try_cleanup_plan(expr.id) {
            // Cleanup runs before the propagated error returns from the
            // current callable, matching the scoped `defer` execution model.
            for cleanup_expr in cleanup {
                match self.lower_value_expr(&cleanup_expr)? {
                    BranchResult::Value(_) => {}
                    BranchResult::Return => return Ok(BranchResult::Return),
                }
            }
        }
        if return_union_matches_operand {
            self.emit_root_return(Some(setup.union_value))?;
        } else {
            self.lower_try_error_return_cases(
                setup.union_addr,
                setup.blob_ty,
                setup.tag,
                err_tys,
                &err_variants,
            )?;
        }

        Ok(self.finalize_join_value(setup.join))
    }

    pub(super) fn lower_try_handle(
        &mut self,
        expr: &sem::ValueExpr,
        fallible_expr: &sem::ValueExpr,
        handler_expr: &sem::ValueExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        let union_value = match self.lower_value_expr(fallible_expr)? {
            BranchResult::Value(value) => value,
            BranchResult::Return => return Ok(BranchResult::Return),
        };

        let setup = self.lower_try_setup(expr, fallible_expr, union_value)?;
        self.lower_try_ok_path(&setup, expr.span)?;
        self.enter_try_error_path(&setup);
        let handler_value = match self.lower_value_expr(handler_expr)? {
            BranchResult::Value(value) => value,
            BranchResult::Return => return Ok(BranchResult::Return),
        };
        let call_plan = self.call_plan(expr.id);
        let mut handler_args = vec![crate::core::backend::lower::lowerer::CallInputValue {
            value: setup.union_value,
            ty: setup.union_ty.clone(),
            is_addr: false,
            drop_def: None,
        }];
        let call_args = self.lower_call_args_from_plan(
            expr.id,
            expr.span,
            &call_plan,
            None,
            &mut handler_args,
        )?;
        let result_ir_ty = self.type_lowerer.lower_type_id(setup.join_expr_ty);
        let handled = self
            .builder
            .call(Callee::Value(handler_value), call_args, result_ir_ty);
        setup.join.emit_branch(self, handled, expr.span)?;

        Ok(self.finalize_join_value(setup.join))
    }

    fn lower_try_setup(
        &mut self,
        expr: &sem::ValueExpr,
        fallible_expr: &sem::ValueExpr,
        union_value: ValueId,
    ) -> Result<TryLoweringSetup, LowerToIrError> {
        let union_ty = self.type_map.type_table().get(fallible_expr.ty).clone();
        let Type::ErrorUnion { ok_ty, .. } = &union_ty else {
            panic!(
                "backend try operator expects error union operand, found {:?}",
                union_ty
            );
        };
        let ok_ty = ok_ty.as_ref().clone();
        let join_expr_ty = self.try_join_expr_ty(expr.ty, &ok_ty);
        let union_ir_ty = self.type_lowerer.lower_type_id(fallible_expr.ty);
        let union_slot = self.materialize_value_slot(union_value, union_ir_ty);
        let (tag_ty, blob_ty, ok_payload_ty, ok_payload_offset) =
            self.try_ok_variant_layout(fallible_expr.ty, &union_ty);

        let tag = self.load_field(union_slot.addr, 0, tag_ty);
        let tag_zero = self.builder.const_int(0, false, 32, tag_ty);
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let is_ok = self.builder.cmp(CmpOp::Eq, tag, tag_zero, bool_ty);

        let ok_bb = self.builder.add_block();
        let err_bb = self.builder.add_block();
        let join_expr = sem::ValueExpr {
            ty: join_expr_ty,
            ..expr.clone()
        };
        let join = self.begin_join(&join_expr);

        self.builder.terminate(Terminator::CondBr {
            cond: is_ok,
            then_bb: ok_bb,
            then_args: Vec::new(),
            else_bb: err_bb,
            else_args: Vec::new(),
        });

        Ok(TryLoweringSetup {
            union_value,
            union_ty,
            ok_ty,
            join_expr_ty,
            union_addr: union_slot.addr,
            tag,
            blob_ty,
            ok_payload_ty,
            ok_payload_offset,
            ok_bb,
            err_bb,
            join,
        })
    }

    fn lower_try_ok_path(
        &mut self,
        setup: &TryLoweringSetup,
        span: crate::core::diag::Span,
    ) -> Result<(), LowerToIrError> {
        self.builder.select_block(setup.ok_bb);
        let ok_value = self.load_union_payload(
            setup.union_addr,
            setup.blob_ty,
            setup.ok_payload_ty,
            setup.ok_payload_offset,
        );
        let try_sem_ty = self.type_map.type_table().get(setup.join_expr_ty).clone();
        let ok_coerced = self.coerce_value(ok_value, &setup.ok_ty, &try_sem_ty);
        setup.join.emit_branch(self, ok_coerced, span)
    }

    fn enter_try_error_path(&mut self, setup: &TryLoweringSetup) {
        setup.join.restore_locals(self);
        self.builder.select_block(setup.err_bb);
    }

    fn try_join_expr_ty(&self, expr_ty: TypeId, ok_ty: &Type) -> TypeId {
        let try_result_ty = self.type_map.type_table().get(expr_ty).clone();
        // Some statement-level `expr?;` forms can reach lowering with an
        // unresolved try-result type variable even though the operand's ok type
        // is concrete. Fall back to the operand ok type so lowering remains
        // deterministic instead of panicking on unresolved backend type ids.
        if try_result_ty.contains_unresolved() {
            self.type_map
                .type_table()
                .lookup_id(ok_ty)
                .unwrap_or(expr_ty)
        } else {
            expr_ty
        }
    }

    fn try_ok_variant_layout(
        &mut self,
        union_ty_id: TypeId,
        union_ty: &Type,
    ) -> (IrTypeId, IrTypeId, IrTypeId, u64) {
        let layout = self.type_lowerer.enum_layout(union_ty_id);
        let ok_variant = layout
            .variants
            .first()
            .unwrap_or_else(|| panic!("backend try missing ok variant in {:?}", union_ty));
        if ok_variant.field_tys.len() != 1 || ok_variant.field_offsets.len() != 1 {
            panic!(
                "backend try expects single-payload ok variant, found {:?}",
                ok_variant.field_tys
            );
        }
        (
            layout.tag_ty,
            layout.blob_ty,
            ok_variant.field_tys[0],
            ok_variant.field_offsets[0],
        )
    }

    fn try_err_variants(
        &mut self,
        union_ty_id: TypeId,
        union_ty: &Type,
    ) -> Vec<(u32, IrTypeId, u64)> {
        self.type_lowerer
            .enum_layout(union_ty_id)
            .variants
            .iter()
            .skip(1)
            .map(|variant| {
                if variant.field_tys.len() != 1 || variant.field_offsets.len() != 1 {
                    panic!(
                        "backend try expects single-payload error variant in {:?}, found {:?}",
                        union_ty, variant.field_tys
                    );
                }
                (variant.tag, variant.field_tys[0], variant.field_offsets[0])
            })
            .collect()
    }

    fn lower_try_error_return_cases(
        &mut self,
        union_addr: ValueId,
        blob_ty: IrTypeId,
        tag: ValueId,
        err_tys: &[Type],
        err_variants: &[(u32, IrTypeId, u64)],
    ) -> Result<(), LowerToIrError> {
        let mut cases = Vec::with_capacity(err_variants.len());
        let mut case_blocks = Vec::with_capacity(err_variants.len());
        for (tag_value, _, _) in err_variants {
            let bb = self.builder.add_block();
            case_blocks.push(bb);
            cases.push(SwitchCase {
                value: ConstValue::Int {
                    value: *tag_value as i128,
                    signed: false,
                    bits: 32,
                },
                target: bb,
                args: Vec::new(),
            });
        }
        let default_bb = self.builder.add_block();
        self.builder.terminate(Terminator::Switch {
            value: tag,
            cases,
            default: default_bb,
            default_args: Vec::new(),
        });

        for (((_, payload_ty, payload_offset), err_ty), case_bb) in err_variants
            .iter()
            .zip(err_tys.iter())
            .zip(case_blocks.iter())
        {
            self.builder.select_block(*case_bb);
            let payload_value =
                self.load_union_payload(union_addr, blob_ty, *payload_ty, *payload_offset);
            let coerced = self.coerce_return_value(payload_value, err_ty);
            self.emit_root_return(Some(coerced))?;
        }

        self.builder.select_block(default_bb);
        self.builder.terminate(Terminator::Unreachable);
        Ok(())
    }

    fn load_union_payload(
        &mut self,
        union_addr: ValueId,
        blob_ty: IrTypeId,
        payload_ty: IrTypeId,
        payload_offset: u64,
    ) -> ValueId {
        let blob_ptr = self.field_addr_typed(union_addr, 1, blob_ty);
        let payload_bytes = self.byte_offset_addr(blob_ptr, payload_offset);
        let payload_ptr_ty = self.type_lowerer.ptr_to(payload_ty);
        let payload_ptr = self
            .builder
            .cast(CastKind::PtrToPtr, payload_bytes, payload_ptr_ty);
        self.builder.load(payload_ptr, payload_ty)
    }

    /// Lowers a statement inside a branching block.
    fn lower_stmt_expr_branching(
        &mut self,
        stmt: &sem::StmtExpr,
    ) -> Result<Option<BranchResult>, LowerToIrError> {
        match &stmt.kind {
            sem::StmtExprKind::While { cond, body } => {
                self.annotate_stmt(stmt);
                self.lower_while_stmt(cond, body)?;
                Ok(None)
            }
            sem::StmtExprKind::Break => {
                self.annotate_stmt(stmt);
                Ok(Some(self.lower_break_stmt(stmt)?))
            }
            sem::StmtExprKind::Continue => {
                self.annotate_stmt(stmt);
                Ok(Some(self.lower_continue_stmt(stmt)?))
            }
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
    ) -> Result<(), LowerToIrError> {
        // Snapshot active drop scopes so we can restore them after lowering
        // control-flow edges that may exit the loop body early.
        let drop_snapshot = self.drop_scopes_snapshot();

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
        let exit_args = self.local_args_for_like(&defs, &locals);

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
            locals: locals.clone(),
        });
        let body_result = self.lower_branching_value_expr(body);
        self.loop_stack.pop();
        let body_result = body_result?;

        // The body may have pushed/popped scopes; restore the outer loop depth
        // before emitting back-edges or continuing at the loop exit.
        self.restore_drop_scopes(&drop_snapshot);

        if let BranchResult::Value(_) = body_result {
            // Collect updated locals and branch back to header.
            let loop_args = self.local_args_for_like(&defs, &locals);
            self.builder.terminate(Terminator::Br {
                target: header_bb,
                args: loop_args,
            });
        }

        // Set up locals for code after the loop and move cursor to exit block.
        self.builder.select_block(exit_bb);
        self.locals
            .set_from_params_like(&defs, &locals, &exit_params);

        // The exit block should see the same drop scope depth as before the loop.
        self.restore_drop_scopes(&drop_snapshot);
        // Loop exits merge control flow; conservatively drop liveness knowledge.
        self.invalidate_drop_liveness();

        Ok(())
    }

    /// Lowers a `break` statement by branching to the loop exit block.
    fn lower_break_stmt(&mut self, stmt: &sem::StmtExpr) -> Result<BranchResult, LowerToIrError> {
        let (target, defs, locals) = {
            let ctx = self.current_loop();
            (ctx.exit_bb, ctx.defs.clone(), ctx.locals.clone())
        };
        self.lower_loop_jump(stmt.id, target, &defs, &locals)
    }

    /// Lowers a `continue` statement by branching to the loop header block.
    fn lower_continue_stmt(
        &mut self,
        stmt: &sem::StmtExpr,
    ) -> Result<BranchResult, LowerToIrError> {
        let (target, defs, locals) = {
            let ctx = self.current_loop();
            (ctx.header_bb, ctx.defs.clone(), ctx.locals.clone())
        };
        self.lower_loop_jump(stmt.id, target, &defs, &locals)
    }
}
