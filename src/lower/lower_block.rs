use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{FuncLowerer, Value};
use crate::mcir::types::*;
use crate::resolve::DefId;
use crate::tree::semantic::{
    BlockItem, PlaceExpr, PlaceExprKind as PEK, StmtExpr, StmtExprKind as SEK, ValueExpr,
};
use crate::tree::{InitInfo, NodeId};
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    // --- Block Lowering ---

    /// Lower a block used as an expression.
    pub(super) fn lower_block_expr(
        &mut self,
        block_ty: &Type,
        items: &[BlockItem],
        tail: Option<&ValueExpr>,
    ) -> Result<Value, LowerError> {
        // Each block introduces a new drop scope for owned locals.
        self.enter_drop_scope();

        // Evaluate all but the last expression for side effects.
        let mut terminated = false;
        for item in items {
            self.lower_block_item(item)?;
            if self.is_curr_block_terminated() {
                terminated = true;
                break;
            }
        }

        let result = if terminated {
            if block_ty.is_scalar() {
                Ok(Value::Scalar(Operand::Const(Const::Unit)))
            } else {
                let ty_id = self.ty_lowerer.lower_ty(block_ty);
                Ok(Value::Aggregate(self.new_temp_aggregate(ty_id)))
            }
        } else {
            match tail {
                Some(expr) => self.lower_expr_value(expr),
                None => Ok(Value::Scalar(Operand::Const(Const::Unit))),
            }
        };

        // Drop owned locals before leaving the block.
        if terminated || self.is_curr_block_terminated() {
            self.pop_drop_scope_without_drops();
        } else {
            self.exit_drop_scope()?;
        }
        result
    }

    /// Lower a block and write its final aggregate value into dst.
    pub(super) fn lower_block_into(
        &mut self,
        dst: Place<Aggregate>,
        items: &[BlockItem],
        tail: Option<&ValueExpr>,
        block_id: NodeId,
    ) -> Result<(), LowerError> {
        // Each block introduces a new drop scope for owned locals.
        self.enter_drop_scope();

        // Evaluate prefix, then write the tail into dst.
        for item in items {
            self.lower_block_item(item)?;
            if self.is_curr_block_terminated() {
                self.pop_drop_scope_without_drops();
                return Ok(());
            }
        }

        let result = match tail {
            Some(expr) => self.lower_agg_value_into(dst, expr),
            None => Err(LowerError::UnsupportedAggregateRhs(block_id)),
        };

        // Drop owned locals before leaving the block.
        if self.is_curr_block_terminated() {
            self.pop_drop_scope_without_drops();
        } else {
            self.exit_drop_scope()?;
        }
        result
    }

    /// Lower a block item (stmt-like expression) for side effects.
    pub(super) fn lower_block_item(&mut self, item: &BlockItem) -> Result<(), LowerError> {
        match item {
            BlockItem::Stmt(stmt) => self.lower_stmt_expr(stmt),
            BlockItem::Expr(expr) => self.lower_expr_value(expr).map(|_| ()),
        }
    }

    pub(super) fn lower_stmt_expr(&mut self, stmt: &StmtExpr) -> Result<(), LowerError> {
        match &stmt.kind {
            SEK::LetBind { pattern, value, .. } => self.lower_binding(pattern, value),
            SEK::VarBind { pattern, value, .. } => self.lower_binding(pattern, value),
            SEK::VarDecl { def_id, .. } => self.lower_var_decl(stmt, *def_id),
            SEK::Assign {
                assignee,
                value,
                init,
            } => self.lower_assign(assignee, value, *init),
            SEK::While { cond, body } => self.lower_while_expr(cond, body).map(|_| ()),
            SEK::For { .. } => {
                unreachable!("for loops should be desugared during elaboration")
            }
            SEK::Break => self.lower_break_stmt(stmt.id),
            SEK::Continue => self.lower_continue_stmt(stmt.id),
            SEK::Return { value } => self.lower_return_stmt(stmt.id, value.as_deref()),
        }
    }

    fn lower_break_stmt(&mut self, node_id: NodeId) -> Result<(), LowerError> {
        let Some(loop_ctx) = self.current_loop_context() else {
            return Err(LowerError::UnsupportedStmt(node_id));
        };
        self.emit_drops_to_depth(loop_ctx.drop_depth)?;
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(loop_ctx.break_bb));
        Ok(())
    }

    fn lower_continue_stmt(&mut self, node_id: NodeId) -> Result<(), LowerError> {
        let Some(loop_ctx) = self.current_loop_context() else {
            return Err(LowerError::UnsupportedStmt(node_id));
        };
        self.emit_drops_to_depth(loop_ctx.drop_depth)?;
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(loop_ctx.continue_bb));
        Ok(())
    }

    fn lower_return_stmt(
        &mut self,
        _node_id: NodeId,
        value: Option<&ValueExpr>,
    ) -> Result<(), LowerError> {
        if let Some(value) = value {
            let ret_ty = self.ret_type()?;
            let ret_id = self.fb.body.ret_local;
            let ret_ty_id = self.fb.body.locals[ret_id.0 as usize].ty;

            if ret_ty.is_scalar() {
                let body_ty = self.ty_from_id(value.ty);
                let op = self.lower_scalar_expr(value)?;
                self.emit_conversion_check(&body_ty, &ret_ty, &op);
                let ret_place = Place::<Scalar>::new(ret_id, ret_ty_id, vec![]);
                self.emit_copy_scalar(ret_place, Rvalue::Use(op));
            } else {
                let ret_place = Place::<Aggregate>::new(ret_id, ret_ty_id, vec![]);
                self.lower_agg_value_into(ret_place, value)?;
            }
        }

        self.emit_drops_to_depth(0)?;
        self.fb.set_terminator(self.curr_block, Terminator::Return);
        Ok(())
    }

    /// Lower an assignment expression.
    pub(super) fn lower_assign(
        &mut self,
        assignee: &PlaceExpr,
        value: &ValueExpr,
        init: InitInfo,
    ) -> Result<(), LowerError> {
        let value_ty = self.ty_from_id(value.ty);

        if value_ty.is_scalar() {
            // Scalar assignment.
            let assignee_place = self.lower_place_scalar(assignee)?;
            let value_operand = self.lower_scalar_expr(value)?;
            if self.is_curr_block_terminated() {
                return Ok(());
            }
            let target_ty = self.ty_from_id(assignee.ty);
            self.emit_conversion_check(&value_ty, &target_ty, &value_operand);

            self.emit_overwrite_drop(
                assignee,
                PlaceAny::Scalar(assignee_place.clone()),
                &target_ty,
                init,
                true,
            );

            self.emit_copy_scalar(assignee_place, Rvalue::Use(value_operand));
            self.mark_initialized_if_needed(assignee, init);
            self.mark_full_init_if_needed(assignee, init);
        } else {
            // Aggregate assignment (value written directly into place).
            let assignee_place = self.lower_place_agg(assignee)?;

            // Overwrite semantics: free the old heap pointer first.
            let target_ty = self.ty_from_id(assignee.ty);
            self.emit_overwrite_drop(
                assignee,
                PlaceAny::Aggregate(assignee_place.clone()),
                &target_ty,
                init,
                false,
            );

            // Write the new value into the assignee place.
            self.lower_agg_value_into(assignee_place, value)?;
            if self.is_curr_block_terminated() {
                return Ok(());
            }
            self.mark_initialized_if_needed(assignee, init);
            self.mark_full_init_if_needed(assignee, init);
        }

        Ok(())
    }

    fn lower_var_decl(&mut self, stmt: &StmtExpr, def_id: DefId) -> Result<(), LowerError> {
        // Reserve storage for the declaration; initialization is handled by
        // a later assignment (tracked by def-init).
        let decl_ty = self.def_ty_for_id(def_id, stmt.id)?;
        let decl_ty_id = self.ty_lowerer.lower_ty(&decl_ty);
        let name = self.def_name(def_id, stmt.id)?;
        self.ensure_local_for_def(def_id, decl_ty_id, Some(name.clone()));

        let is_initialized = self.create_is_initialized(&name, &decl_ty, false);
        self.register_drop(def_id, &decl_ty, is_initialized);

        Ok(())
    }

    pub(super) fn create_is_initialized(
        &mut self,
        name: &str,
        decl_ty: &Type,
        initial: bool,
    ) -> Option<LocalId> {
        if !decl_ty.needs_drop() {
            return None;
        }
        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);
        let flag_id = self.fb.new_local(
            bool_ty_id,
            LocalKind::Temp,
            Some(format!("{}$is_initialized", name)),
        );
        let flag_place = Place::new(flag_id, bool_ty_id, vec![]);
        self.emit_copy_scalar(
            flag_place,
            Rvalue::Use(Operand::Const(Const::Bool(initial))),
        );
        Some(flag_id)
    }

    pub(super) fn emit_overwrite_drop(
        &mut self,
        assignee: &PlaceExpr,
        place: PlaceAny,
        target_ty: &Type,
        init: InitInfo,
        clear_moved: bool,
    ) {
        if !target_ty.needs_drop() {
            return;
        }

        if self.is_out_param_assignee(assignee)
            && matches!(assignee.kind, PEK::Var { .. })
            && init.is_init
        {
            // First assignment to an out param is a full initialization; skip drop.
            return;
        }

        if let Some(flag) = self.is_initialized_for_assignee(assignee) {
            self.emit_drop_place_if_initialized(place, target_ty, flag);
            return;
        }

        if init.is_init {
            return;
        }

        self.emit_drop_place(place, target_ty);
        if clear_moved {
            if let PEK::Var { def_id, .. } = assignee.kind {
                self.clear_moved(def_id);
            }
        }
    }

    pub(super) fn mark_initialized_if_needed(&mut self, assignee: &PlaceExpr, init: InitInfo) {
        if !init.is_init {
            return;
        }
        if let Some(flag) = self.is_initialized_for_assignee(assignee) {
            self.emit_is_initialized(flag, true);
        }
    }

    pub(super) fn mark_full_init_if_needed(&mut self, assignee: &PlaceExpr, init: InitInfo) {
        if !init.promotes_full {
            return;
        }
        let Some(def_id) = self.base_def_for_assignee(assignee) else {
            return;
        };
        let Some(flag) = self.is_initialized_for_def(def_id) else {
            return;
        };
        self.emit_is_initialized(flag, true);
    }

    fn is_initialized_for_assignee(&self, assignee: &PlaceExpr) -> Option<LocalId> {
        if let PEK::Var { def_id, .. } = assignee.kind {
            self.is_initialized_for_def(def_id)
        } else {
            None
        }
    }

    fn is_out_param_assignee(&self, assignee: &PlaceExpr) -> bool {
        self.base_def_for_assignee(assignee)
            .map(|def_id| self.out_param_defs.contains(&def_id))
            .unwrap_or(false)
    }

    fn base_def_for_assignee(&self, assignee: &PlaceExpr) -> Option<DefId> {
        match &assignee.kind {
            PEK::Var { def_id, .. } => Some(*def_id),
            PEK::StructField { target, .. }
            | PEK::TupleField { target, .. }
            | PEK::ArrayIndex { target, .. } => self.base_def_for_assignee(target),
            PEK::Deref { .. } => None,
        }
    }
}
