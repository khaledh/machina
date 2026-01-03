use crate::ast::{BlockItem, Expr, ExprKind, NodeId, StmtExpr, StmtExprKind};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{ExprValue, FuncLowerer};
use crate::mcir::types::*;
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    // --- Block Lowering ---

    /// Lower a block used as an expression.
    pub(super) fn lower_block_expr(
        &mut self,
        items: &[BlockItem],
        tail: Option<&Expr>,
    ) -> Result<ExprValue, LowerError> {
        // Each block introduces a new drop scope for owned locals.
        self.enter_drop_scope();

        // Evaluate all but the last expression for side effects.
        for item in items {
            self.lower_block_item(item)?;
        }

        let result = match tail {
            Some(expr) => self.lower_expr_value(expr),
            None => Ok(ExprValue::Scalar(Operand::Const(Const::Unit))),
        };

        // Drop owned locals before leaving the block.
        self.exit_drop_scope()?;
        result
    }

    /// Lower a block and write its final aggregate value into dst.
    pub(super) fn lower_block_into(
        &mut self,
        dst: Place<Aggregate>,
        items: &[BlockItem],
        tail: Option<&Expr>,
        block_id: NodeId,
    ) -> Result<(), LowerError> {
        // Each block introduces a new drop scope for owned locals.
        self.enter_drop_scope();

        // Evaluate prefix, then write the tail into dst.
        for item in items {
            self.lower_block_item(item)?;
        }

        let result = match tail {
            Some(expr) => self.lower_agg_value_into(dst, expr),
            None => Err(LowerError::UnsupportedAggregateRhs(block_id)),
        };

        // Drop owned locals before leaving the block.
        self.exit_drop_scope()?;
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
            StmtExprKind::LetBind { pattern, value, .. } => self.lower_binding(pattern, value),
            StmtExprKind::VarBind { pattern, value, .. } => self.lower_binding(pattern, value),
            StmtExprKind::VarDecl { name, .. } => self.lower_var_decl(stmt, name),
            StmtExprKind::Assign { assignee, value } => self.lower_assign(assignee, value),
            StmtExprKind::While { cond, body } => self.lower_while_expr(cond, body).map(|_| ()),
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => self.lower_for_expr(pattern, iter, body).map(|_| ()),
        }
    }

    /// Lower an assignment expression.
    pub(super) fn lower_assign(&mut self, assignee: &Expr, value: &Expr) -> Result<(), LowerError> {
        let value_ty = self.ty_for_node(value.id)?;

        if value_ty.is_scalar() {
            // Scalar assignment.
            let assignee_place = self.lower_place_scalar(assignee)?;
            let value_operand = self.lower_scalar_expr(value)?;
            let target_ty = self.ty_for_node(assignee.id)?;
            self.emit_conversion_check(&value_ty, &target_ty, &value_operand);

            self.emit_overwrite_drop(
                assignee,
                PlaceAny::Scalar(assignee_place.clone()),
                &target_ty,
                true,
            );

            self.emit_copy_scalar(assignee_place, Rvalue::Use(value_operand));
            self.mark_initialized_if_needed(assignee);
        } else {
            // Aggregate assignment (value written directly into place).
            let assignee_place = self.lower_place_agg(assignee)?;

            // Overwrite semantics: free the old heap pointer first.
            let target_ty = self.ty_for_node(assignee.id)?;
            self.emit_overwrite_drop(
                assignee,
                PlaceAny::Aggregate(assignee_place.clone()),
                &target_ty,
                false,
            );

            // Write the new value into the assignee place.
            self.lower_agg_value_into(assignee_place, value)?;
            self.mark_initialized_if_needed(assignee);
        }

        Ok(())
    }

    fn lower_var_decl(&mut self, stmt: &StmtExpr, name: &str) -> Result<(), LowerError> {
        // Reserve storage for the declaration; initialization is handled by
        // a later assignment (tracked by def-init).
        let def_id = self.def_for_node(stmt.id)?.id;
        let decl_ty = self.def_ty_for_node(stmt.id)?;
        let decl_ty_id = self.ty_lowerer.lower_ty(&decl_ty);
        self.ensure_local_for_def(def_id, decl_ty_id, Some(name.to_string()));

        let is_initialized = self.create_is_initialized(name, &decl_ty);
        self.register_drop(def_id, &decl_ty, is_initialized);

        Ok(())
    }

    fn create_is_initialized(&mut self, name: &str, decl_ty: &Type) -> Option<LocalId> {
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
        self.emit_copy_scalar(flag_place, Rvalue::Use(Operand::Const(Const::Bool(false))));
        Some(flag_id)
    }

    fn emit_overwrite_drop(
        &mut self,
        assignee: &Expr,
        place: PlaceAny,
        target_ty: &Type,
        clear_moved: bool,
    ) {
        if !target_ty.needs_drop() {
            return;
        }

        if let Some(flag) = self.is_initialized_for_assignee(assignee) {
            self.emit_drop_place_if_initialized(place, target_ty, flag);
            return;
        }

        if matches!(assignee.kind, ExprKind::Var(_)) && self.ctx.init_assigns.contains(&assignee.id)
        {
            return;
        }

        self.emit_drop_place(place, target_ty);
        if clear_moved {
            if let ExprKind::Var(_) = assignee.kind {
                if let Ok(def) = self.def_for_node(assignee.id) {
                    self.clear_moved(def.id);
                }
            }
        }
    }

    fn mark_initialized_if_needed(&mut self, assignee: &Expr) {
        if let Some(flag) = self.is_initialized_for_assignee(assignee) {
            self.set_is_initialized(flag, true);
        }
    }

    fn is_initialized_for_assignee(&mut self, assignee: &Expr) -> Option<LocalId> {
        if !matches!(assignee.kind, ExprKind::Var(_)) {
            return None;
        }
        let def = self.def_for_node(assignee.id).ok()?;
        self.is_initialized_for_def(def.id)
    }
}
