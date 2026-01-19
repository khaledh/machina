use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{FuncLowerer, Value};
use crate::mcir::types::*;
use crate::tree::semantic::{ValueExpr, ValueExprKind as VEK};

impl<'a> FuncLowerer<'a> {
    // --- Control Flow ---

    /// Lower an if expression in value position.
    pub(super) fn lower_if_expr(
        &mut self,
        cond: &ValueExpr,
        then_body: &ValueExpr,
        else_body: &ValueExpr,
    ) -> Result<Value, LowerError> {
        let cond_op = self.lower_scalar_expr(cond)?;
        let result_ty = self.ty_from_id(then_body.ty);
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        if result_ty.is_scalar() {
            // Scalar if-expr: write both branches into the same temp.
            let temp_place = self.new_temp_scalar(result_ty_id);

            self.lower_if_join(
                cond_op,
                |this| {
                    if let Value::Scalar(op) = this.lower_expr_value(then_body)? {
                        if !this.is_curr_block_terminated() {
                            this.emit_copy_scalar(temp_place.clone(), Rvalue::Use(op));
                        }
                    }
                    Ok(())
                },
                |this| {
                    if let Value::Scalar(op) = this.lower_expr_value(else_body)? {
                        if !this.is_curr_block_terminated() {
                            this.emit_copy_scalar(temp_place.clone(), Rvalue::Use(op));
                        }
                    }
                    Ok(())
                },
            )?;

            Ok(Value::Scalar(Operand::Copy(temp_place)))
        } else {
            // Aggregate if-expr: lower both branches into a temp place.
            let temp_place = self.new_temp_aggregate(result_ty_id);

            self.lower_if_join(
                cond_op,
                |this| this.lower_agg_value_into(temp_place.clone(), then_body),
                |this| this.lower_agg_value_into(temp_place.clone(), else_body),
            )?;

            Ok(Value::Aggregate(temp_place))
        }
    }

    /// Lower an if-expression into a destination aggregate place.
    pub(super) fn lower_if_expr_into(
        &mut self,
        dst: Place<Aggregate>,
        cond: &ValueExpr,
        then_body: &ValueExpr,
        else_body: &ValueExpr,
    ) -> Result<(), LowerError> {
        let cond_op = self.lower_scalar_expr(cond)?;

        self.lower_if_join(
            cond_op,
            |this| this.lower_agg_value_into(dst.clone(), then_body),
            |this| this.lower_agg_value_into(dst.clone(), else_body),
        )?;

        Ok(())
    }

    /// Helper for lowering an if-expression.
    pub(super) fn lower_if_join<FThen, FElse>(
        &mut self,
        cond: Operand,
        then_fn: FThen,
        else_fn: FElse,
    ) -> Result<(), LowerError>
    where
        FThen: FnOnce(&mut Self) -> Result<(), LowerError>,
        FElse: FnOnce(&mut Self) -> Result<(), LowerError>,
    {
        let then_bb = self.fb.new_block();
        let else_bb = self.fb.new_block();
        let join_bb = self.fb.new_block();

        self.fb.set_terminator(
            self.curr_block,
            Terminator::If {
                cond,
                then_bb,
                else_bb,
            },
        );

        self.curr_block = then_bb;
        then_fn(self)?;
        if !self.is_curr_block_terminated() {
            self.fb
                .set_terminator(self.curr_block, Terminator::Goto(join_bb));
        }

        self.curr_block = else_bb;
        else_fn(self)?;
        if !self.is_curr_block_terminated() {
            self.fb
                .set_terminator(self.curr_block, Terminator::Goto(join_bb));
        }

        self.curr_block = join_bb;

        Ok(())
    }

    /// Lower a while expression (returns unit).
    pub(super) fn lower_while_expr(
        &mut self,
        cond: &ValueExpr,
        body: &ValueExpr,
    ) -> Result<Value, LowerError> {
        let loop_cond_bb = self.fb.new_block();
        let loop_body_bb = self.fb.new_block();
        let loop_exit_bb = self.fb.new_block();

        // Jump to loop condition
        self.fb
            .set_terminator(self.curr_block, Terminator::Goto(loop_cond_bb));

        // Loop condition
        self.curr_block = loop_cond_bb;
        let cond_op = self.lower_scalar_expr(cond)?;
        self.fb.set_terminator(
            self.curr_block,
            Terminator::If {
                cond: cond_op,
                then_bb: loop_body_bb,
                else_bb: loop_exit_bb,
            },
        );

        // Lower body
        self.curr_block = loop_body_bb;
        let VEK::Block { items, tail } = &body.kind else {
            return Err(LowerError::ExpectedBlock(body.id));
        };
        let body_ty = self.ty_from_id(body.ty);
        self.push_loop_context(loop_exit_bb, loop_cond_bb);
        self.lower_block_expr(&body_ty, items, tail.as_deref())?;
        self.pop_loop_context();
        if !self.is_curr_block_terminated() {
            self.fb
                .set_terminator(self.curr_block, Terminator::Goto(loop_cond_bb));
        }

        // After loop
        self.curr_block = loop_exit_bb;

        // while loops return unit
        let c = Const::Unit;
        Ok(Value::Scalar(Operand::Const(c)))
    }
}
