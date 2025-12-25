use crate::ast::Expr;
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{ExprValue, FuncLowerer};
use crate::mcir::types::*;

impl<'a> FuncLowerer<'a> {
    // --- Calls ---

    /// Lower a call expression and return the produced value.
    pub(super) fn lower_call_expr(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<ExprValue, LowerError> {
        let result_ty = self.ty_for_node(call.id)?;
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        if result_ty.is_scalar() {
            // Scalar call: capture result into a temp.
            let temp_place = self.new_temp_scalar(result_ty_id);
            self.emit_call_into(PlaceAny::Scalar(temp_place.clone()), callee, args)?;
            Ok(ExprValue::Scalar(Operand::Copy(temp_place)))
        } else {
            // Aggregate call: capture result into a temp place.
            let temp_place = self.new_temp_aggregate(result_ty_id);
            self.emit_call_into(PlaceAny::Aggregate(temp_place.clone()), callee, args)?;
            Ok(ExprValue::Aggregate(temp_place))
        }
    }

    /// Emit a call statement that writes into the given destination place.
    pub(super) fn emit_call_into(
        &mut self,
        dst: PlaceAny,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<(), LowerError> {
        let callee_def = self.def_for_node(callee.id)?;
        let callee_id = callee_def.id;

        let arg_vals = args
            .iter()
            .map(|a| self.lower_call_arg_place(a))
            .collect::<Result<Vec<_>, _>>()?;

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst,
                callee: Callee::Def(callee_id),
                args: arg_vals,
            },
        );
        Ok(())
    }

    /// Lower a call argument into a place (or temp if needed).
    pub(super) fn lower_call_arg_place(&mut self, arg: &Expr) -> Result<PlaceAny, LowerError> {
        let ty = self.ty_for_node(arg.id)?;
        let ty_id = self.ty_lowerer.lower_ty(&ty);

        if ty.is_scalar() {
            // Scalar arg: prefer a place, otherwise spill to temp.
            if let Ok(place) = self.lower_place_scalar(arg) {
                return Ok(PlaceAny::Scalar(place));
            }

            // Otherwise, evaluate to operand and spill into a temp.
            let op = self.lower_scalar_expr(arg)?;
            let temp_place = self.new_temp_scalar(ty_id);
            self.emit_copy_scalar(temp_place.clone(), Rvalue::Use(op));
            Ok(PlaceAny::Scalar(temp_place))
        } else {
            // Aggregate arg: prefer a place, otherwise lower into a temp.
            if let Ok(place) = self.lower_place_agg(arg) {
                Ok(PlaceAny::Aggregate(place))
            } else {
                Ok(PlaceAny::Aggregate(self.lower_agg_expr_to_temp(arg)?))
            }
        }
    }
}
