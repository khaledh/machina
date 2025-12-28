use crate::ast::{Expr, ExprKind};
use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{ExprValue, FuncLowerer};
use crate::mcir::abi::RuntimeFn;
use crate::mcir::types::*;
use crate::types::Type;

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

        if result_ty == Type::Unit {
            self.lower_call_into(None, callee, args)?;
            return Ok(ExprValue::Scalar(Operand::Const(Const::Unit)));
        }

        if result_ty.is_scalar() {
            // Scalar call: capture result into a temp.
            let temp_place = self.new_temp_scalar(result_ty_id);
            self.lower_call_into(Some(PlaceAny::Scalar(temp_place.clone())), callee, args)?;
            Ok(ExprValue::Scalar(Operand::Copy(temp_place)))
        } else {
            // Aggregate call: capture result into a temp place.
            let temp_place = self.new_temp_aggregate(result_ty_id);
            self.lower_call_into(Some(PlaceAny::Aggregate(temp_place.clone())), callee, args)?;
            Ok(ExprValue::Aggregate(temp_place))
        }
    }

    /// Lower a call into the given destination place.
    pub(super) fn lower_call_into(
        &mut self,
        dst: Option<PlaceAny>,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<(), LowerError> {
        // Resolve the callee to a runtime function or a function definition.
        let callee = match &callee.kind {
            ExprKind::Var(name) if name == "print" => Callee::Runtime(RuntimeFn::PrintStr),
            ExprKind::Var(name) if name == "println" => Callee::Runtime(RuntimeFn::PrintLn),
            _ => {
                let callee_def = self.def_for_node(callee.id)?;
                Callee::Def(callee_def.id)
            }
        };

        if let Callee::Runtime(runtime_fn) = &callee {
            if runtime_fn.sig().arg_count != args.len() as u8 {
                panic!(
                    concat!(
                        "compiler bug: runtime func {} takes {} arguments, but {} were provided. ",
                        "This should have been caught by the type checker."
                    ),
                    runtime_fn.sig().name,
                    runtime_fn.sig().arg_count,
                    args.len()
                );
            }
        }

        let arg_vals = args
            .iter()
            .map(|a| self.lower_call_arg_place(a))
            .collect::<Result<Vec<_>, _>>()?;

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst,
                callee,
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
