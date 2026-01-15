use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{FuncLowerer, Value};
use crate::mcir::types::*;
use crate::resolve::DefKind;
use crate::tree::InitInfo;
use crate::tree::semantic::{
    CallArg, MethodReceiver, PlaceExpr, PlaceExprKind as PEK, ValueExpr, ValueExprKind as VEK,
};
use crate::types::Type;

impl<'a> FuncLowerer<'a> {
    // --- Calls ---

    /// Lower a call expression and return the produced value.
    pub(super) fn lower_call_expr(
        &mut self,
        call: &ValueExpr,
        callee: &ValueExpr,
        args: &[CallArg],
    ) -> Result<Value, LowerError> {
        let result_ty = self.ty_from_id(call.ty);
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        if result_ty == Type::Unit {
            self.lower_call_into(None, call, Some(callee), None, args)?;
            return Ok(Value::Scalar(Operand::Const(Const::Unit)));
        }

        if result_ty.is_scalar() {
            // Scalar call: capture result into a temp.
            let temp_place = self.new_temp_scalar(result_ty_id);
            self.lower_call_into(
                Some(PlaceAny::Scalar(temp_place.clone())),
                call,
                Some(callee),
                None,
                args,
            )?;
            Ok(Value::Scalar(Operand::Copy(temp_place)))
        } else {
            // Aggregate call: capture result into a temp place.
            let temp_place = self.new_temp_aggregate(result_ty_id);
            self.lower_call_into(
                Some(PlaceAny::Aggregate(temp_place.clone())),
                call,
                Some(callee),
                None,
                args,
            )?;
            Ok(Value::Aggregate(temp_place))
        }
    }

    pub(super) fn lower_method_call_expr(
        &mut self,
        call: &ValueExpr,
        receiver: &MethodReceiver,
        args: &[CallArg],
    ) -> Result<Value, LowerError> {
        let result_ty = self.ty_from_id(call.ty);
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        if result_ty == Type::Unit {
            self.lower_call_into(None, call, None, Some(receiver), args)?;
            return Ok(Value::Scalar(Operand::Const(Const::Unit)));
        }

        if result_ty.is_scalar() {
            let temp_place = self.new_temp_scalar(result_ty_id);
            self.lower_call_into(
                Some(PlaceAny::Scalar(temp_place.clone())),
                call,
                None,
                Some(receiver),
                args,
            )?;
            Ok(Value::Scalar(Operand::Copy(temp_place)))
        } else {
            let temp_place = self.new_temp_aggregate(result_ty_id);
            self.lower_call_into(
                Some(PlaceAny::Aggregate(temp_place.clone())),
                call,
                None,
                Some(receiver),
                args,
            )?;
            Ok(Value::Aggregate(temp_place))
        }
    }

    /// Lower a call into the given destination place.
    pub(super) fn lower_call_into(
        &mut self,
        dst: Option<PlaceAny>,
        call: &ValueExpr,
        callee: Option<&ValueExpr>,
        receiver: Option<&MethodReceiver>,
        args: &[CallArg],
    ) -> Result<(), LowerError> {
        let callee = match self.ctx.type_map.lookup_call_def(call.id) {
            Some(def_id) => Callee::Def(def_id),
            None => {
                let Some(callee) = callee else {
                    panic!("compiler bug: missing callee value for indirect call");
                };
                Callee::Value(self.lower_scalar_expr(callee)?)
            }
        };

        if let Callee::Runtime(runtime_fn) = &callee
            && runtime_fn.sig().arg_count != args.len() as u8
        {
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

        let mut out_args = Vec::new();
        let mut arg_vals = Vec::with_capacity(args.len() + receiver.iter().count());

        if let Some(receiver) = receiver {
            arg_vals.push(self.lower_method_receiver(receiver)?);
        }

        for arg in args {
            arg_vals.push(self.lower_call_arg(arg, &mut out_args)?);
        }

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst,
                callee,
                args: arg_vals,
            },
        );

        for (arg, init) in out_args {
            // Mark out args as initialized after the call.
            self.mark_initialized_if_needed(arg, init);
            self.mark_full_init_if_needed(arg, init);
        }
        Ok(())
    }

    fn lower_method_receiver(&mut self, receiver: &MethodReceiver) -> Result<PlaceAny, LowerError> {
        match receiver {
            MethodReceiver::ValueExpr(value) => self.lower_call_arg_value(value.as_ref()),
            MethodReceiver::PlaceExpr(place) => self.lower_place(place),
        }
    }

    fn lower_call_arg<'b>(
        &mut self,
        arg: &'b CallArg,
        out_args: &mut Vec<(&'b PlaceExpr, InitInfo)>,
    ) -> Result<PlaceAny, LowerError> {
        match arg {
            CallArg::In { expr, .. } | CallArg::Sink { expr, .. } => {
                self.lower_call_arg_value(expr)
            }
            CallArg::InOut { place, .. } => self.lower_place(place),
            CallArg::Out { place, init, .. } => {
                let place_any = self.lower_place(place)?;
                let arg_ty = self.ty_from_id(place.ty);
                self.emit_overwrite_drop(place, place_any.clone(), &arg_ty, *init, true);
                out_args.push((place, *init));
                Ok(place_any)
            }
        }
    }

    fn lower_call_arg_value(&mut self, expr: &ValueExpr) -> Result<PlaceAny, LowerError> {
        let ty = self.ty_from_id(expr.ty);
        let ty_id = self.ty_lowerer.lower_ty(&ty);

        if ty.is_scalar() {
            if let VEK::Load { place } | VEK::Move { place } | VEK::ImplicitMove { place } =
                &expr.kind
            {
                if let PEK::Var { def_id, .. } = place.kind {
                    let def = self.def_for_id(def_id, place.id)?;
                    if !matches!(def.kind, DefKind::FuncDef | DefKind::FuncDecl) {
                        if matches!(expr.kind, VEK::Move { .. } | VEK::ImplicitMove { .. }) {
                            self.record_move_place(place);
                        }
                        return Ok(PlaceAny::Scalar(self.lower_place_scalar(place)?));
                    }
                }
            }

            let op = self.lower_scalar_expr(expr)?;
            let temp_place = self.new_temp_scalar(ty_id);
            self.emit_copy_scalar(temp_place.clone(), Rvalue::Use(op));
            Ok(PlaceAny::Scalar(temp_place))
        } else {
            if let VEK::Load { place } | VEK::Move { place } | VEK::ImplicitMove { place } =
                &expr.kind
            {
                if matches!(expr.kind, VEK::Move { .. } | VEK::ImplicitMove { .. }) {
                    self.record_move_place(place);
                }
                return Ok(PlaceAny::Aggregate(self.lower_place_agg(place)?));
            }
            Ok(PlaceAny::Aggregate(self.lower_agg_expr_to_temp(expr)?))
        }
    }
}
