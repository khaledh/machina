use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{FuncLowerer, Value};
use crate::mcir::abi::RuntimeFn;
use crate::mcir::types::*;
use crate::resolve::DefKind;
use crate::tree::InitInfo;
use crate::tree::semantic::{
    ArgLowering, CallArg, CallInput, CallPlan, CallTarget, IntrinsicCall, MethodReceiver,
    PlaceExpr, PlaceExprKind as PEK, RuntimeCall, ValueExpr, ValueExprKind as VEK,
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
        let call_plan = self.call_plan_for(call)?;
        let callee = self.lower_call_target(callee, &call_plan)?;

        let mut out_args = Vec::new();
        let mut inputs = Vec::with_capacity(args.len() + receiver.iter().count());
        let mut input_types = Vec::with_capacity(args.len() + receiver.iter().count());

        if let Some(receiver) = receiver {
            inputs.push(self.lower_method_receiver(receiver)?);
            input_types.push(self.receiver_type(receiver));
        }

        for arg in args {
            inputs.push(self.lower_call_arg(arg, &mut out_args)?);
            input_types.push(self.call_arg_type(arg));
        }

        let arg_vals = self.lower_call_args_from_plan(call, &call_plan, &inputs)?;

        if let Callee::Runtime(runtime_fn) = &callee
            && runtime_fn.sig().arg_count != arg_vals.len() as u8
        {
            panic!(
                concat!(
                    "compiler bug: runtime func {} takes {} arguments, but {} were provided. ",
                    "This should have been caught by the type checker."
                ),
                runtime_fn.sig().name,
                runtime_fn.sig().arg_count,
                arg_vals.len()
            );
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

        for (place, ty) in self.collect_temp_drops_from_plan(&call_plan, &inputs, &input_types) {
            self.emit_drop_place(place, &ty);
        }
        Ok(())
    }

    fn call_plan_for(&mut self, call: &ValueExpr) -> Result<CallPlan, LowerError> {
        self.ctx
            .type_map
            .lookup_call_plan(call.id)
            .ok_or_else(|| LowerError::UnsupportedOperandExpr(call.id))
    }

    fn lower_call_target(
        &mut self,
        callee: Option<&ValueExpr>,
        call_plan: &CallPlan,
    ) -> Result<Callee, LowerError> {
        match &call_plan.target {
            CallTarget::Direct(def_id) => Ok(Callee::Def(*def_id)),
            CallTarget::Indirect => {
                let Some(callee) = callee else {
                    panic!("compiler bug: missing callee value for indirect call");
                };
                Ok(Callee::Value(self.lower_scalar_expr(callee)?))
            }
            CallTarget::Intrinsic(intrinsic) => match intrinsic {
                IntrinsicCall::StringLen => {
                    panic!("mcir lowering does not support string len intrinsic")
                }
            },
            CallTarget::Runtime(runtime) => Ok(Callee::Runtime(self.runtime_for_call(runtime)?)),
        }
    }

    fn runtime_for_call(&mut self, runtime: &RuntimeCall) -> Result<RuntimeFn, LowerError> {
        match runtime {
            RuntimeCall::Print => Ok(RuntimeFn::Print),
            RuntimeCall::U64ToDec => Ok(RuntimeFn::U64ToDec),
            RuntimeCall::MemSet => Ok(RuntimeFn::MemSet),
            RuntimeCall::StringFromBytes => Ok(RuntimeFn::StringFromBytes),
            RuntimeCall::StringAppendBytes => Ok(RuntimeFn::StringAppendBytes),
        }
    }

    fn lower_call_args_from_plan(
        &mut self,
        call: &ValueExpr,
        call_plan: &CallPlan,
        inputs: &[PlaceAny],
    ) -> Result<Vec<PlaceAny>, LowerError> {
        let mut args = Vec::new();
        for lowering in &call_plan.args {
            match lowering {
                ArgLowering::Direct(input) => {
                    args.push(self.call_input_place(call, call_plan, inputs, input)?);
                }
                ArgLowering::PtrLen { input, len_bits } => {
                    let place = self.call_input_place(call, call_plan, inputs, input)?;
                    let PlaceAny::Aggregate(place) = place else {
                        return Err(LowerError::UnsupportedOperandExpr(call.id));
                    };
                    let (ptr_op, len_op) = match len_bits {
                        32 => self.string_ptr_len_ops(&place),
                        64 => self.slice_ptr_len_ops(&place),
                        _ => panic!("compiler bug: invalid ptr/len width"),
                    };
                    args.push(self.runtime_arg_place(ptr_op));
                    args.push(self.runtime_arg_place(len_op));
                }
            }
        }
        Ok(args)
    }

    fn call_input_place(
        &mut self,
        call: &ValueExpr,
        call_plan: &CallPlan,
        inputs: &[PlaceAny],
        input: &CallInput,
    ) -> Result<PlaceAny, LowerError> {
        let index = match input {
            CallInput::Receiver => {
                if !call_plan.has_receiver {
                    return Err(LowerError::UnsupportedOperandExpr(call.id));
                }
                0
            }
            CallInput::Arg(arg_index) => {
                if call_plan.has_receiver {
                    1 + *arg_index
                } else {
                    *arg_index
                }
            }
        };
        inputs
            .get(index)
            .cloned()
            .ok_or_else(|| LowerError::UnsupportedOperandExpr(call.id))
    }

    pub(super) fn string_ptr_len_ops(
        &mut self,
        string_place: &Place<Aggregate>,
    ) -> (Operand, Operand) {
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let u32_ty_id = self.ty_lowerer.lower_ty(&Type::uint(32));

        let mut ptr_proj = string_place.projections().to_vec();
        ptr_proj.push(Projection::Field { index: 0 });
        let ptr_place = Place::new(string_place.base(), u64_ty_id, ptr_proj);
        let ptr_op = Operand::Copy(ptr_place);

        let mut len_proj = string_place.projections().to_vec();
        len_proj.push(Projection::Field { index: 1 });
        let len_place = Place::new(string_place.base(), u32_ty_id, len_proj);
        let len_u64_place = self.new_temp_scalar(u64_ty_id);
        self.emit_copy_scalar(len_u64_place.clone(), Rvalue::Use(Operand::Copy(len_place)));
        let len_op = Operand::Copy(len_u64_place);

        (ptr_op, len_op)
    }

    pub(super) fn slice_ptr_len_ops(
        &mut self,
        slice_place: &Place<Aggregate>,
    ) -> (Operand, Operand) {
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));

        let mut ptr_proj = slice_place.projections().to_vec();
        ptr_proj.push(Projection::Field { index: 0 });
        let ptr_place = Place::new(slice_place.base(), u64_ty_id, ptr_proj);
        let ptr_op = Operand::Copy(ptr_place);

        let mut len_proj = slice_place.projections().to_vec();
        len_proj.push(Projection::Field { index: 1 });
        let len_place = Place::new(slice_place.base(), u64_ty_id, len_proj);
        let len_op = Operand::Copy(len_place);

        (ptr_op, len_op)
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
                    if !matches!(def.kind, DefKind::FuncDef { .. } | DefKind::FuncDecl { .. }) {
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

    fn collect_temp_drops_from_plan(
        &mut self,
        call_plan: &CallPlan,
        inputs: &[PlaceAny],
        input_types: &[Type],
    ) -> Vec<(PlaceAny, Type)> {
        if input_types.len() != inputs.len() || input_types.len() != call_plan.drop_mask.len() {
            panic!("compiler bug: call input mismatch");
        }

        input_types
            .iter()
            .cloned()
            .zip(inputs.iter())
            .zip(call_plan.drop_mask.iter().copied())
            .filter_map(|((ty, place), should_drop)| {
                if !should_drop || !ty.needs_drop() {
                    return None;
                }
                self.temp_drop_for_input(place, ty)
            })
            .collect()
    }

    fn receiver_type(&self, receiver: &MethodReceiver) -> Type {
        match receiver {
            MethodReceiver::ValueExpr(expr) => self.ty_from_id(expr.ty),
            MethodReceiver::PlaceExpr(place) => self.ty_from_id(place.ty),
        }
    }

    fn call_arg_type(&self, arg: &CallArg) -> Type {
        match arg {
            CallArg::In { expr, .. } | CallArg::Sink { expr, .. } => self.ty_from_id(expr.ty),
            CallArg::InOut { place, .. } | CallArg::Out { place, .. } => self.ty_from_id(place.ty),
        }
    }

    fn temp_drop_for_input(&mut self, place: &PlaceAny, ty: Type) -> Option<(PlaceAny, Type)> {
        let base = match place {
            PlaceAny::Scalar(p) => p.base(),
            PlaceAny::Aggregate(p) => p.base(),
        };
        if self.fb.body.locals[base.index()].kind != LocalKind::Temp {
            return None;
        }

        Some((place.clone(), ty))
    }
}
