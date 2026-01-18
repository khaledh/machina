use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{FuncLowerer, Value};
use crate::mcir::abi::RuntimeFn;
use crate::mcir::types::*;
use crate::resolve::DefKind;
use crate::tree::semantic::{
    CallArg, MethodReceiver, PlaceExpr, PlaceExprKind as PEK, ValueExpr, ValueExprKind as VEK,
};
use crate::tree::{InitInfo, ParamMode};
use crate::typeck::type_map::CallParam;
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
        if let Some(receiver) = receiver {
            if self.try_lower_intrinsic_method_call(call, receiver, args, dst.as_ref())? {
                return Ok(());
            }
        }

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

        let temp_drops = self.collect_temp_arg_drops(call, receiver.is_some(), &arg_vals);

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

        for (place, ty) in temp_drops {
            self.emit_drop_place(place, &ty);
        }
        Ok(())
    }

    fn try_lower_intrinsic_method_call(
        &mut self,
        call: &ValueExpr,
        receiver: &MethodReceiver,
        args: &[CallArg],
        dst: Option<&PlaceAny>,
    ) -> Result<bool, LowerError> {
        let Some(def_id) = self.ctx.type_map.lookup_call_def(call.id) else {
            return Ok(false);
        };
        let def = self.def_for_id(def_id, call.id)?;
        if !def.is_intrinsic() {
            return Ok(false);
        }
        let Some(link_name) = def.link_name() else {
            return Ok(false);
        };

        match link_name {
            "__mc_string_append_bytes" => {
                self.lower_intrinsic_string_append(call, receiver, args, dst)?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn lower_intrinsic_string_append(
        &mut self,
        call: &ValueExpr,
        receiver: &MethodReceiver,
        args: &[CallArg],
        dst: Option<&PlaceAny>,
    ) -> Result<(), LowerError> {
        if dst.is_some() || args.len() != 1 {
            return Err(LowerError::UnsupportedOperandExpr(call.id));
        }

        let receiver_place = self.lower_method_receiver(receiver)?;
        let PlaceAny::Aggregate(receiver_place) = receiver_place else {
            return Err(LowerError::UnsupportedOperandExpr(call.id));
        };

        let (arg_expr, arg_ty) = match &args[0] {
            CallArg::In { expr, .. } | CallArg::Sink { expr, .. } => {
                (expr, self.ty_from_id(expr.ty))
            }
            CallArg::InOut { .. } | CallArg::Out { .. } => {
                return Err(LowerError::UnsupportedOperandExpr(call.id));
            }
        };

        let arg_place = self.lower_call_arg_value(arg_expr)?;
        let PlaceAny::Aggregate(arg_place) = arg_place else {
            return Err(LowerError::UnsupportedOperandExpr(call.id));
        };

        let (ptr_op, len_op) = match arg_ty {
            Type::String => self.string_ptr_len_ops(&arg_place),
            Type::Slice { elem_ty }
                if matches!(
                    *elem_ty,
                    Type::Int {
                        signed: false,
                        bits: 8
                    }
                ) =>
            {
                self.slice_ptr_len_ops(&arg_place)
            }
            _ => return Err(LowerError::UnsupportedOperandExpr(call.id)),
        };
        let ptr_arg = self.runtime_arg_place(ptr_op);
        let len_arg = self.runtime_arg_place(len_op);

        self.fb.push_stmt(
            self.curr_block,
            Statement::Call {
                dst: None,
                callee: Callee::Runtime(RuntimeFn::StringAppendBytes),
                args: vec![PlaceAny::Aggregate(receiver_place), ptr_arg, len_arg],
            },
        );

        Ok(())
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

    fn collect_temp_arg_drops(
        &mut self,
        call: &ValueExpr,
        has_receiver: bool,
        args: &[PlaceAny],
    ) -> Vec<(PlaceAny, Type)> {
        let Some(call_sig) = self.ctx.type_map.lookup_call_sig(call.id) else {
            return Vec::new();
        };

        let mut params = Vec::new();
        if has_receiver {
            if let Some(receiver) = call_sig.receiver {
                params.push(receiver);
            }
        }
        params.extend(call_sig.params);

        args.iter()
            .zip(params.iter())
            .filter_map(|(place, param)| self.temp_drop_for_arg(place, param))
            .collect()
    }

    fn temp_drop_for_arg(
        &mut self,
        place: &PlaceAny,
        param: &CallParam,
    ) -> Option<(PlaceAny, Type)> {
        if param.mode != ParamMode::In || !param.ty.needs_drop() {
            return None;
        }

        let base = match place {
            PlaceAny::Scalar(p) => p.base(),
            PlaceAny::Aggregate(p) => p.base(),
        };
        if self.fb.body.locals[base.index()].kind != LocalKind::Temp {
            return None;
        }

        Some((place.clone(), param.ty.clone()))
    }
}
