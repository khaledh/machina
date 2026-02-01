//! Call lowering.

use crate::diag::Span;
use crate::resolve::DefId;
use crate::ssa::lower::LoweringError;
use crate::ssa::lower::lowerer::{FuncLowerer, LinearValue};
use crate::ssa::model::ir::{Callee, RuntimeFn, ValueId};
use crate::tree::{InitInfo, ParamMode};
use crate::tree::{NodeId, semantic as sem};
use crate::types::Type;

enum OutProj<'a> {
    Struct(&'a str),
    Tuple(usize),
}

struct CallInputValue {
    value: ValueId,
    ty: Type,
    is_addr: bool,
    drop_def: Option<DefId>,
}

fn drop_def_for_value_expr(expr: &sem::ValueExpr) -> Option<DefId> {
    match &expr.kind {
        sem::ValueExprKind::Move { place }
        | sem::ValueExprKind::ImplicitMove { place }
        | sem::ValueExprKind::Load { place } => drop_def_for_place_expr(place),
        _ => None,
    }
}

fn drop_def_for_place_expr(place: &sem::PlaceExpr) -> Option<DefId> {
    match &place.kind {
        sem::PlaceExprKind::Var { def_id, .. } => Some(*def_id),
        sem::PlaceExprKind::ArrayIndex { target, .. }
        | sem::PlaceExprKind::TupleField { target, .. }
        | sem::PlaceExprKind::StructField { target, .. } => drop_def_for_place_expr(target),
        sem::PlaceExprKind::Deref { value } => drop_def_for_value_expr(value),
    }
}

impl<'a, 'g> FuncLowerer<'a, 'g> {
    fn call_input_from_value_expr(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<Option<CallInputValue>, LoweringError> {
        let drop_def = drop_def_for_value_expr(expr);
        let Some(value) = self.lower_value_expr_opt(expr)? else {
            return Ok(None);
        };
        let ty = self.type_map.type_table().get(expr.ty).clone();
        Ok(Some(CallInputValue {
            value,
            ty,
            is_addr: false,
            drop_def,
        }))
    }

    fn call_input_from_place_expr(
        &mut self,
        place: &sem::PlaceExpr,
    ) -> Result<CallInputValue, LoweringError> {
        let drop_def = drop_def_for_place_expr(place);
        let addr = self.lower_place_addr(place)?;
        let ty = self.type_map.type_table().get(place.ty).clone();
        Ok(CallInputValue {
            value: addr.addr,
            ty,
            is_addr: true,
            drop_def,
        })
    }

    fn lower_call_arg_values(
        &mut self,
        args: &[sem::CallArg],
    ) -> Result<Option<Vec<CallInputValue>>, LoweringError> {
        let mut arg_values = Vec::with_capacity(args.len());
        for arg in args {
            match arg {
                sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
                    let Some(input) = self.call_input_from_value_expr(expr)? else {
                        return Ok(None);
                    };
                    arg_values.push(input);
                }
                sem::CallArg::InOut { place, .. } | sem::CallArg::Out { place, .. } => {
                    arg_values.push(self.call_input_from_place_expr(place)?);
                }
            }
        }
        Ok(Some(arg_values))
    }

    fn mark_out_init_flags(&mut self, args: &[sem::CallArg], arg_values: &[CallInputValue]) {
        for (arg, input) in args.iter().zip(arg_values.iter()) {
            match arg {
                sem::CallArg::Out { place, init, .. } => {
                    let Some(def_id) = input.drop_def else {
                        continue;
                    };
                    let should_set = match place.kind {
                        sem::PlaceExprKind::Var { .. } => init.is_init || init.promotes_full,
                        _ => init.promotes_full || self.out_place_promotes_full(place, init),
                    };
                    if should_set {
                        self.set_drop_flag_for_def(def_id, true);
                    }
                }
                sem::CallArg::InOut { .. } => {
                    if let Some(def_id) = input.drop_def {
                        self.set_drop_flag_for_def(def_id, true);
                    }
                }
                _ => {}
            }
        }
    }

    fn out_place_promotes_full(&self, place: &sem::PlaceExpr, init: &InitInfo) -> bool {
        if init.promotes_full || !init.is_init {
            return false;
        }

        let (base_def, proj) = match &place.kind {
            sem::PlaceExprKind::StructField { target, field } => match &target.kind {
                sem::PlaceExprKind::Var { def_id, .. } => {
                    (*def_id, OutProj::Struct(field.as_str()))
                }
                _ => return false,
            },
            sem::PlaceExprKind::TupleField { target, index } => match &target.kind {
                sem::PlaceExprKind::Var { def_id, .. } => (*def_id, OutProj::Tuple(*index)),
                _ => return false,
            },
            _ => return false,
        };

        let base_ty = self.def_type(base_def);
        match (proj, base_ty) {
            (OutProj::Struct(field), Type::Struct { fields, .. }) => {
                fields.len() == 1 && fields[0].name == *field
            }
            (OutProj::Tuple(index), Type::Tuple { field_tys }) => {
                field_tys.len() == 1 && index == 0
            }
            _ => false,
        }
    }

    /// Lowers a call expression, returning `None` if a subexpression returns.
    pub(super) fn lower_call_expr(
        &mut self,
        expr: &sem::ValueExpr,
        callee_expr: &sem::ValueExpr,
        args: &[sem::CallArg],
    ) -> Result<Option<LinearValue>, LoweringError> {
        let call_plan = self.call_plan(expr.id);

        // Direct calls (no receiver) only for now.
        if call_plan.has_receiver {
            panic!("ssa lower_call_expr expected no receiver for {:?}", expr.id);
        }

        // Resolve the callee.
        let callee = match &call_plan.target {
            sem::CallTarget::Direct(def_id) => Callee::Direct(*def_id),
            sem::CallTarget::Indirect => {
                let Some(callee_value) = self.lower_value_expr_opt(callee_expr)? else {
                    return Ok(None);
                };
                Callee::Value(callee_value)
            }
            sem::CallTarget::Intrinsic(intrinsic) => match intrinsic {
                sem::IntrinsicCall::StringLen => {
                    panic!("ssa call expr cannot lower string len without a receiver");
                }
            },
            sem::CallTarget::Runtime(runtime) => Callee::Runtime(self.runtime_for_call(runtime)?),
        };

        // Lower argument expressions.
        let Some(mut arg_values) = self.lower_call_arg_values(args)? else {
            return Ok(None);
        };

        // Apply the call plan to reorder/transform arguments.
        let call_args =
            self.lower_call_args_from_plan(expr.id, expr.span, &call_plan, None, &mut arg_values)?;
        let ty = self.type_lowerer.lower_type_id(expr.ty);
        let result = self.builder.call(callee, call_args, ty);
        self.emit_call_drops(&call_plan, None, &arg_values)?;
        self.clear_sink_drop_flags(&call_plan, None, &arg_values);
        self.mark_out_init_flags(args, &arg_values);
        Ok(Some(result))
    }

    /// Lowers a method call expression, returning `None` if a subexpression returns.
    pub(super) fn lower_method_call_expr(
        &mut self,
        expr: &sem::ValueExpr,
        receiver: &sem::MethodReceiver,
        args: &[sem::CallArg],
    ) -> Result<Option<LinearValue>, LoweringError> {
        let call_plan = self.call_plan(expr.id);

        // Method calls must have a receiver.
        if !call_plan.has_receiver {
            panic!(
                "ssa lower_method_call_expr missing receiver for {:?}",
                expr.id
            );
        }

        // Lower the receiver.
        let mut receiver_value = match receiver {
            sem::MethodReceiver::ValueExpr(expr) => {
                let Some(input) = self.call_input_from_value_expr(expr)? else {
                    return Ok(None);
                };
                input
            }
            sem::MethodReceiver::PlaceExpr(place) => self.call_input_from_place_expr(place)?,
        };

        // Resolve the callee.
        let callee = match &call_plan.target {
            sem::CallTarget::Direct(def_id) => Callee::Direct(*def_id),
            sem::CallTarget::Indirect => {
                // For indirect method calls, treat the receiver as the callee value.
                // The call plan still governs how the receiver/args are passed.
                let callee_value = match receiver {
                    sem::MethodReceiver::ValueExpr(_) => receiver_value.value,
                    sem::MethodReceiver::PlaceExpr(_) => {
                        let ty = self.type_lowerer.lower_type(&receiver_value.ty);
                        self.builder.load(receiver_value.value, ty)
                    }
                };
                Callee::Value(callee_value)
            }
            sem::CallTarget::Intrinsic(intrinsic) => {
                if matches!(intrinsic, sem::IntrinsicCall::StringLen) {
                    // String length is a field load; avoid emitting a runtime call.
                    return self
                        .lower_string_len_method(expr.span, receiver, &receiver_value)
                        .map(Some);
                }
                panic!("ssa method call intrinsic {:?} not supported", intrinsic);
            }
            sem::CallTarget::Runtime(runtime) => Callee::Runtime(self.runtime_for_call(runtime)?),
        };

        // Lower argument expressions.
        let Some(mut arg_values) = self.lower_call_arg_values(args)? else {
            return Ok(None);
        };

        // Apply the call plan to build the final argument list.
        let call_args = self.lower_call_args_from_plan(
            expr.id,
            expr.span,
            &call_plan,
            Some(&mut receiver_value),
            &mut arg_values,
        )?;
        let ty = self.type_lowerer.lower_type_id(expr.ty);
        let result = self.builder.call(callee, call_args, ty);
        self.emit_call_drops(&call_plan, Some(&receiver_value), &arg_values)?;
        self.clear_sink_drop_flags(&call_plan, Some(&receiver_value), &arg_values);
        self.mark_out_init_flags(args, &arg_values);
        Ok(Some(result))
    }

    fn emit_call_drops(
        &mut self,
        call_plan: &sem::CallPlan,
        receiver_value: Option<&CallInputValue>,
        arg_values: &[CallInputValue],
    ) -> Result<(), LoweringError> {
        let expected = (call_plan.has_receiver as usize) + arg_values.len();
        if call_plan.drop_mask.len() != expected {
            panic!(
                "ssa call drop mask length mismatch: expected {}, got {}",
                expected,
                call_plan.drop_mask.len()
            );
        }

        let mut input_index = 0;
        if call_plan.has_receiver {
            let receiver = receiver_value.unwrap_or_else(|| {
                panic!("ssa call drop mask missing receiver value for call plan");
            });
            if call_plan.drop_mask[input_index] {
                if receiver.drop_def.is_none() {
                    self.emit_drop_for_value(receiver.value, &receiver.ty, receiver.is_addr)?;
                }
            }
            input_index += 1;
        }

        for arg in arg_values {
            if call_plan.drop_mask[input_index] {
                if arg.drop_def.is_none() {
                    self.emit_drop_for_value(arg.value, &arg.ty, arg.is_addr)?;
                }
            }
            input_index += 1;
        }

        Ok(())
    }

    fn clear_sink_drop_flags(
        &mut self,
        call_plan: &sem::CallPlan,
        receiver_value: Option<&CallInputValue>,
        arg_values: &[CallInputValue],
    ) {
        let expected = (call_plan.has_receiver as usize) + arg_values.len();
        if call_plan.input_modes.len() != expected {
            panic!(
                "ssa call input modes length mismatch: expected {}, got {}",
                expected,
                call_plan.input_modes.len()
            );
        }

        let mut input_index = 0;
        if call_plan.has_receiver {
            let receiver = receiver_value.unwrap_or_else(|| {
                panic!("ssa call input modes missing receiver value for call plan");
            });
            if call_plan.input_modes[input_index] == ParamMode::Sink {
                self.clear_sink_flag_for_input(receiver);
            }
            input_index += 1;
        }

        for arg in arg_values {
            if call_plan.input_modes[input_index] == ParamMode::Sink {
                self.clear_sink_flag_for_input(arg);
            }
            input_index += 1;
        }
    }

    fn clear_sink_flag_for_input(&mut self, input: &CallInputValue) {
        let Some(def_id) = input.drop_def else {
            return;
        };
        if input.ty.needs_drop() {
            self.set_drop_flag_for_def(def_id, false);
        }
    }

    fn lower_call_args_from_plan(
        &mut self,
        expr_id: NodeId,
        span: Span,
        call_plan: &sem::CallPlan,
        mut receiver_value: Option<&mut CallInputValue>,
        arg_values: &mut [CallInputValue],
    ) -> Result<Vec<ValueId>, LoweringError> {
        if call_plan.has_receiver != receiver_value.is_some() {
            panic!(
                "ssa lower_call_args_from_plan receiver mismatch for {:?}",
                expr_id
            );
        }

        let mut call_args = Vec::with_capacity(call_plan.args.len());
        for lowering in &call_plan.args {
            match lowering {
                sem::ArgLowering::Direct(input) => {
                    let input_value = match input {
                        sem::CallInput::Receiver => {
                            receiver_value.as_deref_mut().unwrap_or_else(|| {
                                panic!("ssa call plan missing receiver value for {:?}", expr_id)
                            })
                        }
                        sem::CallInput::Arg(index) => arg_values
                            .get_mut(*index)
                            .unwrap_or_else(|| panic!("ssa call arg index out of range: {index}")),
                    };

                    if input_value.is_addr || input_value.ty.is_scalar() {
                        call_args.push(input_value.value);
                    } else {
                        let addr = self.materialize_value_addr(input_value.value, &input_value.ty);
                        input_value.value = addr;
                        input_value.is_addr = true;
                        call_args.push(addr);
                    }
                }
                sem::ArgLowering::PtrLen { input, len_bits } => {
                    let input_value = match input {
                        sem::CallInput::Receiver => {
                            receiver_value.as_deref().unwrap_or_else(|| {
                                panic!("ssa call plan missing receiver value for {:?}", expr_id)
                            })
                        }
                        sem::CallInput::Arg(index) => arg_values
                            .get(*index)
                            .unwrap_or_else(|| panic!("ssa call arg index out of range: {index}")),
                    };
                    let (ptr, len) = self.lower_ptr_len_from_value(
                        span,
                        input_value.value,
                        &input_value.ty,
                        *len_bits,
                    )?;
                    call_args.push(ptr);
                    call_args.push(len);
                }
            }
        }
        Ok(call_args)
    }

    fn runtime_for_call(&self, runtime: &sem::RuntimeCall) -> Result<RuntimeFn, LoweringError> {
        match runtime {
            sem::RuntimeCall::Print => Ok(RuntimeFn::Print),
            sem::RuntimeCall::U64ToDec => Ok(RuntimeFn::U64ToDec),
            sem::RuntimeCall::MemSet => Ok(RuntimeFn::MemSet),
            sem::RuntimeCall::StringFromBytes => Ok(RuntimeFn::StringFromBytes),
            sem::RuntimeCall::StringAppendBytes => Ok(RuntimeFn::StringAppendBytes),
        }
    }

    fn lower_string_len_method(
        &mut self,
        span: Span,
        receiver: &sem::MethodReceiver,
        receiver_value: &CallInputValue,
    ) -> Result<ValueId, LoweringError> {
        if !matches!(receiver_value.ty, Type::String) {
            panic!(
                "ssa string len intrinsic expects string receiver, got {:?}",
                receiver_value.ty
            );
        }

        let len = match receiver {
            sem::MethodReceiver::ValueExpr(_) => {
                let (_ptr, len) = self.lower_ptr_len_from_value(
                    span,
                    receiver_value.value,
                    &receiver_value.ty,
                    32,
                )?;
                len
            }
            sem::MethodReceiver::PlaceExpr(_) => {
                let view = self.load_string_view(receiver_value.value);
                view.len
            }
        };

        Ok(len)
    }
}
