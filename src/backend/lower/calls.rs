//! Call lowering.

use crate::backend::lower::LowerToIrError;
use crate::backend::lower::lowerer::{FuncLowerer, LinearValue};
use crate::diag::Span;
use crate::ir::{Callee, RuntimeFn, ValueId};
use crate::resolve::DefId;
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
    ) -> Result<Option<CallInputValue>, LowerToIrError> {
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
    ) -> Result<CallInputValue, LowerToIrError> {
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
    ) -> Result<Option<Vec<CallInputValue>>, LowerToIrError> {
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

    fn call_param_types(
        &self,
        call_plan: &sem::CallPlan,
        callee_expr: Option<&sem::ValueExpr>,
        receiver_value: Option<&CallInputValue>,
    ) -> Option<Vec<Type>> {
        let fn_ty = match &call_plan.target {
            sem::CallTarget::Direct(def_id) => {
                let def = self.def_table.lookup_def(*def_id)?;
                self.type_map.lookup_def_type(def)?
            }
            sem::CallTarget::Indirect => {
                if let Some(callee_expr) = callee_expr {
                    self.type_map.type_table().get(callee_expr.ty).clone()
                } else if let Some(receiver_value) = receiver_value {
                    receiver_value.ty.clone()
                } else {
                    return None;
                }
            }
            sem::CallTarget::Intrinsic(_) | sem::CallTarget::Runtime(_) => {
                let callee_expr = callee_expr?;
                self.type_map.type_table().get(callee_expr.ty).clone()
            }
        };

        let Type::Fn { params, .. } = fn_ty else {
            return None;
        };
        Some(params.into_iter().map(|param| param.ty).collect())
    }

    fn emit_call_conversion_checks(
        &mut self,
        call_plan: &sem::CallPlan,
        receiver_value: Option<&CallInputValue>,
        arg_values: &[CallInputValue],
        param_tys: &[Type],
    ) {
        let expected = (call_plan.has_receiver as usize) + arg_values.len();
        if call_plan.input_modes.len() != expected || param_tys.len() != expected {
            return;
        }

        let mut input_index = 0;
        if call_plan.has_receiver {
            let receiver = receiver_value.unwrap_or_else(|| {
                panic!("backend call conversion checks missing receiver value for call plan");
            });
            if matches!(
                call_plan.input_modes[input_index],
                ParamMode::In | ParamMode::Sink
            ) && !receiver.is_addr
            {
                self.emit_conversion_check(&receiver.ty, &param_tys[input_index], receiver.value);
            }
            input_index += 1;
        }

        for arg in arg_values {
            if matches!(
                call_plan.input_modes[input_index],
                ParamMode::In | ParamMode::Sink
            ) && !arg.is_addr
            {
                self.emit_conversion_check(&arg.ty, &param_tys[input_index], arg.value);
            }
            input_index += 1;
        }
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
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        let call_plan = self.call_plan(expr.id);

        // Direct calls (no receiver) only for now.
        if call_plan.has_receiver {
            panic!(
                "backend lower_call_expr expected no receiver for {:?}",
                expr.id
            );
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
                    panic!("backend call expr cannot lower string len without a receiver");
                }
                sem::IntrinsicCall::DynArrayAppend => {
                    panic!("backend call expr cannot lower dyn-array intrinsic without a receiver");
                }
                sem::IntrinsicCall::SetInsert
                | sem::IntrinsicCall::SetContains
                | sem::IntrinsicCall::SetRemove
                | sem::IntrinsicCall::SetClear
                | sem::IntrinsicCall::MapInsert
                | sem::IntrinsicCall::MapContainsKey
                | sem::IntrinsicCall::MapRemove
                | sem::IntrinsicCall::MapClear => {
                    panic!("backend call expr cannot lower set intrinsic without a receiver");
                }
            },
            sem::CallTarget::Runtime(runtime) => Callee::Runtime(self.runtime_for_call(runtime)?),
        };

        // Lower argument expressions.
        let Some(mut arg_values) = self.lower_call_arg_values(args)? else {
            return Ok(None);
        };

        if let Some(param_tys) = self.call_param_types(&call_plan, Some(callee_expr), None) {
            self.emit_call_conversion_checks(&call_plan, None, &arg_values, &param_tys);
        }

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
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        let call_plan = self.call_plan(expr.id);

        // Method calls must have a receiver.
        if !call_plan.has_receiver {
            panic!(
                "backend lower_method_call_expr missing receiver for {:?}",
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
                return self.lower_method_intrinsic(
                    expr,
                    receiver,
                    args,
                    &call_plan,
                    &mut receiver_value,
                    intrinsic,
                );
            }
            sem::CallTarget::Runtime(runtime) => Callee::Runtime(self.runtime_for_call(runtime)?),
        };

        // Lower argument expressions.
        let Some(mut arg_values) = self.lower_call_arg_values(args)? else {
            return Ok(None);
        };

        if let Some(param_tys) = self.call_param_types(&call_plan, None, Some(&receiver_value)) {
            self.emit_call_conversion_checks(
                &call_plan,
                Some(&receiver_value),
                &arg_values,
                &param_tys,
            );
        }

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

    fn lower_method_intrinsic(
        &mut self,
        expr: &sem::ValueExpr,
        receiver: &sem::MethodReceiver,
        args: &[sem::CallArg],
        call_plan: &sem::CallPlan,
        receiver_value: &mut CallInputValue,
        intrinsic: &sem::IntrinsicCall,
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        match intrinsic {
            sem::IntrinsicCall::StringLen => {
                // String length is a field load; avoid emitting a runtime call.
                self.lower_string_len_method(expr.span, receiver, receiver_value)
                    .map(Some)
            }
            sem::IntrinsicCall::DynArrayAppend => {
                let Some(mut arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if arg_values.len() != 1 {
                    panic!(
                        "backend dyn-array append expects exactly one arg, got {}",
                        arg_values.len()
                    );
                }

                let (dyn_addr, dyn_ty) = self.resolve_dyn_array_receiver(receiver_value);
                let Type::DynArray { elem_ty } = dyn_ty else {
                    panic!("backend dyn-array append expects dyn-array receiver");
                };

                let arg = &mut arg_values[0];
                let elem_addr = if arg.is_addr {
                    arg.value
                } else {
                    let addr = self.materialize_value_addr(arg.value, &arg.ty);
                    arg.value = addr;
                    arg.is_addr = true;
                    addr
                };

                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                let layout = self.type_lowerer.ir_type_cache.layout(elem_ir_ty);
                let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
                let size_val = self
                    .builder
                    .const_int(layout.size() as i128, false, 64, u64_ty);
                let align_val = self
                    .builder
                    .const_int(layout.align() as i128, false, 64, u64_ty);
                let ret_ty = self.type_lowerer.lower_type_id(expr.ty);
                let result = self.builder.call(
                    Callee::Runtime(RuntimeFn::DynArrayAppendElem),
                    vec![dyn_addr, elem_addr, size_val, align_val],
                    ret_ty,
                );

                self.emit_call_drops(call_plan, Some(receiver_value), &arg_values)?;
                self.clear_sink_drop_flags(call_plan, Some(receiver_value), &arg_values);
                self.mark_out_init_flags(args, &arg_values);
                Ok(Some(result))
            }
            sem::IntrinsicCall::SetInsert => {
                let Some(mut arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if arg_values.len() != 1 {
                    panic!(
                        "backend set insert expects exactly one arg, got {}",
                        arg_values.len()
                    );
                }
                let (set_addr, set_ty) = self.resolve_set_receiver(receiver_value);
                let Type::Set { elem_ty } = set_ty else {
                    panic!("backend set insert expects set receiver");
                };
                let arg = &mut arg_values[0];
                let elem_addr = if arg.is_addr {
                    arg.value
                } else {
                    let addr = self.materialize_value_addr(arg.value, &arg.ty);
                    arg.value = addr;
                    arg.is_addr = true;
                    addr
                };
                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                let layout = self.type_lowerer.ir_type_cache.layout(elem_ir_ty);
                let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
                let size_val = self
                    .builder
                    .const_int(layout.size() as i128, false, 64, u64_ty);
                let align_val = self
                    .builder
                    .const_int(layout.align() as i128, false, 64, u64_ty);
                let ret_ty = self.type_lowerer.lower_type_id(expr.ty);
                let result = self.builder.call(
                    Callee::Runtime(RuntimeFn::SetInsertElem),
                    vec![set_addr, elem_addr, size_val, align_val],
                    ret_ty,
                );
                self.emit_call_drops(call_plan, Some(receiver_value), &arg_values)?;
                self.clear_sink_drop_flags(call_plan, Some(receiver_value), &arg_values);
                self.mark_out_init_flags(args, &arg_values);
                Ok(Some(result))
            }
            sem::IntrinsicCall::SetContains | sem::IntrinsicCall::SetRemove => {
                let Some(mut arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if arg_values.len() != 1 {
                    panic!(
                        "backend set method expects exactly one arg, got {}",
                        arg_values.len()
                    );
                }
                let (set_addr, set_ty) = self.resolve_set_receiver(receiver_value);
                let Type::Set { elem_ty } = set_ty else {
                    panic!("backend set method expects set receiver");
                };
                let arg = &mut arg_values[0];
                let elem_addr = if arg.is_addr {
                    arg.value
                } else {
                    let addr = self.materialize_value_addr(arg.value, &arg.ty);
                    arg.value = addr;
                    arg.is_addr = true;
                    addr
                };
                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                let layout = self.type_lowerer.ir_type_cache.layout(elem_ir_ty);
                let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
                let size_val = self
                    .builder
                    .const_int(layout.size() as i128, false, 64, u64_ty);
                let ret_ty = self.type_lowerer.lower_type_id(expr.ty);
                let runtime = match intrinsic {
                    sem::IntrinsicCall::SetContains => RuntimeFn::SetContainsElem,
                    sem::IntrinsicCall::SetRemove => RuntimeFn::SetRemoveElem,
                    _ => unreachable!(),
                };
                let result = self.builder.call(
                    Callee::Runtime(runtime),
                    vec![set_addr, elem_addr, size_val],
                    ret_ty,
                );
                self.emit_call_drops(call_plan, Some(receiver_value), &arg_values)?;
                self.clear_sink_drop_flags(call_plan, Some(receiver_value), &arg_values);
                self.mark_out_init_flags(args, &arg_values);
                Ok(Some(result))
            }
            sem::IntrinsicCall::SetClear => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if !arg_values.is_empty() {
                    panic!(
                        "backend set clear expects zero args, got {}",
                        arg_values.len()
                    );
                }
                let (set_addr, _set_ty) = self.resolve_set_receiver(receiver_value);
                let ret_ty = self.type_lowerer.lower_type_id(expr.ty);
                let result =
                    self.builder
                        .call(Callee::Runtime(RuntimeFn::SetClear), vec![set_addr], ret_ty);
                self.emit_call_drops(call_plan, Some(receiver_value), &arg_values)?;
                self.clear_sink_drop_flags(call_plan, Some(receiver_value), &arg_values);
                self.mark_out_init_flags(args, &arg_values);
                Ok(Some(result))
            }
            sem::IntrinsicCall::MapInsert => {
                let Some(mut arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if arg_values.len() != 2 {
                    panic!(
                        "backend map insert expects exactly two args, got {}",
                        arg_values.len()
                    );
                }
                let (map_addr, map_ty) = self.resolve_map_receiver(receiver_value);
                let Type::Map { key_ty, value_ty } = map_ty else {
                    panic!("backend map insert expects map receiver");
                };

                let key_arg = &mut arg_values[0];
                let key_addr = if key_arg.is_addr {
                    key_arg.value
                } else {
                    let addr = self.materialize_value_addr(key_arg.value, &key_arg.ty);
                    key_arg.value = addr;
                    key_arg.is_addr = true;
                    addr
                };

                let value_arg = &mut arg_values[1];
                let value_addr = if value_arg.is_addr {
                    value_arg.value
                } else {
                    let addr = self.materialize_value_addr(value_arg.value, &value_arg.ty);
                    value_arg.value = addr;
                    value_arg.is_addr = true;
                    addr
                };

                let key_ir_ty = self.type_lowerer.lower_type(&key_ty);
                let key_layout = self.type_lowerer.ir_type_cache.layout(key_ir_ty);
                let value_ir_ty = self.type_lowerer.lower_type(&value_ty);
                let value_layout = self.type_lowerer.ir_type_cache.layout(value_ir_ty);
                let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
                let key_size = self
                    .builder
                    .const_int(key_layout.size() as i128, false, 64, u64_ty);
                let value_size =
                    self.builder
                        .const_int(value_layout.size() as i128, false, 64, u64_ty);
                let ret_ty = self.type_lowerer.lower_type_id(expr.ty);
                let result = self.builder.call(
                    Callee::Runtime(RuntimeFn::MapInsertOrAssign),
                    vec![map_addr, key_addr, value_addr, key_size, value_size],
                    ret_ty,
                );
                self.emit_call_drops(call_plan, Some(receiver_value), &arg_values)?;
                self.clear_sink_drop_flags(call_plan, Some(receiver_value), &arg_values);
                self.mark_out_init_flags(args, &arg_values);
                Ok(Some(result))
            }
            sem::IntrinsicCall::MapContainsKey | sem::IntrinsicCall::MapRemove => {
                let Some(mut arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if arg_values.len() != 1 {
                    panic!(
                        "backend map method expects exactly one arg, got {}",
                        arg_values.len()
                    );
                }
                let (map_addr, map_ty) = self.resolve_map_receiver(receiver_value);
                let Type::Map { key_ty, value_ty } = map_ty else {
                    panic!("backend map method expects map receiver");
                };
                let key_arg = &mut arg_values[0];
                let key_addr = if key_arg.is_addr {
                    key_arg.value
                } else {
                    let addr = self.materialize_value_addr(key_arg.value, &key_arg.ty);
                    key_arg.value = addr;
                    key_arg.is_addr = true;
                    addr
                };
                let key_ir_ty = self.type_lowerer.lower_type(&key_ty);
                let key_layout = self.type_lowerer.ir_type_cache.layout(key_ir_ty);
                let value_ir_ty = self.type_lowerer.lower_type(&value_ty);
                let value_layout = self.type_lowerer.ir_type_cache.layout(value_ir_ty);
                let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
                let key_size = self
                    .builder
                    .const_int(key_layout.size() as i128, false, 64, u64_ty);
                let value_size =
                    self.builder
                        .const_int(value_layout.size() as i128, false, 64, u64_ty);
                let runtime = match intrinsic {
                    sem::IntrinsicCall::MapContainsKey => RuntimeFn::MapContainsKey,
                    sem::IntrinsicCall::MapRemove => RuntimeFn::MapRemoveKey,
                    _ => unreachable!(),
                };
                let ret_ty = self.type_lowerer.lower_type_id(expr.ty);
                let result = self.builder.call(
                    Callee::Runtime(runtime),
                    vec![map_addr, key_addr, key_size, value_size],
                    ret_ty,
                );
                self.emit_call_drops(call_plan, Some(receiver_value), &arg_values)?;
                self.clear_sink_drop_flags(call_plan, Some(receiver_value), &arg_values);
                self.mark_out_init_flags(args, &arg_values);
                Ok(Some(result))
            }
            sem::IntrinsicCall::MapClear => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if !arg_values.is_empty() {
                    panic!(
                        "backend map clear expects zero args, got {}",
                        arg_values.len()
                    );
                }
                let (map_addr, _map_ty) = self.resolve_map_receiver(receiver_value);
                let ret_ty = self.type_lowerer.lower_type_id(expr.ty);
                let result =
                    self.builder
                        .call(Callee::Runtime(RuntimeFn::MapClear), vec![map_addr], ret_ty);
                self.emit_call_drops(call_plan, Some(receiver_value), &arg_values)?;
                self.clear_sink_drop_flags(call_plan, Some(receiver_value), &arg_values);
                self.mark_out_init_flags(args, &arg_values);
                Ok(Some(result))
            }
        }
    }

    fn resolve_dyn_array_receiver(&mut self, receiver_value: &CallInputValue) -> (ValueId, Type) {
        let (_base_ty, deref_count) = receiver_value.ty.peel_heap_with_count();
        let (mut addr, ty) = if receiver_value.is_addr {
            let mut addr = receiver_value.value;
            let mut curr_ty = receiver_value.ty.clone();
            for _ in 0..deref_count {
                let elem_ty = match curr_ty {
                    Type::Heap { elem_ty } | Type::Ref { elem_ty, .. } => elem_ty,
                    other => panic!(
                        "backend dyn-array receiver expects heap/ref, got {:?}",
                        other
                    ),
                };
                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                let ptr_ir_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                addr = self.builder.load(addr, ptr_ir_ty);
                curr_ty = (*elem_ty).clone();
            }
            (addr, curr_ty)
        } else {
            self.resolve_deref_base_value(
                receiver_value.value,
                receiver_value.ty.clone(),
                deref_count,
            )
        };

        let Type::DynArray { .. } = ty else {
            panic!(
                "backend dyn-array receiver resolved to non-dyn-array {:?}",
                receiver_value.ty
            );
        };

        // Value receivers of plain dyn-array type need a temporary slot so
        // field loads/indexing can treat the base as an address.
        if !receiver_value.is_addr && deref_count == 0 {
            addr = self.materialize_value_addr(addr, &ty);
        }

        (addr, ty)
    }

    fn resolve_set_receiver(&mut self, receiver_value: &CallInputValue) -> (ValueId, Type) {
        let (_base_ty, deref_count) = receiver_value.ty.peel_heap_with_count();
        let (mut addr, ty) = if receiver_value.is_addr {
            let mut addr = receiver_value.value;
            let mut curr_ty = receiver_value.ty.clone();
            for _ in 0..deref_count {
                let elem_ty = match curr_ty {
                    Type::Heap { elem_ty } | Type::Ref { elem_ty, .. } => elem_ty,
                    other => panic!("backend set receiver expects heap/ref, got {:?}", other),
                };
                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                let ptr_ir_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                addr = self.builder.load(addr, ptr_ir_ty);
                curr_ty = (*elem_ty).clone();
            }
            (addr, curr_ty)
        } else {
            self.resolve_deref_base_value(
                receiver_value.value,
                receiver_value.ty.clone(),
                deref_count,
            )
        };

        let Type::Set { .. } = ty else {
            panic!(
                "backend set receiver resolved to non-set {:?}",
                receiver_value.ty
            );
        };

        // Value receivers of plain set type need a temporary slot so field
        // loads/mutations can treat the base as an address.
        if !receiver_value.is_addr && deref_count == 0 {
            addr = self.materialize_value_addr(addr, &ty);
        }

        (addr, ty)
    }

    fn resolve_map_receiver(&mut self, receiver_value: &CallInputValue) -> (ValueId, Type) {
        let (_base_ty, deref_count) = receiver_value.ty.peel_heap_with_count();
        let (mut addr, ty) = if receiver_value.is_addr {
            let mut addr = receiver_value.value;
            let mut curr_ty = receiver_value.ty.clone();
            for _ in 0..deref_count {
                let elem_ty = match curr_ty {
                    Type::Heap { elem_ty } | Type::Ref { elem_ty, .. } => elem_ty,
                    other => panic!("backend map receiver expects heap/ref, got {:?}", other),
                };
                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                let ptr_ir_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                addr = self.builder.load(addr, ptr_ir_ty);
                curr_ty = (*elem_ty).clone();
            }
            (addr, curr_ty)
        } else {
            self.resolve_deref_base_value(
                receiver_value.value,
                receiver_value.ty.clone(),
                deref_count,
            )
        };

        let Type::Map { .. } = ty else {
            panic!(
                "backend map receiver resolved to non-map {:?}",
                receiver_value.ty
            );
        };

        // Value receivers of plain map type need a temporary slot so field
        // loads/mutations can treat the base as an address.
        if !receiver_value.is_addr && deref_count == 0 {
            addr = self.materialize_value_addr(addr, &ty);
        }

        (addr, ty)
    }

    fn emit_call_drops(
        &mut self,
        call_plan: &sem::CallPlan,
        receiver_value: Option<&CallInputValue>,
        arg_values: &[CallInputValue],
    ) -> Result<(), LowerToIrError> {
        let expected = (call_plan.has_receiver as usize) + arg_values.len();
        if call_plan.drop_mask.len() != expected {
            panic!(
                "backend call drop mask length mismatch: expected {}, got {}",
                expected,
                call_plan.drop_mask.len()
            );
        }

        let mut input_index = 0;
        if call_plan.has_receiver {
            let receiver = receiver_value.unwrap_or_else(|| {
                panic!("backend call drop mask missing receiver value for call plan");
            });
            if call_plan.drop_mask[input_index] && receiver.drop_def.is_none() {
                self.emit_drop_for_value(receiver.value, &receiver.ty, receiver.is_addr)?;
            }
            input_index += 1;
        }

        for arg in arg_values {
            if call_plan.drop_mask[input_index] && arg.drop_def.is_none() {
                self.emit_drop_for_value(arg.value, &arg.ty, arg.is_addr)?;
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
                "backend call input modes length mismatch: expected {}, got {}",
                expected,
                call_plan.input_modes.len()
            );
        }

        let mut input_index = 0;
        if call_plan.has_receiver {
            let receiver = receiver_value.unwrap_or_else(|| {
                panic!("backend call input modes missing receiver value for call plan");
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
    ) -> Result<Vec<ValueId>, LowerToIrError> {
        if call_plan.has_receiver != receiver_value.is_some() {
            panic!(
                "backend lower_call_args_from_plan receiver mismatch for {:?}",
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
                                panic!("backend call plan missing receiver value for {:?}", expr_id)
                            })
                        }
                        sem::CallInput::Arg(index) => {
                            arg_values.get_mut(*index).unwrap_or_else(|| {
                                panic!("backend call arg index out of range: {index}")
                            })
                        }
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
                                panic!("backend call plan missing receiver value for {:?}", expr_id)
                            })
                        }
                        sem::CallInput::Arg(index) => arg_values.get(*index).unwrap_or_else(|| {
                            panic!("backend call arg index out of range: {index}")
                        }),
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

    fn runtime_for_call(&self, runtime: &sem::RuntimeCall) -> Result<RuntimeFn, LowerToIrError> {
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
    ) -> Result<ValueId, LowerToIrError> {
        if !matches!(receiver_value.ty, Type::String) {
            panic!(
                "backend string len intrinsic expects string receiver, got {:?}",
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
