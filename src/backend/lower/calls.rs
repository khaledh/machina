//! Call lowering.

use crate::backend::lower::LowerToIrError;
use crate::backend::lower::lowerer::{CallInputValue, FuncLowerer, LinearValue};
use crate::core::ast::{CallArg, CallArgMode, Expr, ExprKind, NodeId, ParamMode};
use crate::core::diag::Span;
use crate::core::plans::{
    ArgLowering, CallInput, CallPlan, CallTarget, IntrinsicCall, RuntimeCall,
};
use crate::core::resolve::{DefId, DefKind, DefTable};
use crate::core::types::{Type, TypeId, TypeRenderConfig, render_type};
use crate::ir::{BinOp, Callee, CastKind, RuntimeFn, ValueId};
use std::collections::BTreeMap;

fn drop_def_for_value_expr(def_table: &DefTable, expr: &Expr) -> Option<DefId> {
    match &expr.kind {
        ExprKind::Move { expr: inner }
        | ExprKind::ImplicitMove { expr: inner }
        | ExprKind::Load { expr: inner } => drop_def_for_place_expr(def_table, inner),
        _ => None,
    }
}

fn drop_def_for_place_expr(def_table: &DefTable, place: &Expr) -> Option<DefId> {
    match &place.kind {
        ExprKind::Var { .. } => Some(def_table.def_id(place.id)),
        ExprKind::ArrayIndex { target, .. }
        | ExprKind::TupleField { target, .. }
        | ExprKind::StructField { target, .. } => drop_def_for_place_expr(def_table, target),
        ExprKind::Deref { expr } => drop_def_for_value_expr(def_table, expr),
        _ => None,
    }
}

fn borrowable_place_expr(expr: &Expr) -> Option<&Expr> {
    match &expr.kind {
        ExprKind::Load { expr: place } => Some(place),
        ExprKind::Var { .. }
        | ExprKind::ArrayIndex { .. }
        | ExprKind::TupleField { .. }
        | ExprKind::StructField { .. }
        | ExprKind::Deref { .. } => Some(expr),
        _ => None,
    }
}

#[derive(Clone, Copy)]
enum CollectionReceiverKind {
    DynArray,
    Set,
    Map,
}

impl CollectionReceiverKind {
    fn label(self) -> &'static str {
        match self {
            Self::DynArray => "dyn-array",
            Self::Set => "set",
            Self::Map => "map",
        }
    }

    fn matches(self, ty: &Type) -> bool {
        match self {
            Self::DynArray => matches!(ty, Type::DynArray { .. }),
            Self::Set => matches!(ty, Type::Set { .. }),
            Self::Map => matches!(ty, Type::Map { .. }),
        }
    }
}

impl<'a, 'g> FuncLowerer<'a, 'g> {
    fn render_type_for_type_of(ty: &Type, overrides: Option<&BTreeMap<u32, String>>) -> String {
        render_type(
            ty,
            &TypeRenderConfig {
                show_in_mode: false,
                type_var_names: overrides,
                nominal_name_map: None,
            },
        )
    }

    fn format_nominal_for_type_of(&self, type_id: TypeId) -> Option<String> {
        let key = self.type_map.lookup_nominal_key_for_type_id(type_id)?;
        let def = self.def_table.lookup_def(key.def_id)?;
        if key.type_args.is_empty() {
            return Some(def.name.clone());
        }
        let args = key
            .type_args
            .iter()
            .map(|ty| Self::render_type_for_type_of(ty, None))
            .collect::<Vec<_>>()
            .join(", ");
        Some(format!("{}<{}>", def.name, args))
    }

    fn callable_type_param_names_for_type_of(
        &self,
        def_id: DefId,
    ) -> Option<BTreeMap<u32, String>> {
        self.type_map.lookup_def_type_param_names(def_id).cloned()
    }

    fn type_of_function_symbol_name(&self, args: &[CallArg]) -> Option<String> {
        if args.len() != 1 {
            return None;
        }
        let arg = &args[0];
        match arg.mode {
            CallArgMode::Default | CallArgMode::Move => {}
            CallArgMode::InOut | CallArgMode::Out => return None,
        }
        let place = Self::type_of_symbol_place(&arg.expr)?;
        let ExprKind::Var { .. } = &place.kind else {
            return None;
        };
        let def_id = self.def_table.def_id(place.id);
        let def = self.def_table.lookup_def(def_id)?;
        if !matches!(def.kind, DefKind::FuncDef { .. } | DefKind::FuncDecl { .. }) {
            return None;
        }
        let fn_ty = self.type_map.lookup_def_type(def)?;
        let overrides = self.callable_type_param_names_for_type_of(def_id);
        Some(Self::render_type_for_type_of(&fn_ty, overrides.as_ref()))
    }

    fn type_of_symbol_place(expr: &Expr) -> Option<&Expr> {
        let mut current = expr;
        loop {
            match &current.kind {
                ExprKind::Load { expr: inner }
                | ExprKind::Move { expr: inner }
                | ExprKind::ImplicitMove { expr: inner } => return Some(inner),
                // Type-check inserts coercions around function symbols; peel them
                // to recover the underlying definition.
                ExprKind::Coerce { expr, .. } => {
                    current = expr;
                }
                _ => return None,
            }
        }
    }

    fn lower_type_of_intrinsic(
        &mut self,
        expr: &Expr,
        args: &[CallArg],
        call_plan: &CallPlan,
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
        // Function items are pure symbols; lowering them as runtime values is not
        // required for `type_of`, so read their polymorphic signature directly.
        if let Some(type_name) = self.type_of_function_symbol_name(args) {
            let value = self.lower_static_string_value(expr_ty, type_name.as_bytes());
            return Ok(Some(value));
        }

        let Some(arg_values) = self.lower_call_arg_values(args)? else {
            return Ok(None);
        };
        if arg_values.len() != 1 {
            panic!(
                "backend type_of intrinsic expects exactly one arg, got {}",
                arg_values.len()
            );
        }

        let arg_type_id = self.type_map.type_of(args[0].expr.id);
        let type_name = self
            .format_nominal_for_type_of(arg_type_id)
            .unwrap_or_else(|| Self::render_type_for_type_of(&arg_values[0].ty, None));
        let value = self.lower_static_string_value(expr_ty, type_name.as_bytes());
        self.apply_call_drop_effects(call_plan, args, None, &arg_values)?;
        Ok(Some(value))
    }

    fn lower_static_string_value(&mut self, string_ty_id: TypeId, bytes: &[u8]) -> LinearValue {
        let string_ty = self.type_lowerer.lower_type_id(string_ty_id);
        let slot = self.alloc_value_slot(string_ty);

        let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
        let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);
        let ptr_val = if bytes.is_empty() {
            let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
            let zero = self.builder.const_int(0, false, 64, u64_ty);
            self.builder.cast(CastKind::IntToPtr, zero, u8_ptr_ty)
        } else {
            let global_id = self.add_global_bytes(bytes.to_vec());
            self.builder.const_global_addr(global_id, u8_ptr_ty)
        };

        let len_ty = self.type_lowerer.lower_type(&Type::uint(32));
        let len_val = self
            .builder
            .const_int(bytes.len() as i128, false, 32, len_ty);
        let cap_val = self.builder.const_int(0, false, 32, len_ty);

        self.store_field(slot.addr, 0, u8_ptr_ty, ptr_val);
        self.store_field(slot.addr, 1, len_ty, len_val);
        self.store_field(slot.addr, 2, len_ty, cap_val);
        self.load_slot(&slot)
    }

    fn lower_machine_payload_pack_intrinsic(
        &mut self,
        expr: &Expr,
        args: &[CallArg],
        call_plan: &CallPlan,
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
        let Some(mut arg_values) = self.lower_call_arg_values(args)? else {
            return Ok(None);
        };
        if arg_values.len() != 1 {
            panic!(
                "backend payload-pack intrinsic expects exactly one arg, got {}",
                arg_values.len()
            );
        }

        let payload_arg = &mut arg_values[0];
        if payload_arg.is_addr {
            let payload_ir_ty = self.type_lowerer.lower_type(&payload_arg.ty);
            payload_arg.value = self.builder.load(payload_arg.value, payload_ir_ty);
            payload_arg.is_addr = false;
        }
        let payload_ty = payload_arg.ty.clone();
        let (payload_word, layout_word) =
            self.pack_machine_payload_words(payload_arg.value, &payload_ty);

        // Materialize `(payload0, payload1)` tuple return.
        let ret_ty = self.type_lowerer.lower_type_id(expr_ty);
        let ret_slot = self.alloc_value_slot(ret_ty);
        let field0_ty = self.lower_tuple_field_ty(expr_ty, 0);
        let field1_ty = self.lower_tuple_field_ty(expr_ty, 1);
        self.store_field(ret_slot.addr, 0, field0_ty, payload_word);
        self.store_field(ret_slot.addr, 1, field1_ty, layout_word);
        let packed = self.load_slot(&ret_slot);

        self.apply_call_drop_effects(call_plan, args, None, &arg_values)?;
        Ok(Some(packed))
    }

    fn lower_view_at_intrinsic(
        &mut self,
        expr: &Expr,
        args: &[CallArg],
        call_plan: &CallPlan,
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        let Some(arg_values) = self.lower_call_arg_values(args)? else {
            return Ok(None);
        };
        if arg_values.len() != 1 {
            panic!(
                "backend view_at intrinsic expects exactly one arg, got {}",
                arg_values.len()
            );
        }
        let raw_addr = self.load_call_input_scalar(&arg_values[0]);
        let expr_ty = self
            .type_map
            .type_table()
            .get(self.type_map.type_of(expr.id))
            .clone();
        let Type::View { elem_ty } = expr_ty else {
            panic!("backend view_at intrinsic expected view<T> result");
        };
        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let ptr_ir_ty = self.type_lowerer.ptr_to(elem_ir_ty);
        let view = self.builder.cast(CastKind::IntToPtr, raw_addr, ptr_ir_ty);
        self.apply_call_drop_effects(call_plan, args, None, &arg_values)?;
        Ok(Some(view))
    }

    fn lower_ptr_at_intrinsic(
        &mut self,
        expr: &Expr,
        args: &[CallArg],
        call_plan: &CallPlan,
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        let Some(arg_values) = self.lower_call_arg_values(args)? else {
            return Ok(None);
        };
        if arg_values.len() != 1 {
            panic!(
                "backend ptr_at intrinsic expects exactly one arg, got {}",
                arg_values.len()
            );
        }
        let raw_addr = self.load_call_input_scalar(&arg_values[0]);
        let expr_ty = self
            .type_map
            .type_table()
            .get(self.type_map.type_of(expr.id))
            .clone();
        let Type::RawPtr { elem_ty } = expr_ty else {
            panic!("backend ptr_at intrinsic expected *T result");
        };
        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let ptr_ir_ty = self.type_lowerer.ptr_to(elem_ir_ty);
        let ptr = self.builder.cast(CastKind::IntToPtr, raw_addr, ptr_ir_ty);
        self.apply_call_drop_effects(call_plan, args, None, &arg_values)?;
        Ok(Some(ptr))
    }

    fn lower_view_seq_intrinsic(
        &mut self,
        expr: &Expr,
        args: &[CallArg],
        call_plan: &CallPlan,
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        let Some(arg_values) = self.lower_call_arg_values(args)? else {
            return Ok(None);
        };
        if arg_values.len() != 2 {
            panic!(
                "backend foreign view slice/array intrinsic expects exactly two args, got {}",
                arg_values.len()
            );
        }
        let raw_addr = self.load_call_input_scalar(&arg_values[0]);
        let count = self.load_call_input_scalar(&arg_values[1]);
        let expr_ty_id = self.type_map.type_of(expr.id);
        let expr_ty = self.type_map.type_table().get(expr_ty_id).clone();
        let elem_ty = expr_ty.foreign_view_elem_type().unwrap_or_else(|| {
            panic!("backend foreign view sequence intrinsic expected view_slice/view_array result")
        });
        let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
        let ptr_ir_ty = match &expr_ty {
            Type::ViewSlice { .. } => {
                let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                self.type_lowerer.ptr_to(elem_ptr_ty)
            }
            Type::ViewArray { .. } => self.type_lowerer.ptr_to(elem_ir_ty),
            _ => panic!("backend foreign view sequence intrinsic expected view_slice/view_array"),
        };
        let ptr = self.builder.cast(CastKind::IntToPtr, raw_addr, ptr_ir_ty);

        let ret_ty = self.type_lowerer.lower_type_id(expr_ty_id);
        let ret_slot = self.alloc_value_slot(ret_ty);
        self.store_field(ret_slot.addr, 0, ptr_ir_ty, ptr);
        let len_ty = self.type_lowerer.lower_type(&Type::uint(64));
        self.store_field(ret_slot.addr, 1, len_ty, count);
        let view = self.load_slot(&ret_slot);
        self.apply_call_drop_effects(call_plan, args, None, &arg_values)?;
        Ok(Some(view))
    }

    fn call_input_from_value_expr(
        &mut self,
        expr: &Expr,
    ) -> Result<Option<CallInputValue>, LowerToIrError> {
        let drop_def = drop_def_for_value_expr(self.def_table, expr);
        let Some(value) = self.lower_value_expr_opt(expr)? else {
            return Ok(None);
        };
        let expr_ty = self.type_map.type_of(expr.id);
        let ty = self.type_map.type_table().get(expr_ty).clone();
        Ok(Some(CallInputValue {
            value,
            ty,
            is_addr: false,
            drop_def,
        }))
    }

    fn call_input_from_place_expr(
        &mut self,
        place: &Expr,
    ) -> Result<CallInputValue, LowerToIrError> {
        let drop_def = drop_def_for_place_expr(self.def_table, place);
        let addr = self.lower_place_addr(place)?;
        let place_ty = self.type_map.type_of(place.id);
        let ty = self.type_map.type_table().get(place_ty).clone();
        Ok(CallInputValue {
            value: addr.addr,
            ty,
            is_addr: true,
            drop_def,
        })
    }

    fn lower_call_arg_values(
        &mut self,
        args: &[CallArg],
    ) -> Result<Option<Vec<CallInputValue>>, LowerToIrError> {
        let mut arg_values = Vec::with_capacity(args.len());
        for arg in args {
            match arg.mode {
                CallArgMode::Default => {
                    let expr_ty = self.type_map.type_of(arg.expr.id);
                    let sem_ty = self.type_map.type_table().get(expr_ty).clone();
                    if !sem_ty.is_scalar()
                        && let Some(place) = borrowable_place_expr(&arg.expr)
                    {
                        arg_values.push(self.call_input_from_place_expr(place)?);
                        continue;
                    }
                    let Some(input) = self.call_input_from_value_expr(&arg.expr)? else {
                        return Ok(None);
                    };
                    arg_values.push(input);
                }
                CallArgMode::Move => {
                    let Some(input) = self.call_input_from_value_expr(&arg.expr)? else {
                        return Ok(None);
                    };
                    arg_values.push(input);
                }
                CallArgMode::InOut | CallArgMode::Out => {
                    arg_values.push(self.call_input_from_place_expr(&arg.expr)?);
                }
            }
        }
        Ok(Some(arg_values))
    }

    fn call_param_types(
        &self,
        call_plan: &CallPlan,
        callee_expr: Option<&Expr>,
        receiver_value: Option<&CallInputValue>,
    ) -> Option<Vec<Type>> {
        let fn_ty = match &call_plan.target {
            CallTarget::Direct(def_id) => {
                let def = self.def_table.lookup_def(*def_id)?;
                self.type_map.lookup_def_type(def)?
            }
            CallTarget::Indirect => {
                if let Some(callee_expr) = callee_expr {
                    let callee_ty = self.type_map.type_of(callee_expr.id);
                    self.type_map.type_table().get(callee_ty).clone()
                } else if let Some(receiver_value) = receiver_value {
                    receiver_value.ty.clone()
                } else {
                    return None;
                }
            }
            CallTarget::Intrinsic(IntrinsicCall::TypeOf) => {
                // `type_of` is compile-time metadata extraction; it does not need
                // argument conversion checks against callee parameter types.
                return None;
            }
            CallTarget::Runtime(_) => return None,
            CallTarget::Intrinsic(_) => {
                let callee_expr = callee_expr?;
                let callee_ty = self.type_map.type_of(callee_expr.id);
                self.type_map.type_table().get(callee_ty).clone()
            }
        };

        let Type::Fn { params, .. } = fn_ty else {
            return None;
        };
        Some(params.into_iter().map(|param| param.ty).collect())
    }

    /// Returns the callee's declared semantic return type before any expression-level
    /// widening (for example `T` flowing into `T | SessionError`).
    fn call_result_type(
        &self,
        call_plan: &CallPlan,
        callee_expr: Option<&Expr>,
        receiver_value: Option<&CallInputValue>,
    ) -> Option<Type> {
        let fn_ty = match &call_plan.target {
            CallTarget::Direct(def_id) => {
                let def = self.def(*def_id);
                self.type_map.lookup_def_type(def)?
            }
            CallTarget::Indirect => {
                if let Some(callee_expr) = callee_expr {
                    let callee_ty = self.type_map.type_of(callee_expr.id);
                    self.type_map.type_table().get(callee_ty).clone()
                } else if let Some(receiver_value) = receiver_value {
                    receiver_value.ty.clone()
                } else {
                    return None;
                }
            }
            CallTarget::Intrinsic(IntrinsicCall::TypeOf) => return Some(Type::String),
            CallTarget::Runtime(_) => return None,
            CallTarget::Intrinsic(_) => {
                let callee_expr = callee_expr?;
                let callee_ty = self.type_map.type_of(callee_expr.id);
                self.type_map.type_table().get(callee_ty).clone()
            }
        };

        let Type::Fn { ret_ty, .. } = fn_ty else {
            return None;
        };
        Some(*ret_ty)
    }

    fn emit_call_conversion_checks(
        &mut self,
        call_plan: &CallPlan,
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

    /// Lowers a call expression, returning `None` if a subexpression returns.
    pub(super) fn lower_call_expr(
        &mut self,
        expr: &Expr,
        callee_expr: &Expr,
        args: &[CallArg],
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
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
            CallTarget::Direct(def_id) => Callee::Direct(*def_id),
            CallTarget::Indirect => {
                let Some(callee_value) = self.lower_value_expr_opt(callee_expr)? else {
                    return Ok(None);
                };
                Callee::Value(callee_value)
            }
            CallTarget::Intrinsic(intrinsic) => match intrinsic {
                IntrinsicCall::StringLen => {
                    panic!("backend call expr cannot lower string len without a receiver");
                }
                IntrinsicCall::StringLines => {
                    panic!("backend call expr cannot lower string lines without a receiver");
                }
                IntrinsicCall::StringSplit => {
                    panic!("backend call expr cannot lower string split without a receiver");
                }
                IntrinsicCall::AddressOffset
                | IntrinsicCall::AddressAlignDown
                | IntrinsicCall::AddressAlignUp
                | IntrinsicCall::AddressIsAligned
                | IntrinsicCall::AddressIsSome
                | IntrinsicCall::AddressIsNone
                | IntrinsicCall::AddressUnwrap => {
                    panic!("backend call expr cannot lower address intrinsic without a receiver");
                }
                IntrinsicCall::TypeOf => {
                    return self.lower_type_of_intrinsic(expr, args, &call_plan);
                }
                IntrinsicCall::ViewAt => {
                    return self.lower_view_at_intrinsic(expr, args, &call_plan);
                }
                IntrinsicCall::PtrAt => {
                    return self.lower_ptr_at_intrinsic(expr, args, &call_plan);
                }
                IntrinsicCall::PtrRead | IntrinsicCall::PtrWrite => {
                    panic!(
                        "backend call expr cannot lower raw-pointer intrinsic without a receiver"
                    );
                }
                IntrinsicCall::ViewSliceAt | IntrinsicCall::ViewArrayAt => {
                    return self.lower_view_seq_intrinsic(expr, args, &call_plan);
                }
                IntrinsicCall::MachinePayloadPack => {
                    return self.lower_machine_payload_pack_intrinsic(expr, args, &call_plan);
                }
                IntrinsicCall::DynArrayAppend => {
                    panic!("backend call expr cannot lower dyn-array intrinsic without a receiver");
                }
                IntrinsicCall::SetInsert
                | IntrinsicCall::SetContains
                | IntrinsicCall::SetRemove
                | IntrinsicCall::SetClear
                | IntrinsicCall::MapInsert
                | IntrinsicCall::MapContainsKey
                | IntrinsicCall::MapGet
                | IntrinsicCall::MapRemove
                | IntrinsicCall::MapClear => {
                    panic!("backend call expr cannot lower set intrinsic without a receiver");
                }
            },
            CallTarget::Runtime(runtime) => Callee::Runtime(self.runtime_for_call(runtime)?),
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
        let call_result_ty = self
            .call_result_type(&call_plan, Some(callee_expr), None)
            .unwrap_or_else(|| self.type_map.type_table().get(expr_ty).clone());
        let ir_call_result_ty = self.type_lowerer.lower_type(&call_result_ty);
        let result = self.builder.call(callee, call_args, ir_call_result_ty);
        self.apply_call_drop_effects(&call_plan, args, None, &arg_values)?;
        let expr_sem_ty = self.type_map.type_table().get(expr_ty).clone();
        Ok(Some(self.coerce_value(
            result,
            &call_result_ty,
            &expr_sem_ty,
        )))
    }

    /// Lowers a method call expression, returning `None` if a subexpression returns.
    pub(super) fn lower_method_call_expr(
        &mut self,
        expr: &Expr,
        callee_expr: &Expr,
        args: &[CallArg],
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
        let call_plan = self.call_plan(expr.id);

        // Method calls must have a receiver.
        if !call_plan.has_receiver {
            panic!(
                "backend lower_method_call_expr missing receiver for {:?}",
                expr.id
            );
        }

        // Lower the receiver. Determine if it is a place (InOut/Out self) or value
        // from the first input mode in the call plan.
        let receiver_is_place = !call_plan.input_modes.is_empty()
            && matches!(call_plan.input_modes[0], ParamMode::InOut | ParamMode::Out);
        let mut receiver_value = if receiver_is_place {
            self.call_input_from_place_expr(callee_expr)?
        } else {
            let Some(input) = self.call_input_from_value_expr(callee_expr)? else {
                return Ok(None);
            };
            input
        };

        // Resolve the callee.
        let callee = match &call_plan.target {
            CallTarget::Direct(def_id) => Callee::Direct(*def_id),
            CallTarget::Indirect => {
                // For indirect method calls, treat the receiver as the callee value.
                // The call plan still governs how the receiver/args are passed.
                let callee_value = if receiver_is_place {
                    let ty = self.type_lowerer.lower_type(&receiver_value.ty);
                    self.builder.load(receiver_value.value, ty)
                } else {
                    receiver_value.value
                };
                Callee::Value(callee_value)
            }
            CallTarget::Intrinsic(intrinsic) => {
                return self.lower_method_intrinsic(
                    expr,
                    receiver_is_place,
                    args,
                    &call_plan,
                    &mut receiver_value,
                    intrinsic,
                );
            }
            CallTarget::Runtime(runtime) => Callee::Runtime(self.runtime_for_call(runtime)?),
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
        let call_result_ty = self
            .call_result_type(&call_plan, None, Some(&receiver_value))
            .unwrap_or_else(|| self.type_map.type_table().get(expr_ty).clone());
        let ir_call_result_ty = self.type_lowerer.lower_type(&call_result_ty);
        let result = self.builder.call(callee, call_args, ir_call_result_ty);
        self.apply_call_drop_effects(&call_plan, args, Some(&receiver_value), &arg_values)?;
        let expr_sem_ty = self.type_map.type_table().get(expr_ty).clone();
        Ok(Some(self.coerce_value(
            result,
            &call_result_ty,
            &expr_sem_ty,
        )))
    }

    fn lower_method_intrinsic(
        &mut self,
        expr: &Expr,
        receiver_is_place: bool,
        args: &[CallArg],
        call_plan: &CallPlan,
        receiver_value: &mut CallInputValue,
        intrinsic: &IntrinsicCall,
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        let expr_ty = self.type_map.type_of(expr.id);
        match intrinsic {
            IntrinsicCall::MachinePayloadPack => {
                panic!("backend method intrinsic cannot lower payload-pack with receiver")
            }
            IntrinsicCall::StringLen => {
                // String length is a field load; avoid emitting a runtime call.
                self.lower_string_len_method(expr.span, receiver_is_place, receiver_value)
                    .map(Some)
            }
            IntrinsicCall::StringLines => self.lower_string_lines_method(
                expr,
                receiver_is_place,
                args,
                call_plan,
                receiver_value,
            ),
            IntrinsicCall::StringSplit => self.lower_string_split_method(
                expr,
                receiver_is_place,
                args,
                call_plan,
                receiver_value,
            ),
            IntrinsicCall::ViewAt
            | IntrinsicCall::ViewSliceAt
            | IntrinsicCall::ViewArrayAt
            | IntrinsicCall::PtrAt => {
                panic!(
                    "backend foreign view constructor intrinsic cannot lower with a method receiver"
                );
            }
            IntrinsicCall::PtrRead => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if !arg_values.is_empty() {
                    panic!(
                        "backend raw pointer read expects 0 args, got {}",
                        arg_values.len()
                    );
                }
                let ptr = self.load_receiver_scalar(receiver_is_place, receiver_value);
                let Type::RawPtr { elem_ty } = receiver_value.ty.peel_heap() else {
                    panic!("backend raw pointer read expects *T receiver");
                };
                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                Ok(Some(self.builder.load(ptr, elem_ir_ty)))
            }
            IntrinsicCall::PtrWrite => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if arg_values.len() != 1 {
                    panic!(
                        "backend raw pointer write expects 1 arg, got {}",
                        arg_values.len()
                    );
                }
                let ptr = self.load_receiver_scalar(receiver_is_place, receiver_value);
                let Type::RawPtr { elem_ty } = receiver_value.ty.peel_heap() else {
                    panic!("backend raw pointer write expects *T receiver");
                };
                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                if arg_values[0].is_addr {
                    let src = arg_values[0].value;
                    let layout = self.type_lowerer.ir_type_cache.layout(elem_ir_ty);
                    let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
                    let len = self
                        .builder
                        .const_int(layout.size() as i128, false, 64, u64_ty);
                    self.builder.memcopy(ptr, src, len);
                } else {
                    self.store_value_into_addr(ptr, arg_values[0].value, &elem_ty, elem_ir_ty);
                }
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
                let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
                Ok(Some(self.builder.const_unit(unit_ty)))
            }
            IntrinsicCall::AddressOffset => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if !arg_values.is_empty() {
                    panic!(
                        "backend address offset expects zero args, got {}",
                        arg_values.len()
                    );
                }
                Ok(Some(
                    self.load_receiver_scalar(receiver_is_place, receiver_value),
                ))
            }
            IntrinsicCall::AddressAlignDown => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if arg_values.len() != 1 {
                    panic!(
                        "backend address align_down expects 1 arg, got {}",
                        arg_values.len()
                    );
                }
                let receiver = self.load_receiver_scalar(receiver_is_place, receiver_value);
                let alignment = self.load_call_input_scalar(&arg_values[0]);
                Ok(Some(self.lower_align_down(
                    receiver,
                    alignment,
                    &receiver_value.ty,
                )))
            }
            IntrinsicCall::AddressAlignUp => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if arg_values.len() != 1 {
                    panic!(
                        "backend address align_up expects 1 arg, got {}",
                        arg_values.len()
                    );
                }
                let receiver = self.load_receiver_scalar(receiver_is_place, receiver_value);
                let alignment = self.load_call_input_scalar(&arg_values[0]);
                Ok(Some(self.lower_align_up(
                    receiver,
                    alignment,
                    &receiver_value.ty,
                )))
            }
            IntrinsicCall::AddressIsAligned => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if arg_values.len() != 1 {
                    panic!(
                        "backend address is_aligned expects 1 arg, got {}",
                        arg_values.len()
                    );
                }
                let receiver = self.load_receiver_scalar(receiver_is_place, receiver_value);
                let alignment = self.load_call_input_scalar(&arg_values[0]);
                Ok(Some(self.lower_is_aligned(
                    receiver,
                    alignment,
                    &receiver_value.ty,
                )))
            }
            IntrinsicCall::AddressIsSome => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if !arg_values.is_empty() {
                    panic!(
                        "backend nullable-address is_some expects 0 args, got {}",
                        arg_values.len()
                    );
                }
                let receiver = self.load_receiver_scalar(receiver_is_place, receiver_value);
                Ok(Some(self.lower_nullable_addr_is_some(receiver)))
            }
            IntrinsicCall::AddressIsNone => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if !arg_values.is_empty() {
                    panic!(
                        "backend nullable-address is_none expects 0 args, got {}",
                        arg_values.len()
                    );
                }
                let receiver = self.load_receiver_scalar(receiver_is_place, receiver_value);
                Ok(Some(self.lower_nullable_addr_is_none(receiver)))
            }
            IntrinsicCall::AddressUnwrap => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if !arg_values.is_empty() {
                    panic!(
                        "backend nullable-address unwrap expects 0 args, got {}",
                        arg_values.len()
                    );
                }
                let receiver = self.load_receiver_scalar(receiver_is_place, receiver_value);
                Ok(Some(self.lower_nullable_addr_unwrap(receiver)))
            }
            IntrinsicCall::TypeOf => {
                panic!("backend type_of intrinsic cannot lower with a method receiver");
            }
            IntrinsicCall::DynArrayAppend => {
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

                let elem_addr = self.ensure_call_input_addr(&mut arg_values[0]);
                let size_val = self.runtime_size_const(&elem_ty);
                let align_val = self.runtime_align_const(&elem_ty);
                let ret_ty = self.type_lowerer.lower_type_id(expr_ty);
                let result = self.builder.call(
                    Callee::Runtime(RuntimeFn::DynArrayAppendElem),
                    vec![dyn_addr, elem_addr, size_val, align_val],
                    ret_ty,
                );

                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
                Ok(Some(result))
            }
            IntrinsicCall::SetInsert => {
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
                let elem_addr = self.ensure_call_input_addr(&mut arg_values[0]);
                let ret_ty = self.type_lowerer.lower_type_id(expr_ty);
                let result = if matches!(elem_ty.as_ref(), Type::String) {
                    self.builder.call(
                        Callee::Runtime(RuntimeFn::SetInsertString),
                        vec![set_addr, elem_addr],
                        ret_ty,
                    )
                } else {
                    let size_val = self.runtime_size_const(&elem_ty);
                    let align_val = self.runtime_align_const(&elem_ty);
                    self.builder.call(
                        Callee::Runtime(RuntimeFn::SetInsertElem),
                        vec![set_addr, elem_addr, size_val, align_val],
                        ret_ty,
                    )
                };
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
                Ok(Some(result))
            }
            IntrinsicCall::SetContains | IntrinsicCall::SetRemove => {
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
                let elem_addr = self.ensure_call_input_addr(&mut arg_values[0]);
                let ret_ty = self.type_lowerer.lower_type_id(expr_ty);
                let result = if matches!(elem_ty.as_ref(), Type::String) {
                    let runtime = match intrinsic {
                        IntrinsicCall::SetContains => RuntimeFn::SetContainsString,
                        IntrinsicCall::SetRemove => RuntimeFn::SetRemoveString,
                        _ => unreachable!(),
                    };
                    self.builder
                        .call(Callee::Runtime(runtime), vec![set_addr, elem_addr], ret_ty)
                } else {
                    let size_val = self.runtime_size_const(&elem_ty);
                    let runtime = match intrinsic {
                        IntrinsicCall::SetContains => RuntimeFn::SetContainsElem,
                        IntrinsicCall::SetRemove => RuntimeFn::SetRemoveElem,
                        _ => unreachable!(),
                    };
                    self.builder.call(
                        Callee::Runtime(runtime),
                        vec![set_addr, elem_addr, size_val],
                        ret_ty,
                    )
                };
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
                Ok(Some(result))
            }
            IntrinsicCall::SetClear => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if !arg_values.is_empty() {
                    panic!(
                        "backend set clear expects zero args, got {}",
                        arg_values.len()
                    );
                }
                let (set_addr, set_ty) = self.resolve_set_receiver(receiver_value);
                let Type::Set { elem_ty } = set_ty else {
                    panic!("backend set clear expects set receiver");
                };
                let ret_ty = self.type_lowerer.lower_type_id(expr_ty);
                let runtime = if matches!(elem_ty.as_ref(), Type::String) {
                    RuntimeFn::SetClearString
                } else {
                    RuntimeFn::SetClear
                };
                let result = self
                    .builder
                    .call(Callee::Runtime(runtime), vec![set_addr], ret_ty);
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
                Ok(Some(result))
            }
            IntrinsicCall::MapInsert => {
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

                let key_addr = self.ensure_call_input_addr(&mut arg_values[0]);
                let value_addr = self.ensure_call_input_addr(&mut arg_values[1]);
                let ret_ty = self.type_lowerer.lower_type_id(expr_ty);
                let result = if matches!(key_ty.as_ref(), Type::String) {
                    let value_size = self.runtime_size_const(&value_ty);
                    self.builder.call(
                        Callee::Runtime(RuntimeFn::MapInsertOrAssignStringKey),
                        vec![map_addr, key_addr, value_addr, value_size],
                        ret_ty,
                    )
                } else {
                    let key_size = self.runtime_size_const(&key_ty);
                    let value_size = self.runtime_size_const(&value_ty);
                    self.builder.call(
                        Callee::Runtime(RuntimeFn::MapInsertOrAssign),
                        vec![map_addr, key_addr, value_addr, key_size, value_size],
                        ret_ty,
                    )
                };
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
                Ok(Some(result))
            }
            IntrinsicCall::MapContainsKey | IntrinsicCall::MapRemove => {
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
                let key_addr = self.ensure_call_input_addr(&mut arg_values[0]);
                let ret_ty = self.type_lowerer.lower_type_id(expr_ty);
                let result = if matches!(key_ty.as_ref(), Type::String) {
                    let value_size = self.runtime_size_const(&value_ty);
                    let runtime = match intrinsic {
                        IntrinsicCall::MapContainsKey => RuntimeFn::MapContainsStringKey,
                        IntrinsicCall::MapRemove => RuntimeFn::MapRemoveStringKey,
                        _ => unreachable!(),
                    };
                    self.builder.call(
                        Callee::Runtime(runtime),
                        vec![map_addr, key_addr, value_size],
                        ret_ty,
                    )
                } else {
                    let key_size = self.runtime_size_const(&key_ty);
                    let value_size = self.runtime_size_const(&value_ty);
                    let runtime = match intrinsic {
                        IntrinsicCall::MapContainsKey => RuntimeFn::MapContainsKey,
                        IntrinsicCall::MapRemove => RuntimeFn::MapRemoveKey,
                        _ => unreachable!(),
                    };
                    self.builder.call(
                        Callee::Runtime(runtime),
                        vec![map_addr, key_addr, key_size, value_size],
                        ret_ty,
                    )
                };
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
                Ok(Some(result))
            }
            IntrinsicCall::MapGet => {
                let Some(mut arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if arg_values.len() != 1 {
                    panic!(
                        "backend map get expects exactly one arg, got {}",
                        arg_values.len()
                    );
                }
                let (map_addr, map_ty) = self.resolve_map_receiver(receiver_value);
                let Type::Map { key_ty, value_ty } = map_ty else {
                    panic!("backend map get expects map receiver");
                };

                let key_addr = self.ensure_call_input_addr(&mut arg_values[0]);
                let value_ir_ty = self.type_lowerer.lower_type(&value_ty);
                let value_slot = self.alloc_value_slot(value_ir_ty);

                let value_size = self.runtime_size_const(&value_ty);
                let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
                let hit = if matches!(key_ty.as_ref(), Type::String) {
                    self.builder.call(
                        Callee::Runtime(RuntimeFn::MapGetValueStringKey),
                        vec![map_addr, key_addr, value_size, value_slot.addr],
                        bool_ty,
                    )
                } else {
                    let key_size = self.runtime_size_const(&key_ty);
                    self.builder.call(
                        Callee::Runtime(RuntimeFn::MapGetValue),
                        vec![map_addr, key_addr, key_size, value_size, value_slot.addr],
                        bool_ty,
                    )
                };

                let union_ir_ty = self.type_lowerer.lower_type_id(expr_ty);
                let union_slot = self.alloc_value_slot(union_ir_ty);
                let (tag_ty, blob_ty, payload_offset, payload_ty) = {
                    let layout = self.type_lowerer.enum_layout(expr_ty);
                    let ok_variant = layout
                        .variants
                        .first()
                        .unwrap_or_else(|| panic!("backend map get missing ok union variant"));
                    if ok_variant.field_offsets.len() != 1 || ok_variant.field_tys.len() != 1 {
                        panic!("backend map get expects single-payload ok union variant");
                    }
                    (
                        layout.tag_ty,
                        layout.blob_ty,
                        ok_variant.field_offsets[0],
                        ok_variant.field_tys[0],
                    )
                };

                let hit_u32 = self.builder.int_extend(hit, tag_ty, false);
                let one_u32 = self.builder.const_int(1, false, 32, tag_ty);
                let tag = self.builder.binop(BinOp::Xor, one_u32, hit_u32, tag_ty);
                self.store_field(union_slot.addr, 0, tag_ty, tag);

                let payload = self.builder.load(value_slot.addr, value_ir_ty);
                let blob_ptr = self.field_addr_typed(union_slot.addr, 1, blob_ty);
                self.store_into_blob(blob_ptr, payload_offset, payload, payload_ty);

                let result = self.load_slot(&union_slot);
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
                Ok(Some(result))
            }
            IntrinsicCall::MapClear => {
                let Some(arg_values) = self.lower_call_arg_values(args)? else {
                    return Ok(None);
                };
                if !arg_values.is_empty() {
                    panic!(
                        "backend map clear expects zero args, got {}",
                        arg_values.len()
                    );
                }
                let (map_addr, map_ty) = self.resolve_map_receiver(receiver_value);
                let Type::Map { key_ty, value_ty } = map_ty else {
                    panic!("backend map clear expects map receiver");
                };
                let ret_ty = self.type_lowerer.lower_type_id(expr_ty);
                let result = if matches!(key_ty.as_ref(), Type::String) {
                    let value_size = self.runtime_size_const(&value_ty);
                    self.builder.call(
                        Callee::Runtime(RuntimeFn::MapClearStringKeys),
                        vec![map_addr, value_size],
                        ret_ty,
                    )
                } else {
                    self.builder
                        .call(Callee::Runtime(RuntimeFn::MapClear), vec![map_addr], ret_ty)
                };
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
                Ok(Some(result))
            }
        }
    }

    fn resolve_collection_receiver(
        &mut self,
        receiver_value: &CallInputValue,
        kind: CollectionReceiverKind,
    ) -> (ValueId, Type) {
        let (_base_ty, deref_count) = receiver_value.ty.peel_heap_with_count();
        let (mut addr, ty) = if receiver_value.is_addr {
            let mut addr = receiver_value.value;
            let mut curr_ty = receiver_value.ty.clone();
            for _ in 0..deref_count {
                let elem_ty = match curr_ty {
                    Type::Heap { elem_ty } | Type::Ref { elem_ty, .. } => elem_ty,
                    other => panic!(
                        "backend {} receiver expects heap/ref, got {:?}",
                        kind.label(),
                        other,
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

        if !kind.matches(&ty) {
            panic!(
                "backend {} receiver resolved to mismatched type {:?}",
                kind.label(),
                receiver_value.ty,
            );
        }

        // Plain value receivers need a temporary slot so collection runtime
        // helpers can uniformly treat the base as an address.
        if !receiver_value.is_addr && deref_count == 0 {
            addr = self.materialize_value_addr(addr, &ty);
        }

        (addr, ty)
    }

    fn resolve_dyn_array_receiver(&mut self, receiver_value: &CallInputValue) -> (ValueId, Type) {
        self.resolve_collection_receiver(receiver_value, CollectionReceiverKind::DynArray)
    }

    fn resolve_set_receiver(&mut self, receiver_value: &CallInputValue) -> (ValueId, Type) {
        self.resolve_collection_receiver(receiver_value, CollectionReceiverKind::Set)
    }

    fn resolve_map_receiver(&mut self, receiver_value: &CallInputValue) -> (ValueId, Type) {
        self.resolve_collection_receiver(receiver_value, CollectionReceiverKind::Map)
    }

    fn load_receiver_scalar(
        &mut self,
        receiver_is_place: bool,
        receiver_value: &CallInputValue,
    ) -> LinearValue {
        if receiver_is_place {
            let ir_ty = self.type_lowerer.lower_type(&receiver_value.ty);
            self.builder.load(receiver_value.value, ir_ty)
        } else {
            receiver_value.value
        }
    }

    fn load_call_input_scalar(&mut self, arg_value: &CallInputValue) -> LinearValue {
        if arg_value.is_addr {
            let ir_ty = self.type_lowerer.lower_type(&arg_value.ty);
            self.builder.load(arg_value.value, ir_ty)
        } else {
            arg_value.value
        }
    }

    fn lower_align_mask(
        &mut self,
        alignment: ValueId,
        addr_ty: &Type,
    ) -> (ValueId, crate::ir::IrTypeId, ValueId) {
        let ir_ty = self.type_lowerer.lower_type(addr_ty);
        let one = self.builder.const_int(1, false, 64, ir_ty);
        let alignment_minus_one = self
            .builder
            .binop(crate::ir::BinOp::Sub, alignment, one, ir_ty);
        let mask = self
            .builder
            .unop(crate::ir::UnOp::BitNot, alignment_minus_one, ir_ty);
        (mask, ir_ty, alignment_minus_one)
    }

    fn lower_align_down(
        &mut self,
        addr: ValueId,
        alignment: ValueId,
        addr_ty: &Type,
    ) -> LinearValue {
        let (mask, ir_ty, _) = self.lower_align_mask(alignment, addr_ty);
        self.builder.binop(crate::ir::BinOp::And, addr, mask, ir_ty)
    }

    fn lower_align_up(&mut self, addr: ValueId, alignment: ValueId, addr_ty: &Type) -> LinearValue {
        let (mask, ir_ty, alignment_minus_one) = self.lower_align_mask(alignment, addr_ty);
        let rounded = self
            .builder
            .binop(crate::ir::BinOp::Add, addr, alignment_minus_one, ir_ty);
        self.builder
            .binop(crate::ir::BinOp::And, rounded, mask, ir_ty)
    }

    fn lower_is_aligned(
        &mut self,
        addr: ValueId,
        alignment: ValueId,
        addr_ty: &Type,
    ) -> LinearValue {
        let ir_ty = self.type_lowerer.lower_type(addr_ty);
        let one = self.builder.const_int(1, false, 64, ir_ty);
        let zero = self.builder.const_int(0, false, 64, ir_ty);
        let alignment_minus_one = self
            .builder
            .binop(crate::ir::BinOp::Sub, alignment, one, ir_ty);
        let remainder = self
            .builder
            .binop(crate::ir::BinOp::And, addr, alignment_minus_one, ir_ty);
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        self.builder
            .cmp(crate::ir::CmpOp::Eq, remainder, zero, bool_ty)
    }

    fn lower_nullable_addr_is_some(&mut self, addr: ValueId) -> LinearValue {
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let zero = self.builder.const_int(0, false, 64, u64_ty);
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        self.builder.cmp(crate::ir::CmpOp::Ne, addr, zero, bool_ty)
    }

    fn lower_nullable_addr_is_none(&mut self, addr: ValueId) -> LinearValue {
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let zero = self.builder.const_int(0, false, 64, u64_ty);
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        self.builder.cmp(crate::ir::CmpOp::Eq, addr, zero, bool_ty)
    }

    fn lower_nullable_addr_unwrap(&mut self, addr: ValueId) -> LinearValue {
        let cond = self.lower_nullable_addr_is_some(addr);
        let ok_bb = self.builder.add_block();
        let trap_bb = self.builder.add_block();
        self.builder.terminate(crate::ir::Terminator::CondBr {
            cond,
            then_bb: ok_bb,
            then_args: Vec::new(),
            else_bb: trap_bb,
            else_args: Vec::new(),
        });

        self.builder.select_block(trap_bb);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let msg = "called unwrap() on None address".as_bytes().to_vec();
        let msg_len = self.builder.const_int(msg.len() as i128, false, 64, u64_ty);
        let msg_global = self.globals.add_bytes(msg);
        let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
        let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);
        let msg_ptr = self.builder.const_global_addr(msg_global, u8_ptr_ty);
        let msg_ptr_u64 = self.builder.cast(CastKind::PtrToInt, msg_ptr, u64_ty);
        let kind = self.builder.const_int(6, false, 64, u64_ty);
        let zero = self.builder.const_int(0, false, 64, u64_ty);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::Trap),
            vec![kind, msg_ptr_u64, msg_len, zero],
            unit_ty,
        );
        self.builder.terminate(crate::ir::Terminator::Unreachable);

        self.builder.select_block(ok_bb);
        addr
    }

    pub(super) fn lower_call_args_from_plan(
        &mut self,
        expr_id: NodeId,
        span: Span,
        call_plan: &CallPlan,
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
                ArgLowering::Direct(input) => {
                    let input_value = match input {
                        CallInput::Receiver => receiver_value.as_deref_mut().unwrap_or_else(|| {
                            panic!("backend call plan missing receiver value for {:?}", expr_id)
                        }),
                        CallInput::Arg(index) => arg_values.get_mut(*index).unwrap_or_else(|| {
                            panic!("backend call arg index out of range: {index}")
                        }),
                    };

                    if input_value.is_addr || input_value.ty.is_scalar() {
                        call_args.push(input_value.value);
                    } else {
                        let addr = if input_value.drop_def.is_none()
                            && self.type_needs_owned_copy(&input_value.ty)
                        {
                            self.materialize_value_addr_by_move(input_value.value, &input_value.ty)
                        } else {
                            self.materialize_value_addr(input_value.value, &input_value.ty)
                        };
                        input_value.value = addr;
                        input_value.is_addr = true;
                        call_args.push(addr);
                    }
                }
                ArgLowering::PtrLen { input, len_bits } => {
                    let input_value = match input {
                        CallInput::Receiver => receiver_value.as_deref().unwrap_or_else(|| {
                            panic!("backend call plan missing receiver value for {:?}", expr_id)
                        }),
                        CallInput::Arg(index) => arg_values.get(*index).unwrap_or_else(|| {
                            panic!("backend call arg index out of range: {index}")
                        }),
                    };
                    let (ptr, len) = if input_value.is_addr {
                        self.lower_ptr_len_from_addr(input_value.value, &input_value.ty, *len_bits)?
                    } else {
                        self.lower_ptr_len_from_value(
                            span,
                            input_value.value,
                            &input_value.ty,
                            *len_bits,
                        )?
                    };
                    call_args.push(ptr);
                    call_args.push(len);
                }
            }
        }
        Ok(call_args)
    }

    fn runtime_for_call(&self, runtime: &RuntimeCall) -> Result<RuntimeFn, LowerToIrError> {
        match runtime {
            RuntimeCall::Print => Ok(RuntimeFn::Print),
            RuntimeCall::U64ToDec => Ok(RuntimeFn::U64ToDec),
            RuntimeCall::MemSet => Ok(RuntimeFn::MemSet),
            RuntimeCall::StringFromBytes => Ok(RuntimeFn::StringFromBytes),
            RuntimeCall::StringAppendBytes => Ok(RuntimeFn::StringAppendBytes),
            RuntimeCall::MapIterInit => Ok(RuntimeFn::MapIterInit),
            RuntimeCall::MapIterIsDone => Ok(RuntimeFn::MapIterIsDone),
            RuntimeCall::MapIterLoadBytes => Ok(RuntimeFn::MapIterLoadBytes),
            RuntimeCall::MapIterLoadStringKey => Ok(RuntimeFn::MapIterLoadStringKey),
            RuntimeCall::MapIterAdvance => Ok(RuntimeFn::MapIterAdvance),
        }
    }

    fn lower_string_len_method(
        &mut self,
        span: Span,
        receiver_is_place: bool,
        receiver_value: &CallInputValue,
    ) -> Result<ValueId, LowerToIrError> {
        if !matches!(receiver_value.ty, Type::String) {
            panic!(
                "backend string len intrinsic expects string receiver, got {:?}",
                receiver_value.ty
            );
        }

        let len = if receiver_is_place {
            let view = self.load_string_view(receiver_value.value);
            view.len
        } else {
            let (_ptr, len) =
                self.lower_ptr_len_from_value(span, receiver_value.value, &receiver_value.ty, 32)?;
            len
        };

        Ok(len)
    }

    fn lower_string_lines_method(
        &mut self,
        expr: &Expr,
        receiver_is_place: bool,
        args: &[CallArg],
        call_plan: &CallPlan,
        receiver_value: &mut CallInputValue,
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        if !matches!(receiver_value.ty, Type::String) {
            panic!(
                "backend string lines intrinsic expects string receiver, got {:?}",
                receiver_value.ty
            );
        }
        if !args.is_empty() {
            panic!(
                "backend string lines intrinsic expects 0 args, got {}",
                args.len()
            );
        }

        let (ptr, len) = if receiver_is_place {
            let view = self.load_string_view(receiver_value.value);
            (view.ptr, view.len)
        } else {
            self.lower_ptr_len_from_value(expr.span, receiver_value.value, &receiver_value.ty, 32)?
        };

        let result_ir_ty = self
            .type_lowerer
            .lower_type_id(self.type_map.type_of(expr.id));
        let result_slot = self.alloc_value_slot(result_ir_ty);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        self.builder.call(
            Callee::Runtime(RuntimeFn::StringLines),
            vec![result_slot.addr, ptr, len],
            unit_ty,
        );
        self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &[])?;
        Ok(Some(self.load_slot(&result_slot)))
    }

    fn lower_string_split_method(
        &mut self,
        expr: &Expr,
        receiver_is_place: bool,
        args: &[CallArg],
        call_plan: &CallPlan,
        receiver_value: &mut CallInputValue,
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        if !matches!(receiver_value.ty, Type::String) {
            panic!(
                "backend string split intrinsic expects string receiver, got {:?}",
                receiver_value.ty
            );
        }
        if args.len() != 1 {
            panic!(
                "backend string split intrinsic expects 1 arg, got {}",
                args.len()
            );
        }

        let Some(arg_values) = self.lower_call_arg_values(args)? else {
            return Ok(None);
        };
        if arg_values.len() != 1 {
            panic!(
                "backend string split intrinsic expects exactly one lowered arg, got {}",
                arg_values.len()
            );
        }
        if !matches!(arg_values[0].ty, Type::String) {
            panic!(
                "backend string split intrinsic expects string delimiter, got {:?}",
                arg_values[0].ty
            );
        }

        let (ptr, len) = if receiver_is_place {
            let view = self.load_string_view(receiver_value.value);
            (view.ptr, view.len)
        } else {
            self.lower_ptr_len_from_value(expr.span, receiver_value.value, &receiver_value.ty, 32)?
        };

        let (delim_ptr, delim_len) =
            self.lower_ptr_len_from_value(expr.span, arg_values[0].value, &arg_values[0].ty, 32)?;

        let result_ir_ty = self
            .type_lowerer
            .lower_type_id(self.type_map.type_of(expr.id));
        let result_slot = self.alloc_value_slot(result_ir_ty);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        self.builder.call(
            Callee::Runtime(RuntimeFn::StringSplit),
            vec![result_slot.addr, ptr, len, delim_ptr, delim_len],
            unit_ty,
        );
        self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
        Ok(Some(self.load_slot(&result_slot)))
    }
}
