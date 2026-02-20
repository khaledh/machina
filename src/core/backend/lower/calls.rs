//! Call lowering.

use crate::core::backend::lower::LowerToIrError;
use crate::core::backend::lower::lowerer::{CallInputValue, FuncLowerer, LinearValue};
use crate::core::diag::Span;
use crate::core::ir::{Callee, CastKind, RuntimeFn, ValueId};
use crate::core::resolve::{DefId, DefKind};
use crate::core::tree::ParamMode;
use crate::core::tree::{NodeId, semantic as sem};
use crate::core::types::{Type, TypeId};
use std::collections::BTreeMap;

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
    fn type_of_base_name(name: &str) -> String {
        name.split('<').next().unwrap_or(name).trim().to_string()
    }

    fn replace_tyvars_in_text_with(
        text: &str,
        vars: &mut Vec<u32>,
        overrides: Option<&BTreeMap<u32, String>>,
    ) -> String {
        let mut out = String::with_capacity(text.len());
        let mut chars = text.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == 'T' {
                let mut digits = String::new();
                while let Some(next) = chars.peek() {
                    if next.is_ascii_digit() {
                        digits.push(*next);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if !digits.is_empty()
                    && let Ok(id) = digits.parse::<u32>()
                {
                    out.push_str(&Self::type_of_var_name_with(id, vars, overrides));
                    continue;
                }
                out.push('T');
                out.push_str(&digits);
                continue;
            }
            out.push(ch);
        }
        out
    }

    fn format_nominal_name_for_type_of_with(
        name: &str,
        vars: &mut Vec<u32>,
        overrides: Option<&BTreeMap<u32, String>>,
    ) -> String {
        if !name.contains('<') {
            return Self::type_of_base_name(name);
        }
        Self::replace_tyvars_in_text_with(name, vars, overrides)
    }

    fn type_of_var_name_with(
        var_id: u32,
        vars: &mut Vec<u32>,
        overrides: Option<&BTreeMap<u32, String>>,
    ) -> String {
        if let Some(idx) = vars.iter().position(|id| *id == var_id) {
            if let Some(name) =
                overrides.and_then(|names| names.get(&var_id).or_else(|| names.get(&(idx as u32))))
            {
                return name.clone();
            }
            return format!("T{idx}");
        }
        vars.push(var_id);
        let idx = vars.len() - 1;
        if let Some(name) =
            overrides.and_then(|names| names.get(&var_id).or_else(|| names.get(&(idx as u32))))
        {
            return name.clone();
        }
        format!("T{idx}")
    }

    fn format_type_for_type_of_with(
        ty: &Type,
        vars: &mut Vec<u32>,
        overrides: Option<&BTreeMap<u32, String>>,
    ) -> String {
        match ty {
            Type::Struct { name, .. } | Type::Enum { name, .. } => {
                Self::format_nominal_name_for_type_of_with(name, vars, overrides)
            }
            Type::Array { elem_ty, dims } => {
                let dims_str = dims.iter().map(|d| d.to_string()).collect::<Vec<_>>();
                format!(
                    "{}[{}]",
                    Self::format_type_for_type_of_with(elem_ty, vars, overrides),
                    dims_str.join(", ")
                )
            }
            Type::DynArray { elem_ty } => {
                format!(
                    "{}[*]",
                    Self::format_type_for_type_of_with(elem_ty, vars, overrides)
                )
            }
            Type::Set { elem_ty } => {
                format!(
                    "set<{}>",
                    Self::format_type_for_type_of_with(elem_ty, vars, overrides)
                )
            }
            Type::Map { key_ty, value_ty } => format!(
                "map<{}, {}>",
                Self::format_type_for_type_of_with(key_ty, vars, overrides),
                Self::format_type_for_type_of_with(value_ty, vars, overrides)
            ),
            Type::Tuple { field_tys } => {
                let fields = field_tys
                    .iter()
                    .map(|ty| Self::format_type_for_type_of_with(ty, vars, overrides))
                    .collect::<Vec<_>>();
                format!("({})", fields.join(", "))
            }
            Type::Slice { elem_ty } => {
                format!(
                    "{}[]",
                    Self::format_type_for_type_of_with(elem_ty, vars, overrides)
                )
            }
            Type::Heap { elem_ty } => format!(
                "{}^",
                Self::format_type_for_type_of_with(elem_ty, vars, overrides)
            ),
            Type::Ref { mutable, elem_ty } => {
                if *mutable {
                    format!(
                        "ref mut {}",
                        Self::format_type_for_type_of_with(elem_ty, vars, overrides)
                    )
                } else {
                    format!(
                        "ref {}",
                        Self::format_type_for_type_of_with(elem_ty, vars, overrides)
                    )
                }
            }
            Type::Range { elem_ty } => {
                format!(
                    "range<{}>",
                    Self::format_type_for_type_of_with(elem_ty, vars, overrides)
                )
            }
            Type::Pending { response_tys } => format!(
                "Pending<{}>",
                response_tys
                    .iter()
                    .map(|ty| Self::format_type_for_type_of_with(ty, vars, overrides))
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
            Type::ReplyCap { response_tys } => format!(
                "ReplyCap<{}>",
                response_tys
                    .iter()
                    .map(|ty| Self::format_type_for_type_of_with(ty, vars, overrides))
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
            Type::ErrorUnion { ok_ty, err_tys } => {
                let mut variants = Vec::with_capacity(err_tys.len() + 1);
                variants.push(Self::format_type_for_type_of_with(ok_ty, vars, overrides));
                variants.extend(
                    err_tys
                        .iter()
                        .map(|ty| Self::format_type_for_type_of_with(ty, vars, overrides)),
                );
                variants.join(" | ")
            }
            Type::Fn { params, ret_ty } => {
                let params_str = params
                    .iter()
                    .map(|param| {
                        let mode = match param.mode {
                            crate::core::types::FnParamMode::In => "",
                            crate::core::types::FnParamMode::InOut => "inout ",
                            crate::core::types::FnParamMode::Out => "out ",
                            crate::core::types::FnParamMode::Sink => "sink ",
                        };
                        format!(
                            "{}{}",
                            mode,
                            Self::format_type_for_type_of_with(&param.ty, vars, overrides)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = Self::format_type_for_type_of_with(ret_ty, vars, overrides);
                if vars.is_empty() {
                    format!("fn({params_str}) -> {ret}")
                } else {
                    let tparams = vars
                        .iter()
                        .enumerate()
                        .map(|(idx, var_id)| {
                            overrides
                                .and_then(|names| {
                                    names.get(var_id).or_else(|| names.get(&(idx as u32)))
                                })
                                .cloned()
                                .unwrap_or_else(|| format!("T{idx}"))
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("fn<{tparams}>({params_str}) -> {ret}")
                }
            }
            Type::Var(var) => Self::type_of_var_name_with(var.index(), vars, overrides),
            _ => ty.to_string(),
        }
    }

    fn format_type_for_type_of(ty: &Type, vars: &mut Vec<u32>) -> String {
        Self::format_type_for_type_of_with(ty, vars, None)
    }

    fn format_nominal_for_type_of(&self, type_id: TypeId) -> Option<String> {
        let key = self.type_map.lookup_nominal_key_for_type_id(type_id)?;
        let def = self.def_table.lookup_def(key.def_id)?;
        if key.type_args.is_empty() {
            return Some(def.name.clone());
        }
        let mut vars = Vec::new();
        let args = key
            .type_args
            .iter()
            .map(|ty| Self::format_type_for_type_of(ty, &mut vars))
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

    fn type_of_function_symbol_name(&self, args: &[sem::CallArg]) -> Option<String> {
        if args.len() != 1 {
            return None;
        }
        let expr = match &args[0] {
            sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => expr,
            sem::CallArg::InOut { .. } | sem::CallArg::Out { .. } => return None,
        };
        let place = Self::type_of_symbol_place(expr)?;
        let sem::PlaceExprKind::Var { def_id, .. } = &place.kind else {
            return None;
        };
        let def = self.def_table.lookup_def(*def_id)?;
        if !matches!(def.kind, DefKind::FuncDef { .. } | DefKind::FuncDecl { .. }) {
            return None;
        }
        let fn_ty = self.type_map.lookup_def_type(def)?;
        let overrides = self.callable_type_param_names_for_type_of(*def_id);
        let mut vars = Vec::new();
        Some(Self::format_type_for_type_of_with(
            &fn_ty,
            &mut vars,
            overrides.as_ref(),
        ))
    }

    fn type_of_symbol_place<'b>(expr: &'b sem::ValueExpr) -> Option<&'b sem::PlaceExpr> {
        let mut current = expr;
        loop {
            match &current.kind {
                sem::ValueExprKind::Load { place }
                | sem::ValueExprKind::Move { place }
                | sem::ValueExprKind::ImplicitMove { place } => return Some(place),
                // Type-check inserts coercions around function symbols; peel them
                // to recover the underlying definition.
                sem::ValueExprKind::Coerce { expr, .. } => {
                    current = expr;
                }
                _ => return None,
            }
        }
    }

    fn lower_type_of_intrinsic(
        &mut self,
        expr: &sem::ValueExpr,
        args: &[sem::CallArg],
        call_plan: &sem::CallPlan,
    ) -> Result<Option<LinearValue>, LowerToIrError> {
        // Function items are pure symbols; lowering them as runtime values is not
        // required for `type_of`, so read their polymorphic signature directly.
        if let Some(type_name) = self.type_of_function_symbol_name(args) {
            let value = self.lower_static_string_value(expr.ty, type_name.as_bytes());
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

        let arg_type_id = match &args[0] {
            sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => expr.ty,
            sem::CallArg::InOut { place, .. } | sem::CallArg::Out { place, .. } => place.ty,
        };
        let type_name = self
            .format_nominal_for_type_of(arg_type_id)
            .unwrap_or_else(|| Self::format_type_for_type_of(&arg_values[0].ty, &mut Vec::new()));
        let value = self.lower_static_string_value(expr.ty, type_name.as_bytes());
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
        expr: &sem::ValueExpr,
        args: &[sem::CallArg],
        call_plan: &sem::CallPlan,
    ) -> Result<Option<LinearValue>, LowerToIrError> {
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
        let ret_ty = self.type_lowerer.lower_type_id(expr.ty);
        let ret_slot = self.alloc_value_slot(ret_ty);
        let field0_ty = self.lower_tuple_field_ty(expr.ty, 0);
        let field1_ty = self.lower_tuple_field_ty(expr.ty, 1);
        self.store_field(ret_slot.addr, 0, field0_ty, payload_word);
        self.store_field(ret_slot.addr, 1, field1_ty, layout_word);
        let packed = self.load_slot(&ret_slot);

        self.apply_call_drop_effects(call_plan, args, None, &arg_values)?;
        Ok(Some(packed))
    }

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
            sem::CallTarget::Intrinsic(sem::IntrinsicCall::TypeOf) => {
                // `type_of` is compile-time metadata extraction; it does not need
                // argument conversion checks against callee parameter types.
                return None;
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
                sem::IntrinsicCall::TypeOf => {
                    return self.lower_type_of_intrinsic(expr, args, &call_plan);
                }
                sem::IntrinsicCall::MachinePayloadPack => {
                    return self.lower_machine_payload_pack_intrinsic(expr, args, &call_plan);
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
                | sem::IntrinsicCall::MapGet
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
        self.apply_call_drop_effects(&call_plan, args, None, &arg_values)?;
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
        self.apply_call_drop_effects(&call_plan, args, Some(&receiver_value), &arg_values)?;
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
            sem::IntrinsicCall::MachinePayloadPack => {
                panic!("backend method intrinsic cannot lower payload-pack with receiver")
            }
            sem::IntrinsicCall::StringLen => {
                // String length is a field load; avoid emitting a runtime call.
                self.lower_string_len_method(expr.span, receiver, receiver_value)
                    .map(Some)
            }
            sem::IntrinsicCall::TypeOf => {
                panic!("backend type_of intrinsic cannot lower with a method receiver");
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

                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
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
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
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
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
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
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
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
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
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
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
                Ok(Some(result))
            }
            sem::IntrinsicCall::MapGet => {
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

                let key_arg = &mut arg_values[0];
                let key_addr = if key_arg.is_addr {
                    key_arg.value
                } else {
                    let addr = self.materialize_value_addr(key_arg.value, &key_arg.ty);
                    key_arg.value = addr;
                    key_arg.is_addr = true;
                    addr
                };
                let value_ir_ty = self.type_lowerer.lower_type(&value_ty);
                let value_slot = self.alloc_value_slot(value_ir_ty);

                let key_ir_ty = self.type_lowerer.lower_type(&key_ty);
                let key_layout = self.type_lowerer.ir_type_cache.layout(key_ir_ty);
                let value_layout = self.type_lowerer.ir_type_cache.layout(value_ir_ty);
                let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
                let key_size = self
                    .builder
                    .const_int(key_layout.size() as i128, false, 64, u64_ty);
                let value_size =
                    self.builder
                        .const_int(value_layout.size() as i128, false, 64, u64_ty);

                let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
                let hit = self.builder.call(
                    Callee::Runtime(RuntimeFn::MapGetValue),
                    vec![map_addr, key_addr, key_size, value_size, value_slot.addr],
                    bool_ty,
                );

                let union_ir_ty = self.type_lowerer.lower_type_id(expr.ty);
                let union_slot = self.alloc_value_slot(union_ir_ty);
                let (tag_ty, blob_ty, payload_offset, payload_ty) = {
                    let layout = self.type_lowerer.enum_layout(expr.ty);
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
                let tag = self
                    .builder
                    .binop(crate::core::ir::BinOp::Xor, one_u32, hit_u32, tag_ty);
                self.store_field(union_slot.addr, 0, tag_ty, tag);

                let payload = self.builder.load(value_slot.addr, value_ir_ty);
                let blob_ptr = self.field_addr_typed(union_slot.addr, 1, blob_ty);
                self.store_into_blob(blob_ptr, payload_offset, payload, payload_ty);

                let result = self.load_slot(&union_slot);
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
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
                self.apply_call_drop_effects(call_plan, args, Some(receiver_value), &arg_values)?;
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

    pub(super) fn lower_call_args_from_plan(
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
