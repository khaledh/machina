//! Managed machine descriptor/thunk backend materialization.
//!
//! Elaborate produces `MachinePlanMap` as semantic side tables. This module
//! turns those plans into backend artifacts:
//! - synthetic dispatch-thunk functions (ABI placeholders for v1), and
//! - descriptor byte blobs kept in module globals for runtime bootstrap.

use std::collections::HashMap;

use crate::core::backend::lower::types::TypeLowerer;
use crate::core::backend::lower::{GlobalArena, LoweredFunction};
use crate::core::ir::{
    Callee, CastKind, CmpOp, FunctionBuilder, FunctionSig, GlobalId, IrStructField, IrTypeCache,
    IrTypeId, IrTypeKind, RuntimeFn, Terminator, ValueId,
};
use crate::core::resolve::{DefId, DefKind, DefTable};
use crate::core::tree::semantic as sem;
use crate::core::typecheck::type_map::TypeMap;
use crate::core::types::{FnParam, Type};

/// Appends machine runtime artifacts derived from semantic machine plans.
///
/// The generated dispatch thunks are intentionally conservative in v1:
/// they return `MC_DISPATCH_FAULT` immediately until full decode/dispatch
/// wiring is connected in the runtime bootstrap path.
pub(super) fn append_machine_runtime_artifacts(
    machine_plans: &sem::MachinePlanMap,
    def_table: &DefTable,
    type_map: &TypeMap,
    funcs: &mut Vec<LoweredFunction>,
    globals: &mut GlobalArena,
) {
    if machine_plans.descriptors.is_empty() && machine_plans.thunks.is_empty() {
        return;
    }

    let thunk_ids = assign_thunk_def_ids(machine_plans, def_table.next_def_id());
    let descriptor_globals = append_descriptor_globals(machine_plans, &thunk_ids, globals);
    append_dispatch_thunks(
        machine_plans,
        def_table,
        type_map,
        &thunk_ids,
        &descriptor_globals,
        funcs,
    );
    append_bootstrap_registration_fn(
        machine_plans,
        type_map,
        def_table.next_def_id(),
        &thunk_ids,
        &descriptor_globals,
        funcs,
    );
    append_descriptor_id_helpers(machine_plans, type_map, def_table, funcs);
}

fn assign_thunk_def_ids(
    machine_plans: &sem::MachinePlanMap,
    first_def_id: DefId,
) -> HashMap<DefId, DefId> {
    let mut plans: Vec<_> = machine_plans.thunks.iter().collect();
    plans.sort_by(|(a_id, a_plan), (b_id, b_plan)| {
        a_plan
            .symbol
            .cmp(&b_plan.symbol)
            .then_with(|| a_id.0.cmp(&b_id.0))
    });

    let mut out = HashMap::with_capacity(plans.len());
    for (idx, (handler_def_id, _)) in plans.into_iter().enumerate() {
        let thunk_def_id = DefId(first_def_id.0 + idx as u32);
        out.insert(*handler_def_id, thunk_def_id);
    }
    out
}

fn append_dispatch_thunks(
    machine_plans: &sem::MachinePlanMap,
    def_table: &DefTable,
    type_map: &TypeMap,
    thunk_ids: &HashMap<DefId, DefId>,
    descriptor_globals: &HashMap<String, (GlobalId, usize)>,
    funcs: &mut Vec<LoweredFunction>,
) {
    let mut plans: Vec<_> = machine_plans.thunks.values().collect();
    plans.sort_by(|a, b| {
        a.symbol
            .cmp(&b.symbol)
            .then_with(|| a.handler_def_id.0.cmp(&b.handler_def_id.0))
    });

    for plan in plans {
        let Some(thunk_def_id) = thunk_ids.get(&plan.handler_def_id).copied() else {
            continue;
        };
        let descriptor_global = descriptor_globals
            .get(&plan.typestate_name)
            .map(|(id, _)| *id);
        funcs.push(build_dispatch_thunk(
            plan,
            thunk_def_id,
            descriptor_global,
            def_table,
            type_map,
        ));
    }
}

fn append_bootstrap_registration_fn(
    machine_plans: &sem::MachinePlanMap,
    type_map: &TypeMap,
    first_def_id: DefId,
    thunk_ids: &HashMap<DefId, DefId>,
    descriptor_globals: &HashMap<String, (GlobalId, usize)>,
    funcs: &mut Vec<LoweredFunction>,
) {
    if thunk_ids.is_empty() && descriptor_globals.is_empty() {
        return;
    }

    let bootstrap_def_id = DefId(first_def_id.0 + thunk_ids.len() as u32);
    let mut lowerer = TypeLowerer::new(type_map);
    let u64_ty = lowerer.ir_type_cache.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let u8_ty = lowerer.ir_type_cache.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    let u8_ptr_ty = lowerer.ptr_to(u8_ty);
    let unit_ty = lowerer.lower_type(&Type::Unit);
    let sig = FunctionSig {
        params: Vec::new(),
        ret: unit_ty,
    };
    let mut builder = FunctionBuilder::new(bootstrap_def_id, "__mc_machine_bootstrap", sig);

    let thunk_fn_ty = lowerer.ir_type_cache.add(IrTypeKind::Fn {
        params: vec![u64_ty; 6],
        ret: u8_ty,
    });
    let mut rows = Vec::with_capacity(thunk_ids.len());
    for (handler_def_id, thunk_def_id) in thunk_ids {
        let next_state_tag = machine_plans
            .thunks
            .get(handler_def_id)
            .map(|plan| plan.next_state_tag)
            .unwrap_or(0);
        rows.push((*thunk_def_id, next_state_tag));
    }
    rows.sort_by_key(|(id, _)| id.0);
    for (thunk_def_id, next_state_tag) in rows {
        let thunk_id_word = builder.const_int(thunk_def_id.0 as i128, false, 64, u64_ty);
        let thunk_addr = builder.const_func_addr(thunk_def_id, thunk_fn_ty);
        let thunk_addr_word = builder.cast(CastKind::PtrToInt, thunk_addr, u64_ty);
        let next_state_tag_word = builder.const_int(next_state_tag as i128, false, 64, u64_ty);
        builder.call(
            Callee::Runtime(RuntimeFn::MachineRegisterThunkWithTag),
            vec![thunk_id_word, thunk_addr_word, next_state_tag_word],
            unit_ty,
        );
    }

    let mut descriptors: Vec<_> = descriptor_globals.iter().collect();
    descriptors.sort_by(|(a, _), (b, _)| a.cmp(b));
    for (_, (global_id, desc_len)) in descriptors {
        let desc_ptr = builder.const_global_addr(*global_id, u8_ptr_ty);
        let desc_len_val = builder.const_int(*desc_len as i128, false, 64, u64_ty);
        let _desc_id = builder.call(
            Callee::Runtime(RuntimeFn::MachineRegisterDescriptor),
            vec![desc_ptr, desc_len_val],
            u64_ty,
        );
    }

    let unit_zero = builder.const_int(0, false, 8, unit_ty);
    builder.terminate(Terminator::Return {
        value: Some(unit_zero),
    });
    funcs.push(LoweredFunction {
        func: builder.finish(),
        types: lowerer.ir_type_cache,
        globals: Vec::new(),
    });
}

fn append_descriptor_id_helpers(
    machine_plans: &sem::MachinePlanMap,
    type_map: &TypeMap,
    def_table: &DefTable,
    funcs: &mut Vec<LoweredFunction>,
) {
    if machine_plans.descriptors.is_empty() {
        return;
    }

    let mut descriptor_names: Vec<_> = machine_plans.descriptors.keys().cloned().collect();
    descriptor_names.sort();
    for (idx, typestate_name) in descriptor_names.iter().enumerate() {
        // Keep helper ids aligned with bootstrap registration order so source-
        // level `Typestate::spawn(...)` lowering can bind descriptors without
        // hard-coding typestate-specific runtime metadata in user code.
        let helper_name = format!("__mc_machine_descriptor_id_{typestate_name}");
        let helper_def_id = def_table
            .defs()
            .iter()
            .find(|def| {
                def.name == helper_name
                    && matches!(def.kind, DefKind::FuncDef { .. } | DefKind::FuncDecl { .. })
            })
            .map(|def| def.id);
        let Some(helper_def_id) = helper_def_id else {
            continue;
        };
        funcs.push(build_descriptor_id_helper(
            helper_def_id,
            &helper_name,
            idx as u64 + 1,
            type_map,
        ));
    }
}

fn build_descriptor_id_helper(
    helper_def_id: DefId,
    helper_name: &str,
    descriptor_id: u64,
    type_map: &TypeMap,
) -> LoweredFunction {
    let mut lowerer = TypeLowerer::new(type_map);
    let ret_ty = lowerer.ir_type_cache.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let sig = FunctionSig {
        params: Vec::new(),
        ret: ret_ty,
    };
    let mut builder = FunctionBuilder::new(helper_def_id, helper_name, sig);
    let ret_val = builder.const_int(descriptor_id as i128, false, 64, ret_ty);
    builder.terminate(Terminator::Return {
        value: Some(ret_val),
    });

    LoweredFunction {
        func: builder.finish(),
        types: lowerer.ir_type_cache,
        globals: Vec::new(),
    }
}

fn build_dispatch_thunk(
    plan: &sem::MachineDispatchThunkPlan,
    thunk_def_id: DefId,
    descriptor_global: Option<GlobalId>,
    def_table: &DefTable,
    type_map: &TypeMap,
) -> LoweredFunction {
    let state_ty_src = type_map.type_table().get(plan.state_layout_ty);
    let payload_ty_src = type_map.type_table().get(plan.payload_layout_ty);
    let next_state_ty_src = type_map.type_table().get(plan.next_state_layout_ty);
    if contains_unresolved_type(state_ty_src)
        || contains_unresolved_type(payload_ty_src)
        || contains_unresolved_type(next_state_ty_src)
    {
        return build_fault_only_dispatch_thunk(thunk_def_id, &plan.symbol, descriptor_global);
    }

    let mut lowerer = TypeLowerer::new(type_map);
    let u64_ty = lowerer.ir_type_cache.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let u8_ty = lowerer.ir_type_cache.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    let bool_ty = lowerer.ir_type_cache.add(IrTypeKind::Bool);
    let handler_ret_ty = lowerer.lower_type_id(plan.next_state_layout_ty);
    let handler_state_ty = lowerer.lower_type_id(plan.state_layout_ty);
    let handler_payload_ty = lowerer.lower_type_id(plan.payload_layout_ty);
    let handler_payload_ptr_ty = lowerer.ptr_to(handler_payload_ty);
    let handler_state_ptr_ty = lowerer.ptr_to(handler_state_ty);
    let handler_next_state_ptr_ty = lowerer.ptr_to(handler_ret_ty);
    let env_ty = add_machine_env_type(&mut lowerer.ir_type_cache, u64_ty);
    let env_ptr_ty = lowerer.ptr_to(env_ty);
    let txn_prefix_ty = add_txn_prefix_type(&mut lowerer.ir_type_cache, u64_ty);
    let txn_prefix_ptr_ty = lowerer.ptr_to(txn_prefix_ty);
    let u8_ptr_ty = lowerer.ptr_to(u8_ty);
    let u64_ptr_ty = lowerer.ptr_to(u64_ty);

    let handler_def = def_table
        .lookup_def(plan.handler_def_id)
        .unwrap_or_else(|| {
            panic!(
                "backend machine thunk missing handler def for {:?}",
                plan.handler_def_id
            )
        });
    let handler_ty = type_map.lookup_def_type(handler_def).unwrap_or_else(|| {
        panic!(
            "backend machine thunk missing handler type for {:?}",
            plan.handler_def_id
        )
    });
    let (handler_params, handler_ret_ty_src) = match handler_ty {
        Type::Fn { params, ret_ty } => (params.clone(), ret_ty),
        other => panic!(
            "backend machine thunk expected fn handler type, found {:?}",
            other
        ),
    };
    if handler_params
        .iter()
        .any(|param| contains_unresolved_type(&param.ty))
        || contains_unresolved_type(handler_ret_ty_src.as_ref())
    {
        // Some synthetic handler signatures can still carry unresolved parts in
        // early slices. Fall back to deterministic FAULT thunk until the full
        // managed ABI path resolves all generated types before lowering.
        return build_fault_only_dispatch_thunk(thunk_def_id, &plan.symbol, descriptor_global);
    }

    // Runtime callback ABI:
    //   fn(ctx, machine_id, current_state, env_ptr, txn_ptr, fault_code_ptr) -> dispatch_result
    let sig = FunctionSig {
        params: vec![u64_ty; 6],
        ret: u8_ty,
    };
    let mut builder = FunctionBuilder::new(thunk_def_id, plan.symbol.clone(), sig);
    let entry = builder.current_block();
    let mut abi_params = Vec::new();
    for _ in 0..6 {
        abi_params.push(builder.add_block_param(entry, u64_ty));
    }

    if let Some(global_id) = descriptor_global {
        // Keep descriptor blobs live through module-DCE/global-pruning by
        // referencing them from retained dispatch thunks.
        let _descriptor_ptr = builder.const_global_addr(global_id, u8_ptr_ty);
    }

    // Decode ABI pointers and current-state token.
    let current_state_word = abi_params[2];
    let env_ptr_word = abi_params[3];
    let txn_ptr_word = abi_params[4];
    let fault_ptr_word = abi_params[5];
    let env_ptr = builder.cast(CastKind::IntToPtr, env_ptr_word, env_ptr_ty);
    let txn_ptr = builder.cast(CastKind::IntToPtr, txn_ptr_word, txn_prefix_ptr_ty);
    let fault_ptr = builder.cast(CastKind::IntToPtr, fault_ptr_word, u64_ptr_ty);
    let state_ptr = builder.cast(CastKind::IntToPtr, current_state_word, handler_state_ptr_ty);

    // Envelope payload + correlation fields.
    let pending_id = load_struct_field(&mut builder, env_ptr, 3, u64_ty, u64_ptr_ty);
    let reply_cap_id = load_struct_field(&mut builder, env_ptr, 2, u64_ty, u64_ptr_ty);
    let payload0 = load_struct_field(&mut builder, env_ptr, 4, u64_ty, u64_ptr_ty);
    let payload_ptr = builder.cast(CastKind::IntToPtr, payload0, handler_payload_ptr_ty);
    let origin_payload0 = load_struct_field(&mut builder, env_ptr, 6, u64_ty, u64_ptr_ty);
    let origin_payload_ptr =
        builder.cast(CastKind::IntToPtr, origin_payload0, handler_payload_ptr_ty);

    // Build a typed function pointer for indirect call. Using `Callee::Value`
    // avoids cross-function type-id coupling in verifier signature checks.
    let mut call_arg_tys = Vec::new();
    let mut call_args = Vec::new();
    for (idx, param) in handler_params.iter().enumerate() {
        let abi_ty = lower_handler_param_abi_ty(param, &mut lowerer);
        call_arg_tys.push(abi_ty);

        let from_origin = plan.provenance_param_index == Some(idx);
        let source_payload_ptr = if from_origin {
            origin_payload_ptr
        } else {
            payload_ptr
        };
        let arg = if idx == 0 {
            state_ptr
        } else if matches!(param.ty, Type::Pending { .. }) {
            pending_id
        } else if matches!(param.ty, Type::ReplyCap { .. }) {
            reply_cap_id
        } else if param.ty.is_scalar() {
            // Scalar payload parameters are loaded by value from payload box.
            let scalar_ptr_ty = lowerer.ptr_to(abi_ty);
            let typed_ptr = builder.cast(CastKind::PtrToPtr, source_payload_ptr, scalar_ptr_ty);
            builder.load(typed_ptr, abi_ty)
        } else {
            // Aggregate payload parameters are passed by pointer.
            builder.cast(CastKind::PtrToPtr, source_payload_ptr, abi_ty)
        };
        call_args.push(arg);
    }
    let handler_fn_ty = lowerer.ir_type_cache.add(IrTypeKind::Fn {
        params: call_arg_tys,
        ret: handler_ret_ty,
    });
    let handler_fn = builder.const_func_addr(plan.handler_def_id, handler_fn_ty);
    let next_state_value = builder.call(Callee::Value(handler_fn), call_args, handler_ret_ty);

    // Heap-box the returned next state token.
    let layout = lowerer.ir_type_cache.layout(handler_ret_ty);
    // Zero-sized state layouts still need a non-null token pointer for runtime.
    let alloc_size = std::cmp::max(layout.size(), 1);
    let size = builder.const_int(alloc_size as i128, false, 64, u64_ty);
    let align = builder.const_int(layout.align() as i128, false, 64, u64_ty);
    let next_state_ptr_u8 = builder.call(
        Callee::Runtime(RuntimeFn::Alloc),
        vec![size, align],
        u8_ptr_ty,
    );
    let next_state_word = builder.cast(CastKind::PtrToInt, next_state_ptr_u8, u64_ty);

    // Allocation failure => deterministic FAULT result and fault code.
    let zero = builder.const_int(0, false, 64, u64_ty);
    let alloc_is_null = builder.cmp(CmpOp::Eq, next_state_word, zero, bool_ty);
    let bb_fault = builder.add_block();
    let bb_ok = builder.add_block();
    builder.terminate(Terminator::CondBr {
        cond: alloc_is_null,
        then_bb: bb_fault,
        then_args: Vec::new(),
        else_bb: bb_ok,
        else_args: Vec::new(),
    });

    builder.select_block(bb_fault);
    let fault_code = builder.const_int(1, false, 64, u64_ty);
    builder.store(fault_ptr, fault_code);
    let fault_result = builder.const_int(1, false, 8, u8_ty);
    builder.terminate(Terminator::Return {
        value: Some(fault_result),
    });

    builder.select_block(bb_ok);
    let typed_next_state_ptr = builder.cast(
        CastKind::PtrToPtr,
        next_state_ptr_u8,
        handler_next_state_ptr_ty,
    );
    builder.store(typed_next_state_ptr, next_state_value);

    // Stage txn next-state fields.
    let has_next_ptr = builder.field_addr(txn_ptr, 0, u64_ptr_ty);
    let one_u64 = builder.const_int(1, false, 64, u64_ty);
    builder.store(has_next_ptr, one_u64);
    let next_state_field_ptr = builder.field_addr(txn_ptr, 1, u64_ptr_ty);
    builder.store(next_state_field_ptr, next_state_word);

    let ok = builder.const_int(0, false, 8, u8_ty);
    builder.terminate(Terminator::Return { value: Some(ok) });

    LoweredFunction {
        func: builder.finish(),
        types: lowerer.ir_type_cache,
        globals: Vec::new(),
    }
}

fn build_fault_only_dispatch_thunk(
    thunk_def_id: DefId,
    symbol: &str,
    descriptor_global: Option<GlobalId>,
) -> LoweredFunction {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let u8_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    let u8_ptr_ty = types.add(IrTypeKind::Ptr { elem: u8_ty });
    let sig = FunctionSig {
        params: vec![u64_ty; 6],
        ret: u8_ty,
    };
    let mut builder = FunctionBuilder::new(thunk_def_id, symbol.to_string(), sig);
    let entry = builder.current_block();
    for _ in 0..6 {
        let _ = builder.add_block_param(entry, u64_ty);
    }
    if let Some(global_id) = descriptor_global {
        let _descriptor_ptr = builder.const_global_addr(global_id, u8_ptr_ty);
    }
    let fault = builder.const_int(1, false, 8, u8_ty);
    builder.terminate(Terminator::Return { value: Some(fault) });
    LoweredFunction {
        func: builder.finish(),
        types,
        globals: Vec::new(),
    }
}

fn lower_handler_param_abi_ty(param: &FnParam, lowerer: &mut TypeLowerer<'_>) -> IrTypeId {
    if matches!(param.ty, Type::Pending { .. } | Type::ReplyCap { .. }) {
        return lowerer.lower_type(&Type::uint(64));
    }

    let ty = lowerer.lower_type(&param.ty);
    match param.mode {
        crate::core::types::FnParamMode::In | crate::core::types::FnParamMode::Sink => {
            if param.ty.is_scalar() {
                ty
            } else {
                lowerer.ptr_to(ty)
            }
        }
        crate::core::types::FnParamMode::Out | crate::core::types::FnParamMode::InOut => {
            lowerer.ptr_to(ty)
        }
    }
}

fn add_machine_env_type(types: &mut IrTypeCache, u64_ty: IrTypeId) -> IrTypeId {
    let fields = vec![
        IrStructField {
            name: "kind".to_string(),
            ty: u64_ty,
        },
        IrStructField {
            name: "src".to_string(),
            ty: u64_ty,
        },
        IrStructField {
            name: "reply_cap_id".to_string(),
            ty: u64_ty,
        },
        IrStructField {
            name: "pending_id".to_string(),
            ty: u64_ty,
        },
        IrStructField {
            name: "payload0".to_string(),
            ty: u64_ty,
        },
        IrStructField {
            name: "payload1".to_string(),
            ty: u64_ty,
        },
        IrStructField {
            name: "origin_payload0".to_string(),
            ty: u64_ty,
        },
        IrStructField {
            name: "origin_payload1".to_string(),
            ty: u64_ty,
        },
    ];
    types.add_named(
        IrTypeKind::Struct { fields },
        "__mc_machine_env".to_string(),
    )
}

fn add_txn_prefix_type(types: &mut IrTypeCache, u64_ty: IrTypeId) -> IrTypeId {
    let fields = vec![
        IrStructField {
            name: "has_next_state".to_string(),
            ty: u64_ty,
        },
        IrStructField {
            name: "next_state".to_string(),
            ty: u64_ty,
        },
        IrStructField {
            name: "next_state_tag".to_string(),
            ty: u64_ty,
        },
    ];
    types.add_named(
        IrTypeKind::Struct { fields },
        "__mc_machine_txn_prefix".to_string(),
    )
}

fn load_struct_field(
    builder: &mut FunctionBuilder,
    base_ptr: ValueId,
    field_idx: usize,
    field_ty: IrTypeId,
    field_ptr_ty: IrTypeId,
) -> ValueId {
    let ptr = builder.field_addr(base_ptr, field_idx, field_ptr_ty);
    builder.load(ptr, field_ty)
}

fn contains_unresolved_type(ty: &Type) -> bool {
    match ty {
        Type::Unknown | Type::Var(_) => true,
        Type::Fn { params, ret_ty } => {
            params
                .iter()
                .any(|param| contains_unresolved_type(&param.ty))
                || contains_unresolved_type(ret_ty.as_ref())
        }
        Type::Tuple { field_tys } => field_tys.iter().any(contains_unresolved_type),
        Type::Struct { fields, .. } => fields
            .iter()
            .any(|field| contains_unresolved_type(&field.ty)),
        Type::Enum { variants, .. } => variants
            .iter()
            .any(|variant| variant.payload.iter().any(contains_unresolved_type)),
        Type::Array { elem_ty, .. } | Type::Slice { elem_ty } | Type::Heap { elem_ty } => {
            contains_unresolved_type(elem_ty.as_ref())
        }
        Type::DynArray { elem_ty } => contains_unresolved_type(elem_ty.as_ref()),
        Type::Set { elem_ty } => contains_unresolved_type(elem_ty.as_ref()),
        Type::Map { key_ty, value_ty } => {
            contains_unresolved_type(key_ty.as_ref()) || contains_unresolved_type(value_ty.as_ref())
        }
        Type::Ref { elem_ty, .. } => contains_unresolved_type(elem_ty.as_ref()),
        Type::Pending { response_tys } | Type::ReplyCap { response_tys } => {
            response_tys.iter().any(contains_unresolved_type)
        }
        Type::ErrorUnion { ok_ty, err_tys } => {
            contains_unresolved_type(ok_ty.as_ref()) || err_tys.iter().any(contains_unresolved_type)
        }
        Type::Range { elem_ty } => contains_unresolved_type(elem_ty.as_ref()),
        Type::Int { .. } | Type::Bool | Type::Char | Type::String | Type::Unit => false,
    }
}

fn append_descriptor_globals(
    machine_plans: &sem::MachinePlanMap,
    thunk_ids: &HashMap<DefId, DefId>,
    globals: &mut GlobalArena,
) -> HashMap<String, (GlobalId, usize)> {
    let mut out = HashMap::new();
    let mut descriptors: Vec<_> = machine_plans.descriptors.values().collect();
    descriptors.sort_by(|a, b| a.typestate_name.cmp(&b.typestate_name));

    for desc in descriptors {
        let bytes = serialize_descriptor(desc, thunk_ids);
        let byte_len = bytes.len();
        let global_id = globals.add_bytes(bytes);
        out.insert(desc.typestate_name.clone(), (global_id, byte_len));
    }
    out
}

fn serialize_descriptor(
    desc: &sem::MachineDescriptorPlan,
    thunk_ids: &HashMap<DefId, DefId>,
) -> Vec<u8> {
    let mut bytes = Vec::new();

    // Magic + schema version for forward compatibility.
    bytes.extend_from_slice(b"MCHD");
    push_u32(&mut bytes, 1);
    push_string(&mut bytes, &desc.typestate_name);

    push_u32(&mut bytes, desc.state_tags.len() as u32);
    push_u32(&mut bytes, desc.event_kinds.len() as u32);
    push_u32(&mut bytes, desc.dispatch_table.len() as u32);
    push_u32(&mut bytes, desc.role_impls.len() as u32);

    let mut state_tags = desc.state_tags.clone();
    state_tags.sort_by_key(|s| s.tag);
    for state in &state_tags {
        push_u64(&mut bytes, state.tag);
        push_u64(&mut bytes, state.state_type_def_id.0 as u64);
        push_u64(&mut bytes, state.state_layout_ty.index() as u64);
        push_string(&mut bytes, &state.state_name);
    }

    let mut event_kinds = desc.event_kinds.clone();
    event_kinds.sort_by_key(|e| e.kind);
    for event in &event_kinds {
        push_u64(&mut bytes, event.kind);
        push_u64(&mut bytes, event.payload_layout_ty.index() as u64);
        match &event.key {
            sem::MachineEventKeyPlan::Payload { payload_ty } => {
                push_u8(&mut bytes, 0);
                push_string(&mut bytes, &payload_ty.to_string());
            }
            sem::MachineEventKeyPlan::Response {
                selector_ty,
                response_ty,
            } => {
                push_u8(&mut bytes, 1);
                push_string(&mut bytes, &selector_ty.to_string());
                push_string(&mut bytes, &response_ty.to_string());
            }
        }
    }

    let mut dispatch_rows = desc.dispatch_table.clone();
    dispatch_rows.sort_by_key(|row| (row.state_tag, row.event_kind));
    for row in &dispatch_rows {
        push_u64(&mut bytes, row.state_tag);
        push_u64(&mut bytes, row.event_kind);
        push_u64(
            &mut bytes,
            row.state_local_thunk
                .and_then(|id| thunk_ids.get(&id).copied())
                .map(|id| id.0 as u64)
                .unwrap_or(0),
        );
        push_u64(
            &mut bytes,
            row.typestate_fallback_thunk
                .and_then(|id| thunk_ids.get(&id).copied())
                .map(|id| id.0 as u64)
                .unwrap_or(0),
        );
    }

    let mut role_impls = desc.role_impls.clone();
    role_impls.sort_by(|a, b| a.path.join("::").cmp(&b.path.join("::")));
    for role in &role_impls {
        push_string(&mut bytes, &role.path.join("::"));
        push_u64(
            &mut bytes,
            role.role_def_id.map(|id| id.0 as u64).unwrap_or(0),
        );
    }

    bytes
}

fn push_u8(out: &mut Vec<u8>, value: u8) {
    out.push(value);
}

fn push_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_u64(out: &mut Vec<u8>, value: u64) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_string(out: &mut Vec<u8>, value: &str) {
    let bytes = value.as_bytes();
    push_u32(out, bytes.len() as u32);
    out.extend_from_slice(bytes);
}
