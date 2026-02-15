//! Managed machine descriptor/thunk backend materialization.
//!
//! Elaborate produces `MachinePlanMap` as semantic side tables. This module
//! turns those plans into backend artifacts:
//! - synthetic dispatch-thunk functions (ABI placeholders for v1), and
//! - descriptor byte blobs kept in module globals for runtime bootstrap.

use std::collections::HashMap;

use crate::core::backend::lower::{GlobalArena, LoweredFunction};
use crate::core::ir::{FunctionBuilder, FunctionSig, GlobalId, IrTypeCache, IrTypeKind, Terminator};
use crate::core::resolve::{DefId, DefTable};
use crate::core::tree::semantic as sem;

/// Appends machine runtime artifacts derived from semantic machine plans.
///
/// The generated dispatch thunks are intentionally conservative in v1:
/// they return `MC_DISPATCH_FAULT` immediately until full decode/dispatch
/// wiring is connected in the runtime bootstrap path.
pub(super) fn append_machine_runtime_artifacts(
    machine_plans: &sem::MachinePlanMap,
    def_table: &DefTable,
    funcs: &mut Vec<LoweredFunction>,
    globals: &mut GlobalArena,
) {
    if machine_plans.descriptors.is_empty() && machine_plans.thunks.is_empty() {
        return;
    }

    let thunk_ids = assign_thunk_def_ids(machine_plans, def_table.next_def_id());
    let descriptor_globals = append_descriptor_globals(machine_plans, &thunk_ids, globals);
    append_dispatch_thunks(machine_plans, &thunk_ids, &descriptor_globals, funcs);
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
    thunk_ids: &HashMap<DefId, DefId>,
    descriptor_globals: &HashMap<String, GlobalId>,
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
        let descriptor_global = descriptor_globals.get(&plan.typestate_name).copied();
        funcs.push(build_stub_dispatch_thunk(plan, thunk_def_id, descriptor_global));
    }
}

fn build_stub_dispatch_thunk(
    plan: &sem::MachineDispatchThunkPlan,
    thunk_def_id: DefId,
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

    // Runtime callback ABI:
    //   fn(ctx, machine_id, current_state, env_ptr, txn_ptr, fault_code_ptr) -> dispatch_result
    let sig = FunctionSig {
        params: vec![u64_ty; 6],
        ret: u8_ty,
    };
    let mut builder = FunctionBuilder::new(thunk_def_id, plan.symbol.clone(), sig);
    let entry = builder.current_block();
    for _ in 0..6 {
        let _ = builder.add_block_param(entry, u64_ty);
    }

    if let Some(global_id) = descriptor_global {
        // Keep descriptor blobs live through module-DCE/global-pruning by
        // referencing them from retained dispatch thunks.
        let u8_ty = types.add(IrTypeKind::Int {
            signed: false,
            bits: 8,
        });
        let u8_ptr_ty = types.add(IrTypeKind::Ptr { elem: u8_ty });
        let _descriptor_ptr = builder.const_global_addr(global_id, u8_ptr_ty);
    }

    // Placeholder behavior until real state/payload decode + handler call ABI
    // is wired: always return `MC_DISPATCH_FAULT` (enum value 1).
    let fault = builder.const_int(1, false, 8, u8_ty);
    builder.terminate(Terminator::Return { value: Some(fault) });

    LoweredFunction {
        func: builder.finish(),
        types,
        globals: Vec::new(),
    }
}

fn append_descriptor_globals(
    machine_plans: &sem::MachinePlanMap,
    thunk_ids: &HashMap<DefId, DefId>,
    globals: &mut GlobalArena,
) -> HashMap<String, GlobalId> {
    let mut out = HashMap::new();
    let mut descriptors: Vec<_> = machine_plans.descriptors.values().collect();
    descriptors.sort_by(|a, b| a.typestate_name.cmp(&b.typestate_name));

    for desc in descriptors {
        let bytes = serialize_descriptor(desc, thunk_ids);
        let global_id = globals.add_bytes(bytes);
        out.insert(desc.typestate_name.clone(), global_id);
    }
    out
}

fn serialize_descriptor(desc: &sem::MachineDescriptorPlan, thunk_ids: &HashMap<DefId, DefId>) -> Vec<u8> {
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
        push_u64(&mut bytes, role.role_def_id.map(|id| id.0 as u64).unwrap_or(0));
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
