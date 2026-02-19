//! Deterministic machine payload-layout id assignment.
//!
//! Machine payload envelopes carry a compact `layout_id` used by runtime drop
//! registration/dispatch cleanup. Using compiler-internal `TypeId` indexes in
//! emitted artifacts makes assembly unstable across runs because interning
//! order can vary. This module assigns layout ids from a stable, sorted key
//! space so codegen output remains reproducible.

use std::collections::{BTreeMap, BTreeSet};

use crate::core::tree::semantic as sem;
use crate::core::types::Type;

pub(super) type PayloadLayoutIdMap = BTreeMap<String, u64>;

/// Returns the stable payload key used by layout-id assignment.
pub(super) fn payload_layout_key(ty: &Type) -> String {
    ty.to_string()
}

/// Returns the payload type carried by one machine event key.
pub(super) fn event_payload_type(key: &sem::MachineEventKeyPlan) -> &Type {
    match key {
        sem::MachineEventKeyPlan::Payload { payload_ty } => payload_ty,
        sem::MachineEventKeyPlan::Response {
            selector_ty: _,
            response_ty,
        } => response_ty,
    }
}

/// Builds a deterministic payload-layout id map for all machine payload shapes
/// present in this lowered module.
///
/// Id assignment rule:
/// - collect unique payload keys from all descriptor event kinds,
/// - sort keys lexicographically,
/// - assign ids in ascending order starting at 1.
pub(super) fn build_payload_layout_ids(machine_plans: &sem::MachinePlanMap) -> PayloadLayoutIdMap {
    let mut keys = BTreeSet::<String>::new();
    for descriptor in machine_plans.descriptors.values() {
        for event in &descriptor.event_kinds {
            keys.insert(payload_layout_key(event_payload_type(&event.key)));
        }
    }

    keys.into_iter()
        .enumerate()
        .map(|(idx, key)| (key, (idx + 1) as u64))
        .collect()
}
