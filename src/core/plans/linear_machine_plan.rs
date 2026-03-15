//! Elaboration plans for hosted linear machines.
//!
//! These plans model machines that host many linear-type instances. They are
//! metadata-only for now: the
//! backend does not consume them yet, but they give us a deterministic bridge
//! from `LinearIndex` + resolved defs into later lowering/runtime work.

use std::collections::BTreeMap;

use crate::core::resolve::DefId;
use crate::core::types::Type;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct LinearMachinePlanMap {
    pub machines: BTreeMap<String, LinearMachinePlan>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearMachinePlan {
    pub machine_name: String,
    pub hosted_type_name: String,
    pub key_field_name: String,
    pub key_ty: Type,
    pub state_tags: Vec<LinearMachineStateTagPlan>,
    pub initial_state_tag: u64,
    pub actions: Vec<LinearMachineActionPlan>,
    pub triggers: Vec<LinearMachineTriggerPlan>,
    pub handle_type_def_id: DefId,
    pub spawn_fn_def_id: DefId,
    pub create_fn_def_ids: BTreeMap<String, DefId>,
    pub resume_fn_def_ids: BTreeMap<String, DefId>,
    pub deliver_fn_def_ids: BTreeMap<String, DefId>,
    pub wait_fn_def_ids: BTreeMap<String, DefId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearMachineStateTagPlan {
    pub state_name: String,
    pub tag: u64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearMachineActionPlan {
    pub action_name: String,
    pub source_state_name: String,
    pub source_state_tag: u64,
    pub target_state_name: String,
    pub target_state_tag: u64,
    pub handler_def_id: DefId,
    pub param_tys: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearMachineTriggerPlan {
    pub event_type_name: String,
    pub source_state_name: String,
    pub source_state_tag: u64,
    pub target_state_name: String,
    pub target_state_tag: u64,
    pub handler_def_id: DefId,
}
