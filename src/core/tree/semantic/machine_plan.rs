//! Managed typestate machine descriptor and dispatch-thunk plans.
//!
//! These plans are produced during elaborate and carried as semantic side
//! tables. Backend lowering/runtime bridge consumes them to materialize machine
//! descriptors and dispatch thunks without re-deriving typestate structure.

use std::collections::HashMap;

use crate::core::diag::Span;
use crate::core::resolve::DefId;
use crate::core::types::{Type, TypeId};

/// Descriptor plan for one typestate in managed-runtime mode.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MachineDescriptorPlan {
    /// Source typestate name (for diagnostics/symbol naming).
    pub typestate_name: String,
    /// Deterministic state-token assignments for this typestate.
    pub state_tags: Vec<MachineStateTagPlan>,
    /// Deterministic event-kind assignments for this typestate.
    pub event_kinds: Vec<MachineEventKindPlan>,
    /// Per (state, event) dispatch choice with local-first fallback contract.
    pub dispatch_table: Vec<MachineDispatchEntryPlan>,
    /// Role implementation bindings captured from source typestate declaration.
    pub role_impls: Vec<MachineRoleImplPlan>,
}

/// State-tag assignment plus state layout reference.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MachineStateTagPlan {
    pub state_name: String,
    pub state_type_def_id: DefId,
    pub state_layout_ty: TypeId,
    pub tag: u64,
}

/// Event-kind assignment plus payload layout reference.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MachineEventKindPlan {
    pub key: MachineEventKeyPlan,
    pub payload_layout_ty: TypeId,
    pub kind: u64,
}

/// Event-key identity used for deterministic kind assignment.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MachineEventKeyPlan {
    /// Ordinary incoming payload event.
    Payload { payload_ty: Type },
    /// Response payload event correlated with Pending/ReplyCap.
    Response {
        selector_ty: Type,
        response_ty: Type,
    },
}

impl MachineEventKeyPlan {
    /// Stable textual key used to sort event kinds deterministically.
    pub fn stable_key(&self) -> String {
        match self {
            Self::Payload { payload_ty } => format!("payload:{payload_ty}"),
            Self::Response {
                selector_ty,
                response_ty,
            } => format!("response:{selector_ty}:{response_ty}"),
        }
    }
}

/// Dispatch selection for one (state tag, event kind) cell.
///
/// Backend/lowering contract:
/// - try `state_local_thunk` first,
/// - if absent, try `typestate_fallback_thunk`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MachineDispatchEntryPlan {
    pub state_tag: u64,
    pub event_kind: u64,
    pub state_local_thunk: Option<DefId>,
    pub typestate_fallback_thunk: Option<DefId>,
}

/// One role implementation attached to a typestate.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MachineRoleImplPlan {
    pub path: Vec<String>,
    pub role_def_id: Option<DefId>,
    pub span: Span,
}

/// One backend thunk record for a typestate handler method.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MachineDispatchThunkPlan {
    pub handler_def_id: DefId,
    pub symbol: String,
    pub typestate_name: String,
    pub state_name: String,
    pub event_key: MachineEventKeyPlan,
    pub next_state_tag: u64,
    /// Layout ids required by runtime bridge.
    pub state_layout_ty: TypeId,
    pub payload_layout_ty: TypeId,
    pub next_state_layout_ty: TypeId,
}

/// Aggregate machine side tables produced by elaborate.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct MachinePlanMap {
    pub descriptors: HashMap<String, MachineDescriptorPlan>,
    pub thunks: HashMap<DefId, MachineDispatchThunkPlan>,
}
