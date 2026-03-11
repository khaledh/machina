//! The [`LinearIndex`] — pre-resolve metadata about linear types and machine hosting.
//!
//! Built from the parsed AST before resolve, the index captures:
//! - Per-type: state names, initial state, roles, action signatures
//! - Per-machine: hosted type, key field, generated handle type name
//! - Per-expression: hosted action markers (populated during rewriting)
//!
//! Downstream consumers (type checker, constraint collector, solver) use this
//! to validate session entry (`create`/`resume`), check role permissions, and
//! type hosted action results as fallible.

use std::collections::HashMap;

use crate::core::ast::{Module, NodeId, TypeDefKind, TypeExpr};

use super::machine::machine_handle_type_name;

/// Metadata index for linear types, built after parsing and threaded through
/// the compiler pipeline. The `hosted_action_exprs` map is populated during
/// desugaring (not at build time) because it requires expression-level rewriting
/// to determine which calls are hosted.
#[derive(Clone, Debug, Default)]
pub struct LinearIndex {
    pub types: HashMap<String, LinearTypeInfo>,
    pub machine_hosts: HashMap<String, LinearHostInfo>,
    /// Expression IDs of method calls on hosted bindings. Populated by the
    /// direct-mode rewriter when it encounters an action call on a binding
    /// that originated from `create(...)`. The type checker uses these to
    /// emit `LinearSessionAction` obligations with fallible result types.
    pub hosted_action_exprs: HashMap<NodeId, HostedActionExprInfo>,
}

#[derive(Clone, Debug)]
pub struct LinearTypeInfo {
    pub state_names: Vec<String>,
    /// The first declared state — used as the initial state for `create(...)`.
    pub initial_state: Option<String>,
    pub roles: HashMap<String, LinearRoleInfo>,
    /// Keyed by (source_state, action_name).
    pub actions: HashMap<(String, String), LinearActionInfo>,
}

#[derive(Clone, Debug)]
pub struct LinearRoleInfo {
    pub allowed_actions: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct LinearHostInfo {
    pub hosted_type_name: String,
    pub key_field: String,
    /// Name of the generated struct type that represents a handle to this machine.
    pub handle_type_name: String,
}

#[derive(Clone, Debug)]
pub struct LinearActionInfo {
    pub target_state: String,
    /// AST-level parameter types from the action declaration. Resolved to
    /// semantic types by the constraint collector when emitting obligations.
    pub params: Vec<TypeExpr>,
}

/// Marker recorded by the rewriter for each expression that is a hosted
/// action call. Carries enough info for the constraint collector to look up
/// the action in the linear index and emit the right obligation.
#[derive(Clone, Debug)]
pub struct HostedActionExprInfo {
    pub type_name: String,
    pub source_state: String,
    pub action_name: String,
}

pub fn build_linear_index(module: &Module) -> LinearIndex {
    let mut types = HashMap::new();
    for type_def in module.type_defs() {
        let TypeDefKind::Linear { linear } = &type_def.kind else {
            continue;
        };

        let roles = linear
            .roles
            .iter()
            .map(|role| {
                (
                    role.name.clone(),
                    LinearRoleInfo {
                        allowed_actions: role.allowed_actions.clone(),
                    },
                )
            })
            .collect();

        let state_names = linear
            .states
            .iter()
            .map(|state| state.name.clone())
            .collect::<Vec<_>>();

        let actions = linear
            .actions
            .iter()
            .map(|action| {
                (
                    (action.source_state.clone(), action.name.clone()),
                    LinearActionInfo {
                        target_state: action.target_state.clone(),
                        params: action.params.iter().map(|param| param.ty.clone()).collect(),
                    },
                )
            })
            .collect();

        let initial_state = linear.states.first().map(|state| state.name.clone());

        types.insert(
            type_def.name.clone(),
            LinearTypeInfo {
                state_names,
                initial_state,
                roles,
                actions,
            },
        );
    }

    let mut machine_hosts = HashMap::new();
    for machine_def in module.machine_defs() {
        machine_hosts.insert(
            machine_def.name.clone(),
            LinearHostInfo {
                hosted_type_name: machine_def.host.type_name.clone(),
                key_field: machine_def.host.key_field.clone(),
                handle_type_name: machine_handle_type_name(&machine_def.name),
            },
        );
    }

    LinearIndex {
        types,
        machine_hosts,
        hosted_action_exprs: HashMap::new(),
    }
}
