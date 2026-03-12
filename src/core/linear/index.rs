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

use super::machine::{machine_action_override_fn_name, machine_handle_type_name};

/// Metadata index for linear types, built after parsing and threaded through
/// the compiler pipeline. The `hosted_action_exprs` map is populated during
/// desugaring (not at build time) because it requires expression-level rewriting
/// to determine which calls are hosted.
#[derive(Clone, Debug, Default)]
pub struct LinearIndex {
    pub types: HashMap<String, LinearTypeInfo>,
    pub machine_hosts: HashMap<String, LinearHostInfo>,
    /// Generated machine action override helpers keyed by function name. The
    /// rewriter uses this to seed the source-state binding when rewriting the
    /// cloned helper bodies.
    pub action_override_fns: HashMap<String, GeneratedActionOverrideInfo>,
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
    pub key_ty: TypeExpr,
    /// Name of the generated struct type that represents a handle to this machine.
    pub handle_type_name: String,
    /// Generated override helper functions keyed by action name. When present,
    /// hosted action calls dispatch through these helpers instead of the base
    /// linear action implementation.
    pub action_overrides: HashMap<String, String>,
}

#[derive(Clone, Debug)]
pub struct GeneratedActionOverrideInfo {
    pub hosted_type_name: String,
    pub source_state: String,
    pub instance_param_name: String,
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
    let module_type_defs = module.type_defs();
    let type_defs = module_type_defs
        .iter()
        .map(|type_def| (type_def.name.clone(), *type_def))
        .collect::<HashMap<_, _>>();

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
    let mut action_override_fns = HashMap::new();
    for machine_def in module.machine_defs() {
        // The index is built even for invalid source so early validation can
        // report targeted machine-host diagnostics. Skip malformed host entries
        // here and let validation own the user-facing error.
        let Some(type_def) = type_defs.get(&machine_def.host.type_name) else {
            continue;
        };
        let TypeDefKind::Linear { linear } = &type_def.kind else {
            continue;
        };
        let Some(key_field) = linear
            .fields
            .iter()
            .find(|field| field.name == machine_def.host.key_field)
        else {
            continue;
        };
        let action_overrides =
            collect_machine_action_overrides(machine_def, linear, &mut action_override_fns);
        machine_hosts.insert(
            machine_def.name.clone(),
            LinearHostInfo {
                hosted_type_name: machine_def.host.type_name.clone(),
                key_field: machine_def.host.key_field.clone(),
                key_ty: key_field.ty.clone(),
                handle_type_name: machine_handle_type_name(&machine_def.name),
                action_overrides,
            },
        );
    }

    LinearIndex {
        types,
        machine_hosts,
        action_override_fns,
        hosted_action_exprs: HashMap::new(),
    }
}

fn collect_machine_action_overrides(
    machine_def: &crate::core::ast::MachineDef,
    linear: &crate::core::ast::LinearTypeDef,
    generated: &mut HashMap<String, GeneratedActionOverrideInfo>,
) -> HashMap<String, String> {
    let mut action_counts = HashMap::<&str, usize>::new();
    for action in &linear.actions {
        *action_counts.entry(action.name.as_str()).or_default() += 1;
    }

    let mut overrides = HashMap::new();
    for item in &machine_def.items {
        let crate::core::ast::MachineItem::Action(handler) = item else {
            continue;
        };
        // Leave malformed or ambiguous overrides to validation. The runtime
        // dispatch map only includes handlers we can identify unambiguously.
        if !linear
            .actions
            .iter()
            .any(|action| action.name == handler.name)
        {
            continue;
        }
        if action_counts
            .get(handler.name.as_str())
            .copied()
            .unwrap_or_default()
            != 1
        {
            continue;
        }
        let fn_name = machine_action_override_fn_name(&machine_def.name, &handler.name);
        generated.insert(
            fn_name.clone(),
            GeneratedActionOverrideInfo {
                hosted_type_name: machine_def.host.type_name.clone(),
                source_state: linear
                    .actions
                    .iter()
                    .find(|action| action.name == handler.name)
                    .map(|action| action.source_state.clone())
                    .unwrap_or_default(),
                instance_param_name: handler.instance_param.clone(),
            },
        );
        overrides.insert(handler.name.clone(), fn_name);
    }
    overrides
}
