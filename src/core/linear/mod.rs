//! Linear type support: validation, direct-mode lowering, and hosted machine surface.
//!
//! Linear types (`@linear type`) combine struct fields, enum-like states, transition
//! rules (actions/triggers), and role-based access control in a single declaration.
//! This module runs before resolve and has three responsibilities:
//!
//! 1. **Validation** — reject malformed declarations while the source shape is intact.
//! 2. **Direct-mode lowering** — rewrite linear types into ordinary enums + method
//!    blocks so the rest of the compiler reuses existing type/method machinery.
//! 3. **Machine hosting surface** — generate handle types, spawn/create functions,
//!    and rewrite calls so hosted linear types typecheck through normal paths.
//!
//! The [`LinearIndex`] captures metadata about linear types and machine hosting
//! relationships. It's built after parsing and threaded through to the type checker,
//! where it informs session-entry typing (`create`/`resume`) and hosted action
//! fallibility.

mod index;
mod machine;
mod rewrite;
mod validate;

use std::collections::HashMap;

use crate::core::ast::{Module, NodeIdGen};
use crate::core::resolve::ResolveError;

pub use index::*;
pub(crate) use machine::machine_create_fn_name;
pub(crate) use machine::machine_resume_fn_name;
pub(crate) use machine::machine_spawn_fn_name;
pub(crate) use machine::machine_trigger_handler_fn_name;
pub(crate) use rewrite::direct_action_method_name;

// ── Public entry points ─────────────────────────────────────────────

/// Validate all `@linear type` and `machine` declarations in the module.
/// Returns errors for malformed declarations (missing states, unknown actions
/// in roles, mismatched method signatures, invalid machine host clauses, etc.).
pub fn validate_module(module: &Module) -> Vec<ResolveError> {
    validate::validate_module(module)
}

/// Build the [`LinearIndex`] from parsed linear types and machine definitions.
/// Called once after parsing; the index is then threaded into resolve and typecheck.
pub fn build_linear_index(module: &Module) -> LinearIndex {
    index::build_linear_index(module)
}

/// Desugar linear types and machine hosting into forms the rest of the compiler
/// understands. This is the main rewriting entry point, performing:
///
/// - Linear type defs → enum type defs
/// - Linear method blocks → renamed sink methods on the enum
/// - Linear expressions → enum variant construction + state-tracked method dispatch
/// - Machine defs → handle struct types + spawn/create helper functions
/// - Machine spawn/create calls → calls to the generated helpers
///
/// The `linear_index` is mutated to record hosted action expression IDs, which
/// the type checker later uses to emit fallible-result obligations.
pub fn desugar_module(
    module: &mut Module,
    node_id_gen: &mut NodeIdGen,
    linear_index: &mut LinearIndex,
) -> Vec<ResolveError> {
    let infos = rewrite::collect_direct_linear_infos(module);
    let machine_infos = machine::collect_machine_spawn_infos(module);
    let action_override_infos = machine::collect_machine_action_override_infos(module);
    let trigger_handler_infos = machine::collect_machine_trigger_handler_infos(module);
    let on_handler_infos = machine::collect_machine_on_handler_infos(module);
    let deliver_infos = machine::collect_machine_deliver_infos(module);
    let wait_infos = machine::collect_machine_wait_infos(module);

    // Generate hosted support types and machine surface before direct-mode lowering,
    // since the generated types (MachineError, SessionError, handle structs) need to
    // exist before resolve sees them.
    if !machine_infos.is_empty() {
        machine::ensure_hosted_support_types(module, node_id_gen);
        machine::ensure_hosted_runtime_intrinsics(module, node_id_gen);
        machine::append_machine_spawn_support(
            module,
            &machine_infos,
            &action_override_infos,
            &trigger_handler_infos,
            &on_handler_infos,
            &deliver_infos,
            &wait_infos,
            node_id_gen,
        );
        machine::rewrite_machine_constructor_self_types(module, &machine_infos);
    }

    if infos.is_empty() {
        if !machine_infos.is_empty() {
            machine::rewrite_machine_spawn_calls(module, &machine_infos, node_id_gen);
        }
        return Vec::new();
    }

    // Direct-mode lowering: linear types → enums, methods → renamed sinks,
    // expressions → state-tracked rewrites with use-after-consume checking.
    rewrite::rewrite_linear_type_defs(module, &infos);
    rewrite::rewrite_linear_method_blocks(module, &infos, linear_index, node_id_gen);
    let errors = rewrite::rewrite_linear_exprs(module, &infos, linear_index, node_id_gen);

    if !machine_infos.is_empty() {
        machine::rewrite_machine_spawn_calls(module, &machine_infos, node_id_gen);
    }
    errors
}

// ── Shared helpers ──────────────────────────────────────────────────

/// Collect all type definitions by name for quick lookup.
pub(super) fn type_defs_by_name(module: &Module) -> HashMap<String, &crate::core::ast::TypeDef> {
    let mut type_defs = HashMap::new();
    for type_def in module.type_defs() {
        type_defs.insert(type_def.name.clone(), type_def);
    }
    type_defs
}
