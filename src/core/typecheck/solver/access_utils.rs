//! Shared access/visibility helpers for nominal/property checks.

use std::collections::HashMap;

use crate::core::capsule::ModuleId;
use crate::core::resolve::{DefId, DefTable};

pub(super) fn type_def_id_for_nominal_name(
    name: &str,
    type_symbols: &HashMap<String, DefId>,
) -> Option<DefId> {
    let base = super::diag_utils::compact_nominal_name(name);
    type_symbols.get(&base).copied()
}

pub(super) fn is_external_opaque_access(
    caller_def_id: Option<DefId>,
    owner_type_def_id: DefId,
    def_table: &DefTable,
    def_owners: &HashMap<DefId, ModuleId>,
) -> bool {
    if !def_table
        .lookup_def(owner_type_def_id)
        .is_some_and(|def| def.is_opaque())
    {
        return false;
    }
    let Some(caller_def_id) = caller_def_id else {
        return false;
    };
    let Some(caller_module_id) = def_owners.get(&caller_def_id) else {
        return false;
    };
    let Some(owner_module_id) = def_owners.get(&owner_type_def_id) else {
        return false;
    };
    caller_module_id != owner_module_id
}

pub(super) fn is_def_accessible_from(
    caller_def_id: Option<DefId>,
    target_def_id: DefId,
    def_table: &DefTable,
    def_owners: &HashMap<DefId, ModuleId>,
) -> bool {
    if def_table
        .lookup_def(target_def_id)
        .is_some_and(|def| def.is_public())
    {
        return true;
    }

    let Some(caller_def_id) = caller_def_id else {
        return true;
    };
    let Some(caller_module_id) = def_owners.get(&caller_def_id) else {
        return true;
    };
    let Some(target_module_id) = def_owners.get(&target_def_id) else {
        return true;
    };
    caller_module_id == target_module_id
}
