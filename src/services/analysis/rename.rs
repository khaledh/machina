//! Reference search and rename planning helpers for analysis queries.

use std::collections::HashSet;
use std::path::Path;

use crate::analysis::pipeline::LookupState;
use crate::analysis::query::QueryResult;
use crate::analysis::results::{Location, RenameConflict, RenameEdit, RenamePlan};
use crate::analysis::snapshot::{AnalysisSnapshot, FileId};
use crate::analysis::syntax_index::node_span_map;
use crate::frontend::ModuleId;
use crate::resolve::DefId;

pub(crate) fn references<F>(
    snapshot: &AnalysisSnapshot,
    def_id: DefId,
    mut lookup_state_for_file: F,
) -> QueryResult<Vec<Location>>
where
    F: FnMut(FileId) -> QueryResult<LookupState>,
{
    let mut out = Vec::new();

    for file_id in snapshot.file_ids() {
        let state = lookup_state_for_file(file_id)?;
        let Some(resolved) = state.resolved else {
            continue;
        };

        let node_spans = node_span_map(&resolved.module);
        for (node_id, mapped_def_id) in resolved.def_table.node_def_entries() {
            if mapped_def_id != def_id {
                continue;
            }
            let Some(span) = node_spans.get(&node_id).copied() else {
                continue;
            };
            out.push(Location {
                file_id,
                path: snapshot.path(file_id).map(Path::to_path_buf),
                span,
            });
        }
    }

    out.sort_by_key(|loc| {
        (
            loc.path.clone(),
            loc.span.start.line,
            loc.span.start.column,
            loc.span.end.line,
            loc.span.end.column,
            loc.file_id,
        )
    });
    out.dedup();
    Ok(out)
}

pub(crate) fn rename_plan<F>(
    snapshot: &AnalysisSnapshot,
    def_id: DefId,
    new_name: &str,
    mut lookup_state_for_file: F,
) -> QueryResult<RenamePlan>
where
    F: FnMut(FileId) -> QueryResult<LookupState>,
{
    let references = references(snapshot, def_id, &mut lookup_state_for_file)?;
    let mut plan = RenamePlan {
        def_id,
        old_name: None,
        new_name: new_name.to_string(),
        edits: Vec::new(),
        conflicts: Vec::new(),
    };

    if !is_identifier_name(new_name) {
        plan.conflicts.push(RenameConflict {
            message: format!("`{new_name}` is not a valid identifier"),
            existing_def: None,
        });
        return Ok(plan);
    }

    let mut owner_module = None;
    let mut existing_same_name = HashSet::new();
    for file_id in snapshot.file_ids() {
        let state = lookup_state_for_file(file_id)?;
        let Some(resolved) = state.resolved else {
            continue;
        };

        if plan.old_name.is_none()
            && let Some(def) = resolved.def_table.lookup_def(def_id)
        {
            plan.old_name = Some(def.name.clone());
            owner_module = resolved
                .def_owners
                .get(&def_id)
                .copied()
                .or(Some(ModuleId(file_id.0)));
        }

        let Some(target_owner) = owner_module else {
            continue;
        };
        for def in resolved.def_table.defs() {
            if def.id == def_id || def.name != new_name {
                continue;
            }
            let owner = resolved
                .def_owners
                .get(&def.id)
                .copied()
                .or(Some(ModuleId(file_id.0)));
            if owner == Some(target_owner) && existing_same_name.insert(def.id) {
                plan.conflicts.push(RenameConflict {
                    message: format!("name `{new_name}` already exists as {}", def.kind),
                    existing_def: Some(def.id),
                });
            }
        }
    }

    if let Some(old_name) = &plan.old_name
        && old_name == new_name
    {
        plan.conflicts.push(RenameConflict {
            message: "new name matches existing symbol name".to_string(),
            existing_def: Some(def_id),
        });
    }

    for location in references {
        plan.edits.push(RenameEdit {
            location,
            replacement: new_name.to_string(),
        });
    }

    Ok(plan)
}

fn is_identifier_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}
