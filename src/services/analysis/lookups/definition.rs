//! Definition resolution lookups (go-to-definition, def-at-span).

use crate::core::ast::{Module, NodeId};
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefTable, UNKNOWN_DEF_ID};
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::Location;
use crate::services::analysis::snapshot::{AnalysisSnapshot, FileId};
use crate::services::analysis::syntax_index::{
    call_site_at_span, node_span_map, span_contains_span,
};

use super::identifier_token_at_span;
use std::collections::HashSet;

/// Resolve the definition under `query_span`. Prefers role-binding matches
/// over AST node matches — generated typestate items may carry synthetic
/// spans that shadow the original role reference.
pub(crate) fn def_at_span(
    state: &LookupState,
    query_span: Span,
    source: Option<&str>,
) -> Option<DefId> {
    let token = identifier_token_at_span(source, query_span);
    let query_span = token.as_ref().map(|token| token.span).unwrap_or(query_span);
    let resolved = state.resolved.as_ref()?;
    if let Some(def_id) =
        selected_callable_def_at_span(state, query_span, token.as_ref().map(|t| t.ident.as_str()))
    {
        return Some(def_id);
    }
    if let Some(def_id) = typestate_role_def_at_span(
        &resolved.typestate_role_impls,
        &resolved.def_table,
        query_span,
    ) {
        return Some(def_id);
    }
    best_def_use_at_span(
        &resolved.module,
        &resolved.def_table,
        &state.poisoned_nodes,
        query_span,
    )
    .map(|(_, def_id)| def_id)
}

/// Go-to-definition: resolve the definition under `query_span` and return
/// its source location (file path + span).
pub(crate) fn def_location_at_span(
    snapshot: &AnalysisSnapshot,
    file_id: FileId,
    state: &LookupState,
    query_span: Span,
    source: Option<&str>,
) -> Option<Location> {
    let token = identifier_token_at_span(source, query_span);
    let query_span = token.as_ref().map(|token| token.span).unwrap_or(query_span);
    let resolved = state.resolved.as_ref()?;
    // Try role-binding lookup first — generated typestate items may carry
    // synthetic spans that overlap with the role reference, causing
    // `best_def_use_at_span` to match a generated node instead of the
    // original protocol role.
    let def_id = if let Some(def_id) =
        selected_callable_def_at_span(state, query_span, token.as_ref().map(|t| t.ident.as_str()))
    {
        def_id
    } else if let Some(def_id) = typestate_role_def_at_span(
        &resolved.typestate_role_impls,
        &resolved.def_table,
        query_span,
    ) {
        def_id
    } else if let Some((_, def_id)) = best_def_use_at_span(
        &resolved.module,
        &resolved.def_table,
        &state.poisoned_nodes,
        query_span,
    ) {
        def_id
    } else {
        return None;
    };
    let def_loc = resolved.def_table.lookup_def_location(def_id)?;

    Some(Location {
        file_id,
        path: def_loc
            .path
            .or_else(|| snapshot.path(file_id).map(std::path::Path::to_path_buf)),
        span: def_loc.span,
    })
}

fn selected_callable_def_at_span(
    state: &LookupState,
    query_span: Span,
    query_ident: Option<&str>,
) -> Option<DefId> {
    let typed = state.typed.as_ref()?;
    let query_ident = query_ident?;
    let call = call_site_at_span(&typed.module, query_span)?;
    if let Some(method_name) = call.method_name.as_deref() {
        if method_name != query_ident {
            return None;
        }
    } else {
        let callee_span = node_span_map(&typed.module)
            .get(&call.callee_node_id)
            .copied()?;
        if !span_contains_span(callee_span, query_span) {
            return None;
        }
    }
    typed.call_sigs.get(&call.node_id)?.def_id
}

/// Find the narrowest AST node that contains `query_span` and has a known
/// definition. When multiple nodes share the same minimal width (e.g. a name
/// node and its parent decl), we break ties by preferring the latest start
/// offset — which corresponds to the most specific (innermost) node.
fn best_def_use_at_span(
    module: &Module,
    def_table: &DefTable,
    poisoned_nodes: &HashSet<NodeId>,
    query_span: Span,
) -> Option<(NodeId, DefId)> {
    let spans = node_span_map(module);
    let mut candidates = Vec::new();
    for (node_id, span) in spans {
        if !span_contains_span(span, query_span) {
            continue;
        }
        candidates.push((node_id, span));
    }

    // Narrow to the smallest width, then latest start offset among those.
    let min_width = candidates
        .iter()
        .map(|(_, span)| span.end.offset.saturating_sub(span.start.offset))
        .min()?;
    let max_start = candidates
        .iter()
        .filter(|(_, span)| span.end.offset.saturating_sub(span.start.offset) == min_width)
        .map(|(_, span)| span.start.offset)
        .max()?;

    for (node_id, span) in candidates {
        let width = span.end.offset.saturating_sub(span.start.offset);
        if width != min_width || span.start.offset != max_start {
            continue;
        }
        if poisoned_nodes.contains(&node_id) {
            continue;
        }
        let Some(def_id) = def_table.lookup_node_def_id(node_id) else {
            continue;
        };
        if def_id != UNKNOWN_DEF_ID {
            return Some((node_id, def_id));
        }
    }
    None
}

/// Resolve a typestate protocol-role reference under the cursor.
///
/// This covers:
/// - typestate role impl paths (`typestate X : Proto::Role { ... }`)
/// - explicit peer field bindings (`peer: Machine<Y> as Role`)
///
/// Uses the same narrowest-span heuristic as `best_def_use_at_span`.
pub(super) fn typestate_role_def_at_span(
    role_impls: &[crate::core::context::TypestateRoleImplBinding],
    def_table: &DefTable,
    query_span: Span,
) -> Option<DefId> {
    let mut best: Option<(usize, usize, DefId)> = None;

    let mut consider = |span: Span, def_id: DefId| {
        if def_id == UNKNOWN_DEF_ID {
            return;
        }
        if !span_contains_span(span, query_span) {
            return;
        }
        if def_table.lookup_def(def_id).is_none() {
            return;
        }

        let width = span.end.offset.saturating_sub(span.start.offset);
        let start = span.start.offset;
        let replace = best.as_ref().is_none_or(|(best_width, best_start, _)| {
            width < *best_width || (width == *best_width && start > *best_start)
        });
        if replace {
            best = Some((width, start, def_id));
        }
    };

    for role_impl in role_impls {
        if let Some(def_id) = role_impl.role_def_id {
            consider(role_impl.span, def_id);
        }
        for peer_binding in &role_impl.peer_role_bindings {
            if let Some(def_id) = peer_binding.role_def_id {
                consider(peer_binding.span, def_id);
            }
        }
    }
    best.map(|(_, _, def_id)| def_id)
}
