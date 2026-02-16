//! Definition resolution lookups (go-to-definition, def-at-span).

use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefTable, UNKNOWN_DEF_ID};
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::Location;
use crate::services::analysis::snapshot::{AnalysisSnapshot, FileId};
use crate::services::analysis::syntax_index::{node_span_map, span_contains_span};

/// Resolve the definition under `query_span`. Tries regular AST node
/// definitions first, then falls back to typestate role bindings.
pub(crate) fn def_at_span(state: &LookupState, query_span: Span) -> Option<DefId> {
    let resolved = state.resolved.as_ref()?;
    if let Some((_, def_id)) = best_def_use_at_span(
        &resolved.module,
        &resolved.def_table,
        &state.poisoned_nodes,
        query_span,
    ) {
        return Some(def_id);
    }
    typestate_role_def_at_span(
        &resolved.typestate_role_impls,
        &resolved.def_table,
        query_span,
    )
}

/// Go-to-definition: resolve the definition under `query_span` and return
/// its source location (file path + span).
pub(crate) fn def_location_at_span(
    snapshot: &AnalysisSnapshot,
    file_id: FileId,
    state: &LookupState,
    query_span: Span,
) -> Option<Location> {
    let resolved = state.resolved.as_ref()?;
    let def_id = if let Some((_, def_id)) = best_def_use_at_span(
        &resolved.module,
        &resolved.def_table,
        &state.poisoned_nodes,
        query_span,
    ) {
        def_id
    } else {
        typestate_role_def_at_span(
            &resolved.typestate_role_impls,
            &resolved.def_table,
            query_span,
        )?
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

/// Find the narrowest AST node that contains `query_span` and has a known
/// definition. When multiple nodes share the same minimal width (e.g. a name
/// node and its parent decl), we break ties by preferring the latest start
/// offset — which corresponds to the most specific (innermost) node.
fn best_def_use_at_span<D, T>(
    module: &crate::core::tree::Module<D, T>,
    def_table: &crate::core::resolve::DefTable,
    poisoned_nodes: &std::collections::HashSet<crate::core::tree::NodeId>,
    query_span: Span,
) -> Option<(crate::core::tree::NodeId, DefId)> {
    let spans = node_span_map(module);
    let mut candidates = Vec::new();
    for (node_id, span) in spans {
        if !span_contains_span(span, query_span) {
            continue;
        }
        candidates.push((node_id, span));
    }

    // Narrow to smallest width, then latest start offset among those.
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

/// Resolve a typestate role binding (e.g. `impl Server for Connection`)
/// under the cursor. Uses the same narrowest-span heuristic as
/// `best_def_use_at_span`.
pub(super) fn typestate_role_def_at_span(
    role_impls: &[crate::core::context::TypestateRoleImplBinding],
    def_table: &DefTable,
    query_span: Span,
) -> Option<DefId> {
    let mut best: Option<(usize, usize, DefId)> = None;
    for role_impl in role_impls {
        let Some(def_id) = role_impl.role_def_id else {
            continue;
        };
        if def_id == UNKNOWN_DEF_ID {
            continue;
        }
        if !span_contains_span(role_impl.span, query_span) {
            continue;
        }
        if def_table.lookup_def(def_id).is_none() {
            continue;
        }

        let width = role_impl
            .span
            .end
            .offset
            .saturating_sub(role_impl.span.start.offset);
        let start = role_impl.span.start.offset;
        let replace = best.as_ref().is_none_or(|(best_width, best_start, _)| {
            width < *best_width || (width == *best_width && start > *best_start)
        });
        if replace {
            best = Some((width, start, def_id));
        }
    }
    best.map(|(_, _, def_id)| def_id)
}
