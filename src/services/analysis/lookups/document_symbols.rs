//! Document symbol lookup.

use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::DocumentSymbol;
use crate::services::analysis::syntax_index::{document_symbol_nodes, node_span_map};

pub(crate) fn document_symbols(state: &LookupState) -> Vec<DocumentSymbol> {
    let Some(resolved) = state.resolved.as_ref() else {
        return Vec::new();
    };

    let node_spans = node_span_map(&resolved.module);
    let nodes = document_symbol_nodes(&resolved.module);

    let mut out = Vec::new();
    for (node_id, kind) in nodes {
        let Some(def_id) = resolved.def_table.lookup_node_def_id(node_id) else {
            continue;
        };
        let Some(def) = resolved.def_table.lookup_def(def_id) else {
            continue;
        };
        let Some(span) = node_spans.get(&node_id).copied() else {
            continue;
        };
        out.push(DocumentSymbol {
            name: def.name.clone(),
            kind: kind.clone(),
            def_id,
            span,
            detail: Some(def.kind.to_string()),
        });
    }

    out.sort_by_key(|sym| {
        (
            sym.span.start.line,
            sym.span.start.column,
            sym.span.end.line,
            sym.span.end.column,
            sym.name.clone(),
            sym.def_id,
        )
    });
    out.dedup_by(|a, b| a.def_id == b.def_id && a.kind == b.kind && a.span == b.span);
    out
}
