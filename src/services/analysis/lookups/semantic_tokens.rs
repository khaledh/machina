//! Semantic token lookup.

use crate::core::diag::Span;
use crate::core::resolve::DefKind;
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::{SemanticToken, SemanticTokenKind};
use crate::services::analysis::syntax_index::node_span_map;

pub(crate) fn semantic_tokens(state: &LookupState) -> Vec<SemanticToken> {
    let Some(resolved) = state.resolved.as_ref() else {
        return Vec::new();
    };

    let node_spans = node_span_map(&resolved.module);
    let source = state.source.as_deref();
    let mut out = Vec::new();
    for (node_id, def_id) in resolved.def_table.node_def_entries() {
        let Some(def) = resolved.def_table.lookup_def(def_id) else {
            continue;
        };
        let Some(kind) = semantic_token_kind_for_def(&def.kind) else {
            continue;
        };
        let Some(span) = semantic_token_span(
            state,
            node_id,
            def_id,
            def.name.as_str(),
            source,
            &node_spans,
        ) else {
            continue;
        };
        out.push(SemanticToken { span, kind, def_id });
    }
    out.sort_by_key(|tok| {
        (
            tok.span.start.line,
            tok.span.start.column,
            tok.span.end.line,
            tok.span.end.column,
            tok.def_id,
        )
    });
    out.dedup_by(|a, b| a.span == b.span && a.kind == b.kind && a.def_id == b.def_id);
    out
}

fn semantic_token_span(
    state: &LookupState,
    node_id: crate::core::ast::NodeId,
    def_id: crate::core::resolve::DefId,
    ident: &str,
    source: Option<&str>,
    node_spans: &std::collections::HashMap<crate::core::ast::NodeId, Span>,
) -> Option<Span> {
    let resolved = state.resolved.as_ref()?;
    let node_span = node_spans.get(&node_id).copied()?;
    if resolved.def_table.lookup_def_node_id(def_id) == Some(node_id) {
        let def_span = resolved
            .def_table
            .lookup_def_span(def_id)
            .unwrap_or(node_span);
        return token_span_within(def_span, ident, source).or(Some(def_span));
    }
    Some(node_span)
}

fn token_span_within(container: Span, ident: &str, source: Option<&str>) -> Option<Span> {
    let source = source?;
    let start = container.start.offset.min(source.len());
    let end = container.end.offset.min(source.len());
    let snippet = source.get(start..end)?;
    let rel = snippet.find(ident)?;
    let abs_start = start + rel;
    let abs_end = abs_start + ident.len();
    Some(Span {
        start: super::position_at_offset(source, abs_start),
        end: super::position_at_offset(source, abs_end),
    })
}

fn semantic_token_kind_for_def(kind: &DefKind) -> Option<SemanticTokenKind> {
    match kind {
        DefKind::MachineDef => Some(SemanticTokenKind::Type),
        DefKind::TypeDef { .. } => Some(SemanticTokenKind::Type),
        DefKind::TraitDef { .. } => Some(SemanticTokenKind::Trait),
        DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => Some(SemanticTokenKind::Function),
        DefKind::TypeParam => Some(SemanticTokenKind::TypeParameter),
        DefKind::EnumVariantName => Some(SemanticTokenKind::EnumVariant),
        DefKind::LocalVar { .. } => Some(SemanticTokenKind::Variable),
        DefKind::Param { .. } => Some(SemanticTokenKind::Parameter),
    }
}
