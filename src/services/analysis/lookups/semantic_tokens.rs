//! Semantic token lookup.

use crate::core::resolve::DefKind;
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::{SemanticToken, SemanticTokenKind};
use crate::services::analysis::syntax_index::node_span_map;

pub(crate) fn semantic_tokens(state: &LookupState) -> Vec<SemanticToken> {
    let Some(resolved) = state.resolved.as_ref() else {
        return Vec::new();
    };

    let node_spans = node_span_map(&resolved.module);
    let mut out = Vec::new();
    for (node_id, def_id) in resolved.def_table.node_def_entries() {
        let Some(def) = resolved.def_table.lookup_def(def_id) else {
            continue;
        };
        let Some(kind) = semantic_token_kind_for_def(&def.kind) else {
            continue;
        };
        let Some(span) = node_spans.get(&node_id).copied() else {
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

fn semantic_token_kind_for_def(kind: &DefKind) -> Option<SemanticTokenKind> {
    match kind {
        DefKind::ProtocolDef => Some(SemanticTokenKind::Type),
        DefKind::ProtocolRole => Some(SemanticTokenKind::EnumVariant),
        DefKind::TypeDef { .. } => Some(SemanticTokenKind::Type),
        DefKind::TraitDef { .. } => Some(SemanticTokenKind::Trait),
        DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => Some(SemanticTokenKind::Function),
        DefKind::TypeParam => Some(SemanticTokenKind::TypeParameter),
        DefKind::EnumVariantName => Some(SemanticTokenKind::EnumVariant),
        DefKind::LocalVar { .. } => Some(SemanticTokenKind::Variable),
        DefKind::Param { .. } => Some(SemanticTokenKind::Parameter),
    }
}
