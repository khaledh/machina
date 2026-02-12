//! Symbol lookup helpers used by `AnalysisDb` query entry points.
//!
//! This module keeps per-feature lookup logic out of `analysis::db` so the DB
//! focuses on snapshot/query orchestration.

use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefKind, UNKNOWN_DEF_ID};
use crate::core::types::Type;
use crate::services::analysis::code_actions::code_actions_for_diagnostic_with_source;
use crate::services::analysis::diagnostics::Diagnostic;
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::{
    CodeAction, DocumentSymbol, HoverInfo, Location, SemanticToken, SemanticTokenKind,
    SignatureHelp,
};
use crate::services::analysis::snapshot::{AnalysisSnapshot, FileId};
use crate::services::analysis::syntax_index::{
    active_param_index, call_site_at_span, document_symbol_nodes, node_at_span, node_span_map,
    span_intersects_span,
};

pub(crate) fn def_at_span(state: &LookupState, query_span: Span) -> Option<DefId> {
    let resolved = state.resolved.as_ref()?;
    let node_id = node_at_span(&resolved.module, query_span)?;
    if state.poisoned_nodes.contains(&node_id) {
        return None;
    }
    resolved
        .def_table
        .lookup_node_def_id(node_id)
        .filter(|id| *id != UNKNOWN_DEF_ID)
}

pub(crate) fn def_location_at_span(
    snapshot: &AnalysisSnapshot,
    file_id: FileId,
    state: &LookupState,
    query_span: Span,
) -> Option<Location> {
    let resolved = state.resolved.as_ref()?;
    let use_node_id = node_at_span(&resolved.module, query_span)?;
    if state.poisoned_nodes.contains(&use_node_id) {
        return None;
    }
    let def_id = resolved.def_table.lookup_node_def_id(use_node_id)?;
    if def_id == UNKNOWN_DEF_ID {
        return None;
    }
    let def_span = resolved.def_table.lookup_def_span(def_id).or_else(|| {
        // Fallback for older/synthetic defs without direct span metadata.
        let def_node_id = resolved.def_table.lookup_def_node_id(def_id)?;
        let node_spans = node_span_map(&resolved.module);
        node_spans.get(&def_node_id).copied()
    })?;

    Some(Location {
        file_id,
        path: snapshot.path(file_id).map(std::path::Path::to_path_buf),
        span: def_span,
    })
}

pub(crate) fn type_at_span(state: &LookupState, query_span: Span) -> Option<Type> {
    let typed = state.typed.as_ref()?;
    let node_id = node_at_span(&typed.module, query_span)?;
    if state.poisoned_nodes.contains(&node_id) {
        return None;
    }
    typed
        .type_map
        .lookup_node_type(node_id)
        .filter(|ty| !matches!(ty, Type::Unknown))
}

pub(crate) fn hover_at_span(state: &LookupState, query_span: Span) -> Option<HoverInfo> {
    if let Some(typed) = state.typed.as_ref() {
        let node_id = node_at_span(&typed.module, query_span)?;
        if state.poisoned_nodes.contains(&node_id) {
            return None;
        }
        let def_id = typed
            .def_table
            .lookup_node_def_id(node_id)
            .filter(|id| *id != UNKNOWN_DEF_ID);
        let def_name = def_id.and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()));
        let ty = typed
            .type_map
            .lookup_node_type(node_id)
            .filter(|ty| !matches!(ty, Type::Unknown));
        if def_id.is_none() && ty.is_none() {
            return None;
        }
        let display = format_hover_label(def_name.as_deref(), ty.as_ref());
        return Some(HoverInfo {
            node_id,
            span: query_span,
            def_id,
            def_name,
            ty,
            display,
        });
    }

    let resolved = state.resolved.as_ref()?;
    let node_id = node_at_span(&resolved.module, query_span)?;
    if state.poisoned_nodes.contains(&node_id) {
        return None;
    }
    let def_id = resolved
        .def_table
        .lookup_node_def_id(node_id)
        .filter(|id| *id != UNKNOWN_DEF_ID)?;
    let def_name = resolved
        .def_table
        .lookup_def(def_id)
        .map(|def| def.name.clone());
    let display = format_hover_label(def_name.as_deref(), None);
    Some(HoverInfo {
        node_id,
        span: query_span,
        def_id: Some(def_id),
        def_name,
        ty: None,
        display,
    })
}

pub(crate) fn signature_help_at_span(
    state: &LookupState,
    query_span: Span,
) -> Option<SignatureHelp> {
    let typed = state.typed.as_ref()?;
    let call = call_site_at_span(&typed.module, query_span)?;
    let sig = typed.call_sigs.get(&call.node_id)?;

    let mut params = Vec::with_capacity(sig.params.len());
    for param in &sig.params {
        params.push(format!("{} {}", param_mode_name(&param.mode), param.ty));
    }
    let active_parameter = active_param_index(&call.arg_spans, query_span.start);
    let name = sig
        .def_id
        .and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()))
        .unwrap_or_else(|| "<call>".to_string());
    let label = format!("{name}({})", params.join(", "));

    Some(SignatureHelp {
        label,
        def_id: sig.def_id,
        active_parameter,
        parameters: params,
    })
}

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

pub(crate) fn code_actions_for_range(
    diagnostics: &[Diagnostic],
    range: Span,
    source: Option<&str>,
) -> Vec<CodeAction> {
    let mut actions = Vec::new();
    for diag in diagnostics {
        if !span_intersects_span(diag.span, range) {
            continue;
        }
        actions.extend(code_actions_for_diagnostic_with_source(diag, source));
    }
    actions.sort_by_key(|action| {
        (
            action.title.clone(),
            action.diagnostic_code.clone(),
            action
                .edits
                .first()
                .map(|e| (e.span.start.line, e.span.start.column))
                .unwrap_or((0, 0)),
        )
    });
    actions.dedup_by(|a, b| {
        a.title == b.title && a.diagnostic_code == b.diagnostic_code && a.edits == b.edits
    });
    actions
}

fn format_hover_label(def_name: Option<&str>, ty: Option<&Type>) -> String {
    match (def_name, ty) {
        (Some(name), Some(ty)) => format!("{name}: {ty}"),
        (Some(name), None) => name.to_string(),
        (None, Some(ty)) => ty.to_string(),
        (None, None) => String::new(),
    }
}

fn semantic_token_kind_for_def(kind: &DefKind) -> Option<SemanticTokenKind> {
    match kind {
        DefKind::TypeDef { .. } => Some(SemanticTokenKind::Type),
        DefKind::TraitDef { .. } => Some(SemanticTokenKind::Trait),
        DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => Some(SemanticTokenKind::Function),
        DefKind::TypeParam => Some(SemanticTokenKind::TypeParameter),
        DefKind::EnumVariantName => Some(SemanticTokenKind::EnumVariant),
        DefKind::LocalVar { .. } => Some(SemanticTokenKind::Variable),
        DefKind::Param { .. } => Some(SemanticTokenKind::Parameter),
    }
}

fn param_mode_name(mode: &crate::core::tree::ParamMode) -> &'static str {
    match mode {
        crate::core::tree::ParamMode::In => "in",
        crate::core::tree::ParamMode::InOut => "inout",
        crate::core::tree::ParamMode::Out => "out",
        crate::core::tree::ParamMode::Sink => "sink",
    }
}
