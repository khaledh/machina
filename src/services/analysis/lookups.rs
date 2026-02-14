//! Symbol lookup helpers used by `AnalysisDb` query entry points.
//!
//! This module keeps per-feature lookup logic out of `analysis::db` so the DB
//! focuses on snapshot/query orchestration.

use std::path::Path;

use crate::core::diag::{Position, Span};
use crate::core::resolve::{DefId, DefKind, DefTable, UNKNOWN_DEF_ID};
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
    span_contains_span, span_intersects_span,
};

pub(crate) fn def_at_span(state: &LookupState, query_span: Span) -> Option<DefId> {
    let resolved = state.resolved.as_ref()?;
    best_def_use_at_span(
        &resolved.module,
        &resolved.def_table,
        &state.poisoned_nodes,
        query_span,
    )
    .map(|(_, def_id)| def_id)
}

pub(crate) fn def_location_at_span(
    snapshot: &AnalysisSnapshot,
    file_id: FileId,
    state: &LookupState,
    query_span: Span,
) -> Option<Location> {
    let resolved = state.resolved.as_ref()?;
    let (_, def_id) = best_def_use_at_span(
        &resolved.module,
        &resolved.def_table,
        &state.poisoned_nodes,
        query_span,
    )?;
    let def_loc = resolved.def_table.lookup_def_location(def_id)?;

    Some(Location {
        file_id,
        path: def_loc
            .path
            .or_else(|| snapshot.path(file_id).map(std::path::Path::to_path_buf)),
        span: def_loc.span,
    })
}

fn best_def_use_at_span<D, T>(
    module: &crate::core::tree::Module<D, T>,
    def_table: &crate::core::resolve::DefTable,
    poisoned_nodes: &std::collections::HashSet<crate::core::tree::NodeId>,
    query_span: Span,
) -> Option<(crate::core::tree::NodeId, DefId)> {
    let spans = node_span_map(module);
    let mut candidates = Vec::new();
    for (node_id, span) in spans {
        if !crate::services::analysis::syntax_index::span_contains_span(span, query_span) {
            continue;
        }
        candidates.push((node_id, span));
    }

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

pub(crate) fn hover_at_span_in_file(
    state: &LookupState,
    query_span: Span,
    current_file_path: Option<&Path>,
    source_text: Option<&str>,
) -> Option<HoverInfo> {
    let query_offset =
        source_text.and_then(|source| offset_from_position(source, query_span.start));
    let query_ident = source_text
        .and_then(|source| query_offset.and_then(|offset| identifier_at_offset(source, offset)));
    if source_text.is_some() && query_ident.is_none() {
        return None;
    }
    if let Some(typed) = state.typed.as_ref() {
        if let Some(query_ident) = query_ident.as_deref()
            && let Some(call) = call_site_at_span(&typed.module, query_span)
            && let Some(sig) = typed.call_sigs.get(&call.node_id)
            && let Some(def_id) = sig.def_id
            && let Some(def) = typed.def_table.lookup_def(def_id)
            && (def.name == query_ident
                || def
                    .name
                    .strip_prefix("__ts_ctor_")
                    .is_some_and(|owner| owner == query_ident))
        {
            let ty = typed.type_map.lookup_def_type(def);
            let display = format_hover_label(Some(&def.name), ty.as_ref(), &typed.def_table);
            return Some(HoverInfo {
                node_id: call.callee_node_id,
                span: query_span,
                def_id: Some(def_id),
                def_name: Some(def.name.clone()),
                ty,
                display,
            });
        }

        let demangler = TypestateNameDemangler::from_def_table(&typed.def_table);
        let mut candidates = Vec::new();
        for (node_id, span) in node_span_map(&typed.module) {
            if span_contains_span(span, query_span) {
                candidates.push((node_id, span));
            }
        }
        candidates.sort_by_key(|(_, span)| {
            (
                span.end.offset.saturating_sub(span.start.offset),
                std::cmp::Reverse(span.start.offset),
            )
        });

        let mut best: Option<(u8, usize, usize, HoverInfo)> = None;
        for (node_id, _span) in candidates {
            if state.poisoned_nodes.contains(&node_id) {
                continue;
            }
            let def_id = typed
                .def_table
                .lookup_node_def_id(node_id)
                .filter(|id| *id != UNKNOWN_DEF_ID);
            if let Some(def_id) = def_id
                && should_skip_runtime_hover_def(&typed.def_table, def_id, current_file_path)
            {
                continue;
            }
            let def_name =
                def_id.and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()));
            if let (Some(def_name), Some(query_ident)) =
                (def_name.as_deref(), query_ident.as_deref())
                && !def_name_matches_query(def_name, query_ident, &demangler)
            {
                continue;
            }
            let ty = typed
                .type_map
                .lookup_node_type(node_id)
                .filter(|ty| !matches!(ty, Type::Unknown));
            if def_id.is_none() && ty.is_none() {
                continue;
            }

            if let (Some(query_ident), Some(ty)) = (query_ident.as_deref(), ty.as_ref())
                && let Some(field_ty) = field_type_in_struct(ty, query_ident)
            {
                let field_ty_display = demangler.demangle_text(&field_ty.to_string());
                let info = HoverInfo {
                    node_id,
                    span: query_span,
                    def_id: None,
                    def_name: Some(query_ident.to_string()),
                    ty: Some(field_ty.clone()),
                    display: format!("{query_ident}: {field_ty_display}"),
                };
                let width = _span.end.offset.saturating_sub(_span.start.offset);
                let start = _span.start.offset;
                let score = 6u8;
                let replace =
                    best.as_ref()
                        .is_none_or(|(best_score, best_width, best_start, _)| {
                            score > *best_score
                                || (score == *best_score
                                    && (width < *best_width
                                        || (width == *best_width && start > *best_start)))
                        });
                if replace {
                    best = Some((score, width, start, info));
                }
                continue;
            }

            let display = format_hover_label(def_name.as_deref(), ty.as_ref(), &typed.def_table);
            let score = hover_candidate_score(
                def_name.as_deref(),
                ty.is_some(),
                query_ident.as_deref(),
                &demangler,
            );
            let info = HoverInfo {
                node_id,
                span: query_span,
                def_id,
                def_name,
                ty,
                display,
            };
            let width = _span.end.offset.saturating_sub(_span.start.offset);
            let start = _span.start.offset;
            let replace = best
                .as_ref()
                .is_none_or(|(best_score, best_width, best_start, _)| {
                    score > *best_score
                        || (score == *best_score
                            && (width < *best_width
                                || (width == *best_width && start > *best_start)))
                });
            if replace {
                best = Some((score, width, start, info));
            }
        }
        if let Some((_, _, _, info)) = best {
            return Some(info);
        }
        if let Some(fallback) = fallback_hover_from_def_table(
            &typed.def_table,
            Some(&typed.type_map),
            query_span,
            current_file_path,
            query_ident.as_deref(),
        ) {
            return Some(fallback);
        }
        if let Some(source_text) = source_text
            && let Some(query_offset) = query_offset
            && let Some(query_ident) = query_ident.as_deref()
            && let Some(display) =
                syntactic_field_hover_display(source_text, query_offset, query_ident)
        {
            return Some(HoverInfo {
                node_id: crate::core::tree::NodeId(0),
                span: query_span,
                def_id: None,
                def_name: Some(query_ident.to_string()),
                ty: None,
                display,
            });
        }
        return None;
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
    if should_skip_runtime_hover_def(&resolved.def_table, def_id, current_file_path) {
        return None;
    }
    let demangler = TypestateNameDemangler::from_def_table(&resolved.def_table);
    let def_name = resolved
        .def_table
        .lookup_def(def_id)
        .map(|def| def.name.clone());
    if let (Some(def_name), Some(query_ident)) = (def_name.as_deref(), query_ident.as_deref())
        && !def_name_matches_query(def_name, query_ident, &demangler)
    {
        return None;
    }
    let display = format_hover_label(def_name.as_deref(), None, &resolved.def_table);
    Some(HoverInfo {
        node_id,
        span: query_span,
        def_id: Some(def_id),
        def_name,
        ty: None,
        display,
    })
    .or_else(|| {
        fallback_hover_from_def_table(
            &resolved.def_table,
            None,
            query_span,
            current_file_path,
            query_ident.as_deref(),
        )
    })
}

fn should_skip_runtime_hover_def(
    def_table: &DefTable,
    def_id: DefId,
    current_file_path: Option<&Path>,
) -> bool {
    let Some(current_file_path) = current_file_path else {
        return false;
    };
    let Some(def) = def_table.lookup_def(def_id) else {
        return false;
    };
    if !def.is_runtime() {
        return false;
    }
    let Some(def_loc) = def_table.lookup_def_location(def_id) else {
        return false;
    };
    let Some(def_path) = def_loc.path else {
        return false;
    };
    !paths_equivalent(&def_path, current_file_path)
}

fn paths_equivalent(lhs: &Path, rhs: &Path) -> bool {
    if lhs == rhs {
        return true;
    }
    match (lhs.canonicalize(), rhs.canonicalize()) {
        (Ok(a), Ok(b)) => a == b,
        _ => false,
    }
}

fn fallback_hover_from_def_table(
    def_table: &DefTable,
    type_map: Option<&crate::core::typecheck::type_map::TypeMap>,
    query_span: Span,
    current_file_path: Option<&Path>,
    query_ident: Option<&str>,
) -> Option<HoverInfo> {
    let query_ident = query_ident?;
    let current_file_path = current_file_path?;
    for def in def_table.defs() {
        if def.name != query_ident || def.is_runtime() {
            continue;
        }
        let Some(location) = def_table.lookup_def_location(def.id) else {
            continue;
        };
        let Some(path) = location.path.as_deref() else {
            continue;
        };
        if !paths_equivalent(path, current_file_path)
            || !span_contains_span(location.span, query_span)
        {
            continue;
        }
        let ty = type_map.and_then(|map| map.lookup_def_type(def));
        let display = format_hover_label(Some(&def.name), ty.as_ref(), def_table);
        return Some(HoverInfo {
            node_id: def_table
                .lookup_def_node_id(def.id)
                .unwrap_or(crate::core::tree::NodeId(0)),
            span: query_span,
            def_id: Some(def.id),
            def_name: Some(def.name.clone()),
            ty,
            display,
        });
    }
    None
}

fn identifier_at_offset(source: &str, offset: usize) -> Option<String> {
    if source.is_empty() {
        return None;
    }
    let mut idx = offset.min(source.len());
    while idx > 0 && !source.is_char_boundary(idx) {
        idx -= 1;
    }
    let bytes = source.as_bytes();
    let is_ident = |b: u8| b == b'_' || b.is_ascii_alphanumeric();

    if idx > 0
        && !is_ident(bytes[idx.saturating_sub(1)])
        && (idx >= bytes.len() || !is_ident(bytes[idx]))
    {
        return None;
    }

    let mut start = idx;
    while start > 0 && is_ident(bytes[start - 1]) {
        start -= 1;
    }
    let mut end = idx;
    while end < bytes.len() && is_ident(bytes[end]) {
        end += 1;
    }
    if start >= end {
        return None;
    }
    let ident = source[start..end].to_string();
    if is_keyword_ident(&ident) {
        return None;
    }
    Some(ident)
}

fn offset_from_position(source: &str, position: Position) -> Option<usize> {
    if position.offset > 0 && position.offset <= source.len() {
        return Some(position.offset);
    }
    // LSP query spans currently carry line/column but offset=0.
    let target_line = position.line.max(1);
    let target_col = position.column.max(1);
    let mut line = 1usize;
    let mut col = 1usize;
    for (idx, ch) in source.char_indices() {
        if line == target_line && col == target_col {
            return Some(idx);
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    if line == target_line && col == target_col {
        return Some(source.len());
    }
    None
}

fn is_keyword_ident(ident: &str) -> bool {
    matches!(
        ident,
        "fn" | "let"
            | "var"
            | "if"
            | "else"
            | "while"
            | "for"
            | "in"
            | "match"
            | "return"
            | "break"
            | "continue"
            | "type"
            | "trait"
            | "requires"
            | "state"
            | "fields"
            | "typestate"
            | "true"
            | "false"
    )
}

fn def_name_matches_query(
    def_name: &str,
    query_ident: &str,
    demangler: &TypestateNameDemangler,
) -> bool {
    if def_name == query_ident {
        return true;
    }
    let demangled = demangler
        .demangle_symbol(def_name)
        .unwrap_or_else(|| def_name.to_string());
    if demangled == query_ident {
        return true;
    }
    demangled
        .rsplit_once("::")
        .is_some_and(|(_, tail)| tail == query_ident)
}

fn syntactic_field_hover_display(source: &str, offset: usize, ident: &str) -> Option<String> {
    let bytes = source.as_bytes();
    if offset >= bytes.len() {
        return None;
    }
    let mut line_start = offset;
    while line_start > 0 && bytes[line_start - 1] != b'\n' {
        line_start -= 1;
    }
    let mut line_end = offset;
    while line_end < bytes.len() && bytes[line_end] != b'\n' {
        line_end += 1;
    }
    let line = &source[line_start..line_end];
    let ident_pos = line.find(ident)?;
    let mut i = ident_pos + ident.len();
    while i < line.len() && line.as_bytes()[i].is_ascii_whitespace() {
        i += 1;
    }
    if i >= line.len() || line.as_bytes()[i] != b':' {
        return None;
    }
    i += 1;
    while i < line.len() && line.as_bytes()[i].is_ascii_whitespace() {
        i += 1;
    }
    let ty_start = i;
    while i < line.len() {
        let b = line.as_bytes()[i];
        if b == b',' || b == b'}' || b == b';' || b.is_ascii_whitespace() {
            break;
        }
        i += 1;
    }
    if ty_start >= i {
        return None;
    }
    let ty = line[ty_start..i].trim();
    if ty.is_empty() {
        return None;
    }
    Some(format!("{ident}: {ty}"))
}

fn field_type_in_struct<'a>(ty: &'a Type, field_name: &str) -> Option<&'a Type> {
    match ty {
        Type::Struct { fields, .. } => fields
            .iter()
            .find(|field| field.name == field_name)
            .map(|field| &field.ty),
        _ => None,
    }
}

fn hover_candidate_score(
    def_name: Option<&str>,
    has_type: bool,
    query_ident: Option<&str>,
    demangler: &TypestateNameDemangler,
) -> u8 {
    let mut score: u8 = match (def_name, has_type) {
        (Some("self"), true) => 0,
        (Some("self"), false) => 0,
        (Some(_), true) => 3,
        (Some(_), false) => 2,
        (None, true) => 1,
        (None, false) => 0,
    };
    if let (Some(def_name), Some(query_ident)) = (def_name, query_ident)
        && def_name_matches_query(def_name, query_ident, demangler)
    {
        score = score.saturating_add(2);
    }
    score
}

pub(crate) fn signature_help_at_span(
    state: &LookupState,
    query_span: Span,
) -> Option<SignatureHelp> {
    let typed = state.typed.as_ref()?;
    let call = call_site_at_span(&typed.module, query_span)?;
    let mut provisional = None;
    if let Some(sig) = typed.call_sigs.get(&call.node_id) {
        let mut params = Vec::with_capacity(sig.params.len());
        for param in &sig.params {
            params.push(format!("{} {}", param_mode_name(&param.mode), param.ty));
        }
        let active_parameter = active_param_index(&call.arg_spans, query_span.start);
        let name = sig
            .def_id
            .and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()))
            .unwrap_or_else(|| "<call>".to_string());
        let label = format_signature_label(&typed.def_table, &name, &params);
        let help = SignatureHelp {
            label,
            def_id: sig.def_id,
            active_parameter,
            parameters: params,
        };
        if help.def_id.is_some() {
            return Some(help);
        }
        provisional = Some(help);
    }

    // Fallback for incomplete/mismatched calls where call-site resolution did
    // not produce `call_sigs`: derive the callable signature from the callee
    // expression type.
    let callee_ty = typed.type_map.lookup_node_type(call.callee_node_id)?;
    let crate::core::types::Type::Fn { params, .. } = callee_ty else {
        return None;
    };
    let parameters: Vec<String> = params
        .iter()
        .map(|param| format!("{} {}", fn_param_mode_name(&param.mode), param.ty))
        .collect();
    let def_id = typed
        .def_table
        .lookup_node_def_id(call.callee_node_id)
        .filter(|id| *id != UNKNOWN_DEF_ID);
    let name = def_id
        .and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()))
        .unwrap_or_else(|| "<call>".to_string());
    let label = format_signature_label(&typed.def_table, &name, &parameters);
    let active_parameter = active_param_index(&call.arg_spans, query_span.start);
    if def_id.is_some() {
        return Some(SignatureHelp {
            label,
            def_id,
            active_parameter,
            parameters,
        });
    }
    provisional.or(Some(SignatureHelp {
        label,
        def_id,
        active_parameter,
        parameters,
    }))
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

fn format_hover_label(def_name: Option<&str>, ty: Option<&Type>, def_table: &DefTable) -> String {
    let demangler = TypestateNameDemangler::from_def_table(def_table);
    let render_name = |name: &str| demangler.demangle_text(name);
    let render_type = |ty: &Type| format_type_for_hover(ty, &demangler);
    match (def_name, ty) {
        (Some(name), Some(ty)) => {
            let name = render_name(name);
            let ty = render_type(ty);
            if ty == name
                || ty.starts_with(&format!("{name} "))
                || ty.ends_with(&format!("::{name}"))
            {
                ty
            } else {
                format!("{name}: {ty}")
            }
        }
        (Some(name), None) => render_name(name),
        (None, Some(ty)) => render_type(ty),
        (None, None) => String::new(),
    }
}

fn format_type_for_hover(ty: &Type, demangler: &TypestateNameDemangler) -> String {
    match ty {
        Type::Struct { name, .. } => {
            if let Some(state_name) = demangler.demangle_state_qualified(name) {
                state_name
            } else {
                demangler.demangle_text(&ty.to_string())
            }
        }
        Type::Fn { params, ret_ty } => {
            let params = params
                .iter()
                .map(|param| {
                    format!(
                        "{} {}",
                        fn_param_mode_name(&param.mode),
                        format_type_for_hover(&param.ty, demangler)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "fn({params}) -> {}",
                format_type_for_hover(ret_ty, demangler)
            )
        }
        Type::ErrorUnion { ok_ty, err_tys } => {
            let mut parts = Vec::with_capacity(err_tys.len() + 1);
            parts.push(format_type_for_hover(ok_ty, demangler));
            for err_ty in err_tys {
                parts.push(format_type_for_hover(err_ty, demangler));
            }
            parts.join(" | ")
        }
        _ => demangler.demangle_text(&ty.to_string()),
    }
}

fn format_signature_label(def_table: &DefTable, name: &str, params: &[String]) -> String {
    let demangler = TypestateNameDemangler::from_def_table(def_table);
    let label = format!("{name}({})", params.join(", "));
    demangler.demangle_text(&label)
}

#[derive(Default)]
struct TypestateNameDemangler {
    typestate_names: Vec<String>,
}

impl TypestateNameDemangler {
    fn from_def_table(def_table: &DefTable) -> Self {
        let mut typestate_names: Vec<String> = def_table
            .defs()
            .iter()
            .filter_map(|def| def.name.strip_prefix("__ts_ctor_").map(ToString::to_string))
            .collect();
        typestate_names.sort_by_key(|name| std::cmp::Reverse(name.len()));
        typestate_names.dedup();
        Self { typestate_names }
    }

    fn demangle_text(&self, input: &str) -> String {
        let mut out = String::with_capacity(input.len());
        let mut token = String::new();
        for ch in input.chars() {
            if ch == '_' || ch.is_ascii_alphanumeric() {
                token.push(ch);
                continue;
            }
            self.flush_token(&mut out, &mut token);
            out.push(ch);
        }
        self.flush_token(&mut out, &mut token);
        out
    }

    fn flush_token(&self, out: &mut String, token: &mut String) {
        if token.is_empty() {
            return;
        }
        if let Some(mapped) = self.demangle_symbol(token) {
            out.push_str(&mapped);
        } else {
            out.push_str(token);
        }
        token.clear();
    }

    fn demangle_symbol(&self, symbol: &str) -> Option<String> {
        if let Some(ts_name) = symbol.strip_prefix("__ts_ctor_")
            && !ts_name.is_empty()
        {
            return Some(format!("{ts_name}::new"));
        }
        self.demangle_state_short(symbol)
    }

    fn demangle_state_short(&self, symbol: &str) -> Option<String> {
        let (ts_name, state_name) = self.parse_state_symbol(symbol)?;
        let _ = ts_name;
        Some(state_name)
    }

    fn demangle_state_qualified(&self, symbol: &str) -> Option<String> {
        let (ts_name, state_name) = self.parse_state_symbol(symbol)?;
        Some(format!("{ts_name}::{state_name}"))
    }

    fn parse_state_symbol(&self, symbol: &str) -> Option<(String, String)> {
        let rest = symbol.strip_prefix("__ts_")?;
        for ts_name in &self.typestate_names {
            if let Some(after_ts) = rest.strip_prefix(ts_name)
                && let Some(state_name) = after_ts.strip_prefix('_')
                && !state_name.is_empty()
            {
                return Some((ts_name.clone(), state_name.to_string()));
            }
        }
        None
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

fn fn_param_mode_name(mode: &crate::core::types::FnParamMode) -> &'static str {
    match mode {
        crate::core::types::FnParamMode::In => "in",
        crate::core::types::FnParamMode::InOut => "inout",
        crate::core::types::FnParamMode::Out => "out",
        crate::core::types::FnParamMode::Sink => "sink",
    }
}
