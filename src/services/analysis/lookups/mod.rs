//! Symbol lookup helpers used by `AnalysisDb` query entry points.
//!
//! This module keeps per-feature lookup logic out of `analysis::db` so the DB
//! focuses on snapshot/query orchestration.

mod definition;
mod hover;

pub(crate) use definition::{def_at_span, def_location_at_span};
pub(crate) use hover::hover_at_span_in_file;
use hover::try_format_source_callable_signature;

use crate::core::diag::Span;
use crate::core::resolve::{DefKind, DefTable, UNKNOWN_DEF_ID};
use crate::core::types::Type;
use crate::services::analysis::code_actions::code_actions_for_diagnostic_with_source;
use crate::services::analysis::diagnostics::Diagnostic;
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::{
    CodeAction, DocumentSymbol, SemanticToken, SemanticTokenKind, SignatureHelp,
};
use crate::services::analysis::signature_help::offset_for_position;
use crate::services::analysis::syntax_index::{
    active_param_index, call_site_at_span, document_symbol_nodes, node_at_span, node_span_map,
    position_leq, span_intersects_span,
};

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

/// Build signature help at a call site. First tries call-site resolution
/// (populated by the type checker for fully resolved calls), then falls back
/// to deriving the signature from the callee expression's `Fn` type for
/// incomplete or mismatched calls.
pub(crate) fn signature_help_at_span(
    state: &LookupState,
    query_span: Span,
    source: Option<&str>,
) -> Option<SignatureHelp> {
    let typed = state.typed.as_ref()?;
    let call = call_site_at_span(&typed.module, query_span).or_else(|| {
        // Cursor-at-boundary editing case: when the caret sits just after the
        // last typed argument token (before `,`/`)`), some spans may exclude
        // that boundary point. Nudge left by one column and retry.
        let nudged = nudge_span_left(query_span)?;
        call_site_at_span(&typed.module, nudged)
    })?;
    let active_parameter_for = |param_count: usize| {
        active_param_index_with_comma_context(
            &call.arg_spans,
            query_span.start,
            source,
            param_count,
        )
    };
    let callee_def_id = typed
        .def_table
        .lookup_node_def_id(call.callee_node_id)
        .filter(|id| *id != UNKNOWN_DEF_ID);
    let mut provisional = None;
    if let Some(sig) = typed.call_sigs.get(&call.node_id) {
        let mut params = Vec::with_capacity(sig.params.len());
        for param in &sig.params {
            params.push(format!("{} {}", param_mode_name(&param.mode), param.ty));
        }
        let name = sig
            .def_id
            .and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()))
            .unwrap_or_else(|| "<call>".to_string());
        let label = format_signature_label(&typed.def_table, &name, &params);
        let help = SignatureHelp {
            label,
            def_id: sig.def_id,
            active_parameter: active_parameter_for(params.len()),
            parameters: params,
        };
        if help.def_id.is_some() {
            return Some(help);
        }
        provisional = Some(help);
    }

    // If we can resolve the callee to a named callable definition, prefer
    // source-level signature rendering even when call-site typing failed (for
    // example, generic arity/type mismatch while the user is still typing).
    if let Some(def_id) = callee_def_id {
        if let Some(help) = source_signature_help(typed, Some(def_id), active_parameter_for) {
            return Some(help);
        }
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
    let def_id = callee_def_id;
    let name = def_id
        .and_then(|id| typed.def_table.lookup_def(id).map(|d| d.name.clone()))
        .unwrap_or_else(|| "<call>".to_string());
    let label = format_signature_label(&typed.def_table, &name, &parameters);
    if def_id.is_some() {
        return Some(SignatureHelp {
            label,
            def_id,
            active_parameter: active_parameter_for(parameters.len()),
            parameters,
        });
    }
    provisional.or(Some(SignatureHelp {
        label,
        def_id,
        active_parameter: active_parameter_for(parameters.len()),
        parameters,
    }))
}

fn source_signature_help<F>(
    typed: &crate::core::context::TypeCheckedContext,
    render_def_id: Option<crate::core::resolve::DefId>,
    active_parameter_for: F,
) -> Option<SignatureHelp>
where
    F: Fn(usize) -> usize,
{
    let demangler = TypestateNameDemangler::from_def_table(&typed.def_table);
    let (label, parameters) = try_format_source_callable_signature(
        render_def_id,
        Some(&typed.module),
        Some(&typed.type_map),
        &typed.def_table,
        &demangler,
    )?;
    Some(SignatureHelp {
        label,
        def_id: render_def_id,
        active_parameter: active_parameter_for(parameters.len()),
        parameters,
    })
}

fn active_param_index_with_comma_context(
    arg_spans: &[Span],
    pos: crate::core::diag::Position,
    source: Option<&str>,
    param_count: usize,
) -> usize {
    if param_count == 0 {
        return 0;
    }
    let mut active = active_param_index(arg_spans, pos).min(param_count.saturating_sub(1));
    if arg_spans.is_empty() {
        return active;
    }

    let Some(source) = source else {
        return active;
    };
    let Some(cursor_offset) = offset_for_position(source, pos) else {
        return active;
    };
    let Some(last_arg) = arg_spans.last() else {
        return active;
    };
    if !position_leq(last_arg.end, pos) {
        return active;
    }
    let start = last_arg.end.offset.min(source.len());
    let end = cursor_offset.min(source.len());
    if start >= end {
        return active;
    }

    if source[start..end].bytes().any(|b| b == b',') {
        active = active.max(arg_spans.len().min(param_count.saturating_sub(1)));
    }
    active
}

fn nudge_span_left(span: Span) -> Option<Span> {
    if span.start.column <= 1 || span.end.column <= 1 || span.start.line != span.end.line {
        return None;
    }
    Some(Span {
        start: crate::core::diag::Position {
            offset: span.start.offset.saturating_sub(1),
            line: span.start.line,
            column: span.start.column - 1,
        },
        end: crate::core::diag::Position {
            offset: span.end.offset.saturating_sub(1),
            line: span.end.line,
            column: span.end.column - 1,
        },
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

fn format_signature_label(def_table: &DefTable, name: &str, params: &[String]) -> String {
    let demangler = TypestateNameDemangler::from_def_table(def_table);
    let label = format!("{name}({})", params.join(", "));
    demangler.demangle_text(&label)
}

// --- Shared utilities used by hover and signature_help submodules ---

/// Translates compiler-internal mangled typestate names back to user-facing
/// qualified names for display purposes.
///
/// The compiler mangles typestate identifiers with prefixes:
/// - `__ts_ctor_<Name>` — constructor (`<Name>::new`)
/// - `__ts_spawn_<Name>` — spawn entry (`<Name>::spawn`)
/// - `__ts_<Name>_<State>` — state struct (`<State>` or `<Name>::<State>`)
///
/// Names are sorted longest-first so that greedy prefix matching works
/// correctly when one typestate name is a prefix of another.
#[derive(Default)]
pub(super) struct TypestateNameDemangler {
    typestate_names: Vec<String>,
}

impl TypestateNameDemangler {
    pub(super) fn from_def_table(def_table: &DefTable) -> Self {
        let mut typestate_names: Vec<String> = def_table
            .defs()
            .iter()
            .filter_map(|def| {
                def.name
                    .strip_prefix("__ts_ctor_")
                    .or_else(|| def.name.strip_prefix("__ts_spawn_"))
                    .map(ToString::to_string)
            })
            .collect();
        typestate_names.sort_by_key(|name| std::cmp::Reverse(name.len()));
        typestate_names.dedup();
        Self { typestate_names }
    }

    /// Demangle all mangled tokens in a display string (e.g. a type signature).
    pub(super) fn demangle_text(&self, input: &str) -> String {
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

    /// Demangle a single symbol token. Returns `None` if it isn't mangled.
    pub(super) fn demangle_symbol(&self, symbol: &str) -> Option<String> {
        if let Some(ts_name) = symbol.strip_prefix("__ts_ctor_")
            && !ts_name.is_empty()
        {
            return Some(format!("{ts_name}::new"));
        }
        if let Some(ts_name) = symbol.strip_prefix("__ts_spawn_")
            && !ts_name.is_empty()
        {
            return Some(format!("{ts_name}::spawn"));
        }
        self.demangle_state_short(symbol)
    }

    fn demangle_state_short(&self, symbol: &str) -> Option<String> {
        let (ts_name, state_name) = self.parse_state_symbol(symbol)?;
        let _ = ts_name;
        Some(state_name)
    }

    /// Demangle a state symbol to its fully qualified form: `TypestateName::StateName`.
    pub(super) fn demangle_state_qualified(&self, symbol: &str) -> Option<String> {
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

fn param_mode_name(mode: &crate::core::tree::ParamMode) -> &'static str {
    match mode {
        crate::core::tree::ParamMode::In => "in",
        crate::core::tree::ParamMode::InOut => "inout",
        crate::core::tree::ParamMode::Out => "out",
        crate::core::tree::ParamMode::Sink => "sink",
    }
}

pub(super) fn fn_param_mode_name(mode: &crate::core::types::FnParamMode) -> &'static str {
    match mode {
        crate::core::types::FnParamMode::In => "in",
        crate::core::types::FnParamMode::InOut => "inout",
        crate::core::types::FnParamMode::Out => "out",
        crate::core::types::FnParamMode::Sink => "sink",
    }
}
