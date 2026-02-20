//! Symbol lookup helpers used by `AnalysisDb` query entry points.
//!
//! This module keeps per-feature lookup logic out of `analysis::db` so the DB
//! focuses on snapshot/query orchestration.

mod callable_signature;
mod definition;
mod hover;
mod signature_help;

pub(crate) use definition::{def_at_span, def_location_at_span};
pub(crate) use hover::hover_at_span_in_file;
pub(crate) use signature_help::signature_help_at_span;

use crate::core::diag::Span;
use crate::core::resolve::{DefKind, DefTable};
use crate::core::types::Type;
use crate::services::analysis::code_actions::code_actions_for_diagnostic_with_source;
use crate::services::analysis::diagnostics::Diagnostic;
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::{
    CodeAction, DocumentSymbol, SemanticToken, SemanticTokenKind,
};
use crate::services::analysis::syntax_index::{
    document_symbol_nodes, node_at_span, node_span_map, span_intersects_span,
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
