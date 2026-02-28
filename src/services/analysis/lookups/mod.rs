//! Symbol lookup helpers used by `AnalysisDb` query entry points.
//!
//! This module keeps per-feature lookup logic out of `analysis::db` so the DB
//! focuses on snapshot/query orchestration.

mod callable_signature;
mod definition;
mod document_symbols;
mod hover;
mod semantic_tokens;
mod signature_help;

pub(crate) use definition::{def_at_span, def_location_at_span};
pub(crate) use document_symbols::document_symbols;
pub(crate) use hover::hover_at_span_in_file;
pub(crate) use semantic_tokens::semantic_tokens;
pub(crate) use signature_help::signature_help_at_span;

use crate::core::diag::{Position, Span};
use crate::core::resolve::DefTable;
use crate::core::types::Type;
use crate::services::analysis::code_actions::code_actions_for_diagnostic_with_source;
use crate::services::analysis::diagnostics::Diagnostic;
use crate::services::analysis::pipeline::LookupState;
use crate::services::analysis::results::CodeAction;
use crate::services::analysis::syntax_index::{node_at_span, span_intersects_span};

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

#[derive(Clone, Debug)]
pub(super) struct IdentifierToken {
    pub ident: String,
    pub span: Span,
    pub offset: usize,
}

/// Normalize a point/span hover or lookup position to the enclosing
/// identifier token when possible. Editor-originated queries often arrive as
/// zero-width point spans, while most tree lookups behave more predictably on
/// token spans.
pub(super) fn identifier_token_at_span(
    source: Option<&str>,
    query_span: Span,
) -> Option<IdentifierToken> {
    let source = source?;
    let offset = offset_from_position(source, query_span.start)?;
    identifier_at_offset(source, offset)
}

/// Extract the identifier token surrounding the given byte offset, or `None`
/// if the offset is on whitespace, punctuation, or a language keyword.
fn identifier_at_offset(source: &str, offset: usize) -> Option<IdentifierToken> {
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
    Some(IdentifierToken {
        ident,
        span: Span {
            start: position_at_offset(source, start),
            end: position_at_offset(source, end),
        },
        offset: idx,
    })
}

/// Convert a 1-based line/column position to a byte offset. Uses the
/// pre-computed offset when available; otherwise falls back to a line/column
/// scan (LSP query spans carry line/column but set offset=0).
pub(super) fn offset_from_position(source: &str, position: Position) -> Option<usize> {
    if position.offset > 0 && position.offset <= source.len() {
        return Some(position.offset);
    }
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

fn position_at_offset(source: &str, offset: usize) -> Position {
    let mut line = 1usize;
    let mut column = 1usize;
    for ch in source[..offset].chars() {
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    Position {
        offset,
        line,
        column,
    }
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
            | "use"
            | "as"
            | "protocol"
            | "role"
            | "msg"
            | "on"
            | "effects"
            | "emit"
            | "self"
    )
}

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
