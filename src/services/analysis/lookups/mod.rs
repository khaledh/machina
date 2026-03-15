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
mod symbol_target;

pub(crate) use definition::{def_at_span, def_location_at_span};
pub(crate) use document_symbols::document_symbols;
pub(crate) use hover::hover_at_span_in_file;
pub(crate) use semantic_tokens::semantic_tokens;
pub(crate) use signature_help::signature_help_at_span;
pub(crate) use symbol_target::{
    ResolvedSymbolTarget, hover_for_resolved_target, location_for_resolved_target,
    resolved_target_def_id, signature_help_for_resolved_target_at_call_site,
};

use crate::core::ast::visit::{Visitor, walk_module};
use crate::core::ast::{BindPattern, BindPatternKind, Module, StmtExpr, StmtExprKind};
use crate::core::diag::{Position, Span};
use crate::core::resolve::{DefId, DefTable};
use crate::core::typecheck::type_map::TypeMap;
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

pub(crate) fn resolved_binding_type_for_def(
    module: &Module,
    type_map: &TypeMap,
    def_table: &DefTable,
    def_id: DefId,
    fallback: Option<Type>,
) -> Option<Type> {
    match fallback {
        Some(ty) if !ty.contains_unresolved() => Some(ty),
        _ => binding_value_node_id_for_def(module, def_table, def_id)
            .and_then(|node_id| type_map.lookup_node_type(node_id))
            .filter(|ty| !matches!(ty, Type::Unknown))
            .or(fallback),
    }
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

pub(crate) fn binding_value_node_id_for_def(
    module: &Module,
    def_table: &DefTable,
    target_def_id: DefId,
) -> Option<crate::core::ast::NodeId> {
    struct Finder<'a> {
        def_table: &'a DefTable,
        target_def_id: DefId,
        found: Option<crate::core::ast::NodeId>,
    }

    impl Visitor for Finder<'_> {
        fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
            if self.found.is_some() {
                return;
            }
            match &stmt.kind {
                StmtExprKind::LetBind { pattern, value, .. }
                | StmtExprKind::VarBind { pattern, value, .. } => {
                    if pattern_contains_def_id(pattern, self.def_table, self.target_def_id) {
                        self.found = Some(value.id);
                        return;
                    }
                }
                StmtExprKind::Using { binding, value, .. } => {
                    if self.def_table.lookup_node_def_id(binding.id) == Some(self.target_def_id) {
                        self.found = Some(value.id);
                        return;
                    }
                }
                _ => {}
            }
            crate::core::ast::visit::walk_stmt_expr(self, stmt);
        }
    }

    let mut finder = Finder {
        def_table,
        target_def_id,
        found: None,
    };
    walk_module(&mut finder, module);
    finder.found
}

fn pattern_contains_def_id(
    pattern: &BindPattern,
    def_table: &DefTable,
    target_def_id: DefId,
) -> bool {
    match &pattern.kind {
        BindPatternKind::Name { .. } => {
            def_table.lookup_node_def_id(pattern.id) == Some(target_def_id)
        }
        BindPatternKind::Tuple { patterns } => patterns
            .iter()
            .any(|item| pattern_contains_def_id(item, def_table, target_def_id)),
        BindPatternKind::Struct { fields, .. } => fields
            .iter()
            .any(|field| pattern_contains_def_id(&field.pattern, def_table, target_def_id)),
        BindPatternKind::Array { patterns } => patterns
            .iter()
            .any(|item| pattern_contains_def_id(item, def_table, target_def_id)),
    }
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
/// zero-width point spans, while most AST lookups behave more predictably on
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

pub(super) fn position_at_offset(source: &str, offset: usize) -> Position {
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
            | "true"
            | "false"
            | "use"
            | "as"
            | "role"
            | "msg"
            | "on"
            | "effects"
            | "emit"
            | "self"
    )
}

