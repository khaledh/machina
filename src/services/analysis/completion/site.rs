//! Completion site classification and cursor-context helpers.

use crate::core::diag::{Position, Span};
use crate::core::resolve::DefId;
use crate::core::tree::visit;
use crate::core::tree::visit::Visitor;
use crate::core::tree::{BindPattern, MatchPattern, Module, TypeExpr};
use crate::core::types::Type;
use crate::services::analysis::syntax_index::node_at_span;

use super::source_probe::{is_ident_byte, offset_for_position, single_char_span};

#[derive(Debug, Clone)]
pub(super) enum CompletionSite {
    Scope {
        prefix: String,
        cursor: Position,
    },
    Member {
        prefix: String,
        receiver_ty: Type,
        caller_def_id: Option<DefId>,
    },
    QualifiedPath {
        prefix: String,
        path_segments: Vec<String>,
    },
    RequiresPath {
        prefix: String,
    },
    TypeExpr {
        prefix: String,
    },
    Pattern {
        prefix: String,
    },
}

impl CompletionSite {
    pub(super) fn prefix(&self) -> &str {
        match self {
            Self::Scope { prefix, .. }
            | Self::Member { prefix, .. }
            | Self::QualifiedPath { prefix, .. }
            | Self::RequiresPath { prefix }
            | Self::TypeExpr { prefix }
            | Self::Pattern { prefix } => prefix,
        }
    }
}

#[derive(Debug, Clone)]
struct PrefixProbe {
    prefix: String,
    prefix_start: usize,
    dot_probe: usize,
}

pub(super) fn classify_completion_site(
    source: &str,
    query_span: Span,
    resolved: &crate::core::context::ResolvedContext,
    typed: Option<&crate::core::context::TypeCheckedContext>,
    enclosing_callable_def_id: impl Fn(&Module, Position) -> Option<DefId>,
) -> CompletionSite {
    let cursor = query_span.start;
    let offset = offset_for_position(source, query_span.start).unwrap_or(source.len());
    let probe = prefix_probe(source, offset);

    if let Some(site) = classify_member_site(
        source,
        query_span,
        typed,
        &probe,
        &enclosing_callable_def_id,
    ) {
        return site;
    }
    if requires_site_at_cursor(&resolved.module, cursor) {
        return CompletionSite::RequiresPath {
            prefix: probe.prefix,
        };
    }
    if let Some(site) = classify_qualified_path_site(source, offset, &probe) {
        return site;
    }
    let pattern_classifier = PatternAndTypeClassifier::classify(&resolved.module, cursor);
    if pattern_classifier.in_type_expr {
        return CompletionSite::TypeExpr {
            prefix: probe.prefix,
        };
    }
    if pattern_classifier.in_pattern {
        return CompletionSite::Pattern {
            prefix: probe.prefix,
        };
    }

    CompletionSite::Scope {
        prefix: probe.prefix,
        cursor,
    }
}

fn prefix_probe(source: &str, offset: usize) -> PrefixProbe {
    let bytes = source.as_bytes();
    let mut prefix_start = offset;
    while prefix_start > 0 && is_ident_byte(bytes[prefix_start - 1]) {
        prefix_start -= 1;
    }
    let prefix = source[prefix_start..offset.min(source.len())].to_string();

    let mut dot_probe = prefix_start;
    while dot_probe > 0 && bytes[dot_probe - 1].is_ascii_whitespace() {
        dot_probe -= 1;
    }

    PrefixProbe {
        prefix,
        prefix_start,
        dot_probe,
    }
}

fn classify_member_site(
    source: &str,
    query_span: Span,
    typed: Option<&crate::core::context::TypeCheckedContext>,
    probe: &PrefixProbe,
    enclosing_callable_def_id: &impl Fn(&Module, Position) -> Option<DefId>,
) -> Option<CompletionSite> {
    let bytes = source.as_bytes();
    if probe.dot_probe == 0 || bytes[probe.dot_probe - 1] != b'.' {
        return None;
    }
    let Some(typed) = typed else {
        return None;
    };

    let mut recv_end = probe.dot_probe - 1;
    while recv_end > 0 && bytes[recv_end - 1].is_ascii_whitespace() {
        recv_end -= 1;
    }
    if recv_end == 0 {
        return None;
    }
    let receiver_char = recv_end - 1;
    let Some(receiver_span) = single_char_span(source, receiver_char) else {
        return None;
    };
    let Some(node_id) = node_at_span(&typed.module, receiver_span) else {
        return None;
    };
    let Some(receiver_ty) = typed.type_map.lookup_node_type(node_id) else {
        return None;
    };

    Some(CompletionSite::Member {
        prefix: probe.prefix.clone(),
        receiver_ty,
        caller_def_id: enclosing_callable_def_id(&typed.module, query_span.start),
    })
}

fn classify_qualified_path_site(
    source: &str,
    offset: usize,
    probe: &PrefixProbe,
) -> Option<CompletionSite> {
    let path_segments = qualified_path_segments(source, offset, probe.prefix_start)?;
    Some(CompletionSite::QualifiedPath {
        prefix: probe.prefix.clone(),
        path_segments,
    })
}

fn qualified_path_segments(
    source: &str,
    offset: usize,
    prefix_start: usize,
) -> Option<Vec<String>> {
    let bytes = source.as_bytes();
    let mut cursor = prefix_start.min(offset);
    let mut segments = Vec::<String>::new();

    loop {
        while cursor > 0 && bytes[cursor - 1].is_ascii_whitespace() {
            cursor -= 1;
        }
        if cursor < 2 || bytes[cursor - 1] != b':' || bytes[cursor - 2] != b':' {
            break;
        }
        cursor -= 2;

        while cursor > 0 && bytes[cursor - 1].is_ascii_whitespace() {
            cursor -= 1;
        }
        let seg_end = cursor;
        while cursor > 0 && is_ident_byte(bytes[cursor - 1]) {
            cursor -= 1;
        }
        if seg_end == cursor {
            return None;
        }
        segments.push(source[cursor..seg_end].to_string());
    }

    if segments.is_empty() {
        return None;
    }
    segments.reverse();
    Some(segments)
}

fn requires_site_at_cursor(module: &Module, cursor: Position) -> bool {
    module
        .requires
        .iter()
        .any(|require| span_contains_pos(require.span, cursor))
}

struct PatternAndTypeClassifier {
    cursor: Position,
    in_type_expr: bool,
    in_pattern: bool,
}

impl PatternAndTypeClassifier {
    fn classify(module: &Module, cursor: Position) -> Self {
        let mut classifier = Self {
            cursor,
            in_type_expr: false,
            in_pattern: false,
        };
        classifier.visit_module(module);
        classifier
    }
}

impl Visitor for PatternAndTypeClassifier {
    fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
        if span_contains_pos(type_expr.span, self.cursor) {
            self.in_type_expr = true;
        }
        visit::walk_type_expr(self, type_expr);
    }

    fn visit_bind_pattern(&mut self, pattern: &BindPattern) {
        if span_contains_pos(pattern.span, self.cursor) {
            self.in_pattern = true;
        }
        visit::walk_bind_pattern(self, pattern);
    }

    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        let span = match pattern {
            MatchPattern::Wildcard { span }
            | MatchPattern::BoolLit { span, .. }
            | MatchPattern::IntLit { span, .. }
            | MatchPattern::Binding { span, .. }
            | MatchPattern::TypedBinding { span, .. }
            | MatchPattern::Tuple { span, .. }
            | MatchPattern::EnumVariant { span, .. } => *span,
        };
        if span_contains_pos(span, self.cursor) {
            self.in_pattern = true;
        }
        visit::walk_match_pattern(self, pattern);
    }
}

fn span_contains_pos(span: Span, pos: Position) -> bool {
    use crate::services::analysis::syntax_index::position_leq;
    position_leq(span.start, pos) && position_leq(pos, span.end)
}
