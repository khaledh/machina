//! Completion logic for analysis queries.
//!
//! This module owns completion context detection, scope/member candidate
//! collection, and fallback source synthesis for incomplete member probes.

mod member;
mod scope;
mod site;
mod source_probe;

use std::collections::HashSet;

use crate::core::diag::{Position, Span};
use crate::services::analysis::results::CompletionItem;
use member::{member_completions, qualified_path_completions};
use scope::{enclosing_callable_def_id, scope_completions};
use site::{CompletionSite, classify_completion_site};

pub(crate) fn collect(
    source: &str,
    query_span: Span,
    resolved: &crate::core::context::ResolvedContext,
    typed: Option<&crate::core::context::TypeCheckedContext>,
) -> Vec<CompletionItem> {
    let site = classify_completion_site(
        source,
        query_span,
        resolved,
        typed,
        enclosing_callable_def_id,
    );
    let items = dispatch_site_completions(&site, query_span.start, resolved);
    completion_post_pass(items, site.prefix())
}

pub(crate) fn synthesize_member_completion_source(
    source: &str,
    cursor: Position,
) -> Option<String> {
    source_probe::synthesize_member_completion_source(source, cursor)
}

fn dispatch_site_completions(
    site: &CompletionSite,
    fallback_cursor: Position,
    resolved: &crate::core::context::ResolvedContext,
) -> Vec<CompletionItem> {
    let items = match site {
        CompletionSite::Scope { cursor, .. } => scope_completions(resolved, *cursor),
        CompletionSite::Member {
            receiver_ty,
            caller_def_id,
            ..
        } => member_completions(resolved, receiver_ty, *caller_def_id),
        CompletionSite::QualifiedPath { path_segments, .. } => {
            qualified_path_completions(resolved, path_segments)
        }
        CompletionSite::RequiresPath { .. }
        | CompletionSite::TypeExpr { .. }
        | CompletionSite::Pattern { .. } => Vec::new(),
    };
    if items.is_empty()
        && matches!(
            site,
            CompletionSite::RequiresPath { .. }
                | CompletionSite::TypeExpr { .. }
                | CompletionSite::Pattern { .. }
        )
    {
        scope_completions(resolved, fallback_cursor)
    } else {
        items
    }
}

fn completion_post_pass(mut items: Vec<CompletionItem>, prefix: &str) -> Vec<CompletionItem> {
    items.retain(|item| item.label.starts_with(prefix));
    let mut seen = HashSet::<String>::new();
    items.retain(|item| seen.insert(item.label.clone()));
    items.sort_unstable_by(|a, b| a.label.cmp(&b.label));
    items
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::core::api::{
        parse_module_with_id_gen, resolve_stage_partial, typecheck_stage_partial,
    };
    use crate::core::context::ParsedContext;
    use crate::core::diag::{Position, Span};
    use crate::core::tree::NodeIdGen;

    use super::source_probe::position_for_offset;
    use super::{CompletionSite, classify_completion_site, enclosing_callable_def_id};

    #[test]
    fn classify_site_detects_member_context() {
        let source = r#"
type Point = { x: u64 }

Point :: {
    fn get_x(self) -> u64 { self.x }
}

fn main() {
    let p = Point { x: 1 };
    p.x
}
"#;
        let (resolved, typed) = resolved_and_typed(source);
        let cursor = cursor_at_end_of(source, "p.");
        let site = classify_completion_site(
            source,
            cursor,
            &resolved,
            Some(&typed),
            enclosing_callable_def_id,
        );
        assert!(matches!(site, CompletionSite::Member { .. }));
    }

    #[test]
    fn classify_site_detects_requires_path_context() {
        let source = r#"
requires {
    std::io
}

fn main() {}
"#;
        let resolved = resolved_only(source);
        let cursor = cursor_at_end_of(source, "std::i");
        let site =
            classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
        assert!(matches!(site, CompletionSite::RequiresPath { .. }));
    }

    #[test]
    fn classify_site_detects_type_expression_context() {
        let source = r#"
fn main() {
    let n: u64 = 1;
}
"#;
        let resolved = resolved_only(source);
        let cursor = cursor_at_end_of(source, "u64");
        let site =
            classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
        assert!(matches!(site, CompletionSite::TypeExpr { .. }));
    }

    #[test]
    fn classify_site_detects_pattern_context() {
        let source = r#"
fn f() -> u64 {
    match 1 {
        value => value,
    }
}
"#;
        let resolved = resolved_only(source);
        let cursor = cursor_at_end_of(source, "valu");
        let site =
            classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
        assert!(matches!(site, CompletionSite::Pattern { .. }));
    }

    #[test]
    fn classify_site_detects_qualified_path_context() {
        let source = r#"
type Flag = On | Off

fn main() -> u64 {
    let x = Flag::Of;
    0
}
"#;
        let resolved = resolved_only(source);
        let cursor = cursor_at_end_of(source, "Flag::Of");
        let site =
            classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
        assert!(matches!(site, CompletionSite::QualifiedPath { .. }));
    }

    fn resolved_only(source: &str) -> crate::core::context::ResolvedContext {
        let (module, id_gen) =
            parse_module_with_id_gen(source, NodeIdGen::new()).expect("parse should succeed");
        let parsed = ParsedContext::new(module, id_gen);
        resolve_stage_partial(parsed, HashMap::new(), HashMap::new()).context
    }

    fn resolved_and_typed(
        source: &str,
    ) -> (
        crate::core::context::ResolvedContext,
        crate::core::context::TypeCheckedContext,
    ) {
        let (module, id_gen) =
            parse_module_with_id_gen(source, NodeIdGen::new()).expect("parse should succeed");
        let parsed = ParsedContext::new(module, id_gen);
        let resolve_out = resolve_stage_partial(parsed, HashMap::new(), HashMap::new());
        let resolved = resolve_out.context;
        let typed = typecheck_stage_partial(resolved.clone(), resolve_out.imported_facts).context;
        (resolved, typed)
    }

    fn cursor_at_end_of(source: &str, needle: &str) -> Span {
        let idx = source
            .find(needle)
            .unwrap_or_else(|| panic!("needle not found: {needle}"));
        let offset = idx + needle.len();
        let pos = position_for_offset(source, offset).unwrap_or_else(|| Position {
            offset,
            line: 1,
            column: 1,
        });
        Span {
            start: pos,
            end: pos,
        }
    }
}
