use std::collections::HashMap;

use crate::core::api::{parse_module_with_id_gen, resolve_stage_partial, typecheck_stage_partial};
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
    let site = classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
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
    let site = classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
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
    let site = classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
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
    let site = classify_completion_site(source, cursor, &resolved, None, enclosing_callable_def_id);
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
