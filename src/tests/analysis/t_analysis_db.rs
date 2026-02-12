use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::core::capsule::ModuleId;
use crate::core::diag::{Position, Span};
use crate::core::types::Type;
use crate::services::analysis::db::AnalysisDb;
use crate::services::analysis::diagnostics::DiagnosticPhase;
use crate::services::analysis::module_graph::ModuleGraph;
use crate::services::analysis::pipeline::ROOT_POISON_NODE;
use crate::services::analysis::query::{CancellationToken, QueryCancelled, QueryKey, QueryKind};

static ANALYSIS_TMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[test]
fn analysis_db_snapshot_reflects_overlay_edits() {
    let mut db = AnalysisDb::new();
    let file_id = db.upsert_disk_text(PathBuf::from("src/main.mc"), "fn main() {}");
    db.set_overlay(file_id, "fn main() { let x = 1; }");

    let snap = db.snapshot();
    assert_eq!(
        snap.text(file_id).as_deref(),
        Some("fn main() { let x = 1; }")
    );

    db.clear_overlay(file_id);
    let snap2 = db.snapshot();
    assert_eq!(snap2.text(file_id).as_deref(), Some("fn main() {}"));
}

#[test]
fn analysis_db_module_invalidation_keeps_unaffected_cache() {
    let mut db = AnalysisDb::new();
    let mut edges = HashMap::new();
    // 1 -> 2 -> 3, 4 isolated
    edges.insert(ModuleId(1), vec![ModuleId(2)]);
    edges.insert(ModuleId(2), vec![ModuleId(3)]);
    edges.insert(ModuleId(3), vec![]);
    edges.insert(ModuleId(4), vec![]);
    db.set_module_graph(ModuleGraph::new(edges));

    let rev = 1u64;
    db.execute_query(
        QueryKey::new(QueryKind::Diagnostics, ModuleId(1), rev),
        |rt| {
            rt.execute(
                QueryKey::new(QueryKind::TypecheckModule, ModuleId(2), rev),
                |rt| {
                    rt.execute(
                        QueryKey::new(QueryKind::ResolveModule, ModuleId(3), rev),
                        |_rt| Ok::<u64, QueryCancelled>(7),
                    )
                },
            )
        },
    )
    .expect("initial chain should succeed");
    db.execute_query(
        QueryKey::new(QueryKind::Diagnostics, ModuleId(4), rev),
        |_rt| Ok::<u64, QueryCancelled>(9),
    )
    .expect("independent query should succeed");

    db.clear_cache_stats();
    db.invalidate_changed_modules(&HashSet::from([ModuleId(3)]));

    let mut leaf_recomputed = 0usize;
    let chain = db
        .execute_query(
            QueryKey::new(QueryKind::Diagnostics, ModuleId(1), rev),
            |rt| {
                rt.execute(
                    QueryKey::new(QueryKind::TypecheckModule, ModuleId(2), rev),
                    |rt| {
                        rt.execute(
                            QueryKey::new(QueryKind::ResolveModule, ModuleId(3), rev),
                            |_rt| {
                                leaf_recomputed += 1;
                                Ok::<u64, QueryCancelled>(7)
                            },
                        )
                    },
                )
            },
        )
        .expect("invalidated chain should recompute");
    let isolated = db
        .execute_query(
            QueryKey::new(QueryKind::Diagnostics, ModuleId(4), rev),
            |_rt| Ok::<u64, QueryCancelled>(0),
        )
        .expect("isolated module should stay cached");

    assert_eq!(chain, 7);
    assert_eq!(isolated, 9);
    assert_eq!(leaf_recomputed, 1);
    assert_eq!(db.cache_stats().hits, 1);
}

#[test]
fn diagnostics_follow_overlay_edits() {
    let mut db = AnalysisDb::new();
    let path = PathBuf::from("examples/tmp.mc");
    let file_id = db.upsert_disk_text(path.clone(), "fn main() -> u64 { 0 }");

    let clean = db
        .diagnostics_for_file(file_id)
        .expect("diagnostics query should succeed");
    assert!(clean.is_empty(), "expected no diagnostics for valid source");

    db.set_overlay(file_id, "fn main( {");
    let broken = db
        .diagnostics_for_file(file_id)
        .expect("diagnostics query should succeed");
    assert!(
        !broken.is_empty(),
        "expected diagnostics for broken overlay"
    );
    assert!(broken.iter().any(|d| d.phase == DiagnosticPhase::Parse));

    db.clear_overlay(file_id);
    let recovered = db
        .diagnostics_for_path(&path)
        .expect("diagnostics query should succeed");
    assert!(
        recovered.is_empty(),
        "expected diagnostics to clear after overlay removal"
    );
}

#[test]
fn diagnostics_respect_cancellation() {
    let token = CancellationToken::new();
    token.cancel();

    let mut db = AnalysisDb::with_cancellation_token(token);
    let file_id = db.upsert_disk_text(PathBuf::from("examples/cancel.mc"), "fn main() {}");

    let result = db.diagnostics_for_file(file_id);
    assert_eq!(
        result,
        Err(QueryCancelled),
        "cancelled token should abort diagnostics"
    );
}

#[test]
fn poisoned_nodes_are_exposed_for_broken_files() {
    let mut db = AnalysisDb::new();
    let file_id = db.upsert_disk_text(PathBuf::from("examples/poisoned.mc"), "fn main( {");

    let poisoned = db
        .poisoned_nodes_for_file(file_id)
        .expect("poison query should succeed");
    assert!(poisoned.contains(&ROOT_POISON_NODE));
}

#[test]
fn def_at_returns_definition_for_use_site() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 { id(1) }
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/lookup.mc"), source);

    let mut use_span = span_for_substring(source, "id(1)");
    use_span.end = position_at(source, use_span.start.offset + 2);
    let def_id = db
        .def_at_file(file_id, use_span)
        .expect("def_at query should succeed");

    assert!(def_id.is_some(), "expected def lookup at call site");
}

#[test]
fn def_at_still_works_with_unrelated_resolve_error() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 {
    let y = missing;
    id(1)
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/lookup_partial.mc"), source);

    let mut use_span = span_for_substring(source, "id(1)");
    use_span.end = position_at(source, use_span.start.offset + 2);
    let def_id = db
        .def_at_file(file_id, use_span)
        .expect("def_at query should succeed");

    assert!(def_id.is_some(), "expected def lookup at healthy call site");
}

#[test]
fn def_at_returns_none_for_unknown_symbol_target() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 {
    let y = missing;
    id(1)
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/lookup_unknown_target.mc"), source);

    let query_span = span_for_substring(source, "missing");
    let def_id = db
        .def_at_file(file_id, query_span)
        .expect("def_at query should succeed");

    assert_eq!(def_id, None, "unknown symbol should not resolve to a def");
}

#[test]
fn def_location_points_to_declaration_site() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 { id(1) }
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/lookup_def_location.mc"), source);

    let mut use_span = span_for_substring(source, "id(1)");
    use_span.end = position_at(source, use_span.start.offset + 2);
    let location = db
        .def_location_at_file(file_id, use_span)
        .expect("definition location query should succeed")
        .expect("expected definition location");

    assert_eq!(location.file_id, file_id);
    assert_eq!(location.span.start.line, 2);
    assert_eq!(location.span.start.column, 1);
}

#[test]
fn type_at_returns_expression_type() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 { id(1) }
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/lookup_type.mc"), source);

    let call_span = span_for_substring(source, "id(1)");
    let ty = db
        .type_at_file(file_id, call_span)
        .expect("type_at query should succeed");

    assert_eq!(ty, Some(Type::uint(64)));
}

#[test]
fn type_at_still_works_with_unrelated_type_error() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 {
    let bad: u64 = true;
    id(1)
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/type_at_partial.mc"), source);

    let call_span = span_for_substring(source, "id(1)");
    let ty = db
        .type_at_file(file_id, call_span)
        .expect("type_at query should succeed");

    assert_eq!(ty, Some(Type::uint(64)));
}

#[test]
fn type_at_returns_none_for_unknown_symbol_target() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 {
    let y = missing;
    id(1)
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/type_at_unknown_target.mc"), source);

    let query_span = span_for_substring(source, "missing");
    let ty = db
        .type_at_file(file_id, query_span)
        .expect("type_at query should succeed");

    assert_eq!(ty, None, "unknown symbol should not expose unknown type");
}

#[test]
fn hover_includes_symbol_and_type() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 { id(1) }
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/lookup_hover.mc"), source);

    let mut call_span = span_for_substring(source, "id(1)");
    call_span.end = position_at(source, call_span.start.offset + 2);
    let hover = db
        .hover_at_file(file_id, call_span)
        .expect("hover query should succeed")
        .expect("expected hover info");

    assert_eq!(hover.def_name.as_deref(), Some("id"));
    assert!(hover.ty.is_some(), "expected hover type for symbol");
    assert!(
        hover.display.starts_with("id:"),
        "expected hover label to include symbol name"
    );
}

#[test]
fn hover_returns_none_for_unknown_symbol_target() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 {
    let y = missing;
    id(1)
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/hover_unknown_target.mc"), source);

    let query_span = span_for_substring(source, "missing");
    let hover = db
        .hover_at_file(file_id, query_span)
        .expect("hover query should succeed");

    assert_eq!(
        hover, None,
        "unknown symbol should not return hover payload"
    );
}

#[test]
fn completions_include_callables_and_types() {
    let mut db = AnalysisDb::new();
    let source = r#"
type Packet = {
    id: u64,
}

fn id(x: u64) -> u64 { x }
fn main() -> u64 { id(1) }
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/completions.mc"), source);
    let query_span = cursor_after_substring(source, "{ id(1)");
    let completions = db
        .completions_at_file(file_id, query_span)
        .expect("completions query should succeed");

    assert!(
        completions.iter().any(|c| c.label == "id"),
        "expected function completion for `id`"
    );
    assert!(
        completions.iter().any(|c| c.label == "Packet"),
        "expected type completion for `Packet`"
    );
}

#[test]
fn completions_still_include_symbols_with_unrelated_resolve_error() {
    let mut db = AnalysisDb::new();
    let source = r#"
type Packet = {
    id: u64,
}

fn id(x: u64) -> u64 { x }
fn main() -> u64 {
    let y = missing;
    id(1)
}
"#;
    let file_id = db.upsert_disk_text(
        PathBuf::from("examples/completions_partial_resolve.mc"),
        source,
    );
    let query_span = cursor_after_substring(source, "{\n    ");
    let completions = db
        .completions_at_file(file_id, query_span)
        .expect("completions query should succeed");

    assert!(
        completions.iter().any(|c| c.label == "id"),
        "expected function completion for `id` even with unrelated resolve error"
    );
    assert!(
        completions.iter().any(|c| c.label == "Packet"),
        "expected type completion for `Packet` even with unrelated resolve error"
    );
}

#[test]
fn completions_respect_lexical_scope_and_shadowing() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn main() -> u64 {
    let outer_a = 1;
    {
        let inner_a = 2;
        inner
    }
}
"#;
    let file_id = db.upsert_disk_text(
        PathBuf::from("examples/completions_scope_shadow.mc"),
        source,
    );
    let query_span = span_for_last_substring(source, "inner");
    let completions = db
        .completions_at_file(file_id, query_span)
        .expect("completions query should succeed");

    assert!(
        completions.iter().any(|c| c.label == "inner_a"),
        "expected local completion from current scope"
    );
    assert!(
        !completions.iter().any(|c| c.label == "outer_a"),
        "did not expect unrelated outer-name prefix in filtered results"
    );
}

#[test]
fn completions_do_not_leak_sibling_block_bindings() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn main() -> u64 {
    {
        let hidden = 1;
    }
    hi
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/completions_scope_leak.mc"), source);
    let query_span = span_for_last_substring(source, "hi");
    let completions = db
        .completions_at_file(file_id, query_span)
        .expect("completions query should succeed");

    assert!(
        !completions.iter().any(|c| c.label == "hidden"),
        "sibling block binding should not be visible outside its block"
    );
}

#[test]
fn completions_support_member_mode_for_struct_fields_methods_and_properties() {
    let mut db = AnalysisDb::new();
    let source = r#"
type Point = {
    x: u64,
    y: u64,
}

Point :: {
    fn sum(self) -> u64 {
        self.x + self.y
    }

    prop total: u64 {
        get { self.x + self.y }
    }
}

fn main() -> u64 {
    let p = Point { x: 1, y: 2 };
    let probe = p.t;
    0
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/completions_members.mc"), source);
    let query_span = span_for_substring(source, "p.");
    let completions = db
        .completions_at_file(file_id, query_span)
        .expect("completions query should succeed");

    assert!(
        completions.iter().any(|c| c.label == "total"),
        "expected property completion on receiver"
    );
    assert!(
        completions.iter().any(|c| c.label == "sum"),
        "expected method completion on receiver"
    );
    assert!(
        completions.iter().any(|c| c.label == "x"),
        "expected field completion on receiver"
    );
    assert!(
        !completions.iter().any(|c| c.label == "main"),
        "member completion should not include unrelated global symbols"
    );
}

#[test]
fn member_completions_survive_unrelated_resolve_errors() {
    let mut db = AnalysisDb::new();
    let source = r#"
type Point = {
    x: u64,
    y: u64,
}

Point :: {
    fn sum(self) -> u64 {
        self.x + self.y
    }

    prop total: u64 {
        get { self.x + self.y }
    }
}

fn main() -> u64 {
    let p = Point { x: 1, y: 2 };
    let bad = missing_symbol;
    let probe = p.t;
    0
}
"#;
    let file_id = db.upsert_disk_text(
        PathBuf::from("examples/completions_members_partial_resolve.mc"),
        source,
    );
    let query_span = span_for_substring(source, "p.");
    let completions = db
        .completions_at_file(file_id, query_span)
        .expect("completions query should succeed");

    assert!(
        completions.iter().any(|c| c.label == "total"),
        "expected property completion even with unrelated resolve error"
    );
    assert!(
        completions.iter().any(|c| c.label == "sum"),
        "expected method completion even with unrelated resolve error"
    );
    assert!(
        completions.iter().any(|c| c.label == "x"),
        "expected field completion even with unrelated resolve error"
    );
}

#[test]
fn member_completions_work_when_cursor_is_after_dot_in_incomplete_member_expr() {
    let mut db = AnalysisDb::new();
    let source = r#"
type Point = {
    x: u64,
    y: u64,
}

Point :: {
    fn sum(self) -> u64 {
        self.x + self.y
    }

    prop total: u64 {
        get { self.x + self.y }
    }
}

fn main() -> u64 {
    let p = Point { x: 1, y: 2 };
    let bad = missing_symbol;
    let probe = p.;
    0
}
"#;
    let file_id = db.upsert_disk_text(
        PathBuf::from("examples/completions_members_after_dot.mc"),
        source,
    );
    let query_span = span_for_substring(source, "p.");
    let completions = db
        .completions_at_file(file_id, query_span)
        .expect("completions query should succeed");

    assert!(
        completions.iter().any(|c| c.label == "total"),
        "expected property completion when querying at `p.`"
    );
    assert!(
        completions.iter().any(|c| c.label == "sum"),
        "expected method completion when querying at `p.`"
    );
    assert!(
        completions.iter().any(|c| c.label == "x"),
        "expected field completion when querying at `p.`"
    );
}

#[test]
fn member_completions_work_for_standalone_dot_expr_without_semicolon() {
    let mut db = AnalysisDb::new();
    let source = r#"
type Point = {
    x: u64,
    y: u64,
}

Point :: {
    fn sum(self) -> u64 {
        self.x + self.y
    }

    prop total: u64 {
        get { self.x + self.y }
    }
}

fn main() {
    let p = Point { x: 1, y: 2 };
    p.
    let z = 0;
}
"#;
    let file_id = db.upsert_disk_text(
        PathBuf::from("examples/completions_members_standalone_dot.mc"),
        source,
    );
    let query_span = span_for_substring(source, "p.");
    let completions = db
        .completions_at_file(file_id, query_span)
        .expect("completions query should succeed");

    assert!(
        completions.iter().any(|c| c.label == "total"),
        "expected property completion for standalone `p.`"
    );
    assert!(
        completions.iter().any(|c| c.label == "sum"),
        "expected method completion for standalone `p.`"
    );
    assert!(
        completions.iter().any(|c| c.label == "x"),
        "expected field completion for standalone `p.`"
    );
}

#[test]
fn completions_apply_prefix_filtering() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn alpha() -> u64 { 1 }
fn beta() -> u64 { 2 }
fn main() -> u64 {
    al
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/completions_prefix.mc"), source);
    let query_span = span_for_last_substring(source, "al");
    let completions = db
        .completions_at_file(file_id, query_span)
        .expect("completions query should succeed");

    assert!(
        completions.iter().any(|c| c.label == "alpha"),
        "expected matching prefix candidate"
    );
    assert!(
        !completions.iter().any(|c| c.label == "beta"),
        "did not expect non-matching prefix candidate"
    );
}

#[test]
fn mixed_region_resolve_fixture_keeps_healthy_symbol_queries() {
    let mut db = AnalysisDb::new();
    let source = r#"
type Packet = {
    id: u64,
}

fn id(x: u64) -> u64 { x }

fn bad_region() -> u64 {
    missing
}

fn main() -> u64 {
    id(1)
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/mixed_resolve_regions.mc"), source);

    let mut id_use_span = span_for_substring(source, "id(1)");
    id_use_span.end = position_at(source, id_use_span.start.offset + 2);

    let def_id = db
        .def_at_file(file_id, id_use_span)
        .expect("def_at query should succeed");
    assert!(
        def_id.is_some(),
        "expected def lookup in healthy region despite unresolved symbol elsewhere"
    );

    let hover = db
        .hover_at_file(file_id, id_use_span)
        .expect("hover query should succeed")
        .expect("expected hover info in healthy region");
    assert_eq!(hover.def_name.as_deref(), Some("id"));

    let completions = db
        .completions_at_file(file_id, cursor_after_substring(source, "{\n    id(1)"))
        .expect("completions query should succeed");
    assert!(
        completions.iter().any(|c| c.label == "id"),
        "expected function completion in mixed resolve fixture"
    );
    assert!(
        completions.iter().any(|c| c.label == "Packet"),
        "expected type completion in mixed resolve fixture"
    );
}

#[test]
fn mixed_region_type_fixture_keeps_healthy_type_queries() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }

fn bad_region() -> u64 {
    let v: u64 = true;
    v
}

fn main() -> u64 {
    id(1)
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/mixed_type_regions.mc"), source);

    let mut id_use_span = span_for_substring(source, "id(1)");
    id_use_span.end = position_at(source, id_use_span.start.offset + 2);

    let def_id = db
        .def_at_file(file_id, id_use_span)
        .expect("def_at query should succeed");
    assert!(
        def_id.is_some(),
        "expected def lookup in healthy region despite type error elsewhere"
    );

    let ty = db
        .type_at_file(file_id, span_for_substring(source, "id(1)"))
        .expect("type_at query should succeed");
    assert!(
        ty.is_some(),
        "expected a concrete type in healthy region despite type error elsewhere"
    );

    let hover = db
        .hover_at_file(file_id, id_use_span)
        .expect("hover query should succeed")
        .expect("expected hover info in healthy region");
    assert_eq!(hover.def_name.as_deref(), Some("id"));
    assert!(
        hover.ty.is_some(),
        "expected hover type in healthy region despite type error elsewhere"
    );

    let completions = db
        .completions_at_file(file_id, cursor_after_substring(source, "{\n    id(1)"))
        .expect("completions query should succeed");
    assert!(
        completions.iter().any(|c| c.label == "id"),
        "expected function completion in mixed type fixture"
    );
}

#[test]
fn signature_help_returns_call_signature_and_active_parameter() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn pair(a: u64, b: u64) -> u64 { a + b }
fn main() -> u64 { pair(1, 2) }
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/signature_help.mc"), source);

    let query_span = span_for_substring(source, "2");
    let sig = db
        .signature_help_at_file(file_id, query_span)
        .expect("signature help query should succeed")
        .expect("expected signature help at call site");

    assert!(
        sig.label.starts_with("pair("),
        "unexpected label: {}",
        sig.label
    );
    assert_eq!(sig.parameters.len(), 2);
    assert_eq!(sig.active_parameter, 1);
}

#[test]
fn references_returns_definition_and_use_sites() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 { id(1) + id(2) }
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/references.mc"), source);

    let mut def_span = span_for_substring(source, "id(x");
    def_span.end = position_at(source, def_span.start.offset + 2);
    let def_id = db
        .def_at_file(file_id, def_span)
        .expect("def lookup should succeed")
        .expect("expected def id for `id`");

    let refs = db
        .references(def_id)
        .expect("references query should succeed");
    assert!(
        refs.len() >= 3,
        "expected definition plus both call sites, got {}",
        refs.len()
    );
    for pair in refs.windows(2) {
        let lhs = &pair[0];
        let rhs = &pair[1];
        assert!(
            (lhs.path.clone(), lhs.span.start.offset) <= (rhs.path.clone(), rhs.span.start.offset),
            "references should be deterministically sorted"
        );
    }
}

#[test]
fn rename_plan_reports_conflicts_for_existing_name() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn foo() -> u64 { 1 }
fn bar() -> u64 { foo() }
fn baz() -> u64 { 2 }
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/rename_conflict.mc"), source);

    let mut foo_span = span_for_substring(source, "foo()");
    foo_span.end = position_at(source, foo_span.start.offset + 3);
    let def_id = db
        .def_at_file(file_id, foo_span)
        .expect("def lookup should succeed")
        .expect("expected def id for foo");

    let plan = db
        .rename_plan(def_id, "baz")
        .expect("rename plan query should succeed");
    assert!(
        !plan.can_apply(),
        "rename should be blocked by existing symbol conflict"
    );
    assert!(!plan.conflicts.is_empty(), "expected conflict details");
}

#[test]
fn rename_plan_returns_deterministic_edits() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn foo() -> u64 { 1 }
fn main() -> u64 {
    let a = foo();
    let b = foo();
    a + b
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/rename_ok.mc"), source);

    let mut foo_span = span_for_substring(source, "foo()");
    foo_span.end = position_at(source, foo_span.start.offset + 3);
    let def_id = db
        .def_at_file(file_id, foo_span)
        .expect("def lookup should succeed")
        .expect("expected def id for foo");

    let plan = db
        .rename_plan(def_id, "renamed")
        .expect("rename plan query should succeed");
    assert!(
        plan.conflicts.is_empty(),
        "unexpected conflicts: {:?}",
        plan.conflicts
    );
    assert!(plan.can_apply(), "expected rename to be applicable");
    assert!(
        plan.edits.len() >= 3,
        "expected definition plus use sites, got {}",
        plan.edits.len()
    );
    assert!(
        plan.edits.iter().all(|edit| edit.replacement == "renamed"),
        "all edits should use new identifier spelling"
    );
    for pair in plan.edits.windows(2) {
        let lhs = &pair[0].location;
        let rhs = &pair[1].location;
        assert!(
            (lhs.path.clone(), lhs.span.start.offset) <= (rhs.path.clone(), rhs.span.start.offset),
            "rename edits should be deterministically sorted"
        );
    }
}

#[test]
fn document_symbols_returns_outline_entries() {
    let mut db = AnalysisDb::new();
    let source = r#"
type Packet = { id: u64 }
trait Runnable { fn run(self) -> u64; }
fn ping() -> u64 { 1 }
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/document_symbols.mc"), source);
    let symbols = db
        .document_symbols_at_file(file_id)
        .expect("document symbol query should succeed");

    assert!(
        symbols.iter().any(|s| s.name == "Packet"),
        "expected type symbol"
    );
    assert!(
        symbols.iter().any(|s| s.name == "Runnable"),
        "expected trait symbol"
    );
    assert!(
        symbols.iter().any(|s| s.name == "ping"),
        "expected function symbol"
    );
}

#[test]
fn semantic_tokens_are_stable_for_same_snapshot() {
    let mut db = AnalysisDb::new();
    let source = r#"
fn id(x: u64) -> u64 { x }
fn main() -> u64 { id(1) + id(2) }
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/semantic_tokens.mc"), source);

    let tokens_a = db
        .semantic_tokens_at_file(file_id)
        .expect("semantic token query should succeed");
    let tokens_b = db
        .semantic_tokens_at_file(file_id)
        .expect("semantic token query should succeed");

    assert_eq!(tokens_a, tokens_b, "token query should be deterministic");
    assert!(!tokens_a.is_empty(), "expected at least one semantic token");
}

#[test]
fn code_actions_include_try_union_fix() {
    let mut db = AnalysisDb::new();
    let source = r#"
type ParseErr = {}
type OtherErr = {}

fn may_fail() -> u64 | ParseErr {
    ParseErr{}
}

fn use_try() -> u64 | OtherErr {
    let x = may_fail()?;
    x
}
"#;
    let file_id = db.upsert_disk_text(PathBuf::from("examples/code_actions_try.mc"), source);
    let query_span = span_for_substring(source, "may_fail()?");

    let actions = db
        .code_actions_at_file(file_id, query_span)
        .expect("code action query should succeed");

    assert!(
        actions.iter().any(|a| a
            .title
            .contains("Add `ParseErr` to function return error union")),
        "expected try-propagation quick fix, got: {:?}",
        actions
    );
}

#[test]
fn code_actions_include_union_match_exhaustiveness_fix() {
    let mut db = AnalysisDb::new();
    let source = r#"
type ParseErr = {}
type IoErr = {}

fn load() -> u64 | ParseErr | IoErr {
    IoErr{}
}

fn main() -> u64 {
    match load() {
        v: u64 => v,
        ParseErr => 0,
    }
}
"#;
    let file_id = db.upsert_disk_text(
        PathBuf::from("examples/code_actions_match_union.mc"),
        source,
    );
    let query_span = span_for_substring(source, "match load()");

    let actions = db
        .code_actions_at_file(file_id, query_span)
        .expect("code action query should succeed");

    assert!(
        actions.iter().any(|a| a
            .title
            .contains("Add match arms for missing union variants: ParseErr | IoErr")),
        "expected union match exhaustiveness quick fix, got: {:?}",
        actions
    );
    let union_fix = actions
        .iter()
        .find(|a| a.diagnostic_code == "MC-SEMCK-NonExhaustiveUnionMatch")
        .expect("expected semcheck union quick fix action");
    assert!(
        !union_fix.edits.is_empty(),
        "expected actionable text edits"
    );
    assert!(
        union_fix.edits[0].new_text.contains("v0: ParseErr =>"),
        "expected generated ParseErr arm edit, got: {:?}",
        union_fix.edits
    );
    assert!(
        union_fix.edits[0].new_text.contains("v1: IoErr =>"),
        "expected generated IoErr arm edit, got: {:?}",
        union_fix.edits
    );
}

#[test]
fn code_actions_are_deterministic() {
    let mut db = AnalysisDb::new();
    let source = r#"
type ParseErr = {}
fn may_fail() -> u64 | ParseErr { ParseErr{} }
fn use_try() -> u64 {
    let x = may_fail()?;
    x
}
"#;
    let file_id = db.upsert_disk_text(
        PathBuf::from("examples/code_actions_deterministic.mc"),
        source,
    );
    let query_span = span_for_substring(source, "may_fail()?");

    let a = db
        .code_actions_at_file(file_id, query_span)
        .expect("first code action query should succeed");
    let b = db
        .code_actions_at_file(file_id, query_span)
        .expect("second code action query should succeed");

    assert_eq!(a, b, "code actions should be deterministic");
    assert!(!a.is_empty(), "expected at least one code action");
}

#[test]
fn diagnostics_for_program_file_uses_dependency_overlay() {
    let run_id = ANALYSIS_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_analysis_program_overlay_{}_{}",
        std::process::id(),
        run_id
    ));
    fs::create_dir_all(temp_dir.join("app")).expect("failed to create temp module tree");

    let entry_path = temp_dir.join("main.mc");
    let dep_path = temp_dir.join("app").join("dep.mc");
    let entry_source = r#"
requires {
    app::dep as dep
}

fn main() -> u64 {
    0
}
"#;
    let dep_source = r#"
@[public]
fn value() -> u64 {
    1
}
"#;

    fs::write(&entry_path, entry_source).expect("failed to write entry source");
    fs::write(&dep_path, dep_source).expect("failed to write dependency source");

    let mut db = AnalysisDb::new();
    let entry_id = db.upsert_disk_text(entry_path.clone(), entry_source);
    let dep_id = db.upsert_disk_text(dep_path.clone(), dep_source);

    let before = db
        .diagnostics_for_program_file(entry_id)
        .expect("program diagnostics query should succeed");
    assert!(
        before.is_empty(),
        "expected clean diagnostics before dependency overlay edits"
    );

    db.set_overlay(dep_id, "fn value( {");
    let after = db
        .diagnostics_for_program_file(entry_id)
        .expect("program diagnostics query should succeed");
    assert!(
        !after.is_empty(),
        "expected dependency overlay parse error to surface via entry diagnostics"
    );

    let _ = fs::remove_dir_all(&temp_dir);
}

#[test]
fn diagnostics_for_program_file_reuses_unchanged_module_queries() {
    let run_id = ANALYSIS_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_analysis_program_cache_{}_{}",
        std::process::id(),
        run_id
    ));
    fs::create_dir_all(temp_dir.join("app")).expect("failed to create temp module tree");

    let entry_path = temp_dir.join("main.mc");
    let dep_path = temp_dir.join("app").join("dep.mc");
    let entry_source = r#"
requires {
    app::dep as dep
}

fn main() -> u64 {
    0
}
"#;
    let dep_source = r#"
@[public]
fn value() -> u64 {
    1
}
"#;

    fs::write(&entry_path, entry_source).expect("failed to write entry source");
    fs::write(&dep_path, dep_source).expect("failed to write dependency source");

    let mut db = AnalysisDb::new();
    let entry_id = db.upsert_disk_text(entry_path.clone(), entry_source);
    let dep_id = db.upsert_disk_text(dep_path.clone(), dep_source);

    db.diagnostics_for_program_file(entry_id)
        .expect("initial diagnostics query should succeed");
    db.clear_cache_stats();

    db.set_overlay(
        dep_id,
        r#"
@[public]
fn value() -> u64 {
    2
}
"#,
    );
    db.diagnostics_for_program_file(entry_id)
        .expect("diagnostics query after dependency overlay should succeed");

    let stats = db.cache_stats();
    assert!(
        stats.hits > 0,
        "expected at least one cache hit for unchanged module queries"
    );
    assert!(
        stats.misses > 0,
        "expected misses for changed dependency queries"
    );

    let _ = fs::remove_dir_all(&temp_dir);
}

#[test]
fn diagnostics_for_program_file_respects_cancellation_during_overlay_churn() {
    let run_id = ANALYSIS_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_analysis_program_cancel_{}_{}",
        std::process::id(),
        run_id
    ));
    fs::create_dir_all(temp_dir.join("app")).expect("failed to create temp module tree");

    let entry_path = temp_dir.join("main.mc");
    let dep_path = temp_dir.join("app").join("dep.mc");
    let entry_source = r#"
requires {
    app::dep as dep
}

fn main() -> u64 {
    0
}
"#;
    let dep_source = r#"
@[public]
fn value() -> u64 {
    1
}
"#;

    fs::write(&entry_path, entry_source).expect("failed to write entry source");
    fs::write(&dep_path, dep_source).expect("failed to write dependency source");

    let token = CancellationToken::new();
    let mut db = AnalysisDb::with_cancellation_token(token.clone());
    let entry_id = db.upsert_disk_text(entry_path.clone(), entry_source);
    let dep_id = db.upsert_disk_text(dep_path.clone(), dep_source);

    db.set_overlay(dep_id, "fn value( {");
    db.set_overlay(dep_id, dep_source);
    token.cancel();

    let result = db.diagnostics_for_program_file(entry_id);
    assert_eq!(
        result,
        Err(QueryCancelled),
        "expected cancellation to abort program diagnostics under rapid overlay edits"
    );

    let _ = fs::remove_dir_all(&temp_dir);
}

#[test]
fn diagnostics_for_program_file_resolves_public_symbol_imports() {
    let run_id = ANALYSIS_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_analysis_program_symbol_import_{}_{}",
        std::process::id(),
        run_id
    ));
    let app_dir = temp_dir.join("app");
    fs::create_dir_all(&app_dir).expect("failed to create temp module tree");

    let entry_path = temp_dir.join("main.mc");
    let dep_path = app_dir.join("dep.mc");
    let entry_source = r#"
requires {
    app::dep::run
}

fn main() -> u64 {
    run()
}
"#;
    let dep_source = r#"
@[public]
fn run() -> u64 { 1 }
"#;

    fs::write(&entry_path, entry_source).expect("failed to write entry source");
    fs::write(&dep_path, dep_source).expect("failed to write dependency source");

    let mut db = AnalysisDb::new();
    let entry_id = db.upsert_disk_text(entry_path.clone(), entry_source);
    db.upsert_disk_text(dep_path.clone(), dep_source);

    let diagnostics = db
        .diagnostics_for_program_file(entry_id)
        .expect("program diagnostics query should succeed");
    assert!(
        diagnostics.is_empty(),
        "expected symbol import to resolve without diagnostics, got: {diagnostics:#?}"
    );

    let _ = fs::remove_dir_all(&temp_dir);
}

#[test]
fn diagnostics_for_program_file_typechecks_symbol_import_calls() {
    let run_id = ANALYSIS_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_analysis_program_symbol_import_typecheck_{}_{}",
        std::process::id(),
        run_id
    ));
    let app_dir = temp_dir.join("app");
    fs::create_dir_all(&app_dir).expect("failed to create temp module tree");

    let entry_path = temp_dir.join("main.mc");
    let dep_path = app_dir.join("dep.mc");
    let entry_source = r#"
requires {
    app::dep::run
}

fn main() -> u64 {
    run(true)
}
"#;
    let dep_source = r#"
@[public]
fn run(x: u64) -> u64 { x }
"#;

    fs::write(&entry_path, entry_source).expect("failed to write entry source");
    fs::write(&dep_path, dep_source).expect("failed to write dependency source");

    let mut db = AnalysisDb::new();
    let entry_id = db.upsert_disk_text(entry_path.clone(), entry_source);
    db.upsert_disk_text(dep_path.clone(), dep_source);

    let diagnostics = db
        .diagnostics_for_program_file(entry_id)
        .expect("program diagnostics query should succeed");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.phase == DiagnosticPhase::Typecheck && d.message.contains("arg 1")),
        "expected imported callable signature to typecheck calls, got: {diagnostics:#?}"
    );
    assert!(
        diagnostics
            .iter()
            .all(|d| d.phase != DiagnosticPhase::Resolve),
        "expected symbol import to resolve before typecheck, got: {diagnostics:#?}"
    );

    let _ = fs::remove_dir_all(&temp_dir);
}

#[test]
fn diagnostics_for_program_file_typechecks_symbol_import_types() {
    let run_id = ANALYSIS_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_analysis_program_symbol_import_type_{}_{}",
        std::process::id(),
        run_id
    ));
    let app_dir = temp_dir.join("app");
    fs::create_dir_all(&app_dir).expect("failed to create temp module tree");

    let entry_path = temp_dir.join("main.mc");
    let dep_path = app_dir.join("dep.mc");
    let entry_source = r#"
requires {
    app::dep::Num
}

fn main() -> u64 {
    let x: Num = true;
    true
}
"#;
    let dep_source = r#"
@[public]
type Num = bool
"#;

    fs::write(&entry_path, entry_source).expect("failed to write entry source");
    fs::write(&dep_path, dep_source).expect("failed to write dependency source");

    let mut db = AnalysisDb::new();
    let entry_id = db.upsert_disk_text(entry_path.clone(), entry_source);
    db.upsert_disk_text(dep_path.clone(), dep_source);

    let diagnostics = db
        .diagnostics_for_program_file(entry_id)
        .expect("program diagnostics query should succeed");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.phase == DiagnosticPhase::Typecheck),
        "expected imported public type alias to participate in typecheck, got: {diagnostics:#?}"
    );
    assert!(
        diagnostics
            .iter()
            .all(|d| d.phase != DiagnosticPhase::Resolve),
        "expected symbol import to resolve before typecheck, got: {diagnostics:#?}"
    );

    let _ = fs::remove_dir_all(&temp_dir);
}

#[test]
fn diagnostics_for_program_file_typechecks_symbol_import_traits() {
    let run_id = ANALYSIS_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_analysis_program_symbol_import_trait_{}_{}",
        std::process::id(),
        run_id
    ));
    let app_dir = temp_dir.join("app");
    fs::create_dir_all(&app_dir).expect("failed to create temp module tree");

    let entry_path = temp_dir.join("main.mc");
    let dep_path = app_dir.join("dep.mc");
    let entry_source = r#"
requires {
    app::dep::Runnable
}

type Process = {
    id: u64,
}

Process :: Runnable {
    fn run(self) -> u64 {
        self.id
    }
}

fn accept<T: Runnable>(value: T) -> u64 {
    value.run()
}

fn main() -> u64 {
    accept(Process { id: 7 })
}
"#;
    let dep_source = r#"
@[public]
trait Runnable {
    fn run(self) -> u64;
}
"#;

    fs::write(&entry_path, entry_source).expect("failed to write entry source");
    fs::write(&dep_path, dep_source).expect("failed to write dependency source");

    let mut db = AnalysisDb::new();
    let entry_id = db.upsert_disk_text(entry_path.clone(), entry_source);
    db.upsert_disk_text(dep_path.clone(), dep_source);

    let diagnostics = db
        .diagnostics_for_program_file(entry_id)
        .expect("program diagnostics query should succeed");
    assert!(
        diagnostics.is_empty(),
        "expected imported public trait to resolve and typecheck, got: {diagnostics:#?}"
    );

    let _ = fs::remove_dir_all(&temp_dir);
}

fn span_for_substring(source: &str, needle: &str) -> Span {
    let start = source
        .find(needle)
        .expect("needle should exist in source for span helper");
    let end = start + needle.len();
    let start_pos = position_at(source, start);
    let end_pos = position_at(source, end);
    Span {
        start: start_pos,
        end: end_pos,
    }
}

fn span_for_last_substring(source: &str, needle: &str) -> Span {
    let start = source
        .rfind(needle)
        .expect("needle should exist in source for span helper");
    let end = start + needle.len();
    let start_pos = position_at(source, start);
    let end_pos = position_at(source, end);
    Span {
        start: start_pos,
        end: end_pos,
    }
}

fn cursor_after_substring(source: &str, needle: &str) -> Span {
    let start = source
        .find(needle)
        .expect("needle should exist in source for cursor helper");
    let offset = start + needle.len();
    let pos = position_at(source, offset);
    Span {
        start: pos,
        end: pos,
    }
}

fn position_at(source: &str, offset: usize) -> Position {
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
