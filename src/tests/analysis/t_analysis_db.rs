use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use crate::analysis::db::AnalysisDb;
use crate::analysis::diagnostics::DiagnosticPhase;
use crate::analysis::module_graph::ModuleGraph;
use crate::analysis::query::{CancellationToken, QueryCancelled, QueryKey, QueryKind};
use crate::diag::{Position, Span};
use crate::frontend::ModuleId;
use crate::types::Type;

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
