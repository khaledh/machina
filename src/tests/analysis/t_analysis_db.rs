use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use crate::analysis::db::AnalysisDb;
use crate::analysis::module_graph::ModuleGraph;
use crate::analysis::query::{QueryCancelled, QueryKey, QueryKind};
use crate::frontend::ModuleId;

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
