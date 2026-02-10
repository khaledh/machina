use std::collections::{HashMap, HashSet};

use crate::analysis::module_graph::ModuleGraph;
use crate::analysis::query::{QueryCancelled, QueryKey, QueryKind, QueryRuntime};
use crate::frontend::ModuleId;

#[test]
fn invalidation_closure_includes_transitive_dependents() {
    // A -> B -> C, D isolated
    let mut edges = HashMap::new();
    edges.insert(ModuleId(1), vec![ModuleId(2)]);
    edges.insert(ModuleId(2), vec![ModuleId(3)]);
    edges.insert(ModuleId(3), vec![]);
    edges.insert(ModuleId(4), vec![]);
    let graph = ModuleGraph::new(edges);

    let changed = HashSet::from([ModuleId(3)]);
    let closure = graph.invalidation_closure(&changed);
    assert!(closure.contains(&ModuleId(3)));
    assert!(closure.contains(&ModuleId(2)));
    assert!(closure.contains(&ModuleId(1)));
    assert!(!closure.contains(&ModuleId(4)));
}

#[test]
fn invalidate_modules_keeps_unaffected_module_cached() {
    let mut runtime = QueryRuntime::new();
    let rev = 1u64;

    // Represent A -> B -> C via nested query execution.
    runtime
        .execute(
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
        .expect("initial graph query should succeed");

    runtime
        .execute(
            QueryKey::new(QueryKind::Diagnostics, ModuleId(4), rev),
            |_rt| Ok::<u64, QueryCancelled>(9),
        )
        .expect("independent module query should succeed");

    runtime.clear_stats();
    runtime.invalidate_modules(&HashSet::from([ModuleId(3), ModuleId(2), ModuleId(1)]));

    let mut recomputed = 0usize;
    let a = runtime
        .execute(
            QueryKey::new(QueryKind::Diagnostics, ModuleId(1), rev),
            |rt| {
                rt.execute(
                    QueryKey::new(QueryKind::TypecheckModule, ModuleId(2), rev),
                    |rt| {
                        rt.execute(
                            QueryKey::new(QueryKind::ResolveModule, ModuleId(3), rev),
                            |_rt| {
                                recomputed += 1;
                                Ok::<u64, QueryCancelled>(7)
                            },
                        )
                    },
                )
            },
        )
        .expect("invalidated chain should recompute");
    let d = runtime
        .execute(
            QueryKey::new(QueryKind::Diagnostics, ModuleId(4), rev),
            |_rt| Ok::<u64, QueryCancelled>(0),
        )
        .expect("unaffected module should stay cached");

    assert_eq!(a, 7);
    assert_eq!(d, 9);
    assert_eq!(recomputed, 1);

    let stats = runtime.cache_stats();
    assert_eq!(stats.hits, 1, "module 4 should have remained cached");
}
