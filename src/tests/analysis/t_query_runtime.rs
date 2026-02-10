use crate::analysis::query::{
    CancellationToken, QueryCancelled, QueryKey, QueryKind, QueryRuntime,
};
use crate::frontend::ModuleId;

#[test]
fn query_runtime_memoizes_by_key() {
    let key = QueryKey::new(QueryKind::ParseModule, ModuleId(1), 42);
    let mut runtime = QueryRuntime::new();
    let mut calls = 0usize;

    let first = runtime
        .execute(key, |_rt| {
            calls += 1;
            Ok::<u64, QueryCancelled>(99)
        })
        .expect("first execute should succeed");
    let second = runtime
        .execute(key, |_rt| {
            calls += 1;
            Ok::<u64, QueryCancelled>(0)
        })
        .expect("second execute should hit cache");

    assert_eq!(first, 99);
    assert_eq!(second, 99);
    assert_eq!(calls, 1);

    let stats = runtime.cache_stats();
    assert_eq!(stats.misses, 1);
    assert_eq!(stats.hits, 1);
}

#[test]
fn query_runtime_tracks_direct_dependencies() {
    let root = QueryKey::new(QueryKind::Diagnostics, ModuleId(1), 1);
    let child = QueryKey::new(QueryKind::TypecheckModule, ModuleId(1), 1);
    let leaf = QueryKey::new(QueryKind::ResolveModule, ModuleId(1), 1);

    let mut runtime = QueryRuntime::new();
    let out = runtime.execute(root, |rt| {
        rt.execute(child, |rt| {
            rt.execute(leaf, |_rt| Ok::<u64, QueryCancelled>(7))
        })
    });

    assert_eq!(out, Ok(7));
    assert!(runtime.dependencies_of(root).contains(&child));
    assert!(runtime.dependencies_of(child).contains(&leaf));
    assert!(runtime.dependents_of(leaf).contains(&child));
    assert!(runtime.dependents_of(child).contains(&root));
}

#[test]
fn query_runtime_honors_cancellation() {
    let token = CancellationToken::new();
    token.cancel();

    let mut runtime = QueryRuntime::with_cancellation_token(token);
    let key = QueryKey::new(QueryKind::ParseModule, ModuleId(2), 1);
    let result = runtime.execute(key, |_rt| Ok::<u64, QueryCancelled>(1));
    assert_eq!(result, Err(QueryCancelled));
}

#[test]
fn query_runtime_invalidation_removes_dependents() {
    let root = QueryKey::new(QueryKind::Diagnostics, ModuleId(3), 2);
    let child = QueryKey::new(QueryKind::TypecheckModule, ModuleId(3), 2);
    let leaf = QueryKey::new(QueryKind::ResolveModule, ModuleId(3), 2);

    let mut runtime = QueryRuntime::new();
    runtime
        .execute(root, |rt| {
            rt.execute(child, |rt| {
                rt.execute(leaf, |_rt| Ok::<u64, QueryCancelled>(11))
            })
        })
        .expect("initial execution should succeed");
    runtime.clear_stats();

    runtime.invalidate(leaf);

    let mut calls = 0usize;
    let rerun = runtime
        .execute(root, |rt| {
            rt.execute(child, |rt| {
                rt.execute(leaf, |_rt| {
                    calls += 1;
                    Ok::<u64, QueryCancelled>(11)
                })
            })
        })
        .expect("recompute should succeed");

    assert_eq!(rerun, 11);
    assert_eq!(calls, 1);
    let stats = runtime.cache_stats();
    assert_eq!(stats.hits, 0);
    assert_eq!(stats.misses, 3);
}
