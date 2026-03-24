use crate::core::capsule::ModuleId;
use crate::services::analysis::query::{
    CancellationToken, QueryCancelled, QueryKey, QueryKind, QueryRuntime,
};
use crate::services::analysis::trace::{
    AnalysisTraceCategory, AnalysisTraceEvent, AnalysisTraceSink, AnalysisTracer,
};
use std::sync::{Arc, Mutex};

#[derive(Default)]
struct RecordingTraceSink {
    events: Mutex<Vec<AnalysisTraceEvent>>,
}

impl RecordingTraceSink {
    fn messages(&self) -> Vec<String> {
        self.events
            .lock()
            .expect("trace events should lock")
            .iter()
            .map(|event| format!("{}:{}", event.category, event.message))
            .collect()
    }
}

impl AnalysisTraceSink for RecordingTraceSink {
    fn emit(&self, event: &AnalysisTraceEvent) {
        self.events
            .lock()
            .expect("trace events should lock")
            .push(event.clone());
    }
}

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

#[test]
fn query_runtime_distinguishes_query_input_keys() {
    let base = QueryKey::new(QueryKind::LookupState, ModuleId(7), 9);
    let synthetic = QueryKey::with_input(QueryKind::LookupState, ModuleId(7), 9, 123);
    let mut runtime = QueryRuntime::new();
    let mut calls = 0usize;

    let a = runtime
        .execute(base, |_rt| {
            calls += 1;
            Ok::<u64, QueryCancelled>(10)
        })
        .expect("base execute should succeed");
    let b = runtime
        .execute(synthetic, |_rt| {
            calls += 1;
            Ok::<u64, QueryCancelled>(20)
        })
        .expect("synthetic execute should succeed");
    let a_cached = runtime
        .execute(base, |_rt| Ok::<u64, QueryCancelled>(0))
        .expect("base should hit cache");
    let b_cached = runtime
        .execute(synthetic, |_rt| Ok::<u64, QueryCancelled>(0))
        .expect("synthetic should hit cache");

    assert_eq!(a, 10);
    assert_eq!(b, 20);
    assert_eq!(a_cached, 10);
    assert_eq!(b_cached, 20);
    assert_eq!(calls, 2);
}

#[test]
fn query_runtime_emits_trace_events_when_enabled() {
    let key = QueryKey::new(QueryKind::ParseModule, ModuleId(9), 3);
    let sink = Arc::new(RecordingTraceSink::default());
    let tracer = AnalysisTracer::with_sink([AnalysisTraceCategory::Query], sink.clone());
    let mut runtime = QueryRuntime::with_tracer(tracer);

    runtime
        .execute(key, |_rt| Ok::<u64, QueryCancelled>(7))
        .expect("first execute should succeed");
    runtime
        .execute(key, |_rt| Ok::<u64, QueryCancelled>(8))
        .expect("cached execute should succeed");
    runtime.invalidate(key);

    let messages = sink.messages();
    assert!(
        messages
            .iter()
            .any(|message| message.contains("query:execute start")),
        "expected execute start trace, got: {messages:#?}"
    );
    assert!(
        messages
            .iter()
            .any(|message| message.contains("query:cache miss")),
        "expected cache miss trace, got: {messages:#?}"
    );
    assert!(
        messages
            .iter()
            .any(|message| message.contains("query:cache hit")),
        "expected cache hit trace, got: {messages:#?}"
    );
    assert!(
        messages
            .iter()
            .any(|message| message.contains("query:invalidate root")),
        "expected invalidate trace, got: {messages:#?}"
    );
}
