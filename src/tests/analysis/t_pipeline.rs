use std::sync::Arc;

use crate::analysis::pipeline::{ROOT_POISON_NODE, run_module_pipeline, to_lookup_state};
use crate::analysis::query::QueryRuntime;
use crate::frontend::ModuleId;

#[test]
fn pipeline_marks_parse_failure_as_poisoned() {
    let mut rt = QueryRuntime::new();
    let source = Arc::<str>::from("fn main( {");

    let state = run_module_pipeline(&mut rt, ModuleId(0), 1, source).expect("pipeline should run");

    assert!(!state.parsed.diagnostics.is_empty());
    assert!(state.parsed.product.is_none());
    assert!(state.parsed.poisoned_nodes.contains(&ROOT_POISON_NODE));

    let lookup = to_lookup_state(&state);
    assert!(lookup.poisoned_nodes.contains(&ROOT_POISON_NODE));
    assert!(lookup.resolved.is_none());
    assert!(lookup.typed.is_none());
}

#[test]
fn pipeline_marks_resolve_failure_as_poisoned() {
    let mut rt = QueryRuntime::new();
    let source = Arc::<str>::from("fn main() -> u64 { x }");

    let state = run_module_pipeline(&mut rt, ModuleId(1), 1, source).expect("pipeline should run");

    assert!(state.parsed.product.is_some());
    assert!(!state.resolved.diagnostics.is_empty());
    assert!(state.resolved.product.is_some());
    assert!(state.resolved.poisoned_nodes.contains(&ROOT_POISON_NODE));

    let lookup = to_lookup_state(&state);
    assert!(lookup.poisoned_nodes.contains(&ROOT_POISON_NODE));
    assert!(lookup.resolved.is_some());
    assert!(lookup.typed.is_none());
}

#[test]
fn pipeline_marks_typecheck_failure_as_poisoned() {
    let mut rt = QueryRuntime::new();
    let source = Arc::<str>::from("fn main() -> u64 { true }");

    let state = run_module_pipeline(&mut rt, ModuleId(2), 1, source).expect("pipeline should run");

    assert!(state.parsed.product.is_some());
    assert!(state.resolved.product.is_some());
    assert!(!state.typechecked.diagnostics.is_empty());
    assert!(state.typechecked.product.is_some());
    assert!(state.typechecked.poisoned_nodes.contains(&ROOT_POISON_NODE));

    let lookup = to_lookup_state(&state);
    assert!(lookup.poisoned_nodes.contains(&ROOT_POISON_NODE));
    assert!(lookup.resolved.is_some());
    assert!(lookup.typed.is_some());
}
