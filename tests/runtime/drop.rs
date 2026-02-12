use std::collections::HashMap;
use std::path::PathBuf;

use machina::driver::compile::CompileOptions;

use crate::common::run_program_with_opts;

fn trace_opts() -> CompileOptions {
    CompileOptions {
        dump: None,
        emit_ir: false,
        verify_ir: false,
        trace_alloc: true,
        trace_drops: false,
        inject_prelude: true,
    }
}

fn load_fixture(name: &str) -> String {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = repo_root
        .join("tests")
        .join("fixtures")
        .join("runtime")
        .join("drop")
        .join(format!("{name}.mc"));
    std::fs::read_to_string(&path).expect("failed to read runtime drop fixture")
}

fn parse_trace(output: &std::process::Output) -> (HashMap<String, usize>, HashMap<String, usize>) {
    let mut allocs: HashMap<String, usize> = HashMap::new();
    let mut frees: HashMap<String, usize> = HashMap::new();

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    for line in stdout.lines().chain(stderr.lines()) {
        if !line.contains("[mc]") {
            continue;
        }
        let is_alloc = line.contains("alloc");
        let is_free = line.contains("free");
        if !is_alloc && !is_free {
            continue;
        }
        let Some(ptr) = line
            .split("ptr=")
            .nth(1)
            .and_then(|rest| rest.split_whitespace().next())
        else {
            continue;
        };
        let entry = if is_alloc { &mut allocs } else { &mut frees };
        *entry.entry(ptr.to_string()).or_insert(0) += 1;
    }

    (allocs, frees)
}

fn assert_no_leak_or_double_free(output: &std::process::Output) {
    let (allocs, frees) = parse_trace(output);
    assert!(
        !allocs.is_empty(),
        "expected trace_alloc to record allocations"
    );

    for (ptr, count) in &frees {
        let alloc_count = allocs.get(ptr).copied().unwrap_or(0);
        assert!(alloc_count > 0, "free for unknown ptr {ptr}");
        assert_eq!(
            alloc_count, *count,
            "ptr {ptr} freed {count} times, allocated {alloc_count} times"
        );
    }

    for (ptr, count) in &allocs {
        let free_count = frees.get(ptr).copied().unwrap_or(0);
        assert_eq!(
            *count, free_count,
            "ptr {ptr} allocated {count} times, freed {free_count} times"
        );
    }
}

fn assert_fixture_no_leak(name: &str) {
    let source = load_fixture(name);
    let run = run_program_with_opts(name, &source, trace_opts());
    assert_eq!(run.status.code(), Some(0));
    assert_no_leak_or_double_free(&run);
}

#[test]
fn test_drop_trace_linked_list() {
    assert_fixture_no_leak("linked_list");
}

#[test]
fn test_drop_trace_sink_params() {
    assert_fixture_no_leak("sink_params");
}

#[test]
fn test_drop_trace_partial_init_drop() {
    assert_fixture_no_leak("partial_init_drop");
}

#[test]
fn test_drop_trace_string_append() {
    assert_fixture_no_leak("string_append");
}

#[test]
fn test_drop_trace_methods() {
    assert_fixture_no_leak("methods");
}

#[test]
fn test_drop_trace_heap_alloc() {
    assert_fixture_no_leak("heap_alloc");
}

#[test]
fn test_drop_trace_out_params() {
    assert_fixture_no_leak("out_params");
}

#[test]
fn test_drop_trace_inout_heap_arg() {
    assert_fixture_no_leak("inout_heap_arg");
}

#[test]
fn test_drop_trace_var_decl_drop() {
    assert_fixture_no_leak("var_decl_drop");
}

#[test]
fn test_drop_trace_drop_path() {
    assert_fixture_no_leak("drop_path");
}
