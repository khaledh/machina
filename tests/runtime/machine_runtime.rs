use crate::common::{run_c_program, run_program};
use std::path::PathBuf;

fn runtime_c_fixture_path(file_name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("runtime")
        .join("tests")
        .join(file_name)
}

fn assert_runtime_c_fixture_ok(stem: &str) {
    let source_path = runtime_c_fixture_path(&format!("{stem}.c"));
    let run = run_c_program(stem, &source_path);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_machine_runtime_core() {
    assert_runtime_c_fixture_ok("machine_runtime_basic");
}

#[test]
fn test_machine_runtime_transactional_commit_rollback() {
    assert_runtime_c_fixture_ok("machine_runtime_txn");
}

#[test]
fn test_machine_runtime_request_reply_transport() {
    assert_runtime_c_fixture_ok("machine_runtime_reqreply");
}

#[test]
fn test_machine_runtime_transactional_request_reply_staging() {
    assert_runtime_c_fixture_ok("machine_runtime_txn_reqreply");
}

#[test]
fn test_machine_runtime_pending_cleanup_and_metrics_hooks() {
    assert_runtime_c_fixture_ok("machine_runtime_pending_cleanup");
}

#[test]
fn test_machine_runtime_payload_ownership_cleanup_paths() {
    assert_runtime_c_fixture_ok("machine_runtime_payload_ownership");
}

#[test]
fn test_machine_runtime_emit_shims_stage_transactional_effects() {
    assert_runtime_c_fixture_ok("machine_runtime_emit_staging");
}

#[test]
fn test_machine_runtime_handle_bridge_helpers() {
    assert_runtime_c_fixture_ok("machine_runtime_handle_bridge");
}

#[test]
fn test_machine_runtime_bound_dispatch_step() {
    assert_runtime_c_fixture_ok("machine_runtime_bound_dispatch");
}

#[test]
fn test_machine_runtime_stop_path_eager_cleanup() {
    assert_runtime_c_fixture_ok("machine_runtime_stop_cleanup");
}

#[test]
fn test_machine_runtime_bootstrap_hook_called_once() {
    assert_runtime_c_fixture_ok("machine_runtime_bootstrap_hook");
}

#[test]
fn test_machine_runtime_managed_bootstrap_and_shutdown_bridge() {
    assert_runtime_c_fixture_ok("machine_runtime_managed");
}

#[test]
fn test_machine_runtime_descriptor_dispatch_selection() {
    assert_runtime_c_fixture_ok("machine_runtime_descriptor_dispatch");
}

#[test]
fn test_machine_runtime_hosted_instance_table() {
    assert_runtime_c_fixture_ok("hosted_instance_basic");
}

#[test]
fn test_machine_runtime_hosted_linear_bridge() {
    assert_runtime_c_fixture_ok("machine_runtime_hosted_linear_bridge");
}

#[test]
fn test_std_machine_module_bridge_compiles_and_runs() {
    let source = r#"
requires {
    std::machine::new_runtime
    std::machine::close_runtime
    std::machine::spawn
    std::machine::start
    std::machine::send
    std::machine::step
    std::machine::StepStatus
}

fn main() -> u64 {
    var rt = new_runtime();
    var id = 0;
    match spawn(rt, 4) {
        machine_id: u64 => {
            id = machine_id;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    }

    match start(rt, id) {
        _ok: () => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    }
    match send(rt, id, 1, 0, 0) {
        _ok: () => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    }
    let stepped = match step(rt) {
        StepStatus::DidWork => true,
        _ => false,
    };
    close_runtime(inout rt);
    if stepped { 0 } else { 1 }
}
"#;

    let run = run_program("std_machine_bridge", source);
    assert_eq!(run.status.code(), Some(0));
}
