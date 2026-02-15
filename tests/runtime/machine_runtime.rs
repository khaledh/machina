use std::path::PathBuf;

use machina::driver::compile::CompileOptions;

use crate::common::{run_c_program, run_program, run_program_with_opts};

#[test]
fn test_machine_runtime_core() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_basic.c");

    let run = run_c_program("machine_runtime_basic", &source_path);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_machine_runtime_transactional_commit_rollback() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_txn.c");

    let run = run_c_program("machine_runtime_txn", &source_path);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_machine_runtime_request_reply_transport() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_reqreply.c");

    let run = run_c_program("machine_runtime_reqreply", &source_path);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_machine_runtime_transactional_request_reply_staging() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_txn_reqreply.c");

    let run = run_c_program("machine_runtime_txn_reqreply", &source_path);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_machine_runtime_emit_shims_stage_transactional_effects() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_emit_staging.c");

    let run = run_c_program("machine_runtime_emit_staging", &source_path);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_machine_runtime_handle_bridge_helpers() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_handle_bridge.c");

    let run = run_c_program("machine_runtime_handle_bridge", &source_path);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_machine_runtime_bound_dispatch_step() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_bound_dispatch.c");

    let run = run_c_program("machine_runtime_bound_dispatch", &source_path);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_machine_runtime_bootstrap_hook_called_once() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_bootstrap_hook.c");

    let run = run_c_program("machine_runtime_bootstrap_hook", &source_path);
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn test_machine_runtime_descriptor_dispatch_selection() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_descriptor_dispatch.c");

    let run = run_c_program("machine_runtime_descriptor_dispatch", &source_path);
    assert_eq!(run.status.code(), Some(0));
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
    };

    match start(rt, id) {
        _ok: () => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match send(rt, id, 1, 0, 0) {
        _ok: () => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
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

#[test]
fn test_typestate_machine_runtime_descriptor_bootstrap_executes_handler() {
    let source = r#"
requires {
    std::io::println
    std::machine::new_runtime
    std::machine::close_runtime
    std::machine::spawn
    std::machine::start
    std::machine::send
    std::machine::bind_descriptor
    std::machine::step
    std::machine::StepStatus
}

type Ping = {}

typestate M {
    fn new() -> S {
        S {}
    }

    state S {
        on Ping(e: Ping) -> S {
            e;
            println("handled");
            S {}
        }
    }
}

fn main() -> u64 {
    var rt = new_runtime();
    var id = 0;
    match spawn(rt, 8) {
        machine_id: u64 => {
            id = machine_id;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    // Descriptor id/state tag are deterministic for this single-typestate fixture.
    match bind_descriptor(rt, id, 1, 1) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match start(rt, id) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match send(rt, id, 1, 0, 0) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    let status = step(rt);
    close_runtime(inout rt);
    match status {
        StepStatus::DidWork => 0,
        _ => 1,
    }
}
"#;

    let run = run_program_with_opts(
        "typestate_machine_runtime_exec",
        source,
        CompileOptions {
            dump: None,
            emit_ir: false,
            verify_ir: false,
            trace_alloc: false,
            trace_drops: false,
            inject_prelude: true,
            experimental_typestate: true,
        },
    );
    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("handled"),
        "expected handler output in runtime execution, got stdout: {stdout}"
    );
}

#[test]
fn test_typestate_machine_runtime_two_machine_request_reply_approved_and_denied() {
    let source = r#"
requires {
    std::io::println
    std::machine::new_runtime
    std::machine::close_runtime
    std::machine::spawn
    std::machine::start
    std::machine::send
    std::machine::bind_descriptor
    std::machine::step
    std::machine::StepStatus
}

type KickApprove = {}
type KickDeny = {}
type ReqApprove = {}
type ReqDeny = {}
type Response = {}
type AuthApproved = {}
type AuthDenied = {}

typestate AuthClient {
    fn new() -> Idle {
        Idle {}
    }

    state Idle {
        on KickApprove(e: KickApprove) -> AwaitAuth {
            e;
            let pending: Pending<AuthApproved | AuthDenied> =
                emit Request(to: 1, ReqApprove {});
            AwaitAuth { pending: pending }
        }

        on KickDeny(e: KickDeny) -> AwaitAuth {
            e;
            let pending: Pending<AuthApproved | AuthDenied> =
                emit Request(to: 1, ReqDeny {});
            AwaitAuth { pending: pending }
        }
    }

    state AwaitAuth {
        fields {
            pending: Pending<AuthApproved | AuthDenied>,
        }

        on Response(pending, AuthApproved) -> Idle {
            println("approved");
            Idle {}
        }

        on Response(pending, AuthDenied) -> Idle {
            println("denied");
            Idle {}
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on ReqApprove(req: ReqApprove, cap: ReplyCap<AuthApproved | AuthDenied>) -> Ready {
            req;
            reply(cap, AuthApproved {});
            Ready {}
        }

        on ReqDeny(req: ReqDeny, cap: ReplyCap<AuthApproved | AuthDenied>) -> Ready {
            req;
            reply(cap, AuthDenied {});
            Ready {}
        }
    }
}

fn main() -> u64 {
    var rt = new_runtime();

    // Spawn server first so its machine id is 1 (used by client Request `to:`).
    var server_id = 0;
    match spawn(rt, 8) {
        id: u64 => {
            server_id = id;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    var client_id = 0;
    match spawn(rt, 8) {
        id: u64 => {
            client_id = id;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    // Descriptor ids are deterministic by typestate name sort:
    //   AuthClient -> 1, AuthServer -> 2.
    // State tags are deterministic by state name sort:
    //   AuthClient: AwaitAuth -> 1, Idle -> 2.
    //   AuthServer: Ready -> 1.
    match bind_descriptor(rt, server_id, 2, 1) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match bind_descriptor(rt, client_id, 1, 2) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    match start(rt, server_id) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match start(rt, client_id) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    // Kick approve flow (3 dispatches): client -> server -> client(response).
    match send(rt, client_id, 1, 0, 0) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    // Kick deny flow (same 3-dispatch pattern).
    match send(rt, client_id, 2, 0, 0) {
        ok: () => {
            ok;
        }
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };
    match step(rt) {
        StepStatus::DidWork => {}
        _ => {
            close_runtime(inout rt);
            return 1;
        }
    };

    close_runtime(inout rt);
    0
}
"#;

    let run = run_program_with_opts(
        "typestate_machine_runtime_reqreply",
        source,
        CompileOptions {
            dump: None,
            emit_ir: false,
            verify_ir: false,
            trace_alloc: false,
            trace_drops: false,
            inject_prelude: true,
            experimental_typestate: true,
        },
    );
    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("approved") && stdout.contains("denied"),
        "expected both request/reply outcomes in stdout, got: {stdout}"
    );
}
