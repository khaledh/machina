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
fn test_machine_runtime_pending_cleanup_and_metrics_hooks() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_pending_cleanup.c");

    let run = run_c_program("machine_runtime_pending_cleanup", &source_path);
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
fn test_machine_runtime_managed_bootstrap_and_shutdown_bridge() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let source_path = repo_root
        .join("runtime")
        .join("tests")
        .join("machine_runtime_managed.c");

    let run = run_c_program("machine_runtime_managed", &source_path);
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
    std::machine::managed_runtime
    std::machine::step
    std::machine::Runtime
    std::machine::StepStatus
}

type Ping = {}

typestate M {
    fn new() -> S {
        S {}
    }

    state S {
        on Ping(e) -> stay {
            e;
            println("handled");
        }
    }
}

@[machines]
fn main() -> u64 {
    match M::spawn() {
        m: Machine<M> => {
            match m.send(1, 0, 0) {
                ok: () => { ok; }
                _ => { return 1; },
            };
        }
        _ => { return 1; },
    };
    let rt: Runtime = match managed_runtime() {
        r: Runtime => r,
        _ => { return 1; },
    };

    match step(rt) {
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
fn test_typestate_machine_runtime_two_machine_request_reply_with_labeled_provenance() {
    let source = r#"
requires {
    std::io::println
    std::machine::managed_runtime
    std::machine::step
    std::machine::Runtime
    std::machine::StepStatus
}

type KickApprove = {}
type KickDeny = {}
type AuthCheck = {}
type AuthReply = {}

typestate AuthClient {
    fn new() -> Idle {
        Idle {}
    }

    state Idle {
        on KickApprove(e) -> stay {
            e;
            let p: Pending<AuthReply> =
                request:approve(1, AuthCheck {});
            p;
        }

        on KickDeny(e) -> stay {
            e;
            let p: Pending<AuthReply> =
                request:deny(1, AuthCheck {});
            p;
        }

        on AuthReply(resp) for AuthCheck:approve(req) -> stay {
            req;
            resp;
            println("approve path");
            println("approved");
        }

        on AuthReply(resp) for AuthCheck:deny(req) -> stay {
            req;
            resp;
            println("deny path");
            println("denied");
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on AuthCheck(req: AuthCheck, cap: ReplyCap<AuthReply>) -> stay {
            req;
            reply(cap, AuthReply {});
        }
    }
}

@[machines]
fn main() -> u64 {
    // Spawn server first so its machine id is 1 (used by client Request `to:`).
    match AuthServer::spawn() {
        m: Machine<AuthServer> => { m; }
        _ => { return 1; },
    };
    match AuthClient::spawn() {
        client: Machine<AuthClient> => {
            // Queue two same-type requests from distinct labeled request sites.
            match client.send(1, 0, 0) {
                ok: () => { ok; }
                _ => { return 1; },
            };
            match client.send(2, 0, 0) {
                ok: () => { ok; }
                _ => { return 1; },
            };
        }
        _ => { return 1; },
    };
    let rt: Runtime = match managed_runtime() {
        r: Runtime => r,
        _ => { return 1; },
    };

    // Drain a bounded number of dispatch steps.
    match step(rt) { StepStatus::Faulted => { return 1; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return 1; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return 1; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return 1; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return 1; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return 1; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return 1; } _ => {} };
    match step(rt) { StepStatus::Faulted => { return 1; } _ => {} };
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
        stdout.contains("approve path")
            && stdout.contains("approved")
            && stdout.contains("deny path")
            && stdout.contains("denied"),
        "expected deterministic labeled routing outcomes in stdout, got: {stdout}"
    );
}

#[test]
fn test_typestate_spawn_forwards_constructor_args_under_managed_runtime() {
    let source = r#"
requires {
    std::io::println
    std::machine::managed_runtime
    std::machine::step
    std::machine::Runtime
    std::machine::StepStatus
}

type Ping = {}

typestate Worker {
    fn new(seed: u64) -> Idle {
        if seed == 42 {
            println("new-42");
        } else {
            println("new-other");
        };
        Idle {}
    }

    state Idle {
        on Ping(e) -> stay {
            e;
            println("handled");
        }
    }
}

@[machines]
fn main() -> u64 {
    match Worker::spawn(42) {
        m: Machine<Worker> => {
            match m.send(1, 0, 0) {
                _ok: () => {}
                _ => { return 1; }
            };
        }
        _ => { return 1; }
    };

    let rt: Runtime = match managed_runtime() {
        r: Runtime => r,
        _ => { return 1; },
    };

    match step(rt) {
        StepStatus::DidWork => 0,
        _ => 1,
    }
}
"#;

    let run = run_program_with_opts(
        "typestate_spawn_constructor_args",
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
        stdout.contains("new-42") && stdout.contains("handled"),
        "expected constructor argument forwarding + managed handler execution, got: {stdout}"
    );
}
