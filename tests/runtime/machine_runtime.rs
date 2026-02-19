use std::path::PathBuf;
use std::process::Output;

use machina::driver::compile::CompileOptions;

use crate::common::{run_c_program, run_program, run_program_with_opts};

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

fn typestate_compile_opts() -> CompileOptions {
    CompileOptions {
        dump: None,
        emit_ir: false,
        verify_ir: false,
        trace_alloc: false,
        trace_drops: false,
        inject_prelude: true,
        experimental_typestate: true,
    }
}

fn run_typestate_program(name: &str, source: &str) -> Output {
    run_program_with_opts(name, source, typestate_compile_opts())
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

@machines
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

    let run = run_typestate_program("typestate_machine_runtime_exec", source);
    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("handled"),
        "expected handler output in runtime execution, got stdout: {stdout}"
    );
}

#[test]
fn test_typestate_machines_entrypoint_auto_drives_runtime_without_plumbing() {
    let source = r#"
requires {
    std::io::println
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

@machines
fn main() {
    match M::spawn() {
        m: Machine<M> => {
            match m.send(1, 0, 0) {
                ok: () => { ok; }
                _ => { return; },
            };
        }
        _ => { return; },
    };
}
"#;

    let run = run_typestate_program("typestate_machine_runtime_auto_drive", source);
    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("handled"),
        "expected handler output via auto-driven managed runtime, got stdout: {stdout}"
    );
}

#[test]
fn test_typestate_machine_handle_typed_send_with_non_empty_payload_executes_handler() {
    let source = r#"
requires {
    std::io::println
}

type Ping = { id: u64 }

typestate M {
    fn new() -> S {
        S {}
    }

    state S {
        on Ping(p) -> stay {
            println(f"handled {p.id}");
        }
    }
}

@machines
fn main() -> () | MachineError {
    let m = M::spawn()?;
    m.send(Ping { id: 7 })?;
}
"#;

    let run = run_typestate_program("typestate_machine_runtime_typed_send_payload", source);
    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("handled 7"),
        "expected typed payload handler output, got stdout: {stdout}"
    );
}

#[test]
fn test_typestate_final_state_stops_machine_and_rejects_future_send() {
    let source = r#"
requires {
    std::machine::managed_runtime
    std::machine::step
    std::machine::Runtime
    std::machine::StepStatus
}

type Start = {}

typestate OneShot {
    fn new() -> Idle {
        Idle {}
    }

    state Idle {
        on Start(s) -> Done {
            s;
            Done {}
        }
    }

    @final
    state Done {}
}

@machines
fn main() -> u64 {
    let machine: Machine<OneShot> = match OneShot::spawn() {
        m: Machine<OneShot> => m,
        _ => { return 1; },
    };

    match machine.send(1, 0, 0) {
        _ok: () => {}
        _ => { return 1; },
    };

    let rt: Runtime = match managed_runtime() {
        r: Runtime => r,
        _ => { return 1; },
    };

    match step(rt) {
        StepStatus::DidWork => {}
        _ => { return 1; },
    };

    var stopped = false;
    match machine.send(1, 0, 0) {
        _ok: () => {
            stopped = false;
        }
        err: MachineError => {
            match err {
                MachineError::NotRunning => {
                    stopped = true;
                }
                _ => {
                    stopped = false;
                }
            };
        }
    };
    if stopped {
        0
    } else {
        1
    }
}
"#;

    let run = run_typestate_program("typestate_final_state_stop", source);
    assert_eq!(run.status.code(), Some(0));
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

@machines
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

    let run = run_typestate_program("typestate_machine_runtime_reqreply", source);
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

@machines
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

    let run = run_typestate_program("typestate_spawn_constructor_args", source);
    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("new-42") && stdout.contains("handled"),
        "expected constructor argument forwarding + managed handler execution, got: {stdout}"
    );
}
