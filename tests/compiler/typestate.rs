use crate::common::run_program_with_opts;
use machina::core::capsule::CapsuleError;
use machina::core::diag::CompileError;
use machina::core::parse::ParseError;
use machina::core::resolve::ResolveError;
use machina::core::typecheck::TypeCheckErrorKind;
use machina::driver::compile::{CompileOptions, check_with_path, compile_with_path};
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn typestate_opts(experimental_typestate: bool) -> CompileOptions {
    CompileOptions {
        dump: None,
        emit_ir: false,
        verify_ir: false,
        trace_alloc: false,
        trace_drops: false,
        inject_prelude: true,
        experimental_typestate,
    }
}

fn compile_example(path: &Path, experimental_typestate: bool) -> Result<(), Vec<CompileError>> {
    let source = std::fs::read_to_string(path).expect("failed to read example source");
    compile_with_path(&source, Some(path), &typestate_opts(experimental_typestate)).map(|_| ())
}

fn check_example(path: &Path, experimental_typestate: bool) -> Result<(), Vec<CompileError>> {
    let source = std::fs::read_to_string(path).expect("failed to read example source");
    check_with_path(&source, path, true, experimental_typestate)
}

fn check_source(
    source: &str,
    virtual_path: &str,
    experimental_typestate: bool,
) -> Result<(), Vec<CompileError>> {
    check_with_path(
        source,
        &repo_root().join(virtual_path),
        true,
        experimental_typestate,
    )
}

fn valid_examples() -> Vec<PathBuf> {
    let root = repo_root();
    vec![
        root.join("tests/fixtures/typestate/connection.mc"),
        root.join("tests/fixtures/typestate/file_handle.mc"),
        root.join("tests/fixtures/typestate/job.mc"),
        root.join("tests/fixtures/typestate/request_builder.mc"),
        root.join("tests/fixtures/typestate/service_lifecycle.mc"),
    ]
}

fn managed_check_examples() -> Vec<PathBuf> {
    let root = repo_root();
    vec![
        root.join("tests/fixtures/typestate/managed_state_transitions.mc"),
        root.join("tests/fixtures/typestate/machine_events.mc"),
        root.join("tests/fixtures/typestate/inter_machine_request_reply.mc"),
        root.join("tests/fixtures/typestate/typed_handle_request_reply.mc"),
        root.join("tests/fixtures/typestate/final_state_machine.mc"),
    ]
}

fn invalid_example_cases() -> Vec<(PathBuf, fn(&ResolveError) -> bool)> {
    let root = repo_root();
    vec![
        (
            root.join("tests/fixtures/typestate/connection_invalid.mc"),
            |err| matches!(err, ResolveError::TypestateStateLiteralOutsideTypestate(..)),
        ),
        (
            root.join("tests/fixtures/typestate/file_handle_invalid.mc"),
            |err| matches!(err, ResolveError::TypestateInvalidTransitionReturn(..)),
        ),
        (
            root.join("tests/fixtures/typestate/job_invalid.mc"),
            |err| matches!(err, ResolveError::TypestateMissingNew(..)),
        ),
        (
            root.join("tests/fixtures/typestate/request_builder_invalid.mc"),
            |err| {
                matches!(
                    err,
                    ResolveError::TypestateStateFieldShadowsCarriedField(..)
                )
            },
        ),
        (
            root.join("tests/fixtures/typestate/service_lifecycle_invalid.mc"),
            |err| matches!(err, ResolveError::TypestateDuplicateTransition(..)),
        ),
    ]
}

#[test]
fn typestate_examples_compile_with_experimental_flag() {
    for path in valid_examples() {
        compile_example(&path, true)
            .unwrap_or_else(|errs| panic!("expected success for {}: {errs:?}", path.display()));
    }
}

#[test]
fn typestate_examples_are_rejected_without_experimental_flag() {
    for path in valid_examples() {
        let errors = compile_example(&path, false)
            .expect_err("typestate examples should fail without experimental flag");
        assert!(errors.iter().any(|err| {
            matches!(
                err,
                CompileError::Parse(ParseError::FeatureDisabled { feature, .. })
                    if *feature == "typestate"
            ) || matches!(
                err,
                CompileError::Capsule(CapsuleError::Parse {
                    error: ParseError::FeatureDisabled { feature, .. },
                    ..
                }) if *feature == "typestate"
            )
        }));
    }
}

#[test]
fn typestate_invalid_examples_emit_expected_diagnostics() {
    for (path, predicate) in invalid_example_cases() {
        let errors = compile_example(&path, true)
            .expect_err("invalid typestate example should fail in experimental mode");
        assert!(errors.iter().any(|err| {
            matches!(err, CompileError::Resolve(resolve_err) if predicate(resolve_err))
        }));
    }
}

#[test]
fn typestate_example_runs_in_experimental_mode() {
    let path = repo_root().join("tests/fixtures/typestate/connection.mc");
    let source = std::fs::read_to_string(&path).expect("failed to read typestate runtime fixture");
    let run = run_program_with_opts("typestate_connection", &source, typestate_opts(true));
    assert_eq!(run.status.code(), Some(0));
}

#[test]
fn typestate_managed_examples_typecheck_with_experimental_flag() {
    for path in managed_check_examples() {
        check_example(&path, true)
            .unwrap_or_else(|errs| panic!("expected success for {}: {errs:?}", path.display()));
    }
}

#[test]
fn typestate_machine_handle_request_example_runs_in_experimental_mode() {
    let path = repo_root().join("tests/fixtures/typestate/typed_handle_request_reply.mc");
    let source = std::fs::read_to_string(&path)
        .expect("failed to read typestate machine-handle request fixture");
    let run = run_program_with_opts(
        "typestate_machine_handle_request",
        &source,
        typestate_opts(true),
    );
    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("got 42"),
        "expected typed handle request/reply output, got: {stdout}"
    );
}

#[test]
fn typestate_managed_state_transitions_example_runs_in_experimental_mode() {
    let path = repo_root().join("tests/fixtures/typestate/managed_state_transitions.mc");
    let source = std::fs::read_to_string(&path)
        .expect("failed to read typestate managed state-transition fixture");
    let run = run_program_with_opts(
        "typestate_managed_state_transitions",
        &source,
        typestate_opts(true),
    );
    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("door transitioned: Closed -> Waiting -> Open"),
        "expected managed state-transition output, got: {stdout}"
    );
}

#[test]
fn typestate_cross_machine_typed_handle_request_runs_in_experimental_mode() {
    let source = r#"
requires {
    std::io::println
}

type AuthCheck = { token: u64 }
type AuthReply = { accepted: u64 }

typestate AuthServer {
    fn new() -> Ready { Ready {} }

    state Ready {
        on AuthCheck(req: AuthCheck, cap: ReplyCap<AuthReply>) -> stay {
            println(f"server {req.token}");
            cap.reply(AuthReply { accepted: req.token + 1 });
        }
    }
}

typestate Client {
    fn new() -> Ready { Ready {} }

    state Ready {
        on AuthReply(resp) for AuthCheck(req) -> stay {
            println(f"client {resp.accepted}");
            req;
        }
    }
}

@machines
fn main() -> () | MachineError {
    let auth = AuthServer::spawn()?;
    let client = Client::spawn()?;
    let p: Pending<AuthReply> = client.request(auth, AuthCheck { token: 41 })?;
    p;
}
"#;

    let run = run_program_with_opts(
        "typestate_cross_machine_typed_request",
        source,
        typestate_opts(true),
    );
    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("server 41"),
        "expected server handler output, got: {stdout}"
    );
    assert!(
        stdout.contains("client 42"),
        "expected client response handler output, got: {stdout}"
    );
}

#[test]
fn typestate_managed_examples_are_rejected_without_experimental_flag() {
    for path in managed_check_examples() {
        let errors = check_example(&path, false)
            .expect_err("typestate managed examples should fail without experimental flag");
        assert!(errors.iter().any(|err| {
            matches!(
                err,
                CompileError::Parse(ParseError::FeatureDisabled { feature, .. })
                    if *feature == "typestate"
            ) || matches!(
                err,
                CompileError::Capsule(CapsuleError::Parse {
                    error: ParseError::FeatureDisabled { feature, .. },
                    ..
                }) if *feature == "typestate"
            )
        }));
    }
}

#[test]
fn typestate_example_lists_cover_all_typestate_fixtures() {
    use std::collections::HashSet;
    let root = repo_root();
    let fixture_dir = root.join("tests/fixtures/typestate");
    let disk: HashSet<PathBuf> = std::fs::read_dir(&fixture_dir)
        .expect("failed to read typestate fixture directory")
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(|path| path.extension().and_then(|e| e.to_str()) == Some("mc"))
        .collect();

    let mut covered: HashSet<PathBuf> = HashSet::new();
    covered.extend(valid_examples());
    covered.extend(managed_check_examples());
    covered.extend(invalid_example_cases().into_iter().map(|(path, _)| path));

    assert_eq!(
        disk, covered,
        "typestate fixtures and test coverage lists are out of sync"
    );
}

#[test]
fn typestate_managed_ambiguous_provenance_is_rejected() {
    let source = r#"
type Kick = {}
type AuthCheck = {}
type AuthApproved = {}

typestate Client {
    fn new() -> Ready { Ready {} }

    state Ready {
        on Kick(k) -> stay {
            k;
            let pending: Pending<AuthApproved> = request(0, AuthCheck {});
            pending;
        }

        on AuthApproved(ok) for AuthCheck(req) -> stay {
            ok;
            req;
        }

        on AuthApproved(ok) for AuthCheck:auth2(req) -> stay {
            ok;
            req;
        }
    }
}
"#;

    let errors = check_source(
        source,
        "tests/fixtures/typestate/managed_ambiguous_provenance.mc",
        true,
    )
    .expect_err("expected ambiguous provenance diagnostic");
    assert!(
        errors.iter().any(|err| {
            matches!(
                err,
                CompileError::TypeCheck(type_err)
                    if matches!(
                        type_err.kind(),
                        TypeCheckErrorKind::TypestateAmbiguousResponseProvenance(..)
                    )
            )
        }),
        "expected MC-TYPECHECK-TypestateAmbiguousResponseProvenance, got: {errors:?}"
    );
}

#[test]
fn typestate_managed_labeled_provenance_is_accepted() {
    let source = r#"
type Kick = {}
type AuthCheck = {}
type AuthApproved = {}

typestate Client {
    fn new() -> Ready { Ready {} }

    state Ready {
        on Kick(k) -> stay {
            k;
            let p1: Pending<AuthApproved> = request:auth1(0, AuthCheck {});
            let p2: Pending<AuthApproved> = request:auth2(0, AuthCheck {});
            p1;
            p2;
        }

        on AuthApproved(ok) for AuthCheck:auth1(req) -> stay {
            ok;
            req;
        }

        on AuthApproved(ok) for AuthCheck:auth2(req) -> stay {
            ok;
            req;
        }
    }
}
"#;

    check_source(
        source,
        "tests/fixtures/typestate/managed_labeled_provenance.mc",
        true,
    )
    .unwrap_or_else(|errs| panic!("expected success for labeled provenance source: {errs:?}"));
}
