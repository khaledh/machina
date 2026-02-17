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
        root.join("examples/typestate/connection.mc"),
        root.join("examples/typestate/file_handle.mc"),
        root.join("examples/typestate/job.mc"),
        root.join("examples/typestate/request_builder.mc"),
        root.join("examples/typestate/service_lifecycle.mc"),
    ]
}

fn managed_check_examples() -> Vec<PathBuf> {
    let root = repo_root();
    vec![
        root.join("examples/typestate/machine_events_check.mc"),
        root.join("examples/typestate/inter_machine_req_reply_check.mc"),
        root.join("examples/typestate/machine_handle_request_check.mc"),
        root.join("examples/typestate/final_state_machine.mc"),
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
    let root = repo_root();
    let cases: Vec<(PathBuf, fn(&ResolveError) -> bool)> = vec![
        (
            root.join("examples/typestate/connection_invalid.mc"),
            |err| matches!(err, ResolveError::TypestateStateLiteralOutsideTypestate(..)),
        ),
        (
            root.join("examples/typestate/file_handle_invalid.mc"),
            |err| matches!(err, ResolveError::TypestateInvalidTransitionReturn(..)),
        ),
        (root.join("examples/typestate/job_invalid.mc"), |err| {
            matches!(err, ResolveError::TypestateMissingNew(..))
        }),
        (
            root.join("examples/typestate/request_builder_invalid.mc"),
            |err| {
                matches!(
                    err,
                    ResolveError::TypestateStateFieldShadowsCarriedField(..)
                )
            },
        ),
        (
            root.join("examples/typestate/service_lifecycle_invalid.mc"),
            |err| matches!(err, ResolveError::TypestateDuplicateTransition(..)),
        ),
    ];

    for (path, predicate) in cases {
        let errors = compile_example(&path, true)
            .expect_err("invalid typestate example should fail in experimental mode");
        assert!(errors.iter().any(|err| {
            matches!(err, CompileError::Resolve(resolve_err) if predicate(resolve_err))
        }));
    }
}

#[test]
fn typestate_example_runs_in_experimental_mode() {
    let path = repo_root().join("examples/typestate/connection.mc");
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
    let path = repo_root().join("examples/typestate/machine_handle_request_check.mc");
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
        "examples/typestate/managed_ambiguous_provenance.mc",
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
        "examples/typestate/managed_labeled_provenance.mc",
        true,
    )
    .unwrap_or_else(|errs| panic!("expected success for labeled provenance source: {errs:?}"));
}
