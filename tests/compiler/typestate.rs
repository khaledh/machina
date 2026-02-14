use crate::common::run_program_with_opts;
use machina::core::capsule::CapsuleError;
use machina::core::diag::CompileError;
use machina::core::parse::ParseError;
use machina::core::resolve::ResolveError;
use machina::driver::compile::{CompileOptions, compile_with_path};
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
