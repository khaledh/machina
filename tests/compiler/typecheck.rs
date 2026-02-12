use crate::common::run_program;
use machina::core::capsule::CapsuleError;
use machina::core::diag::CompileError;
use machina::core::typecheck::TypeCheckErrorKind;
use machina::driver::compile::{CompileOptions, check_with_path, compile_with_path};
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[test]
fn test_overloaded_functions() {
    let run = run_program(
        "overloads",
        r#"
            requires {
                std::io as io
            }

            type Small = u64: bounds(0, 10);

            fn id(x: u64) -> u64 {
                x
            }

            fn id(x: Small) -> u64 {
                let y: u64 = x;
                y + 100
            }

            fn main() -> u64 {
                let a: Small = 5;
                let b = 20;
                io::println(id(a));
                io::println(id(b));
                0
            }
        "#,
    );
    assert_eq!(run.status.code(), Some(0));

    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "105\n20\n", "unexpected stdout: {stdout}");
}

fn with_temp_program(
    name: &str,
    entry_source: &str,
    extra_modules: &[(&str, &str)],
    test: impl FnOnce(&Path, &str),
) {
    let run_id = TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_compiler_test_{}_{}_{}",
        name,
        std::process::id(),
        run_id
    ));
    std::fs::create_dir_all(&temp_dir).expect("failed to create temp dir");

    let entry_path = temp_dir.join("main.mc");
    std::fs::write(&entry_path, entry_source).expect("failed to write entry source");

    for (module_path, source) in extra_modules {
        let file_path = temp_dir.join(module_path);
        if let Some(parent) = file_path.parent() {
            std::fs::create_dir_all(parent).expect("failed to create module parent");
        }
        std::fs::write(&file_path, source).expect("failed to write module source");
    }

    test(&entry_path, entry_source);
    let _ = std::fs::remove_dir_all(&temp_dir);
}

fn typecheck_with_modules(
    entry_path: &Path,
    entry_source: &str,
) -> Result<machina::driver::compile::CompileOutput, Vec<CompileError>> {
    compile_with_path(
        entry_source,
        Some(entry_path),
        &CompileOptions {
            dump: None,
            emit_ir: false,
            verify_ir: false,
            trace_alloc: false,
            trace_drops: false,
            inject_prelude: false,
        },
    )
}

fn check_with_modules(entry_path: &Path, entry_source: &str) -> Result<(), Vec<CompileError>> {
    check_with_path(entry_source, entry_path, true)
}

#[test]
fn test_modules_opaque_field_access_rejected() {
    let entry_source = r#"
        requires {
            app::secret
        }

        fn read_secret(v: Secret) -> u64 {
            v.x
        }

        fn main() -> u64 {
            0
        }
    "#;

    let secret_source = r#"
        @[opaque]
        type Secret = { x: u64 }
    "#;

    with_temp_program(
        "opaque_field_access",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for opaque field access"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::OpaqueFieldAccess(type_name, field, _)
                                    if type_name == "Secret" && field == "x"
                            )
                    )
                }));
            }
        },
    );
}

#[test]
fn test_modules_opaque_construction_rejected() {
    let entry_source = r#"
        requires {
            app::secret
        }

        fn main() -> u64 {
            let s = Secret { x: 1 };
            secret.take(s)
        }
    "#;

    let secret_source = r#"
        @[opaque]
        type Secret = { x: u64 }

        @[public]
        fn take(v: Secret) -> u64 {
            0
        }
    "#;

    with_temp_program(
        "opaque_construction",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for opaque construction"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::OpaqueTypeConstruction(type_name, _)
                                    if type_name == "Secret"
                            )
                    )
                }));
            }
        },
    );
}

#[test]
fn test_modules_private_method_call_rejected() {
    let entry_source = r#"
        requires {
            app::secret
        }

        fn main() -> u64 {
            var c = Counter { ticks: 0 };
            c.inc();
            0
        }
    "#;

    let secret_source = r#"
        @[public]
        type Counter = { ticks: u64 }

        Counter :: {
            fn inc(inout self) {
                self.ticks = self.ticks + 1;
            }
        }
    "#;

    with_temp_program(
        "private_method_call",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for private method call"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::CallableNotAccessible(name, _)
                                    if name == "inc"
                            )
                    )
                }));
            }
        },
    );
}

#[test]
fn test_modules_private_property_access_rejected() {
    let entry_source = r#"
        requires {
            app::secret
        }

        fn main() -> u64 {
            let c = Counter { ticks: 3 };
            c.tick_count
        }
    "#;

    let secret_source = r#"
        @[public]
        type Counter = { ticks: u64 }

        Counter :: {
            prop tick_count: u64 {
                get { self.ticks }
            }
        }
    "#;

    with_temp_program(
        "private_property_access",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for private property access"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::PropertyNotAccessible(name, _)
                                    if name == "tick_count"
                            )
                    )
                }));
            }
        },
    );
}

#[test]
fn test_strict_compile_rejects_mixed_region_resolve_fixture() {
    let entry_source = r#"
        fn id(x: u64) -> u64 { x }

        fn bad_region() -> u64 {
            missing
        }

        fn main() -> u64 {
            id(1)
        }
    "#;

    with_temp_program(
        "strict_mixed_region_resolve",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "strict compile must still fail on unresolved symbols"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::Resolve(resolve_err)
                            if resolve_err.to_string().contains("Undefined variable")
                    )
                }));
            }
        },
    );
}

#[test]
fn test_strict_compile_rejects_mixed_region_type_fixture() {
    let entry_source = r#"
        fn id(x: u64) -> u64 { x }

        fn bad_region() -> u64 {
            let v: u64 = true;
            v
        }

        fn main() -> u64 {
            id(1)
        }
    "#;

    with_temp_program(
        "strict_mixed_region_type",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "strict compile must still fail on type errors"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(type_err.kind(), TypeCheckErrorKind::DeclTypeMismatch(_, _, _))
                    )
                }));
            }
        },
    );
}

#[test]
fn test_check_module_graph_parity_std_io_import() {
    let entry_source = r#"
        requires {
            std::io as io
        }

        fn main() -> u64 {
            io::println("ok");
            0
        }
    "#;

    with_temp_program(
        "check_std_io_parity",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = check_with_modules(entry_path, entry_src);
            assert!(
                result.is_ok(),
                "module-aware check should accept std::io usage without unresolved symbol false positives"
            );
        },
    );
}

#[test]
fn test_check_module_graph_parity_multi_module_requires() {
    let entry_source = r#"
        requires {
            app::util as util
        }

        fn main() -> u64 {
            util::answer()
        }
    "#;

    let util_source = r#"
        @[public]
        fn answer() -> u64 {
            42
        }
    "#;

    with_temp_program(
        "check_multi_module_parity",
        entry_source,
        &[("app/util.mc", util_source)],
        |entry_path, entry_src| {
            let result = check_with_modules(entry_path, entry_src);
            assert!(
                result.is_ok(),
                "module-aware check should follow requires/module flattening like compile"
            );
        },
    );
}

#[test]
fn test_check_single_file_behavior_unchanged() {
    let entry_source = r#"
        fn main() -> u64 {
            missing
        }
    "#;

    with_temp_program(
        "check_single_file_behavior",
        entry_source,
        &[],
        |entry_path, entry_src| {
            let result = check_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "single-file unresolved symbol should still fail check"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(err, CompileError::Resolve(resolve_err) if resolve_err.to_string().contains("Undefined variable"))
                }));
            }
        },
    );
}

#[test]
fn test_modules_opaque_pattern_destructure_rejected() {
    let entry_source = r#"
        requires {
            app::secret
        }

        fn main() -> u64 {
            let Token { raw } = secret.make();
            raw
        }
    "#;

    let secret_source = r#"
        @[opaque]
        type Token = { raw: u64 }

        @[public]
        fn make() -> Token {
            Token { raw: 7 }
        }
    "#;

    with_temp_program(
        "opaque_pattern_destructure",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for opaque destructuring pattern"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::TypeCheck(type_err)
                            if matches!(
                                type_err.kind(),
                                TypeCheckErrorKind::OpaquePatternDestructure(type_name, _)
                                    if type_name == "Token"
                            )
                    )
                }));
            }
        },
    );
}

#[test]
fn test_modules_private_trait_bound_rejected() {
    let entry_source = r#"
        requires {
            app::secret as sec
        }

        fn run_it<T: sec::Internal>(value: T) -> u64 {
            0
        }

        fn main() -> u64 { 0 }
    "#;

    let secret_source = r#"
        trait Internal {
            fn run(self);
        }
    "#;

    with_temp_program(
        "private_trait_bound",
        entry_source,
        &[("app/secret.mc", secret_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_err(),
                "compile should fail for private trait bound"
            );
            if let Err(errors) = result {
                assert!(errors.iter().any(|err| {
                    matches!(
                        err,
                        CompileError::Capsule(CapsuleError::RequireMemberPrivate {
                            alias,
                            member,
                            expected_kind,
                            ..
                        }) if alias == "sec" && member == "Internal" && *expected_kind == "trait"
                    )
                }));
            }
        },
    );
}

#[test]
fn test_modules_duplicate_public_function_names_allowed_with_aliases() {
    let entry_source = r#"
        requires {
            app::util as util
            app::math as math
        }

        fn main() -> u64 {
            util::answer() + math::answer()
        }
    "#;

    let util_source = r#"
        @[public]
        fn answer() -> u64 { 7 }
    "#;

    let math_source = r#"
        @[public]
        fn answer() -> u64 { 11 }
    "#;

    with_temp_program(
        "duplicate_public_callables",
        entry_source,
        &[("app/util.mc", util_source), ("app/math.mc", math_source)],
        |entry_path, entry_src| {
            let result = typecheck_with_modules(entry_path, entry_src);
            assert!(
                result.is_ok(),
                "compile should succeed for aliased duplicate exports"
            );
        },
    );
}
