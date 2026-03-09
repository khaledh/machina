use crate::common::run_program;
use machina::core::diag::CompileError;
use machina::driver::compile::{CompileOptions, check_with_path, compile_with_path};
use std::path::PathBuf;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn linear_opts() -> CompileOptions {
    CompileOptions {
        dump: None,
        emit_ir: false,
        verify_ir: false,
        trace_alloc: false,
        trace_drops: false,
        inject_prelude: true,
        experimental_typestate: false,
    }
}

fn compile_linear_source(source: &str, virtual_path: &str) -> Result<(), Vec<CompileError>> {
    compile_with_path(
        source,
        Some(&repo_root().join(virtual_path)),
        &linear_opts(),
    )
    .map(|_| ())
}

fn check_linear_source(source: &str, virtual_path: &str) -> Result<(), Vec<CompileError>> {
    check_with_path(source, &repo_root().join(virtual_path), true, false)
}

#[test]
fn linear_type_direct_mode_door_example_runs() {
    let run = run_program(
        "linear_type_direct_door",
        r#"
            @linear
            type Door = {
                states {
                    Closed,
                    Open,
                }

                actions {
                    open: Closed -> Open,
                    close: Open -> Closed,
                }
            }

            Door :: {
                fn open(self) -> Open {
                    println("opening");
                    Open {}
                }

                fn close(self) -> Closed {
                    println("closing");
                    Closed {}
                }
            }

            fn main() {
                let door = Door::Closed {};
                let door = door.open();
                let _door = door.close();
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "opening\nclosing\n", "unexpected stdout: {stdout}");
}

#[test]
fn linear_type_direct_mode_supports_state_payload_matching() {
    let run = run_program(
        "linear_type_state_payload_matching",
        r#"
            @linear
            type Door = {
                states {
                    Closed,
                    Locked(u64),
                }

                actions {
                    lock(code: u64): Closed -> Locked,
                }
            }

            Door :: {
                fn lock(self, code: u64) -> Locked {
                    Locked(code)
                }
            }

            fn main() {
                let door = Door::Closed {};
                let door = door.lock(1234);
                match door {
                    Door::Locked(code) => {
                        println(code);
                    }
                    _ => {}
                }
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "1234\n", "unexpected stdout: {stdout}");
}

#[test]
fn linear_type_rejects_use_after_consume() {
    let source = r#"
        @linear
        type Door = {
            states {
                Closed,
                Open,
            }

            actions {
                open: Closed -> Open,
            }
        }

        Door :: {
            fn open(self) -> Open {
                Open {}
            }
        }

        fn main() {
            let door = Door::Closed {};
            let _open = door.open();
            let _again = door.open();
        }
    "#;

    let errors = check_linear_source(source, "tests/fixtures/linear/use_after_consume.mc")
        .expect_err("linear use-after-consume should be rejected");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("use-after-consume")
            || rendered.contains("UseAfter")
            || rendered.contains("consumed"),
        "expected a linear use-after-consume diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_rejects_wrong_state_action() {
    let source = r#"
        @linear
        type Door = {
            states {
                Closed,
                Open,
            }

            actions {
                open: Closed -> Open,
                close: Open -> Closed,
            }
        }

        Door :: {
            fn open(self) -> Open {
                Open {}
            }

            fn close(self) -> Closed {
                Closed {}
            }
        }

        fn main() {
            let door = Door::Closed {};
            let _door = door.close();
        }
    "#;

    let errors = check_linear_source(source, "tests/fixtures/linear/wrong_state_action.mc")
        .expect_err("wrong-state actions should be rejected");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("Function overload not found")
            || rendered.contains("OverloadNoMatch")
            || rendered.contains("MC-TYPECHECK-OverloadNoMatch"),
        "expected wrong-state action diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_requires_receiver_annotation_for_ambiguous_actions() {
    let source = r#"
        @linear
        type PullRequest = {
            states {
                Draft,
                Review,
            }

            actions {
                comment(text: string): Draft -> Draft,
                comment(text: string): Review -> Review,
            }
        }

        PullRequest :: {
            fn comment(self, text: string) -> Draft {
                text;
                Draft {}
            }

            fn comment(self, text: string) -> Review {
                text;
                Review {}
            }
        }
    "#;

    let errors = compile_linear_source(
        source,
        "tests/fixtures/linear/ambiguous_receiver_annotation.mc",
    )
    .expect_err("ambiguous action receivers should be rejected");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("receiver")
            || rendered.contains("ambiguous")
            || rendered.contains("LinearMethodAmbiguousReceiver")
            || rendered.contains("MC-METHOD-AMBIGUOUS-RECEIVER"),
        "expected ambiguous receiver diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_allows_receiver_annotation_for_same_named_actions() {
    let run = run_program(
        "linear_type_receiver_annotation_positive",
        r#"
            @linear
            type PullRequest = {
                states {
                    Draft,
                    Review,
                }

                actions {
                    comment(text: string): Draft -> Draft,
                    submit: Draft -> Review,
                    comment(text: string): Review -> Review,
                }
            }

            PullRequest :: {
                fn comment(self: Draft, text: string) -> Draft {
                    println(text);
                    Draft {}
                }

                fn submit(self) -> Review {
                    println("submit");
                    Review {}
                }

                fn comment(self: Review, text: string) -> Review {
                    println(text);
                    Review {}
                }
            }

            fn main() {
                let pr = PullRequest::Draft {};
                let pr = pr.comment("draft");
                let pr = pr.submit();
                let _pr = pr.comment("review");
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "draft\nsubmit\nreview\n",
        "unexpected stdout: {stdout}"
    );
}
