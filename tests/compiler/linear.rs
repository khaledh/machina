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

#[test]
fn linear_type_hosted_create_returns_initial_state() {
    let run = run_program(
        "linear_type_hosted_create_initial_state",
        r#"
            @linear
            type PullRequest = {
                id: u64,

                states {
                    Draft,
                    Review,
                }

                actions {
                    submit: Draft -> Review,
                }

                roles {
                    Author { submit }
                }
            }

            PullRequest :: {
                fn submit(self) -> Review {
                    println("submit");
                    Review {}
                }
            }

            machine PRService hosts PullRequest(key: id) {
                fn new() -> Self {
                    Self {}
                }
            }

            @machines
            fn main() -> () | MachineError | SessionError {
                let service = PRService::spawn()?;
                let draft = service.create(PullRequest as Author)?;
                let _review = draft.submit()?;
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "submit\n", "unexpected stdout: {stdout}");
}

#[test]
fn linear_type_hosted_action_uses_machine_override_when_present() {
    let run = run_program(
        "linear_type_hosted_action_override_dispatch",
        r#"
            @linear
            type PullRequest = {
                id: u64,

                states {
                    Draft,
                    Review,
                }

                actions {
                    submit: Draft -> Review,
                }

                roles {
                    Author { submit }
                }
            }

            PullRequest :: {
                fn submit(self) -> Review {
                    println("base");
                    Review {}
                }
            }

            machine PRService hosts PullRequest(key: id) {
                fn new() -> Self {
                    Self {}
                }

                action submit(draft) -> Review {
                    draft;
                    println("override");
                    Review {}
                }
            }

            @machines
            fn main() -> () | MachineError | SessionError {
                let service = PRService::spawn()?;
                let draft = service.create(PullRequest as Author)?;
                let _review = draft.submit()?;
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "override\n", "unexpected stdout: {stdout}");
}

#[test]
fn linear_type_hosted_action_rejects_role_disallowed() {
    let source = r#"
        @linear
        type Approval = {
            id: u64,

            states {
                Review,
                Approved,
            }

            actions {
                comment(text: string): Review -> Review,
                approve: Review -> Approved,
            }

            roles {
                Author { comment }
                Reviewer { comment, approve }
            }
        }

        Approval :: {
            fn comment(self, text: string) -> Review {
                println(text);
                Review {}
            }

            fn approve(self) -> Approved {
                Approved {}
            }
        }

        machine ApprovalService hosts Approval(key: id) {
            fn new() -> Self {
                Self {}
            }
        }

        @machines
        fn main() -> () | MachineError | SessionError {
            let service = ApprovalService::spawn()?;
            let review = service.create(Approval as Author)?;
            let _approved = review.approve()?;
        }
    "#;

    let errors = check_linear_source(
        source,
        "tests/fixtures/linear/hosted_role_disallowed_action.mc",
    )
    .expect_err("hosted sessions should reject actions not allowed for the role");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("MC-SESSION-ACTION-NOT-ALLOWED")
            || rendered.contains("not available")
            || (rendered.contains("approve") && rendered.contains("Author")),
        "expected a role-based session action diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_hosted_resume_returns_state_union() {
    let source = r#"
        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
                @final Merged,
            }

            actions {
                submit: Draft -> Review,
                merge: Review -> Merged,
            }

            roles {
                Author { submit, merge }
            }
        }

        PullRequest :: {
            fn submit(self) -> Review {
                Review {}
            }

            fn merge(self) -> Merged {
                Merged {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }
        }

        @machines
        fn main() -> () | MachineError | SessionError {
            let service = PRService::spawn()?;
            let session = service.resume(PullRequest as Author, 42)?;

            match session {
                PullRequest::Draft => {}
                PullRequest::Review => {}
                PullRequest::Merged => {}
            }
        }
    "#;

    compile_linear_source(source, "tests/fixtures/linear/hosted_resume_union.mc")
        .expect("hosted resume should typecheck with a union of states");
}

#[test]
fn linear_type_hosted_create_rejects_unknown_role() {
    let source = r#"
        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
            }

            actions {}

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }
        }

        @machines
        fn main() -> () | MachineError | SessionError {
            let service = PRService::spawn()?;
            let _session = service.create(PullRequest as Reviewer)?;
        }
    "#;

    let errors = check_linear_source(source, "tests/fixtures/linear/hosted_unknown_role.mc")
        .expect_err("creating a session for an unknown role should be rejected");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("role")
            || rendered.contains("Reviewer")
            || rendered.contains("MC-SESSION-UNKNOWN-ROLE"),
        "expected an unknown-role diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_hosted_create_rejects_machine_host_mismatch() {
    let source = r#"
        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
            }

            actions {}

            roles {
                Author {}
            }
        }

        @linear
        type Issue = {
            id: u64,

            states {
                Open,
            }

            actions {}

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }
        }

        @machines
        fn main() -> () | MachineError | SessionError {
            let service = PRService::spawn()?;
            let _session = service.create(Issue as Author)?;
        }
    "#;

    let errors = check_linear_source(source, "tests/fixtures/linear/hosted_machine_mismatch.mc")
        .expect_err("creating a session for the wrong hosted type should be rejected");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("hosts")
            || rendered.contains("Issue")
            || rendered.contains("PullRequest")
            || rendered.contains("MC-SESSION-HOST-MISMATCH"),
        "expected a machine-host mismatch diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_machine_requires_trigger_handlers() {
    let source = r#"
        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {}

            triggers {
                ci_passed: Draft -> Review,
            }

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }
        }
    "#;

    let errors = compile_linear_source(
        source,
        "tests/fixtures/linear/machine_missing_trigger_handler.mc",
    )
    .expect_err("machines should be required to implement every trigger handler");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("MC-MACHINE-MISSING-TRIGGER-HANDLER")
            || rendered.contains("missing trigger")
            || rendered.contains("ci_passed"),
        "expected missing trigger handler diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_machine_accepts_valid_trigger_handler_body() {
    let source = r#"
        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {}

            triggers {
                ci_passed: Draft -> Review,
            }

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            trigger ci_passed(draft) {
                draft;
                Review {}
            }
        }
    "#;

    compile_linear_source(
        source,
        "tests/fixtures/linear/machine_trigger_valid_body.mc",
    )
    .expect("valid trigger handlers should compile");
}

#[test]
fn linear_type_machine_rejects_trigger_wrong_target_state() {
    let source = r#"
        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {}

            triggers {
                ci_passed: Draft -> Review,
            }

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            trigger ci_passed(draft) {
                draft;
                Draft {}
            }
        }
    "#;

    let errors = compile_linear_source(
        source,
        "tests/fixtures/linear/machine_trigger_wrong_target.mc",
    )
    .expect_err("trigger handlers returning the wrong state should be rejected");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("MC-MACHINE-HANDLER-TYPE-MISMATCH")
            || rendered.contains("target state")
            || rendered.contains("ci_passed"),
        "expected trigger target mismatch diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_machine_rejects_extra_action_override() {
    let source = r#"
        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {
                submit: Draft -> Review,
            }

            roles {
                Author { submit }
            }
        }

        PullRequest :: {
            fn submit(self) -> Review {
                Review {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            action merge(review) -> Review {
                review
            }
        }
    "#;

    let errors = compile_linear_source(source, "tests/fixtures/linear/machine_extra_action.mc")
        .expect_err("undeclared machine action overrides should be rejected");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("MC-MACHINE-EXTRA-HANDLER")
            || rendered.contains("extra handler")
            || rendered.contains("merge"),
        "expected extra machine handler diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_machine_rejects_extra_trigger_params() {
    let source = r#"
        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {}

            triggers {
                ci_passed: Draft -> Review,
            }

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            trigger ci_passed(draft, extra: u64) {
                draft;
                extra;
            }
        }
    "#;

    let errors = compile_linear_source(
        source,
        "tests/fixtures/linear/machine_trigger_extra_params.mc",
    )
    .expect_err("trigger handlers with extra params should be rejected");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("MC-MACHINE-HANDLER-TYPE-MISMATCH")
            || rendered.contains("parameter")
            || rendered.contains("ci_passed"),
        "expected machine handler type mismatch diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_machine_rejects_override_error_subset() {
    let source = r#"
        type IoError = DiskFull | Busy

        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {
                submit: Draft -> Review | IoError,
            }

            roles {
                Author { submit }
            }
        }

        PullRequest :: {
            fn submit(self) -> Review | IoError {
                Review {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            action submit(draft) -> Review {
                draft.submit()?
            }
        }
    "#;

    let errors = compile_linear_source(
        source,
        "tests/fixtures/linear/machine_override_error_subset.mc",
    )
    .expect_err("machine action overrides must not drop base action errors");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("MC-MACHINE-OVERRIDE-ERROR-SUBSET")
            || rendered.contains("error subset")
            || rendered.contains("submit"),
        "expected machine override error-subset diagnostic, got: {rendered}"
    );
}
