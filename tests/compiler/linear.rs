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
    check_with_path(source, &repo_root().join(virtual_path), true)
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
fn linear_type_direct_mode_supports_shared_field_access() {
    let run = run_program(
        "linear_type_shared_field_access",
        r#"
            @linear
            type Approval = {
                id: u64,

                states {
                    Review,
                    Approved,
                }

                actions {
                    approve: Review -> Approved,
                }
            }

            Approval :: {
                fn approve(self) -> Approved {
                    Approved {}
                }
            }

            fn main() {
                let review = Approval::Review { id: 7 };
                println(review.id);
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "7\n", "unexpected stdout: {stdout}");
}

#[test]
fn linear_type_hosted_runtime_is_available_without_machines_attr() {
    let run = run_program(
        "linear_type_hosted_without_machines_attr",
        r#"
            @linear
            type Approval = {
                id: u64,

                states {
                    Review,
                    Approved,
                }

                actions {
                    approve: Review -> Approved,
                }

                roles {
                    Author { approve }
                }
            }

            Approval :: {
                fn approve(self) -> Approved {
                    Approved {}
                }
            }

            machine ApprovalService hosts Approval(key: id) {
                fn new() -> Self { Self {} }
            }

            fn main() -> () | MachineError | SessionError {
                let service = ApprovalService::spawn()?;
                let review = service.create(Approval as Author)?;
                let approved = review.approve()?;
                match approved {
                    Approval::Approved(_) => println("approved"),
                    _ => println("unexpected"),
                };
                ()
            }
        "#,
    );

    assert_eq!(
        run.status.code(),
        Some(0),
        "hosted linear program without @machines should run, stderr: {}",
        String::from_utf8_lossy(&run.stderr)
    );
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "approved\n", "unexpected stdout: {stdout}");
}

#[test]
fn linear_type_hosted_machine_handle_is_user_facing_in_helper_functions() {
    let source = r#"
            type FraudAlert = {
                payment_id: u64,
            }

            @linear
            type Payment = {
                id: u64,

                states {
                    Created,
                    Authorized,
                    Declined,
                }

                actions {
                    authorize: Created -> Authorized,
                }

                triggers {
                    FraudAlert: Authorized -> Declined,
                }

                roles {
                    Merchant { authorize }
                }
            }

            Payment :: {
                fn authorize(self) -> Authorized {
                    Authorized {}
                }
            }

            machine PaymentService hosts Payment(key: id) {
                fn new() -> Self { Self {} }

                trigger FraudAlert(payment) {
                    payment;
                    Declined {}
                }

                on FraudAlert(event) {
                    let _result = self.deliver(event.payment_id, event);
                }
            }

            fn checkout(service: Machine<PaymentService>) -> u64 | MachineError | SessionError {
                let created = service.create(Payment as Merchant)?;
                created.id
            }

            fn fraud_service(
                service: Machine<PaymentService>,
                payment_id: u64,
            ) -> () | MachineError {
                service.send(FraudAlert { payment_id })?;
                ()
            }

            fn merchant_check(
                service: Machine<PaymentService>,
                payment_id: u64,
            ) -> () | MachineError | SessionError {
                let payment = service.resume(Payment as Merchant, payment_id)?;
                match payment {
                    Payment::Declined(_) => println("declined"),
                    Payment::Authorized(_) => println("authorized"),
                    _ => println("unexpected"),
                };
                ()
            }

            fn main() -> () | MachineError | SessionError {
                let service = PaymentService::spawn()?;
                let payment_id = checkout(service)?;
                let payment = service.resume(Payment as Merchant, payment_id)?;
                match payment {
                    Payment::Created(_) => {
                        let _authorized = payment.authorize()?;
                    }
                    _ => println("gateway-unexpected"),
                };
                fraud_service(service, payment_id)?;
                merchant_check(service, payment_id)?;
                ()
            }
        "#;

    compile_linear_source(
        source,
        "tests/fixtures/linear/hosted_machine_handle_user_facing.mc",
    )
    .expect("hosted helper functions should accept Machine<HostedMachine> handles");
}

#[test]
fn linear_type_hosted_machine_handle_runtime_helper_can_create_and_act() {
    let run = run_program(
        "linear_type_hosted_machine_handle_runtime_helper_create",
        r#"
            @linear
            type Approval = {
                id: u64,

                states {
                    Review,
                    Approved,
                }

                actions {
                    approve: Review -> Approved,
                }

                roles {
                    Author { approve }
                }
            }

            Approval :: {
                fn approve(self) -> Approved {
                    Approved {}
                }
            }

            machine ApprovalService hosts Approval(key: id) {
                fn new() -> Self { Self {} }
            }

            fn process(service: Machine<ApprovalService>) -> () | MachineError | SessionError {
                let review = service.create(Approval as Author)?;
                let approved = review.approve()?;
                match approved {
                    Approval::Approved(_) => println("approved"),
                    _ => println("unexpected"),
                };
                ()
            }

            fn main() -> () | MachineError | SessionError {
                let service = ApprovalService::spawn()?;
                process(service)?;
                ()
            }
        "#,
    );

    assert_eq!(
        run.status.code(),
        Some(0),
        "helper create/act program should run, stderr: {}",
        String::from_utf8_lossy(&run.stderr)
    );
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "approved\n", "unexpected stdout: {stdout}");
}

#[test]
fn linear_type_hosted_machine_handle_runtime_helper_can_return_created_key() {
    let run = run_program(
        "linear_type_hosted_machine_handle_runtime_helper_key",
        r#"
            @linear
            type Approval = {
                id: u64,

                states {
                    Review,
                }

                actions {}

                roles {
                    Author {}
                }
            }

            machine ApprovalService hosts Approval(key: id) {
                fn new() -> Self { Self {} }
            }

            fn create_review(service: Machine<ApprovalService>) -> u64 | SessionError {
                let review = service.create(Approval as Author)?;
                review.id
            }

            fn main() -> () | MachineError | SessionError {
                let service = ApprovalService::spawn()?;
                let review_id = create_review(service)?;
                println(review_id);
                ()
            }
        "#,
    );

    assert_eq!(
        run.status.code(),
        Some(0),
        "helper returning created key should run, stderr: {}",
        String::from_utf8_lossy(&run.stderr)
    );
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "1\n", "unexpected stdout: {stdout}");
}

#[test]
fn linear_type_hosted_machine_handle_runtime_helper_can_send() {
    let run = run_program(
        "linear_type_hosted_machine_handle_runtime_helper_send",
        r#"
            type Start = {}

            @linear
            type Approval = {
                id: u64,

                states {
                    Review,
                    Approved,
                }

                actions {}

                triggers {
                    Start: Review -> Approved,
                }

                roles {
                    Author {}
                }
            }

            machine ApprovalService hosts Approval(key: id) {
                fn new() -> Self { Self {} }

                trigger Start(review) {
                    review;
                    Approved {}
                }

                on Start(event) {
                    event;
                    println("handled");
                }
            }

            fn push(service: Machine<ApprovalService>) -> () | MachineError {
                service.send(Start {})?;
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                ()
            }

            fn main() -> () | MachineError {
                let service = ApprovalService::spawn()?;
                push(service)?;
                ()
            }
        "#,
    );

    assert_eq!(
        run.status.code(),
        Some(0),
        "helper send program should run, stderr: {}",
        String::from_utf8_lossy(&run.stderr)
    );
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "handled\n", "unexpected stdout: {stdout}");
}

#[test]
fn linear_type_hosted_multi_actor_payment_lifecycle_through_helper_functions() {
    let run = run_program(
        "linear_type_hosted_multi_actor_payment_helpers",
        r#"
            type FraudAlert = {
                payment_id: u64,
            }

            @linear
            type Payment = {
                id: u64,

                states {
                    Created,
                    Authorized,
                    Declined,
                }

                actions {
                    authorize: Created -> Authorized,
                }

                triggers {
                    FraudAlert: Authorized -> Declined,
                }

                roles {
                    Merchant { authorize }
                }
            }

            Payment :: {
                fn authorize(self) -> Authorized {
                    Authorized {}
                }
            }

            machine PaymentService hosts Payment(key: id) {
                fn new() -> Self { Self {} }

                trigger FraudAlert(payment) {
                    payment;
                    Declined {}
                }

                on FraudAlert(event) {
                    let _result = self.deliver(event.payment_id, event);
                }
            }

            fn checkout(service: Machine<PaymentService>) -> u64 | MachineError | SessionError {
                let created = service.create(Payment as Merchant)?;
                println("checkout");
                created.id
            }

            fn gateway_authorize(
                service: Machine<PaymentService>,
                payment_id: u64,
            ) -> () | MachineError | SessionError {
                let payment = service.resume(Payment as Merchant, payment_id)?;
                match payment {
                    Payment::Created(_) => {
                        let _authorized = payment.authorize()?;
                        println("gateway");
                    }
                    _ => println("gateway-unexpected"),
                };
                ()
            }

            fn fraud_service(
                service: Machine<PaymentService>,
                payment_id: u64,
            ) -> () | MachineError {
                service.send(FraudAlert { payment_id })?;
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                println("fraud");
                ()
            }

            fn merchant_capture(
                service: Machine<PaymentService>,
                payment_id: u64,
            ) -> () | MachineError | SessionError {
                let payment = service.resume(Payment as Merchant, payment_id)?;
                match payment {
                    Payment::Declined(_) => println("declined"),
                    Payment::Authorized(_) => println("authorized"),
                    _ => println("unexpected"),
                };
                ()
            }

            fn main() -> () | MachineError | SessionError {
                let service = PaymentService::spawn()?;
                let payment_id = checkout(service)?;
                gateway_authorize(service, payment_id)?;
                fraud_service(service, payment_id)?;
                merchant_capture(service, payment_id)?;
                ()
            }
        "#,
    );

    assert_eq!(
        run.status.code(),
        Some(0),
        "multi-actor helper payment flow should run, stderr: {}",
        String::from_utf8_lossy(&run.stderr)
    );
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "checkout\ngateway\nfraud\ndeclined\n",
        "unexpected stdout: {stdout}"
    );
}

#[test]
fn linear_type_direct_mode_preserves_shared_fields_across_transitions() {
    let run = run_program(
        "linear_type_shared_field_preservation",
        r#"
            @linear
            type Approval = {
                id: u64,

                states {
                    Review,
                    Approved,
                }

                actions {
                    approve: Review -> Approved,
                }
            }

            Approval :: {
                fn approve(self) -> Approved {
                    Approved {}
                }
            }

            fn main() {
                let review = Approval::Review { id: 7 };
                let approved = review.approve();
                println(approved.id);
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "7\n", "unexpected stdout: {stdout}");
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
fn linear_type_hosted_create_assigns_runtime_backed_key() {
    let run = run_program(
        "linear_type_hosted_create_runtime_key",
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
                println(draft.id);
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    let key = stdout
        .trim()
        .parse::<u64>()
        .expect("hosted create should print a numeric key");
    assert!(
        key > 0,
        "expected a nonzero runtime-assigned key, got {key}"
    );
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
fn linear_type_hosted_action_rejects_stale_session_at_runtime() {
    let run = run_program(
        "linear_type_hosted_action_rejects_stale_session_at_runtime",
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
                let draft1 = service.create(PullRequest as Author)?;
                let resumed = service.resume(PullRequest as Author, draft1.id)?;
                let _review = draft1.submit()?;

                match resumed {
                    PullRequest::Draft(_) => match resumed.submit() {
                        review: PullRequest => match review {
                            PullRequest::Review(_) => println("fresh"),
                            PullRequest::Draft(_) => println("fresh"),
                        },
                        err: SessionError => match err {
                            InvalidState => println("stale"),
                            InstanceNotFound => println("missing"),
                        },
                    },
                    PullRequest::Review(id) => {
                        println(id);
                        return ();
                    },
                }
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "stale\n",
        "hosted actions should reject stale sessions with InvalidState"
    );
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
fn linear_type_hosted_fallible_action_does_not_advance_state_on_error() {
    let run = run_program(
        "linear_type_hosted_action_error_keeps_state",
        r#"
            @linear
            type PullRequest = {
                id: u64,

                states {
                    Draft,
                    Review,
                }

                actions {
                    submit: Draft -> Review | SessionError,
                }

                roles {
                    Author { submit }
                }
            }

            PullRequest :: {
                fn submit(self) -> Review | SessionError {
                    always_fail()
                }
            }

            fn always_fail() -> PullRequest | SessionError {
                SessionError::InvalidState
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
                let draft_id = draft.id;

                match draft.submit() {
                    _ok: PullRequest => {}
                    _err: SessionError => {}
                };

                let resumed = service.resume(PullRequest as Author, draft_id)?;
                match resumed {
                    PullRequest::Draft(_) => println("draft"),
                    PullRequest::Review(_) => println("review"),
                };
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "draft\n",
        "failed hosted actions should leave runtime state unchanged"
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
                PullRequest::Draft(_) => {}
                PullRequest::Review(_) => {}
                PullRequest::Merged(_) => {}
            }
        }
    "#;

    compile_linear_source(source, "tests/fixtures/linear/hosted_resume_union.mc")
        .expect("hosted resume should typecheck with a union of states");
}

#[test]
fn linear_type_hosted_resume_reads_runtime_backed_state() {
    let run = run_program(
        "linear_type_hosted_resume_runtime_state",
        r#"
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
                let draft = service.create(PullRequest as Author)?;
                let resumed = service.resume(PullRequest as Author, draft.id)?;

                match resumed {
                    PullRequest::Draft(id) => if id > 0 { println("ok") } else { println("bad") },
                    PullRequest::Review(id) => if id > 0 { println("ok") } else { println("bad") },
                    PullRequest::Merged(id) => if id > 0 { println("ok") } else { println("bad") },
                }
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "ok\n",
        "resume should preserve the runtime-assigned key"
    );
}

#[test]
fn linear_type_hosted_lookup_reads_current_instance_state() {
    let run = run_program(
        "linear_type_hosted_lookup_reads_state",
        r#"
            @linear
            type PullRequest = {
                id: u64,

                states {
                    Draft,
                    Review,
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
                let draft = service.create(PullRequest as Author)?;
                let current = service.lookup(PullRequest, draft.id)?;
                match current {
                    PullRequest::Draft(id) => println(id),
                    PullRequest::Review(id) => println(id),
                };
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "1\n", "unexpected stdout: {stdout}");
}

#[test]
fn linear_type_hosted_lookup_reports_missing_instance() {
    let run = run_program(
        "linear_type_hosted_lookup_missing",
        r#"
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
                match service.lookup(PullRequest, 42) {
                    pr: PullRequest => match pr {
                        PullRequest::Draft(id) => println(id),
                    },
                    err: SessionError => match err {
                        InvalidState => println("invalid"),
                        InstanceNotFound => println("missing"),
                    },
                };
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(stdout, "missing\n", "unexpected stdout: {stdout}");
}

#[test]
fn linear_type_hosted_wait_typechecks_for_trigger_driven_state() {
    let source = r#"
        type CIPassed = {}
        type CIFailed = {}

        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                PendingCI,
                Review,
            }

            actions {
                submit: Draft -> PendingCI,
            }

            triggers {
                CIPassed: PendingCI -> Review,
                CIFailed: PendingCI -> Draft,
            }

            roles {
                Author { submit }
            }
        }

        PullRequest :: {
            fn submit(self) -> PendingCI {
                PendingCI {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            trigger CIPassed(pending) {
                pending;
                Review {}
            }

            trigger CIFailed(pending) {
                pending;
                Draft {}
            }
        }

        @machines
        fn main() -> () | MachineError | SessionError {
            let service = PRService::spawn()?;
            let draft = service.create(PullRequest as Author)?;
            let pending = draft.submit()?;
            let next = pending.wait()?;

            // Today `wait()?` resumes as the hosted enum type, so the match
            // still needs a fallback arm even though only `Draft` and `Review`
            // are reachable from `PendingCI`. We can tighten this once the
            // type system grows a way to represent state subsets explicitly.
            match next {
                PullRequest::Draft(_) => {}
                PullRequest::Review(_) => {}
                _ => {}
            }
        }
    "#;

    compile_linear_source(source, "tests/fixtures/linear/hosted_wait_union.mc")
        .expect("hosted wait should typecheck for trigger-driven hosted states");
}

#[test]
fn linear_type_hosted_full_lifecycle_runs_end_to_end() {
    let run = run_program(
        "linear_type_hosted_full_lifecycle",
        r#"
            type CIPassed = {
                pr_id: u64,
            }

            @linear
            type PullRequest = {
                id: u64,

                states {
                    Draft,
                    PendingCI,
                    Review,
                }

                actions {
                    submit: Draft -> PendingCI,
                    retry: PendingCI -> PendingCI,
                }

                triggers {
                    CIPassed: PendingCI -> Review,
                }

                roles {
                    Author { submit, retry }
                }
            }

            PullRequest :: {
                fn submit(self) -> PendingCI {
                    PendingCI {}
                }

                fn retry(self) -> PendingCI {
                    PendingCI {}
                }
            }

            machine PRService hosts PullRequest(key: id) {
                fn new() -> Self {
                    Self {}
                }

                trigger CIPassed(pending) {
                    pending;
                    Review {}
                }

                on CIPassed(event) {
                    match self.deliver(event.pr_id, event) {
                        Delivered => println("delivered"),
                        InstanceNotFound => println("deliver-failed"),
                        InvalidState => println("deliver-failed"),
                    }
                }
            }

            @machines
            fn main() -> () | MachineError | SessionError {
                let service = PRService::spawn()?;
                let draft = service.create(PullRequest as Author)?;
                let pending = draft.submit()?;
                let pending_id = pending.id;
                let resumed = service.resume(PullRequest as Author, pending.id)?;

                service.send(CIPassed { pr_id: pending_id })?;

                let next = pending.wait()?;
                match next {
                    PullRequest::Review(_) => println("wait-review"),
                    PullRequest::PendingCI(_) => println("wait-pending"),
                    PullRequest::Draft(_) => println("wait-draft"),
                };

                let fresh = service.resume(PullRequest as Author, pending_id)?;
                match fresh {
                    PullRequest::Review(_) => println("resume-review"),
                    PullRequest::PendingCI(_) => println("resume-pending"),
                    PullRequest::Draft(_) => println("resume-draft"),
                };

                match resumed {
                    PullRequest::PendingCI(_) => match resumed.retry() {
                        next_state: PullRequest => match next_state {
                            PullRequest::Review(_) => println("unexpected-fresh-review"),
                            PullRequest::PendingCI(_) => println("unexpected-fresh-pending"),
                            PullRequest::Draft(_) => println("unexpected-fresh-draft"),
                        },
                        err: SessionError => match err {
                            InvalidState => println("stale"),
                            InstanceNotFound => println("missing"),
                        },
                    },
                    PullRequest::Review(_) => println("resumed-review"),
                    PullRequest::Draft(_) => println("resumed-draft"),
                };
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "delivered\nwait-review\nresume-review\nstale\n",
        "hosted full lifecycle should stay coherent end-to-end"
    );
}

#[test]
fn linear_type_hosted_on_handler_dispatches_from_mailbox() {
    let run = run_program(
        "linear_type_hosted_on_handler_dispatch",
        r#"
            type CIPassed = {
                pr_id: u64,
            }

            @linear
            type PullRequest = {
                id: u64,

                states {
                    Draft,
                    PendingCI,
                    Review,
                }

                actions {
                    submit: Draft -> PendingCI,
                }

                triggers {
                    CIPassed: PendingCI -> Review,
                }

                roles {
                    Author {
                        submit,
                    }
                }
            }

            PullRequest :: {
                fn submit(self) -> PendingCI {
                    PendingCI {}
                }
            }

            machine PRService hosts PullRequest(key: id) {
                fn new() -> Self {
                    Self {}
                }

                trigger CIPassed(pending) {
                    pending;
                    Review {}
                }

                on CIPassed(event) {
                    match self.deliver(event.pr_id, event) {
                        Delivered => println("handler-delivered"),
                        InstanceNotFound => println("handler-missing"),
                        InvalidState => println("handler-stale"),
                    }
                }
            }

            @machines
            fn main() -> () | MachineError | SessionError {
                let service = PRService::spawn()?;
                let draft = service.create(PullRequest as Author)?;
                let pending = draft.submit()?;
                let event = CIPassed { pr_id: pending.id };
                match service.send(event) {
                    _ok: () => println("send-ok"),
                    err: MachineError => match err {
                        SpawnFailed => println("send-spawn"),
                        BindFailed => println("send-bind"),
                        StartFailed => println("send-start"),
                        RuntimeUnavailable => println("send-runtime"),
                        Unknown => println("send-unknown"),
                        NotRunning => println("send-stopped"),
                        MailboxFull => println("send-full"),
                        RequestFailed => println("send-request"),
                    },
                };

                match pending.wait() {
                    next: PullRequest => match next {
                        PullRequest::Review(_) => println("wait-review"),
                        PullRequest::PendingCI(_) => println("wait-pending"),
                        PullRequest::Draft(_) => println("wait-draft"),
                    },
                    err: SessionError => match err {
                        InvalidState => println("wait-invalid"),
                        InstanceNotFound => println("wait-missing"),
                    },
                };
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "send-ok\nhandler-delivered\nwait-review\n",
        "hosted on-handler ingress should drive trigger delivery through the mailbox"
    );
}

#[test]
fn linear_type_hosted_on_handler_can_emit_send() {
    let run = run_program(
        "linear_type_hosted_on_handler_emit_send",
        r#"
            type Start = {}
            type Note = {}

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

                on Start(_event) {
                    println("start");
                    emit Send(to: self, Note {});
                }

                on Note(_event) {
                    println("note");
                }
            }

            @machines
            fn main() -> () | MachineError {
                let service = PRService::spawn()?;
                service.send(Start {})?;
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "start\nnote\n",
        "hosted on handlers should be able to stage emitted sends during dispatch"
    );
}

#[test]
fn linear_type_hosted_action_override_send_statement_targets_self() {
    let run = run_program(
        "linear_type_hosted_action_send_statement",
        r#"
            type Note = {}

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

                on Note(_event) {
                    println("note");
                }

                action submit(draft) -> Review {
                    draft;
                    send(self, Note {});
                    Review {}
                }
            }

            fn main() -> () | MachineError | SessionError {
                let service = PRService::spawn()?;
                let draft = service.create(PullRequest as Author)?;
                let _review = draft.submit()?;
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "note\n",
        "hosted action overrides should accept send(...) syntax and route via the existing mailbox send path"
    );
}

#[test]
fn linear_type_hosted_action_override_send_statement_targets_other_machine() {
    let run = run_program(
        "linear_type_hosted_action_override_send_statement_targets_other_machine",
        r#"
            type Note = {}

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

            machine Receiver hosts PullRequest(key: id) {
                fn new() -> Self {
                    Self {}
                }

                on Note(_event) {
                    println("note");
                }
            }

            machine Sender hosts PullRequest(key: id) {
                fields {
                    receiver: Machine<Receiver>,
                }

                fn new(receiver: Machine<Receiver>) -> Self {
                    Self { receiver: receiver }
                }

                action submit(draft) -> Review {
                    send(self.receiver, Note {});
                    draft;
                    Review {}
                }
            }

            fn main() -> () | MachineError | SessionError {
                let receiver = Receiver::spawn()?;
                let sender = Sender::spawn(receiver)?;
                let draft = sender.create(PullRequest as Author)?;
                let _review = draft.submit()?;
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "note\n",
        "hosted action overrides should accept send(...) syntax with machine fields wired through spawn(...)"
    );
}

#[test]
fn linear_type_hosted_on_handler_emit_uses_enclosing_machine_routing() {
    let run = run_program(
        "linear_type_hosted_on_handler_emit_overlapping_machine_names",
        r#"
            type Start = {}
            type Ack = {}
            type Noise = {}

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

            machine PR hosts PullRequest(key: id) {
                fn new() -> Self {
                    Self {}
                }

                on Noise(_event) {}

                on Start(_event) {
                    emit Send(to: self, Ack {});
                }

                on Ack(_event) {
                    println("pr-ack");
                }
            }

            machine PR_Service hosts PullRequest(key: id) {
                fn new() -> Self {
                    Self {}
                }

                on Start(_event) {
                    emit Send(to: self, Ack {});
                }

                on Ack(_event) {
                    println("service-ack");
                }
            }

            @machines
            fn main() -> () | MachineError {
                let service = PR_Service::spawn()?;
                service.send(Start {})?;
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "service-ack\n",
        "hosted emit routing should use the enclosing machine's handler metadata even when machine names overlap"
    );
}

#[test]
fn linear_type_hosted_trigger_handler_body_runs_during_delivery() {
    let run = run_program(
        "linear_type_hosted_trigger_body_runs",
        r#"
            type CIPassed = {
                pr_id: u64,
            }

            @linear
            type PullRequest = {
                id: u64,

                states {
                    Draft,
                    PendingCI,
                    Review,
                }

                actions {
                    submit: Draft -> PendingCI,
                }

                triggers {
                    CIPassed: PendingCI -> Review,
                }

                roles {
                    Author { submit }
                }
            }

            PullRequest :: {
                fn submit(self) -> PendingCI {
                    PendingCI {}
                }
            }

            machine PRService hosts PullRequest(key: id) {
                fn new() -> Self {
                    Self {}
                }

                trigger CIPassed(pending) {
                    pending;
                    println("trigger-body");
                    Review {}
                }

                on CIPassed(event) {
                    let _result = self.deliver(event.pr_id, event);
                }
            }

            @machines
            fn main() -> () | MachineError | SessionError {
                let service = PRService::spawn()?;
                let draft = service.create(PullRequest as Author)?;
                let pending = draft.submit()?;
                service.send(CIPassed { pr_id: pending.id })?;

                let next = pending.wait()?;
                match next {
                    PullRequest::Review(_) => println("wait-review"),
                    PullRequest::PendingCI(_) => println("wait-pending"),
                    PullRequest::Draft(_) => println("wait-draft"),
                };
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "trigger-body\nwait-review\n",
        "hosted trigger delivery should execute the trigger handler body before committing state"
    );
}

#[test]
fn linear_type_hosted_trigger_handler_can_emit_send() {
    let run = run_program(
        "linear_type_hosted_trigger_handler_emit_send",
        r#"
            type CIPassed = {
                pr_id: u64,
            }

            type Note = {}

            @linear
            type PullRequest = {
                id: u64,

                states {
                    Draft,
                    PendingCI,
                    Review,
                }

                actions {
                    submit: Draft -> PendingCI,
                }

                triggers {
                    CIPassed: PendingCI -> Review,
                }

                roles {
                    Author { submit }
                }
            }

            PullRequest :: {
                fn submit(self) -> PendingCI {
                    PendingCI {}
                }
            }

            machine PRService hosts PullRequest(key: id) {
                fn new() -> Self {
                    Self {}
                }

                trigger CIPassed(pending) {
                    pending;
                    println("trigger-body");
                    emit Send(to: self, Note {});
                    Review {}
                }

                on CIPassed(event) {
                    let _result = self.deliver(event.pr_id, event);
                }

                on Note(_event) {
                    println("note");
                }
            }

            @machines
            fn main() -> () | MachineError | SessionError {
                let service = PRService::spawn()?;
                let draft = service.create(PullRequest as Author)?;
                let pending = draft.submit()?;
                service.send(CIPassed { pr_id: pending.id })?;

                let next = pending.wait()?;
                match next {
                    PullRequest::Review(_) => println("wait-review"),
                    PullRequest::PendingCI(_) => println("wait-pending"),
                    PullRequest::Draft(_) => println("wait-draft"),
                };

                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "trigger-body\nwait-review\nnote\n",
        "hosted trigger handlers should be able to stage emitted sends before commit"
    );
}

#[test]
fn linear_type_direct_mode_rejects_wait() {
    let source = r#"
        type CIPassed = {}

        @linear
        type PullRequest = {
            states {
                PendingCI,
                Review,
            }

            triggers {
                CIPassed: PendingCI -> Review,
            }
        }

        fn main() {
            let pending = PullRequest::PendingCI {};
            let _next = pending.wait();
        }
    "#;

    let errors = compile_linear_source(source, "tests/fixtures/linear/direct_wait_rejected.mc")
        .expect_err("direct-mode linear values should not support wait()");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("OverloadNoMatch(\"wait\")")
            || rendered.contains("wait")
            || rendered.contains("MC-TYPECHECK-OVERLOAD-NO-MATCH"),
        "expected direct-mode wait rejection, got: {rendered}"
    );
}

#[test]
fn linear_type_hosted_action_override_can_emit_send() {
    let run = run_program(
        "linear_type_hosted_action_emit_send",
        r#"
            type Start = {}
            type Note = {}

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

                on Start(_event) {
                    println("start");
                }

                on Note(_event) {
                    println("note");
                }

                action submit(draft) -> Review {
                    draft;
                    emit Send(to: self, Note {});
                    Review {}
                }
            }

            @machines
            fn main() -> () | MachineError | SessionError {
                let service = PRService::spawn()?;
                service.send(Start {})?;
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                let draft = service.create(PullRequest as Author)?;
                let _review = draft.submit()?;
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "start\nnote\n",
        "hosted action overrides should be able to stage send effects and commit them after success"
    );
}

#[test]
fn linear_type_hosted_action_override_rejects_emit_request() {
    let source = r#"
        type Note = {}
        type Ack = {}

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

            on Note(_event) {}

            action submit(draft) -> Review {
                draft;
                let _pending = emit Request(to: self, Note {});
                Review {}
            }
        }
    "#;

    let errors = compile_linear_source(
        source,
        "tests/fixtures/linear/hosted_action_emit_request_rejected.mc",
    )
    .expect_err("hosted action overrides should still reject unsupported emit kinds");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("MC-MACHINE-HOSTED-ACTION-EMIT-UNSUPPORTED")
            || rendered.contains("MachineHostedActionEmitUnsupported")
            || rendered.contains("emit request")
            || rendered.contains("only `emit Send(...)`"),
        "expected hosted action emit-request diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_hosted_on_handler_rejects_unsupported_payload_shape() {
    let source = r#"
        type TooWide = {
            id: u64,
            build: u64,
            attempt: u64,
        }

        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
            }

            actions {}
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            on TooWide(_event) {}
        }
    "#;

    let errors = compile_linear_source(
        source,
        "tests/fixtures/linear/hosted_on_payload_unsupported.mc",
    )
    .expect_err("unsupported hosted on payload shapes should be diagnosed");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("MC-MACHINE-HOSTED-ON-PAYLOAD-UNSUPPORTED")
            || rendered.contains("MachineHostedOnPayloadUnsupported")
            || rendered.contains("not supported by hosted mailbox ingress")
            || rendered.contains("supported payloads are"),
        "expected hosted on payload diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_hosted_fallible_action_does_not_commit_emitted_send_on_error() {
    let run = run_program(
        "linear_type_hosted_action_emit_send_aborts_on_error",
        r#"
            type Note = {}

            @linear
            type PullRequest = {
                id: u64,

                states {
                    Draft,
                    Review,
                }

                actions {
                    submit: Draft -> Review | SessionError,
                }

                roles {
                    Author { submit }
                }
            }

            PullRequest :: {
                fn submit(self) -> Review | SessionError {
                    SessionError::InvalidState
                }
            }

            machine PRService hosts PullRequest(key: id) {
                fn new() -> Self {
                    Self {}
                }

                on Note(_event) {
                    println("note");
                }

                action submit(draft) -> Review | SessionError {
                    draft;
                    emit Send(to: self, Note {});
                    if true {
                        SessionError::InvalidState
                    } else {
                        Review {}
                    }
                }
            }

            @machines
            fn main() -> () | MachineError | SessionError {
                let service = PRService::spawn()?;
                let draft = service.create(PullRequest as Author)?;
                match draft.submit() {
                    _ok: PullRequest => println("ok"),
                    _err: SessionError => println("err"),
                };
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                let looked_up = service.lookup(PullRequest, 1)?;
                match looked_up {
                    PullRequest::Draft(_) => println("draft"),
                    PullRequest::Review(_) => println("review"),
                };
                ()
            }
        "#,
    );

    assert_eq!(run.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout, "err\ndraft\n",
        "failed hosted action overrides should discard staged sends and leave state unchanged"
    );
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
        type CIPassed = {}

        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {}

            triggers {
                CIPassed: Draft -> Review,
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
            || rendered.contains("CIPassed"),
        "expected missing trigger handler diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_machine_accepts_valid_trigger_handler_body() {
    let source = r#"
        type CIPassed = {}

        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {}

            triggers {
                CIPassed: Draft -> Review,
            }

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            trigger CIPassed(draft) {
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
        type CIPassed = {}

        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {}

            triggers {
                CIPassed: Draft -> Review,
            }

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            trigger CIPassed(draft) {
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
            || rendered.contains("CIPassed"),
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
        type CIPassed = {}

        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {}

            triggers {
                CIPassed: Draft -> Review,
            }

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            trigger CIPassed(draft, extra: u64) {
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
            || rendered.contains("CIPassed"),
        "expected machine handler type mismatch diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_machine_deliver_accepts_declared_event_type() {
    let source = r#"
        type CIPassed = {
            pr_id: u64,
        }

        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {}

            triggers {
                CIPassed: Draft -> Review,
            }

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            trigger CIPassed(draft) {
                draft;
                Review {}
            }

            on CIPassed(event) {
                match self.deliver(event.pr_id, event) {
                    Delivered => {}
                    InstanceNotFound => {}
                    InvalidState => {}
                }
            }
        }
    "#;

    compile_linear_source(
        source,
        "tests/fixtures/linear/machine_deliver_declared_event.mc",
    )
    .expect("declared trigger event delivery should compile");
}

#[test]
fn linear_type_machine_deliver_rejects_key_type_mismatch() {
    let source = r#"
        type CIPassed = {
            pr_id: u64,
        }

        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {}

            triggers {
                CIPassed: Draft -> Review,
            }

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            trigger CIPassed(draft) {
                draft;
                Review {}
            }

            on CIPassed(event) {
                let _result = self.deliver("wrong", event);
            }
        }
    "#;

    let errors = compile_linear_source(
        source,
        "tests/fixtures/linear/machine_deliver_wrong_key_type.mc",
    )
    .expect_err("deliver should reject key type mismatches");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("deliver")
            || rendered.contains("key")
            || rendered.contains("LinearMachineDeliverKeyTypeMismatch")
            || rendered.contains("MC-MACHINE-DELIVER-KEY-TYPE"),
        "expected deliver key type diagnostic, got: {rendered}"
    );
}

#[test]
fn linear_type_machine_deliver_rejects_unknown_event_type() {
    let source = r#"
        type CIPassed = {
            pr_id: u64,
        }

        type CIFailed = {
            pr_id: u64,
        }

        @linear
        type PullRequest = {
            id: u64,

            states {
                Draft,
                Review,
            }

            actions {}

            triggers {
                CIPassed: Draft -> Review,
            }

            roles {
                Author {}
            }
        }

        machine PRService hosts PullRequest(key: id) {
            fn new() -> Self {
                Self {}
            }

            trigger CIPassed(draft) {
                draft;
                Review {}
            }

            on CIFailed(event) {
                let _result = self.deliver(event.pr_id, event);
            }
        }
    "#;

    let errors = compile_linear_source(
        source,
        "tests/fixtures/linear/machine_deliver_unknown_event.mc",
    )
    .expect_err("deliver should reject undeclared trigger event types");
    let rendered = format!("{errors:#?}");
    assert!(
        rendered.contains("deliver")
            || rendered.contains("trigger")
            || rendered.contains("CIFailed")
            || rendered.contains("MC-MACHINE-DELIVER-UNKNOWN-TRIGGER"),
        "expected deliver unknown-trigger diagnostic, got: {rendered}"
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

#[test]
fn linear_type_hosted_multi_actor_payment_lifecycle() {
    let run = run_program(
        "linear_type_hosted_multi_actor_payment",
        r#"
            type FraudAlert = {
                payment_id: u64,
            }

            @linear
            type Payment = {
                id: u64,

                states {
                    Created,
                    Authorized,
                    Captured,
                    Declined,
                    Refunded,
                }

                actions {
                    authorize: Created -> Authorized,
                    capture: Authorized -> Captured,
                    refund: Captured -> Refunded,
                }

                triggers {
                    FraudAlert: Authorized -> Declined,
                }

                roles {
                    Merchant { authorize, capture }
                    Compliance { refund }
                }
            }

            Payment :: {
                fn authorize(self) -> Authorized { Authorized {} }
                fn capture(self) -> Captured { Captured {} }
                fn refund(self) -> Refunded { Refunded {} }
            }

            machine PaymentService hosts Payment(key: id) {
                fn new() -> Self { Self {} }

                trigger FraudAlert(payment) {
                    payment;
                    Declined {}
                }

                on FraudAlert(event) {
                    let _result = self.deliver(event.payment_id, event);
                }
            }

            @machines
            fn main() -> () | MachineError | SessionError {
                let service = PaymentService::spawn()?;

                // Stage 1: checkout creates the payment.
                let created = service.create(Payment as Merchant)?;
                let payment_id = created.id;
                println("checkout: payment created");

                // Stage 2: gateway authorizes it.
                let payment = service.resume(Payment as Merchant, payment_id)?;
                match payment {
                    Payment::Created(_) => {
                        let _authorized = payment.authorize()?;
                        println("gateway: authorized");
                    },
                    _ => println("gateway: unexpected state"),
                };

                // Stage 3: fraud service sends an alert.
                service.send(FraudAlert { payment_id })?;
                // Process the mailbox so the on handler + trigger fires.
                __mc_machine_runtime_step_u64(__mc_machine_runtime_managed_current_u64());
                println("fraud: alert sent");

                // Stage 4: merchant tries to capture — but the fraud alert
                // already moved the payment to Declined.
                let payment = service.resume(Payment as Merchant, payment_id)?;
                match payment {
                    Payment::Authorized(_) => {
                        let _captured = payment.capture()?;
                        println("merchant: captured");
                    },
                    Payment::Declined(_) => println("merchant: cannot capture, payment was declined"),
                    _ => println("merchant: unexpected state"),
                };

                ()
            }
        "#,
    );

    assert_eq!(
        run.status.code(),
        Some(0),
        "multi-actor payment lifecycle should compile and run, stderr: {}",
        String::from_utf8_lossy(&run.stderr)
    );
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert_eq!(
        stdout,
        "checkout: payment created\ngateway: authorized\nfraud: alert sent\nmerchant: cannot capture, payment was declined\n",
        "multi-actor payment lifecycle output mismatch"
    );
}
