use crate::core::api::{
    FrontendPolicy, ResolveInputs, elaborate_stage, parse_module_with_id_gen,
    resolve_stage_with_policy, semcheck_stage, typecheck_stage_with_policy,
};
use crate::core::ast::*;
use crate::core::context::ParsedContext;
use crate::core::resolve::ResolveErrorKind;

fn parsed_context(source: &str) -> ParsedContext {
    let id_gen = NodeIdGen::new();
    let (module, id_gen) =
        parse_module_with_id_gen(source, id_gen).expect("parse should succeed for test source");
    ParsedContext::new(module, id_gen)
}

fn resolve_errors(source: &str) -> Vec<crate::core::resolve::ResolveError> {
    let parsed = parsed_context(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(
        out.context.is_none(),
        "strict resolve should stop on validation errors"
    );
    out.errors
}

#[test]
fn resolve_policy_strict_vs_partial() {
    let source = r#"
fn main() -> u64 {
    missing()
}
"#;
    let parsed = parsed_context(source);

    let strict = resolve_stage_with_policy(
        parsed.clone(),
        ResolveInputs::default(),
        FrontendPolicy::Strict,
    );
    assert!(strict.has_errors());
    assert!(strict.context.is_none());

    let partial =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Partial);
    assert!(partial.has_errors());
    assert!(partial.context.is_some());
}

#[test]
fn linear_type_missing_states_reports_targeted_error() {
    let errors = resolve_errors(
        r#"
        @linear
        type Door = {
            actions {
                open: Closed -> Open,
            }
        }
        "#,
    );

    assert!(
        errors.iter().any(
            |err| matches!(err.kind(), ResolveErrorKind::LinearNoStates(name) if name == "Door")
        )
    );
}

#[test]
fn linear_type_unknown_role_action_reports_targeted_error() {
    let errors = resolve_errors(
        r#"
        @linear
        type PullRequest = {
            states {
                Draft,
                Review,
            }

            actions {
                submit: Draft -> Review,
            }

            roles {
                Author { submit, approve }
            }
        }
        "#,
    );

    assert!(errors.iter().any(|err| {
        matches!(
            err.kind(),
            ResolveErrorKind::LinearUnknownActionInRole(ty, role, action)
                if ty == "PullRequest" && role == "Author" && action == "approve"
        )
    }));
}

#[test]
fn linear_resolve_succeeds() {
    let parsed = parsed_context(
        r#"
        @linear
        type Door = {
            states { Closed, Open }
            actions { open: Closed -> Open }
        }

        Door :: {
            fn open(self) -> Open {
                Open {}
            }
        }
        "#,
    );

    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let _resolved = out
        .context
        .expect("expected resolved context for linear program without legacy defs");
}

#[test]
fn linear_type_ambiguous_receiver_reports_targeted_error() {
    let errors = resolve_errors(
        r#"
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
        }
        "#,
    );

    assert!(errors.iter().any(|err| {
        matches!(
            err.kind(),
            ResolveErrorKind::LinearMethodAmbiguousReceiver(ty, action)
                if ty == "PullRequest" && action == "comment"
        )
    }));
}

#[test]
fn linear_type_missing_action_method_reports_targeted_error() {
    let errors = resolve_errors(
        r#"
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
        "#,
    );

    assert!(errors.iter().any(|err| {
        matches!(
            err.kind(),
            ResolveErrorKind::LinearMethodMissingAction(ty, action, source)
                if ty == "Door" && action == "open" && source == "Closed"
        )
    }));
}

#[test]
fn machine_host_unknown_type_reports_targeted_error() {
    let errors = resolve_errors(
        r#"
        machine DoorService hosts Door(key: id) {}
        "#,
    );

    assert!(errors.iter().any(|err| {
        matches!(
            err.kind(),
            ResolveErrorKind::MachineHostedTypeUndefined(machine, ty)
                if machine == "DoorService" && ty == "Door"
        )
    }));
}

#[test]
fn machine_host_non_linear_type_reports_targeted_error() {
    let errors = resolve_errors(
        r#"
        type Door = { id: u64 }

        machine DoorService hosts Door(key: id) {}
        "#,
    );

    assert!(errors.iter().any(|err| {
        matches!(
            err.kind(),
            ResolveErrorKind::MachineHostedTypeNotLinear(machine, ty)
                if machine == "DoorService" && ty == "Door"
        )
    }));
}

#[test]
fn machine_host_invalid_key_field_reports_targeted_error() {
    let errors = resolve_errors(
        r#"
        @linear
        type Door = {
            id: u64,

            states { Closed, Open }

            actions { open: Closed -> Open }
        }

        Door :: {
            fn open(self) -> Open { Open {} }
        }

        machine DoorService hosts Door(key: missing) {}
        "#,
    );

    assert!(errors.iter().any(|err| {
        matches!(
            err.kind(),
            ResolveErrorKind::MachineInvalidKeyField(machine, ty, field)
                if machine == "DoorService" && ty == "Door" && field == "missing"
        )
    }));
}

#[test]
fn machine_host_facts_are_recorded_in_resolved_context() {
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
        }
    "#;

    let parsed = parsed_context(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved = out.context.expect("expected resolved context");

    let host = resolved
        .linear_index
        .machine_hosts
        .get("PRService")
        .expect("expected machine host facts");
    assert_eq!(host.hosted_type_name, "PullRequest");
    assert_eq!(host.key_field, "id");

    let linear_ty = resolved
        .linear_index
        .types
        .get("PullRequest")
        .expect("expected linear type facts");
    assert_eq!(linear_ty.initial_state.as_deref(), Some("Draft"));
    assert_eq!(linear_ty.state_names, vec!["Draft", "Review"]);
    assert!(linear_ty.roles.contains_key("Author"));
}

fn elaborate_linear_semantic(source: &str) -> crate::core::context::SemanticContext {
    let parsed = parsed_context(source);
    let resolved =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved_ctx = resolved
        .context
        .expect("resolve should succeed for linear elaborate test");
    let typed = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Strict,
    )
    .context
    .expect("typecheck should succeed for linear elaborate test");
    let sem_checked = semcheck_stage(typed).expect("semcheck should succeed for linear test");
    elaborate_stage(sem_checked)
}

#[test]
fn linear_index_recognizes_derived_interaction_pattern() {
    let source = r#"
        type AuthCheck = {}
        type AuthApproved = { order_id: u64 }
        type AuthDenied = { order_id: u64 }

        @linear
        type Order = {
            id: u64,

            states {
                Draft,
                PendingAuth,
                Confirmed,
                Rejected,
            }

            actions {
                submit: Draft -> PendingAuth,
            }

            triggers {
                AuthApproved: PendingAuth -> Confirmed,
                AuthDenied: PendingAuth -> Rejected,
            }

            roles {
                Buyer { submit }
            }
        }

        Order :: {
            fn submit(self) -> PendingAuth {
                PendingAuth {}
            }
        }

        @linear
        type AuthWorker = {
            id: u64,

            states { Idle }
            roles { System {} }
        }

        machine AuthService hosts AuthWorker(key: id) {
            fn new() -> Self {
                Self {}
            }

            on AuthCheck(_check) {}
        }

        machine OrderService hosts Order(key: id) {
            fields {
                auth_service: Machine<AuthService>,
            }

            fn new(auth_service: Machine<AuthService>) -> Self {
                Self { auth_service: auth_service }
            }

            action submit(draft) -> PendingAuth {
                send(self.auth_service, AuthCheck {});
                draft.submit()
            }

            trigger AuthApproved(pending) {
                Confirmed {}
            }

            trigger AuthDenied(pending) {
                Rejected {}
            }
        }
    "#;

    let parsed = parsed_context(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved = out.context.expect("expected resolved context");

    let interaction = resolved
        .linear_index
        .derived_interactions
        .get(&("OrderService".to_string(), "submit".to_string()))
        .expect("expected derived interaction facts");
    assert_eq!(interaction.request_type_name, "AuthCheck");
    assert_eq!(interaction.waiting_state, "PendingAuth");
    assert_eq!(interaction.reply_types, vec!["AuthApproved", "AuthDenied"]);
}

#[test]
fn linear_index_does_not_recognize_multi_send_override_as_interaction() {
    let source = r#"
        type Audit = {}
        type AuthCheck = {}
        type AuthApproved = { order_id: u64 }

        @linear
        type Order = {
            id: u64,

            states {
                Draft,
                PendingAuth,
                Confirmed,
            }

            actions {
                submit: Draft -> PendingAuth,
            }

            triggers {
                AuthApproved: PendingAuth -> Confirmed,
            }

            roles {
                Buyer { submit }
            }
        }

        Order :: {
            fn submit(self) -> PendingAuth {
                PendingAuth {}
            }
        }

        @linear
        type AuthWorker = {
            id: u64,

            states { Idle }
            roles { System {} }
        }

        machine AuthService hosts AuthWorker(key: id) {
            fn new() -> Self {
                Self {}
            }

            on AuthCheck(_check) {}
            on Audit(_audit) {}
        }

        machine OrderService hosts Order(key: id) {
            fields {
                auth_service: Machine<AuthService>,
            }

            fn new(auth_service: Machine<AuthService>) -> Self {
                Self { auth_service: auth_service }
            }

            action submit(draft) -> PendingAuth {
                send(self.auth_service, AuthCheck {});
                send(self.auth_service, Audit {});
                draft.submit()
            }

            trigger AuthApproved(pending) {
                Confirmed {}
            }
        }
    "#;

    let parsed = parsed_context(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved = out.context.expect("expected resolved context");

    assert!(
        !resolved
            .linear_index
            .derived_interactions
            .contains_key(&("OrderService".to_string(), "submit".to_string())),
        "multi-send overrides should stay plain V1 send semantics"
    );
}

#[test]
fn linear_index_does_not_recognize_mixed_exit_state_as_interaction() {
    let source = r#"
        type AuthCheck = {}
        type AuthApproved = { order_id: u64 }

        @linear
        type Order = {
            id: u64,

            states {
                Draft,
                PendingAuth,
                Confirmed,
            }

            actions {
                submit: Draft -> PendingAuth,
                retry: PendingAuth -> PendingAuth,
            }

            triggers {
                AuthApproved: PendingAuth -> Confirmed,
            }

            roles {
                Buyer { submit, retry }
            }
        }

        Order :: {
            fn submit(self) -> PendingAuth {
                PendingAuth {}
            }

            fn retry(self) -> PendingAuth {
                PendingAuth {}
            }
        }

        @linear
        type AuthWorker = {
            id: u64,

            states { Idle }
            roles { System {} }
        }

        machine AuthService hosts AuthWorker(key: id) {
            fn new() -> Self {
                Self {}
            }

            on AuthCheck(_check) {}
        }

        machine OrderService hosts Order(key: id) {
            fields {
                auth_service: Machine<AuthService>,
            }

            fn new(auth_service: Machine<AuthService>) -> Self {
                Self { auth_service: auth_service }
            }

            action submit(draft) -> PendingAuth {
                send(self.auth_service, AuthCheck {});
                draft.submit()
            }

            trigger AuthApproved(pending) {
                Confirmed {}
            }
        }
    "#;

    let parsed = parsed_context(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved = out.context.expect("expected resolved context");

    assert!(
        !resolved
            .linear_index
            .derived_interactions
            .contains_key(&("OrderService".to_string(), "submit".to_string())),
        "states with both actions and triggers should stay plain V1 send semantics"
    );
}

#[test]
fn linear_index_does_not_recognize_no_trigger_wait_state_as_interaction() {
    let source = r#"
        type AuthCheck = {}

        @linear
        type Order = {
            id: u64,

            states {
                Draft,
                PendingAuth,
            }

            actions {
                submit: Draft -> PendingAuth,
            }

            roles {
                Buyer { submit }
            }
        }

        Order :: {
            fn submit(self) -> PendingAuth {
                PendingAuth {}
            }
        }

        @linear
        type AuthWorker = {
            id: u64,

            states { Idle }
            roles { System {} }
        }

        machine AuthService hosts AuthWorker(key: id) {
            fn new() -> Self {
                Self {}
            }

            on AuthCheck(_check) {}
        }

        machine OrderService hosts Order(key: id) {
            fields {
                auth_service: Machine<AuthService>,
            }

            fn new(auth_service: Machine<AuthService>) -> Self {
                Self { auth_service: auth_service }
            }

            action submit(draft) -> PendingAuth {
                send(self.auth_service, AuthCheck {});
                draft.submit()
            }
        }
    "#;

    let parsed = parsed_context(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved = out.context.expect("expected resolved context");

    assert!(
        !resolved
            .linear_index
            .derived_interactions
            .contains_key(&("OrderService".to_string(), "submit".to_string())),
        "waiting states without trigger exits should stay plain V1 send semantics"
    );
}

#[test]
fn elaborate_linear_program_produces_semantic_output() {
    let _semantic = elaborate_linear_semantic(
        r#"
        @linear
        type Door = {
            id: u64,

            states { Closed, Open }
            actions { open: Closed -> Open }
            roles { User { open } }
        }

        Door :: {
            fn open(self) -> Open {
                Open {}
            }
        }

        machine DoorService hosts Door(key: id) {
            fn new() -> Self {
                Self {}
            }
        }
        "#,
    );
}
