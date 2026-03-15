use crate::core::api::{
    FrontendPolicy, ParseModuleOptions, ResolveInputs, elaborate_stage, parse_module_with_id_gen,
    parse_module_with_id_gen_and_options, resolve_stage_with_policy,
    resolve_typecheck_pipeline_with_policy, semcheck_stage, typecheck_stage_with_policy,
};
use crate::core::ast::visit::{self, Visitor};
use crate::core::ast::*;
use crate::core::context::ParsedContext;
use crate::core::machine::request_site::labeled_request_site_key;
use crate::core::plans::MachineEventKeyPlan;
use crate::core::resolve::ResolveErrorKind;
use crate::core::semck::SemCheckErrorKind;
use crate::core::types::Type;

fn parsed_context(source: &str) -> ParsedContext {
    let id_gen = NodeIdGen::new();
    let (module, id_gen) =
        parse_module_with_id_gen(source, id_gen).expect("parse should succeed for test source");
    ParsedContext::new(module, id_gen)
}

fn parsed_context_typestate(source: &str) -> ParsedContext {
    let id_gen = NodeIdGen::new();
    let (module, id_gen) = parse_module_with_id_gen_and_options(
        source,
        id_gen,
        ParseModuleOptions {
            experimental_typestate: true,
        },
    )
    .expect("parse should succeed for typestate test source");
    ParsedContext::new(module, id_gen)
}

fn semcheck_errors_typestate(source: &str) -> Vec<crate::core::semck::SemCheckError> {
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    let typed = out
        .typed_context
        .expect("expected typed context before semcheck in typestate api test");
    semcheck_stage(typed).expect_err("expected semcheck errors for typestate api test")
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
fn resolve_protocol_only_program_keeps_legacy_protocol_tables_empty() {
    let source = r#"
        type Ping = {}

        protocol Net {
            role Client;
            role Server;
            req Client -> Server: Ping => Ping;
        }
    "#;

    let parsed = parsed_context_typestate(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved = out
        .context
        .expect("protocol-only legacy source should still resolve internally");

    assert!(
        resolved.protocol_index.protocols.is_empty(),
        "standalone retired protocol defs should no longer build protocol facts"
    );
    assert!(
        resolved.protocol_index.typestate_bindings.is_empty(),
        "standalone retired protocol defs should not create typestate bindings"
    );
}

#[test]
fn semcheck_protocol_only_program_keeps_legacy_progression_empty() {
    let source = r#"
        type Ping = {}

        protocol Net {
            role Client;
            role Server;
            req Client -> Server: Ping => Ping;
        }
    "#;

    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        !out.has_errors(),
        "protocol-only retired source should still typecheck internally, got resolve={:?} type={:?}",
        out.resolve_errors,
        out.type_errors
    );
    let typed = out
        .typed_context
        .expect("expected typed context for protocol-only semcheck retirement test");
    let sem_checked = semcheck_stage(typed).expect("protocol-only retired source should semcheck");

    assert!(
        sem_checked.protocol_progression.handlers.is_empty(),
        "standalone retired protocol defs should no longer build progression handlers"
    );
    assert!(
        sem_checked.protocol_progression.by_handler_def.is_empty(),
        "standalone retired protocol defs should not index protocol handlers"
    );
    assert!(
        sem_checked.protocol_progression.by_state.is_empty(),
        "standalone retired protocol defs should not index protocol states"
    );
}

#[test]
fn typestate_missing_new_reports_targeted_error() {
    let source = r#"
typestate Connection {
    state Disconnected {}
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(out.context.is_none());
    assert!(out.errors.iter().any(
        |err| matches!(err.kind(), ResolveErrorKind::TypestateMissingNew(name) if name == "Connection")
    ));
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
fn linear_resolve_keeps_legacy_protocol_tables_empty() {
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
    let resolved = out
        .context
        .expect("expected resolved context for linear program without legacy defs");

    assert!(
        resolved.typestate_role_impls.is_empty(),
        "linear programs should not populate legacy typestate role bindings"
    );
    assert!(
        resolved.protocol_index.protocols.is_empty(),
        "linear programs should not build protocol facts by default"
    );
    assert!(
        resolved.protocol_index.typestate_bindings.is_empty(),
        "linear programs should not build typestate protocol bindings by default"
    );
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

#[test]
fn typestate_duplicate_state_and_invalid_transition_return_report_errors() {
    let source = r#"
typestate Connection {
    fn new() -> Disconnected {
        Disconnected {}
    }

    state Disconnected {
        fn connect() -> UnknownState {
            UnknownState {}
        }
    }

    state Disconnected {}
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(out.context.is_none());
    assert!(out.errors.iter().any(
        |err| matches!(err.kind(), ResolveErrorKind::TypestateDuplicateState(ts, state) if ts == "Connection" && state == "Disconnected")
    ));
    assert!(out.errors.iter().any(|err| {
        matches!(err.kind(), ResolveErrorKind::TypestateInvalidTransitionReturn(ts, state, method)
                if ts == "Connection" && state == "Disconnected" && method == "connect"
        )
    }));
}

#[test]
fn typestate_field_constraints_report_errors() {
    let source = r#"
typestate Connection {
    fields { id: u64 }
    fields { retries: u64 }

    fn new() -> Disconnected {
        Disconnected { id: 0, retries: 0 }
    }

    state Disconnected {
        fields { id: u64 }
        fields { fd: u64 }
    }
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(out.context.is_none());
    assert!(out.errors.iter().any(
        |err| matches!(err.kind(), ResolveErrorKind::TypestateDuplicateFieldsBlock(name) if name == "Connection")
    ));
    assert!(out.errors.iter().any(|err| {
        matches!(err.kind(), ResolveErrorKind::TypestateDuplicateStateFieldsBlock(ts, state)
                if ts == "Connection" && state == "Disconnected"
        )
    }));
    assert!(out.errors.iter().any(|err| {
        matches!(err.kind(), ResolveErrorKind::TypestateStateFieldShadowsCarriedField(ts, state, field)
                if ts == "Connection" && state == "Disconnected" && field == "id"
        )
    }));
}

#[test]
fn typestate_duplicate_transition_and_invalid_new_return_report_errors() {
    let source = r#"
typestate Connection {
    fn new() -> UnknownState {
        UnknownState {}
    }

    state Disconnected {
        fn connect(x: u64) -> Disconnected {
            Disconnected {}
        }

        fn connect() -> Disconnected {
            Disconnected {}
        }
    }
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(out.context.is_none());
    assert!(out.errors.iter().any(
        |err| matches!(err.kind(), ResolveErrorKind::TypestateInvalidNewReturn(name) if name == "Connection")
    ));
    assert!(out.errors.iter().any(|err| {
        matches!(err.kind(), ResolveErrorKind::TypestateDuplicateTransition(ts, state, method)
                if ts == "Connection" && state == "Disconnected" && method == "connect"
        )
    }));
}

#[test]
fn typestate_external_state_literal_reports_targeted_error() {
    let source = r#"
typestate Connection {
    fields { retries: u64 }

    fn new() -> Disconnected {
        Disconnected { retries: 0 }
    }

    state Disconnected {}
}

fn main() -> u64 {
    let bad = Disconnected { retries: 1 };
    bad.retries
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(out.context.is_none());
    assert!(out.errors.iter().any(|err| {
        matches!(err.kind(), ResolveErrorKind::TypestateStateLiteralOutsideTypestate(state)
                if state == "Disconnected"
        )
    }));
}

#[test]
fn typestate_invalid_on_handler_return_reports_targeted_error() {
    let source = r#"
type Ping = {}

typestate Connection {
    fn new() -> Disconnected {
        Disconnected {}
    }

    state Disconnected {
        on Ping() -> u64 {
            0
        }
    }
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(out.context.is_none());
    assert!(out.errors.iter().any(|err| {
        matches!(err.kind(), ResolveErrorKind::TypestateInvalidStateOnHandlerReturn(ts, state)
                if ts == "Connection" && state == "Disconnected"
        )
    }));
}

#[test]
fn typestate_on_handler_stay_union_return_is_accepted() {
    let source = r#"
type Ping = {}
type Recoverable = {}

typestate Connection {
    fn new() -> Disconnected {
        Disconnected {}
    }

    state Disconnected {
        on Ping() -> stay | Recoverable {
            if true {
                return Recoverable {};
            };
        }
    }
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        !out.has_errors(),
        "expected `on` handler `stay | Error` return shape to pass, got resolve={:?}, type={:?}",
        out.resolve_errors,
        out.type_errors
    );
}

#[test]
fn typestate_protocol_shape_missing_handler_reports_type_error() {
    let source = r#"
type AuthReq = {}
type AuthOk = {}
type Start = {}

protocol Auth {
    msg Start;
    msg AuthReq;
    msg AuthOk;

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }
        state Awaiting {
            on AuthOk@Server -> Idle;
        }
    }
    role Server {
        state Ready {
            on AuthReq@Client -> Ready {
                effects: [ AuthOk ~> Client ]
            }
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {}
}

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
    }

    fn new(server: Machine<AuthServer>) -> Idle {
        Idle { server: server }
    }

    state Idle {
        on Start() -> Awaiting {
            emit Send(to: self.server, AuthReq {});
            Awaiting { server: self.server }
        }
    }

    state Awaiting {}
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    let resolved = out
        .resolved_context
        .as_ref()
        .expect("expected resolved context for protocol conformance test");
    assert_eq!(resolved.typestate_role_impls.len(), 1);
    assert!(resolved.typestate_role_impls[0].role_def_id.is_some());
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::ProtocolStateHandlerMissing(ts, role, state, _)
                    if ts == "Gateway" && role == "Auth::Client" && state == "Awaiting"
            )
        }),
        "expected missing handler protocol conformance error, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_protocol_shape_outgoing_payload_violation_reports_type_error() {
    let source = r#"
type AuthReq = {}
type AuthOk = {}
type Other = {}
type Start = {}

protocol Auth {
    msg Start;
    msg AuthReq;
    msg AuthOk;

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }
        state Awaiting {
            on AuthOk@Server -> Idle;
        }
    }
    role Server {
        state Ready {
            on AuthReq@Client -> Ready {
                effects: [ AuthOk ~> Client ]
            }
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {}
}

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
    }

    fn new(server: Machine<AuthServer>) -> Idle {
        Idle { server: server }
    }

    state Idle {
        on Start() -> Awaiting {
            emit Send(to: self.server, Other {});
            Awaiting { server: self.server }
        }
    }

    state Awaiting {
        on AuthOk() -> Idle {
            Idle { server: self.server }
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::ProtocolStateOutgoingPayloadNotAllowed(ts, role, state, _)
                    if ts == "Gateway" && role == "Auth::Client" && state == "Idle"
            )
        }),
        "expected outgoing payload protocol conformance error, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_protocol_shape_accepts_valid_handler_and_outgoing_payload() {
    let source = r#"
type AuthReq = {}
type AuthOk = {}
type Start = {}

protocol Auth {
    msg Start;
    msg AuthReq;
    msg AuthOk;

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }
        state Awaiting {
            on AuthOk@Server -> Idle;
        }
    }
    role Server {
        state Ready {
            on AuthReq@Client -> Ready {
                effects: [ AuthOk ~> Client ]
            }
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {}
}

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
    }

    fn new(server: Machine<AuthServer>) -> Idle {
        Idle { server: server }
    }

    state Idle {
        on Start() -> Awaiting {
            emit Send(to: self.server, AuthReq {});
            Awaiting { server: self.server }
        }
    }

    state Awaiting {
        on AuthOk() -> Idle {
            Idle { server: self.server }
        }
    }
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        !out.has_errors(),
        "expected valid protocol shape conformance, got resolve={:?} type={:?}",
        out.resolve_errors,
        out.type_errors
    );
}

#[test]
fn typestate_protocol_shape_uses_transition_surface_for_conformance() {
    let source = r#"
type Start = {}
type AuthReq = {}
type AuthOk = {}

protocol Auth {
    msg Start;
    msg AuthReq;
    msg AuthOk;
    req Client -> Server: AuthReq => AuthOk;

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }
        state Awaiting {
            on AuthOk@Server -> Idle;
        }
    }

    role Server {
        state Ready {
            on AuthReq@Client -> Ready {
                effects: [ AuthOk ~> Client ]
            }
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {}
}

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
    }

    fn new(server: Machine<AuthServer>) -> Idle {
        Idle { server: server }
    }

    state Idle {
        fn request() -> Idle {
            emit Send(to: 0, AuthReq {});
            Idle {}
        }
    }
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    let resolved = out
        .resolved_context
        .as_ref()
        .expect("expected resolved context for transition-surface protocol test");
    let protocols = resolved.module.protocol_defs();
    let protocol = protocols.first().expect("expected protocol definition");
    assert_eq!(protocol.messages.len(), 3);
    assert_eq!(protocol.request_contracts.len(), 1);
    assert_eq!(protocol.roles.len(), 2);
    assert!(!protocol.roles[0].states.is_empty());
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::ProtocolStateHandlerMissing(ts, role, state, _)
                    if ts == "Gateway" && role == "Auth::Client" && state == "Awaiting"
            )
        }),
        "expected missing handler conformance error from transition surface, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_protocol_shape_state_local_outgoing_violation_reports_type_error() {
    let source = r#"
type Start = {}
type AuthReq = {}
type AuthOk = {}
type Other = {}

protocol Auth {
    msg Start;
    msg AuthReq;
    msg AuthOk;

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }
        state Awaiting {
            on AuthOk@Server -> Idle;
        }
    }

    role Server {
        state Ready {
            on AuthReq@Client -> Ready {
                effects: [ AuthOk ~> Client ]
            }
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {}
}

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
    }

    fn new(server: Machine<AuthServer>) -> Idle {
        Idle { server: server }
    }

    state Idle {
        on Start() -> Awaiting {
            emit Send(to: 0, Other {});
            Awaiting { server: self.server }
        }
    }

    state Awaiting {
        on AuthOk() -> Idle {
            Idle { server: self.server }
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::ProtocolStateOutgoingPayloadNotAllowed(ts, role, state, _)
                    if ts == "Gateway" && role == "Auth::Client" && state == "Idle"
            )
        }),
        "expected state-local outgoing payload protocol conformance error, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_protocol_shape_accepts_valid_state_local_conformance() {
    let source = r#"
type Start = {}
type AuthReq = {}
type AuthOk = {}

protocol Auth {
    msg Start;
    msg AuthReq;
    msg AuthOk;

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }
        state Awaiting {
            on AuthOk@Server -> Idle;
        }
    }

    role Server {
        state Ready {
            on AuthReq@Client -> Ready {
                effects: [ AuthOk ~> Client ]
            }
        }
    }
}

typestate AuthServer {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {}
}

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
    }

    fn new(server: Machine<AuthServer>) -> Idle {
        Idle { server: server }
    }

    state Idle {
        on Start() -> Awaiting {
            emit Send(to: self.server, AuthReq {});
            Awaiting { server: self.server }
        }
    }

    state Awaiting {
        on AuthOk() -> Idle {
            Idle { server: self.server }
        }
    }
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        !out.has_errors(),
        "expected valid state-local protocol conformance, got resolve={:?} type={:?}",
        out.resolve_errors,
        out.type_errors
    );
}

#[test]
fn typestate_protocol_emit_destination_role_mismatch_reports_type_error() {
    let source = r#"
type Start = {}
type AuthReq = {}
type AuthOk = {}

protocol Auth {
    msg Start;
    msg AuthReq;
    msg AuthOk;

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }
        state Awaiting {
            on AuthOk@Server -> Idle;
        }
    }
    role Server {
        state Ready {
            on AuthReq@Client -> Ready {
                effects: [ AuthOk ~> Client ]
            }
        }
    }
    role Audit {
        state Sink {}
    }
}

typestate AuthServer { fn new() -> Ready { Ready {} } state Ready {} }
typestate AuditSink { fn new() -> Sink { Sink {} } state Sink {} }

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
        audit: Machine<AuditSink> as Audit,
    }

    fn new(server: Machine<AuthServer>, audit: Machine<AuditSink>) -> Idle {
        Idle { server: server, audit: audit }
    }

    state Idle {
        on Start() -> Awaiting {
            emit Send(to: self.audit, AuthReq {});
            Awaiting { server: self.server, audit: self.audit }
        }
    }

    state Awaiting {
        on AuthOk() -> Idle {
            Idle { server: self.server, audit: self.audit }
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::ProtocolStateEmitDestinationRoleMismatch(
                    ts, role, state, _, expected, field, bound, ..
                )
                    if ts == "Gateway"
                        && role == "Auth::Client"
                        && state == "Idle"
                        && expected == "Server"
                        && field == "audit"
                        && bound == "Audit"
            )
        }),
        "expected destination role mismatch diagnostic, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_protocol_method_send_destination_role_mismatch_reports_type_error() {
    let source = r#"
type Start = {}
type AuthReq = {}
type AuthOk = {}

protocol Auth {
    msg Start;
    msg AuthReq;
    msg AuthOk;

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Audit ]
            }
        }
        state Awaiting {
            on AuthOk@Server -> Idle;
        }
    }
    role Server {
        state Ready {}
    }
    role Audit {
        state Sink {
            on AuthReq@Client -> Sink;
        }
    }
}

typestate AuthServer {
    fn new() -> Ready { Ready {} }
    state Ready {
        on AuthReq() -> Ready { Ready {} }
    }
}
typestate AuditSink { fn new() -> Sink { Sink {} } state Sink {} }

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
        audit: Machine<AuditSink> as Audit,
    }

    fn new(server: Machine<AuthServer>, audit: Machine<AuditSink>) -> Idle {
        Idle { server: server, audit: audit }
    }

    state Idle {
        on Start() -> Awaiting {
            self.server.send(AuthReq {});
            Awaiting { server: self.server, audit: self.audit }
        }
    }

    state Awaiting {
        on AuthOk() -> Idle {
            Idle { server: self.server, audit: self.audit }
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::ProtocolStateEmitDestinationRoleMismatch(
                    ts, role, state, _, expected, field, bound, ..
                )
                    if ts == "Gateway"
                        && role == "Auth::Client"
                        && state == "Idle"
                        && expected == "Audit"
                        && field == "server"
                        && bound == "Server"
            )
        }),
        "expected destination role mismatch diagnostic for handle send, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_protocol_request_contract_rejects_extra_response_variant() {
    let source = r#"
type Start = {}
type AuthReq = {}
type AuthOk = {}
type AuthErr = {}

protocol Auth {
    msg Start;
    msg AuthReq;
    msg AuthOk;
    msg AuthErr;
    req Client -> Server: AuthReq => AuthOk;

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }
        state Awaiting {
            on AuthOk@Server -> Idle;
            on AuthErr@Server -> Idle;
        }
    }
    role Server {
        state Ready {
            on AuthReq@Client -> Ready {
                effects: [ AuthOk ~> Client ]
            }
        }
    }
}

typestate AuthServer { fn new() -> Ready { Ready {} } state Ready {} }

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
    }

    fn new(server: Machine<AuthServer>) -> Idle {
        Idle { server: server }
    }

    state Idle {
        on Start() -> Awaiting {
            let p: Pending<AuthOk | AuthErr> = emit Request(to: self.server, AuthReq {});
            p;
            Awaiting { server: self.server }
        }
    }

    state Awaiting {
        on AuthOk() -> Idle {
            Idle { server: self.server }
        }
        on AuthErr() -> Idle {
            Idle { server: self.server }
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::ProtocolRequestResponseNotInContract(
                    ts,
                    role,
                    _,
                    to_role,
                    _,
                    ..
                )
                    if ts == "Gateway" && role == "Auth::Client" && to_role == "Server"
            )
        }),
        "expected contract response-set violation diagnostic, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_protocol_request_contract_ambiguous_reports_type_error() {
    let source = r#"
type Start = {}
type AuthReq = {}
type AuthOk = {}
type AuthErr = {}

protocol Auth {
    msg Start;
    msg AuthReq;
    msg AuthOk;
    msg AuthErr;
    req Client -> Server: AuthReq => AuthOk;
    req Client -> Server: AuthReq => AuthErr;

    role Client {
        state Idle {
            on Start -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }
        state Awaiting {
            on AuthOk@Server -> Idle;
            on AuthErr@Server -> Idle;
        }
    }
    role Server {
        state Ready {
            on AuthReq@Client -> Ready {
                effects: [ AuthOk ~> Client ]
            }
        }
    }
}

typestate AuthServer { fn new() -> Ready { Ready {} } state Ready {} }

typestate Gateway : Auth::Client {
    fields {
        server: Machine<AuthServer> as Server,
    }

    fn new(server: Machine<AuthServer>) -> Idle {
        Idle { server: server }
    }

    state Idle {
        on Start() -> Awaiting {
            let p: Pending<AuthOk | AuthErr> = emit Request(to: self.server, AuthReq {});
            p;
            Awaiting { server: self.server }
        }
    }

    state Awaiting {
        on AuthOk() -> Idle {
            Idle { server: self.server }
        }
        on AuthErr() -> Idle {
            Idle { server: self.server }
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::ProtocolRequestContractAmbiguous(ts, role, _, to_role)
                    if ts == "Gateway" && role == "Auth::Client" && to_role == "Server"
            )
        }),
        "expected ambiguous request-contract diagnostic, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_protocol_reply_payload_must_be_allowed_by_state_effects() {
    let source = r#"
type AuthReq = {}
type AuthOk = {}

protocol Auth {
    msg AuthReq;
    msg AuthOk;

    role Client {
        state Idle {}
    }
    role Server {
        state Ready {
            on AuthReq@Client -> Ready;
        }
    }
}

typestate Gateway : Auth::Server {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on AuthReq(req: AuthReq, cap: ReplyCap<AuthOk>) -> Ready {
            req;
            reply(cap, AuthOk {});
            Ready {}
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::ProtocolStateOutgoingPayloadNotAllowed(ts, role, state, _)
                    if ts == "Gateway" && role == "Auth::Server" && state == "Ready"
            )
        }),
        "expected protocol outgoing-payload violation for reply, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_reply_cap_rejects_double_consume() {
    let source = r#"
type AuthReq = {}
type AuthOk = {}

typestate Gateway {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on AuthReq(req: AuthReq, cap: ReplyCap<AuthOk>) -> Ready {
            reply(cap, AuthOk {});
            reply(cap, AuthOk {});
            Ready {}
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::ReplyCapConsumedMultipleTimes(name) if name == "cap"
            )
        }),
        "expected double-consume reply cap error, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_reply_cap_requires_consumption_on_all_paths() {
    let source = r#"
type AuthReq = { allow: bool }
type AuthOk = {}

typestate Gateway {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on AuthReq(req: AuthReq, cap: ReplyCap<AuthOk>) -> Ready {
            if req.allow {
                reply(cap, AuthOk {});
            } else {
                // missing reply
            };
            Ready {}
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::ReplyCapMustBeConsumed(name) if name == "cap"
            )
        }),
        "expected missing-consume reply cap error, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_reply_cap_rejects_payload_outside_response_set() {
    let source = r#"
type AuthReq = {}
type AuthOk = {}
type AuthErr = {}

typestate Gateway {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on AuthReq(req: AuthReq, cap: ReplyCap<AuthOk>) -> Ready {
            reply(cap, AuthErr {});
            Ready {}
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors
            .iter()
            .any(|e| matches!(e.kind(), SemCheckErrorKind::ReplyPayloadNotAllowed(_, _))),
        "expected invalid reply payload error, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_reply_outside_handler_is_rejected() {
    let source = r#"
type AuthOk = {}

typestate Gateway {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        fn bad() -> Ready {
            reply(0, AuthOk {});
            Ready {}
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors
            .iter()
            .any(|e| matches!(e.kind(), SemCheckErrorKind::ReplyOutsideHandler)),
        "expected reply outside handler error, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_reply_cap_all_paths_consumed_is_accepted() {
    let source = r#"
type AuthReq = { allow: bool }
type AuthOk = {}
type AuthErr = {}

typestate Gateway {
    fn new() -> Ready {
        Ready {}
    }

    state Ready {
        on AuthReq(req: AuthReq, cap: ReplyCap<AuthOk | AuthErr>) -> Ready {
            if req.allow {
                reply(cap, AuthOk {});
            } else {
                reply(cap, AuthErr {});
            };
            Ready {}
        }
    }
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        !out.has_errors(),
        "expected valid reply cap usage, got resolve={:?} type={:?}",
        out.resolve_errors,
        out.type_errors
    );
}

#[test]
fn typestate_pattern_on_handlers_overlap_is_rejected() {
    let source = r#"
type Response = {}
type AuthApproved = {}
type AuthDenied = {}

typestate Connection {
    fn new() -> AwaitAuth {
        AwaitAuth {}
    }

    state AwaitAuth {
        on Response(pending, AuthApproved) -> AwaitAuth {
            AwaitAuth {}
        }

        on Response(pending, AuthApproved) -> AwaitAuth {
            AwaitAuth {}
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::TypestateOverlappingOnHandlers(ts, state, _, _)
                    if ts == "Connection" && state == "AwaitAuth"
            )
        }),
        "expected overlapping typestate on-handler error, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_pattern_on_handlers_disjoint_are_accepted() {
    let source = r#"
type Response = {}
type AuthApproved = {}
type AuthDenied = {}

typestate Connection {
    fn new() -> AwaitAuth {
        AwaitAuth {}
    }

    state AwaitAuth {
        on Response(pending, AuthApproved) -> AwaitAuth {
            AwaitAuth {}
        }

        on Response(pending, AuthDenied) -> AwaitAuth {
            AwaitAuth {}
        }
    }
}
"#;
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        !out.has_errors(),
        "expected disjoint typestate response handlers to be accepted, got resolve={:?} type={:?}",
        out.resolve_errors,
        out.type_errors
    );
}

#[test]
fn typestate_for_provenance_with_distinct_site_labels_is_accepted() {
    let source = r#"
type AuthCheck = {}
type AuthApproved = {}

typestate Connection {
    fn new() -> AwaitAuth {
        AwaitAuth {}
    }

    state AwaitAuth {
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        !out.has_errors(),
        "expected labeled provenance handlers to disambiguate overlap, got resolve={:?} type={:?}",
        out.resolve_errors,
        out.type_errors
    );
}

#[test]
fn typestate_for_provenance_label_mixture_is_rejected_as_ambiguous() {
    let source = r#"
type AuthCheck = {}
type AuthApproved = {}

typestate Connection {
    fn new() -> AwaitAuth {
        AwaitAuth {}
    }

    state AwaitAuth {
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
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::TypestateAmbiguousResponseProvenance(
                    ts,
                    state,
                    _,
                    _,
                ) if ts == "Connection" && state == "AwaitAuth"
            )
        }),
        "expected ambiguous provenance diagnostic for mixed labeled/unlabeled handlers, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_for_provenance_missing_response_variant_is_rejected() {
    let source = r#"
type Start = {}
type AuthCheck = {}
type AuthApproved = {}
type AuthDenied = {}

typestate Connection {
    fn new() -> AwaitAuth {
        AwaitAuth {}
    }

    state AwaitAuth {
        on Start(start) -> stay {
            let pending: Pending<AuthApproved | AuthDenied> = request(0, AuthCheck {});
            pending;
            start;
        }

        on AuthApproved(ok) for AuthCheck(req) -> stay {
            ok;
            req;
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::TypestateRequestMissingResponseHandler(ts, _, _, _)
                    if ts == "Connection"
            )
        }),
        "expected missing response-variant diagnostic, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_for_provenance_unsupported_response_variant_is_rejected() {
    let source = r#"
type Start = {}
type AuthCheck = {}
type AuthApproved = {}
type AuthDenied = {}

typestate Connection {
    fn new() -> AwaitAuth {
        AwaitAuth {}
    }

    state AwaitAuth {
        on Start(start) -> stay {
            let pending: Pending<AuthApproved> = request:auth1(0, AuthCheck {});
            pending;
            start;
        }

        on AuthDenied(err) for AuthCheck:auth1(req) -> stay {
            err;
            req;
        }
    }
}
"#;
    let sem_errors = semcheck_errors_typestate(source);
    assert!(
        sem_errors.iter().any(|e| {
            matches!(
                e.kind(),
                SemCheckErrorKind::TypestateHandlerUnsupportedResponseVariant(ts, _, _, _)
                    if ts == "Connection"
            )
        }),
        "expected unsupported response-variant diagnostic, got {:?}",
        sem_errors
    );
}

#[test]
fn typestate_handler_surface_sugar_normalizes_before_resolve() {
    let source = r#"
type Ping = {}
type Pong = {}

typestate M {
    fn new() -> S { S {} }

    state S {
        on Ping(p) -> stay {
            p;
            send(0, Ping {});
            request(0, Ping {});
        }

        on Pong {
        }
    }
}
"#;

    let parsed = parsed_context_typestate(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(
        out.errors.is_empty(),
        "expected typestate sugar to resolve cleanly, got {:?}",
        out.errors
    );
    let resolved = out
        .context
        .expect("resolve should succeed for typestate sugar normalization test");

    let mut found_handler = false;
    let mut aggregate = HandlerCommandNormalizationFinder::default();
    for item in &resolved.module.top_level_items {
        let TopLevelItem::MethodBlock(block) = item else {
            continue;
        };
        if !block.type_name.starts_with("__ts_") {
            continue;
        }
        for method_item in &block.method_items {
            let MethodItem::Def(method) = method_item else {
                continue;
            };
            if !method.sig.name.starts_with("__ts_on_") {
                continue;
            }
            found_handler = true;
            assert!(matches!(
                method.sig.ret_ty_expr.kind,
                TypeExprKind::Named { ref ident, .. } if ident != "stay"
            ));
            aggregate.visit_expr(&method.body);
        }
    }

    assert!(
        found_handler,
        "expected lowered typestate handler method in resolved module"
    );
    assert!(
        aggregate.emit_send_seen,
        "expected send(...) shorthand to normalize into emit send"
    );
    assert!(
        aggregate.emit_request_seen,
        "expected request(...) shorthand to normalize into emit request"
    );
    assert!(
        !aggregate.sugar_send_call_seen && !aggregate.sugar_request_call_seen,
        "expected no raw send/request call sugar after typestate normalization"
    );
}

#[test]
fn typestate_for_provenance_lowers_to_hidden_pending_and_origin_param() {
    let source = r#"
type AuthCheck = {}
type AuthApproved = {}

typestate Connection {
    fn new() -> AwaitAuth { AwaitAuth {} }

    state AwaitAuth {
        on AuthApproved(ok) for AuthCheck(req) -> stay {
            ok;
            req;
        }
    }
}
"#;

    let parsed = parsed_context_typestate(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(
        out.errors.is_empty(),
        "expected provenance handler sugar to resolve cleanly, got {:?}",
        out.errors
    );
    let resolved = out
        .context
        .expect("resolve should succeed for provenance lowering test");

    let handler = resolved
        .module
        .method_blocks()
        .iter()
        .filter(|block| block.type_name.starts_with("__ts_"))
        .flat_map(|block| &block.method_items)
        .filter_map(|item| match item {
            MethodItem::Def(def) if def.sig.name.starts_with("__ts_on_") => Some(def),
            _ => None,
        })
        .next()
        .expect("expected lowered typestate handler");

    let names = handler
        .sig
        .params
        .iter()
        .map(|param| param.ident.as_str())
        .collect::<Vec<_>>();
    assert_eq!(names, vec!["__event", "__pending", "req", "ok"]);
    assert!(
        matches!(handler.sig.params[1].typ.kind, TypeExprKind::Named { ref ident, .. } if ident == "Pending"),
        "expected hidden pending parameter in lowered provenance handler"
    );
    assert!(
        matches!(handler.sig.params[2].typ.kind, TypeExprKind::Named { ref ident, .. } if ident == "AuthCheck"),
        "expected provenance binding parameter type to be preserved"
    );
}

#[test]
fn typestate_for_provenance_sets_thunk_origin_param_index() {
    let source = r#"
type AuthCheck = {}
type AuthApproved = {}

typestate Connection {
    fn new() -> AwaitAuth { AwaitAuth {} }

    state AwaitAuth {
        on AuthApproved(ok) for AuthCheck(req) -> stay {
            ok;
            req;
        }
    }
}
"#;

    let parsed = parsed_context_typestate(source);
    let resolved =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved_ctx = resolved
        .context
        .expect("resolve should succeed for provenance plan test");
    let typed = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Strict,
    )
    .context
    .expect("typecheck should succeed for provenance plan test");
    let sem_checked =
        semcheck_stage(typed).expect("semcheck should succeed for provenance plan test");
    let elaborated = elaborate_stage(sem_checked);

    let handler_def_id = elaborated
        .module
        .method_blocks()
        .iter()
        .filter(|block| block.type_name.starts_with("__ts_"))
        .flat_map(|block| &block.method_items)
        .find_map(|item| match item {
            MethodItem::Def(def) if def.sig.name.starts_with("__ts_on_") => {
                Some(elaborated.def_table.def_id(def.id))
            }
            _ => None,
        })
        .expect("expected lowered typestate handler");
    let thunk = elaborated
        .machine_plans
        .thunks
        .get(&handler_def_id)
        .expect("expected dispatch thunk plan for handler");
    assert_eq!(
        thunk.provenance_param_index,
        Some(2),
        "expected provenance binding to map to param index 2"
    );
}

#[test]
fn typestate_for_provenance_label_maps_to_site_specific_dispatch_row() {
    let source = r#"
type AuthCheck = {}
type AuthApproved = {}

typestate Connection {
    fn new() -> AwaitAuth { AwaitAuth {} }

    state AwaitAuth {
        on AuthApproved(ok) for AuthCheck:auth1(req) -> stay {
            ok;
            req;
        }
    }
}
"#;

    let parsed = parsed_context_typestate(source);
    let resolved =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved_ctx = resolved
        .context
        .expect("resolve should succeed for provenance label dispatch-row test");
    let typed = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Strict,
    )
    .context
    .expect("typecheck should succeed for provenance label dispatch-row test");
    let sem_checked = semcheck_stage(typed)
        .expect("semcheck should succeed for provenance label dispatch-row test");
    let elaborated = elaborate_stage(sem_checked);

    let descriptor = elaborated
        .machine_plans
        .descriptors
        .get("Connection")
        .expect("expected Connection machine descriptor plan");
    let expected_site = labeled_request_site_key("auth1");
    assert!(
        descriptor
            .dispatch_table
            .iter()
            .any(|row| row.request_site_key == Some(expected_site)),
        "expected site-specific dispatch row for auth1 provenance label"
    );
}

#[test]
fn typestate_spawn_mirrors_constructor_params_and_forwards_call() {
    let source = r#"
typestate Connection {
    fn new(addr: string, retries: u64) -> Disconnected {
        Disconnected {}
    }

    state Disconnected {}
}
"#;

    let parsed = parsed_context_typestate(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(
        out.errors.is_empty(),
        "expected typestate spawn lowering to resolve cleanly, got {:?}",
        out.errors
    );
    let resolved = out
        .context
        .expect("resolve should succeed for typestate spawn contract test");

    let mut ctor: Option<&FuncDef> = None;
    let mut spawn: Option<&FuncDef> = None;
    for item in &resolved.module.top_level_items {
        let TopLevelItem::FuncDef(func) = item else {
            continue;
        };
        if func.sig.name == "__ts_ctor_Connection" {
            ctor = Some(func);
        } else if func.sig.name == "__ts_spawn_Connection" {
            spawn = Some(func);
        }
    }
    let ctor = ctor.expect("expected generated typestate constructor");
    let spawn = spawn.expect("expected generated typestate spawn function");

    assert_eq!(ctor.sig.params.len(), 2);
    assert_eq!(spawn.sig.params.len(), ctor.sig.params.len());
    assert_eq!(spawn.sig.params[0].ident, "addr");
    assert_eq!(spawn.sig.params[1].ident, "retries");

    let mut finder = SpawnForwardCtorCallFinder::default();
    finder.visit_expr(&spawn.body);
    assert!(
        finder.forwards_ctor_call,
        "expected spawn body to forward constructor args through __ts_ctor_Connection call"
    );
}

#[test]
fn machines_entrypoint_injects_managed_runtime_calls_without_prelude() {
    let source = r#"
@machines
fn main() {}
"#;

    let parsed = parsed_context_typestate(source);
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(
        out.errors.is_empty(),
        "expected @machines rewrite to resolve cleanly, got {:?}",
        out.errors
    );
    let resolved = out
        .context
        .expect("resolve should succeed for machines entrypoint rewrite");

    let mut found_main = false;
    let mut found_bootstrap_call = false;
    for item in &resolved.module.top_level_items {
        let TopLevelItem::FuncDef(func) = item else {
            continue;
        };
        if func.sig.name != "main" {
            continue;
        }
        found_main = true;
        let mut finder = BootstrapCallFinder::default();
        finder.visit_expr(&func.body);
        found_bootstrap_call = finder.found;
        break;
    }

    assert!(found_main, "expected rewritten main function");
    assert!(
        found_bootstrap_call,
        "expected managed runtime bootstrap call in rewritten main body"
    );
}

#[derive(Default)]
struct HandlerCommandNormalizationFinder {
    sugar_send_call_seen: bool,
    sugar_request_call_seen: bool,
    emit_send_seen: bool,
    emit_request_seen: bool,
}

impl Visitor for HandlerCommandNormalizationFinder {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Call { callee, .. } => {
                if let ExprKind::Var { ident, .. } = &callee.kind {
                    if ident == "send" {
                        self.sugar_send_call_seen = true;
                    } else if ident == "request" {
                        self.sugar_request_call_seen = true;
                    }
                }
            }
            ExprKind::Emit { kind } => match kind {
                EmitKind::Send { .. } => self.emit_send_seen = true,
                EmitKind::Request { .. } => self.emit_request_seen = true,
            },
            _ => {}
        }
        visit::walk_expr(self, expr);
    }
}

#[derive(Default)]
struct SpawnForwardCtorCallFinder {
    forwards_ctor_call: bool,
}

impl Visitor for SpawnForwardCtorCallFinder {
    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Call { callee, args } = &expr.kind
            && let ExprKind::Var { ident, .. } = &callee.kind
            && ident == "__ts_ctor_Connection"
            && args.len() == 2
        {
            self.forwards_ctor_call = true;
        }
        visit::walk_expr(self, expr);
    }
}

#[derive(Default)]
struct BootstrapCallFinder {
    found: bool,
}

impl Visitor for BootstrapCallFinder {
    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Call { callee, .. } = &expr.kind
            && let ExprKind::Var { ident, .. } = &callee.kind
            && ident == "__mc_machine_runtime_managed_bootstrap_u64"
        {
            self.found = true;
        }
        visit::walk_expr(self, expr);
    }
}

#[test]
fn typecheck_policy_strict_vs_partial() {
    let source = r#"
fn main() -> u64 {
    true
}
"#;
    let parsed = parsed_context(source);
    let resolved =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved_ctx = resolved
        .context
        .expect("resolve should succeed for typecheck policy test");

    let strict = typecheck_stage_with_policy(
        resolved_ctx.clone(),
        resolved.imported_facts.clone(),
        FrontendPolicy::Strict,
    );
    assert!(strict.has_errors());
    assert!(strict.context.is_none());

    let partial = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Partial,
    );
    assert!(partial.has_errors());
    assert!(partial.context.is_some());
}

#[test]
fn resolve_typecheck_pipeline_policy_strict_vs_partial() {
    let source = r#"
fn main() -> u64 {
    missing()
}
"#;
    let parsed = parsed_context(source);

    let strict = resolve_typecheck_pipeline_with_policy(
        parsed.clone(),
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(strict.has_errors());
    assert!(strict.resolved_context.is_none());
    assert!(strict.typed_context.is_none());

    let partial = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Partial,
    );
    assert!(partial.has_errors());
    assert!(partial.resolved_context.is_some());
    assert!(partial.typed_context.is_some());
}

#[test]
fn elaborate_output_has_no_for_statements_after_syntax_desugar_pass() {
    let source = r#"
fn main() -> u64 {
    var sum = 0;
    for x in [1, 2, 3] {
        sum = sum + x;
    }
    sum
}
"#;
    let parsed = parsed_context(source);
    let resolved =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved_ctx = resolved
        .context
        .expect("resolve should succeed for desugar test");
    let typed = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Strict,
    )
    .context
    .expect("typecheck should succeed for desugar test");
    let sem_checked = semcheck_stage(typed).expect("semcheck should succeed for desugar test");
    let semantic = elaborate_stage(sem_checked);
    assert!(
        !semantic_module_has_for(&semantic.module),
        "semantic module should not contain StmtExprKind::For after elaborate pipeline"
    );
}

#[test]
fn elaborate_output_has_no_defer_or_using_after_syntax_desugar_pass() {
    let source = r#"
type Resource = {
    id: u64,
}

Resource :: {
    fn close_ignore_error(sink self) {
    }
}

fn make_resource() -> Resource {
    Resource { id: 1 }
}

fn main() {
    using resource = make_resource() {
        let n = resource.id;
    }
}
"#;
    let parsed = parsed_context(source);
    let resolved =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved_ctx = resolved
        .context
        .expect("resolve should succeed for using desugar test");
    let typed = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Strict,
    )
    .context
    .expect("typecheck should succeed for using desugar test");
    let sem_checked =
        semcheck_stage(typed).expect("semcheck should succeed for using desugar test");
    let semantic = elaborate_stage(sem_checked);
    assert!(
        !semantic_module_has_defer_or_using(&semantic.module),
        "semantic module should not contain StmtExprKind::Defer/Using after elaborate pipeline"
    );
}

#[test]
fn elaborate_runs_using_cleanup_before_nested_return() {
    let semantic = elaborate_typestate_semantic(
        r#"
type Resource = {
    id: u64,
}

Resource :: {
    fn close_ignore_error(sink self) {
    }
}

fn make_resource() -> Resource {
    Resource { id: 1 }
}

fn main() {
    using resource = make_resource() {
        if true {
            return;
        } else {
            let n = resource.id;
            n;
        }
    }
}
"#,
    );

    assert!(
        semantic_module_has_cleanup_before_control_transfer(
            &semantic.module,
            ControlTransferKind::Return,
        ),
        "syntax desugar should stage using cleanup before nested return"
    );
}

#[test]
fn elaborate_runs_using_cleanup_before_nested_continue() {
    let semantic = elaborate_typestate_semantic(
        r#"
type Resource = {
    id: u64,
}

Resource :: {
    fn close_ignore_error(sink self) {
    }
}

fn make_resource() -> Resource {
    Resource { id: 1 }
}

fn main() {
    while true {
        using resource = make_resource() {
            if true {
                continue;
            } else {
                let n = resource.id;
                n;
            }
        }
    }
}
"#,
    );

    assert!(
        semantic_module_has_cleanup_before_control_transfer(
            &semantic.module,
            ControlTransferKind::Continue,
        ),
        "syntax desugar should stage using cleanup before nested continue"
    );
}

#[test]
fn elaborate_records_cleanup_plan_for_try_propagation_after_defer() {
    let source = r#"
type IoError = {
    code: u64,
}

fn cleanup() {
}

fn ok(v: u64) -> u64 | IoError {
    v
}

fn main() -> u64 | IoError {
    defer cleanup();
    let value = ok(7)?;
    value
}
"#;
    let parsed = parsed_context(source);
    let resolved =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved_ctx = resolved
        .context
        .expect("resolve should succeed for try cleanup plan test");
    let typed = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Strict,
    )
    .context
    .expect("typecheck should succeed for try cleanup plan test");
    let sem_checked =
        semcheck_stage(typed).expect("semcheck should succeed for try cleanup plan test");
    let semantic = elaborate_stage(sem_checked);

    let try_id = find_first_bare_try_id(&semantic.module)
        .expect("expected bare try expression after elaborate for cleanup plan test");
    let cleanup = semantic
        .lowering_plans
        .lookup_try_cleanup_plan(try_id)
        .expect("expected cleanup plan for bare try inside defer scope");
    assert_eq!(cleanup.len(), 1, "expected exactly one deferred cleanup");
    assert!(
        expr_is_close_ignore_error(&cleanup[0]),
        "expected deferred cleanup expression to be the recorded cleanup call"
    );
}

#[test]
fn typestate_elaborate_builds_machine_descriptor_with_fallback_rows() {
    let source = r#"
type Ping = {}
type Pong = {}

typestate Connection {
    fn new() -> Disconnected { Disconnected {} }

    state Disconnected {
        on Ping() -> Disconnected {
            Disconnected {}
        }
        on Pong() -> Connected {
            Connected {}
        }
    }

    state Connected {
        on Ping() -> Connected {
            Connected {}
        }
    }
}
"#;
    let semantic = elaborate_typestate_semantic(source);
    let descriptor = semantic
        .machine_plans
        .descriptors
        .get("Connection")
        .expect("expected Connection machine descriptor plan");

    assert_eq!(descriptor.state_tags.len(), 2);
    assert_eq!(descriptor.event_kinds.len(), 2);
    assert_eq!(
        descriptor.dispatch_table.len(),
        descriptor.state_tags.len() * descriptor.event_kinds.len(),
        "descriptor should materialize full (state,event) dispatch grid"
    );

    // Deterministic state-tag assignment is lexical by state name.
    assert_eq!(descriptor.state_tags[0].state_name, "Connected");
    assert_eq!(descriptor.state_tags[0].tag, 1);
    assert_eq!(descriptor.state_tags[1].state_name, "Disconnected");
    assert_eq!(descriptor.state_tags[1].tag, 2);

    let ping_kind = descriptor
        .event_kinds
        .iter()
        .find(|event| {
            matches!(
                &event.key,
                MachineEventKeyPlan::Payload {
                    payload_ty: Type::Struct { name, .. }
                } if name == "Ping"
            )
        })
        .map(|event| event.kind)
        .expect("expected Ping event kind");
    let pong_kind = descriptor
        .event_kinds
        .iter()
        .find(|event| {
            matches!(
                &event.key,
                MachineEventKeyPlan::Payload {
                    payload_ty: Type::Struct { name, .. }
                } if name == "Pong"
            )
        })
        .map(|event| event.kind)
        .expect("expected Pong event kind");

    // Connected + Ping -> state-local only (each state has its own handler def,
    // so stricter fallback detection no longer considers it a typestate-level fallback).
    let connected_ping = descriptor
        .dispatch_table
        .iter()
        .find(|row| row.state_tag == 1 && row.event_kind == ping_kind)
        .expect("expected dispatch row for Connected/Ping");
    assert!(
        connected_ping.state_local_thunk.is_some(),
        "expected state-local handler for Ping in Connected"
    );
    assert!(connected_ping.typestate_fallback_thunk.is_none());

    // Disconnected + Pong -> state-local handler only.
    let disconnected_pong = descriptor
        .dispatch_table
        .iter()
        .find(|row| row.state_tag == 2 && row.event_kind == pong_kind)
        .expect("expected dispatch row for Disconnected/Pong");
    assert!(
        disconnected_pong.state_local_thunk.is_some(),
        "expected state-local handler for Pong"
    );
    assert!(disconnected_pong.typestate_fallback_thunk.is_none());

    // Connected + Pong -> unresolved (no local and no fallback).
    let connected_pong = descriptor
        .dispatch_table
        .iter()
        .find(|row| row.state_tag == 1 && row.event_kind == pong_kind)
        .expect("expected dispatch row for Connected/Pong");
    assert!(connected_pong.state_local_thunk.is_none());
    assert!(connected_pong.typestate_fallback_thunk.is_none());
}

#[test]
fn typestate_machine_tag_mapping_is_deterministic_across_runs() {
    let source = r#"
type Ping = {}

typestate Connection {
    fn new() -> Disconnected { Disconnected {} }

    state Disconnected {
        on Ping() -> Connected { Connected {} }
    }

    state Connected {
        on Ping() -> Connected { Connected {} }
    }
}
"#;
    let first = elaborate_typestate_semantic(source);
    let second = elaborate_typestate_semantic(source);
    let first_plan = first
        .machine_plans
        .descriptors
        .get("Connection")
        .expect("expected Connection descriptor in first run");
    let second_plan = second
        .machine_plans
        .descriptors
        .get("Connection")
        .expect("expected Connection descriptor in second run");

    // Compare only deterministic fields (state_name, tag) — TypeId / DefId values
    // are non-deterministic across separate compilations within the same process.
    let first_state_tags: Vec<_> = first_plan
        .state_tags
        .iter()
        .map(|s| (&s.state_name, s.tag))
        .collect();
    let second_state_tags: Vec<_> = second_plan
        .state_tags
        .iter()
        .map(|s| (&s.state_name, s.tag))
        .collect();
    assert_eq!(
        first_state_tags, second_state_tags,
        "state tags must be deterministic"
    );

    let first_event_kinds: Vec<_> = first_plan
        .event_kinds
        .iter()
        .map(|e| (e.key.stable_key(), e.kind))
        .collect();
    let second_event_kinds: Vec<_> = second_plan
        .event_kinds
        .iter()
        .map(|e| (e.key.stable_key(), e.kind))
        .collect();
    assert_eq!(
        first_event_kinds, second_event_kinds,
        "event kinds must be deterministic"
    );
}

#[test]
fn linear_machine_plan_captures_deterministic_state_tags() {
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
        Review {}
    }

    trigger CIFailed(pending) {
        Draft {}
    }
}
"#;

    let semantic = elaborate_linear_semantic(source);
    let plan = semantic
        .linear_machine_plans
        .machines
        .get("PRService")
        .expect("expected linear machine plan");

    assert_eq!(plan.machine_name, "PRService");
    assert_eq!(plan.hosted_type_name, "PullRequest");
    assert_eq!(plan.key_field_name, "id");
    assert_eq!(
        plan.key_ty,
        Type::Int {
            signed: false,
            bits: 64,
            bounds: None,
            nonzero: false,
        }
    );
    assert_eq!(plan.initial_state_tag, 1);
    assert_eq!(
        plan.state_tags
            .iter()
            .map(|state| (state.state_name.as_str(), state.tag))
            .collect::<Vec<_>>(),
        vec![("Draft", 1), ("PendingCI", 2), ("Review", 3)]
    );
}

#[test]
fn linear_machine_plan_prefers_overrides_and_tracks_generated_helpers() {
    let source = r#"
type CIPassed = {}

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
        merge: Review -> Review,
    }

    triggers {
        CIPassed: PendingCI -> Review,
    }

    roles {
        Author { submit, merge }
    }
}

PullRequest :: {
    fn submit(self) -> PendingCI {
        PendingCI {}
    }

    fn merge(self) -> Review {
        Review {}
    }
}

machine PRService hosts PullRequest(key: id) {
    fn new() -> Self {
        Self {}
    }

    action submit(draft) -> PendingCI {
        draft;
        PendingCI {}
    }

    trigger CIPassed(pending) {
        Review {}
    }
}
"#;

    let semantic = elaborate_linear_semantic(source);
    let plan = semantic
        .linear_machine_plans
        .machines
        .get("PRService")
        .expect("expected linear machine plan");
    let def_table = &semantic.def_table;

    assert_eq!(
        def_name(def_table, plan.handle_type_def_id),
        "__mc_machine_handle_PRService"
    );
    assert_eq!(
        def_name(def_table, plan.spawn_fn_def_id),
        "__mc_machine_spawn_PRService"
    );
    assert_eq!(
        def_name(
            def_table,
            *plan
                .create_fn_def_ids
                .get("Author")
                .expect("expected create helper for Author"),
        ),
        "__mc_machine_create_PRService_PullRequest_Author"
    );
    assert_eq!(
        def_name(
            def_table,
            *plan
                .resume_fn_def_ids
                .get("Author")
                .expect("expected resume helper for Author"),
        ),
        "__mc_machine_resume_PRService_PullRequest_Author"
    );
    assert_eq!(
        def_name(
            def_table,
            *plan
                .deliver_fn_def_ids
                .get("CIPassed")
                .expect("expected deliver helper for CIPassed"),
        ),
        "__mc_machine_deliver_PRService_CIPassed"
    );
    assert_eq!(
        def_name(
            def_table,
            *plan
                .wait_fn_def_ids
                .get("PendingCI")
                .expect("expected wait helper for PendingCI"),
        ),
        "__mc_machine_wait_PRService_PullRequest_PendingCI"
    );

    let submit_plan = plan
        .actions
        .iter()
        .find(|action| action.action_name == "submit")
        .expect("expected submit action plan");
    assert_eq!(
        def_name(def_table, submit_plan.handler_def_id),
        "__mc_machine_action_PRService_submit"
    );

    let merge_plan = plan
        .actions
        .iter()
        .find(|action| action.action_name == "merge")
        .expect("expected merge action plan");
    assert_eq!(
        def_name(def_table, merge_plan.handler_def_id),
        "__linear__Review__merge"
    );

    let trigger_plan = plan
        .triggers
        .iter()
        .find(|trigger| trigger.event_type_name == "CIPassed")
        .expect("expected trigger plan for CIPassed");
    assert_eq!(
        def_name(def_table, trigger_plan.handler_def_id),
        "__mc_machine_trigger_PRService_CIPassed"
    );
}

fn elaborate_typestate_semantic(source: &str) -> crate::core::context::SemanticContext {
    let parsed = parsed_context_typestate(source);
    let resolved =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved_ctx = resolved
        .context
        .expect("resolve should succeed for typestate elaborate test");
    let typed = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Strict,
    )
    .context
    .expect("typecheck should succeed for typestate elaborate test");
    let sem_checked = semcheck_stage(typed).expect("semcheck should succeed for typestate test");
    elaborate_stage(sem_checked)
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
fn semcheck_linear_program_keeps_protocol_progression_empty() {
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
    let resolved =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    let resolved_ctx = resolved
        .context
        .expect("resolve should succeed for linear semcheck test");
    let typed = typecheck_stage_with_policy(
        resolved_ctx,
        resolved.imported_facts,
        FrontendPolicy::Strict,
    )
    .context
    .expect("typecheck should succeed for linear semcheck test");
    let sem_checked =
        semcheck_stage(typed).expect("semcheck should succeed for linear semcheck test");

    assert!(
        sem_checked.protocol_progression.handlers.is_empty(),
        "pure linear programs should not build legacy protocol progression facts"
    );
    assert!(
        sem_checked.protocol_progression.by_handler_def.is_empty(),
        "pure linear programs should not index legacy protocol handlers"
    );
    assert!(
        sem_checked.protocol_progression.by_state.is_empty(),
        "pure linear programs should not index legacy protocol states"
    );
}

#[test]
fn elaborate_linear_program_keeps_legacy_machine_plans_empty() {
    let semantic = elaborate_linear_semantic(
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

    assert!(
        semantic.machine_plans.descriptors.is_empty(),
        "pure linear programs should not materialize legacy typestate machine descriptors"
    );
    assert!(
        semantic.machine_plans.thunks.is_empty(),
        "pure linear programs should not materialize legacy typestate dispatch thunks"
    );
    assert!(
        !semantic.linear_machine_plans.machines.is_empty(),
        "linear machine plans should still be produced for hosted linear machines"
    );
}

fn def_name(
    def_table: &crate::core::resolve::DefTable,
    def_id: crate::core::resolve::DefId,
) -> String {
    def_table
        .lookup_def(def_id)
        .expect("expected def for plan id")
        .name
        .clone()
}

fn semantic_module_has_for(module: &Module) -> bool {
    module.top_level_items.iter().any(top_level_item_has_for)
}

fn semantic_module_has_defer_or_using(module: &Module) -> bool {
    module
        .top_level_items
        .iter()
        .any(top_level_item_has_defer_or_using)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ControlTransferKind {
    Return,
    Continue,
}

fn semantic_module_has_cleanup_before_control_transfer(
    module: &Module,
    kind: ControlTransferKind,
) -> bool {
    module
        .top_level_items
        .iter()
        .any(|item| top_level_item_has_cleanup_before_control_transfer(item, kind))
}

fn find_first_bare_try_id(module: &Module) -> Option<NodeId> {
    module
        .top_level_items
        .iter()
        .find_map(top_level_item_first_bare_try_id)
}

fn top_level_item_has_for(item: &TopLevelItem) -> bool {
    match item {
        TopLevelItem::FuncDef(def) => value_has_for(&def.body),
        TopLevelItem::MethodBlock(block) => block
            .method_items
            .iter()
            .filter_map(|item| match item {
                MethodItem::Def(def) => Some(def),
                MethodItem::Decl(_) => None,
            })
            .any(|def| value_has_for(&def.body)),
        TopLevelItem::TraitDef(_)
        | TopLevelItem::TypeDef(_)
        | TopLevelItem::FuncDecl(_)
        | TopLevelItem::ProtocolDef(_)
        | TopLevelItem::TypestateDef(_)
        | TopLevelItem::MachineDef(_)
        | TopLevelItem::ClosureDef(_) => false,
    }
}

fn top_level_item_has_defer_or_using(item: &TopLevelItem) -> bool {
    match item {
        TopLevelItem::FuncDef(def) => value_has_defer_or_using(&def.body),
        TopLevelItem::MethodBlock(block) => block
            .method_items
            .iter()
            .filter_map(|item| match item {
                MethodItem::Def(def) => Some(def),
                MethodItem::Decl(_) => None,
            })
            .any(|def| value_has_defer_or_using(&def.body)),
        TopLevelItem::TraitDef(_)
        | TopLevelItem::TypeDef(_)
        | TopLevelItem::FuncDecl(_)
        | TopLevelItem::ProtocolDef(_)
        | TopLevelItem::TypestateDef(_)
        | TopLevelItem::MachineDef(_)
        | TopLevelItem::ClosureDef(_) => false,
    }
}

fn top_level_item_has_cleanup_before_control_transfer(
    item: &TopLevelItem,
    kind: ControlTransferKind,
) -> bool {
    match item {
        TopLevelItem::FuncDef(def) => value_has_cleanup_before_control_transfer(&def.body, kind),
        TopLevelItem::MethodBlock(block) => block
            .method_items
            .iter()
            .filter_map(|item| match item {
                MethodItem::Def(def) => Some(def),
                MethodItem::Decl(_) => None,
            })
            .any(|def| value_has_cleanup_before_control_transfer(&def.body, kind)),
        TopLevelItem::TraitDef(_)
        | TopLevelItem::TypeDef(_)
        | TopLevelItem::FuncDecl(_)
        | TopLevelItem::ProtocolDef(_)
        | TopLevelItem::TypestateDef(_)
        | TopLevelItem::MachineDef(_)
        | TopLevelItem::ClosureDef(_) => false,
    }
}

fn top_level_item_first_bare_try_id(item: &TopLevelItem) -> Option<NodeId> {
    match item {
        TopLevelItem::FuncDef(def) => value_first_bare_try_id(&def.body),
        TopLevelItem::MethodBlock(block) => block
            .method_items
            .iter()
            .filter_map(|item| match item {
                MethodItem::Def(def) => Some(def),
                MethodItem::Decl(_) => None,
            })
            .find_map(|def| value_first_bare_try_id(&def.body)),
        TopLevelItem::TraitDef(_)
        | TopLevelItem::TypeDef(_)
        | TopLevelItem::FuncDecl(_)
        | TopLevelItem::ProtocolDef(_)
        | TopLevelItem::TypestateDef(_)
        | TopLevelItem::MachineDef(_)
        | TopLevelItem::ClosureDef(_) => None,
    }
}

fn value_has_for(value: &Expr) -> bool {
    match &value.kind {
        ExprKind::Block { items, tail } => {
            items.iter().any(block_item_has_for)
                || tail.as_ref().is_some_and(|tail| value_has_for(tail))
        }
        ExprKind::If {
            cond,
            then_body,
            else_body,
        } => value_has_for(cond) || value_has_for(then_body) || value_has_for(else_body),
        ExprKind::Match { scrutinee, arms } => {
            value_has_for(scrutinee) || arms.iter().any(|arm| value_has_for(&arm.body))
        }
        ExprKind::Call { callee, args } => {
            value_has_for(callee) || args.iter().any(call_arg_has_for)
        }
        ExprKind::MethodCall { callee, args, .. } => {
            value_has_for(callee) || args.iter().any(call_arg_has_for)
        }
        ExprKind::Emit { kind } => match kind {
            EmitKind::Send { to, payload } | EmitKind::Request { to, payload, .. } => {
                value_has_for(to) || value_has_for(payload)
            }
        },
        ExprKind::Reply { cap, value } => value_has_for(cap) || value_has_for(value),
        ExprKind::StructLit { fields, .. } => {
            fields.iter().any(|field| value_has_for(&field.value))
        }
        ExprKind::StructUpdate { target, fields } => {
            value_has_for(target) || fields.iter().any(|field| value_has_for(&field.value))
        }
        ExprKind::EnumVariant { payload, .. } | ExprKind::TupleLit(payload) => {
            payload.iter().any(value_has_for)
        }
        ExprKind::ArrayLit { init, .. } => match init {
            ArrayLitInit::Elems(elems) => elems.iter().any(value_has_for),
            ArrayLitInit::Repeat(value, _) => value_has_for(value),
        },
        ExprKind::SetLit { elems, .. } => elems.iter().any(value_has_for),
        ExprKind::MapLit { entries, .. } => entries
            .iter()
            .any(|entry| value_has_for(&entry.key) || value_has_for(&entry.value)),
        ExprKind::UnaryOp { expr, .. }
        | ExprKind::HeapAlloc { expr }
        | ExprKind::Coerce { expr, .. } => value_has_for(expr),
        ExprKind::Try {
            fallible_expr,
            on_error,
        } => {
            value_has_for(fallible_expr)
                || on_error
                    .as_ref()
                    .is_some_and(|handler| value_has_for(handler))
        }
        ExprKind::BinOp { left, right, .. } => value_has_for(left) || value_has_for(right),
        ExprKind::Range { start, end } => value_has_for(start) || value_has_for(end),
        ExprKind::Slice { start, end, .. } => {
            start.as_ref().is_some_and(|start| value_has_for(start))
                || end.as_ref().is_some_and(|end| value_has_for(end))
        }
        ExprKind::MapGet { target, key } => value_has_for(target) || value_has_for(key),
        ExprKind::UnitLit
        | ExprKind::IntLit(_)
        | ExprKind::BoolLit(_)
        | ExprKind::CharLit(_)
        | ExprKind::StringLit { .. }
        | ExprKind::StringFmt { .. }
        | ExprKind::Move { .. }
        | ExprKind::ImplicitMove { .. }
        | ExprKind::AddrOf { .. }
        | ExprKind::Load { .. }
        | ExprKind::Len { .. }
        | ExprKind::ClosureRef { .. }
        | ExprKind::Var { .. }
        | ExprKind::ArrayIndex { .. }
        | ExprKind::TupleField { .. }
        | ExprKind::StructField { .. }
        | ExprKind::Deref { .. }
        | ExprKind::Closure { .. }
        | ExprKind::RoleProjection { .. } => false,
    }
}

fn value_has_defer_or_using(value: &Expr) -> bool {
    match &value.kind {
        ExprKind::Block { items, tail } => {
            items.iter().any(block_item_has_defer_or_using)
                || tail
                    .as_ref()
                    .is_some_and(|tail| value_has_defer_or_using(tail))
        }
        ExprKind::If {
            cond,
            then_body,
            else_body,
        } => {
            value_has_defer_or_using(cond)
                || value_has_defer_or_using(then_body)
                || value_has_defer_or_using(else_body)
        }
        ExprKind::Match { scrutinee, arms } => {
            value_has_defer_or_using(scrutinee)
                || arms.iter().any(|arm| value_has_defer_or_using(&arm.body))
        }
        ExprKind::Call { callee, args } => {
            value_has_defer_or_using(callee) || args.iter().any(call_arg_has_defer_or_using)
        }
        ExprKind::MethodCall { callee, args, .. } => {
            value_has_defer_or_using(callee) || args.iter().any(call_arg_has_defer_or_using)
        }
        ExprKind::Emit { kind } => match kind {
            EmitKind::Send { to, payload } | EmitKind::Request { to, payload, .. } => {
                value_has_defer_or_using(to) || value_has_defer_or_using(payload)
            }
        },
        ExprKind::Reply { cap, value } => {
            value_has_defer_or_using(cap) || value_has_defer_or_using(value)
        }
        ExprKind::StructLit { fields, .. } => fields
            .iter()
            .any(|field| value_has_defer_or_using(&field.value)),
        ExprKind::StructUpdate { target, fields } => {
            value_has_defer_or_using(target)
                || fields
                    .iter()
                    .any(|field| value_has_defer_or_using(&field.value))
        }
        ExprKind::EnumVariant { payload, .. } | ExprKind::TupleLit(payload) => {
            payload.iter().any(value_has_defer_or_using)
        }
        ExprKind::ArrayLit { init, .. } => match init {
            ArrayLitInit::Elems(elems) => elems.iter().any(value_has_defer_or_using),
            ArrayLitInit::Repeat(value, _) => value_has_defer_or_using(value),
        },
        ExprKind::SetLit { elems, .. } => elems.iter().any(value_has_defer_or_using),
        ExprKind::MapLit { entries, .. } => entries.iter().any(|entry| {
            value_has_defer_or_using(&entry.key) || value_has_defer_or_using(&entry.value)
        }),
        ExprKind::UnaryOp { expr, .. }
        | ExprKind::HeapAlloc { expr }
        | ExprKind::Coerce { expr, .. } => value_has_defer_or_using(expr),
        ExprKind::Try {
            fallible_expr,
            on_error,
        } => {
            value_has_defer_or_using(fallible_expr)
                || on_error
                    .as_ref()
                    .is_some_and(|handler| value_has_defer_or_using(handler))
        }
        ExprKind::BinOp { left, right, .. } => {
            value_has_defer_or_using(left) || value_has_defer_or_using(right)
        }
        ExprKind::Range { start, end } => {
            value_has_defer_or_using(start) || value_has_defer_or_using(end)
        }
        ExprKind::Slice { start, end, .. } => {
            start
                .as_ref()
                .is_some_and(|start| value_has_defer_or_using(start))
                || end
                    .as_ref()
                    .is_some_and(|end| value_has_defer_or_using(end))
        }
        ExprKind::MapGet { target, key } => {
            value_has_defer_or_using(target) || value_has_defer_or_using(key)
        }
        ExprKind::UnitLit
        | ExprKind::IntLit(_)
        | ExprKind::BoolLit(_)
        | ExprKind::CharLit(_)
        | ExprKind::StringLit { .. }
        | ExprKind::StringFmt { .. }
        | ExprKind::Move { .. }
        | ExprKind::ImplicitMove { .. }
        | ExprKind::AddrOf { .. }
        | ExprKind::Load { .. }
        | ExprKind::Len { .. }
        | ExprKind::ClosureRef { .. }
        | ExprKind::Var { .. }
        | ExprKind::ArrayIndex { .. }
        | ExprKind::TupleField { .. }
        | ExprKind::StructField { .. }
        | ExprKind::Deref { .. }
        | ExprKind::Closure { .. }
        | ExprKind::RoleProjection { .. } => false,
    }
}

fn value_has_cleanup_before_control_transfer(value: &Expr, kind: ControlTransferKind) -> bool {
    match &value.kind {
        ExprKind::Block { items, tail } => {
            block_has_cleanup_before_control_transfer(items, kind)
                || tail
                    .as_ref()
                    .is_some_and(|tail| value_has_cleanup_before_control_transfer(tail, kind))
        }
        ExprKind::If {
            cond,
            then_body,
            else_body,
        } => {
            value_has_cleanup_before_control_transfer(cond, kind)
                || value_has_cleanup_before_control_transfer(then_body, kind)
                || value_has_cleanup_before_control_transfer(else_body, kind)
        }
        ExprKind::Match { scrutinee, arms } => {
            value_has_cleanup_before_control_transfer(scrutinee, kind)
                || arms
                    .iter()
                    .any(|arm| value_has_cleanup_before_control_transfer(&arm.body, kind))
        }
        ExprKind::Call { callee, args } => {
            value_has_cleanup_before_control_transfer(callee, kind)
                || args
                    .iter()
                    .any(|arg| call_arg_has_cleanup_before_control_transfer(arg, kind))
        }
        ExprKind::MethodCall { callee, args, .. } => {
            value_has_cleanup_before_control_transfer(callee, kind)
                || args
                    .iter()
                    .any(|arg| call_arg_has_cleanup_before_control_transfer(arg, kind))
        }
        ExprKind::Emit { kind: emit_kind } => match emit_kind {
            EmitKind::Send { to, payload } | EmitKind::Request { to, payload, .. } => {
                value_has_cleanup_before_control_transfer(to, kind)
                    || value_has_cleanup_before_control_transfer(payload, kind)
            }
        },
        ExprKind::Reply { cap, value } => {
            value_has_cleanup_before_control_transfer(cap, kind)
                || value_has_cleanup_before_control_transfer(value, kind)
        }
        ExprKind::StructLit { fields, .. } => fields
            .iter()
            .any(|field| value_has_cleanup_before_control_transfer(&field.value, kind)),
        ExprKind::StructUpdate { target, fields } => {
            value_has_cleanup_before_control_transfer(target, kind)
                || fields
                    .iter()
                    .any(|field| value_has_cleanup_before_control_transfer(&field.value, kind))
        }
        ExprKind::EnumVariant { payload, .. } | ExprKind::TupleLit(payload) => payload
            .iter()
            .any(|value| value_has_cleanup_before_control_transfer(value, kind)),
        ExprKind::ArrayLit { init, .. } => match init {
            ArrayLitInit::Elems(elems) => elems
                .iter()
                .any(|value| value_has_cleanup_before_control_transfer(value, kind)),
            ArrayLitInit::Repeat(value, _) => {
                value_has_cleanup_before_control_transfer(value, kind)
            }
        },
        ExprKind::SetLit { elems, .. } => elems
            .iter()
            .any(|value| value_has_cleanup_before_control_transfer(value, kind)),
        ExprKind::MapLit { entries, .. } => entries.iter().any(|entry| {
            value_has_cleanup_before_control_transfer(&entry.key, kind)
                || value_has_cleanup_before_control_transfer(&entry.value, kind)
        }),
        ExprKind::UnaryOp { expr, .. }
        | ExprKind::HeapAlloc { expr }
        | ExprKind::Coerce { expr, .. } => value_has_cleanup_before_control_transfer(expr, kind),
        ExprKind::Try {
            fallible_expr,
            on_error,
        } => {
            value_has_cleanup_before_control_transfer(fallible_expr, kind)
                || on_error
                    .as_ref()
                    .is_some_and(|handler| value_has_cleanup_before_control_transfer(handler, kind))
        }
        ExprKind::BinOp { left, right, .. } => {
            value_has_cleanup_before_control_transfer(left, kind)
                || value_has_cleanup_before_control_transfer(right, kind)
        }
        ExprKind::Range { start, end } => {
            value_has_cleanup_before_control_transfer(start, kind)
                || value_has_cleanup_before_control_transfer(end, kind)
        }
        ExprKind::Slice { start, end, .. } => {
            start
                .as_ref()
                .is_some_and(|start| value_has_cleanup_before_control_transfer(start, kind))
                || end
                    .as_ref()
                    .is_some_and(|end| value_has_cleanup_before_control_transfer(end, kind))
        }
        ExprKind::MapGet { target, key } => {
            value_has_cleanup_before_control_transfer(target, kind)
                || value_has_cleanup_before_control_transfer(key, kind)
        }
        ExprKind::UnitLit
        | ExprKind::IntLit(_)
        | ExprKind::BoolLit(_)
        | ExprKind::CharLit(_)
        | ExprKind::StringLit { .. }
        | ExprKind::StringFmt { .. }
        | ExprKind::Move { .. }
        | ExprKind::ImplicitMove { .. }
        | ExprKind::AddrOf { .. }
        | ExprKind::Load { .. }
        | ExprKind::Len { .. }
        | ExprKind::ClosureRef { .. }
        | ExprKind::Var { .. }
        | ExprKind::ArrayIndex { .. }
        | ExprKind::TupleField { .. }
        | ExprKind::StructField { .. }
        | ExprKind::Deref { .. }
        | ExprKind::Closure { .. }
        | ExprKind::RoleProjection { .. } => false,
    }
}

fn value_first_bare_try_id(value: &Expr) -> Option<NodeId> {
    match &value.kind {
        ExprKind::Try {
            fallible_expr,
            on_error: Some(handler),
        } => value_first_bare_try_id(fallible_expr).or_else(|| value_first_bare_try_id(handler)),
        ExprKind::Try { fallible_expr, .. } => {
            value_first_bare_try_id(fallible_expr).or(Some(value.id))
        }
        ExprKind::Block { items, tail } => items
            .iter()
            .find_map(block_item_first_bare_try_id)
            .or_else(|| tail.as_ref().and_then(|tail| value_first_bare_try_id(tail))),
        ExprKind::If {
            cond,
            then_body,
            else_body,
        } => value_first_bare_try_id(cond)
            .or_else(|| value_first_bare_try_id(then_body))
            .or_else(|| value_first_bare_try_id(else_body)),
        ExprKind::Match { scrutinee, arms } => value_first_bare_try_id(scrutinee).or_else(|| {
            arms.iter()
                .find_map(|arm| value_first_bare_try_id(&arm.body))
        }),
        ExprKind::Call { callee, args } => value_first_bare_try_id(callee)
            .or_else(|| args.iter().find_map(call_arg_first_bare_try_id)),
        ExprKind::MethodCall { callee, args, .. } => value_first_bare_try_id(callee)
            .or_else(|| args.iter().find_map(call_arg_first_bare_try_id)),
        ExprKind::Emit { kind } => match kind {
            EmitKind::Send { to, payload } | EmitKind::Request { to, payload, .. } => {
                value_first_bare_try_id(to).or_else(|| value_first_bare_try_id(payload))
            }
        },
        ExprKind::Reply { cap, value } => {
            value_first_bare_try_id(cap).or_else(|| value_first_bare_try_id(value))
        }
        ExprKind::StructLit { fields, .. } => fields
            .iter()
            .find_map(|field| value_first_bare_try_id(&field.value)),
        ExprKind::StructUpdate { target, fields } => {
            value_first_bare_try_id(target).or_else(|| {
                fields
                    .iter()
                    .find_map(|field| value_first_bare_try_id(&field.value))
            })
        }
        ExprKind::EnumVariant { payload, .. } | ExprKind::TupleLit(payload) => {
            payload.iter().find_map(value_first_bare_try_id)
        }
        ExprKind::ArrayLit { init, .. } => match init {
            ArrayLitInit::Elems(elems) => elems.iter().find_map(value_first_bare_try_id),
            ArrayLitInit::Repeat(value, _) => value_first_bare_try_id(value),
        },
        ExprKind::SetLit { elems, .. } => elems.iter().find_map(value_first_bare_try_id),
        ExprKind::MapLit { entries, .. } => entries.iter().find_map(|entry| {
            value_first_bare_try_id(&entry.key).or_else(|| value_first_bare_try_id(&entry.value))
        }),
        ExprKind::UnaryOp { expr, .. }
        | ExprKind::HeapAlloc { expr }
        | ExprKind::Coerce { expr, .. } => value_first_bare_try_id(expr),
        ExprKind::BinOp { left, right, .. } => {
            value_first_bare_try_id(left).or_else(|| value_first_bare_try_id(right))
        }
        ExprKind::Range { start, end } => {
            value_first_bare_try_id(start).or_else(|| value_first_bare_try_id(end))
        }
        ExprKind::Slice { start, end, .. } => start
            .as_ref()
            .and_then(|start| value_first_bare_try_id(start))
            .or_else(|| end.as_ref().and_then(|end| value_first_bare_try_id(end))),
        ExprKind::MapGet { target, key } => {
            value_first_bare_try_id(target).or_else(|| value_first_bare_try_id(key))
        }
        ExprKind::UnitLit
        | ExprKind::IntLit(_)
        | ExprKind::BoolLit(_)
        | ExprKind::CharLit(_)
        | ExprKind::StringLit { .. }
        | ExprKind::StringFmt { .. }
        | ExprKind::Move { .. }
        | ExprKind::ImplicitMove { .. }
        | ExprKind::AddrOf { .. }
        | ExprKind::Load { .. }
        | ExprKind::Len { .. }
        | ExprKind::ClosureRef { .. }
        | ExprKind::Var { .. }
        | ExprKind::ArrayIndex { .. }
        | ExprKind::TupleField { .. }
        | ExprKind::StructField { .. }
        | ExprKind::Deref { .. }
        | ExprKind::Closure { .. }
        | ExprKind::RoleProjection { .. } => None,
    }
}

fn block_item_has_for(item: &BlockItem) -> bool {
    match item {
        BlockItem::Stmt(stmt) => stmt_has_for(stmt),
        BlockItem::Expr(expr) => value_has_for(expr),
    }
}

fn block_item_has_defer_or_using(item: &BlockItem) -> bool {
    match item {
        BlockItem::Stmt(stmt) => stmt_has_defer_or_using(stmt),
        BlockItem::Expr(expr) => value_has_defer_or_using(expr),
    }
}

fn block_has_cleanup_before_control_transfer(
    items: &[BlockItem],
    kind: ControlTransferKind,
) -> bool {
    items.windows(2).any(|window| match window {
        [BlockItem::Expr(expr), BlockItem::Stmt(stmt)] => {
            expr_is_close_ignore_error(expr) && stmt_matches_control_transfer(stmt, kind)
        }
        _ => false,
    }) || items.iter().any(|item| match item {
        BlockItem::Stmt(stmt) => stmt_has_cleanup_before_control_transfer(stmt, kind),
        BlockItem::Expr(expr) => value_has_cleanup_before_control_transfer(expr, kind),
    })
}

fn block_item_first_bare_try_id(item: &BlockItem) -> Option<NodeId> {
    match item {
        BlockItem::Stmt(stmt) => stmt_first_bare_try_id(stmt),
        BlockItem::Expr(expr) => value_first_bare_try_id(expr),
    }
}

fn stmt_has_for(stmt: &StmtExpr) -> bool {
    match &stmt.kind {
        StmtExprKind::For { .. } => true,
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            value_has_for(value)
        }
        StmtExprKind::Assign { value, .. } | StmtExprKind::CompoundAssign { value, .. } => {
            value_has_for(value)
        }
        StmtExprKind::While { cond, body } => value_has_for(cond) || value_has_for(body),
        StmtExprKind::Return { value } => value.as_ref().is_some_and(|value| value_has_for(value)),
        StmtExprKind::Defer { value } => value_has_for(value),
        StmtExprKind::Using { value, body, .. } => value_has_for(value) || value_has_for(body),
        StmtExprKind::VarDecl { .. } | StmtExprKind::Break | StmtExprKind::Continue => false,
    }
}

fn stmt_has_defer_or_using(stmt: &StmtExpr) -> bool {
    match &stmt.kind {
        StmtExprKind::Defer { .. } | StmtExprKind::Using { .. } => true,
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            value_has_defer_or_using(value)
        }
        StmtExprKind::Assign { value, .. } | StmtExprKind::CompoundAssign { value, .. } => {
            value_has_defer_or_using(value)
        }
        StmtExprKind::While { cond, body } => {
            value_has_defer_or_using(cond) || value_has_defer_or_using(body)
        }
        StmtExprKind::For { iter, body, .. } => {
            value_has_defer_or_using(iter) || value_has_defer_or_using(body)
        }
        StmtExprKind::Return { value } => value
            .as_ref()
            .is_some_and(|value| value_has_defer_or_using(value)),
        StmtExprKind::VarDecl { .. } | StmtExprKind::Break | StmtExprKind::Continue => false,
    }
}

fn stmt_has_cleanup_before_control_transfer(stmt: &StmtExpr, kind: ControlTransferKind) -> bool {
    match &stmt.kind {
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            value_has_cleanup_before_control_transfer(value, kind)
        }
        StmtExprKind::Assign { value, .. } | StmtExprKind::CompoundAssign { value, .. } => {
            value_has_cleanup_before_control_transfer(value, kind)
        }
        StmtExprKind::While { cond, body } => {
            value_has_cleanup_before_control_transfer(cond, kind)
                || value_has_cleanup_before_control_transfer(body, kind)
        }
        StmtExprKind::For { iter, body, .. } => {
            value_has_cleanup_before_control_transfer(iter, kind)
                || value_has_cleanup_before_control_transfer(body, kind)
        }
        StmtExprKind::Return { value } => value
            .as_ref()
            .is_some_and(|value| value_has_cleanup_before_control_transfer(value, kind)),
        StmtExprKind::Defer { value } => value_has_cleanup_before_control_transfer(value, kind),
        StmtExprKind::Using { value, body, .. } => {
            value_has_cleanup_before_control_transfer(value, kind)
                || value_has_cleanup_before_control_transfer(body, kind)
        }
        StmtExprKind::VarDecl { .. } | StmtExprKind::Break | StmtExprKind::Continue => false,
    }
}

fn stmt_first_bare_try_id(stmt: &StmtExpr) -> Option<NodeId> {
    match &stmt.kind {
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            value_first_bare_try_id(value)
        }
        StmtExprKind::Assign { value, .. } | StmtExprKind::CompoundAssign { value, .. } => {
            value_first_bare_try_id(value)
        }
        StmtExprKind::While { cond, body } => {
            value_first_bare_try_id(cond).or_else(|| value_first_bare_try_id(body))
        }
        StmtExprKind::For { iter, body, .. } => {
            value_first_bare_try_id(iter).or_else(|| value_first_bare_try_id(body))
        }
        StmtExprKind::Return { value } => value
            .as_ref()
            .and_then(|value| value_first_bare_try_id(value)),
        StmtExprKind::Defer { value } => value_first_bare_try_id(value),
        StmtExprKind::Using { value, body, .. } => {
            value_first_bare_try_id(value).or_else(|| value_first_bare_try_id(body))
        }
        StmtExprKind::VarDecl { .. } | StmtExprKind::Break | StmtExprKind::Continue => None,
    }
}

fn call_arg_has_for(arg: &CallArg) -> bool {
    value_has_for(&arg.expr)
}

fn call_arg_has_defer_or_using(arg: &CallArg) -> bool {
    value_has_defer_or_using(&arg.expr)
}

fn call_arg_has_cleanup_before_control_transfer(arg: &CallArg, kind: ControlTransferKind) -> bool {
    value_has_cleanup_before_control_transfer(&arg.expr, kind)
}

fn call_arg_first_bare_try_id(arg: &CallArg) -> Option<NodeId> {
    value_first_bare_try_id(&arg.expr)
}

fn expr_is_close_ignore_error(expr: &Expr) -> bool {
    matches!(
        &expr.kind,
        ExprKind::MethodCall { method_name, .. } if method_name == "close_ignore_error"
    ) || matches!(
        &expr.kind,
        ExprKind::Call { callee, .. }
            if matches!(
                &callee.kind,
                ExprKind::Load { expr }
                    if matches!(
                        &expr.kind,
                        ExprKind::Var { ident, .. } if ident == "cleanup"
                    )
            )
    )
}

fn stmt_matches_control_transfer(stmt: &StmtExpr, kind: ControlTransferKind) -> bool {
    matches!(
        (&stmt.kind, kind),
        (StmtExprKind::Return { .. }, ControlTransferKind::Return)
            | (StmtExprKind::Continue, ControlTransferKind::Continue)
    )
}
