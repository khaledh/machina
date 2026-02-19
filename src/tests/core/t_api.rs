use crate::core::api::{
    FrontendPolicy, ParseModuleOptions, ResolveInputs, elaborate_stage, parse_module_with_id_gen,
    parse_module_with_id_gen_and_options, resolve_stage_with_policy,
    resolve_typecheck_pipeline_with_policy, semcheck_stage, typecheck_stage_with_policy,
};
use crate::core::context::ParsedContext;
use crate::core::machine::request_site::labeled_request_site_key;
use crate::core::resolve::ResolveError;
use crate::core::tree::NodeIdGen;
use crate::core::tree::resolved as res;
use crate::core::tree::semantic as sem;
use crate::core::tree::visit::{self, Visitor};
use crate::core::typecheck::TypeCheckErrorKind;
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
        |err| matches!(err, ResolveError::TypestateMissingNew(name, _) if name == "Connection")
    ));
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
        |err| matches!(err, ResolveError::TypestateDuplicateState(ts, state, _) if ts == "Connection" && state == "Disconnected")
    ));
    assert!(out.errors.iter().any(|err| {
        matches!(
            err,
            ResolveError::TypestateInvalidTransitionReturn(ts, state, method, _)
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
        |err| matches!(err, ResolveError::TypestateDuplicateFieldsBlock(name, _) if name == "Connection")
    ));
    assert!(out.errors.iter().any(|err| {
        matches!(
            err,
            ResolveError::TypestateDuplicateStateFieldsBlock(ts, state, _)
                if ts == "Connection" && state == "Disconnected"
        )
    }));
    assert!(out.errors.iter().any(|err| {
        matches!(
            err,
            ResolveError::TypestateStateFieldShadowsCarriedField(ts, state, field, _)
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
        |err| matches!(err, ResolveError::TypestateInvalidNewReturn(name, _) if name == "Connection")
    ));
    assert!(out.errors.iter().any(|err| {
        matches!(
            err,
            ResolveError::TypestateDuplicateTransition(ts, state, method, _)
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
        matches!(
            err,
            ResolveError::TypestateStateLiteralOutsideTypestate(state, _)
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
        matches!(
            err,
            ResolveError::TypestateInvalidStateOnHandlerReturn(ts, state, _)
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
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ProtocolStateHandlerMissing(ts, role, state, _, _)
                    if ts == "Gateway" && role == "Auth::Client" && state == "Awaiting"
            )
        }),
        "expected missing handler protocol conformance error, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ProtocolStateOutgoingPayloadNotAllowed(
                    ts, role, state, _, _
                )
                    if ts == "Gateway" && role == "Auth::Client" && state == "Idle"
            )
        }),
        "expected outgoing payload protocol conformance error, got {:?}",
        out.type_errors
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
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ProtocolStateHandlerMissing(ts, role, state, _, _)
                    if ts == "Gateway" && role == "Auth::Client" && state == "Awaiting"
            )
        }),
        "expected missing handler conformance error from transition surface, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ProtocolStateOutgoingPayloadNotAllowed(ts, role, state, _, _)
                    if ts == "Gateway" && role == "Auth::Client" && state == "Idle"
            )
        }),
        "expected state-local outgoing payload protocol conformance error, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ProtocolStateEmitDestinationRoleMismatch(
                    ts, role, state, _, expected, field, bound, _
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
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ProtocolStateEmitDestinationRoleMismatch(
                    ts, role, state, _, expected, field, bound, _
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
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ProtocolRequestResponseNotInContract(
                    ts,
                    role,
                    _,
                    to_role,
                    _,
                    _
                )
                    if ts == "Gateway" && role == "Auth::Client" && to_role == "Server"
            )
        }),
        "expected contract response-set violation diagnostic, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ProtocolRequestContractAmbiguous(ts, role, _, to_role, _)
                    if ts == "Gateway" && role == "Auth::Client" && to_role == "Server"
            )
        }),
        "expected ambiguous request-contract diagnostic, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ProtocolStateOutgoingPayloadNotAllowed(
                    ts, role, state, _, _
                ) if ts == "Gateway" && role == "Auth::Server" && state == "Ready"
            )
        }),
        "expected protocol outgoing-payload violation for reply, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ReplyCapConsumedMultipleTimes(name, _) if name == "cap"
            )
        }),
        "expected double-consume reply cap error, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ReplyCapMustBeConsumed(name, _) if name == "cap"
            )
        }),
        "expected missing-consume reply cap error, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::ReplyPayloadNotAllowed(_, _, _)
        )),
        "expected invalid reply payload error, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors
            .iter()
            .any(|e| matches!(e.kind(), TypeCheckErrorKind::ReplyOutsideHandler(_))),
        "expected reply outside handler error, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::TypestateOverlappingOnHandlers(ts, state, _, _, _)
                    if ts == "Connection" && state == "AwaitAuth"
            )
        }),
        "expected overlapping typestate on-handler error, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::TypestateAmbiguousResponseProvenance(
                    ts,
                    state,
                    _,
                    _,
                    _
                ) if ts == "Connection" && state == "AwaitAuth"
            )
        }),
        "expected ambiguous provenance diagnostic for mixed labeled/unlabeled handlers, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::TypestateRequestMissingResponseHandler(ts, _, _, _, _)
                    if ts == "Connection"
            )
        }),
        "expected missing response-variant diagnostic, got {:?}",
        out.type_errors
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
    let parsed = parsed_context_typestate(source);
    let out = resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        None,
        FrontendPolicy::Strict,
    );
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::TypestateHandlerUnsupportedResponseVariant(ts, _, _, _, _)
                    if ts == "Connection"
            )
        }),
        "expected unsupported response-variant diagnostic, got {:?}",
        out.type_errors
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
        let res::TopLevelItem::MethodBlock(block) = item else {
            continue;
        };
        if !block.type_name.starts_with("__ts_") {
            continue;
        }
        for method_item in &block.method_items {
            let res::MethodItem::Def(method) = method_item else {
                continue;
            };
            if !method.sig.name.starts_with("__ts_on_") {
                continue;
            }
            found_handler = true;
            assert!(matches!(
                method.sig.ret_ty_expr.kind,
                res::TypeExprKind::Named { ref ident, .. } if ident != "stay"
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
            res::MethodItem::Def(def) if def.sig.name.starts_with("__ts_on_") => Some(def),
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
        matches!(handler.sig.params[1].typ.kind, res::TypeExprKind::Named { ref ident, .. } if ident == "Pending"),
        "expected hidden pending parameter in lowered provenance handler"
    );
    assert!(
        matches!(handler.sig.params[2].typ.kind, res::TypeExprKind::Named { ref ident, .. } if ident == "AuthCheck"),
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
            sem::MethodItem::Def(def) if def.sig.name.starts_with("__ts_on_") => Some(def.def_id),
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

    let mut ctor: Option<&res::FuncDef> = None;
    let mut spawn: Option<&res::FuncDef> = None;
    for item in &resolved.module.top_level_items {
        let res::TopLevelItem::FuncDef(func) = item else {
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
        let res::TopLevelItem::FuncDef(func) = item else {
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

impl Visitor<crate::core::resolve::DefId> for HandlerCommandNormalizationFinder {
    fn visit_expr(&mut self, expr: &res::Expr) {
        match &expr.kind {
            res::ExprKind::Call { callee, .. } => {
                if let res::ExprKind::Var { ident, .. } = &callee.kind {
                    if ident == "send" {
                        self.sugar_send_call_seen = true;
                    } else if ident == "request" {
                        self.sugar_request_call_seen = true;
                    }
                }
            }
            res::ExprKind::Emit { kind } => match kind {
                res::EmitKind::Send { .. } => self.emit_send_seen = true,
                res::EmitKind::Request { .. } => self.emit_request_seen = true,
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

impl Visitor<crate::core::resolve::DefId> for SpawnForwardCtorCallFinder {
    fn visit_expr(&mut self, expr: &res::Expr) {
        if let res::ExprKind::Call { callee, args } = &expr.kind
            && let res::ExprKind::Var { ident, .. } = &callee.kind
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

impl Visitor<crate::core::resolve::DefId> for BootstrapCallFinder {
    fn visit_expr(&mut self, expr: &res::Expr) {
        if let res::ExprKind::Call { callee, .. } = &expr.kind
            && let res::ExprKind::Var { ident, .. } = &callee.kind
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
                sem::MachineEventKeyPlan::Payload {
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
                sem::MachineEventKeyPlan::Payload {
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

fn semantic_module_has_for(module: &sem::Module) -> bool {
    module.top_level_items.iter().any(top_level_item_has_for)
}

fn top_level_item_has_for(item: &sem::TopLevelItem) -> bool {
    match item {
        sem::TopLevelItem::FuncDef(def) => value_has_for(&def.body),
        sem::TopLevelItem::MethodBlock(block) => block
            .method_items
            .iter()
            .filter_map(|item| match item {
                sem::MethodItem::Def(def) => Some(def),
                sem::MethodItem::Decl(_) => None,
            })
            .any(|def| value_has_for(&def.body)),
        sem::TopLevelItem::TraitDef(_)
        | sem::TopLevelItem::TypeDef(_)
        | sem::TopLevelItem::FuncDecl(_) => false,
    }
}

fn value_has_for(value: &sem::ValueExpr) -> bool {
    match &value.kind {
        sem::ValueExprKind::Block { items, tail } => {
            items.iter().any(block_item_has_for)
                || tail.as_ref().is_some_and(|tail| value_has_for(tail))
        }
        sem::ValueExprKind::If {
            cond,
            then_body,
            else_body,
        } => value_has_for(cond) || value_has_for(then_body) || value_has_for(else_body),
        sem::ValueExprKind::Match { scrutinee, arms } => {
            value_has_for(scrutinee) || arms.iter().any(|arm| value_has_for(&arm.body))
        }
        sem::ValueExprKind::Call { callee, args } => {
            value_has_for(callee) || args.iter().any(call_arg_has_for)
        }
        sem::ValueExprKind::MethodCall { receiver, args, .. } => {
            let receiver_has_for = match receiver {
                sem::MethodReceiver::ValueExpr(value) => value_has_for(value),
                sem::MethodReceiver::PlaceExpr(_) => false,
            };
            receiver_has_for || args.iter().any(call_arg_has_for)
        }
        sem::ValueExprKind::EmitSend { to, payload }
        | sem::ValueExprKind::EmitRequest { to, payload, .. } => {
            value_has_for(to) || value_has_for(payload)
        }
        sem::ValueExprKind::Reply { cap, value } => value_has_for(cap) || value_has_for(value),
        sem::ValueExprKind::StructLit { fields, .. } => {
            fields.iter().any(|field| value_has_for(&field.value))
        }
        sem::ValueExprKind::StructUpdate { target, fields } => {
            value_has_for(target) || fields.iter().any(|field| value_has_for(&field.value))
        }
        sem::ValueExprKind::EnumVariant { payload, .. } | sem::ValueExprKind::TupleLit(payload) => {
            payload.iter().any(value_has_for)
        }
        sem::ValueExprKind::ArrayLit { init, .. } => match init {
            sem::ArrayLitInit::Elems(elems) => elems.iter().any(value_has_for),
            sem::ArrayLitInit::Repeat(value, _) => value_has_for(value),
        },
        sem::ValueExprKind::SetLit { elems, .. } => elems.iter().any(value_has_for),
        sem::ValueExprKind::MapLit { entries, .. } => entries
            .iter()
            .any(|entry| value_has_for(&entry.key) || value_has_for(&entry.value)),
        sem::ValueExprKind::UnaryOp { expr, .. }
        | sem::ValueExprKind::HeapAlloc { expr }
        | sem::ValueExprKind::Coerce { expr, .. } => value_has_for(expr),
        sem::ValueExprKind::BinOp { left, right, .. } => {
            value_has_for(left) || value_has_for(right)
        }
        sem::ValueExprKind::Range { start, end } => value_has_for(start) || value_has_for(end),
        sem::ValueExprKind::Slice { start, end, .. } => {
            start.as_ref().is_some_and(|start| value_has_for(start))
                || end.as_ref().is_some_and(|end| value_has_for(end))
        }
        sem::ValueExprKind::MapGet { target, key } => value_has_for(target) || value_has_for(key),
        sem::ValueExprKind::UnitLit
        | sem::ValueExprKind::IntLit(_)
        | sem::ValueExprKind::BoolLit(_)
        | sem::ValueExprKind::CharLit(_)
        | sem::ValueExprKind::StringLit { .. }
        | sem::ValueExprKind::StringFmt { .. }
        | sem::ValueExprKind::Move { .. }
        | sem::ValueExprKind::ImplicitMove { .. }
        | sem::ValueExprKind::AddrOf { .. }
        | sem::ValueExprKind::Load { .. }
        | sem::ValueExprKind::Len { .. }
        | sem::ValueExprKind::ClosureRef { .. } => false,
    }
}

fn block_item_has_for(item: &sem::BlockItem) -> bool {
    match item {
        sem::BlockItem::Stmt(stmt) => stmt_has_for(stmt),
        sem::BlockItem::Expr(expr) => value_has_for(expr),
    }
}

fn stmt_has_for(stmt: &sem::StmtExpr) -> bool {
    match &stmt.kind {
        sem::StmtExprKind::For { .. } => true,
        sem::StmtExprKind::LetBind { value, .. } | sem::StmtExprKind::VarBind { value, .. } => {
            value_has_for(value)
        }
        sem::StmtExprKind::Assign { value, .. } => value_has_for(value),
        sem::StmtExprKind::While { cond, body } => value_has_for(cond) || value_has_for(body),
        sem::StmtExprKind::Return { value } => {
            value.as_ref().is_some_and(|value| value_has_for(value))
        }
        sem::StmtExprKind::VarDecl { .. }
        | sem::StmtExprKind::Break
        | sem::StmtExprKind::Continue => false,
    }
}

fn call_arg_has_for(arg: &sem::CallArg) -> bool {
    match arg {
        sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => value_has_for(expr),
        sem::CallArg::InOut { .. } | sem::CallArg::Out { .. } => false,
    }
}
