use crate::core::api::{
    FrontendPolicy, ParseModuleOptions, ResolveInputs, elaborate_stage, parse_module_with_id_gen,
    parse_module_with_id_gen_and_options, resolve_stage_with_policy,
    resolve_typecheck_pipeline_with_policy, semcheck_stage, typecheck_stage_with_policy,
};
use crate::core::context::ParsedContext;
use crate::core::resolve::ResolveError;
use crate::core::tree::NodeIdGen;
use crate::core::tree::semantic as sem;
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
fn typestate_protocol_shape_missing_handler_reports_type_error() {
    let source = r#"
type AuthReq = {}
type AuthOk = {}

protocol Auth {
    role Client;
    role Server;
    flow Server -> Client: AuthOk;
    flow Client -> Server: AuthReq;
}

typestate Gateway : Auth::Client {
    fn new() -> Idle {
        Idle {}
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
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ProtocolFlowHandlerMissing(ts, role, _, _)
                    if ts == "Gateway" && role == "Auth::Client"
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

protocol Auth {
    role Client;
    role Server;
    flow Server -> Client: AuthOk;
    flow Client -> Server: AuthReq;
}

typestate Gateway : Auth::Client {
    fn new() -> Idle {
        Idle {}
    }

    state Idle {
        on AuthOk() -> Idle {
            Idle {}
        }

        fn request() -> Idle {
            emit Send(to: 0, Other {});
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
    assert!(
        out.type_errors.iter().any(|e| {
            matches!(
                e.kind(),
                TypeCheckErrorKind::ProtocolOutgoingPayloadNotAllowed(ts, role, _, _)
                    if ts == "Gateway" && role == "Auth::Client"
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

protocol Auth {
    role Client;
    role Server;
    flow Server -> Client: AuthOk;
    flow Client -> Server: AuthReq;
}

typestate Gateway : Auth::Client {
    fn new() -> Idle {
        Idle {}
    }

    state Idle {
        on AuthOk() -> Idle {
            Idle {}
        }

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
    assert!(
        !out.has_errors(),
        "expected valid protocol shape conformance, got resolve={:?} type={:?}",
        out.resolve_errors,
        out.type_errors
    );
}

#[test]
fn typestate_reply_cap_rejects_double_consume() {
    let source = r#"
type AuthReq = {}
type AuthOk = {}

protocol Auth {
    role Client;
    role Server;
    flow Client -> Server: AuthReq;
}

typestate Gateway : Auth::Server {
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

protocol Auth {
    role Client;
    role Server;
    flow Client -> Server: AuthReq;
}

typestate Gateway : Auth::Server {
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

protocol Auth {
    role Client;
    role Server;
    flow Client -> Server: AuthReq;
}

typestate Gateway : Auth::Server {
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

protocol Auth {
    role Client;
    role Server;
    flow Client -> Server: AuthReq;
}

typestate Gateway : Auth::Server {
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

    // Connected + Ping -> typestate fallback only.
    let connected_ping = descriptor
        .dispatch_table
        .iter()
        .find(|row| row.state_tag == 1 && row.event_kind == ping_kind)
        .expect("expected dispatch row for Connected/Ping");
    assert!(connected_ping.state_local_thunk.is_none());
    assert!(
        connected_ping.typestate_fallback_thunk.is_some(),
        "expected typestate-level fallback handler for Ping"
    );

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

    assert_eq!(
        first_plan.state_tags, second_plan.state_tags,
        "state tags must be deterministic"
    );
    assert_eq!(
        first_plan.event_kinds, second_plan.event_kinds,
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
