use crate::core::api::{
    FrontendPolicy, ParseModuleOptions, ResolveInputs, elaborate_stage, parse_module_with_id_gen,
    parse_module_with_id_gen_and_options, resolve_stage_with_policy,
    resolve_typecheck_pipeline_with_policy, semcheck_stage, typecheck_stage_with_policy,
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
