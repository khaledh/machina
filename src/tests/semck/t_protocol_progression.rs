use crate::core::api::{FrontendPolicy, ResolveInputs, resolve_stage_with_policy};
use crate::core::context::ParsedContext;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::{Parser, ParserOptions};
use crate::core::resolve::ImportedFacts;
use crate::core::semck::sem_check;
use crate::core::tree::NodeIdGen;
use crate::core::typecheck::type_check_with_imported_facts;

fn semcheck_typestate_source(source: &str) -> crate::core::context::SemanticCheckedContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("failed to tokenize");
    let mut parser = Parser::new_with_id_gen_and_options(
        &tokens,
        NodeIdGen::new(),
        ParserOptions {
            experimental_typestate: true,
        },
    );
    let module = parser.parse().expect("failed to parse");
    let parsed = ParsedContext::new(module, parser.into_id_gen());

    let resolved_out =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(
        resolved_out.errors.is_empty(),
        "expected clean resolve, got {:?}",
        resolved_out.errors
    );
    let resolved = resolved_out
        .context
        .expect("strict resolve should produce context");
    let typed = type_check_with_imported_facts(resolved, ImportedFacts::default())
        .expect("typecheck should succeed");
    sem_check(typed).expect("semcheck should succeed")
}

fn semcheck_typestate_source_err(source: &str) -> Vec<crate::core::semck::SemCheckError> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("failed to tokenize");
    let mut parser = Parser::new_with_id_gen_and_options(
        &tokens,
        NodeIdGen::new(),
        ParserOptions {
            experimental_typestate: true,
        },
    );
    let module = parser.parse().expect("failed to parse");
    let parsed = ParsedContext::new(module, parser.into_id_gen());

    let resolved_out =
        resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(
        resolved_out.errors.is_empty(),
        "expected clean resolve, got {:?}",
        resolved_out.errors
    );
    let resolved = resolved_out
        .context
        .expect("strict resolve should produce context");
    let typed = type_check_with_imported_facts(resolved, ImportedFacts::default())
        .expect("typecheck should succeed");
    sem_check(typed).expect_err("semcheck should fail")
}

#[test]
fn semck_extracts_protocol_progression_emit_and_transition_facts() {
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
    fn new() -> Ready { Ready {} }
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

    let sem = semcheck_typestate_source(source);
    let facts = &sem.protocol_progression;
    assert!(
        !facts.handlers.is_empty(),
        "expected extracted protocol progression handlers"
    );

    let idle_start = facts
        .handlers
        .iter()
        .find(|fact| {
            fact.typestate_name == "Gateway"
                && fact.entry_state.protocol_name == "Auth"
                && fact.entry_state.role_name == "Client"
                && fact.entry_state.state_name == "Idle"
        })
        .expect("expected progression fact for Gateway Idle handler");

    let emit = idle_start
        .cfg
        .node_events
        .values()
        .flat_map(|events| events.iter())
        .find_map(|event| match event {
            crate::core::context::ProtocolProgressionEvent::Emit(emit) => Some(emit),
            _ => None,
        })
        .expect("expected emit event in progression CFG");
    assert_eq!(emit.to_field_name.as_deref(), Some("server"));
    assert_eq!(emit.to_role_name.as_deref(), Some("Server"));
    assert!(!emit.is_request);

    let has_transition = idle_start
        .cfg
        .node_events
        .values()
        .flat_map(|events| events.iter())
        .any(|event| {
            matches!(
                event,
                crate::core::context::ProtocolProgressionEvent::ReturnState(state)
                    if state.to_state_name.as_deref() == Some("Awaiting")
            )
        });
    assert!(
        has_transition,
        "expected return-state transition to Awaiting"
    );
}

#[test]
fn semck_extracts_protocol_progression_from_machine_send_method_calls() {
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
    fn new() -> Ready { Ready {} }
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
            self.server.send(AuthReq {});
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

    let sem = semcheck_typestate_source(source);
    let facts = &sem.protocol_progression;
    let idle_start = facts
        .handlers
        .iter()
        .find(|fact| {
            fact.typestate_name == "Gateway"
                && fact.entry_state.protocol_name == "Auth"
                && fact.entry_state.role_name == "Client"
                && fact.entry_state.state_name == "Idle"
        })
        .expect("expected progression fact for Gateway Idle handler");

    let emit = idle_start
        .cfg
        .node_events
        .values()
        .flat_map(|events| events.iter())
        .find_map(|event| match event {
            crate::core::context::ProtocolProgressionEvent::Emit(emit) => Some(emit),
            _ => None,
        })
        .expect("expected emit event from machine send method call");
    assert_eq!(emit.to_field_name.as_deref(), Some("server"));
    assert_eq!(emit.to_role_name.as_deref(), Some("Server"));
    assert!(!emit.is_request);
}

#[test]
fn semck_extracts_request_response_sets_for_progression() {
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
    req Client -> Server: AuthReq => AuthOk | AuthErr;

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

typestate AuthServer {
    fn new() -> Ready { Ready {} }
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
            let p: Pending<AuthOk | AuthErr> = emit Request(to: self.server, AuthReq {});
            p;
            Awaiting { server: self.server }
        }
    }

    state Awaiting {
        on AuthOk(ok) for AuthReq(req) -> Idle {
            ok;
            req;
            Idle { server: self.server }
        }
        on AuthErr(err) for AuthReq(req) -> Idle {
            err;
            req;
            Idle { server: self.server }
        }
    }
}
"#;

    let sem = semcheck_typestate_source(source);
    let facts = &sem.protocol_progression;
    let idle_start = facts
        .handlers
        .iter()
        .find(|fact| {
            fact.typestate_name == "Gateway"
                && fact.entry_state.protocol_name == "Auth"
                && fact.entry_state.role_name == "Client"
                && fact.entry_state.state_name == "Idle"
        })
        .expect("expected progression fact for Gateway Idle handler");

    let request_emit = idle_start
        .cfg
        .node_events
        .values()
        .flat_map(|events| events.iter())
        .find_map(|event| match event {
            crate::core::context::ProtocolProgressionEvent::Emit(emit) if emit.is_request => {
                Some(emit)
            }
            _ => None,
        })
        .expect("expected request emit event");

    assert_eq!(request_emit.to_role_name.as_deref(), Some("Server"));
    assert_eq!(request_emit.request_response_tys.len(), 2);
    assert!(
        !facts.by_handler_def.is_empty(),
        "expected handler index map to be populated"
    );
    assert!(
        !facts.by_state.is_empty(),
        "expected state index map to be populated"
    );
}

#[test]
fn semck_reports_missing_trigger_transition_for_extra_handler() {
    let source = r#"
type Start = {}
type Local = {}

protocol Auth {
    msg Start;

    role Client {
        state Idle {
            on Start -> Idle;
        }
    }
}

typestate Gateway : Auth::Client {
    fn new() -> Idle { Idle {} }

    state Idle {
        on Start() -> Idle {
            Idle {}
        }

        on Local() -> Idle {
            Idle {}
        }
    }
}
"#;

    let errors = semcheck_typestate_source_err(source);
    assert!(
        errors.iter().any(|err| matches!(
            err,
            crate::core::semck::SemCheckError::ProtocolProgressionMissingTriggerTransition(
                typestate,
                protocol,
                role,
                state,
                selector,
                _
            ) if typestate == "Gateway"
                && protocol == "Auth"
                && role == "Client"
                && state == "Idle"
                && selector.to_string().contains("Local")
        )),
        "expected missing-trigger progression diagnostic, got {:?}",
        errors
    );
}

#[test]
fn semck_reports_impossible_emit_and_return_state_for_trigger() {
    let source = r#"
type Start = {}
type Cancel = {}
type AuthReq = {}
type CancelReq = {}

protocol Auth {
    msg Start;
    msg Cancel;
    msg AuthReq;
    msg CancelReq;

    role Client {
        state Idle {
            on Start@Server -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
            on Cancel@Server -> Idle {
                effects: [ CancelReq ~> Server ]
            }
        }
        state Awaiting {
            on Cancel@Server -> Idle {
                effects: [ CancelReq ~> Server ]
            }
        }
    }

    role Server {
        state Ready {}
    }
}

typestate AuthServer : Auth::Server {
    fn new() -> Ready { Ready {} }
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
        on Start() -> Idle {
            emit Send(to: self.server, CancelReq {});
            Idle { server: self.server }
        }

        on Cancel() -> Idle {
            emit Send(to: self.server, CancelReq {});
            Idle { server: self.server }
        }
    }

    state Awaiting {
        on Cancel() -> Idle {
            emit Send(to: self.server, CancelReq {});
            Idle { server: self.server }
        }
    }
}
"#;

    let errors = semcheck_typestate_source_err(source);
    assert!(
        errors.iter().any(|err| matches!(
            err,
            crate::core::semck::SemCheckError::ProtocolProgressionImpossibleEmit(
                typestate,
                protocol,
                role,
                state,
                selector,
                payload,
                to_role,
                _
            ) if typestate == "Gateway"
                && protocol == "Auth"
                && role == "Client"
                && state == "Idle"
                && selector.to_string().contains("Start")
                && payload.to_string().contains("CancelReq")
                && to_role == "Server"
        )),
        "expected impossible-emit progression diagnostic, got {:?}",
        errors
    );
    assert!(
        errors.iter().any(|err| matches!(
            err,
            crate::core::semck::SemCheckError::ProtocolProgressionImpossibleReturnState(
                typestate,
                protocol,
                role,
                state,
                selector,
                to_state,
                _
            ) if typestate == "Gateway"
                && protocol == "Auth"
                && role == "Client"
                && state == "Idle"
                && selector.to_string().contains("Start")
                && to_state == "Idle"
        )),
        "expected impossible-return progression diagnostic, got {:?}",
        errors
    );
}

#[test]
fn semck_progression_accepts_valid_emit_and_return_for_trigger() {
    let source = r#"
type Start = {}
type AuthReq = {}

protocol Auth {
    msg Start;
    msg AuthReq;

    role Client {
        state Idle {
            on Start@Server -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
        }
        state Awaiting {}
    }

    role Server {
        state Ready {}
    }
}

typestate AuthServer : Auth::Server {
    fn new() -> Ready { Ready {} }
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

    // This should remain clean: every observed emit/return edge is allowed by
    // the protocol transition for the handler trigger.
    let _ = semcheck_typestate_source(source);
}

#[test]
fn semck_progression_deduplicates_repeated_impossible_emit() {
    let source = r#"
type Start = {}
type Cancel = {}
type AuthReq = {}
type CancelReq = {}

protocol Auth {
    msg Start;
    msg Cancel;
    msg AuthReq;
    msg CancelReq;

    role Client {
        state Idle {
            on Start@Server -> Awaiting {
                effects: [ AuthReq ~> Server ]
            }
            on Cancel@Server -> Idle {
                effects: [ CancelReq ~> Server ]
            }
        }
        state Awaiting {}
    }

    role Server {
        state Ready {}
    }
}

typestate AuthServer : Auth::Server {
    fn new() -> Ready { Ready {} }
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
            emit Send(to: self.server, CancelReq {});
            emit Send(to: self.server, CancelReq {});
            Awaiting { server: self.server }
        }

        on Cancel() -> Idle {
            Idle { server: self.server }
        }
    }

    state Awaiting {}
}
"#;

    let errors = semcheck_typestate_source_err(source);
    let impossible_emit_count = errors
        .iter()
        .filter(|err| {
            matches!(
                err,
                crate::core::semck::SemCheckError::ProtocolProgressionImpossibleEmit(
                    typestate,
                    protocol,
                    role,
                    state,
                    selector,
                    payload,
                    to_role,
                    _
                ) if typestate == "Gateway"
                    && protocol == "Auth"
                    && role == "Client"
                    && state == "Idle"
                    && selector.to_string().contains("Start")
                    && payload.to_string().contains("CancelReq")
                    && to_role == "Server"
            )
        })
        .count();
    assert_eq!(
        impossible_emit_count, 1,
        "expected deduped impossible-emit diagnostic, got {:?}",
        errors
    );
}
