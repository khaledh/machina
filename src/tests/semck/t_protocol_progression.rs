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
