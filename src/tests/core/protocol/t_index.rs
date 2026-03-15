use crate::core::api::{FrontendPolicy, ResolveInputs, resolve_stage_with_policy};
use crate::core::ast::NodeIdGen;
use crate::core::context::{ParsedContext, ResolvedContext};
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::{Parser, ParserOptions};

fn resolve_source(source: &str) -> ResolvedContext {
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
    let out = resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
    assert!(
        out.errors.is_empty(),
        "expected clean resolve, got {:?}",
        out.errors
    );
    out.context.expect("strict resolve should produce context")
}

#[test]
fn protocol_index_collects_typestate_role_bindings() {
    let source = r#"
            type AuthReq = {}
            type AuthOk = {}

            protocol Auth {
                role Client;
                role Server;
                req Client -> Server: AuthReq => AuthOk;
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

                fn new(server: Machine<AuthServer>) -> Ready {
                    Ready { server: server }
                }

                state Ready {}
            }
        "#;

    let resolved = resolve_source(source);
    let bindings = resolved
        .protocol_index
        .typestate_bindings
        .get("Gateway")
        .expect("expected typestate protocol binding");
    assert_eq!(bindings.len(), 1);
    let binding = &bindings[0];
    assert_eq!(binding.protocol_name, "Auth");
    assert_eq!(binding.role_name, "Client");
    assert_eq!(binding.peer_role_bindings.len(), 1);
    assert_eq!(binding.peer_role_bindings[0].field_name, "server");
    assert_eq!(binding.peer_role_bindings[0].role_name, "Server");
}
