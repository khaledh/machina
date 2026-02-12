use super::*;
use crate::core::context::ParsedContext;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::Parser;
use crate::core::resolve::resolve;
use crate::core::typecheck::{collect, constraints, engine::TypecheckEngine};

fn resolve_source(source: &str) -> crate::core::context::ResolvedContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");
    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();
    let ast_context = ParsedContext::new(module, id_gen);
    resolve(ast_context).expect("Failed to resolve")
}

#[test]
fn test_solver_resolves_basic_local_types() {
    let source = r#"
        fn test() -> u64 {
            let x = 1;
            x
        }
    "#;

    let resolved = resolve_source(source);
    let mut engine = TypecheckEngine::new(resolved);
    collect::run(&mut engine).expect("collect pass failed");
    constraints::run(&mut engine).expect("constrain pass failed");
    run(&mut engine).expect("solve pass failed");

    assert!(!engine.state().solve.resolved_node_types.is_empty());
    assert!(!engine.state().solve.resolved_def_types.is_empty());
    assert_eq!(engine.state().solve.failed_constraints, 0);
    assert!(
        engine
            .state()
            .solve
            .resolved_def_types
            .values()
            .any(|ty| *ty == Type::uint(64))
    );
}
