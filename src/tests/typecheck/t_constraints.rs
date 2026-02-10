use super::*;
use crate::context::ParsedContext;
use crate::lexer::{LexError, Lexer, Token};
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::typecheck::{collect, engine::TypecheckEngine};

fn resolve_source(source: &str) -> crate::context::ResolvedContext {
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

fn collect_constraints(source: &str) -> ConstrainOutput {
    let resolved = resolve_source(source);
    let mut engine = TypecheckEngine::new(resolved);
    collect::run(&mut engine).expect("collect pass failed");
    run(&mut engine).expect("constrain pass failed");
    engine.state().constrain.clone()
}

#[test]
fn test_collect_constraints_for_let_binding() {
    let source = r#"
        fn test() -> u64 {
            let x = 1;
            x
        }
    "#;

    let out = collect_constraints(source);
    assert!(!out.constraints.is_empty());
    assert!(
        out.pattern_obligations
            .iter()
            .any(|ob| matches!(ob, PatternObligation::Bind { .. }))
    );
}

#[test]
fn test_collect_call_obligation() {
    let source = r#"
        fn id(x: u64) -> u64 { x }

        fn test() -> u64 {
            id(1)
        }
    "#;

    let out = collect_constraints(source);
    assert_eq!(out.call_obligations.len(), 1);
    assert!(matches!(
        out.call_obligations[0].callee,
        CallCallee::NamedFunction { .. }
    ));
}

#[test]
fn test_collect_control_facts_for_loop_flow() {
    let source = r#"
        fn test() -> u64 {
            while true {
                break;
            }
            0
        }
    "#;

    let out = collect_constraints(source);
    assert!(
        out.control_facts
            .iter()
            .any(|fact| matches!(fact, ControlFact::Break { loop_depth: d, .. } if *d > 0))
    );
}

#[test]
fn test_collect_expr_obligations() {
    let source = r#"
        fn test() -> u64 {
            let arr = [1, 2, 3];
            let x = (arr[0] + 1) * 2;
            for item in arr {
                if item > 0 {
                    break;
                }
            }
            x
        }
    "#;

    let out = collect_constraints(source);
    assert!(
        out.expr_obligations
            .iter()
            .any(|ob| matches!(ob, ExprObligation::ArrayIndex { .. }))
    );
    assert!(
        out.expr_obligations
            .iter()
            .any(|ob| matches!(ob, ExprObligation::BinOp { .. }))
    );
    assert!(
        out.expr_obligations
            .iter()
            .any(|ob| matches!(ob, ExprObligation::ForIter { .. }))
    );
}
