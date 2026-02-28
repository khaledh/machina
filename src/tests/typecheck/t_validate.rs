use super::*;
use crate::core::context::ParsedContext;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::Parser;
use crate::core::resolve::resolve;
use crate::core::typecheck::{collect, constraints, engine::TypecheckEngine, solver};

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

fn run_validate(source: &str) -> Result<(), Vec<TypeCheckError>> {
    let resolved = resolve_source(source);
    let mut engine = TypecheckEngine::new(resolved, crate::core::resolve::ImportedFacts::default());
    collect::run(&mut engine).expect("collect pass failed");
    constraints::run(&mut engine).expect("constrain pass failed");
    solver::run(&mut engine).expect("solve pass failed");
    run(&mut engine)
}

#[test]
fn test_validate_break_outside_loop() {
    let source = r#"
        fn test() -> u64 {
            break;
            0
        }
    "#;

    let result = run_validate(source);
    assert!(result.is_err());
    let errors = result.expect_err("expected error");
    assert!(
        errors
            .iter()
            .any(|err| matches!(err.kind(), TypeCheckErrorKind::BreakOutsideLoop))
    );
}

#[test]
fn test_validate_return_value_unexpected() {
    let source = r#"
        fn test() -> () {
            return 1;
        }
    "#;

    let result = run_validate(source);
    assert!(result.is_err());
    let errors = result.expect_err("expected error");
    assert!(
        errors
            .iter()
            .any(|err| matches!(err.kind(), TypeCheckErrorKind::ReturnValueUnexpected))
    );
}

#[test]
fn test_validate_return_value_missing() {
    let source = r#"
        fn test() -> u64 {
            return;
        }
    "#;

    let result = run_validate(source);
    assert!(result.is_err());
    let errors = result.expect_err("expected error");
    assert!(
        errors
            .iter()
            .any(|err| matches!(err.kind(), TypeCheckErrorKind::ReturnValueMissing(_, ..)))
    );
}

#[test]
fn test_validate_defer_rejects_fallible_expression() {
    let source = r#"
        type IoError = {
            code: u64,
        }

        fn close() -> () | IoError {
            IoError { code: 1 }
        }

        fn test() {
            defer close();
        }
    "#;

    let result = run_validate(source);
    assert!(result.is_err());
    let errors = result.expect_err("expected error");
    assert!(
        errors
            .iter()
            .any(|err| matches!(err.kind(), TypeCheckErrorKind::DeferExprFallible(_)))
    );
}

#[test]
fn test_validate_defer_rejects_bare_try() {
    let source = r#"
        type IoError = {
            code: u64,
        }

        fn close() -> () | IoError {
            IoError { code: 1 }
        }

        fn test() -> () | IoError {
            defer close()?;
            ()
        }
    "#;

    let result = run_validate(source);
    assert!(result.is_err());
    let errors = result.expect_err("expected error");
    assert!(
        errors
            .iter()
            .any(|err| matches!(err.kind(), TypeCheckErrorKind::DeferBareTry))
    );
}
