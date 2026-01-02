use crate::context::AstContext;
use crate::lexer::{LexError, Lexer, Token};
use crate::parser::Parser;
use crate::resolve::resolve;
use crate::semck::{SemCheckError, move_check};
use crate::typeck::type_check;

fn move_check_source(source: &str) -> Vec<SemCheckError> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");

    let ast_context = AstContext::new(module);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    let type_checked_context = type_check(resolved_context).expect("Failed to type check");

    move_check::check(&type_checked_context)
}

#[test]
fn test_use_after_move() {
    let source = r#"
        fn test() -> u64 {
            let a = [1, 2, 3];
            let b = move a;
            a[0]
        }
    "#;

    let errors = move_check_source(source);
    assert!(!errors.is_empty(), "Expected a move-check error");
    assert!(
        matches!(errors[0], SemCheckError::UseAfterMove(_, _)),
        "Expected UseAfterMove error, got {errors:?}"
    );
}

#[test]
fn test_move_ok() {
    let source = r#"
        fn test() -> u64 {
            let a = [1, 2, 3];
            let b = move a;
            0
        }
    "#;

    let errors = move_check_source(source);
    assert!(errors.is_empty(), "Expected no move-check errors");
}

#[test]
fn test_invalid_move_target() {
    let source = r#"
        fn test() -> u64 {
            let a = [1, 2, 3];
            let b = move a[0];
            0
        }
    "#;

    let errors = move_check_source(source);
    assert!(!errors.is_empty(), "Expected a move-check error");
    assert!(
        matches!(errors[0], SemCheckError::InvalidMoveTarget(_)),
        "Expected InvalidMoveTarget error, got {errors:?}"
    );
}

#[test]
fn test_heap_move_required() {
    let source = r#"
        fn test() -> u64 {
            let p = ^1;
            let q = p;
            0
        }
    "#;

    let errors = move_check_source(source);
    assert!(!errors.is_empty(), "Expected a move-check error");
    assert!(
        matches!(errors[0], SemCheckError::OwnedMoveRequired(_)),
        "Expected OwnedMoveRequired error, got {errors:?}"
    );
}

#[test]
fn test_heap_move_ok() {
    let source = r#"
        fn test() -> u64 {
            let p = ^1;
            let q = move p;
            0
        }
    "#;

    let errors = move_check_source(source);
    assert!(errors.is_empty(), "Expected no move-check errors");
}

#[test]
fn test_heap_field_access_allows_borrow() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            let p = ^Point { x: 1, y: 2 };
            p.x
        }
    "#;

    let errors = move_check_source(source);
    assert!(errors.is_empty(), "Expected no move-check errors");
}
