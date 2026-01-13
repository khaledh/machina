use crate::context::ParsedContext;
use crate::lexer::{LexError, Lexer, Token};
use crate::normalize::normalize;
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::semck::{SemCheckError, move_check};
use crate::typeck::type_check;

fn move_check_source(source: &str) -> move_check::MoveCheckResult {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();

    let ast_context = ParsedContext::new(module, id_gen);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    let type_checked_context = type_check(resolved_context).expect("Failed to type check");
    let normalized_context = normalize(type_checked_context);

    move_check::check(&normalized_context)
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

    let result = move_check_source(source);
    assert!(!result.errors.is_empty(), "Expected a move-check error");
    assert!(
        matches!(result.errors[0], SemCheckError::UseAfterMove(_, _)),
        "Expected UseAfterMove error, got {:?}",
        result.errors
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

    let result = move_check_source(source);
    assert!(result.errors.is_empty(), "Expected no move-check errors");
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

    let result = move_check_source(source);
    assert!(!result.errors.is_empty(), "Expected a move-check error");
    assert!(
        matches!(result.errors[0], SemCheckError::InvalidMoveTarget(_)),
        "Expected InvalidMoveTarget error, got {:?}",
        result.errors
    );
}

#[test]
fn test_heap_move_required() {
    let source = r#"
        fn test() -> u64 {
            let p = ^1;
            let q = p;
            let r = p;
            0
        }
    "#;

    let result = move_check_source(source);
    assert!(!result.errors.is_empty(), "Expected a move-check error");
    assert!(
        matches!(result.errors[0], SemCheckError::OwnedMoveRequired(_)),
        "Expected OwnedMoveRequired error, got {:?}",
        result.errors
    );
}

#[test]
fn test_heap_implicit_move_ok() {
    let source = r#"
        fn test() -> u64 {
            let p = ^1;
            let q = p;
            0
        }
    "#;

    let result = move_check_source(source);
    assert!(result.errors.is_empty(), "Expected no move-check errors");
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

    let result = move_check_source(source);
    assert!(result.errors.is_empty(), "Expected no move-check errors");
}

#[test]
fn test_method_sink_self_moves() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point :: {
            fn consume(sink self) -> u64 {
                self.x + self.y
            }
        }

        fn main() -> u64 {
            let p = Point { x: 1, y: 2 };
            let _sum = p.consume();
            p.x
        }
    "#;

    let result = move_check_source(source);
    assert!(!result.errors.is_empty(), "Expected a move-check error");
    assert!(
        matches!(result.errors[0], SemCheckError::UseAfterMove(_, _)),
        "Expected UseAfterMove error, got {:?}",
        result.errors
    );
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

    let result = move_check_source(source);
    assert!(result.errors.is_empty(), "Expected no move-check errors");
}

#[test]
fn test_move_from_param_rejected() {
    let source = r#"
        fn test(p: ^u64) -> u64 {
            let q = p;
            0
        }
    "#;

    let result = move_check_source(source);
    assert!(!result.errors.is_empty(), "Expected a move-check error");
    assert!(
        matches!(result.errors[0], SemCheckError::MoveFromParam(_)),
        "Expected MoveFromParam error, got {:?}",
        result.errors
    );
}

#[test]
fn test_move_from_sink_param_marks_moved() {
    let source = r#"
        fn test(sink p: ^u64) -> u64 {
            let q = move p;
            let r = p;
            0
        }
    "#;

    let result = move_check_source(source);
    assert!(!result.errors.is_empty(), "Expected a move-check error");
    assert!(
        matches!(result.errors[0], SemCheckError::UseAfterMove(_, _)),
        "Expected UseAfterMove error, got {:?}",
        result.errors
    );
}

#[test]
fn test_borrow_param_in_call_ok() {
    let source = r#"
        fn foo(p: ^u64) -> u64 {
            0
        }

        fn test(p: ^u64) -> u64 {
            foo(p);
            0
        }
    "#;

    let result = move_check_source(source);
    assert!(result.errors.is_empty(), "Expected no move-check errors");
}

#[test]
fn test_sink_call_implicit_move_ok() {
    let source = r#"
        fn consume(sink p: ^u64) -> u64 {
            0
        }

        fn test() -> u64 {
            let p = ^1;
            consume(move p);
            0
        }
    "#;

    let result = move_check_source(source);
    assert!(result.errors.is_empty(), "Expected no move-check errors");
}

#[test]
fn test_sink_call_requires_move_on_reuse() {
    let source = r#"
        fn consume(sink p: ^u64) -> u64 {
            0
        }

        fn test() -> u64 {
            let p = ^1;
            consume(move p);
            p;
            0
        }
    "#;

    let result = move_check_source(source);
    assert!(!result.errors.is_empty(), "Expected a move-check error");
    assert!(
        matches!(result.errors[0], SemCheckError::UseAfterMove(_, _)),
        "Expected UseAfterMove error, got {:?}",
        result.errors
    );
}
