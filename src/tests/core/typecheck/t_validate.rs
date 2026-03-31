use super::*;
use crate::core::context::ParsedContext;
use crate::core::context::ResolvedContext;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::Parser;
use crate::core::resolve::{ImportedFacts, resolve};
use crate::core::typecheck::{collect, constraints, engine::TypecheckEngine, solver};

fn resolve_source(source: &str) -> ResolvedContext {
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
    let mut engine = TypecheckEngine::new(resolved, ImportedFacts::default());
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
fn test_validate_empty_return_allowed_for_unit_error_union() {
    let source = r#"
        type IoError = {
            code: u64,
        }

        fn test() -> () | IoError {
            return;
        }
    "#;

    let result = run_validate(source);
    assert!(
        result.is_ok(),
        "expected bare return to be accepted for () | IoError"
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

#[test]
fn test_validate_ptr_at_requires_unsafe_block() {
    let source = r#"
        @intrinsic
        fn ptr_at<T>(addr: vaddr) -> *T;

        fn test(addr: vaddr) -> *u64 {
            let ptr: *u64 = ptr_at(addr);
            ptr
        }
    "#;

    let result = run_validate(source);
    assert!(result.is_err());
    let errors = result.expect_err("expected error");
    assert!(errors.iter().any(|err| matches!(
        err.kind(),
        TypeCheckErrorKind::UnsafeOperationRequiresUnsafeBlock(op) if op == "ptr_at"
    )));
}

#[test]
fn test_validate_view_constructors_require_unsafe_block() {
    let source = r#"
        @intrinsic
        fn view_at<T>(addr: vaddr) -> view<T>;

        @intrinsic
        fn view_slice_at<T>(addr: vaddr, count: u64) -> view_slice<T>;

        @intrinsic
        fn view_array_at<T>(addr: vaddr, count: u64) -> view_array<T>;

        @layout(fixed)
        type Header = {
            magic: u64,
        }

        fn one(addr: vaddr) -> view<Header> {
            view_at(addr)
        }

        fn many(addr: vaddr, count: u64) -> view_slice<Header> {
            view_slice_at(addr, count)
        }

        fn flat(addr: vaddr, count: u64) -> view_array<Header> {
            view_array_at(addr, count)
        }
    "#;

    let result = run_validate(source);
    assert!(result.is_err());
    let errors = result.expect_err("expected error");
    assert!(errors.iter().any(|err| matches!(
        err.kind(),
        TypeCheckErrorKind::UnsafeOperationRequiresUnsafeBlock(op) if op == "view_at"
    )));
    assert!(errors.iter().any(|err| matches!(
        err.kind(),
        TypeCheckErrorKind::UnsafeOperationRequiresUnsafeBlock(op) if op == "view_slice_at"
    )));
    assert!(errors.iter().any(|err| matches!(
        err.kind(),
        TypeCheckErrorKind::UnsafeOperationRequiresUnsafeBlock(op) if op == "view_array_at"
    )));
}

#[test]
fn test_validate_raw_pointer_read_requires_unsafe_block() {
    let source = r#"
        fn test(ptr: *u64) -> u64 {
            ptr.read()
        }
    "#;

    let result = run_validate(source);
    assert!(result.is_err());
    let errors = result.expect_err("expected error");
    assert!(errors.iter().any(|err| matches!(
        err.kind(),
        TypeCheckErrorKind::UnsafeOperationRequiresUnsafeBlock(op) if op == "raw pointer read"
    )));
}

#[test]
fn test_validate_raw_pointer_write_requires_unsafe_block() {
    let source = r#"
        fn test(ptr: *u64) {
            ptr.write(1);
        }
    "#;

    let result = run_validate(source);
    assert!(result.is_err());
    let errors = result.expect_err("expected error");
    assert!(errors.iter().any(|err| matches!(
        err.kind(),
        TypeCheckErrorKind::UnsafeOperationRequiresUnsafeBlock(op) if op == "raw pointer write"
    )));
}

#[test]
fn test_validate_raw_pointer_ops_allowed_inside_unsafe_block() {
    let source = r#"
        @intrinsic
        fn ptr_at<T>(addr: vaddr) -> *T;

        fn make(addr: vaddr) -> *u64 {
            unsafe {
                ptr_at(addr)
            }
        }

        fn read(ptr: *u64) -> u64 {
            unsafe {
                ptr.read()
            }
        }

        fn write(ptr: *u64) {
            unsafe {
                ptr.write(1);
            }
        }
    "#;

    let result = run_validate(source);
    assert!(
        result.is_ok(),
        "expected raw pointer operations inside unsafe blocks to be accepted"
    );
}

#[test]
fn test_validate_view_constructors_allowed_inside_unsafe_block() {
    let source = r#"
        @intrinsic
        fn view_at<T>(addr: vaddr) -> view<T>;

        @intrinsic
        fn view_slice_at<T>(addr: vaddr, count: u64) -> view_slice<T>;

        @intrinsic
        fn view_array_at<T>(addr: vaddr, count: u64) -> view_array<T>;

        @layout(fixed)
        type Header = {
            magic: u64,
        }

        fn one(addr: vaddr) -> view<Header> {
            unsafe {
                view_at(addr)
            }
        }

        fn many(addr: vaddr, count: u64) -> view_slice<Header> {
            unsafe {
                view_slice_at(addr, count)
            }
        }

        fn flat(addr: vaddr, count: u64) -> view_array<Header> {
            unsafe {
                view_array_at(addr, count)
            }
        }
    "#;

    let result = run_validate(source);
    assert!(
        result.is_ok(),
        "expected foreign view constructors inside unsafe blocks to be accepted"
    );
}
