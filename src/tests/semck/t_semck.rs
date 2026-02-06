use crate::context::{ParsedContext, SemanticCheckedContext};
use crate::lexer::{LexError, Lexer, Token};
use crate::normalize::normalize;
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::semck::{SemCheckError, sem_check};
use crate::typecheck::TypeCheckErrorKind;
use crate::typecheck::type_check;
use crate::types::Type;

fn sem_check_source(source: &str) -> Result<SemanticCheckedContext, Vec<SemCheckError>> {
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
    sem_check(normalized_context)
}

#[test]
fn test_for_range_invalid_bounds() {
    let source = r#"
        fn test() -> u64 {
            for i in 5..5 { i; }
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InvalidRangeBounds(min, max, _) => {
                assert_eq!(*min, 5);
                assert_eq!(*max, 5);
            }
            e => panic!("Expected InvalidRangeBounds error, got {:?}", e),
        }
    }
}

#[test]
fn test_range_literal_in_bounds() {
    let source = r#"
        fn test() -> u64 {
            let x: u64: bounds(100) = 42;
            0
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_range_literal_out_of_bounds() {
    let source = r#"
        fn test() -> u64 {
            let x: u64: bounds(50, 100) = 42;
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::ValueOutOfRange(value, min, max, _) => {
                assert_eq!(*value, 42);
                assert_eq!(*min, 50);
                assert_eq!(*max, 100);
            }
            e => panic!("Expected ValueOutOfRange error, got {:?}", e),
        }
    }
}

#[test]
fn test_signed_bounds_accept_negative_literal() {
    let source = r#"
        fn test() -> u64 {
            let x: i8: bounds(-5, 5) = -3;
            let _y: i8 = x;
            0
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_signed_bounds_rejects_out_of_range_literal() {
    let source = r#"
        fn test() -> u64 {
            let x: i8: bounds(-5, 5) = -6;
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::ValueOutOfRange(value, min, max, _) => {
                assert_eq!(*value, -6);
                assert_eq!(*min, -5);
                assert_eq!(*max, 5);
            }
            e => panic!("Expected ValueOutOfRange error, got {:?}", e),
        }
    }
}

#[test]
fn test_range_out_of_bounds_via_const_binding() {
    let source = r#"
        type MidRange = u64: bounds(50, 100);

        fn take_bounded_int(x: MidRange) -> u64 { x }

        fn test() -> u64 {
            let x = 42;
            let bad2: MidRange = x;
            take_bounded_int(x);
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, SemCheckError::ValueOutOfRange(42, 50, 100, _))),
            "Expected ValueOutOfRange(42, 50, 100), got {:?}",
            errors
        );
    }
}

#[test]
fn test_nonzero_literal_rejected() {
    let source = r#"
        fn test() -> u64 {
            let x: u64: nonzero = 0;
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::ValueNotNonZero(value, _) => {
                assert_eq!(*value, 0);
            }
            e => panic!("Expected ValueNotNonZero error, got {:?}", e),
        }
    }
}

#[test]
fn test_nonzero_out_of_bounds_via_const_binding() {
    let source = r#"
        type NonZero = u64: nonzero;

        fn take_nonzero(x: NonZero) -> u64 { x }

        fn test() -> u64 {
            let x = 0;
            take_nonzero(x);
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, SemCheckError::ValueNotNonZero(0, _))),
            "Expected ValueNotNonZero(0), got {:?}",
            errors
        );
    }
}

#[test]
fn test_mod_by_zero_rejected() {
    let source = r#"
        fn test() -> u64 {
            let _x = 10 % 0;
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::DivisionByZero(_) => {}
            e => panic!("Expected DivisionByZero error, got {:?}", e),
        }
    }
}

#[test]
fn test_div_by_zero_via_const_binding() {
    let source = r#"
        fn test() -> u64 {
            let z = 0;
            let _x = 10 / z;
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::DivisionByZero(_) => {}
            e => panic!("Expected DivisionByZero error, got {:?}", e),
        }
    }
}

#[test]
fn test_range_invalid_bounds() {
    let source = r#"
        fn test() -> u64 {
            let x: u64: bounds(10, 10) = 0;
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InvalidRangeBounds(min, max, _) => {
                assert_eq!(*min, 10);
                assert_eq!(*max, 10);
            }
            e => panic!("Expected InvalidRangeBounds error, got {:?}", e),
        }
    }
}

#[test]
fn test_closure_capture_read_only_ok() {
    let source = r#"
        fn test() -> u64 {
            var x = 5;
            let add = |y: u64| -> u64 {
                x + y
            };
            add(2)
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_closure_capture_mutation_allowed() {
    let source = r#"
        fn test() -> u64 {
            var x = 0;
            let bump = || {
                x = x + 1;
            };
            0
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_closure_imm_capture_conflict() {
    let source = r#"
        fn test() -> u64 {
            var x = 0;
            let f = || -> u64 x;
            x = 1;
            f()
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, SemCheckError::ClosureBorrowConflict(_, _))),
            "Expected ClosureBorrowConflict error, got {:?}",
            errors
        );
    }
}

#[test]
fn test_closure_mut_capture_conflict() {
    let source = r#"
        fn test() -> u64 {
            var x = 0;
            let f = || -> u64 {
                x = x + 1;
                x
            };
            let y = x;
            f()
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, SemCheckError::ClosureBorrowConflict(_, _))),
            "Expected ClosureBorrowConflict error, got {:?}",
            errors
        );
    }
}

#[test]
fn test_closure_borrow_after_last_use_allowed() {
    let source = r#"
        fn test() -> u64 {
            var x = 0;
            let f = || -> u64 x;
            let tmp = f();
            x = 2;
            x
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_closure_capture_escape_arg_rejected() {
    let source = r#"
        fn apply(f: fn(u64) -> u64) -> u64 {
            f(1)
        }

        fn test() -> u64 {
            var x = 0;
            let f = |y: u64| -> u64 x + y;
            apply(f)
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, SemCheckError::ClosureEscapeArg(_))),
            "Expected ClosureEscapeArg error, got {:?}",
            errors
        );
    }
}

#[test]
fn test_closure_move_capture_use_after_move() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            let x = ^Point { x: 1, y: 2 };
            let f = [move x] || -> u64 x.x;
            let y = x;
            f()
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|err| matches!(err, SemCheckError::UseAfterMove(_, _))),
            "Expected UseAfterMove error, got {:?}",
            errors
        );
    }
}

#[test]
fn test_struct_pattern_missing_field() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            let Point { x } = Point { x: 1, y: 2 };
            x
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::StructFieldsMissing(_, _) => {}
            e => panic!("Expected StructFieldsMissing error, got {:?}", e),
        }
    }
}

#[test]
fn test_struct_pattern_unknown_field() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            let Point { z } = Point { x: 1, y: 2 };
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::UnknownStructField(_, _) => {}
            e => panic!("Expected UnknownStructField error, got {:?}", e),
        }
    }
}

#[test]
fn test_struct_pattern_duplicate_field() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            let Point { x: a, x: b } = Point { x: 1, y: 2 };
            a + b
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::DuplicateStructField(_, _) => {}
            e => panic!("Expected DuplicateStructField error, got {:?}", e),
        }
    }
}

#[test]
fn test_method_inout_self_requires_mutable() {
    let source = r#"
        type Counter = { value: u64 }

        Counter :: {
            fn bump(inout self) {
                self.value = self.value + 1;
            }
        }

        fn main() -> u64 {
            let c = Counter { value: 0 };
            c.bump();
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InOutArgNotMutable(_) => {}
            e => panic!("Expected InOutArgNotMutable error, got {:?}", e),
        }
    }
}

#[test]
fn test_method_inout_self_requires_lvalue() {
    let source = r#"
        type Counter = { value: u64 }

        Counter :: {
            fn bump(inout self) {
                self.value = self.value + 1;
            }
        }

        fn main() -> u64 {
            Counter { value: 0 }.bump();
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InOutArgNotLvalue(_) => {}
            e => panic!("Expected InOutArgNotLvalue error, got {:?}", e),
        }
    }
}

#[test]
fn test_method_out_self_rejected() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point :: {
            fn init(out self) {
                self.x = 1;
                self.y = 2;
            }
        }

        fn main() -> u64 {
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::OutSelfNotAllowed(_) => {}
            e => panic!("Expected OutSelfNotAllowed error, got {:?}", e),
        }
    }
}

#[test]
fn test_struct_update_unknown_field() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            let p = Point { x: 1, y: 2 };
            let _q = { p | z: 3 };
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::UnknownStructField(_, _) => {}
            e => panic!("Expected UnknownStructField error, got {:?}", e),
        }
    }
}

#[test]
fn test_sink_param_requires_owned_type() {
    let source = r#"
        fn consume(sink p: u64) -> u64 {
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::SinkParamNotOwned(_, _) => {}
            e => panic!("Expected SinkParamNotOwned error, got {:?}", e),
        }
    }
}

#[test]
fn test_sink_param_owned_type_ok() {
    let source = r#"
        fn consume(sink p: ^u64) -> u64 {
            0
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_sink_arg_missing_move() {
    let source = r#"
        fn consume(sink p: ^u64) -> u64 {
            0
        }

        fn main() {
            let p = ^1;
            consume(p);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::SinkArgMissingMove(_) => {}
            e => panic!("Expected SinkArgMissingMove error, got {:?}", e),
        }
    }
}

#[test]
fn test_var_decl_use_before_init_rejected() {
    let source = r#"
        fn test() -> u64 {
            var x: u64;
            x
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::UseBeforeInit(_, _) => {}
            e => panic!("Expected UseBeforeInit error, got {:?}", e),
        }
    }
}

#[test]
fn test_var_decl_init_before_use_ok() {
    let source = r#"
        fn test() -> u64 {
            var x: u64;
            x = 1;
            x
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_var_decl_field_write_rejected() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            var p: Point;
            p.x = 1;
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::UseBeforeInit(_, _) => {}
            e => panic!("Expected UseBeforeInit error, got {:?}", e),
        }
    }
}

#[test]
fn test_struct_update_duplicate_field() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            let p = Point { x: 1, y: 2 };
            let _q = { p | x: 3, x: 4 };
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::DuplicateStructField(_, _) => {}
            e => panic!("Expected DuplicateStructField error, got {:?}", e),
        }
    }
}

#[test]
fn test_enum_variant_payload_arity_mismatch() {
    let source = r#"
        type Pair = A(u64) | B(u64)

        fn test() -> Pair {
            Pair::A(1, 2)
        }
    "#;

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
    let result = type_check(resolved_context);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::EnumVariantPayloadArityMismatch(name, expected, got, _) => {
                assert_eq!(name, "A");
                assert_eq!(*expected, 1);
                assert_eq!(*got, 2);
            }
            e => panic!(
                "Expected EnumVariantPayloadArityMismatch error, got {:?}",
                e
            ),
        }
    }
}

#[test]
fn test_match_non_exhaustive() {
    let source = r#"
        type Color = Red | Green

        fn test(c: Color) -> u64 {
            match c {
                Red => 0,
            }
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::NonExhaustiveMatch(_) => {}
            e => panic!("Expected NonExhaustiveMatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_match_exhaustive_without_wildcard() {
    let source = r#"
        type Color = Red | Green

        fn test(c: Color) -> u64 {
            match c {
                Red => 0,
                Green => 1,
            }
        }
    "#;

    sem_check_source(source).expect("Expected match to be exhaustive");
}

#[test]
fn test_match_duplicate_variant() {
    let source = r#"
        type Color = Red | Green

        fn test(c: Color) -> u64 {
            match c {
                Red => 0,
                Red => 1,
                _ => 2,
            }
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::DuplicateMatchVariant(name, _) => {
                assert_eq!(name, "Red");
            }
            e => panic!("Expected DuplicateMatchVariant error, got {:?}", e),
        }
    }
}

#[test]
fn test_match_bool_exhaustive() {
    let source = r#"
        fn test(b: bool) -> u64 {
            match b {
                true => 1,
                false => 0,
            }
        }
    "#;

    sem_check_source(source).expect("Expected match to be exhaustive");
}

#[test]
fn test_match_bool_non_exhaustive() {
    let source = r#"
        fn test(b: bool) -> u64 {
            match b {
                true => 1,
            }
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::NonExhaustiveMatch(_) => {}
            e => panic!("Expected NonExhaustiveMatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_match_bool_invalid_pattern() {
    let source = r#"
        fn test(b: bool) -> u64 {
            match b {
                Foo => 0,
                _ => 1,
            }
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InvalidMatchPattern(_, _) => {}
            e => panic!("Expected InvalidMatchPattern error, got {:?}", e),
        }
    }
}

#[test]
fn test_match_int_exhaustive() {
    let source = r#"
        fn test(x: u64) -> u64 {
            match x {
                0 => 1,
                _ => 2,
            }
        }
    "#;

    sem_check_source(source).expect("Expected match to be exhaustive");
}

#[test]
fn test_match_int_non_exhaustive() {
    let source = r#"
        fn test(x: u64) -> u64 {
            match x {
                0 => 1,
            }
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::NonExhaustiveMatch(_) => {}
            e => panic!("Expected NonExhaustiveMatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_match_int_invalid_pattern() {
    let source = r#"
        fn test(x: u64) -> u64 {
            match x {
                true => 1,
                _ => 2,
            }
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InvalidMatchPattern(_, _) => {}
            e => panic!("Expected InvalidMatchPattern error, got {:?}", e),
        }
    }
}

#[test]
fn test_match_int_duplicate_literal() {
    let source = r#"
        fn test(x: u64) -> u64 {
            match x {
                1 => 1,
                1 => 2,
                _ => 3,
            }
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::DuplicateMatchVariant(_, _) => {}
            e => panic!("Expected DuplicateMatchVariant error, got {:?}", e),
        }
    }
}

#[test]
fn test_match_tuple_single_arm_ok() {
    let source = r#"
        fn test(t: (u64, bool)) -> u64 {
            match t {
                (x, _) => x,
            }
        }
    "#;

    sem_check_source(source).expect("Expected tuple match to be accepted");
}

#[test]
fn test_match_tuple_nested_ok() {
    let source = r#"
        fn test(t: (u64, (bool, u64))) -> u64 {
            match t {
                (x, (y, _)) => x,
            }
        }
    "#;

    sem_check_source(source).expect("Expected nested tuple match to be accepted");
}

#[test]
fn test_match_tuple_multiple_arms_ok() {
    let source = r#"
        fn test(t: (u64, bool)) -> u64 {
            match t {
                (x, _) => x,
                _ => 0,
            }
        }
    "#;

    sem_check_source(source).expect("Expected tuple match to be accepted");
}

#[test]
fn test_match_tuple_literals_require_wildcard() {
    let source = r#"
        fn test(t: (u64, bool)) -> u64 {
            match t {
                (1, true) => 1,
            }
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::NonExhaustiveMatch(_) => {}
            e => panic!("Expected NonExhaustiveMatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_match_tuple_literals_with_wildcard_ok() {
    let source = r#"
        type Flag = On | Off

        fn test(t: (u64, Flag, bool)) -> u64 {
            match t {
                (1, Flag::On, true) => 1,
                _ => 0,
            }
        }
    "#;

    sem_check_source(source).expect("Expected tuple match to be accepted");
}

#[test]
fn test_match_wildcard_not_last() {
    let source = r#"
        type Color = Red | Green

        fn test(c: Color) -> u64 {
            match c {
                _ => 0,
                Red => 1,
            }
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SemCheckError::WildcardArmNotLast(_))),
            "Expected WildcardArmNotLast error, got {:?}",
            errors
        );
    }
}

#[test]
fn test_match_target_not_enum() {
    let source = r#"
        fn test() -> u64 {
            match "hi" {
                _ => 0,
            }
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::MatchTargetNotEnum(ty, _) => {
                assert_eq!(*ty, Type::String);
            }
            e => panic!("Expected MatchTargetNotEnum error, got {:?}", e),
        }
    }
}

#[test]
fn test_match_enum_through_heap() {
    let source = r#"
        type Msg = Ping(u64) | Pong(u64)

        fn test() -> u64 {
            let msg = ^Msg::Ping(5);
            match msg {
                Msg::Ping(n) => n,
                Msg::Pong(n) => n,
                _ => 0,
            }
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_ok(), "Expected no semck errors, got {result:?}");
}

#[test]
fn test_inout_param_scalar_rejected() {
    let source = r#"
        fn update(inout x: u64) {
            x = 1;
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InOutParamNotAggregate(_, _) => {}
            e => panic!("Expected InOutParamNotAggregate error, got {:?}", e),
        }
    }
}

#[test]
fn test_inout_param_heap_allowed() {
    let source = r#"
        type Point = { x: u64, y: u64 }
        type Box = { p: ^Point }

        fn take(inout p: ^Point) {
            p.x = 3;
        }

        fn main() {
            var b = Box { p: ^Point { x: 1, y: 2 } };
            take(inout b.p);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_ok(), "Expected no semck errors, got {result:?}");
}

#[test]
fn test_out_param_scalar_rejected() {
    let source = r#"
        fn update(out x: u64) {
            x = 1;
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::OutParamNotAggregate(_, _) => {}
            e => panic!("Expected OutParamNotAggregate error, got {:?}", e),
        }
    }
}

#[test]
fn test_inout_arg_not_lvalue() {
    let source = r#"
        fn update(inout arr: u64[3]) {
            arr[0] = 10;
        }

        fn main() {
            update(inout u64[1, 2, 3]);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InOutArgNotLvalue(_) => {}
            e => panic!("Expected InOutArgNotLvalue error, got {:?}", e),
        }
    }
}

#[test]
fn test_inout_arg_missing_mode() {
    let source = r#"
        fn update(inout arr: u64[3]) {
            arr[0] = 10;
        }

        fn main() {
            var arr = u64[1, 2, 3];
            update(arr);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InOutArgMissingMode(_) => {}
            e => panic!("Expected InOutArgMissingMode error, got {:?}", e),
        }
    }
}

#[test]
fn test_out_arg_not_lvalue() {
    let source = r#"
        fn update(out arr: u64[3]) {
            arr[0] = 10;
        }

        fn main() {
            update(out u64[1, 2, 3]);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::OutArgNotLvalue(_) => {}
            e => panic!("Expected OutArgNotLvalue error, got {:?}", e),
        }
    }
}

#[test]
fn test_out_arg_missing_mode() {
    let source = r#"
        fn update(out arr: u64[3]) {
            arr[0] = 10;
        }

        fn main() {
            var arr = u64[1, 2, 3];
            update(arr);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::OutArgMissingMode(_) => {}
            e => panic!("Expected OutArgMissingMode error, got {:?}", e),
        }
    }
}

#[test]
fn test_inout_arg_not_mutable() {
    let source = r#"
        fn update(inout arr: u64[3]) {
            arr[0] = 10;
        }

        fn main() {
            let arr = u64[1, 2, 3];
            update(inout arr);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InOutArgNotMutable(_) => {}
            e => panic!("Expected InOutArgNotMutable error, got {:?}", e),
        }
    }
}

#[test]
fn test_out_arg_not_mutable() {
    let source = r#"
        fn update(out arr: u64[3]) {
            arr[0] = 10;
        }

        fn main() {
            let arr = u64[1, 2, 3];
            update(out arr);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::OutArgNotMutable(_) => {}
            e => panic!("Expected OutArgNotMutable error, got {:?}", e),
        }
    }
}

#[test]
fn test_out_arg_allows_uninit_var() {
    let source = r#"
        fn fill(out arr: u64[3]) {
            arr = u64[1, 2, 3];
        }

        fn main() {
            var arr: u64[3];
            fill(out arr);
            arr[0];
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_out_arg_allows_partial_init_via_subfields() {
    let source = r#"
        type Pair = { a: u64[2], b: u64[2] }

        fn fill_a(out a: u64[2]) {
            a = u64[1, 2];
        }

        fn fill_b(out b: u64[2]) {
            b = u64[3, 4];
        }

        fn main() -> u64 {
            var p: Pair;
            fill_a(out p.a);
            fill_b(out p.b);
            p.a[0] + p.b[0]
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_partial_init_local_without_full_init_rejected() {
    let source = r#"
        type Point = { x: u64, y: u64 }
        type Boxed = { p: ^Point }
        type Pair = { a: Boxed, b: Boxed }

        fn fill_a(out a: Boxed) {
            a = Boxed { p: ^Point { x: 1, y: 2 } };
        }

        fn main() {
            var p: Pair;
            fill_a(out p.a);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::PartialInitNotAllowed(_, _) => {}
            e => panic!("Expected PartialInitNotAllowed error, got {:?}", e),
        }
    }
}

#[test]
fn test_out_param_requires_init_on_all_paths() {
    let source = r#"
        type Pair = { x: u64, y: u64 }

        fn fill(out p: Pair) {
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::OutParamNotInitialized(_, _) => {}
            e => panic!("Expected OutParamNotInitialized error, got {:?}", e),
        }
    }
}

#[test]
fn test_out_param_struct_fields_ok() {
    let source = r#"
        type Pair = { x: u64, y: u64 }

        fn fill(out p: Pair) {
            p.x = 1;
            p.y = 2;
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_out_param_struct_field_use_before_init_rejected() {
    let source = r#"
        type Pair = { x: u64, y: u64 }

        fn fill(out p: Pair) -> u64 {
            p.x = 1;
            p.y
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::UseBeforeInit(_, _) => {}
            e => panic!("Expected UseBeforeInit error, got {:?}", e),
        }
    }
}

#[test]
fn test_out_param_array_indices_ok() {
    let source = r#"
        fn fill(out arr: u64[2]) {
            arr[0] = 1;
            arr[1] = 2;
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_overlap_inout_same_base_rejected() {
    let source = r#"
        fn update(inout a: u64[2], inout b: u64[2]) {
            a[0] = 1;
            b[1] = 2;
        }

        fn main() {
            var arr = u64[0, 0];
            update(inout arr, inout arr);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SemCheckError::OverlappingLvalueArgs(_))),
            "Expected OverlappingLvalueArgs error"
        );
    }
}

#[test]
fn test_overlap_in_only_ok() {
    let source = r#"
        fn read(a: u64[2], b: u64[2]) -> u64 {
            a[0] + b[1]
        }

        fn main() -> u64 {
            let arr = u64[1, 2];
            read(arr, arr)
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_overlap_disjoint_bases_ok() {
    let source = r#"
        fn update(inout a: u64[2], inout b: u64[2]) {
            a[0] = 1;
            b[1] = 2;
        }

        fn main() {
            var a = u64[1, 2];
            var b = u64[3, 4];
            update(inout a, inout b);
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_overlap_disjoint_fields_after_dynamic_index_ok() {
    let source = r#"
        type Pair = { x: u64[2], y: u64[2] }

        fn update(inout a: u64[2], inout b: u64[2]) {
            a[0] = 1;
            b[0] = 2;
        }

        fn main() {
            var arr = [
                Pair { x: [1, 2], y: [3, 4] },
                Pair { x: [5, 6], y: [7, 8] },
            ];
            let i = 0;
            let j = 1;
            update(inout arr[i].x, inout arr[j].y);
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_overlap_slices_rejected() {
    let source = r#"
        fn update(inout a: u64[], inout b: u64[]) {
        }

        fn main() {
            var arr = [1, 2, 3, 4];
            update(inout arr[0..3], inout arr[1..4]);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SemCheckError::OverlappingLvalueArgs(_))),
            "Expected OverlappingLvalueArgs error"
        );
    }
}

#[test]
fn test_overlap_borrow_and_sink_move_rejected() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn take(p: ^Point, sink q: ^Point) {
        }

        fn main() {
            let p = ^Point { x: 1, y: 2 };
            take(p, move p);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SemCheckError::OverlappingLvalueArgs(_))),
            "Expected OverlappingLvalueArgs error"
        );
    }
}

#[test]
fn test_slice_borrow_blocks_mutation() {
    let source = r#"
        fn main() {
            var arr = [1, 2, 3];
            let s = arr[0..2];
            arr[0] = 9;
            s[0];
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SemCheckError::SliceBorrowConflict(_))),
            "Expected SliceBorrowConflict error"
        );
    }
}

#[test]
fn test_slice_borrow_allows_mutation_after_last_use() {
    let source = r#"
        fn main() {
            var arr = [1, 2, 3];
            let s = arr[0..2];
            let v = s[0];
            arr[0] = 9;
            v;
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_ok());
}

#[test]
fn test_slice_borrow_flow_sensitive_rebind() {
    let source = r#"
        fn main() {
            var a = [1, 2, 3];
            var b = [4, 5, 6];

            var s = a[0..2];
            s[0];

            s = b[0..2];
            a[0] = 9;
            s[0];
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_ok());
}

#[test]
fn test_slice_borrow_rebind_not_all_paths_rejected() {
    let source = r#"
        fn main() {
            var a = [1, 2, 3];
            var b = [4, 5, 6];
            var s = a[0..2];

            if true {
                s = b[0..2];
            } else {
                s[0];
            };

            a[0] = 9;
            s[0];
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SemCheckError::SliceBorrowConflict(_)))
        );
    }
}

#[test]
fn test_slice_return_forbidden() {
    let source = r#"
        fn test() -> u64[] {
            let arr = [1, 2, 3];
            let s = arr[0..2];
            s
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SemCheckError::SliceEscapeReturn(_))),
            "Expected SliceEscapeReturn error"
        );
    }
}

#[test]
fn test_slice_store_forbidden_in_struct() {
    let source = r#"
        type Holder = { s: u64[] }

        fn test() -> u64 {
            let arr = [1, 2, 3];
            let s = arr[0..2];
            let _h = Holder { s: s };
            0
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SemCheckError::SliceEscapeStore(_))),
            "Expected SliceEscapeStore error"
        );
    }
}

#[test]
fn test_slice_target_requires_lvalue() {
    let source = r#"
        fn test() -> u64 {
            let s = [1, 2, 3][0..2];
            s[0]
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, SemCheckError::SliceTargetNotLvalue(_))),
            "Expected SliceTargetNotLvalue error"
        );
    }
}

#[test]
fn test_slice_target_lvalue_allowed() {
    let source = r#"
        fn test() -> u64 {
            let arr = [1, 2, 3];
            let s = arr[0..2];
            s[0]
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_ok());
}
