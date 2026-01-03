use crate::context::{AstContext, SemanticCheckedContext};
use crate::lexer::{LexError, Lexer, Token};
use crate::parser::Parser;
use crate::resolve::resolve;
use crate::semck::{SemCheckError, sem_check};
use crate::typeck::type_check;
use crate::types::Type;

fn sem_check_source(source: &str) -> Result<SemanticCheckedContext, Vec<SemCheckError>> {
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
    sem_check(type_checked_context)
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
            let x: range(100) = 42;
            0
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
}

#[test]
fn test_range_literal_out_of_bounds() {
    let source = r#"
        fn test() -> u64 {
            let x: range(50, 100) = 42;
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
fn test_range_invalid_bounds() {
    let source = r#"
        fn test() -> u64 {
            let x: range(10, 10) = 0;
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

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::EnumVariantPayloadArityMismatch(name, expected, got, _) => {
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
                Green => 1,
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
fn test_match_target_not_enum() {
    let source = r#"
        fn test() -> u64 {
            match 1 {
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
                assert_eq!(*ty, Type::uint(64));
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
            SemCheckError::InoutParamNotAggregate(_, _) => {}
            e => panic!("Expected InoutParamNotAggregate error, got {:?}", e),
        }
    }
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
            update(u64[1, 2, 3]);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InoutArgNotLvalue(_) => {}
            e => panic!("Expected InoutArgNotLvalue error, got {:?}", e),
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
            update(u64[1, 2, 3]);
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
fn test_inout_arg_not_mutable() {
    let source = r#"
        fn update(inout arr: u64[3]) {
            arr[0] = 10;
        }

        fn main() {
            let arr = u64[1, 2, 3];
            update(arr);
        }
    "#;

    let result = sem_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            SemCheckError::InoutArgNotMutable(_) => {}
            e => panic!("Expected InoutArgNotMutable error, got {:?}", e),
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
            update(arr);
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
            fill(arr);
            arr[0];
        }
    "#;

    let _ctx = sem_check_source(source).expect("Failed to sem check");
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
fn test_overlap_inout_same_base_rejected() {
    let source = r#"
        fn update(inout a: u64[2], inout b: u64[2]) {
            a[0] = 1;
            b[1] = 2;
        }

        fn main() {
            var arr = u64[0, 0];
            update(arr, arr);
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
            update(a, b);
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
            update(arr[i].x, arr[j].y);
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
            update(arr[0..3], arr[1..4]);
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
