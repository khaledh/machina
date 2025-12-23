use super::*;
use crate::context::AstContext;
use crate::lexer::{LexError, Lexer, Token};
use crate::parser::Parser;
use crate::resolve::resolve;
use crate::types::Type;

fn type_check_source(source: &str) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");

    let ast_context = AstContext::new(module);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    type_check(resolved_context)
}

#[test]
fn test_multidim_array_literal_type() {
    let source = r#"
        fn test() -> u64 {
            let arr = [[1, 2], [3, 4]];
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
    // If it type checks successfully, the test passes
}

#[test]
fn test_multidim_array_type_inference() {
    let source = r#"
        fn test() -> u64[2, 2] {
            [[1, 2], [3, 4]]
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
    // If it type checks, the inferred type matches the return type
}

#[test]
fn test_multi_index_returns_element_type() {
    let source = r#"
        fn test() -> u64 {
            let arr = [[1, 2, 3], [4, 5, 6]];
            arr[1, 2]
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
    // If it type checks with u64 return type, the index expr has type u64
}

#[test]
fn test_3d_array_type() {
    let source = r#"
        fn test() -> u64 {
            let arr = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]];
            arr[1, 1, 1]
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_too_many_indices_error() {
    let source = r#"
        fn test() -> u64 {
            let arr = [[1, 2], [3, 4]];
            arr[0, 0, 0]
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::TooManyIndices(expected, got, _) => {
                assert_eq!(*expected, 2);
                assert_eq!(*got, 3);
            }
            e => panic!("Expected TooManyIndices error, got {:?}", e),
        }
    }
}

#[test]
fn test_array_dimension_mismatch() {
    let source = r#"
        fn test() -> u64 {
            let arr = [[1, 2, 3], [4, 5]];
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::ArrayElementTypeMismatch(_, _, _) => {
                // Expected error
            }
            e => panic!("Expected ArrayElementTypeMismatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_index_type_must_be_u64() {
    let source = r#"
        fn test() -> u64 {
            let arr = [[1, 2], [3, 4]];
            arr[true, 0]
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::IndexTypeNotInt(ty, _) => {
                assert_eq!(*ty, Type::Bool);
            }
            e => panic!("Expected IndexTypeNotInt error, got {:?}", e),
        }
    }
}

#[test]
fn test_partial_indexing_returns_subarray() {
    let source = r#"
        fn test() -> u64[3] {
            let arr = [[1, 2, 3], [4, 5, 6]];
            arr[1]
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
    // If it type checks with u64[3] return type, partial indexing works
}

#[test]
fn test_assignment_to_multidim_array_element() {
    let source = r#"
        fn test() -> u64 {
            var arr = [[1, 2], [3, 4]];
            arr[0, 1] = 99;
            arr[0, 1]
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_tuple_pattern_basic() {
    let source = r#"
        fn test() -> u64 {
            let (a, b) = (42, true);
            a
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_tuple_pattern_nested() {
    let source = r#"
        fn test() -> u64 {
            let (a, (b, c)) = (1, (2, 3));
            a + b + c
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_tuple_pattern_with_array() {
    let source = r#"
        fn test() -> u64 {
            let (a, [b, c]) = (1, [2, 3]);
            a + b + c
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_enum_literal_type() {
    let source = r#"
        type Color = Red | Green | Blue

        fn main() -> Color {
            Color::Green
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_enum_literal_in_let() {
    let source = r#"
        type Color = Red | Green

        fn main() -> Color {
            let c: Color = Color::Red;
            c
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_tuple_pattern_length_mismatch() {
    let source = r#"
        fn test() -> u64 {
            let (a, b, c) = (1, 2);
            a
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::TuplePatternLengthMismatch(expected, got, _) => {
                assert_eq!(*expected, 2);
                assert_eq!(*got, 3);
            }
            e => panic!("Expected TuplePatternLengthMismatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_tuple_pattern_type_mismatch() {
    let source = r#"
        fn test() -> u64 {
            let (a, b) = 42;
            a
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::PatternTypeMismatch(_, _, _) => {
                // Expected error
            }
            e => panic!("Expected PatternTypeMismatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_var_tuple_pattern() {
    let source = r#"
        fn test() -> u64 {
            var (a, b) = (1, 2);
            a = 10;
            b = 20;
            a + b
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_struct_pattern_shorthand() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            let Point { x, y } = Point { x: 1, y: 2 };
            x + y
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
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

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::StructFieldsMissing(_, _) => {
                // Expected error
            }
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

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::UnknownStructField(_, _) => {
                // Expected error
            }
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

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::DuplicateStructField(_, _) => {
                // Expected error
            }
            e => panic!("Expected DuplicateStructField error, got {:?}", e),
        }
    }
}

#[test]
fn test_struct_update_basic() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            let p = Point { x: 1, y: 2 };
            let q = { p | x: 3 };
            q.x
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
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

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::UnknownStructField(_, _) => {
                // Expected error
            }
            e => panic!("Expected UnknownStructField error, got {:?}", e),
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

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::DuplicateStructField(_, _) => {
                // Expected error
            }
            e => panic!("Expected DuplicateStructField error, got {:?}", e),
        }
    }
}

#[test]
fn test_struct_update_field_type_mismatch() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            let p = Point { x: 1, y: 2 };
            let _q = { p | x: true };
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::StructFieldTypeMismatch(_, _, _, _) => {
                // Expected error
            }
            e => panic!("Expected StructFieldTypeMismatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_struct_update_invalid_target() {
    let source = r#"
        fn test() -> u64 {
            let _q = { 1 | x: 2 };
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::InvalidStructUpdateTarget(_, _) => {
                // Expected error
            }
            e => panic!("Expected InvalidStructUpdateTarget error, got {:?}", e),
        }
    }
}

#[test]
fn test_enum_variant_payload_ok() {
    let source = r#"
        type Pair = A(u64) | B(u64)

        fn test() -> Pair {
            Pair::B(1)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_enum_variant_payload_arity_mismatch() {
    let source = r#"
        type Pair = A(u64) | B(u64)

        fn test() -> Pair {
            Pair::A(1, 2)
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::EnumVariantPayloadArityMismatch(name, expected, got, _) => {
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
fn test_enum_variant_payload_type_mismatch() {
    let source = r#"
        type Pair = A(u64) | B(u64)

        fn test() -> Pair {
            Pair::A(true)
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match &errors[0] {
            TypeCheckError::EnumVariantPayloadTypeMismatch(name, index, expected, found, _) => {
                assert_eq!(name, "A");
                assert_eq!(*index, 0);
                assert_eq!(*expected, Type::UInt64);
                assert_eq!(*found, Type::Bool);
            }
            e => panic!("Expected EnumVariantPayloadTypeMismatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_match_enum_ok() {
    let source = r#"
        type Color = Red(u64) | Green

        fn test(c: Color) -> u64 {
            match c {
                Red(x) => x,
                Green => 0,
                _ => 1,
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
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

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            TypeCheckError::NonExhaustiveMatch(_) => {
                // Expected error
            }
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

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            TypeCheckError::DuplicateMatchVariant(name, _) => {
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

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match &errors[0] {
            TypeCheckError::MatchTargetNotEnum(ty, _) => {
                assert_eq!(*ty, Type::UInt64);
            }
            e => panic!("Expected MatchTargetNotEnum error, got {:?}", e),
        }
    }
}
