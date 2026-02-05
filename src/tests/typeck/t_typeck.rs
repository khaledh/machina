use super::*;
use crate::context::ParsedContext;
use crate::lexer::{LexError, Lexer, Token};
use crate::parse::Parser;
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
    let id_gen = parser.into_id_gen();

    let ast_context = ParsedContext::new(module, id_gen);
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
fn test_typed_array_literal_type() {
    let source = r#"
        fn test() -> u64 {
            let arr = u8[1, 2, 3];
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_typed_array_literal_type_mismatch() {
    let source = r#"
        fn test() -> u64 {
            let arr = u8[1, true];
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
}

#[test]
fn test_array_repeat_literal_type() {
    let source = r#"
        fn test() -> u64 {
            let arr = [1; 4];
            arr[0]
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_typed_array_repeat_literal_type_mismatch() {
    let source = r#"
        fn test() -> u64 {
            let arr = u8[true; 4];
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
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
fn test_struct_field_access_through_heap() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn test() -> u64 {
            let p = ^Point { x: 1, y: 2 };
            p.x
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_for_range_expr_typecheck() {
    let source = r#"
        fn test() -> u64 {
            let start = 0;
            let end = 4;
            for i in start..end { i; }
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_len_intrinsic_on_array() {
    let source = r#"
        fn test() -> u64 {
            let arr = [1, 2, 3];
            arr.len
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_len_intrinsic_on_string() {
    let source = r#"
        fn test() -> u64 {
            let s = "hello";
            s.len
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_if_without_else_requires_unit() {
    let source = r#"
        fn test() -> () {
            if true { 1 };
            ()
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
}

#[test]
fn test_if_without_else_unit_ok() {
    let source = r#"
        fn test() -> () {
            if true { };
            ()
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_tuple_field_access_through_heap() {
    let source = r#"
        fn test() -> u64 {
            let t = ^(1, 2);
            t.0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_identity_infers_from_arg() {
    let source = r#"
        fn id<T>(x: T) -> T {
            let y: T = x;
            y
        }

        fn test() -> u64 {
            id(3)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_overload_prefers_concrete() {
    let source = r#"
        fn foo(x: u64) -> u64 { x }
        fn foo<T>(x: T) -> T { x }

        fn test() -> u64 {
            foo(1)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_method_infers_from_arg() {
    let source = r#"
        type Boxed = { value: u64 }

        Boxed::{
            fn cast<T>(self, x: T) -> T { x }
        }

        fn test() -> u64 {
            let b = Boxed { value: 0 };
            b.cast(3)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_infers_from_expected_type() {
    let source = r#"
        fn make<T>() -> T;

        fn test() -> u64 {
            let value: u64 = make();
            value
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_expected_type_missing_is_error() {
    let source = r#"
        fn make<T>() -> T;

        fn test() -> () {
            let value = make();
            ()
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
}

#[test]
fn test_generic_struct_lit_with_type_args() {
    let source = r#"
        type Pair<T> = { left: T, right: T }

        fn test() -> u64 {
            let p = Pair<u64> { left: 1, right: 2 };
            p.left + p.right
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_enum_variant_with_type_args() {
    let source = r#"
        type Option<T> = None | Some(T)

        fn test() -> u64 {
            let value: Option<u64> = Option<u64>::Some(3);
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_type_args_missing_is_error() {
    let source = r#"
        type Pair<T> = { left: T, right: T }

        fn test() -> u64 {
            let p = Pair { left: 1, right: 2 };
            p.left
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
}

#[test]
fn test_array_index_through_heap() {
    let source = r#"
        fn test() -> u64 {
            let arr = ^[1, 2, 3];
            arr[1]
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_slice_index_typechecks() {
    let source = r#"
        fn test() -> u64 {
            let arr = [1, 2, 3];
            let s = arr[0..2];
            s[0]
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_slice_of_slice_typechecks() {
    let source = r#"
        fn test() -> u64 {
            let arr = [1, 2, 3];
            let s = arr[0..3];
            let t = s[1..3];
            t[0]
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_slice_2d_array_then_slice_row_typechecks() {
    let source = r#"
        fn test() -> u64 {
            let arr = [[1, 2, 3], [4, 5, 6]];
            let s_rows = arr[0..2];
            let row = s_rows[0];
            let s_elems = row[0..3];
            s_elems[0]
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_array_to_slice_call_typechecks() {
    let source = r#"
        fn sum(xs: u64[]) -> u64 {
            0
        }

        fn test() -> u64 {
            let arr = [1, 2, 3];
            sum(arr)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
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

    let _ctx = type_check_source(source).expect("Failed to type check");
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

        match errors[0].kind() {
            TypeCheckErrorKind::TooManyIndices(expected, got, _) => {
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

        match errors[0].kind() {
            TypeCheckErrorKind::DeclTypeMismatch(_, _, _) => {
                // Expected error
            }
            e => panic!("Expected DeclTypeMismatch error, got {:?}", e),
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

        match errors[0].kind() {
            TypeCheckErrorKind::IndexTypeNotInt(ty, _) => {
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
fn test_enum_literal_unqualified_in_let() {
    let source = r#"
        type Color = Red | Green

        fn main() -> Color {
            let c: Color = Red;
            c
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_enum_literal_unqualified_in_let() {
    let source = r#"
        type Option<T> = Some(T) | None

        fn main() -> Option<u64> {
            let c: Option<u64> = Some(3);
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

        match errors[0].kind() {
            TypeCheckErrorKind::TuplePatternLengthMismatch(expected, got, _) => {
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

        match errors[0].kind() {
            TypeCheckErrorKind::PatternTypeMismatch(_, _, _) => {
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

        match errors[0].kind() {
            TypeCheckErrorKind::StructFieldTypeMismatch(_, _, _, _) => {
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

        match errors[0].kind() {
            TypeCheckErrorKind::InvalidStructUpdateTarget(_, _) => {
                // Expected error
            }
            e => panic!("Expected InvalidStructUpdateTarget error, got {:?}", e),
        }
    }
}

#[test]
fn test_method_block_basic() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point :: {
            fn sum(self) -> u64 {
                self.x + self.y
            }
        }

        fn main() -> u64 {
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_method_return_mismatch() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point :: {
            fn bad(self) -> u64 {
                true
            }
        }

        fn main() -> u64 {
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");

        match errors[0].kind() {
            TypeCheckErrorKind::DeclTypeMismatch(_, _, _) => {
                // Expected error
            }
            e => panic!("Expected DeclTypeMismatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_enum_method_block_basic() {
    let source = r#"
        type Msg = Ping(u64) | Pong(u64)

        Msg :: {
            fn is_ping(self) -> bool {
                match self {
                    Msg::Ping(n) => n == 0,
                    Msg::Pong(n) => n == 0,
                    _ => false,
                }
            }
        }

        fn main() -> u64 {
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_enum_method_call_typechecks() {
    let source = r#"
        type Msg = Ping(u64) | Pong(u64)

        Msg :: {
            fn is_ping(self) -> bool {
                match self {
                    Msg::Ping(_) => true,
                    _ => false,
                }
            }
        }

        fn main() -> u64 {
            let msg = Msg::Ping(1);
            if msg.is_ping() { 1 } else { 0 }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_method_call_typechecks() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point :: {
            fn sum(self) -> u64 {
                self.x + self.y
            }
        }

        fn main() -> u64 {
            let p = Point { x: 1, y: 2 };
            p.sum()
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_method_call_arg_type_mismatch() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point :: {
            fn add(self, n: u64) -> u64 {
                self.x + self.y + n
            }
        }

        fn main() -> u64 {
            let p = Point { x: 1, y: 2 };
            p.add(true)
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::ArgTypeMismatch(_, _, _, _) => {}
            e => panic!("Expected ArgTypeMismatch error, got {:?}", e),
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

        match errors[0].kind() {
            TypeCheckErrorKind::EnumVariantPayloadTypeMismatch(name, index, expected, found, _) => {
                assert_eq!(name, "A");
                assert_eq!(*index, 0);
                assert_eq!(*expected, Type::uint(64));
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
fn test_for_range_typechecks() {
    let source = r#"
        fn test() -> u64 {
            for i in 0..3 { i; }
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_for_array_typechecks() {
    let source = r#"
        fn test() -> u64 {
            let arr = [1, 2, 3];
            var acc = 0;
            for x in arr { acc = acc + x; }
            acc
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_for_slice_typechecks() {
    let source = r#"
        fn test() -> u64 {
            let arr = [1, 2, 3];
            let xs = arr[0..3];
            var acc = 0;
            for x in xs { acc = acc + x; }
            acc
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_for_non_iterable_rejected() {
    let source = r#"
        fn test() -> u64 {
            for x in 0 { x; }
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::ForIterNotIterable(ty, _) => {
                assert_eq!(*ty, Type::uint(64));
            }
            e => panic!("Expected ForIterNotIterable error, got {:?}", e),
        }
    }
}

#[test]
fn test_logical_and_requires_bool() {
    let source = r#"
        fn test() -> bool {
            true && 1
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::LogicalOperandNotBoolean(ty, _) => {
                assert_eq!(*ty, Type::uint(64));
            }
            e => panic!("Expected LogicalOperandNotBoolean error, got {:?}", e),
        }
    }
}

#[test]
fn test_logical_not_requires_bool() {
    let source = r#"
        fn test() -> u64 {
            let _x = !1;
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::LogicalOperandNotBoolean(ty, _) => {
                assert_eq!(*ty, Type::uint(64));
            }
            e => panic!("Expected LogicalOperandNotBoolean error, got {:?}", e),
        }
    }
}

#[test]
fn test_mod_typechecks() {
    let source = r#"
        fn test() -> u64 {
            let _x = 10 % 3;
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_mod_requires_int() {
    let source = r#"
        fn test() -> u64 {
            let _x = true % 1;
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::ArithOperandNotInt(ty, _) => {
                assert_eq!(*ty, Type::Bool);
            }
            e => panic!("Expected ArithOperandNotInt error, got {:?}", e),
        }
    }
}

#[test]
fn test_bitwise_ops_typecheck() {
    let source = r#"
        fn test() -> u64 {
            let _a = 1 & 2;
            let _b = 1 | 2;
            let _c = 1 ^ 2;
            let _d = 1 << 2;
            let _e = 8 >> 1;
            let _f = ~1;
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_bitwise_ops_require_int() {
    let source = r#"
        fn test() -> u64 {
            let _x = true & 1;
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::ArithOperandNotInt(ty, _) => {
                assert_eq!(*ty, Type::Bool);
            }
            e => panic!("Expected ArithOperandNotInt error, got {:?}", e),
        }
    }
}

#[test]
fn test_negation_requires_int() {
    let source = r#"
        fn test() -> u64 {
            let _x = -true;
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::NegationOperandNotInt(ty, _) => {
                assert_eq!(*ty, Type::Bool);
            }
            e => panic!("Expected NegationOperandNotInt error, got {:?}", e),
        }
    }
}

#[test]
fn test_range_non_literal_assignment() {
    let source = r#"
        fn test() -> u64 {
            let y = 5;
            let x: u64: bounds(100) = y;  // allowed at compile-time; checked at runtime
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(
        result.is_ok(),
        "expected successful type check for non-literal range assignment"
    );
}

#[test]
fn test_nonzero_redundant_on_unsigned_bounds() {
    let source = r#"
        type NonZeroSmall = u64: bounds(1, 10) & nonzero;

        fn test() -> u64 {
            let _x: NonZeroSmall = 5;
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::RedundantNonZero(min, max, _) => {
                assert_eq!(*min, 1);
                assert_eq!(*max, 10);
            }
            e => panic!("Expected RedundantNonZero error, got {:?}", e),
        }
    }
}

#[test]
fn test_closure_typechecks() {
    let source = r#"
        fn test() -> u64 {
            let add = |a: u64, b: u64| -> u64 a + b;
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_closure_return_mismatch_rejected() {
    let source = r#"
        fn test() -> u64 {
            let _f = |a: u64| -> u64 { true };
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::DeclTypeMismatch(_, found, _) => {
                assert_eq!(*found, Type::Bool);
            }
            e => panic!("Expected DeclTypeMismatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_property_getter_typechecks() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point :: {
            prop sum: u64 {
                get { self.x + self.y }
            }
        }

        fn test() -> u64 {
            let p = Point { x: 1, y: 2 };
            p.sum
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_property_setter_typechecks() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point :: {
            prop y_val: u64 {
                get { self.y }
                set(v) { self.y = v; }
            }
        }

        fn test() -> u64 {
            var p = Point { x: 1, y: 2 };
            p.y_val = 5;
            p.y_val
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_property_read_only_rejects_assignment() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point :: {
            prop sum: u64 {
                get { self.x + self.y }
            }
        }

        fn test() -> u64 {
            var p = Point { x: 1, y: 2 };
            p.sum = 3;
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::PropertyNotWritable(name, _) => {
                assert_eq!(name, "sum");
            }
            e => panic!("Expected PropertyNotWritable error, got {:?}", e),
        }
    }
}

#[test]
fn test_property_write_only_rejects_read() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point :: {
            prop y_val: u64 {
                set(v) { self.y = v; }
            }
        }

        fn test() -> u64 {
            let p = Point { x: 1, y: 2 };
            p.y_val
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::PropertyNotReadable(name, _) => {
                assert_eq!(name, "y_val");
            }
            e => panic!("Expected PropertyNotReadable error, got {:?}", e),
        }
    }
}

#[test]
fn test_property_conflicts_with_field() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        Point :: {
            prop x: u64 {
                get { self.x }
            }
        }

        fn test() -> u64 {
            let p = Point { x: 1, y: 2 };
            p.x
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::PropertyConflictsWithField(name, field, _) => {
                assert_eq!(name, "x");
                assert_eq!(field, "x");
            }
            e => panic!("Expected PropertyConflictsWithField error, got {:?}", e),
        }
    }
}

#[test]
fn test_fn_type_annotation_typechecks() {
    let source = r#"
        fn test() -> u64 {
            let add: fn(u64, u64) -> u64 = |a: u64, b: u64| -> u64 a + b;
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_fn_type_alias_typechecks() {
    let source = r#"
        type Add = fn(u64, u64) -> u64

        fn test() -> u64 {
            let add: Add = |a: u64, b: u64| -> u64 a + b;
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_fn_type_annotation_mismatch_rejected() {
    let source = r#"
        fn test() -> u64 {
            let add: fn(u64, u64) -> u64 = |a: u64| -> u64 a;
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::DeclTypeMismatch(_, _, _) => {
                // Expected error
            }
            e => panic!("Expected DeclTypeMismatch error, got {:?}", e),
        }
    }
}

#[test]
fn test_closure_value_call_typechecks() {
    let source = r#"
        fn test() -> u64 {
            let inc: fn(u64) -> u64 = |x: u64| -> u64 x + 1;
            inc(2)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_function_value_assign_typechecks() {
    let source = r#"
        fn add(a: u64, b: u64) -> u64 {
            a + b
        }

        fn test() -> u64 {
            let f: fn(u64, u64) -> u64 = add;
            f(1, 2)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}
