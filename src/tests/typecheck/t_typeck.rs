use super::*;
use std::collections::HashMap;

use crate::core::capsule::ModuleId;
use crate::core::context::{ParsedContext, TypeCheckedContext};
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::Parser;
use crate::core::resolve::resolve;
use crate::core::types::Type;

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

fn type_check_source_with_def_owners(
    source: &str,
    owner_by_name: &[(&str, ModuleId)],
) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
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

    let owner_by_name = owner_by_name
        .iter()
        .map(|(name, module_id)| ((*name).to_string(), *module_id))
        .collect::<HashMap<_, _>>();
    let mut def_owners = HashMap::new();
    for def in resolved_context.def_table.clone() {
        if let Some(module_id) = owner_by_name.get(&def.name) {
            def_owners.insert(def.id, *module_id);
        }
    }

    type_check(resolved_context.with_def_owners(def_owners))
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
fn test_set_literal_type() {
    let source = r#"
        fn test() -> u64 {
            let s = {1, 2, 3};
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_typed_empty_set_literal_type() {
    let source = r#"
        fn test() -> u64 {
            let s = set<u64>{};
            0
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_set_methods_and_properties_typecheck() {
    let source = r#"
        fn test() -> u64 {
            var s = set<u64>{1, 2};
            let inserted = s.insert(3);
            let has_three = s.contains(3);
            let removed = s.remove(2);
            let cap = s.capacity;
            s.clear();
            if inserted && has_three && removed { cap } else { s.len }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_set_property_called_as_method_rejected() {
    let source = r#"
        fn test() -> u64 {
            let s = set<u64>{1, 2};
            s.capacity()
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
}

#[test]
fn test_set_unsupported_element_type_rejected() {
    let source = r#"
        type Foo = { x: u64^ }

        fn test() -> u64 {
            let s = set<Foo>{};
            s.len
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind(), TypeCheckErrorKind::TypeNotHashable(_, _, _, _)))
        );
    }
}

#[test]
fn test_map_literal_type() {
    let source = r#"
        fn test() -> u64 {
            let m = {1: 10, 2: 20};
            if m.contains_key(1) { m.len } else { 0 }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_typed_empty_map_literal_type() {
    let source = r#"
        fn test() -> u64 {
            let m = map<u64, u64>{};
            m.len
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_map_methods_and_properties_typecheck() {
    let source = r#"
        fn test() -> u64 {
            var m = map<u64, string>{1: "one"};
            let inserted = m.insert(2, "two");
            let has_one = m.contains_key(1);
            let removed = m.remove(1);
            let cap = m.capacity;
            let empty0 = m.is_empty;
            m.clear();
            let empty1 = m.is_empty;
            if inserted && has_one && removed && !empty0 && empty1 { cap } else { m.len }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_map_property_called_as_method_rejected() {
    let source = r#"
        fn test() -> u64 {
            let m = map<u64, bool>{1: true};
            m.capacity()
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
}

#[test]
fn test_map_unsupported_key_type_rejected() {
    let source = r#"
        type Foo = { x: u64^ }

        fn test() -> u64 {
            let m = map<Foo, u64>{};
            m.len
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind(), TypeCheckErrorKind::TypeNotHashable(_, _, _, _)))
        );
    }
}

#[test]
fn test_map_index_get_typecheck() {
    let source = r#"
        fn test() -> u64 {
            let m = map<u64, u64>{1: 10, 2: 20};
            match m[2] {
                value: u64 => value,
                _ => 0,
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_map_index_key_type_mismatch_rejected() {
    let source = r#"
        fn test() -> u64 {
            let m = map<u64, u64>{1: 10};
            let x = m[true];
            match x {
                value: u64 => value,
                _ => 0,
            }
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind(), TypeCheckErrorKind::MapKeyTypeMismatch(_, _, _)))
        );
    }
}

#[test]
fn test_map_method_get_typecheck() {
    let source = r#"
        fn test() -> u64 {
            let m = map<u64, u64>{1: 10, 2: 20};
            match m.get(2) {
                value: u64 => value,
                _ => 0,
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_map_method_get_key_type_mismatch_rejected() {
    let source = r#"
        fn test() -> u64 {
            let m = map<u64, u64>{1: 10};
            let x = m.get(true);
            match x {
                value: u64 => value,
                _ => 0,
            }
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind(), TypeCheckErrorKind::ArgTypeMismatch(_, _, _, _)))
        );
    }
}

#[test]
fn test_equality_bool_operands_typecheck() {
    let source = r#"
        fn test() -> bool {
            true == false
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_equality_string_operands_typecheck() {
    let source = r#"
        fn test() -> bool {
            "abc" == "abc"
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_equality_struct_operands_typecheck() {
    let source = r#"
        type Pair = { a: u64, b: bool }

        fn test() -> bool {
            let x = Pair { a: 1, b: true };
            let y = Pair { a: 1, b: true };
            x == y
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_equality_enum_operands_typecheck() {
    let source = r#"
        type E = A(u64, bool) | B

        fn test() -> bool {
            let x = E::A(3, true);
            let y = E::A(3, true);
            x == y
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_equality_non_equatable_operand_rejected() {
    let source = r#"
        fn test() -> bool {
            let s = set<u64>{1, 2};
            s == s
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind(), TypeCheckErrorKind::TypeNotEquatable(_, _, _, _)))
        );
    }
}

#[test]
fn test_set_non_hashable_reports_root_path() {
    let source = r#"
        type Inner = { p: u64^ }
        type Foo = { inner: Inner }

        fn test() -> u64 {
            let s = set<Foo>{};
            s.len
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
    if let Err(errors) = result {
        assert!(errors.iter().any(|e| match e.kind() {
            TypeCheckErrorKind::TypeNotHashable(_, path, _, _) => path == "value",
            _ => false,
        }));
    }
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
fn test_opaque_struct_construction_cross_module_rejected() {
    let source = r#"
        @[opaque]
        type Secret = { x: u64 }

        fn build_secret() -> Secret {
            Secret { x: 1 }
        }
    "#;

    let result = type_check_source_with_def_owners(
        source,
        &[("Secret", ModuleId(1)), ("build_secret", ModuleId(2))],
    );
    assert!(result.is_err());
    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::OpaqueTypeConstruction(name, _) if name == "Secret"
        )));
    }
}

#[test]
fn test_opaque_struct_field_access_cross_module_rejected() {
    let source = r#"
        @[opaque]
        type Secret = { x: u64 }

        fn read_secret(s: Secret) -> u64 {
            s.x
        }
    "#;

    let result = type_check_source_with_def_owners(
        source,
        &[("Secret", ModuleId(1)), ("read_secret", ModuleId(2))],
    );
    assert!(result.is_err());
    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::OpaqueFieldAccess(type_name, field, _)
                if type_name == "Secret" && field == "x"
        )));
    }
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
fn test_dyn_array_append_and_properties_typecheck() {
    let source = r#"
        fn test() -> u64 {
            var arr: u64[*] = [1, 2, 3];
            arr.append(4);
            let cap = arr.capacity;
            let empty = arr.is_empty;
            if empty { 0 } else { cap + arr[3] }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_dyn_array_property_called_as_method_rejected() {
    let source = r#"
        fn test() -> u64 {
            var arr: u64[*] = [1, 2, 3];
            arr.capacity()
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());
}

#[test]
fn test_dyn_array_to_slice_coercion_typecheck() {
    let source = r#"
        fn first(xs: u64[]) -> u64 {
            xs[0]
        }

        fn test() -> u64 {
            var arr: u64[*] = [1, 2, 3];
            first(arr)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_dyn_array_append_drop_elem_typecheck() {
    let source = r#"
        fn test() -> u64 {
            var arr: string[*] = ["a"];
            arr.append("b");
            0
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
fn test_generic_return_infers_into_unannotated_let() {
    let source = r#"
        type Option<T> = None | Some(T)

        fn make_some<T>(x: T) -> Option<T> { Some(x) }

        fn test() -> u64 {
            let opt = make_some(4);
            match opt {
                Some(x) => x,
                None => 0,
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_return_infers_from_usage() {
    let source = r#"
        fn make<T>() -> T;

        fn test() -> u64 {
            let value = make();
            value
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_match_pattern_constrains_unknown_tuple_scrutinee() {
    let source = r#"
        fn make<T>() -> T;

        fn test() -> u64 {
            let value = make();
            match value {
                (x, y) => x + y,
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_match_pattern_constrains_unknown_enum_scrutinee() {
    let source = r#"
        type Option<T> = None | Some(T)

        fn make<T>() -> T;

        fn test() -> u64 {
            let value = make();
            match value {
                Option::Some(x) => x,
                Option::None => 0,
            }
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
fn test_generic_struct_lit_infers_from_fields() {
    let source = r#"
        type Pair<T> = { left: T, right: T }

        fn test() -> u64 {
            let p = Pair { left: 1, right: 2 };
            p.left + p.right
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_struct_lit_omitted_args_in_generic_fn_body() {
    let source = r#"
        type Pair<L, R> = { left: L, right: R }

        fn pair<L, R>(left: L, right: R) -> Pair<L, R> {
            Pair { left: left, right: right }
        }

        fn test() -> u64 {
            let p = pair(1, 2);
            p.right
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_enum_variant_infers_from_payload() {
    let source = r#"
        type Option<T> = None | Some(T)

        fn test() -> u64 {
            let value = Option::Some(3);
            match value {
                Some(x) => x,
                None => 0,
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_generic_type_args_missing_is_error_without_constraints() {
    let source = r#"
        type Option<T> = Some(T) | None

        fn test() -> () {
            let value = Option::None;
            ()
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
fn test_trait_method_impl_and_call_typechecks() {
    let source = r#"
        trait Runnable {
            fn run(self) -> u64;
        }

        type Process = { name: string }

        Process :: Runnable {
            fn run(self) -> u64 {
                1
            }
        }

        fn main() -> u64 {
            let p = Process { name: "demo" };
            p.run()
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_trait_method_missing_impl_errors() {
    let source = r#"
        trait Runnable {
            fn run(self);
            fn stop(self);
        }

        type Process = { name: string }

        Process :: Runnable {
            fn run(self) {
                ()
            }
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::TraitMethodMissingImpl(type_name, trait_name, method, _)
                if type_name == "Process" && trait_name == "Runnable" && method == "stop"
        )));
    }
}

#[test]
fn test_trait_method_not_in_trait_errors() {
    let source = r#"
        trait Runnable {
            fn run(self);
        }

        type Process = { name: string }

        Process :: Runnable {
            fn run(self) {
                ()
            }

            fn stop(self) {
                ()
            }
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::TraitMethodNotInTrait(type_name, trait_name, method, _)
                if type_name == "Process" && trait_name == "Runnable" && method == "stop"
        )));
    }
}

#[test]
fn test_trait_method_signature_mismatch_errors() {
    let source = r#"
        trait Runnable {
            fn run(self, n: u64) -> u64;
        }

        type Process = { name: string }

        Process :: Runnable {
            fn run(self, n: bool) -> u64 {
                if n { 1 } else { 0 }
            }
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::TraitMethodSignatureMismatch(type_name, trait_name, method, _)
                if type_name == "Process" && trait_name == "Runnable" && method == "run"
        )));
    }
}

#[test]
fn test_duplicate_trait_impl_block_errors() {
    let source = r#"
        trait Runnable {
            fn run(self);
        }

        type Process = { name: string }

        Process :: Runnable {
            fn run(self) {
                ()
            }
        }

        Process :: Runnable {
            fn run(self) {
                ()
            }
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::TraitImplDuplicate(type_name, trait_name, _)
                if type_name == "Process" && trait_name == "Runnable"
        )));
    }
}

#[test]
fn test_trait_property_contract_getter_setter_ok() {
    let source = r#"
        trait HasLength {
            prop len: u64 { get; }
            prop value: u64 { get; set; }
        }

        type Buffer = { len: u64, value: u64 }

        Buffer :: HasLength {
            prop len: u64 {
                get { self.len }
            }

            prop value: u64 {
                get { self.value }
                set(v) { self.value = v; }
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_trait_property_contract_missing_setter_errors() {
    let source = r#"
        trait HasValue {
            prop value: u64 { get; set; }
        }

        type Buffer = { value: u64 }

        Buffer :: HasValue {
            prop value: u64 {
                get { self.value }
            }
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::TraitPropertyMissingSetter(type_name, trait_name, prop, _)
                if type_name == "Buffer" && trait_name == "HasValue" && prop == "value"
        )));
    }
}

#[test]
fn test_property_getter_call_syntax_errors() {
    let source = r#"
        type Buffer = { value: u64 }

        Buffer :: {
            prop value_prop: u64 {
                get { self.value }
            }
        }

        fn main() -> u64 {
            let b = Buffer { value: 7 };
            b.value_prop()
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::PropertyCalledAsMethod(prop, _) if prop == "value_prop"
        )));
    }
}

#[test]
fn test_property_setter_call_syntax_errors() {
    let source = r#"
        type Buffer = { value: u64 }

        Buffer :: {
            prop value_prop: u64 {
                get { self.value }
                set(v) { self.value = v; }
            }
        }

        fn main() {
            var b = Buffer { value: 7 };
            b.value_prop(9);
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::PropertyCalledAsMethod(prop, _) if prop == "value_prop"
        )));
    }
}

#[test]
fn test_trait_bound_allows_implementing_argument() {
    let source = r#"
        trait Runnable {
            fn run(self) -> u64;
        }

        type Process = { ticks: u64 }

        Process :: Runnable {
            fn run(self) -> u64 {
                self.ticks + 1
            }
        }

        fn execute<T: Runnable>(value: T) -> u64 {
            value.run()
        }

        fn main() -> u64 {
            execute(Process { ticks: 41 })
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_trait_bound_rejects_non_implementing_argument() {
    let source = r#"
        trait Runnable {
            fn run(self) -> u64;
        }

        type Process = { ticks: u64 }
        type Task = { id: u64 }

        Process :: Runnable {
            fn run(self) -> u64 {
                self.ticks + 1
            }
        }

        fn execute<T: Runnable>(value: T) -> u64 {
            value.run()
        }

        fn main() -> u64 {
            execute(Task { id: 1 })
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::TraitBoundNotSatisfied(trait_name, ty, _)
                if trait_name == "Runnable"
                    && matches!(ty, Type::Struct { name, .. } if name == "Task")
        )));
        assert!(
            !errors
                .iter()
                .any(|e| matches!(e.kind(), TypeCheckErrorKind::UnknownType(_))),
            "unexpected cascaded UnknownType diagnostic: {errors:?}"
        );
    }
}

#[test]
fn test_closure_infers_param_and_return_types_from_call_site() {
    let source = r#"
        fn test() -> u64 {
            let add = |x| x + 1;
            add(41)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_closure_infers_return_type_without_annotation() {
    let source = r#"
        fn test() -> u64 {
            let forty_two = || 42;
            forty_two()
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_higher_order_typeless_closure_infers_through_call() {
    let source = r#"
        fn test() -> u64 {
            let apply = |f, x| f(x);
            apply(|n| n + 1, 41)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_higher_order_typeless_closure_propagates_return_mismatch() {
    let source = r#"
        fn test() -> u64 {
            let apply = |f, x| f(x);
            let y: bool = apply(|n| n + 1, 41);
            if y { 1 } else { 0 }
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(!errors.is_empty(), "Expected at least one error");
        match errors[0].kind() {
            TypeCheckErrorKind::DeclTypeMismatch(_, _, _) => {}
            e => panic!("Expected DeclTypeMismatch error, got {:?}", e),
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
fn test_try_operator_propagates_error_union() {
    let source = r#"
        type IoError = { code: u64 }

        fn fail() -> u64 | IoError {
            IoError { code: 1 }
        }

        fn run() -> u64 | IoError {
            fail()?;
            fail()
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_try_operator_allows_direct_success_return_expression() {
    let source = r#"
        type IoError = { code: u64 }

        fn ok(value: u64) -> u64 | IoError {
            value
        }

        fn fail() -> u64 | IoError {
            IoError { code: 1 }
        }

        fn choose(flag: bool, value: u64) -> u64 | IoError {
            if flag {
                ok(value)
            } else {
                fail()
            }
        }

        fn add_one(flag: bool, value: u64) -> u64 | IoError {
            let base = choose(flag, value)?;
            base + 1
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_try_operator_rejects_non_union_operand() {
    let source = r#"
        fn run() -> u64 {
            let value = 7?;
            value
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind(), TypeCheckErrorKind::TryOperandNotErrorUnion(_, _)))
        );
    }
}

#[test]
fn test_try_operator_rejects_non_union_return_type() {
    let source = r#"
        type IoError = { code: u64 }

        fn read() -> u64 | IoError {
            IoError { code: 1 }
        }

        fn run() -> u64 {
            let value = read()?;
            value
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(errors.iter().any(|e| matches!(
            e.kind(),
            TypeCheckErrorKind::TryReturnTypeNotErrorUnion(_, _)
        )));
    }
}

#[test]
fn test_try_operator_rejects_error_not_in_return_set() {
    let source = r#"
        type IoError = { code: u64 }
        type ParseError = { line: u64 }

        fn read() -> u64 | IoError {
            IoError { code: 1 }
        }

        fn run() -> u64 | ParseError {
            let value = read()?;
            value
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        let mismatch = errors
            .iter()
            .find_map(|e| match e.kind() {
                TypeCheckErrorKind::TryErrorNotInReturn(missing, _, _) => Some(missing),
                _ => None,
            })
            .expect("expected TryErrorNotInReturn");
        assert_eq!(mismatch.len(), 1);
        assert!(mismatch.iter().any(|name| name == "IoError"));
    }
}

#[test]
fn test_try_operator_reports_all_missing_error_variants() {
    let source = r#"
        type IoError = { code: u64 }
        type ParseError = { line: u64 }
        type ValidationError = { field: string }

        fn read() -> u64 | IoError | ParseError {
            IoError { code: 1 }
        }

        fn run() -> u64 | ValidationError {
            let value = read()?;
            value
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        let err = errors
            .iter()
            .find(|e| matches!(e.kind(), TypeCheckErrorKind::TryErrorNotInReturn(_, _, _)))
            .expect("expected TryErrorNotInReturn");
        match err.kind() {
            TypeCheckErrorKind::TryErrorNotInReturn(missing, _, _) => {
                assert_eq!(missing.len(), 2);
                assert!(missing.iter().any(|name| name == "IoError"));
                assert!(missing.iter().any(|name| name == "ParseError"));
            }
            other => panic!("expected TryErrorNotInReturn, got {other:?}"),
        }

        let rendered = format!("{}", err.kind());
        assert!(
            rendered.contains("add them to the return union")
                && rendered.contains("handle them with match"),
            "expected fix-oriented text in diagnostic, got: {rendered}"
        );
        assert!(
            !rendered.contains("[") && !rendered.contains("\""),
            "expected compact variant names, got: {rendered}"
        );
    }
}

#[test]
fn test_return_union_mismatch_reports_union_variants() {
    let source = r#"
        type IoError = { code: u64 }

        fn run() -> u64 | IoError {
            return true;
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        let err = errors
            .iter()
            .find(|e| matches!(e.kind(), TypeCheckErrorKind::ReturnNotInErrorUnion(_, _, _)))
            .expect("expected ReturnNotInErrorUnion");
        match err.kind() {
            TypeCheckErrorKind::ReturnNotInErrorUnion(variants, found, _) => {
                assert!(variants.iter().any(|name| name == "u64"));
                assert!(variants.iter().any(|name| name == "IoError"));
                assert_eq!(*found, Type::Bool);
            }
            other => panic!("expected ReturnNotInErrorUnion, got {other:?}"),
        }
    }
}

#[test]
fn test_match_typed_binding_rejects_non_union_variant_type() {
    let source = r#"
        type IoError = { code: u64 }

        fn read(v: u64) -> u64 | IoError {
            v
        }

        fn run(v: u64) -> u64 {
            let result = read(v);
            match result {
                s: string => 0,
                _ => 1,
            }
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        let err = errors
            .iter()
            .find(|e| {
                matches!(
                    e.kind(),
                    TypeCheckErrorKind::MatchTypedBindingTypeMismatch(_, _, _)
                )
            })
            .expect("expected MatchTypedBindingTypeMismatch");
        match err.kind() {
            TypeCheckErrorKind::MatchTypedBindingTypeMismatch(expected, found, _) => {
                assert!(matches!(found, Type::String));
                assert_eq!(expected.len(), 2);
                assert!(expected.iter().any(|name| name == "u64"));
                assert!(expected.iter().any(|name| name == "IoError"));
            }
            other => panic!("expected MatchTypedBindingTypeMismatch, got {other:?}"),
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
                assert_eq!(*ty, Type::sint(32));
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
                assert_eq!(*ty, Type::sint(32));
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
                assert_eq!(*ty, Type::sint(32));
            }
            e => panic!("Expected LogicalOperandNotBoolean error, got {:?}", e),
        }
    }
}

#[test]
fn test_default_int_literal_is_i32_when_unconstrained() {
    let source = r#"
        fn takes_i32(x: i32) -> i32 { x }

        fn test() -> i32 {
            let x = 1;
            takes_i32(x)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_int_literal_still_coerces_from_context() {
    let source = r#"
        fn takes_u64(x: u64) -> u64 { x }

        fn test() -> u64 {
            takes_u64(1)
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
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

#[test]
fn test_error_union_return_type_typechecks() {
    let source = r#"
        type IoError = { code: u64 }

        fn ok(v: u64) -> u64 | IoError {
            v
        }

        fn err() -> u64 | IoError {
            IoError { code: 1 }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_if_join_lifts_success_into_error_union() {
    let source = r#"
        type IoError = { code: u64 }

        fn choose(flag: bool, value: u64) -> u64 | IoError {
            if flag {
                value
            } else {
                IoError { code: 7 }
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_match_join_lifts_success_into_error_union() {
    let source = r#"
        type IoError = { code: u64 }
        type Tag = A | B

        fn choose(tag: Tag, value: u64) -> u64 | IoError {
            match tag {
                Tag::A => value,
                Tag::B => IoError { code: 9 },
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_if_join_infers_union_without_expected_type() {
    let source = r#"
        type IoError = { code: u64 }

        fn demo(flag: bool, value: u64) -> u64 {
            let joined = if flag {
                value
            } else {
                IoError { code: 13 }
            };
            match joined {
                value: u64 => value,
                err: IoError => err.code,
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_match_join_infers_union_without_expected_type() {
    let source = r#"
        type IoError = { code: u64 }

        fn demo(flag: bool, value: u64) -> u64 {
            let joined = match flag {
                true => value,
                false => IoError { code: 14 },
            };
            match joined {
                value: u64 => value,
                err: IoError => err.code,
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_if_join_canonical_order_keeps_try_success_path_stable() {
    let source = r#"
        type IoError = { code: u64 }

        fn demo(flag: bool, value: u64) -> u64 | IoError {
            let joined = if flag {
                IoError { code: 17 }
            } else {
                value
            };
            let n = joined?;
            n
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_join_arm_mismatch_reports_join_diagnostic() {
    let source = r#"
        type IoError = { code: u64 }

        fn demo(flag: bool) -> u64 {
            let joined = if flag {
                1
            } else {
                IoError { code: 19 }
            };
            let bad: bool = joined;
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind(), TypeCheckErrorKind::JoinArmTypeMismatch(_, _, _)))
        );
    }
}

#[test]
fn test_tuple_typed_binding_pattern_typechecks() {
    let source = r#"
        fn test(t: (u64, bool)) -> u64 {
            match t {
                (x: u64, _) => x,
            }
        }
    "#;

    let _ctx = type_check_source(source).expect("Failed to type check");
}

#[test]
fn test_error_union_not_allowed_in_param_type() {
    let source = r#"
        type IoError = { code: u64 }

        fn bad(x: u64 | IoError) -> u64 {
            0
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind(), TypeCheckErrorKind::UnionNotAllowedHere(_)))
        );
    }
}

#[test]
fn test_error_union_not_allowed_in_struct_field_type() {
    let source = r#"
        type Bad = {
            value: u64 | bool,
        }

        fn use_bad(x: Bad) -> u64 {
            x.value
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind(), TypeCheckErrorKind::UnionNotAllowedHere(_)))
        );
    }
}

#[test]
fn test_error_union_not_allowed_in_generic_argument_type() {
    let source = r#"
        type Box<T> = {
            value: T,
        }

        fn bad(x: Box<u64 | bool>) -> u64 {
            x.value
        }
    "#;

    let result = type_check_source(source);
    assert!(result.is_err());

    if let Err(errors) = result {
        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind(), TypeCheckErrorKind::UnionNotAllowedHere(_)))
        );
    }
}
