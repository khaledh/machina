use std::borrow::Cow;

use super::*;

#[test]
fn test_map_cow_borrows_when_unchanged() {
    let ty = Type::Array {
        elem_ty: Box::new(Type::uint(64)),
        dims: vec![4],
    };

    let mapped = ty.map_cow(&|_| None);
    assert!(matches!(mapped, Cow::Borrowed(_)));
}

#[test]
fn test_map_cow_rewrites_only_when_needed() {
    let ty = Type::Tuple {
        field_tys: vec![Type::Var(TyVarId::new(1)), Type::Bool],
    };

    let mapped = ty.map_cow(&|t| match t {
        Type::Var(var) if *var == TyVarId::new(1) => Some(Type::uint(64)),
        _ => None,
    });
    assert!(matches!(mapped, Cow::Owned(_)));
    assert_eq!(
        mapped.into_owned(),
        Type::Tuple {
            field_tys: vec![Type::uint(64), Type::Bool]
        }
    );
}

#[test]
fn test_type_shape_eq_distinguishes_nominal_shapes_with_same_name() {
    let left = Type::Struct {
        name: "Box<u64>".to_string(),
        type_args: Vec::new(),
        fields: vec![StructField {
            name: "value".to_string(),
            ty: Type::uint(64),
        }],
    };
    let right = Type::Struct {
        name: "Box<u64>".to_string(),
        type_args: Vec::new(),
        fields: vec![StructField {
            name: "value".to_string(),
            ty: Type::String,
        }],
    };

    assert_eq!(
        left, right,
        "shallow nominal equality should remain unchanged"
    );
    assert!(
        !left.shape_eq(&right),
        "shape equality should see through nominal field differences"
    );
}

#[test]
fn test_type_shape_eq_recurse_through_nested_nominals() {
    let left = Type::Tuple {
        field_tys: vec![Type::Struct {
            name: "Box<u64>".to_string(),
            type_args: Vec::new(),
            fields: vec![StructField {
                name: "value".to_string(),
                ty: Type::uint(64),
            }],
        }],
    };
    let right = Type::Tuple {
        field_tys: vec![Type::Struct {
            name: "Box<u64>".to_string(),
            type_args: Vec::new(),
            fields: vec![StructField {
                name: "value".to_string(),
                ty: Type::String,
            }],
        }],
    };

    assert!(
        !left.shape_eq(&right),
        "shape equality should recurse through containing types"
    );
}

#[test]
fn test_borrow_type_display_and_runtime_shape() {
    let borrowed_string = Type::Borrow {
        elem_ty: Box::new(Type::String),
    };
    let borrowed_u64 = Type::Borrow {
        elem_ty: Box::new(Type::uint(64)),
    };

    assert_eq!(borrowed_string.to_string(), "borrow<string>");
    assert_eq!(borrowed_string.size_of(), Type::String.size_of());
    assert_eq!(borrowed_string.align_of(), Type::String.align_of());
    assert!(borrowed_string.is_compound());
    assert!(!borrowed_string.needs_drop());
    assert!(!borrowed_string.is_move_tracked());

    assert_eq!(borrowed_u64.to_string(), "borrow<u64>");
    assert!(!borrowed_u64.is_compound());
    assert_eq!(borrowed_u64.size_of(), Type::uint(64).size_of());
}
