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
