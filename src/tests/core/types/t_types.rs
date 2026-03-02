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
