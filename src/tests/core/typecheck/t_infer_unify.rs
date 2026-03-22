use crate::core::typecheck::{InferUnifier, InferUnifyError};
use crate::core::types::{EnumVariant, FnParam, FnParamMode, StructField, Type};

#[test]
fn test_unify_var_with_concrete() {
    let mut unifier = InferUnifier::new();
    let var = unifier.new_var();
    unifier
        .unify(&Type::Var(var), &Type::uint(64))
        .expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(var)), Type::uint(64));
}

#[test]
fn test_unify_var_with_var() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let v2 = unifier.new_var();
    unifier
        .unify(&Type::Var(v1), &Type::Var(v2))
        .expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(v1)), Type::Var(v2));
}

#[test]
fn test_unify_occurs_check() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let ty = Type::Array {
        elem_ty: Box::new(Type::Var(v1)),
        dims: vec![1],
    };
    let err = unifier.unify(&Type::Var(v1), &ty).unwrap_err();
    assert!(matches!(err, InferUnifyError::OccursCheckFailed(_, _)));
}

#[test]
fn test_unify_fn_types() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let v2 = unifier.new_var();
    let left = Type::Fn {
        params: vec![FnParam {
            mode: FnParamMode::In,
            ty: Type::Var(v1),
        }],
        ret_ty: Box::new(Type::Var(v2)),
    };
    let right = Type::Fn {
        params: vec![FnParam {
            mode: FnParamMode::In,
            ty: Type::uint(64),
        }],
        ret_ty: Box::new(Type::uint(64)),
    };
    unifier.unify(&left, &right).expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(v1)), Type::uint(64));
    assert_eq!(unifier.apply(&Type::Var(v2)), Type::uint(64));
}

#[test]
fn test_unify_mismatch() {
    let mut unifier = InferUnifier::new();
    let err = unifier.unify(&Type::uint(32), &Type::uint(64)).unwrap_err();
    assert!(matches!(err, InferUnifyError::Mismatch(_, _)));
}

#[test]
fn test_unify_array_matching_dims() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let left = Type::Array {
        elem_ty: Box::new(Type::Var(v1)),
        dims: vec![10],
    };
    let right = Type::Array {
        elem_ty: Box::new(Type::uint(32)),
        dims: vec![10],
    };
    unifier.unify(&left, &right).expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(v1)), Type::uint(32));
}

#[test]
fn test_unify_array_mismatching_dims() {
    let mut unifier = InferUnifier::new();
    let left = Type::Array {
        elem_ty: Box::new(Type::uint(32)),
        dims: vec![10],
    };
    let right = Type::Array {
        elem_ty: Box::new(Type::uint(32)),
        dims: vec![20],
    };
    let err = unifier.unify(&left, &right).unwrap_err();
    assert!(matches!(err, InferUnifyError::Mismatch(_, _)));
}

#[test]
fn test_unify_tuple_matching_arity() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let v2 = unifier.new_var();
    let left = Type::Tuple {
        field_tys: vec![Type::Var(v1), Type::Var(v2)],
    };
    let right = Type::Tuple {
        field_tys: vec![Type::uint(32), Type::uint(64)],
    };
    unifier.unify(&left, &right).expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(v1)), Type::uint(32));
    assert_eq!(unifier.apply(&Type::Var(v2)), Type::uint(64));
}

#[test]
fn test_unify_tuple_mismatching_arity() {
    let mut unifier = InferUnifier::new();
    let left = Type::Tuple {
        field_tys: vec![Type::uint(32)],
    };
    let right = Type::Tuple {
        field_tys: vec![Type::uint(32), Type::uint(64)],
    };
    let err = unifier.unify(&left, &right).unwrap_err();
    assert!(matches!(err, InferUnifyError::Mismatch(_, _)));
}

#[test]
fn test_unify_struct_binds_nested_type_vars() {
    let mut unifier = InferUnifier::new();
    let inner = unifier.new_var();
    let left = Type::Struct {
        name: "MapIter".to_string(),
        fields: vec![
            StructField {
                name: "source".to_string(),
                ty: Type::Struct {
                    name: "CounterIter".to_string(),
                    fields: vec![],
                },
            },
            StructField {
                name: "f".to_string(),
                ty: Type::Fn {
                    params: vec![FnParam {
                        mode: FnParamMode::In,
                        ty: Type::Var(inner),
                    }],
                    ret_ty: Box::new(Type::Var(inner)),
                },
            },
        ],
    };
    let right = Type::Struct {
        name: "MapIter".to_string(),
        fields: vec![
            StructField {
                name: "source".to_string(),
                ty: Type::Struct {
                    name: "CounterIter".to_string(),
                    fields: vec![],
                },
            },
            StructField {
                name: "f".to_string(),
                ty: Type::Fn {
                    params: vec![FnParam {
                        mode: FnParamMode::In,
                        ty: Type::uint(64),
                    }],
                    ret_ty: Box::new(Type::uint(64)),
                },
            },
        ],
    };

    unifier.unify(&left, &right).expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(inner)), Type::uint(64));
}

#[test]
fn test_unify_error_union_binds_payload_vars() {
    let mut unifier = InferUnifier::new();
    let payload = unifier.new_var();
    let left = Type::ErrorUnion {
        ok_ty: Box::new(Type::Var(payload)),
        err_tys: vec![Type::Struct {
            name: "IterDone".to_string(),
            fields: vec![],
        }],
    };
    let right = Type::ErrorUnion {
        ok_ty: Box::new(Type::uint(64)),
        err_tys: vec![Type::Struct {
            name: "IterDone".to_string(),
            fields: vec![],
        }],
    };

    unifier.unify(&left, &right).expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(payload)), Type::uint(64));
}

#[test]
fn test_unify_enum_binds_nested_payload_vars() {
    let mut unifier = InferUnifier::new();
    let payload = unifier.new_var();
    let left = Type::Enum {
        name: "Option".to_string(),
        variants: vec![
            EnumVariant {
                name: "Some".to_string(),
                payload: vec![Type::Var(payload)],
            },
            EnumVariant {
                name: "None".to_string(),
                payload: vec![],
            },
        ],
    };
    let right = Type::Enum {
        name: "Option".to_string(),
        variants: vec![
            EnumVariant {
                name: "Some".to_string(),
                payload: vec![Type::uint(64)],
            },
            EnumVariant {
                name: "None".to_string(),
                payload: vec![],
            },
        ],
    };

    unifier.unify(&left, &right).expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(payload)), Type::uint(64));
}

#[test]
fn test_unify_range_types() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let left = Type::Range {
        elem_ty: Box::new(Type::Var(v1)),
    };
    let right = Type::Range {
        elem_ty: Box::new(Type::sint(32)),
    };
    unifier.unify(&left, &right).expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(v1)), Type::sint(32));
}

#[test]
fn test_unify_slice_types() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let left = Type::Slice {
        elem_ty: Box::new(Type::Var(v1)),
    };
    let right = Type::Slice {
        elem_ty: Box::new(Type::uint(8)),
    };
    unifier.unify(&left, &right).expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(v1)), Type::uint(8));
}

#[test]
fn test_unify_heap_types() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let left = Type::Heap {
        elem_ty: Box::new(Type::Var(v1)),
    };
    let right = Type::Heap {
        elem_ty: Box::new(Type::uint(64)),
    };
    unifier.unify(&left, &right).expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(v1)), Type::uint(64));
}

#[test]
fn test_unify_ref_matching_mutability() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let left = Type::Ref {
        mutable: true,
        elem_ty: Box::new(Type::Var(v1)),
    };
    let right = Type::Ref {
        mutable: true,
        elem_ty: Box::new(Type::uint(32)),
    };
    unifier.unify(&left, &right).expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(v1)), Type::uint(32));
}

#[test]
fn test_unify_ref_mismatching_mutability() {
    let mut unifier = InferUnifier::new();
    let left = Type::Ref {
        mutable: true,
        elem_ty: Box::new(Type::uint(32)),
    };
    let right = Type::Ref {
        mutable: false,
        elem_ty: Box::new(Type::uint(32)),
    };
    let err = unifier.unify(&left, &right).unwrap_err();
    assert!(matches!(err, InferUnifyError::Mismatch(_, _)));
}

#[test]
fn test_unify_fn_mismatching_arity() {
    let mut unifier = InferUnifier::new();
    let left = Type::Fn {
        params: vec![FnParam {
            mode: FnParamMode::In,
            ty: Type::uint(32),
        }],
        ret_ty: Box::new(Type::uint(64)),
    };
    let right = Type::Fn {
        params: vec![
            FnParam {
                mode: FnParamMode::In,
                ty: Type::uint(32),
            },
            FnParam {
                mode: FnParamMode::In,
                ty: Type::uint(32),
            },
        ],
        ret_ty: Box::new(Type::uint(64)),
    };
    let err = unifier.unify(&left, &right).unwrap_err();
    assert!(matches!(err, InferUnifyError::Mismatch(_, _)));
}

#[test]
fn test_unify_fn_mismatching_param_mode() {
    let mut unifier = InferUnifier::new();
    let left = Type::Fn {
        params: vec![FnParam {
            mode: FnParamMode::In,
            ty: Type::uint(32),
        }],
        ret_ty: Box::new(Type::uint(64)),
    };
    let right = Type::Fn {
        params: vec![FnParam {
            mode: FnParamMode::Out,
            ty: Type::uint(32),
        }],
        ret_ty: Box::new(Type::uint(64)),
    };
    let err = unifier.unify(&left, &right).unwrap_err();
    assert!(matches!(err, InferUnifyError::Mismatch(_, _)));
}

#[test]
fn test_unify_transitive() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let v2 = unifier.new_var();
    let v3 = unifier.new_var();

    // Chain: v1 -> v2 -> v3 -> uint(32)
    unifier
        .unify(&Type::Var(v1), &Type::Var(v2))
        .expect("unify should succeed");
    unifier
        .unify(&Type::Var(v2), &Type::Var(v3))
        .expect("unify should succeed");
    unifier
        .unify(&Type::Var(v3), &Type::uint(32))
        .expect("unify should succeed");

    // All should resolve to uint(32)
    assert_eq!(unifier.apply(&Type::Var(v1)), Type::uint(32));
    assert_eq!(unifier.apply(&Type::Var(v2)), Type::uint(32));
    assert_eq!(unifier.apply(&Type::Var(v3)), Type::uint(32));
}

#[test]
fn test_unify_occurs_check_in_tuple() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let ty = Type::Tuple {
        field_tys: vec![Type::uint(32), Type::Var(v1)],
    };
    let err = unifier.unify(&Type::Var(v1), &ty).unwrap_err();
    assert!(matches!(err, InferUnifyError::OccursCheckFailed(_, _)));
}

#[test]
fn test_unify_occurs_check_in_slice() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let ty = Type::Slice {
        elem_ty: Box::new(Type::Var(v1)),
    };
    let err = unifier.unify(&Type::Var(v1), &ty).unwrap_err();
    assert!(matches!(err, InferUnifyError::OccursCheckFailed(_, _)));
}

#[test]
fn test_unify_identical_types() {
    let mut unifier = InferUnifier::new();
    let ty = Type::Tuple {
        field_tys: vec![Type::uint(32), Type::uint(64)],
    };
    unifier
        .unify(&ty, &ty)
        .expect("identical types should unify");
}

#[test]
fn test_unify_nested_structures() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    let left = Type::Slice {
        elem_ty: Box::new(Type::Array {
            elem_ty: Box::new(Type::Var(v1)),
            dims: vec![5],
        }),
    };
    let right = Type::Slice {
        elem_ty: Box::new(Type::Array {
            elem_ty: Box::new(Type::uint(16)),
            dims: vec![5],
        }),
    };
    unifier.unify(&left, &right).expect("unify should succeed");
    assert_eq!(unifier.apply(&Type::Var(v1)), Type::uint(16));
}

#[test]
fn test_unify_var_with_itself() {
    let mut unifier = InferUnifier::new();
    let v1 = unifier.new_var();
    // Unifying a variable with itself should succeed (idempotent)
    unifier
        .unify(&Type::Var(v1), &Type::Var(v1))
        .expect("unify should succeed");
    // Variable should remain unbound
    assert_eq!(unifier.apply(&Type::Var(v1)), Type::Var(v1));
}
