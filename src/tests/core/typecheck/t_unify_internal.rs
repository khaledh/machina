use super::*;
use crate::core::resolve::DefId;
use crate::core::types::Type;

#[test]
fn test_unify_binds_infer_var() {
    let mut vars = TypeVarStore::default();
    let infer = vars.fresh_infer_local();
    let mut unifier = TcUnifier::new(vars);

    unifier.unify(&Type::Var(infer), &Type::uint(64)).unwrap();
    assert_eq!(unifier.apply(&Type::Var(infer)), Type::uint(64));
}

#[test]
fn test_unify_rejects_rigid_binding() {
    let mut vars = TypeVarStore::default();
    let rigid = TyVarId::new(0);
    vars.register_rigid_param(rigid, DefId(1));
    let mut unifier = TcUnifier::new(vars);

    let err = unifier
        .unify(&Type::Var(rigid), &Type::uint(64))
        .expect_err("expected rigid var error");
    assert!(matches!(err, TcUnifyError::CannotBindRigid(_, _)));
}

#[test]
fn test_unify_occurs_check() {
    let mut vars = TypeVarStore::default();
    let infer = vars.fresh_infer_local();
    let mut unifier = TcUnifier::new(vars);

    let recursive = Type::Array {
        elem_ty: Box::new(Type::Var(infer)),
        dims: vec![1],
    };
    let err = unifier
        .unify(&Type::Var(infer), &recursive)
        .expect_err("expected occurs-check failure");
    assert!(matches!(err, TcUnifyError::OccursCheckFailed(_, _)));
}
