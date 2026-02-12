//! Shared term/type normalization helpers for solver modules.

use crate::core::typecheck::typesys::TypeVarStore;
use crate::core::typecheck::unify::TcUnifier;
use crate::core::types::Type;

pub(super) fn resolve_term_for_diagnostics(ty: &Type, unifier: &TcUnifier) -> Type {
    default_infer_ints_for_diagnostics(resolve_term(ty, unifier), unifier.vars())
}

pub(super) fn resolve_term(ty: &Type, unifier: &TcUnifier) -> Type {
    canonicalize_type(unifier.apply(ty))
}

pub(super) fn default_infer_ints_for_diagnostics(ty: Type, vars: &TypeVarStore) -> Type {
    ty.map(&|t| match t {
        Type::Var(var)
            if matches!(
                vars.kind(var),
                Some(crate::core::typecheck::typesys::TypeVarKind::InferInt)
            ) =>
        {
            Type::sint(32)
        }
        other => other,
    })
}

pub(super) fn is_int_like(ty: &Type) -> bool {
    matches!(ty, Type::Int { .. })
}

pub(super) fn is_unresolved(ty: &Type) -> bool {
    ty.any(&|t| matches!(t, Type::Unknown | Type::Var(_)))
}

pub(super) fn peel_heap(ty: Type) -> Type {
    ty.peel_heap()
}

pub(super) fn canonicalize_type(ty: Type) -> Type {
    ty.map(&|t| match t {
        // Flatten nested arrays.
        Type::Array { elem_ty, dims } => match *elem_ty {
            Type::Array {
                elem_ty: inner_elem,
                dims: inner_dims,
            } => {
                let mut merged = dims;
                merged.extend(inner_dims);
                Type::Array {
                    elem_ty: inner_elem,
                    dims: merged,
                }
            }
            other => Type::Array {
                elem_ty: Box::new(other),
                dims,
            },
        },
        other => other,
    })
}
