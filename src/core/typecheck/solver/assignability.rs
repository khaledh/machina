//! Assignability solving helpers.
//!
//! Centralizes type-assignability unification and ranking logic used by
//! constraint solving and call overload selection.

use crate::core::typecheck::unify::{TcUnifier, TcUnifyError};
use crate::core::types::{Type, TypeAssignability, array_to_slice_assignable, type_assignable};

pub(super) fn solve_assignable(
    from: &Type,
    to: &Type,
    unifier: &mut TcUnifier,
) -> Result<(), TcUnifyError> {
    let from_raw = super::term_utils::canonicalize_type(from.clone());
    let to_raw = super::term_utils::canonicalize_type(to.clone());
    let from_applied = super::term_utils::canonicalize_type(unifier.apply(&from_raw));
    let to_applied = super::term_utils::canonicalize_type(unifier.apply(&to_raw));

    if let Some(result) = infer_array_to_slice_assignability(&from_applied, &to_applied, unifier) {
        return result;
    }
    if let Some(result) =
        infer_array_to_dyn_array_assignability(&from_applied, &to_applied, unifier)
    {
        return result;
    }
    if let Some(result) =
        infer_dyn_array_to_slice_assignability(&from_applied, &to_applied, unifier)
    {
        return result;
    }

    if !super::term_utils::is_unresolved(&from_applied)
        && !super::term_utils::is_unresolved(&to_applied)
    {
        return match type_assignable(&from_applied, &to_applied) {
            TypeAssignability::Incompatible
                if array_to_slice_assignable(&from_applied, &to_applied) =>
            {
                Ok(())
            }
            TypeAssignability::Incompatible if int_repr_compatible(&from_applied, &to_applied) => {
                Ok(())
            }
            TypeAssignability::Incompatible => {
                Err(TcUnifyError::Mismatch(to_applied, from_applied))
            }
            _ => Ok(()),
        };
    }

    unifier.unify(&erase_refinements(&from_raw), &erase_refinements(&to_raw))
}

pub(super) fn assignability_rank(from: &Type, to: &Type) -> i32 {
    if super::term_utils::is_unresolved(from) || super::term_utils::is_unresolved(to) {
        return 0;
    }
    match type_assignable(from, to) {
        TypeAssignability::Exact => 3,
        TypeAssignability::IntLitToInt { .. } => 2,
        TypeAssignability::RefinedToInt | TypeAssignability::IntToRefined { .. } => 1,
        TypeAssignability::Incompatible => -1000,
    }
}

fn infer_array_to_slice_assignability(
    from: &Type,
    to: &Type,
    unifier: &mut TcUnifier,
) -> Option<Result<(), TcUnifyError>> {
    let Type::Slice {
        elem_ty: slice_elem_ty,
    } = to
    else {
        return None;
    };
    let Some(array_item_ty) = from.array_item_type() else {
        return None;
    };
    Some(unifier.unify(&array_item_ty, slice_elem_ty))
}

fn infer_array_to_dyn_array_assignability(
    from: &Type,
    to: &Type,
    unifier: &mut TcUnifier,
) -> Option<Result<(), TcUnifyError>> {
    let Type::DynArray {
        elem_ty: dyn_elem_ty,
    } = to
    else {
        return None;
    };
    let array_item_ty = from.array_item_type()?;
    Some(unifier.unify(&array_item_ty, dyn_elem_ty))
}

fn infer_dyn_array_to_slice_assignability(
    from: &Type,
    to: &Type,
    unifier: &mut TcUnifier,
) -> Option<Result<(), TcUnifyError>> {
    let Type::DynArray {
        elem_ty: dyn_elem_ty,
    } = from
    else {
        return None;
    };
    let Type::Slice {
        elem_ty: slice_elem_ty,
    } = to
    else {
        return None;
    };
    Some(unifier.unify(dyn_elem_ty, slice_elem_ty))
}

fn int_repr_compatible(from: &Type, to: &Type) -> bool {
    match (from, to) {
        (
            Type::Int {
                signed: from_signed,
                bits: from_bits,
                ..
            },
            Type::Int {
                signed: to_signed,
                bits: to_bits,
                ..
            },
        ) => from_signed == to_signed && from_bits == to_bits,
        _ => false,
    }
}

fn erase_refinements(ty: &Type) -> Type {
    ty.map_cloned(&|t| match t {
        Type::Int { signed, bits, .. } => Type::Int {
            signed,
            bits,
            bounds: None,
            nonzero: false,
        },
        other => other,
    })
}
