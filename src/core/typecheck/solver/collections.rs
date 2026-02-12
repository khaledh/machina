//! Collection-specific obligation solving.
//!
//! This module owns set/map hashability checks emitted by constraint
//! collection, keeping the main solver loop focused on control flow.

use std::collections::HashSet;

use crate::core::tree::NodeId;
use crate::core::typecheck::constraints::ExprObligation;
use crate::core::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::core::typecheck::unify::TcUnifier;

pub(super) fn try_check_expr_obligation_collections(
    obligation: &ExprObligation,
    unifier: &mut TcUnifier,
    errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) -> bool {
    match obligation {
        ExprObligation::SetElemType {
            expr_id,
            elem_ty,
            span,
        } => {
            let elem_ty = super::term_utils::resolve_term(elem_ty, unifier);
            let elem_ty_for_diag = super::term_utils::default_infer_ints_for_diagnostics(
                elem_ty.clone(),
                unifier.vars(),
            );
            if super::term_utils::is_unresolved(&elem_ty_for_diag) {
                return true;
            }
            if let Err(failure) = super::ensure_hashable(&elem_ty_for_diag) {
                errors.push(
                    TypeCheckErrorKind::TypeNotHashable(
                        elem_ty_for_diag,
                        failure.path,
                        failure.failing_ty,
                        *span,
                    )
                    .into(),
                );
                covered_exprs.insert(*expr_id);
            }
            true
        }
        ExprObligation::MapKeyType {
            expr_id,
            key_ty,
            span,
        } => {
            let key_ty = super::term_utils::resolve_term(key_ty, unifier);
            let key_ty_for_diag = super::term_utils::default_infer_ints_for_diagnostics(
                key_ty.clone(),
                unifier.vars(),
            );
            if super::term_utils::is_unresolved(&key_ty_for_diag) {
                return true;
            }
            if let Err(failure) = super::ensure_hashable(&key_ty_for_diag) {
                errors.push(
                    TypeCheckErrorKind::TypeNotHashable(
                        key_ty_for_diag,
                        failure.path,
                        failure.failing_ty,
                        *span,
                    )
                    .into(),
                );
                covered_exprs.insert(*expr_id);
            }
            true
        }
        _ => false,
    }
}
