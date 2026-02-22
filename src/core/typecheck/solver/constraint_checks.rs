//! Constraint solving and deferred constraint diagnostics.
//!
//! This module keeps constraint-application mechanics separate from the main
//! solver orchestration loop.

use crate::core::diag::Span;
use crate::core::tree::NodeId;
use crate::core::typecheck::constraints::{Constraint, ConstraintReason};
use crate::core::typecheck::errors::{TEK, TypeCheckError};
use crate::core::typecheck::unify::{TcUnifier, TcUnifyError};
use crate::core::types::Type;

pub(super) fn apply_assignable_inference(constraint: &Constraint, unifier: &mut TcUnifier) {
    if let Constraint::Assignable { from, to, .. } = constraint {
        let _ = super::assignability::solve_assignable(from, to, unifier);
    }
}

pub(super) fn apply_constraint(
    constraint: &Constraint,
    unifier: &mut TcUnifier,
    failed_constraints: &mut usize,
    deferred_expr_errors: &mut Vec<(NodeId, TcUnifyError, Span)>,
    deferred_pattern_errors: &mut Vec<(NodeId, TcUnifyError, Span)>,
    non_expr_errors: &mut Vec<TypeCheckError>,
) {
    let reason = constraint_reason(constraint);
    let result = match constraint {
        Constraint::Eq { left, right, .. } => unifier.unify(
            &super::term_utils::canonicalize_type(left.clone()),
            &super::term_utils::canonicalize_type(right.clone()),
        ),
        Constraint::Assignable { from, to, .. } => {
            super::assignability::solve_assignable(from, to, unifier)
        }
    };
    if let Err(err) = result {
        *failed_constraints += 1;
        match reason_subject_id(constraint) {
            ConstraintSubject::Expr(expr_id) => {
                deferred_expr_errors.push((expr_id, err, reason_span(constraint)));
            }
            ConstraintSubject::Pattern(pattern_id) => {
                deferred_pattern_errors.push((pattern_id, err, reason_span(constraint)));
            }
            ConstraintSubject::Other => {
                non_expr_errors.push(unify_error_to_diag_with_reason(err, reason, constraint));
            }
        }
    }
}

pub(super) fn remap_index_unify_error(err: &TcUnifyError, span: Span) -> Option<TypeCheckError> {
    match err {
        TcUnifyError::Mismatch(left, right) => {
            if !super::term_utils::is_int_like(left) && !super::term_utils::is_unresolved(left) {
                return Some(TEK::IndexTypeNotInt(left.clone()).at(span));
            }
            if !super::term_utils::is_int_like(right) && !super::term_utils::is_unresolved(right) {
                return Some(TEK::IndexTypeNotInt(right.clone()).at(span));
            }
            None
        }
        TcUnifyError::CannotBindRigid(_, found)
            if !super::term_utils::is_int_like(found)
                && !super::term_utils::is_unresolved(found) =>
        {
            Some(TEK::IndexTypeNotInt(found.clone()).at(span))
        }
        _ => None,
    }
}

pub(super) fn remap_enum_payload_unify_error(
    err: &TcUnifyError,
    variant: &str,
    index: usize,
    span: Span,
) -> Option<TypeCheckError> {
    match err {
        TcUnifyError::Mismatch(expected, found)
            if !super::term_utils::is_unresolved(expected)
                && !super::term_utils::is_unresolved(found) =>
        {
            Some(
                TEK::EnumVariantPayloadTypeMismatch(
                    variant.to_string(),
                    index,
                    expected.clone(),
                    found.clone(),
                )
                .at(span),
            )
        }
        _ => None,
    }
}

pub(super) fn unify_error_to_diag(err: TcUnifyError, span: Span) -> TypeCheckError {
    match err {
        TcUnifyError::Mismatch(expected, found) => TEK::DeclTypeMismatch(expected, found).at(span),
        TcUnifyError::CannotBindRigid(var, found) => {
            TEK::DeclTypeMismatch(Type::Var(var), found).at(span)
        }
        TcUnifyError::OccursCheckFailed(_, _) => TEK::UnknownType.at(span),
    }
}

fn unify_error_to_diag_with_reason(
    err: TcUnifyError,
    reason: &ConstraintReason,
    constraint: &Constraint,
) -> TypeCheckError {
    match reason {
        ConstraintReason::Return(_, span) => return_unify_error_to_diag(err, *span),
        _ => unify_error_to_diag(err, reason_span(constraint)),
    }
}

fn return_unify_error_to_diag(err: TcUnifyError, span: Span) -> TypeCheckError {
    match err {
        TcUnifyError::Mismatch(expected, found) => {
            if let Type::ErrorUnion { ok_ty, err_tys } = &expected {
                let mut variants = std::iter::once(ok_ty.as_ref())
                    .chain(err_tys.iter())
                    .map(super::diag_utils::compact_type_name)
                    .collect::<Vec<_>>();
                variants.sort();
                variants.dedup();
                return TEK::ReturnNotInErrorUnion(variants, found).at(span);
            }
            TEK::ReturnTypeMismatch(expected, found).at(span)
        }
        TcUnifyError::CannotBindRigid(var, found) => {
            TEK::ReturnTypeMismatch(Type::Var(var), found).at(span)
        }
        TcUnifyError::OccursCheckFailed(_, _) => TEK::UnknownType.at(span),
    }
}

fn reason_span(constraint: &Constraint) -> Span {
    let reason = constraint_reason(constraint);
    match reason {
        ConstraintReason::Expr(_, span)
        | ConstraintReason::Stmt(_, span)
        | ConstraintReason::Pattern(_, span)
        | ConstraintReason::Decl(_, span)
        | ConstraintReason::Return(_, span) => *span,
    }
}

fn constraint_reason(constraint: &Constraint) -> &ConstraintReason {
    match constraint {
        Constraint::Eq { reason, .. } => reason,
        Constraint::Assignable { reason, .. } => reason,
    }
}

enum ConstraintSubject {
    Expr(NodeId),
    Pattern(NodeId),
    Other,
}

fn reason_subject_id(constraint: &Constraint) -> ConstraintSubject {
    let reason = constraint_reason(constraint);
    match reason {
        ConstraintReason::Expr(expr_id, _) => ConstraintSubject::Expr(*expr_id),
        ConstraintReason::Pattern(pattern_id, _) => ConstraintSubject::Pattern(*pattern_id),
        _ => ConstraintSubject::Other,
    }
}
