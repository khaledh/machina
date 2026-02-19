//! Control-flow obligation solving.
//!
//! Handles obligations that reason about control-sensitive typing behavior:
//! join type compatibility, `?` propagation, and `for` iteration typing.

use std::collections::{HashMap, HashSet};

use crate::core::resolve::DefId;
use crate::core::tree::NodeId;
use crate::core::typecheck::constraints::ExprObligation;
use crate::core::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::core::typecheck::unify::TcUnifier;
use crate::core::types::{Type, TypeAssignability, type_assignable};

fn try_error_variant_accepted(err_ty: &Type, return_err_ty: &Type) -> bool {
    if !matches!(
        type_assignable(err_ty, return_err_ty),
        TypeAssignability::Incompatible
    ) {
        return true;
    }

    // Allow `?` to propagate into wrapper enum errors (for example,
    // `() | AppError` where `AppError = Io(IoError) | Parse(ParseError)`).
    let Type::Enum { variants, .. } = return_err_ty else {
        return false;
    };

    variants.iter().any(|variant| {
        variant.payload.len() == 1
            && !matches!(
                type_assignable(err_ty, &variant.payload[0]),
                TypeAssignability::Incompatible
            )
    })
}

pub(super) fn try_check_expr_obligation_control(
    obligation: &ExprObligation,
    def_terms: &HashMap<DefId, Type>,
    unifier: &mut TcUnifier,
    errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) -> bool {
    match obligation {
        ExprObligation::Join {
            expr_id,
            arms,
            result,
            span,
        } => {
            let resolved_arms = arms
                .iter()
                .map(|arm| super::term_utils::resolve_term(arm, unifier))
                .collect::<Vec<_>>();
            let mut result_ty = super::term_utils::resolve_term(result, unifier);

            if super::term_utils::is_unresolved(&result_ty)
                && let Some(inferred_join) = super::joins::infer_join_type_from_arms(
                    &resolved_arms
                        .iter()
                        .cloned()
                        .map(|ty| {
                            super::term_utils::default_infer_ints_for_diagnostics(
                                ty,
                                unifier.vars(),
                            )
                        })
                        .collect::<Vec<_>>(),
                )
            {
                let _ = unifier.unify(result, &inferred_join);
                result_ty = super::term_utils::resolve_term(result, unifier);
            }

            let result_ty_diag = super::term_utils::default_infer_ints_for_diagnostics(
                result_ty.clone(),
                unifier.vars(),
            );
            if super::term_utils::is_unresolved(&result_ty_diag) {
                return true;
            }

            for (arm_term, arm_ty) in arms.iter().zip(resolved_arms.into_iter()) {
                if super::term_utils::is_unresolved(&arm_ty) {
                    let _ = unifier.unify(arm_term, &result_ty_diag);
                    continue;
                }
                let arm_ty_diag =
                    super::term_utils::default_infer_ints_for_diagnostics(arm_ty, unifier.vars());
                if matches!(
                    type_assignable(&arm_ty_diag, &result_ty_diag),
                    TypeAssignability::Incompatible
                ) {
                    if let Some(variants) =
                        super::diag_utils::error_union_variant_names(&result_ty_diag)
                    {
                        errors.push(
                            TypeCheckErrorKind::JoinArmNotInErrorUnion(variants, arm_ty_diag)
                                .at(*span)
                                .into(),
                        );
                    } else {
                        errors.push(
                            TypeCheckErrorKind::JoinArmTypeMismatch(
                                result_ty_diag.clone(),
                                arm_ty_diag,
                            )
                            .at(*span)
                            .into(),
                        );
                    }
                    covered_exprs.insert(*expr_id);
                    break;
                }
            }
            true
        }
        ExprObligation::Try {
            expr_id,
            operand,
            result,
            expected_return_ty,
            callable_def_id,
            span,
        } => {
            let operand_ty = super::term_utils::resolve_term(operand, unifier);
            let operand_ty_for_diag = super::term_utils::default_infer_ints_for_diagnostics(
                operand_ty.clone(),
                unifier.vars(),
            );
            let Type::ErrorUnion { ok_ty, err_tys } = &operand_ty else {
                if !super::term_utils::is_unresolved(&operand_ty_for_diag) {
                    errors.push(
                        TypeCheckErrorKind::TryOperandNotErrorUnion(operand_ty_for_diag)
                            .at(*span)
                            .into(),
                    );
                    covered_exprs.insert(*expr_id);
                }
                return true;
            };

            let _ = unifier.unify(result, ok_ty);

            let mut return_ty = expected_return_ty
                .as_ref()
                .map(|term| super::term_utils::resolve_term(term, unifier))
                .unwrap_or(Type::Unknown);

            if super::term_utils::is_unresolved(&return_ty)
                && let Some(def_id) = callable_def_id
                && let Some(callable_term) = def_terms.get(def_id)
                && let Type::Fn { ret_ty, .. } =
                    super::term_utils::resolve_term(callable_term, unifier)
            {
                return_ty = *ret_ty;
            }

            if super::term_utils::is_unresolved(&return_ty)
                && let Some(return_ty_term) = expected_return_ty
            {
                let fresh_ok = Type::Var(unifier.vars_mut().fresh_infer_local());
                let expected_union = Type::ErrorUnion {
                    ok_ty: Box::new(fresh_ok),
                    err_tys: err_tys.clone(),
                };
                let _ = unifier.unify(return_ty_term, &expected_union);
                return_ty = super::term_utils::resolve_term(return_ty_term, unifier);
            }

            if expected_return_ty.is_none() && callable_def_id.is_none() {
                errors.push(TypeCheckErrorKind::TryOutsideFunction.at(*span).into());
                covered_exprs.insert(*expr_id);
                return true;
            }

            match &return_ty {
                Type::ErrorUnion {
                    err_tys: return_err_tys,
                    ..
                } => {
                    let mut missing = Vec::new();
                    for err_ty in err_tys {
                        let present = return_err_tys
                            .iter()
                            .any(|return_err_ty| try_error_variant_accepted(err_ty, return_err_ty));
                        if !present
                            && !super::term_utils::is_unresolved(err_ty)
                            && !missing.iter().any(|seen| seen == err_ty)
                        {
                            missing.push(err_ty.clone());
                        }
                    }
                    if !missing.is_empty() {
                        let mut missing_names = missing
                            .iter()
                            .map(super::diag_utils::compact_type_name)
                            .collect::<Vec<_>>();
                        missing_names.sort();
                        missing_names.dedup();
                        let mut return_variant_names = std::iter::once(ok_ty.as_ref())
                            .chain(return_err_tys.iter())
                            .map(super::diag_utils::compact_type_name)
                            .collect::<Vec<_>>();
                        return_variant_names.sort();
                        return_variant_names.dedup();
                        errors.push(
                            TypeCheckErrorKind::TryErrorNotInReturn(
                                missing_names,
                                return_variant_names,
                            )
                            .at(*span)
                            .into(),
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
                ty if super::term_utils::is_unresolved(ty) => {}
                _ => {
                    errors.push(
                        TypeCheckErrorKind::TryReturnTypeNotErrorUnion(return_ty.clone())
                            .at(*span)
                            .into(),
                    );
                    covered_exprs.insert(*expr_id);
                }
            }
            true
        }
        ExprObligation::ForIter {
            stmt_id,
            iter,
            pattern,
            span,
        } => {
            let iter_ty = super::term_utils::resolve_term(iter, unifier);
            let iter_ty_for_diag = super::term_utils::default_infer_ints_for_diagnostics(
                iter_ty.clone(),
                unifier.vars(),
            );
            if !super::is_iterable(&iter_ty_for_diag)
                && !super::term_utils::is_unresolved(&iter_ty_for_diag)
            {
                errors.push(
                    TypeCheckErrorKind::ForIterNotIterable(iter_ty_for_diag)
                        .at(*span)
                        .into(),
                );
                covered_exprs.insert(*stmt_id);
                return true;
            }

            if let Some(elem_ty) = super::iterable_elem_type(&iter_ty) {
                let _ = unifier.unify(pattern, &elem_ty);
                let pattern_ty = super::term_utils::resolve_term(pattern, unifier);
                if matches!(
                    type_assignable(&pattern_ty, &elem_ty),
                    TypeAssignability::Incompatible
                ) && !super::term_utils::is_unresolved(&pattern_ty)
                    && !super::term_utils::is_unresolved(&elem_ty)
                {
                    errors.push(
                        TypeCheckErrorKind::DeclTypeMismatch(pattern_ty, elem_ty)
                            .at(*span)
                            .into(),
                    );
                    covered_exprs.insert(*stmt_id);
                }
            }
            true
        }
        _ => false,
    }
}
