//! Index/slice/range obligation solving.
//!
//! Keeping these checks in a focused module makes the main solver loop easier
//! to scan while preserving exactly the same obligation behavior.

use std::collections::HashSet;

use crate::core::tree::NodeId;
use crate::core::typecheck::constraints::ExprObligation;
use crate::core::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::core::typecheck::typesys::TypeVarKind;
use crate::core::typecheck::unify::TcUnifier;
use crate::core::types::Type;

pub(super) fn try_check_expr_obligation_index(
    obligation: &ExprObligation,
    unifier: &mut TcUnifier,
    errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) -> bool {
    match obligation {
        ExprObligation::ArrayIndex {
            expr_id,
            target,
            indices,
            index_nodes,
            index_spans,
            result,
            span,
        } => {
            let target_ty = super::term_utils::resolve_term(target, unifier);
            let indexed_target_ty = super::term_utils::peel_heap(target_ty.clone());

            match &indexed_target_ty {
                Type::Array { elem_ty, dims } => {
                    if let Some((idx_i, bad_idx_ty)) = indices
                        .iter()
                        .map(|term| super::term_utils::resolve_term(term, unifier))
                        .enumerate()
                        .find(|(_, ty)| {
                            !super::term_utils::is_int_like(ty)
                                && !super::term_utils::is_unresolved(ty)
                        })
                    {
                        emit_bad_int_index(
                            errors,
                            covered_exprs,
                            *expr_id,
                            index_nodes,
                            index_spans,
                            *span,
                            idx_i,
                            bad_idx_ty,
                        );
                        return true;
                    }
                    for index_term in indices {
                        force_unresolved_index_to_u64(index_term, unifier);
                    }
                    if indices.len() > dims.len() {
                        errors.push(
                            TypeCheckErrorKind::TooManyIndices(dims.len(), indices.len())
                                .at(*span)
                                .into(),
                        );
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    let result_ty = if indices.len() == dims.len() {
                        (**elem_ty).clone()
                    } else {
                        Type::Array {
                            elem_ty: elem_ty.clone(),
                            dims: dims[indices.len()..].to_vec(),
                        }
                    };
                    let _ = unifier.unify(result, &result_ty);
                }
                Type::Slice { elem_ty } => {
                    if let Some((idx_i, bad_idx_ty)) = indices
                        .iter()
                        .map(|term| super::term_utils::resolve_term(term, unifier))
                        .enumerate()
                        .find(|(_, ty)| {
                            !super::term_utils::is_int_like(ty)
                                && !super::term_utils::is_unresolved(ty)
                        })
                    {
                        emit_bad_int_index(
                            errors,
                            covered_exprs,
                            *expr_id,
                            index_nodes,
                            index_spans,
                            *span,
                            idx_i,
                            bad_idx_ty,
                        );
                        return true;
                    }
                    for index_term in indices {
                        force_unresolved_index_to_u64(index_term, unifier);
                    }
                    if indices.len() != 1 {
                        errors.push(
                            TypeCheckErrorKind::TooManyIndices(1, indices.len())
                                .at(*span)
                                .into(),
                        );
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    let _ = unifier.unify(result, elem_ty);
                }
                Type::DynArray { elem_ty } => {
                    if let Some((idx_i, bad_idx_ty)) = indices
                        .iter()
                        .map(|term| super::term_utils::resolve_term(term, unifier))
                        .enumerate()
                        .find(|(_, ty)| {
                            !super::term_utils::is_int_like(ty)
                                && !super::term_utils::is_unresolved(ty)
                        })
                    {
                        emit_bad_int_index(
                            errors,
                            covered_exprs,
                            *expr_id,
                            index_nodes,
                            index_spans,
                            *span,
                            idx_i,
                            bad_idx_ty,
                        );
                        return true;
                    }
                    for index_term in indices {
                        force_unresolved_index_to_u64(index_term, unifier);
                    }
                    if indices.len() != 1 {
                        errors.push(
                            TypeCheckErrorKind::TooManyIndices(1, indices.len())
                                .at(*span)
                                .into(),
                        );
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    let _ = unifier.unify(result, elem_ty);
                }
                Type::String => {
                    if let Some((idx_i, bad_idx_ty)) = indices
                        .iter()
                        .map(|term| super::term_utils::resolve_term(term, unifier))
                        .enumerate()
                        .find(|(_, ty)| {
                            !super::term_utils::is_int_like(ty)
                                && !super::term_utils::is_unresolved(ty)
                        })
                    {
                        emit_bad_int_index(
                            errors,
                            covered_exprs,
                            *expr_id,
                            index_nodes,
                            index_spans,
                            *span,
                            idx_i,
                            bad_idx_ty,
                        );
                        return true;
                    }
                    for index_term in indices {
                        force_unresolved_index_to_u64(index_term, unifier);
                    }
                    if indices.len() != 1 {
                        errors.push(
                            TypeCheckErrorKind::TooManyIndices(1, indices.len())
                                .at(*span)
                                .into(),
                        );
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    let _ = unifier.unify(result, &Type::uint(8));
                }
                Type::Map { key_ty, value_ty } => {
                    if indices.len() != 1 {
                        errors.push(
                            TypeCheckErrorKind::TooManyIndices(1, indices.len())
                                .at(*span)
                                .into(),
                        );
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    let key_index_ty = super::term_utils::resolve_term(&indices[0], unifier);
                    if let Err(_) =
                        super::assignability::solve_assignable(&key_index_ty, key_ty, unifier)
                    {
                        if !super::term_utils::is_unresolved(&key_index_ty) {
                            let diag_span = index_spans.first().copied().unwrap_or(*span);
                            errors.push(
                                TypeCheckErrorKind::MapKeyTypeMismatch(
                                    key_ty.as_ref().clone(),
                                    key_index_ty,
                                )
                                .at(diag_span)
                                .into(),
                            );
                            covered_exprs.insert(*expr_id);
                            if let Some(node_id) = index_nodes.first() {
                                covered_exprs.insert(*node_id);
                            }
                        }
                        return true;
                    }
                    if !super::term_utils::is_unresolved(key_ty)
                        && let Err(failure) = super::ensure_hashable(key_ty)
                    {
                        errors.push(
                            TypeCheckErrorKind::TypeNotHashable(
                                key_ty.as_ref().clone(),
                                failure.path,
                                failure.failing_ty,
                            )
                            .at(*span)
                            .into(),
                        );
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    if value_ty.needs_drop() && !super::term_utils::is_unresolved(value_ty) {
                        errors.push(
                            TypeCheckErrorKind::MapIndexValueNotCopySafe(value_ty.as_ref().clone())
                                .at(*span)
                                .into(),
                        );
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    let result_ty = Type::ErrorUnion {
                        ok_ty: value_ty.clone(),
                        err_tys: vec![super::diag_utils::map_key_not_found_type()],
                    };
                    let _ = unifier.unify(result, &result_ty);
                }
                ty if super::term_utils::is_unresolved(ty) => {}
                _ => {
                    errors.push(
                        TypeCheckErrorKind::InvalidIndexTargetType(indexed_target_ty)
                            .at(*span)
                            .into(),
                    );
                    covered_exprs.insert(*expr_id);
                    return true;
                }
            }
            true
        }
        ExprObligation::MapIndexAssign {
            stmt_id,
            target,
            span,
        } => {
            let owner_ty =
                super::term_utils::peel_heap(super::term_utils::resolve_term(target, unifier));
            if matches!(owner_ty, Type::Map { .. }) {
                errors.push(
                    TypeCheckErrorKind::MapIndexAssignUnsupported
                        .at(*span)
                        .into(),
                );
                covered_exprs.insert(*stmt_id);
            }
            true
        }
        ExprObligation::Slice {
            expr_id,
            target,
            start,
            end,
            result,
            span,
        } => {
            let target_ty = super::term_utils::resolve_term(target, unifier);
            let sliced_target_ty = super::term_utils::peel_heap(target_ty.clone());
            let mut bad_bound_ty = None;
            if let Some(start_ty) = start
                .as_ref()
                .map(|term| super::term_utils::resolve_term(term, unifier))
                && !super::term_utils::is_int_like(&start_ty)
                && !super::term_utils::is_unresolved(&start_ty)
            {
                bad_bound_ty = Some(start_ty);
            }
            if bad_bound_ty.is_none()
                && let Some(end_ty) = end
                    .as_ref()
                    .map(|term| super::term_utils::resolve_term(term, unifier))
                && !super::term_utils::is_int_like(&end_ty)
                && !super::term_utils::is_unresolved(&end_ty)
            {
                bad_bound_ty = Some(end_ty);
            }
            if let Some(bound_ty) = bad_bound_ty {
                errors.push(
                    TypeCheckErrorKind::IndexTypeNotInt(bound_ty)
                        .at(*span)
                        .into(),
                );
                covered_exprs.insert(*expr_id);
                return true;
            }

            match &sliced_target_ty {
                Type::Array { elem_ty, dims } => {
                    if dims.is_empty() {
                        errors.push(
                            TypeCheckErrorKind::SliceTargetZeroDimArray(sliced_target_ty)
                                .at(*span)
                                .into(),
                        );
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    let slice_elem_ty = if dims.len() == 1 {
                        (**elem_ty).clone()
                    } else {
                        Type::Array {
                            elem_ty: elem_ty.clone(),
                            dims: dims[1..].to_vec(),
                        }
                    };
                    let _ = unifier.unify(
                        result,
                        &Type::Slice {
                            elem_ty: Box::new(slice_elem_ty),
                        },
                    );
                }
                Type::Slice { elem_ty } => {
                    let _ = unifier.unify(
                        result,
                        &Type::Slice {
                            elem_ty: elem_ty.clone(),
                        },
                    );
                }
                Type::DynArray { elem_ty } => {
                    let _ = unifier.unify(
                        result,
                        &Type::Slice {
                            elem_ty: elem_ty.clone(),
                        },
                    );
                }
                Type::String => {
                    let _ = unifier.unify(
                        result,
                        &Type::Slice {
                            elem_ty: Box::new(Type::uint(8)),
                        },
                    );
                }
                ty if super::term_utils::is_unresolved(ty) => {}
                _ => {
                    errors.push(
                        TypeCheckErrorKind::SliceTargetNotArrayOrString(sliced_target_ty)
                            .at(*span)
                            .into(),
                    );
                    covered_exprs.insert(*expr_id);
                    return true;
                }
            }
            true
        }
        ExprObligation::Range {
            expr_id,
            start,
            end,
            result,
            span,
        } => {
            let start_ty = super::term_utils::resolve_term(start, unifier);
            if !super::term_utils::is_int_like(&start_ty)
                && !super::term_utils::is_unresolved(&start_ty)
            {
                errors.push(
                    TypeCheckErrorKind::IndexTypeNotInt(start_ty)
                        .at(*span)
                        .into(),
                );
                covered_exprs.insert(*expr_id);
                return true;
            }
            let end_ty = super::term_utils::resolve_term(end, unifier);
            if !super::term_utils::is_int_like(&end_ty)
                && !super::term_utils::is_unresolved(&end_ty)
            {
                errors.push(TypeCheckErrorKind::IndexTypeNotInt(end_ty).at(*span).into());
                covered_exprs.insert(*expr_id);
                return true;
            }
            let _ = unifier.unify(
                result,
                &Type::Range {
                    elem_ty: Box::new(Type::uint(64)),
                },
            );
            true
        }
        _ => false,
    }
}

fn force_unresolved_index_to_u64(index_term: &Type, unifier: &mut TcUnifier) {
    let resolved = super::term_utils::resolve_term(index_term, unifier);
    if let Type::Var(var) = resolved
        && matches!(unifier.vars().kind(var), Some(TypeVarKind::InferInt))
    {
        let _ = unifier.unify(index_term, &Type::uint(64));
    }
}

fn emit_bad_int_index(
    errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
    expr_id: NodeId,
    index_nodes: &[NodeId],
    index_spans: &[crate::core::diag::Span],
    fallback_span: crate::core::diag::Span,
    idx_i: usize,
    bad_idx_ty: Type,
) {
    let diag_span = index_spans.get(idx_i).copied().unwrap_or(fallback_span);
    errors.push(
        TypeCheckErrorKind::IndexTypeNotInt(bad_idx_ty)
            .at(diag_span)
            .into(),
    );
    covered_exprs.insert(expr_id);
    if let Some(node_id) = index_nodes.get(idx_i) {
        covered_exprs.insert(*node_id);
    }
}
