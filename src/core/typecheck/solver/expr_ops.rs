//! Expression operator obligation solving.
//!
//! Handles diagnostics and type expectations for binary/unary expression
//! obligations (arithmetic, comparisons, logical operators).

use std::collections::HashSet;

use crate::core::ast::{BinaryOp, NodeId, UnaryOp};
use crate::core::diag::Span;
use crate::core::typecheck::capability::ensure_equatable;
use crate::core::typecheck::constraints::ExprObligation;
use crate::core::typecheck::errors::{TEK, TypeCheckError};
use crate::core::typecheck::solver::term_utils;
use crate::core::typecheck::unify::TcUnifier;
use crate::core::types::Type;

pub(super) fn try_check_expr_obligation_ops(
    obligation: &ExprObligation,
    unifier: &mut TcUnifier,
    errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) -> bool {
    match obligation {
        ExprObligation::BinOp {
            expr_id,
            op,
            left,
            right,
            result,
            ..
        } => {
            match op {
                BinaryOp::Add | BinaryOp::Sub => {
                    if let Some(err) = solve_add_sub_obligation(*op, left, right, result, unifier) {
                        errors.push(err.at(op_span(obligation)));
                        covered_exprs.insert(*expr_id);
                    }
                }
                BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::BitOr
                | BinaryOp::BitXor
                | BinaryOp::BitAnd
                | BinaryOp::Shl
                | BinaryOp::Shr => {
                    let left_ty = term_utils::resolve_term_for_diagnostics(left, unifier);
                    let right_ty = term_utils::resolve_term_for_diagnostics(right, unifier);
                    if let Some(err) =
                        first_non_int_operand(&left_ty, &right_ty, op_span(obligation))
                    {
                        errors.push(err);
                        covered_exprs.insert(*expr_id);
                    }
                }
                BinaryOp::Eq | BinaryOp::Ne => {
                    let left_ty = term_utils::resolve_term_for_diagnostics(left, unifier);
                    let right_ty = term_utils::resolve_term_for_diagnostics(right, unifier);
                    if let Some(err) =
                        first_non_equatable_cmp_operand(&left_ty, &right_ty, op_span(obligation))
                    {
                        errors.push(err);
                        covered_exprs.insert(*expr_id);
                    }
                }
                BinaryOp::Lt | BinaryOp::Gt | BinaryOp::LtEq | BinaryOp::GtEq => {
                    let left_ty = term_utils::resolve_term_for_diagnostics(left, unifier);
                    let right_ty = term_utils::resolve_term_for_diagnostics(right, unifier);
                    if let Some(err) =
                        first_non_int_cmp_operand(&left_ty, &right_ty, op_span(obligation))
                    {
                        errors.push(err);
                        covered_exprs.insert(*expr_id);
                    }
                }
                BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                    let left_ty = term_utils::resolve_term_for_diagnostics(left, unifier);
                    let right_ty = term_utils::resolve_term_for_diagnostics(right, unifier);
                    if let Some(err) =
                        first_non_bool_operand(&left_ty, &right_ty, op_span(obligation))
                    {
                        errors.push(err);
                        covered_exprs.insert(*expr_id);
                    }
                }
            }
            true
        }
        ExprObligation::UnaryOp {
            expr_id,
            op,
            operand,
            span,
            ..
        } => {
            let operand_ty = term_utils::resolve_term_for_diagnostics(operand, unifier);
            match op {
                UnaryOp::Neg | UnaryOp::BitNot => {
                    if !term_utils::is_int_like(&operand_ty)
                        && !term_utils::is_unresolved(&operand_ty)
                    {
                        crate::core::typecheck::tc_push_error!(
                            errors,
                            *span,
                            TEK::NegationOperandNotInt(operand_ty)
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
                UnaryOp::LogicalNot => {
                    if operand_ty != Type::Bool && !term_utils::is_unresolved(&operand_ty) {
                        crate::core::typecheck::tc_push_error!(
                            errors,
                            *span,
                            TEK::LogicalOperandNotBoolean(operand_ty)
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
            }
            true
        }
        _ => false,
    }
}

fn solve_add_sub_obligation(
    op: BinaryOp,
    left: &Type,
    right: &Type,
    result: &Type,
    unifier: &mut TcUnifier,
) -> Option<TEK> {
    let left_ty = term_utils::resolve_term(left, unifier);
    let right_ty = term_utils::resolve_term(right, unifier);
    let result_ty = term_utils::resolve_term(result, unifier);

    if left_ty.is_address() && right_ty.is_address() {
        if !left_ty.shape_eq(&right_ty) {
            return Some(TEK::ArithTypeMismatch(left_ty, right_ty));
        }
        if op != BinaryOp::Sub {
            return Some(TEK::ArithTypeMismatch(left_ty, right_ty));
        }
        let _ = unifier.unify(left, &left_ty);
        let _ = unifier.unify(right, &right_ty);
        let _ = unifier.unify(result, &Type::uint(64));
        return None;
    }

    let address_anchor = if left_ty.is_address() {
        Some(left_ty.clone())
    } else if result_ty.is_address() {
        Some(result_ty.clone())
    } else {
        None
    };

    if let Some(addr_ty) = address_anchor {
        if right_ty.is_address() {
            return Some(TEK::ArithTypeMismatch(addr_ty, right_ty));
        }
        let _ = unifier.unify(left, &addr_ty);
        let _ = unifier.unify(result, &addr_ty);
        let _ = unifier.unify(right, &Type::uint(64));

        let left_diag = term_utils::resolve_term_for_diagnostics(left, unifier);
        let right_diag = term_utils::resolve_term_for_diagnostics(right, unifier);
        let result_diag = term_utils::resolve_term_for_diagnostics(result, unifier);
        if !left_diag.is_address() {
            return Some(TEK::ArithTypeMismatch(left_diag, right_diag));
        }
        if !matches!(
            right_diag,
            Type::Int {
                signed: false,
                bits: 64,
                ..
            }
        ) {
            return Some(TEK::ArithTypeMismatch(left_diag, right_diag));
        }
        if !result_diag.is_address() {
            return Some(TEK::ArithTypeMismatch(result_diag, addr_ty));
        }
        return None;
    }

    let _ = unifier.unify(left, right);
    let _ = unifier.unify(result, left);
    let left_diag = term_utils::resolve_term_for_diagnostics(left, unifier);
    let right_diag = term_utils::resolve_term_for_diagnostics(right, unifier);
    if !term_utils::is_int_like(&left_diag) && !term_utils::is_unresolved(&left_diag) {
        return Some(TEK::ArithOperandNotInt(left_diag));
    }
    if !term_utils::is_int_like(&right_diag) && !term_utils::is_unresolved(&right_diag) {
        return Some(TEK::ArithOperandNotInt(right_diag));
    }
    None
}

fn op_span(obligation: &ExprObligation) -> Span {
    match obligation {
        ExprObligation::BinOp { span, .. } | ExprObligation::UnaryOp { span, .. } => *span,
        _ => unreachable!("operator span helper should only see BinOp/UnaryOp"),
    }
}

fn first_non_int_operand(left: &Type, right: &Type, span: Span) -> Option<TypeCheckError> {
    if !term_utils::is_int_like(left) && !term_utils::is_unresolved(left) {
        return Some(TEK::ArithOperandNotInt(left.clone()).at(span));
    }
    if !term_utils::is_int_like(right) && !term_utils::is_unresolved(right) {
        return Some(TEK::ArithOperandNotInt(right.clone()).at(span));
    }
    None
}

fn first_non_int_cmp_operand(left: &Type, right: &Type, span: Span) -> Option<TypeCheckError> {
    if !term_utils::is_int_like(left) && !term_utils::is_unresolved(left) {
        return Some(TEK::CmpOperandNotInt(left.clone()).at(span));
    }
    if !term_utils::is_int_like(right) && !term_utils::is_unresolved(right) {
        return Some(TEK::CmpOperandNotInt(right.clone()).at(span));
    }
    None
}

fn first_non_equatable_cmp_operand(
    left: &Type,
    right: &Type,
    span: Span,
) -> Option<TypeCheckError> {
    if !term_utils::is_unresolved(left)
        && let Err(failure) = ensure_equatable(left)
    {
        return Some(
            TEK::TypeNotEquatable(left.clone(), failure.path, failure.failing_ty).at(span),
        );
    }
    if !term_utils::is_unresolved(right)
        && let Err(failure) = ensure_equatable(right)
    {
        return Some(
            TEK::TypeNotEquatable(right.clone(), failure.path, failure.failing_ty).at(span),
        );
    }
    None
}

fn first_non_bool_operand(left: &Type, right: &Type, span: Span) -> Option<TypeCheckError> {
    if *left != Type::Bool && !term_utils::is_unresolved(left) {
        return Some(TEK::LogicalOperandNotBoolean(left.clone()).at(span));
    }
    if *right != Type::Bool && !term_utils::is_unresolved(right) {
        return Some(TEK::LogicalOperandNotBoolean(right.clone()).at(span));
    }
    None
}
