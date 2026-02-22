//! Expression operator obligation solving.
//!
//! Handles diagnostics and type expectations for binary/unary expression
//! obligations (arithmetic, comparisons, logical operators).

use std::collections::HashSet;

use crate::core::diag::Span;
use crate::core::tree::{BinaryOp, NodeId, UnaryOp};
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
            ..
        } => {
            let left_ty = term_utils::resolve_term_for_diagnostics(left, unifier);
            let right_ty = term_utils::resolve_term_for_diagnostics(right, unifier);
            match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::BitOr
                | BinaryOp::BitXor
                | BinaryOp::BitAnd
                | BinaryOp::Shl
                | BinaryOp::Shr => {
                    if let Some(err) =
                        first_non_int_operand(&left_ty, &right_ty, op_span(obligation))
                    {
                        errors.push(err);
                        covered_exprs.insert(*expr_id);
                    }
                }
                BinaryOp::Eq | BinaryOp::Ne => {
                    if let Some(err) =
                        first_non_equatable_cmp_operand(&left_ty, &right_ty, op_span(obligation))
                    {
                        errors.push(err);
                        covered_exprs.insert(*expr_id);
                    }
                }
                BinaryOp::Lt | BinaryOp::Gt | BinaryOp::LtEq | BinaryOp::GtEq => {
                    if let Some(err) =
                        first_non_int_cmp_operand(&left_ty, &right_ty, op_span(obligation))
                    {
                        errors.push(err);
                        covered_exprs.insert(*expr_id);
                    }
                }
                BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
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

fn op_span(obligation: &ExprObligation) -> Span {
    match obligation {
        ExprObligation::BinOp { span, .. } | ExprObligation::UnaryOp { span, .. } => *span,
        _ => unreachable!("operator span helper should only see BinOp/UnaryOp"),
    }
}

fn first_non_int_operand(left: &Type, right: &Type, span: Span) -> Option<TypeCheckError> {
    if !term_utils::is_int_like(left) && !term_utils::is_unresolved(left) {
        return Some(TEK::ArithOperandNotInt(left.clone()).at(span).into());
    }
    if !term_utils::is_int_like(right) && !term_utils::is_unresolved(right) {
        return Some(TEK::ArithOperandNotInt(right.clone()).at(span).into());
    }
    None
}

fn first_non_int_cmp_operand(left: &Type, right: &Type, span: Span) -> Option<TypeCheckError> {
    if !term_utils::is_int_like(left) && !term_utils::is_unresolved(left) {
        return Some(TEK::CmpOperandNotInt(left.clone()).at(span).into());
    }
    if !term_utils::is_int_like(right) && !term_utils::is_unresolved(right) {
        return Some(TEK::CmpOperandNotInt(right.clone()).at(span).into());
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
            TEK::TypeNotEquatable(left.clone(), failure.path, failure.failing_ty)
                .at(span)
                .into(),
        );
    }
    if !term_utils::is_unresolved(right)
        && let Err(failure) = ensure_equatable(right)
    {
        return Some(
            TEK::TypeNotEquatable(right.clone(), failure.path, failure.failing_ty)
                .at(span)
                .into(),
        );
    }
    None
}

fn first_non_bool_operand(left: &Type, right: &Type, span: Span) -> Option<TypeCheckError> {
    if *left != Type::Bool && !term_utils::is_unresolved(left) {
        return Some(TEK::LogicalOperandNotBoolean(left.clone()).at(span).into());
    }
    if *right != Type::Bool && !term_utils::is_unresolved(right) {
        return Some(TEK::LogicalOperandNotBoolean(right.clone()).at(span).into());
    }
    None
}
