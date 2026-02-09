//! Expression operator obligation solving.
//!
//! Handles diagnostics and type expectations for binary/unary expression
//! obligations (arithmetic, comparisons, logical operators).

use std::collections::HashSet;

use crate::diag::Span;
use crate::tree::NodeId;
use crate::typecheck::capability::ensure_equatable;
use crate::typecheck::constraints::ExprObligation;
use crate::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::typecheck::unify::TcUnifier;
use crate::types::Type;

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
            let left_ty = super::resolve_term_for_diagnostics(left, unifier);
            let right_ty = super::resolve_term_for_diagnostics(right, unifier);
            match op {
                crate::tree::resolved::BinaryOp::Add
                | crate::tree::resolved::BinaryOp::Sub
                | crate::tree::resolved::BinaryOp::Mul
                | crate::tree::resolved::BinaryOp::Div
                | crate::tree::resolved::BinaryOp::Mod
                | crate::tree::resolved::BinaryOp::BitOr
                | crate::tree::resolved::BinaryOp::BitXor
                | crate::tree::resolved::BinaryOp::BitAnd
                | crate::tree::resolved::BinaryOp::Shl
                | crate::tree::resolved::BinaryOp::Shr => {
                    if let Some(err) =
                        first_non_int_operand(&left_ty, &right_ty, op_span(obligation))
                    {
                        errors.push(err);
                        covered_exprs.insert(*expr_id);
                    }
                }
                crate::tree::resolved::BinaryOp::Eq | crate::tree::resolved::BinaryOp::Ne => {
                    if let Some(err) =
                        first_non_equatable_cmp_operand(&left_ty, &right_ty, op_span(obligation))
                    {
                        errors.push(err);
                        covered_exprs.insert(*expr_id);
                    }
                }
                crate::tree::resolved::BinaryOp::Lt
                | crate::tree::resolved::BinaryOp::Gt
                | crate::tree::resolved::BinaryOp::LtEq
                | crate::tree::resolved::BinaryOp::GtEq => {
                    if let Some(err) =
                        first_non_int_cmp_operand(&left_ty, &right_ty, op_span(obligation))
                    {
                        errors.push(err);
                        covered_exprs.insert(*expr_id);
                    }
                }
                crate::tree::resolved::BinaryOp::LogicalAnd
                | crate::tree::resolved::BinaryOp::LogicalOr => {
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
            let operand_ty = super::resolve_term_for_diagnostics(operand, unifier);
            match op {
                crate::tree::resolved::UnaryOp::Neg | crate::tree::resolved::UnaryOp::BitNot => {
                    if !super::is_int_like(&operand_ty) && !super::is_unresolved(&operand_ty) {
                        errors.push(
                            TypeCheckErrorKind::NegationOperandNotInt(operand_ty, *span).into(),
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
                crate::tree::resolved::UnaryOp::LogicalNot => {
                    if operand_ty != Type::Bool && !super::is_unresolved(&operand_ty) {
                        errors.push(
                            TypeCheckErrorKind::LogicalOperandNotBoolean(operand_ty, *span).into(),
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
                crate::tree::resolved::UnaryOp::Try => {}
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
    if !super::is_int_like(left) && !super::is_unresolved(left) {
        return Some(TypeCheckErrorKind::ArithOperandNotInt(left.clone(), span).into());
    }
    if !super::is_int_like(right) && !super::is_unresolved(right) {
        return Some(TypeCheckErrorKind::ArithOperandNotInt(right.clone(), span).into());
    }
    None
}

fn first_non_int_cmp_operand(left: &Type, right: &Type, span: Span) -> Option<TypeCheckError> {
    if !super::is_int_like(left) && !super::is_unresolved(left) {
        return Some(TypeCheckErrorKind::CmpOperandNotInt(left.clone(), span).into());
    }
    if !super::is_int_like(right) && !super::is_unresolved(right) {
        return Some(TypeCheckErrorKind::CmpOperandNotInt(right.clone(), span).into());
    }
    None
}

fn first_non_equatable_cmp_operand(
    left: &Type,
    right: &Type,
    span: Span,
) -> Option<TypeCheckError> {
    if !super::is_unresolved(left)
        && let Err(failure) = ensure_equatable(left)
    {
        return Some(
            TypeCheckErrorKind::TypeNotEquatable(
                left.clone(),
                failure.path,
                failure.failing_ty,
                span,
            )
            .into(),
        );
    }
    if !super::is_unresolved(right)
        && let Err(failure) = ensure_equatable(right)
    {
        return Some(
            TypeCheckErrorKind::TypeNotEquatable(
                right.clone(),
                failure.path,
                failure.failing_ty,
                span,
            )
            .into(),
        );
    }
    None
}

fn first_non_bool_operand(left: &Type, right: &Type, span: Span) -> Option<TypeCheckError> {
    if *left != Type::Bool && !super::is_unresolved(left) {
        return Some(TypeCheckErrorKind::LogicalOperandNotBoolean(left.clone(), span).into());
    }
    if *right != Type::Bool && !super::is_unresolved(right) {
        return Some(TypeCheckErrorKind::LogicalOperandNotBoolean(right.clone(), span).into());
    }
    None
}
