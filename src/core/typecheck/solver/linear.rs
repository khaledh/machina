//! Hosted linear-type expression obligations.
//!
//! This module handles the first hosted session-entry step:
//! `Machine<T>.create(LinearType as Role)`. The surface stays an ordinary
//! method call; the solver simply uses hosted linear metadata to type the
//! result and validate the requested type/role pair.

use std::collections::{HashMap, HashSet};

use crate::core::ast::NodeId;
use crate::core::linear::LinearIndex;
use crate::core::typecheck::constraints::ExprObligation;
use crate::core::typecheck::errors::{TEK, TypeCheckError};
use crate::core::typecheck::solver::term_utils;
use crate::core::typecheck::unify::TcUnifier;
use crate::core::types::Type;

pub(super) fn try_check_expr_obligation_linear(
    obligation: &ExprObligation,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    linear_index: &LinearIndex,
    errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) -> bool {
    if let ExprObligation::LinearSessionAction {
        expr_id,
        receiver: _,
        type_name,
        role_name,
        source_state,
        action_name,
        arg_terms,
        expected_arg_tys,
        result,
        span,
    } = obligation
    {
        return check_linear_session_action(
            *expr_id,
            type_name,
            role_name,
            source_state,
            action_name,
            arg_terms,
            expected_arg_tys,
            result,
            *span,
            unifier,
            type_defs,
            linear_index,
            errors,
            covered_exprs,
        );
    }

    if let ExprObligation::LinearMachineResume {
        expr_id,
        receiver,
        type_name,
        role_name,
        key_term,
        expected_key_ty,
        result,
        span,
    } = obligation
    {
        return check_linear_machine_resume(
            *expr_id,
            receiver,
            type_name,
            role_name,
            key_term,
            expected_key_ty,
            result,
            *span,
            unifier,
            type_defs,
            linear_index,
            errors,
            covered_exprs,
        );
    }

    if let ExprObligation::LinearMachineLookup {
        expr_id,
        receiver,
        type_name,
        key_term,
        expected_key_ty,
        result,
        span,
    } = obligation
    {
        return check_linear_machine_lookup(
            *expr_id,
            receiver,
            type_name,
            key_term,
            expected_key_ty,
            result,
            *span,
            unifier,
            type_defs,
            linear_index,
            errors,
            covered_exprs,
        );
    }

    if let ExprObligation::LinearMachineDeliver {
        expr_id,
        receiver,
        key_term,
        event_term,
        result,
        span,
    } = obligation
    {
        return check_linear_machine_deliver(
            *expr_id,
            receiver,
            key_term,
            event_term,
            result,
            *span,
            unifier,
            type_defs,
            linear_index,
            errors,
            covered_exprs,
        );
    }

    let ExprObligation::LinearMachineCreate {
        expr_id,
        receiver,
        type_name,
        role_name,
        result,
        span,
    } = obligation
    else {
        return false;
    };

    let receiver_ty = term_utils::resolve_term(receiver, unifier);
    if term_utils::is_unresolved(&receiver_ty) {
        // `create` rides on top of earlier ordinary call solving (`spawn()?`,
        // local binding propagation, etc.), so defer diagnostics until those
        // surrounding receiver types have become concrete.
        return true;
    }
    let Some((machine_name, host_info)) = machine_host_for_receiver(&receiver_ty, linear_index)
    else {
        crate::core::typecheck::tc_push_error!(
            errors,
            *span,
            TEK::OverloadNoMatch("create".to_string())
        );
        covered_exprs.insert(*expr_id);
        return true;
    };

    if host_info.hosted_type_name != *type_name {
        crate::core::typecheck::tc_push_error!(
            errors,
            *span,
            TEK::LinearSessionHostMismatch(
                machine_name.to_string(),
                host_info.hosted_type_name.clone(),
                type_name.clone(),
            )
        );
        covered_exprs.insert(*expr_id);
        return true;
    }

    let Some(type_info) = linear_index.types.get(type_name) else {
        crate::core::typecheck::tc_push_error!(errors, *span, TEK::UnknownType);
        covered_exprs.insert(*expr_id);
        return true;
    };

    if !type_info.roles.contains_key(role_name) {
        crate::core::typecheck::tc_push_error!(
            errors,
            *span,
            TEK::LinearSessionUnknownRole(type_name.clone(), role_name.clone())
        );
        covered_exprs.insert(*expr_id);
        return true;
    }

    let Some(hosted_ty) = type_defs.get(type_name) else {
        crate::core::typecheck::tc_push_error!(errors, *span, TEK::UnknownType);
        covered_exprs.insert(*expr_id);
        return true;
    };

    let session_error_ty = type_defs
        .get("SessionError")
        .cloned()
        .unwrap_or(Type::Enum {
            name: "SessionError".to_string(),
            variants: Vec::new(),
        });
    let session_result_ty = Type::ErrorUnion {
        ok_ty: Box::new(hosted_ty.clone()),
        err_tys: vec![session_error_ty],
    };

    if let Err(err) = unifier.unify(
        &term_utils::canonicalize_type(result.clone()),
        &term_utils::canonicalize_type(session_result_ty),
    ) {
        errors.push(super::constraint_checks::unify_error_to_diag(err, *span));
    }

    covered_exprs.insert(*expr_id);
    true
}

fn check_linear_machine_deliver(
    expr_id: NodeId,
    receiver: &Type,
    key_term: &Type,
    event_term: &Type,
    result: &Type,
    span: crate::core::diag::Span,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    linear_index: &LinearIndex,
    errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) -> bool {
    let receiver_ty = term_utils::resolve_term(receiver, unifier);
    if term_utils::is_unresolved(&receiver_ty) {
        return true;
    }
    let Some((machine_name, host_info)) = machine_host_for_receiver(&receiver_ty, linear_index)
    else {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::OverloadNoMatch("deliver".to_string())
        );
        covered_exprs.insert(expr_id);
        return true;
    };

    let expected_key_ty = resolve_linear_type_expr(&host_info.key_ty, type_defs);
    let actual_key_ty = term_utils::resolve_term(key_term, unifier);
    if unifier
        .unify(
            &term_utils::canonicalize_type(actual_key_ty.clone()),
            &term_utils::canonicalize_type(expected_key_ty.clone()),
        )
        .is_err()
    {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::LinearMachineDeliverKeyTypeMismatch(
                machine_name.to_string(),
                host_info.hosted_type_name.clone(),
                expected_key_ty.to_string(),
                actual_key_ty.to_string(),
            )
        );
        covered_exprs.insert(expr_id);
        return true;
    }

    let Some(type_info) = linear_index.types.get(&host_info.hosted_type_name) else {
        crate::core::typecheck::tc_push_error!(errors, span, TEK::UnknownType);
        covered_exprs.insert(expr_id);
        return true;
    };

    let event_ty = term_utils::resolve_term(event_term, unifier);
    let Some(event_type_name) = named_type_name(&event_ty) else {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::LinearMachineDeliverUnknownTrigger(
                machine_name.to_string(),
                host_info.hosted_type_name.clone(),
                format!("{event_ty}"),
            )
        );
        covered_exprs.insert(expr_id);
        return true;
    };

    if !type_info.triggers.contains_key(&event_type_name) {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::LinearMachineDeliverUnknownTrigger(
                machine_name.to_string(),
                host_info.hosted_type_name.clone(),
                event_type_name.to_string(),
            )
        );
        covered_exprs.insert(expr_id);
        return true;
    }

    let deliver_result_ty = type_defs
        .get("DeliverResult")
        .cloned()
        .unwrap_or(Type::Enum {
            name: "DeliverResult".to_string(),
            variants: Vec::new(),
        });
    if let Err(err) = unifier.unify(
        &term_utils::canonicalize_type(result.clone()),
        &term_utils::canonicalize_type(deliver_result_ty),
    ) {
        errors.push(super::constraint_checks::unify_error_to_diag(err, span));
    }

    covered_exprs.insert(expr_id);
    true
}

fn resolve_linear_type_expr(
    type_expr: &crate::core::ast::TypeExpr,
    type_defs: &HashMap<String, Type>,
) -> Type {
    use crate::core::ast::TypeExprKind as TEK;
    match &type_expr.kind {
        TEK::Named { ident, .. } => match ident.as_str() {
            "()" => Type::Unit,
            "u8" => Type::uint(8),
            "u16" => Type::uint(16),
            "u32" => Type::uint(32),
            "u64" => Type::uint(64),
            "i8" => Type::sint(8),
            "i16" => Type::sint(16),
            "i32" => Type::sint(32),
            "i64" => Type::sint(64),
            "bool" => Type::Bool,
            "char" => Type::Char,
            "string" => Type::String,
            _ => type_defs.get(ident).cloned().unwrap_or(Type::Unknown),
        },
        TEK::Union { variants } => {
            let mut resolved = variants
                .iter()
                .map(|variant| resolve_linear_type_expr(variant, type_defs))
                .collect::<Vec<_>>();
            if resolved.is_empty() {
                Type::Unknown
            } else {
                let ok_ty = resolved.remove(0);
                Type::ErrorUnion {
                    ok_ty: Box::new(ok_ty),
                    err_tys: resolved,
                }
            }
        }
        TEK::Array { elem_ty_expr, dims } => Type::Array {
            elem_ty: Box::new(resolve_linear_type_expr(elem_ty_expr, type_defs)),
            dims: dims.clone(),
        },
        TEK::Tuple { field_ty_exprs } => Type::Tuple {
            field_tys: field_ty_exprs
                .iter()
                .map(|field| resolve_linear_type_expr(field, type_defs))
                .collect(),
        },
        TEK::Heap { elem_ty_expr } => Type::Heap {
            elem_ty: Box::new(resolve_linear_type_expr(elem_ty_expr, type_defs)),
        },
        TEK::Slice { elem_ty_expr } => Type::Slice {
            elem_ty: Box::new(resolve_linear_type_expr(elem_ty_expr, type_defs)),
        },
        _ => Type::Unknown,
    }
}

fn named_type_name(ty: &Type) -> Option<String> {
    match ty.peel_heap() {
        Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name.clone()),
        _ => None,
    }
}

fn check_linear_session_action(
    expr_id: NodeId,
    type_name: &str,
    role_name: &str,
    source_state: &str,
    action_name: &str,
    arg_terms: &[Type],
    expected_arg_tys: &[Type],
    result: &Type,
    span: crate::core::diag::Span,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    linear_index: &LinearIndex,
    errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) -> bool {
    let Some(type_info) = linear_index.types.get(type_name) else {
        crate::core::typecheck::tc_push_error!(errors, span, TEK::UnknownType);
        covered_exprs.insert(expr_id);
        return true;
    };

    let Some(role_info) = type_info.roles.get(role_name) else {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::LinearSessionUnknownRole(type_name.to_string(), role_name.to_string())
        );
        covered_exprs.insert(expr_id);
        return true;
    };

    if !role_info
        .allowed_actions
        .iter()
        .any(|allowed| allowed == action_name)
    {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::LinearSessionActionNotAllowed(
                action_name.to_string(),
                type_name.to_string(),
                source_state.to_string(),
                role_name.to_string(),
            )
        );
        covered_exprs.insert(expr_id);
        return true;
    }

    for (arg_ty, expected_ty) in arg_terms.iter().zip(expected_arg_tys.iter()) {
        if let Err(err) = unifier.unify(
            &term_utils::canonicalize_type(arg_ty.clone()),
            &term_utils::canonicalize_type(expected_ty.clone()),
        ) {
            errors.push(super::constraint_checks::unify_error_to_diag(err, span));
        }
    }

    let Some(hosted_ty) = type_defs.get(type_name) else {
        crate::core::typecheck::tc_push_error!(errors, span, TEK::UnknownType);
        covered_exprs.insert(expr_id);
        return true;
    };

    let session_error_ty = type_defs
        .get("SessionError")
        .cloned()
        .unwrap_or(Type::Enum {
            name: "SessionError".to_string(),
            variants: Vec::new(),
        });
    let session_result_ty = Type::ErrorUnion {
        ok_ty: Box::new(hosted_ty.clone()),
        err_tys: vec![session_error_ty],
    };

    if let Err(err) = unifier.unify(
        &term_utils::canonicalize_type(result.clone()),
        &term_utils::canonicalize_type(session_result_ty),
    ) {
        errors.push(super::constraint_checks::unify_error_to_diag(err, span));
    }

    covered_exprs.insert(expr_id);
    true
}

fn check_linear_machine_resume(
    expr_id: NodeId,
    receiver: &Type,
    type_name: &str,
    role_name: &str,
    key_term: &Type,
    expected_key_ty: &Type,
    result: &Type,
    span: crate::core::diag::Span,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    linear_index: &LinearIndex,
    errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) -> bool {
    let receiver_ty = term_utils::resolve_term(receiver, unifier);
    if term_utils::is_unresolved(&receiver_ty) {
        return true;
    }
    let Some((machine_name, host_info)) = machine_host_for_receiver(&receiver_ty, linear_index)
    else {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::OverloadNoMatch("resume".to_string())
        );
        covered_exprs.insert(expr_id);
        return true;
    };

    if host_info.hosted_type_name != *type_name {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::LinearSessionHostMismatch(
                machine_name.to_string(),
                host_info.hosted_type_name.clone(),
                type_name.to_string(),
            )
        );
        covered_exprs.insert(expr_id);
        return true;
    }

    let Some(type_info) = linear_index.types.get(type_name) else {
        crate::core::typecheck::tc_push_error!(errors, span, TEK::UnknownType);
        covered_exprs.insert(expr_id);
        return true;
    };

    if !type_info.roles.contains_key(role_name) {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::LinearSessionUnknownRole(type_name.to_string(), role_name.to_string())
        );
        covered_exprs.insert(expr_id);
        return true;
    }

    if let Err(err) = unifier.unify(
        &term_utils::canonicalize_type(key_term.clone()),
        &term_utils::canonicalize_type(expected_key_ty.clone()),
    ) {
        errors.push(super::constraint_checks::unify_error_to_diag(err, span));
    }

    let Some(hosted_ty) = type_defs.get(type_name) else {
        crate::core::typecheck::tc_push_error!(errors, span, TEK::UnknownType);
        covered_exprs.insert(expr_id);
        return true;
    };
    let session_error_ty = type_defs
        .get("SessionError")
        .cloned()
        .unwrap_or(Type::Enum {
            name: "SessionError".to_string(),
            variants: Vec::new(),
        });
    let session_result_ty = Type::ErrorUnion {
        ok_ty: Box::new(hosted_ty.clone()),
        err_tys: vec![session_error_ty],
    };

    if let Err(err) = unifier.unify(
        &term_utils::canonicalize_type(result.clone()),
        &term_utils::canonicalize_type(session_result_ty),
    ) {
        errors.push(super::constraint_checks::unify_error_to_diag(err, span));
    }

    covered_exprs.insert(expr_id);
    true
}

fn check_linear_machine_lookup(
    expr_id: NodeId,
    receiver: &Type,
    type_name: &str,
    key_term: &Type,
    expected_key_ty: &Type,
    result: &Type,
    span: crate::core::diag::Span,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    linear_index: &LinearIndex,
    errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) -> bool {
    let receiver_ty = term_utils::resolve_term(receiver, unifier);
    if term_utils::is_unresolved(&receiver_ty) {
        return true;
    }
    let Some((machine_name, host_info)) = machine_host_for_receiver(&receiver_ty, linear_index)
    else {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::OverloadNoMatch("lookup".to_string())
        );
        covered_exprs.insert(expr_id);
        return true;
    };

    if host_info.hosted_type_name != *type_name {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::LinearSessionHostMismatch(
                machine_name.to_string(),
                host_info.hosted_type_name.clone(),
                type_name.to_string(),
            )
        );
        covered_exprs.insert(expr_id);
        return true;
    }

    let actual_key_ty = term_utils::resolve_term(key_term, unifier);
    if unifier
        .unify(
            &term_utils::canonicalize_type(actual_key_ty.clone()),
            &term_utils::canonicalize_type(expected_key_ty.clone()),
        )
        .is_err()
    {
        crate::core::typecheck::tc_push_error!(
            errors,
            span,
            TEK::LinearMachineLookupKeyTypeMismatch(
                machine_name.to_string(),
                host_info.hosted_type_name.clone(),
                expected_key_ty.to_string(),
                actual_key_ty.to_string(),
            )
        );
        covered_exprs.insert(expr_id);
        return true;
    }

    let Some(hosted_ty) = type_defs.get(type_name) else {
        crate::core::typecheck::tc_push_error!(errors, span, TEK::UnknownType);
        covered_exprs.insert(expr_id);
        return true;
    };
    let session_error_ty = type_defs
        .get("SessionError")
        .cloned()
        .unwrap_or(Type::Enum {
            name: "SessionError".to_string(),
            variants: Vec::new(),
        });
    let session_result_ty = Type::ErrorUnion {
        ok_ty: Box::new(hosted_ty.clone()),
        err_tys: vec![session_error_ty],
    };

    if let Err(err) = unifier.unify(
        &term_utils::canonicalize_type(result.clone()),
        &term_utils::canonicalize_type(session_result_ty),
    ) {
        errors.push(super::constraint_checks::unify_error_to_diag(err, span));
    }

    covered_exprs.insert(expr_id);
    true
}

fn machine_host_for_receiver<'a>(
    receiver_ty: &Type,
    linear_index: &'a LinearIndex,
) -> Option<(&'a str, &'a crate::core::linear::LinearHostInfo)> {
    let Type::Struct { name, .. } = receiver_ty else {
        return None;
    };
    linear_index
        .machine_hosts
        .iter()
        .find_map(|(machine_name, host_info)| {
            (host_info.handle_type_name == *name).then_some((machine_name.as_str(), host_info))
        })
}
