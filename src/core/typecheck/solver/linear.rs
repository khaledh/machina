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
