//! Call obligation solving.
//!
//! This module owns overload selection, callable instantiation, and builtin
//! method handling for dynamic arrays, sets, and maps.

use std::collections::{HashMap, HashSet};

use crate::core::ast::NodeId;
use crate::core::capsule::ModuleId;
use crate::core::resolve::{DefId, DefTable};
use crate::core::typecheck::builtin_methods;
use crate::core::typecheck::builtin_methods::BuiltinMethodRet;
use crate::core::typecheck::constraints::{CallCallee, CallObligation};
use crate::core::typecheck::engine::{
    CollectedCallableSig, CollectedPropertySig, CollectedTraitSig, lookup_property,
};
use crate::core::typecheck::errors::{TEK, TypeCheckError};
use crate::core::typecheck::tc_push_error;
use crate::core::typecheck::unify::TcUnifier;
use crate::core::types::{TyVarId, Type, TypeAssignability, type_assignable};

#[allow(clippy::too_many_arguments)]
pub(super) fn check_call_obligations(
    obligations: &[CallObligation],
    unifier: &mut TcUnifier,
    func_sigs: &HashMap<String, Vec<CollectedCallableSig>>,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
    property_sigs: &HashMap<String, HashMap<String, CollectedPropertySig>>,
    trait_sigs: &HashMap<String, CollectedTraitSig>,
    trait_impls: &HashMap<String, HashSet<String>>,
    var_trait_bounds: &HashMap<TyVarId, Vec<String>>,
    def_table: &DefTable,
    def_owners: &HashMap<DefId, ModuleId>,
    defer_on_unresolved_args: bool,
) -> (
    Vec<TypeCheckError>,
    HashMap<NodeId, DefId>,
    Vec<CallObligation>,
) {
    let mut errors = Vec::new();
    let mut resolved_call_defs = HashMap::new();
    let mut deferred = Vec::new();
    for obligation in obligations {
        if let CallCallee::Dynamic { expr_id } = &obligation.callee {
            if let Some(callee_term) = &obligation.callee_ty {
                let callee_ty = super::term_utils::resolve_term(callee_term, unifier);
                if let Type::Fn { params, ret_ty } = callee_ty {
                    if params.len() != obligation.arg_terms.len() {
                        tc_push_error!(
                            errors,
                            obligation.span,
                            TEK::ArgCountMismatch(
                                "<fn>".to_string(),
                                params.len(),
                                obligation.arg_terms.len(),
                            )
                        );
                        continue;
                    }
                    let mut arg_failed = false;
                    for (index, (arg_term, param)) in
                        obligation.arg_terms.iter().zip(params.iter()).enumerate()
                    {
                        let arg_ty = super::term_utils::resolve_term(arg_term, unifier);
                        if solve_call_arg_assignable(&arg_ty, &param.ty, unifier, method_sigs)
                            .is_err()
                        {
                            tc_push_error!(
                                errors,
                                obligation.span,
                                TEK::ArgTypeMismatch(
                                    index + 1,
                                    super::term_utils::canonicalize_type(unifier.apply(&param.ty)),
                                    super::term_utils::canonicalize_type(unifier.apply(&arg_ty)),
                                )
                            );
                            arg_failed = true;
                            break;
                        }
                    }
                    if arg_failed {
                        continue;
                    }
                    if let Err(err) = solve_call_arg_assignable(
                        &super::term_utils::canonicalize_type((*ret_ty).clone()),
                        &super::term_utils::canonicalize_type(obligation.ret_ty.clone()),
                        unifier,
                        method_sigs,
                    ) {
                        errors.push(super::constraint_checks::unify_error_to_diag(
                            err,
                            obligation.span,
                        ));
                    }
                } else if super::term_utils::is_unresolved(&callee_ty) {
                    deferred.push(obligation.clone());
                } else {
                    tc_push_error!(
                        errors,
                        obligation.span,
                        TEK::OverloadNoMatch(format!("<dynamic:{expr_id}>"))
                    );
                }
            } else {
                deferred.push(obligation.clone());
            }
            continue;
        }

        let has_unresolved_receiver = obligation
            .receiver
            .as_ref()
            .map(|receiver_term| {
                let receiver_ty = super::term_utils::resolve_term(receiver_term, unifier);
                super::term_utils::is_unresolved(&receiver_ty)
            })
            .unwrap_or(false);
        // Receiver uncertainty almost always merits deferring. For arguments,
        // defer only in rounds that explicitly allow it; the final retry pass
        // runs with this disabled so concrete diagnostics still surface.
        let has_unresolved_args = obligation.arg_terms.iter().any(|arg_term| {
            let arg_ty = super::term_utils::resolve_term(arg_term, unifier);
            super::term_utils::is_unresolved(&arg_ty)
        });
        let should_defer_for_unresolved =
            has_unresolved_receiver || (defer_on_unresolved_args && has_unresolved_args);

        let mut candidates: Vec<CollectedCallableSig> = match &obligation.callee {
            CallCallee::NamedFunction { name, .. } => {
                named_call_candidates(name, obligation.arg_terms.len(), func_sigs)
            }
            CallCallee::Method { name } => {
                if let Some(result) = try_solve_builtin_method(obligation, name, unifier) {
                    if let Err(err) = result {
                        errors.push(err);
                    }
                    continue;
                }
                let receiver_ty = obligation
                    .receiver
                    .as_ref()
                    .map(|term| super::term_utils::resolve_term(term, unifier))
                    .zip(obligation.receiver_witness.as_ref())
                    .map(|(receiver_ty, witness_ty)| {
                        let witness_ty = super::term_utils::resolve_term(witness_ty, unifier);
                        if super::term_utils::is_unresolved(&witness_ty) {
                            receiver_ty
                        } else {
                            witness_ty
                        }
                    })
                    .or_else(|| {
                        obligation
                            .receiver
                            .as_ref()
                            .map(|term| super::term_utils::resolve_term(term, unifier))
                    })
                    .map(super::term_utils::peel_heap);
                method_call_candidates(
                    name,
                    receiver_ty.as_ref(),
                    obligation.arg_terms.len(),
                    method_sigs,
                    trait_sigs,
                    var_trait_bounds,
                )
            }
            CallCallee::Dynamic { .. } => Vec::new(),
        };

        let inaccessible_count = candidates
            .iter()
            .filter(|sig| {
                !super::access_utils::is_def_accessible_from(
                    obligation.caller_def_id,
                    sig.def_id,
                    def_table,
                    def_owners,
                )
            })
            .count();
        candidates.retain(|sig| {
            super::access_utils::is_def_accessible_from(
                obligation.caller_def_id,
                sig.def_id,
                def_table,
                def_owners,
            )
        });

        if candidates.is_empty() {
            if should_defer_for_unresolved {
                deferred.push(obligation.clone());
                continue;
            }
            let name = match &obligation.callee {
                CallCallee::NamedFunction { name, .. } => name.clone(),
                CallCallee::Method { name } => name.clone(),
                CallCallee::Dynamic { expr_id, .. } => format!("<dynamic:{expr_id}>"),
            };
            if inaccessible_count > 0 {
                tc_push_error!(errors, obligation.span, TEK::CallableNotAccessible(name));
            } else {
                tc_push_error!(errors, obligation.span, TEK::OverloadNoMatch(name));
            }
            continue;
        }

        let mut best_choice: Option<(i32, i32, i32, TcUnifier, DefId)> = None;
        let mut ambiguous_best = false;
        let mut first_error = None;
        for sig in candidates.drain(..) {
            let mut trial = unifier.clone();
            let instantiated = instantiate_sig(&sig, &mut trial);
            let mut failed = false;
            let mut score = 0i32;
            let method_priority = if sig.impl_trait.is_none() { 1 } else { 0 };
            let specialization_priority = if sig.type_param_count == 0 { 1 } else { 0 };
            if let (Some(receiver_term), Some(expected_receiver_ty)) =
                (&obligation.receiver, &instantiated.receiver_ty)
            {
                let receiver_ty = obligation
                    .receiver_witness
                    .as_ref()
                    .map(|witness_ty| super::term_utils::resolve_term(witness_ty, &trial))
                    .filter(|witness_ty| !super::term_utils::is_unresolved(witness_ty))
                    .unwrap_or_else(|| super::term_utils::resolve_term(receiver_term, &trial));
                if solve_call_arg_assignable(
                    &receiver_ty,
                    expected_receiver_ty,
                    &mut trial,
                    method_sigs,
                )
                .is_err()
                {
                    continue;
                }
                score += super::assignability::assignability_rank(
                    &super::term_utils::resolve_term(receiver_term, &trial),
                    &super::term_utils::canonicalize_type(trial.apply(expected_receiver_ty)),
                );
            }
            for (index, (arg_term, param_ty)) in obligation
                .arg_terms
                .iter()
                .zip(instantiated.params.iter())
                .enumerate()
            {
                let arg_ty = obligation
                    .arg_witnesses
                    .get(index)
                    .and_then(|witness_ty| witness_ty.as_ref())
                    .map(|witness_ty| super::term_utils::resolve_term(witness_ty, &trial))
                    .filter(|witness_ty| !super::term_utils::is_unresolved(witness_ty))
                    .unwrap_or_else(|| super::term_utils::resolve_term(arg_term, &trial));
                if solve_call_arg_assignable(&arg_ty, param_ty, &mut trial, method_sigs).is_err() {
                    first_error.get_or_insert_with(|| {
                        TEK::ArgTypeMismatch(
                            index + 1,
                            super::term_utils::canonicalize_type(trial.apply(param_ty)),
                            super::term_utils::canonicalize_type(trial.apply(&arg_ty)),
                        )
                        .at(obligation.span)
                    });
                    failed = true;
                    break;
                }
                score += call_arg_assignability_rank(
                    &super::term_utils::resolve_term(arg_term, &trial),
                    &super::term_utils::canonicalize_type(trial.apply(param_ty)),
                    method_sigs,
                );
            }
            if failed {
                continue;
            }
            if let Err(err) = trial.unify(
                &super::term_utils::canonicalize_type(obligation.ret_ty.clone()),
                &super::term_utils::canonicalize_type(instantiated.ret_ty.clone()),
            ) {
                first_error.get_or_insert_with(|| {
                    super::constraint_checks::unify_error_to_diag(err, obligation.span)
                });
                continue;
            }

            if let Some((trait_name, ty)) =
                unsatisfied_trait_bound(&instantiated.bound_terms, &trial, trait_impls)
            {
                first_error.get_or_insert_with(|| {
                    TEK::TraitBoundNotSatisfied(trait_name, ty).at(obligation.span)
                });
                continue;
            }
            match &best_choice {
                Some((best_score, best_method_priority, best_specialization_priority, _, _))
                    if score < *best_score
                        || (score == *best_score && method_priority < *best_method_priority)
                        || (score == *best_score
                            && method_priority == *best_method_priority
                            && specialization_priority < *best_specialization_priority) => {}
                Some((best_score, best_method_priority, best_specialization_priority, _, _))
                    if score == *best_score
                        && method_priority == *best_method_priority
                        && specialization_priority == *best_specialization_priority =>
                {
                    ambiguous_best = true;
                }
                _ => {
                    ambiguous_best = false;
                    best_choice = Some((
                        score,
                        method_priority,
                        specialization_priority,
                        trial,
                        sig.def_id,
                    ));
                }
            }
        }

        if let Some((_, _, _, next, def_id)) = best_choice {
            if ambiguous_best {
                if should_defer_for_unresolved {
                    deferred.push(obligation.clone());
                    continue;
                }
                let name = match &obligation.callee {
                    CallCallee::NamedFunction { name, .. } => name.clone(),
                    CallCallee::Method { name } => name.clone(),
                    CallCallee::Dynamic { expr_id, .. } => format!("<dynamic:{expr_id}>"),
                };
                tc_push_error!(errors, obligation.span, TEK::OverloadAmbiguous(name));
                continue;
            }
            if let Some(prop_name) = called_property_name(obligation, def_id, property_sigs, &next)
            {
                // Preserve substitutions to reduce follow-on inference noise, but
                // reject property accessor call syntax in source.
                *unifier = next;
                tc_push_error!(
                    errors,
                    obligation.span,
                    TEK::PropertyCalledAsMethod(prop_name)
                );
                continue;
            }
            *unifier = next;
            resolved_call_defs.insert(obligation.call_node, def_id);
        } else if let Some(err) = first_error {
            if should_defer_for_unresolved {
                deferred.push(obligation.clone());
                continue;
            }
            errors.push(err);
        } else {
            if should_defer_for_unresolved {
                deferred.push(obligation.clone());
                continue;
            }
            let name = match &obligation.callee {
                CallCallee::NamedFunction { name, .. } => name.clone(),
                CallCallee::Method { name } => name.clone(),
                CallCallee::Dynamic { expr_id, .. } => format!("<dynamic:{expr_id}>"),
            };
            tc_push_error!(errors, obligation.span, TEK::OverloadNoMatch(name));
        }
    }
    (errors, resolved_call_defs, deferred)
}

fn call_arg_assignability_rank(
    from: &Type,
    to: &Type,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> i32 {
    let Type::Iterable {
        item_ty: to_item_ty,
    } = to
    else {
        return super::assignability::assignability_rank(from, to);
    };

    let Some(plan) = super::iterable_plan(from, method_sigs) else {
        return super::assignability::assignability_rank(from, to);
    };

    match type_assignable(&plan.item_ty, to_item_ty) {
        TypeAssignability::Exact => 3,
        TypeAssignability::IntLitToInt { .. } => 2,
        TypeAssignability::RefinedToInt | TypeAssignability::IntToRefined { .. } => 1,
        TypeAssignability::Incompatible => -1000,
    }
}

fn solve_call_arg_assignable(
    from: &Type,
    to: &Type,
    unifier: &mut TcUnifier,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> Result<(), crate::core::typecheck::unify::TcUnifyError> {
    if let Some(result) = solve_iterable_param_assignable(from, to, unifier, method_sigs) {
        return result;
    }
    super::assignability::solve_assignable(from, to, unifier)
}

fn solve_iterable_param_assignable(
    from: &Type,
    to: &Type,
    unifier: &mut TcUnifier,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> Option<Result<(), crate::core::typecheck::unify::TcUnifyError>> {
    let Type::Iterable {
        item_ty: to_item_ty,
    } = to
    else {
        return None;
    };

    let resolved_from = super::term_utils::canonicalize_type(unifier.apply(from));
    if super::term_utils::is_unresolved(&resolved_from) {
        return Some(Ok(()));
    }

    if let Type::Iterable {
        item_ty: from_item_ty,
    } = &resolved_from
    {
        return Some(super::assignability::solve_assignable(
            from_item_ty,
            to_item_ty,
            unifier,
        ));
    }

    let Some(plan) = super::iterable_plan(&resolved_from, method_sigs) else {
        return Some(Err(crate::core::typecheck::unify::TcUnifyError::Mismatch(
            to.clone(),
            resolved_from,
        )));
    };

    let item_match = super::assignability::solve_assignable(&plan.item_ty, to_item_ty, unifier);
    Some(item_match)
}

fn named_call_candidates(
    name: &str,
    arity: usize,
    func_sigs: &HashMap<String, Vec<CollectedCallableSig>>,
) -> Vec<CollectedCallableSig> {
    func_sigs
        .get(name)
        .map(|overloads| {
            overloads
                .iter()
                .filter(|sig| sig.params.len() == arity)
                .cloned()
                .collect::<Vec<_>>()
        })
        .unwrap_or_default()
}

fn method_call_candidates(
    method_name: &str,
    receiver_ty: Option<&Type>,
    arity: usize,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
    trait_sigs: &HashMap<String, CollectedTraitSig>,
    var_trait_bounds: &HashMap<TyVarId, Vec<String>>,
) -> Vec<CollectedCallableSig> {
    let Some(receiver_ty) = receiver_ty else {
        return Vec::new();
    };
    match receiver_ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => method_sigs
            .get(name)
            .and_then(|by_name| by_name.get(method_name))
            .map(|overloads| {
                overloads
                    .iter()
                    .filter(|sig| sig.params.len() == arity)
                    .cloned()
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default(),
        Type::String => method_sigs
            .get("string")
            .and_then(|by_name| by_name.get(method_name))
            .map(|overloads| {
                overloads
                    .iter()
                    .filter(|sig| sig.params.len() == arity)
                    .cloned()
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default(),
        Type::Var(var) => {
            let mut candidates = Vec::new();
            for trait_name in var_trait_bounds.get(var).into_iter().flat_map(|v| v.iter()) {
                let Some(trait_sig) = trait_sigs.get(trait_name) else {
                    continue;
                };
                let Some(method) = trait_sig.methods.get(method_name) else {
                    continue;
                };
                if method.params.len() != arity {
                    continue;
                }
                candidates.push(CollectedCallableSig {
                    def_id: trait_sig.def_id,
                    self_ty: None,
                    params: method.params.clone(),
                    ret_ty: method.ret_ty.clone(),
                    type_param_count: method.type_param_count,
                    type_param_var_names: std::collections::BTreeMap::new(),
                    type_param_bounds: method.type_param_bounds.clone(),
                    self_mode: Some(method.self_mode.clone()),
                    impl_trait: Some(trait_name.clone()),
                });
            }
            candidates
        }
        _ => Vec::new(),
    }
}

fn called_property_name(
    obligation: &CallObligation,
    selected_def_id: DefId,
    property_sigs: &HashMap<String, HashMap<String, CollectedPropertySig>>,
    unifier: &TcUnifier,
) -> Option<String> {
    let CallCallee::Method { name } = &obligation.callee else {
        return None;
    };

    let owner_ty = obligation
        .receiver
        .as_ref()
        .map(|term| super::term_utils::resolve_term(term, unifier))
        .map(super::term_utils::peel_heap)?;
    let prop = lookup_property(property_sigs, &owner_ty, name)?;
    if prop.getter == Some(selected_def_id) || prop.setter == Some(selected_def_id) {
        Some(name.clone())
    } else {
        None
    }
}

fn try_solve_builtin_method(
    obligation: &CallObligation,
    method_name: &str,
    unifier: &mut TcUnifier,
) -> Option<Result<(), TypeCheckError>> {
    let receiver_term = obligation.receiver.as_ref()?;
    let receiver_ty = super::term_utils::resolve_term(receiver_term, unifier);
    if !builtin_methods::is_builtin_collection_receiver(&receiver_ty) {
        return None;
    }

    // Properties that should not be called as methods.
    if builtin_methods::resolve_builtin_property(&receiver_ty, method_name).is_some() {
        return Some(Err(
            TEK::PropertyCalledAsMethod(method_name.to_string()).at(obligation.span)
        ));
    }

    let builtin = builtin_methods::resolve_builtin_method(&receiver_ty, method_name)?;
    if let Some(hashable_ty) = builtin.hashable_ty()
        && !super::term_utils::is_unresolved(hashable_ty)
        && let Err(failure) = super::ensure_hashable(hashable_ty)
    {
        return Some(Err(TEK::TypeNotHashable(
            hashable_ty.clone(),
            failure.path,
            failure.failing_ty,
        )
        .at(obligation.span)));
    }
    let params = builtin.params();

    // Generic arity + assignability check.
    let arity = obligation.arg_terms.len();
    if arity != params.len() {
        return Some(Err(TEK::ArgCountMismatch(
            method_name.to_string(),
            params.len(),
            arity,
        )
        .at(obligation.span)));
    }
    for (index, (arg_term, expected_ty)) in
        obligation.arg_terms.iter().zip(params.iter()).enumerate()
    {
        let arg_ty = super::term_utils::resolve_term(arg_term, unifier);
        if super::assignability::solve_assignable(&arg_ty, &expected_ty.ty, unifier).is_err() {
            return Some(Err(TEK::ArgTypeMismatch(
                index + 1,
                expected_ty.ty.clone(),
                arg_ty,
            )
            .at(obligation.span)));
        }
    }

    let ret_ty = match builtin.ret_kind() {
        BuiltinMethodRet::Unit => Type::Unit,
        BuiltinMethodRet::Bool => Type::Bool,
        BuiltinMethodRet::MapGet { value_ty } => {
            if value_ty.needs_drop() && !super::term_utils::is_unresolved(&value_ty) {
                return Some(Err(
                    TEK::MapIndexValueNotCopySafe(value_ty).at(obligation.span)
                ));
            }
            Type::ErrorUnion {
                ok_ty: Box::new(value_ty),
                err_tys: vec![super::diag_utils::map_key_not_found_type()],
            }
        }
    };

    Some(
        unifier
            .unify(&obligation.ret_ty, &ret_ty)
            .map_err(|err| super::constraint_checks::unify_error_to_diag(err, obligation.span)),
    )
}

struct InstantiatedSig {
    receiver_ty: Option<Type>,
    params: Vec<Type>,
    ret_ty: Type,
    bound_terms: Vec<(String, Type)>,
}

fn instantiate_sig(sig: &CollectedCallableSig, unifier: &mut TcUnifier) -> InstantiatedSig {
    if sig.type_param_count == 0 {
        return InstantiatedSig {
            receiver_ty: sig.self_ty.clone(),
            params: sig.params.iter().map(|param| param.ty.clone()).collect(),
            ret_ty: sig.ret_ty.clone(),
            bound_terms: Vec::new(),
        };
    }
    let mut map = HashMap::new();
    let mut bound_terms = Vec::new();
    for index in 0..sig.type_param_count {
        let fresh = unifier.vars_mut().fresh_infer_local();
        map.insert(TyVarId::new(index as u32), Type::Var(fresh));
        if let Some(Some(trait_name)) = sig.type_param_bounds.get(index) {
            bound_terms.push((trait_name.clone(), Type::Var(fresh)));
        }
    }
    let params = sig
        .params
        .iter()
        .map(|param| subst_type_vars(&param.ty, &map))
        .collect::<Vec<_>>();
    let receiver_ty = sig.self_ty.as_ref().map(|ty| subst_type_vars(ty, &map));
    let ret_ty = subst_type_vars(&sig.ret_ty, &map);
    InstantiatedSig {
        receiver_ty,
        params,
        ret_ty,
        bound_terms,
    }
}

fn unsatisfied_trait_bound(
    bound_terms: &[(String, Type)],
    unifier: &TcUnifier,
    trait_impls: &HashMap<String, HashSet<String>>,
) -> Option<(String, Type)> {
    for (trait_name, term) in bound_terms {
        let resolved = super::term_utils::canonicalize_type(unifier.apply(term));
        let ty_name = match &resolved {
            Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name.as_str()),
            _ if super::term_utils::is_unresolved(&resolved) => None,
            _ => Some(""),
        };

        match ty_name {
            None => continue,
            Some(name) if !name.is_empty() => {
                let satisfied = trait_impls
                    .get(name)
                    .is_some_and(|traits| traits.contains(trait_name));
                if !satisfied {
                    return Some((trait_name.clone(), resolved));
                }
            }
            _ => return Some((trait_name.clone(), resolved)),
        }
    }
    None
}

fn subst_type_vars(ty: &Type, map: &HashMap<TyVarId, Type>) -> Type {
    ty.map_cloned(&|t| match t {
        Type::Var(var) => map.get(&var).cloned().unwrap_or(Type::Var(var)),
        other => other,
    })
}
