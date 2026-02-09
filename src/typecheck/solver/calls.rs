//! Call obligation solving.
//!
//! This module owns overload selection, callable instantiation, and builtin
//! method handling for dynamic arrays, sets, and maps.

use std::collections::{HashMap, HashSet};

use crate::frontend::ModuleId;
use crate::resolve::{DefId, DefTable};
use crate::tree::NodeId;
use crate::typecheck::constraints::{CallCallee, CallObligation};
use crate::typecheck::engine::{CollectedCallableSig, CollectedPropertySig, CollectedTraitSig};
use crate::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::typecheck::unify::TcUnifier;
use crate::types::{TyVarId, Type};

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
                let callee_ty = super::resolve_term(callee_term, unifier);
                if let Type::Fn { params, ret_ty } = callee_ty {
                    if params.len() != obligation.arg_terms.len() {
                        errors.push(
                            TypeCheckErrorKind::ArgCountMismatch(
                                "<fn>".to_string(),
                                params.len(),
                                obligation.arg_terms.len(),
                                obligation.span,
                            )
                            .into(),
                        );
                        continue;
                    }
                    let mut arg_failed = false;
                    for (index, (arg_term, param)) in
                        obligation.arg_terms.iter().zip(params.iter()).enumerate()
                    {
                        let arg_ty = super::resolve_term(arg_term, unifier);
                        if let Err(_) =
                            super::assignability::solve_assignable(&arg_ty, &param.ty, unifier)
                        {
                            errors.push(
                                TypeCheckErrorKind::ArgTypeMismatch(
                                    index + 1,
                                    super::canonicalize_type(unifier.apply(&param.ty)),
                                    super::canonicalize_type(unifier.apply(&arg_ty)),
                                    obligation.span,
                                )
                                .into(),
                            );
                            arg_failed = true;
                            break;
                        }
                    }
                    if arg_failed {
                        continue;
                    }
                    if let Err(err) = unifier.unify(
                        &super::canonicalize_type(obligation.ret_ty.clone()),
                        &super::canonicalize_type((*ret_ty).clone()),
                    ) {
                        errors.push(super::constraint_checks::unify_error_to_diag(
                            err,
                            obligation.span,
                        ));
                    }
                } else if super::is_unresolved(&callee_ty) {
                    deferred.push(obligation.clone());
                } else {
                    errors.push(
                        TypeCheckErrorKind::OverloadNoMatch(
                            format!("<dynamic:{expr_id}>"),
                            obligation.span,
                        )
                        .into(),
                    );
                }
            } else {
                deferred.push(obligation.clone());
            }
            continue;
        }

        let mut candidates: Vec<CollectedCallableSig> = match &obligation.callee {
            CallCallee::NamedFunction { name, .. } => {
                named_call_candidates(name, obligation.arg_terms.len(), func_sigs)
            }
            CallCallee::Method { name } => {
                if let Some(result) = try_solve_dyn_array_builtin_method(obligation, name, unifier)
                {
                    if let Err(err) = result {
                        errors.push(err);
                    }
                    continue;
                }
                if let Some(result) = try_solve_set_builtin_method(obligation, name, unifier) {
                    if let Err(err) = result {
                        errors.push(err);
                    }
                    continue;
                }
                if let Some(result) = try_solve_map_builtin_method(obligation, name, unifier) {
                    if let Err(err) = result {
                        errors.push(err);
                    }
                    continue;
                }
                let receiver_ty = obligation
                    .receiver
                    .as_ref()
                    .map(|term| super::resolve_term(term, unifier))
                    .map(super::peel_heap);
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
                !super::is_def_accessible_from(
                    obligation.caller_def_id,
                    sig.def_id,
                    def_table,
                    def_owners,
                )
            })
            .count();
        candidates.retain(|sig| {
            super::is_def_accessible_from(
                obligation.caller_def_id,
                sig.def_id,
                def_table,
                def_owners,
            )
        });

        if candidates.is_empty() {
            let name = match &obligation.callee {
                CallCallee::NamedFunction { name, .. } => name.clone(),
                CallCallee::Method { name } => name.clone(),
                CallCallee::Dynamic { expr_id, .. } => format!("<dynamic:{expr_id}>"),
            };
            if inaccessible_count > 0 {
                errors
                    .push(TypeCheckErrorKind::CallableNotAccessible(name, obligation.span).into());
            } else {
                errors.push(TypeCheckErrorKind::OverloadNoMatch(name, obligation.span).into());
            }
            continue;
        }

        let mut best_choice: Option<(i32, i32, TcUnifier, DefId)> = None;
        let mut ambiguous_best = false;
        let mut first_error = None;
        for sig in candidates.drain(..) {
            let mut trial = unifier.clone();
            let instantiated = instantiate_sig(&sig, &mut trial);
            let mut failed = false;
            let mut score = 0i32;
            let method_priority = if sig.impl_trait.is_none() { 1 } else { 0 };
            for (index, (arg_term, param_ty)) in obligation
                .arg_terms
                .iter()
                .zip(instantiated.params.iter())
                .enumerate()
            {
                let arg_ty = super::resolve_term(arg_term, &trial);
                if let Err(_) =
                    super::assignability::solve_assignable(&arg_ty, param_ty, &mut trial)
                {
                    first_error.get_or_insert_with(|| {
                        TypeCheckErrorKind::ArgTypeMismatch(
                            index + 1,
                            super::canonicalize_type(trial.apply(param_ty)),
                            super::canonicalize_type(trial.apply(&arg_ty)),
                            obligation.span,
                        )
                        .into()
                    });
                    failed = true;
                    break;
                }
                score += super::assignability::assignability_rank(
                    &super::resolve_term(arg_term, &trial),
                    &super::canonicalize_type(trial.apply(param_ty)),
                );
            }
            if failed {
                continue;
            }
            if let Err(err) = trial.unify(
                &super::canonicalize_type(obligation.ret_ty.clone()),
                &super::canonicalize_type(instantiated.ret_ty.clone()),
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
                    TypeCheckErrorKind::TraitBoundNotSatisfied(trait_name, ty, obligation.span)
                        .into()
                });
                continue;
            }
            match &best_choice {
                Some((best_score, best_priority, _, _))
                    if score < *best_score
                        || (score == *best_score && method_priority < *best_priority) => {}
                Some((best_score, best_priority, _, _))
                    if score == *best_score && method_priority == *best_priority =>
                {
                    ambiguous_best = true;
                }
                _ => {
                    ambiguous_best = false;
                    best_choice = Some((score, method_priority, trial, sig.def_id));
                }
            }
        }

        if let Some((_, _, next, def_id)) = best_choice {
            if ambiguous_best {
                let name = match &obligation.callee {
                    CallCallee::NamedFunction { name, .. } => name.clone(),
                    CallCallee::Method { name } => name.clone(),
                    CallCallee::Dynamic { expr_id, .. } => format!("<dynamic:{expr_id}>"),
                };
                errors.push(TypeCheckErrorKind::OverloadAmbiguous(name, obligation.span).into());
                continue;
            }
            if let Some(prop_name) = called_property_name(obligation, def_id, property_sigs, &next)
            {
                // Preserve substitutions to reduce follow-on inference noise, but
                // reject property accessor call syntax in source.
                *unifier = next;
                errors.push(
                    TypeCheckErrorKind::PropertyCalledAsMethod(prop_name, obligation.span).into(),
                );
                continue;
            }
            *unifier = next;
            resolved_call_defs.insert(obligation.call_node, def_id);
        } else if let Some(err) = first_error {
            errors.push(err);
        } else {
            let name = match &obligation.callee {
                CallCallee::NamedFunction { name, .. } => name.clone(),
                CallCallee::Method { name } => name.clone(),
                CallCallee::Dynamic { expr_id, .. } => format!("<dynamic:{expr_id}>"),
            };
            errors.push(TypeCheckErrorKind::OverloadNoMatch(name, obligation.span).into());
        }
    }
    (errors, resolved_call_defs, deferred)
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
                    params: method.params.clone(),
                    ret_ty: method.ret_ty.clone(),
                    type_param_count: method.type_param_count,
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
        .map(|term| super::resolve_term(term, unifier))
        .map(super::peel_heap)?;
    let prop = super::lookup_property(property_sigs, &owner_ty, name)?;
    if prop.getter == Some(selected_def_id) || prop.setter == Some(selected_def_id) {
        Some(name.clone())
    } else {
        None
    }
}

fn try_solve_dyn_array_builtin_method(
    obligation: &CallObligation,
    method_name: &str,
    unifier: &mut TcUnifier,
) -> Option<Result<(), TypeCheckError>> {
    let receiver_term = obligation.receiver.as_ref()?;
    let receiver_ty = super::resolve_term(receiver_term, unifier);
    let Type::DynArray { elem_ty } = receiver_ty.peel_heap() else {
        return None;
    };

    let arity = obligation.arg_terms.len();
    let result = match method_name {
        "append" => {
            if arity != 1 {
                return Some(Err(TypeCheckErrorKind::ArgCountMismatch(
                    method_name.to_string(),
                    1,
                    arity,
                    obligation.span,
                )
                .into()));
            }
            let arg_ty = super::resolve_term(&obligation.arg_terms[0], unifier);
            if let Err(_) = super::assignability::solve_assignable(&arg_ty, &elem_ty, unifier) {
                return Some(Err(TypeCheckErrorKind::ArgTypeMismatch(
                    1,
                    (*elem_ty).clone(),
                    arg_ty,
                    obligation.span,
                )
                .into()));
            }
            unifier
                .unify(&obligation.ret_ty, &Type::Unit)
                .map_err(|err| super::constraint_checks::unify_error_to_diag(err, obligation.span))
        }
        "len" | "capacity" | "is_empty" => Err(TypeCheckErrorKind::PropertyCalledAsMethod(
            method_name.to_string(),
            obligation.span,
        )
        .into()),
        _ => return None,
    };

    Some(result)
}

fn try_solve_set_builtin_method(
    obligation: &CallObligation,
    method_name: &str,
    unifier: &mut TcUnifier,
) -> Option<Result<(), TypeCheckError>> {
    let receiver_term = obligation.receiver.as_ref()?;
    let receiver_ty = super::resolve_term(receiver_term, unifier);
    let Type::Set { elem_ty } = receiver_ty.peel_heap() else {
        return None;
    };

    if !super::is_unresolved(&elem_ty)
        && let Err(failure) = super::ensure_hashable(&elem_ty)
    {
        return Some(Err(TypeCheckErrorKind::TypeNotHashable(
            (*elem_ty).clone(),
            failure.path,
            failure.failing_ty,
            obligation.span,
        )
        .into()));
    }

    let arity = obligation.arg_terms.len();
    let result = match method_name {
        "insert" => {
            if arity != 1 {
                return Some(Err(TypeCheckErrorKind::ArgCountMismatch(
                    method_name.to_string(),
                    1,
                    arity,
                    obligation.span,
                )
                .into()));
            }
            let arg_ty = super::resolve_term(&obligation.arg_terms[0], unifier);
            if let Err(_) = super::assignability::solve_assignable(&arg_ty, &elem_ty, unifier) {
                return Some(Err(TypeCheckErrorKind::ArgTypeMismatch(
                    1,
                    (*elem_ty).clone(),
                    arg_ty,
                    obligation.span,
                )
                .into()));
            }
            unifier
                .unify(&obligation.ret_ty, &Type::Bool)
                .map_err(|err| super::constraint_checks::unify_error_to_diag(err, obligation.span))
        }
        "remove" | "contains" => {
            if arity != 1 {
                return Some(Err(TypeCheckErrorKind::ArgCountMismatch(
                    method_name.to_string(),
                    1,
                    arity,
                    obligation.span,
                )
                .into()));
            }
            let arg_ty = super::resolve_term(&obligation.arg_terms[0], unifier);
            if let Err(_) = super::assignability::solve_assignable(&arg_ty, &elem_ty, unifier) {
                return Some(Err(TypeCheckErrorKind::ArgTypeMismatch(
                    1,
                    (*elem_ty).clone(),
                    arg_ty,
                    obligation.span,
                )
                .into()));
            }
            unifier
                .unify(&obligation.ret_ty, &Type::Bool)
                .map_err(|err| super::constraint_checks::unify_error_to_diag(err, obligation.span))
        }
        "clear" => {
            if arity != 0 {
                return Some(Err(TypeCheckErrorKind::ArgCountMismatch(
                    method_name.to_string(),
                    0,
                    arity,
                    obligation.span,
                )
                .into()));
            }
            unifier
                .unify(&obligation.ret_ty, &Type::Unit)
                .map_err(|err| super::constraint_checks::unify_error_to_diag(err, obligation.span))
        }
        "len" | "capacity" | "is_empty" => Err(TypeCheckErrorKind::PropertyCalledAsMethod(
            method_name.to_string(),
            obligation.span,
        )
        .into()),
        _ => return None,
    };

    Some(result)
}

fn try_solve_map_builtin_method(
    obligation: &CallObligation,
    method_name: &str,
    unifier: &mut TcUnifier,
) -> Option<Result<(), TypeCheckError>> {
    let receiver_term = obligation.receiver.as_ref()?;
    let receiver_ty = super::resolve_term(receiver_term, unifier);
    let Type::Map { key_ty, value_ty } = receiver_ty.peel_heap() else {
        return None;
    };

    if !super::is_unresolved(&key_ty)
        && let Err(failure) = super::ensure_hashable(&key_ty)
    {
        return Some(Err(TypeCheckErrorKind::TypeNotHashable(
            (*key_ty).clone(),
            failure.path,
            failure.failing_ty,
            obligation.span,
        )
        .into()));
    }

    let arity = obligation.arg_terms.len();
    let result = match method_name {
        "insert" => {
            if arity != 2 {
                return Some(Err(TypeCheckErrorKind::ArgCountMismatch(
                    method_name.to_string(),
                    2,
                    arity,
                    obligation.span,
                )
                .into()));
            }
            let key_arg_ty = super::resolve_term(&obligation.arg_terms[0], unifier);
            if let Err(_) = super::assignability::solve_assignable(&key_arg_ty, &key_ty, unifier) {
                return Some(Err(TypeCheckErrorKind::ArgTypeMismatch(
                    1,
                    (*key_ty).clone(),
                    key_arg_ty,
                    obligation.span,
                )
                .into()));
            }
            let value_arg_ty = super::resolve_term(&obligation.arg_terms[1], unifier);
            if let Err(_) =
                super::assignability::solve_assignable(&value_arg_ty, &value_ty, unifier)
            {
                return Some(Err(TypeCheckErrorKind::ArgTypeMismatch(
                    2,
                    (*value_ty).clone(),
                    value_arg_ty,
                    obligation.span,
                )
                .into()));
            }
            unifier
                .unify(&obligation.ret_ty, &Type::Bool)
                .map_err(|err| super::constraint_checks::unify_error_to_diag(err, obligation.span))
        }
        "remove" | "contains_key" => {
            if arity != 1 {
                return Some(Err(TypeCheckErrorKind::ArgCountMismatch(
                    method_name.to_string(),
                    1,
                    arity,
                    obligation.span,
                )
                .into()));
            }
            let key_arg_ty = super::resolve_term(&obligation.arg_terms[0], unifier);
            if let Err(_) = super::assignability::solve_assignable(&key_arg_ty, &key_ty, unifier) {
                return Some(Err(TypeCheckErrorKind::ArgTypeMismatch(
                    1,
                    (*key_ty).clone(),
                    key_arg_ty,
                    obligation.span,
                )
                .into()));
            }
            unifier
                .unify(&obligation.ret_ty, &Type::Bool)
                .map_err(|err| super::constraint_checks::unify_error_to_diag(err, obligation.span))
        }
        "get" => {
            if arity != 1 {
                return Some(Err(TypeCheckErrorKind::ArgCountMismatch(
                    method_name.to_string(),
                    1,
                    arity,
                    obligation.span,
                )
                .into()));
            }
            let key_arg_ty = super::resolve_term(&obligation.arg_terms[0], unifier);
            if let Err(_) = super::assignability::solve_assignable(&key_arg_ty, &key_ty, unifier) {
                return Some(Err(TypeCheckErrorKind::ArgTypeMismatch(
                    1,
                    (*key_ty).clone(),
                    key_arg_ty,
                    obligation.span,
                )
                .into()));
            }
            if value_ty.needs_drop() && !super::is_unresolved(&value_ty) {
                return Some(Err(TypeCheckErrorKind::MapIndexValueNotCopySafe(
                    value_ty.as_ref().clone(),
                    obligation.span,
                )
                .into()));
            }
            let result_ty = Type::ErrorUnion {
                ok_ty: value_ty.clone(),
                err_tys: vec![super::map_key_not_found_type()],
            };
            unifier
                .unify(&obligation.ret_ty, &result_ty)
                .map_err(|err| super::constraint_checks::unify_error_to_diag(err, obligation.span))
        }
        "clear" => {
            if arity != 0 {
                return Some(Err(TypeCheckErrorKind::ArgCountMismatch(
                    method_name.to_string(),
                    0,
                    arity,
                    obligation.span,
                )
                .into()));
            }
            unifier
                .unify(&obligation.ret_ty, &Type::Unit)
                .map_err(|err| super::constraint_checks::unify_error_to_diag(err, obligation.span))
        }
        "len" | "capacity" | "is_empty" => Err(TypeCheckErrorKind::PropertyCalledAsMethod(
            method_name.to_string(),
            obligation.span,
        )
        .into()),
        _ => return None,
    };

    Some(result)
}

struct InstantiatedSig {
    params: Vec<Type>,
    ret_ty: Type,
    bound_terms: Vec<(String, Type)>,
}

fn instantiate_sig(sig: &CollectedCallableSig, unifier: &mut TcUnifier) -> InstantiatedSig {
    if sig.type_param_count == 0 {
        return InstantiatedSig {
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
    let ret_ty = subst_type_vars(&sig.ret_ty, &map);
    InstantiatedSig {
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
        let resolved = super::canonicalize_type(unifier.apply(term));
        let ty_name = match &resolved {
            Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name.as_str()),
            _ if super::is_unresolved(&resolved) => None,
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
