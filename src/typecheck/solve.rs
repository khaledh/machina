//! Pass 3 of the type checker: solve constraints and obligations.
//!
//! The solver executes in stages:
//! 1. solve hard equalities,
//! 2. opportunistically propagate assignable constraints for inference,
//! 3. discharge expression/call obligations,
//! 4. re-check assignability constraints for diagnostics,
//! 5. finalize node/def type substitutions.
//!
//! This split keeps inference productive while preserving precise
//! assignability/runtime-refinement behavior.

use std::collections::HashMap;
use std::collections::HashSet;

use crate::diag::Span;
use crate::frontend::ModuleId;
use crate::resolve::{DefId, DefKind, DefTable};
use crate::tree::NodeId;
use crate::tree::resolved::{BindPattern, BindPatternKind, MatchPattern, MatchPatternBinding};
use crate::typecheck::constraints::{
    CallCallee, CallObligation, Constraint, ConstraintReason, ExprObligation,
};
use crate::typecheck::engine::{
    CollectedCallableSig, CollectedPropertySig, CollectedTraitSig, TypecheckEngine,
};
use crate::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::typecheck::type_map::{resolve_type_def_with_args, resolve_type_expr};
use crate::typecheck::typesys::TypeVarKind;
use crate::typecheck::unify::{TcUnifier, TcUnifyError};
use crate::types::{TyVarId, Type, TypeAssignability, array_to_slice_assignable, type_assignable};

#[derive(Debug, Clone, Default)]
#[allow(dead_code)]
pub(crate) struct SolveOutput {
    pub(crate) resolved_node_types: HashMap<NodeId, Type>,
    pub(crate) resolved_def_types: HashMap<DefId, Type>,
    pub(crate) resolved_call_defs: HashMap<NodeId, DefId>,
    pub(crate) failed_constraints: usize,
}

/// Pass 3: solve constraints and obligations.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let constrain = engine.state().constrain.clone();
    let vars = std::mem::take(engine.type_vars_mut());
    let mut unifier = TcUnifier::new(vars);

    let mut failed_constraints = 0usize;
    let mut non_expr_errors = Vec::new();
    let mut deferred_expr_errors = Vec::new();
    let mut deferred_pattern_errors = Vec::new();
    // Stage A: strict equalities.
    for constraint in constrain
        .constraints
        .iter()
        .filter(|constraint| matches!(constraint, Constraint::Eq { .. }))
    {
        apply_constraint(
            constraint,
            &mut unifier,
            &mut failed_constraints,
            &mut deferred_expr_errors,
            &mut deferred_pattern_errors,
            &mut non_expr_errors,
        );
    }

    // Stage B: best-effort assignable inference to reduce unresolved vars
    // before semantic obligations are interpreted.
    for constraint in constrain
        .constraints
        .iter()
        .filter(|constraint| matches!(constraint, Constraint::Assignable { .. }))
    {
        apply_assignable_inference(constraint, &mut unifier);
    }

    // Pre-pass: apply pattern-driven unifications before expression/call
    // obligations so match-arm bindings have concrete types when arm bodies
    // are checked (e.g. field access and formatting on typed bindings).
    let _ = check_pattern_obligations(
        &constrain.pattern_obligations,
        &constrain.def_terms,
        &mut unifier,
        &engine.env().type_defs,
        &engine.env().type_symbols,
        &engine.context().def_table,
        &engine.context().def_owners,
        &engine.context().module,
    );

    // Stage C: expression obligations.
    let (mut expr_errors, mut covered_exprs) = check_expr_obligations(
        &constrain.expr_obligations,
        &constrain.def_terms,
        &mut unifier,
        &engine.context().def_table,
        &engine.env().type_defs,
        &engine.env().type_symbols,
        &engine.context().def_owners,
        &engine.env().property_sigs,
        &engine.env().trait_sigs,
        &constrain.var_trait_bounds,
    );
    let index_nodes = collect_index_nodes(&constrain.expr_obligations);
    let enum_payload_nodes = collect_enum_payload_nodes(&constrain.expr_obligations);

    // Stage D: call obligations (overload/generic resolution). Dynamic calls
    // may need a second pass when callee function types become known only
    // after other call constraints are solved.
    let mut call_errors = Vec::new();
    let mut resolved_call_defs = HashMap::new();
    let mut pending_calls = constrain.call_obligations.clone();
    while !pending_calls.is_empty() {
        let prior_pending = pending_calls.len();
        let (mut round_errors, round_resolved, deferred) = check_call_obligations(
            &pending_calls,
            &mut unifier,
            &engine.env().func_sigs,
            &engine.env().method_sigs,
            &engine.env().property_sigs,
            &engine.env().trait_sigs,
            &engine.env().trait_impls,
            &constrain.var_trait_bounds,
            &engine.context().def_table,
            &engine.context().def_owners,
        );
        call_errors.append(&mut round_errors);
        resolved_call_defs.extend(round_resolved);

        if deferred.is_empty() {
            break;
        }

        if deferred.len() == prior_pending {
            // No progress this round: keep legacy behavior for remaining
            // unresolved dynamic calls and let other obligations/late checks
            // decide whether diagnostics are needed.
            break;
        }

        pending_calls = deferred;
    }

    // Stage E: retry unresolved expression obligations that depend on
    // post-call type information (e.g. field projections over generic call
    // results). The first pass may see unresolved owner types and skip them.
    let retry_expr_obligations = constrain
        .expr_obligations
        .iter()
        .filter(|obligation| should_retry_post_call_expr_obligation(obligation, &unifier))
        .cloned()
        .collect::<Vec<_>>();
    if !retry_expr_obligations.is_empty() {
        let (mut retry_errors, retry_covered) = check_expr_obligations(
            &retry_expr_obligations,
            &constrain.def_terms,
            &mut unifier,
            &engine.context().def_table,
            &engine.env().type_defs,
            &engine.env().type_symbols,
            &engine.context().def_owners,
            &engine.env().property_sigs,
            &engine.env().trait_sigs,
            &constrain.var_trait_bounds,
        );
        expr_errors.append(&mut retry_errors);
        covered_exprs.extend(retry_covered);
    }

    // Stage F: final assignability check (diagnostics-producing).
    for constraint in constrain
        .constraints
        .iter()
        .filter(|constraint| matches!(constraint, Constraint::Assignable { .. }))
    {
        apply_constraint(
            constraint,
            &mut unifier,
            &mut failed_constraints,
            &mut deferred_expr_errors,
            &mut deferred_pattern_errors,
            &mut non_expr_errors,
        );
    }

    let covered_expr_spans = expr_errors
        .iter()
        .map(TypeCheckError::span)
        .collect::<Vec<_>>();

    for (expr_id, err, span) in deferred_expr_errors {
        if let Some(index_span) = index_nodes.get(&expr_id)
            && let Some(index_error) = remap_index_unify_error(&err, *index_span)
        {
            expr_errors.push(index_error);
            continue;
        }
        if let Some((variant, index, payload_span)) = enum_payload_nodes.get(&expr_id)
            && let Some(payload_error) =
                remap_enum_payload_unify_error(&err, variant, *index, *payload_span)
        {
            expr_errors.push(payload_error);
            continue;
        }
        if !covered_exprs.contains(&expr_id) && !covered_expr_spans.contains(&span) {
            expr_errors.push(unify_error_to_diag(err, span));
        }
    }

    // Stage G: pattern obligations and late unresolved-local checks.
    let (mut pattern_errors, covered_patterns) = check_pattern_obligations(
        &constrain.pattern_obligations,
        &constrain.def_terms,
        &mut unifier,
        &engine.env().type_defs,
        &engine.env().type_symbols,
        &engine.context().def_table,
        &engine.context().def_owners,
        &engine.context().module,
    );
    for (pattern_id, err, span) in deferred_pattern_errors {
        if !covered_patterns.contains(&pattern_id) {
            pattern_errors.push(unify_error_to_diag(err, span));
        }
    }

    let mut errors = Vec::new();
    errors.append(&mut call_errors);
    errors.extend(expr_errors);
    errors.extend(pattern_errors);
    errors.extend(non_expr_errors);

    // Apply local numeric defaulting after all inference/obligation solving.
    // Any still-unresolved integer-literal vars become i32.
    default_unresolved_int_vars(&mut unifier);

    let mut output = SolveOutput {
        resolved_node_types: HashMap::new(),
        resolved_def_types: HashMap::new(),
        resolved_call_defs,
        failed_constraints,
    };

    for (node_id, term) in &constrain.node_terms {
        output
            .resolved_node_types
            .insert(*node_id, canonicalize_type(unifier.apply(term)));
    }
    for (def_id, term) in &constrain.def_terms {
        output
            .resolved_def_types
            .insert(*def_id, canonicalize_type(unifier.apply(term)));
    }

    errors.extend(check_unresolved_local_infer_vars(
        &output.resolved_def_types,
        &constrain.constraints,
        &constrain.pattern_obligations,
        &engine.context().def_table,
        unifier.vars(),
        &errors,
    ));

    *engine.type_vars_mut() = unifier.vars().clone();
    engine.state_mut().solve = output;

    if errors.is_empty() {
        Ok(())
    } else {
        engine.state_mut().diags.extend(errors.clone());
        Err(errors)
    }
}

fn apply_assignable_inference(constraint: &Constraint, unifier: &mut TcUnifier) {
    if let Constraint::Assignable { from, to, .. } = constraint {
        let _ = solve_assignable(from, to, unifier);
    }
}

fn apply_constraint(
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
            &canonicalize_type(left.clone()),
            &canonicalize_type(right.clone()),
        ),
        Constraint::Assignable { from, to, .. } => solve_assignable(from, to, unifier),
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

fn solve_assignable(from: &Type, to: &Type, unifier: &mut TcUnifier) -> Result<(), TcUnifyError> {
    let from_raw = canonicalize_type(from.clone());
    let to_raw = canonicalize_type(to.clone());
    let from_applied = canonicalize_type(unifier.apply(&from_raw));
    let to_applied = canonicalize_type(unifier.apply(&to_raw));

    if let Some(result) = infer_array_to_slice_assignability(&from_applied, &to_applied, unifier) {
        return result;
    }
    if let Some(result) =
        infer_array_to_dyn_array_assignability(&from_applied, &to_applied, unifier)
    {
        return result;
    }
    if let Some(result) =
        infer_dyn_array_to_slice_assignability(&from_applied, &to_applied, unifier)
    {
        return result;
    }

    if !is_unresolved(&from_applied) && !is_unresolved(&to_applied) {
        return match type_assignable(&from_applied, &to_applied) {
            TypeAssignability::Incompatible
                if array_to_slice_assignable(&from_applied, &to_applied) =>
            {
                Ok(())
            }
            TypeAssignability::Incompatible if int_repr_compatible(&from_applied, &to_applied) => {
                Ok(())
            }
            TypeAssignability::Incompatible => {
                Err(TcUnifyError::Mismatch(to_applied, from_applied))
            }
            _ => Ok(()),
        };
    }

    unifier.unify(&erase_refinements(&from_raw), &erase_refinements(&to_raw))
}

fn infer_array_to_slice_assignability(
    from: &Type,
    to: &Type,
    unifier: &mut TcUnifier,
) -> Option<Result<(), TcUnifyError>> {
    let Type::Slice {
        elem_ty: slice_elem_ty,
    } = to
    else {
        return None;
    };
    let Some(array_item_ty) = from.array_item_type() else {
        return None;
    };
    Some(unifier.unify(&array_item_ty, slice_elem_ty))
}

fn infer_array_to_dyn_array_assignability(
    from: &Type,
    to: &Type,
    unifier: &mut TcUnifier,
) -> Option<Result<(), TcUnifyError>> {
    let Type::DynArray {
        elem_ty: dyn_elem_ty,
    } = to
    else {
        return None;
    };
    let Some(array_item_ty) = from.array_item_type() else {
        return None;
    };
    Some(unifier.unify(&array_item_ty, dyn_elem_ty))
}

fn infer_dyn_array_to_slice_assignability(
    from: &Type,
    to: &Type,
    unifier: &mut TcUnifier,
) -> Option<Result<(), TcUnifyError>> {
    let Type::DynArray {
        elem_ty: dyn_elem_ty,
    } = from
    else {
        return None;
    };
    let Type::Slice {
        elem_ty: slice_elem_ty,
    } = to
    else {
        return None;
    };
    Some(unifier.unify(dyn_elem_ty, slice_elem_ty))
}

fn assignability_rank(from: &Type, to: &Type) -> i32 {
    if is_unresolved(from) || is_unresolved(to) {
        return 0;
    }
    match type_assignable(from, to) {
        TypeAssignability::Exact => 3,
        TypeAssignability::IntLitToInt { .. } => 2,
        TypeAssignability::RefinedToInt | TypeAssignability::IntToRefined { .. } => 1,
        TypeAssignability::Incompatible => -1000,
    }
}

fn push_unique_join_variant(variants: &mut Vec<Type>, candidate: Type) {
    if !variants.iter().any(|existing| existing == &candidate) {
        variants.push(candidate);
    }
}

fn join_variant_rank(ty: &Type) -> u8 {
    match ty {
        Type::Unit => 0,
        Type::Int { .. } | Type::Bool | Type::Char | Type::String | Type::Range { .. } => 1,
        Type::Tuple { .. } | Type::Array { .. } | Type::DynArray { .. } | Type::Set { .. } => 2,
        Type::Struct { .. } | Type::Enum { .. } => 3,
        Type::Fn { .. } => 4,
        Type::Slice { .. } | Type::Ref { .. } | Type::Heap { .. } => 5,
        Type::Var(_) | Type::Unknown => 6,
        Type::ErrorUnion { .. } => 7,
    }
}

fn sort_join_variants_canonical(variants: &mut [Type]) {
    variants.sort_by(|left, right| {
        let left_key = (join_variant_rank(left), format!("{left:?}"));
        let right_key = (join_variant_rank(right), format!("{right:?}"));
        left_key.cmp(&right_key)
    });
}

fn collect_join_variants(ty: &Type, out: &mut Vec<Type>) {
    match ty {
        Type::ErrorUnion { ok_ty, err_tys } => {
            collect_join_variants(ok_ty, out);
            for err_ty in err_tys {
                collect_join_variants(err_ty, out);
            }
        }
        _ => push_unique_join_variant(out, ty.clone()),
    }
}

fn infer_join_type_from_arms(arms: &[Type]) -> Option<Type> {
    let mut variants = Vec::new();
    for arm_ty in arms {
        if is_unresolved(arm_ty) {
            continue;
        }
        collect_join_variants(arm_ty, &mut variants);
    }
    sort_join_variants_canonical(&mut variants);
    variants.dedup();
    if variants.is_empty() {
        return None;
    }
    if variants.len() == 1 {
        return variants.into_iter().next();
    }
    let ok_ty = variants[0].clone();
    let err_tys = variants.into_iter().skip(1).collect::<Vec<_>>();
    Some(Type::ErrorUnion {
        ok_ty: Box::new(ok_ty),
        err_tys,
    })
}

fn check_call_obligations(
    obligations: &[CallObligation],
    unifier: &mut TcUnifier,
    func_sigs: &HashMap<String, Vec<CollectedCallableSig>>,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
    property_sigs: &HashMap<String, HashMap<String, CollectedPropertySig>>,
    trait_sigs: &HashMap<String, crate::typecheck::engine::CollectedTraitSig>,
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
                let callee_ty = resolve_term(callee_term, unifier);
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
                        let arg_ty = resolve_term(arg_term, unifier);
                        if let Err(_) = solve_assignable(&arg_ty, &param.ty, unifier) {
                            errors.push(
                                TypeCheckErrorKind::ArgTypeMismatch(
                                    index + 1,
                                    canonicalize_type(unifier.apply(&param.ty)),
                                    canonicalize_type(unifier.apply(&arg_ty)),
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
                        &canonicalize_type(obligation.ret_ty.clone()),
                        &canonicalize_type((*ret_ty).clone()),
                    ) {
                        errors.push(unify_error_to_diag(err, obligation.span));
                    }
                } else if is_unresolved(&callee_ty) {
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
                let receiver_ty = obligation
                    .receiver
                    .as_ref()
                    .map(|term| resolve_term(term, unifier))
                    .map(peel_heap);
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
                !is_def_accessible_from(obligation.caller_def_id, sig.def_id, def_table, def_owners)
            })
            .count();
        candidates.retain(|sig| {
            is_def_accessible_from(obligation.caller_def_id, sig.def_id, def_table, def_owners)
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
                let arg_ty = resolve_term(arg_term, &trial);
                if let Err(_) = solve_assignable(&arg_ty, param_ty, &mut trial) {
                    first_error.get_or_insert_with(|| {
                        TypeCheckErrorKind::ArgTypeMismatch(
                            index + 1,
                            canonicalize_type(trial.apply(param_ty)),
                            canonicalize_type(trial.apply(&arg_ty)),
                            obligation.span,
                        )
                        .into()
                    });
                    failed = true;
                    break;
                }
                score += assignability_rank(
                    &resolve_term(arg_term, &trial),
                    &canonicalize_type(trial.apply(param_ty)),
                );
            }
            if failed {
                continue;
            }
            if let Err(err) = trial.unify(
                &canonicalize_type(obligation.ret_ty.clone()),
                &canonicalize_type(instantiated.ret_ty.clone()),
            ) {
                first_error.get_or_insert_with(|| unify_error_to_diag(err, obligation.span));
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

fn named_call_candidates<'a>(
    name: &str,
    arity: usize,
    func_sigs: &'a HashMap<String, Vec<CollectedCallableSig>>,
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
    trait_sigs: &HashMap<String, crate::typecheck::engine::CollectedTraitSig>,
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
        .map(|term| resolve_term(term, unifier))
        .map(peel_heap)?;
    let prop = lookup_property(property_sigs, &owner_ty, name)?;
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
    let receiver_ty = resolve_term(receiver_term, unifier);
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
            let arg_ty = resolve_term(&obligation.arg_terms[0], unifier);
            if let Err(_) = solve_assignable(&arg_ty, &elem_ty, unifier) {
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
                .map_err(|err| unify_error_to_diag(err, obligation.span))
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
    let receiver_ty = resolve_term(receiver_term, unifier);
    let Type::Set { elem_ty } = receiver_ty.peel_heap() else {
        return None;
    };

    if !is_unresolved(&elem_ty) && !is_supported_set_elem_type(&elem_ty) {
        return Some(Err(TypeCheckErrorKind::SetElementTypeUnsupported(
            (*elem_ty).clone(),
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
            let arg_ty = resolve_term(&obligation.arg_terms[0], unifier);
            if let Err(_) = solve_assignable(&arg_ty, &elem_ty, unifier) {
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
                .map_err(|err| unify_error_to_diag(err, obligation.span))
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
            let arg_ty = resolve_term(&obligation.arg_terms[0], unifier);
            if let Err(_) = solve_assignable(&arg_ty, &elem_ty, unifier) {
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
                .map_err(|err| unify_error_to_diag(err, obligation.span))
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
                .map_err(|err| unify_error_to_diag(err, obligation.span))
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
        let resolved = canonicalize_type(unifier.apply(term));
        let ty_name = match &resolved {
            Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name.as_str()),
            _ if is_unresolved(&resolved) => None,
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

fn check_expr_obligations(
    obligations: &[ExprObligation],
    def_terms: &HashMap<DefId, Type>,
    unifier: &mut TcUnifier,
    def_table: &DefTable,
    type_defs: &HashMap<String, Type>,
    type_symbols: &HashMap<String, DefId>,
    def_owners: &HashMap<DefId, ModuleId>,
    property_sigs: &HashMap<String, HashMap<String, CollectedPropertySig>>,
    trait_sigs: &HashMap<String, CollectedTraitSig>,
    var_trait_bounds: &HashMap<TyVarId, Vec<String>>,
) -> (Vec<TypeCheckError>, HashSet<NodeId>) {
    let mut errors = Vec::new();
    let mut covered_exprs = HashSet::new();

    for obligation in obligations {
        match obligation {
            ExprObligation::BinOp {
                expr_id,
                op,
                left,
                right,
                ..
            } => {
                let left_ty = resolve_term_for_diagnostics(left, unifier);
                let right_ty = resolve_term_for_diagnostics(right, unifier);
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
                        if let Some(err) = first_non_int_operand(
                            &left_ty,
                            &right_ty,
                            *expr_id,
                            op_span(obligation),
                        ) {
                            errors.push(err);
                            covered_exprs.insert(*expr_id);
                        }
                    }
                    crate::tree::resolved::BinaryOp::Eq
                    | crate::tree::resolved::BinaryOp::Ne
                    | crate::tree::resolved::BinaryOp::Lt
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
            }
            ExprObligation::UnaryOp {
                expr_id,
                op,
                operand,
                span,
                ..
            } => {
                let operand_ty = resolve_term_for_diagnostics(operand, unifier);
                match op {
                    crate::tree::resolved::UnaryOp::Neg
                    | crate::tree::resolved::UnaryOp::BitNot => {
                        if !is_int_like(&operand_ty) && !is_unresolved(&operand_ty) {
                            errors.push(
                                TypeCheckErrorKind::NegationOperandNotInt(operand_ty, *span).into(),
                            );
                            covered_exprs.insert(*expr_id);
                        }
                    }
                    crate::tree::resolved::UnaryOp::LogicalNot => {
                        if operand_ty != Type::Bool && !is_unresolved(&operand_ty) {
                            errors.push(
                                TypeCheckErrorKind::LogicalOperandNotBoolean(operand_ty, *span)
                                    .into(),
                            );
                            covered_exprs.insert(*expr_id);
                        }
                    }
                    crate::tree::resolved::UnaryOp::Try => {}
                }
            }
            ExprObligation::Join {
                expr_id,
                arms,
                result,
                span,
            } => {
                let resolved_arms = arms
                    .iter()
                    .map(|arm| resolve_term(arm, unifier))
                    .collect::<Vec<_>>();
                let mut result_ty = resolve_term(result, unifier);

                if is_unresolved(&result_ty)
                    && let Some(inferred_join) = infer_join_type_from_arms(
                        &resolved_arms
                            .iter()
                            .cloned()
                            .map(|ty| default_infer_ints_for_diagnostics(ty, unifier.vars()))
                            .collect::<Vec<_>>(),
                    )
                {
                    let _ = unifier.unify(result, &inferred_join);
                    result_ty = resolve_term(result, unifier);
                }

                let result_ty_diag =
                    default_infer_ints_for_diagnostics(result_ty.clone(), unifier.vars());
                if is_unresolved(&result_ty_diag) {
                    continue;
                }

                for (arm_term, arm_ty) in arms.iter().zip(resolved_arms.into_iter()) {
                    if is_unresolved(&arm_ty) {
                        let _ = unifier.unify(arm_term, &result_ty_diag);
                        continue;
                    }
                    let arm_ty_diag = default_infer_ints_for_diagnostics(arm_ty, unifier.vars());
                    if matches!(
                        type_assignable(&arm_ty_diag, &result_ty_diag),
                        TypeAssignability::Incompatible
                    ) {
                        if let Some(variants) = error_union_variant_names(&result_ty_diag) {
                            errors.push(
                                TypeCheckErrorKind::JoinArmNotInErrorUnion(
                                    variants,
                                    arm_ty_diag,
                                    *span,
                                )
                                .into(),
                            );
                        } else {
                            errors.push(
                                TypeCheckErrorKind::JoinArmTypeMismatch(
                                    result_ty_diag.clone(),
                                    arm_ty_diag,
                                    *span,
                                )
                                .into(),
                            );
                        }
                        covered_exprs.insert(*expr_id);
                        break;
                    }
                }
            }
            ExprObligation::Try {
                expr_id,
                operand,
                result,
                expected_return_ty,
                callable_def_id,
                span,
            } => {
                let operand_ty = resolve_term(operand, unifier);
                let operand_ty_for_diag =
                    default_infer_ints_for_diagnostics(operand_ty.clone(), unifier.vars());
                let Type::ErrorUnion { ok_ty, err_tys } = &operand_ty else {
                    if !is_unresolved(&operand_ty_for_diag) {
                        errors.push(
                            TypeCheckErrorKind::TryOperandNotErrorUnion(operand_ty_for_diag, *span)
                                .into(),
                        );
                        covered_exprs.insert(*expr_id);
                    }
                    continue;
                };

                let _ = unifier.unify(result, ok_ty);

                let mut return_ty = expected_return_ty
                    .as_ref()
                    .map(|term| resolve_term(term, unifier))
                    .unwrap_or(Type::Unknown);

                if is_unresolved(&return_ty)
                    && let Some(def_id) = callable_def_id
                    && let Some(callable_term) = def_terms.get(def_id)
                    && let Type::Fn { ret_ty, .. } = resolve_term(callable_term, unifier)
                {
                    return_ty = *ret_ty;
                }

                if is_unresolved(&return_ty)
                    && let Some(return_ty_term) = expected_return_ty
                {
                    let fresh_ok = Type::Var(unifier.vars_mut().fresh_infer_local());
                    let expected_union = Type::ErrorUnion {
                        ok_ty: Box::new(fresh_ok),
                        err_tys: err_tys.clone(),
                    };
                    let _ = unifier.unify(return_ty_term, &expected_union);
                    return_ty = resolve_term(return_ty_term, unifier);
                }

                if expected_return_ty.is_none() && callable_def_id.is_none() {
                    errors.push(TypeCheckErrorKind::TryOutsideFunction(*span).into());
                    covered_exprs.insert(*expr_id);
                    continue;
                }

                match &return_ty {
                    Type::ErrorUnion {
                        err_tys: return_err_tys,
                        ..
                    } => {
                        let mut missing = Vec::new();
                        for err_ty in err_tys {
                            let present = return_err_tys.iter().any(|return_err_ty| {
                                !matches!(
                                    type_assignable(err_ty, return_err_ty),
                                    TypeAssignability::Incompatible
                                )
                            });
                            if !present
                                && !is_unresolved(err_ty)
                                && !missing.iter().any(|seen| seen == err_ty)
                            {
                                missing.push(err_ty.clone());
                            }
                        }
                        if !missing.is_empty() {
                            let mut missing_names =
                                missing.iter().map(compact_type_name).collect::<Vec<_>>();
                            missing_names.sort();
                            missing_names.dedup();
                            let mut return_variant_names = std::iter::once(ok_ty.as_ref())
                                .chain(return_err_tys.iter())
                                .map(compact_type_name)
                                .collect::<Vec<_>>();
                            return_variant_names.sort();
                            return_variant_names.dedup();
                            errors.push(
                                TypeCheckErrorKind::TryErrorNotInReturn(
                                    missing_names,
                                    return_variant_names,
                                    *span,
                                )
                                .into(),
                            );
                            covered_exprs.insert(*expr_id);
                        }
                    }
                    ty if is_unresolved(ty) => {}
                    _ => {
                        errors.push(
                            TypeCheckErrorKind::TryReturnTypeNotErrorUnion(
                                return_ty.clone(),
                                *span,
                            )
                            .into(),
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
            }
            ExprObligation::ArrayIndex {
                expr_id,
                target,
                indices,
                index_nodes,
                index_spans,
                result,
                span,
            } => {
                let target_ty = resolve_term(target, unifier);
                let indexed_target_ty = peel_heap(target_ty.clone());
                if let Some((idx_i, bad_idx_ty)) = indices
                    .iter()
                    .map(|term| resolve_term(term, unifier))
                    .enumerate()
                    .find(|(_, ty)| !is_int_like(ty) && !is_unresolved(ty))
                {
                    let diag_span = index_spans.get(idx_i).copied().unwrap_or(*span);
                    errors.push(TypeCheckErrorKind::IndexTypeNotInt(bad_idx_ty, diag_span).into());
                    covered_exprs.insert(*expr_id);
                    if let Some(node_id) = index_nodes.get(idx_i) {
                        covered_exprs.insert(*node_id);
                    }
                    continue;
                }

                match &indexed_target_ty {
                    Type::Array { elem_ty, dims } => {
                        if indices.len() > dims.len() {
                            errors.push(
                                TypeCheckErrorKind::TooManyIndices(
                                    dims.len(),
                                    indices.len(),
                                    *span,
                                )
                                .into(),
                            );
                            covered_exprs.insert(*expr_id);
                            continue;
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
                        if indices.len() != 1 {
                            errors.push(
                                TypeCheckErrorKind::TooManyIndices(1, indices.len(), *span).into(),
                            );
                            covered_exprs.insert(*expr_id);
                            continue;
                        }
                        let _ = unifier.unify(result, elem_ty);
                    }
                    Type::DynArray { elem_ty } => {
                        if indices.len() != 1 {
                            errors.push(
                                TypeCheckErrorKind::TooManyIndices(1, indices.len(), *span).into(),
                            );
                            covered_exprs.insert(*expr_id);
                            continue;
                        }
                        let _ = unifier.unify(result, elem_ty);
                    }
                    Type::String => {
                        if indices.len() != 1 {
                            errors.push(
                                TypeCheckErrorKind::TooManyIndices(1, indices.len(), *span).into(),
                            );
                            covered_exprs.insert(*expr_id);
                            continue;
                        }
                        let _ = unifier.unify(result, &Type::uint(8));
                    }
                    ty if is_unresolved(ty) => {}
                    _ => {
                        errors.push(
                            TypeCheckErrorKind::InvalidIndexTargetType(indexed_target_ty, *span)
                                .into(),
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
            }
            ExprObligation::Slice {
                expr_id,
                target,
                start,
                end,
                result,
                span,
            } => {
                let target_ty = resolve_term(target, unifier);
                let sliced_target_ty = peel_heap(target_ty.clone());
                let mut bad_bound_ty = None;
                if let Some(start_ty) = start.as_ref().map(|term| resolve_term(term, unifier))
                    && !is_int_like(&start_ty)
                    && !is_unresolved(&start_ty)
                {
                    bad_bound_ty = Some(start_ty);
                }
                if bad_bound_ty.is_none()
                    && let Some(end_ty) = end.as_ref().map(|term| resolve_term(term, unifier))
                    && !is_int_like(&end_ty)
                    && !is_unresolved(&end_ty)
                {
                    bad_bound_ty = Some(end_ty);
                }
                if let Some(bound_ty) = bad_bound_ty {
                    errors.push(TypeCheckErrorKind::IndexTypeNotInt(bound_ty, *span).into());
                    covered_exprs.insert(*expr_id);
                    continue;
                }

                match &sliced_target_ty {
                    Type::Array { elem_ty, dims } => {
                        if dims.is_empty() {
                            errors.push(
                                TypeCheckErrorKind::SliceTargetZeroDimArray(
                                    sliced_target_ty,
                                    *span,
                                )
                                .into(),
                            );
                            covered_exprs.insert(*expr_id);
                            continue;
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
                    ty if is_unresolved(ty) => {}
                    _ => {
                        errors.push(
                            TypeCheckErrorKind::SliceTargetNotArrayOrString(
                                sliced_target_ty,
                                *span,
                            )
                            .into(),
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
            }
            ExprObligation::Range {
                expr_id,
                start,
                end,
                result,
                span,
            } => {
                let start_ty = resolve_term(start, unifier);
                if !is_int_like(&start_ty) && !is_unresolved(&start_ty) {
                    errors.push(TypeCheckErrorKind::IndexTypeNotInt(start_ty, *span).into());
                    covered_exprs.insert(*expr_id);
                    continue;
                }
                let end_ty = resolve_term(end, unifier);
                if !is_int_like(&end_ty) && !is_unresolved(&end_ty) {
                    errors.push(TypeCheckErrorKind::IndexTypeNotInt(end_ty, *span).into());
                    covered_exprs.insert(*expr_id);
                    continue;
                }
                let _ = unifier.unify(
                    result,
                    &Type::Range {
                        elem_ty: Box::new(Type::uint(64)),
                    },
                );
            }
            ExprObligation::SetElemType {
                expr_id,
                elem_ty,
                span,
            } => {
                let elem_ty = resolve_term(elem_ty, unifier);
                let elem_ty_for_diag =
                    default_infer_ints_for_diagnostics(elem_ty.clone(), unifier.vars());
                if is_unresolved(&elem_ty_for_diag) {
                    continue;
                }
                if !is_supported_set_elem_type(&elem_ty_for_diag) {
                    errors.push(
                        TypeCheckErrorKind::SetElementTypeUnsupported(elem_ty_for_diag, *span)
                            .into(),
                    );
                    covered_exprs.insert(*expr_id);
                }
            }
            ExprObligation::ForIter {
                stmt_id,
                iter,
                pattern,
                span,
            } => {
                let iter_ty = resolve_term(iter, unifier);
                let iter_ty_for_diag =
                    default_infer_ints_for_diagnostics(iter_ty.clone(), unifier.vars());
                if !is_iterable(&iter_ty_for_diag) && !is_unresolved(&iter_ty_for_diag) {
                    errors.push(
                        TypeCheckErrorKind::ForIterNotIterable(iter_ty_for_diag, *span).into(),
                    );
                    covered_exprs.insert(*stmt_id);
                    continue;
                }

                if let Some(elem_ty) = iterable_elem_type(&iter_ty) {
                    let _ = unifier.unify(pattern, &elem_ty);
                    let pattern_ty = resolve_term(pattern, unifier);
                    if matches!(
                        type_assignable(&pattern_ty, &elem_ty),
                        TypeAssignability::Incompatible
                    ) && !is_unresolved(&pattern_ty)
                        && !is_unresolved(&elem_ty)
                    {
                        errors.push(
                            TypeCheckErrorKind::DeclTypeMismatch(pattern_ty, elem_ty, *span).into(),
                        );
                        covered_exprs.insert(*stmt_id);
                    }
                }
            }
            ExprObligation::EnumVariantPayload {
                expr_id,
                enum_name,
                variant,
                payload,
                span,
                ..
            } => {
                let Some(Type::Enum { variants, .. }) = type_defs.get(enum_name) else {
                    continue;
                };
                let Some(expected_variant) = variants.iter().find(|v| v.name == *variant) else {
                    continue;
                };
                if payload.len() != expected_variant.payload.len() {
                    errors.push(
                        TypeCheckErrorKind::EnumVariantPayloadArityMismatch(
                            variant.clone(),
                            expected_variant.payload.len(),
                            payload.len(),
                            *span,
                        )
                        .into(),
                    );
                    covered_exprs.insert(*expr_id);
                    continue;
                }
                for (idx, (expected_ty, found_term)) in expected_variant
                    .payload
                    .iter()
                    .zip(payload.iter())
                    .enumerate()
                {
                    let found_ty = resolve_term(found_term, unifier);
                    if matches!(
                        type_assignable(&found_ty, expected_ty),
                        TypeAssignability::Incompatible
                    ) && !is_unresolved(&found_ty)
                    {
                        errors.push(
                            TypeCheckErrorKind::EnumVariantPayloadTypeMismatch(
                                variant.clone(),
                                idx,
                                expected_ty.clone(),
                                found_ty,
                                *span,
                            )
                            .into(),
                        );
                        covered_exprs.insert(*expr_id);
                        break;
                    }
                }
            }
            ExprObligation::StructConstruct {
                expr_id,
                type_name,
                caller_def_id,
                span,
            } => {
                if let Some(type_def_id) = type_def_id_for_nominal_name(type_name, type_symbols)
                    && is_external_opaque_access(*caller_def_id, type_def_id, def_table, def_owners)
                {
                    let diag_name = def_table
                        .lookup_def(type_def_id)
                        .map(|def| def.name.clone())
                        .unwrap_or_else(|| compact_nominal_name(type_name));
                    errors
                        .push(TypeCheckErrorKind::OpaqueTypeConstruction(diag_name, *span).into());
                    covered_exprs.insert(*expr_id);
                }
            }
            ExprObligation::StructUpdate {
                expr_id,
                target,
                fields,
                result,
                caller_def_id,
                span,
            } => {
                let target_ty = resolve_term(target, unifier);
                let target_ty_for_diag =
                    default_infer_ints_for_diagnostics(target_ty.clone(), unifier.vars());
                match &target_ty_for_diag {
                    Type::Struct {
                        name,
                        fields: struct_fields,
                    } => {
                        if let Some(type_def_id) = type_def_id_for_nominal_name(name, type_symbols)
                            && is_external_opaque_access(
                                *caller_def_id,
                                type_def_id,
                                def_table,
                                def_owners,
                            )
                        {
                            let diag_name = def_table
                                .lookup_def(type_def_id)
                                .map(|def| def.name.clone())
                                .unwrap_or_else(|| compact_nominal_name(name));
                            let field_name = fields
                                .first()
                                .map(|(field, _)| field.clone())
                                .unwrap_or_else(|| "<update>".to_string());
                            errors.push(
                                TypeCheckErrorKind::OpaqueFieldAccess(diag_name, field_name, *span)
                                    .into(),
                            );
                            covered_exprs.insert(*expr_id);
                            continue;
                        }
                        for (field_name, found_term) in fields {
                            let found_ty = resolve_term(found_term, unifier);
                            let Some(expected_field) =
                                struct_fields.iter().find(|field| field.name == *field_name)
                            else {
                                continue;
                            };
                            if let Err(_) = solve_assignable(&found_ty, &expected_field.ty, unifier)
                            {
                                let found_ty = canonicalize_type(unifier.apply(&found_ty));
                                if is_unresolved(&found_ty) {
                                    continue;
                                }
                                errors.push(
                                    TypeCheckErrorKind::StructFieldTypeMismatch(
                                        field_name.clone(),
                                        expected_field.ty.clone(),
                                        found_ty,
                                        *span,
                                    )
                                    .into(),
                                );
                                covered_exprs.insert(*expr_id);
                                break;
                            }
                        }
                        let _ = unifier.unify(result, &target_ty);
                    }
                    ty if is_unresolved(ty) => {}
                    _ => {
                        errors.push(
                            TypeCheckErrorKind::InvalidStructUpdateTarget(
                                target_ty_for_diag,
                                *span,
                            )
                            .into(),
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
            }
            ExprObligation::TupleField {
                expr_id,
                target,
                index,
                result,
                span,
            } => {
                let target_ty = resolve_term(target, unifier);
                let tuple_target_ty = peel_heap(target_ty.clone());
                match &tuple_target_ty {
                    Type::Tuple { field_tys } => {
                        if *index >= field_tys.len() {
                            errors.push(
                                TypeCheckErrorKind::TupleFieldOutOfBounds(
                                    field_tys.len(),
                                    *index,
                                    *span,
                                )
                                .into(),
                            );
                            covered_exprs.insert(*expr_id);
                            continue;
                        }
                        let _ = unifier.unify(result, &field_tys[*index]);
                    }
                    ty if is_unresolved(ty) => {}
                    _ => {
                        errors.push(
                            TypeCheckErrorKind::InvalidTupleFieldTarget(tuple_target_ty, *span)
                                .into(),
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
            }
            ExprObligation::StructField {
                expr_id,
                target,
                field,
                result,
                caller_def_id,
                span,
            } => {
                let target_ty = resolve_term(target, unifier);
                let owner_ty = peel_heap(target_ty.clone());
                match resolve_property_access(
                    &owner_ty,
                    field,
                    property_sigs,
                    trait_sigs,
                    var_trait_bounds,
                    *caller_def_id,
                    def_table,
                    def_owners,
                ) {
                    Ok(prop) => {
                        if !prop.readable {
                            errors.push(
                                TypeCheckErrorKind::PropertyNotReadable(field.clone(), *span)
                                    .into(),
                            );
                            covered_exprs.insert(*expr_id);
                            continue;
                        }
                        let _ = unifier.unify(result, &prop.ty);
                        continue;
                    }
                    Err(PropertyResolution::Ambiguous) => {
                        errors.push(
                            TypeCheckErrorKind::OverloadAmbiguous(field.clone(), *span).into(),
                        );
                        covered_exprs.insert(*expr_id);
                        continue;
                    }
                    Err(PropertyResolution::Private) => {
                        errors.push(
                            TypeCheckErrorKind::PropertyNotAccessible(field.clone(), *span).into(),
                        );
                        covered_exprs.insert(*expr_id);
                        continue;
                    }
                    Err(PropertyResolution::Missing) => {}
                }
                if field == "len" {
                    if is_len_target(&owner_ty) {
                        let _ = unifier.unify(result, &Type::uint(64));
                        continue;
                    }
                }
                if let Type::DynArray { .. } = &owner_ty {
                    match field.as_str() {
                        "capacity" => {
                            let _ = unifier.unify(result, &Type::uint(64));
                            continue;
                        }
                        "is_empty" => {
                            let _ = unifier.unify(result, &Type::Bool);
                            continue;
                        }
                        _ => {}
                    }
                }
                if let Type::Set { .. } = &owner_ty {
                    match field.as_str() {
                        "capacity" => {
                            let _ = unifier.unify(result, &Type::uint(64));
                            continue;
                        }
                        "is_empty" => {
                            let _ = unifier.unify(result, &Type::Bool);
                            continue;
                        }
                        _ => {}
                    }
                }
                match &owner_ty {
                    Type::Struct { name, fields } => {
                        if let Some(type_def_id) = type_def_id_for_nominal_name(name, type_symbols)
                            && is_external_opaque_access(
                                *caller_def_id,
                                type_def_id,
                                def_table,
                                def_owners,
                            )
                        {
                            let diag_name = def_table
                                .lookup_def(type_def_id)
                                .map(|def| def.name.clone())
                                .unwrap_or_else(|| compact_nominal_name(name));
                            errors.push(
                                TypeCheckErrorKind::OpaqueFieldAccess(
                                    diag_name,
                                    field.clone(),
                                    *span,
                                )
                                .into(),
                            );
                            covered_exprs.insert(*expr_id);
                            continue;
                        }
                        if let Some(struct_field) = fields.iter().find(|f| f.name == *field) {
                            let _ = unifier.unify(result, &struct_field.ty);
                        } else {
                            let _ = unifier.unify(result, &Type::Unknown);
                        }
                    }
                    ty if is_unresolved(ty) => {}
                    _ => {
                        errors.push(
                            TypeCheckErrorKind::InvalidStructFieldTarget(owner_ty, *span).into(),
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
            }
            ExprObligation::StructFieldAssign {
                stmt_id,
                assignee_expr_id: _,
                target,
                field,
                assignee,
                value,
                caller_def_id,
                span,
            } => {
                let target_ty = resolve_term(target, unifier);
                let value_ty = resolve_term(value, unifier);
                let owner_ty = peel_heap(target_ty.clone());

                match resolve_property_access(
                    &owner_ty,
                    field,
                    property_sigs,
                    trait_sigs,
                    var_trait_bounds,
                    *caller_def_id,
                    def_table,
                    def_owners,
                ) {
                    Ok(prop) => {
                        if !prop.writable {
                            errors.push(
                                TypeCheckErrorKind::PropertyNotWritable(field.clone(), *span)
                                    .into(),
                            );
                            covered_exprs.insert(*stmt_id);
                            continue;
                        }
                        let _ = unifier.unify(assignee, &prop.ty);
                        if let Err(_) = solve_assignable(&value_ty, &prop.ty, unifier) {
                            let value_ty = canonicalize_type(unifier.apply(&value_ty));
                            if is_unresolved(&value_ty) {
                                continue;
                            }
                            errors.push(
                                TypeCheckErrorKind::AssignTypeMismatch(prop.ty, value_ty, *span)
                                    .into(),
                            );
                            covered_exprs.insert(*stmt_id);
                        }
                        continue;
                    }
                    Err(PropertyResolution::Ambiguous) => {
                        errors.push(
                            TypeCheckErrorKind::OverloadAmbiguous(field.clone(), *span).into(),
                        );
                        covered_exprs.insert(*stmt_id);
                        continue;
                    }
                    Err(PropertyResolution::Private) => {
                        errors.push(
                            TypeCheckErrorKind::PropertyNotAccessible(field.clone(), *span).into(),
                        );
                        covered_exprs.insert(*stmt_id);
                        continue;
                    }
                    Err(PropertyResolution::Missing) => {}
                }

                if field == "len" && is_len_target(&owner_ty) {
                    errors
                        .push(TypeCheckErrorKind::PropertyNotWritable(field.clone(), *span).into());
                    covered_exprs.insert(*stmt_id);
                    continue;
                }
                if let Type::DynArray { .. } = &owner_ty
                    && matches!(field.as_str(), "capacity" | "is_empty")
                {
                    errors
                        .push(TypeCheckErrorKind::PropertyNotWritable(field.clone(), *span).into());
                    covered_exprs.insert(*stmt_id);
                    continue;
                }
                if let Type::Set { .. } = &owner_ty
                    && matches!(field.as_str(), "capacity" | "is_empty")
                {
                    errors
                        .push(TypeCheckErrorKind::PropertyNotWritable(field.clone(), *span).into());
                    covered_exprs.insert(*stmt_id);
                    continue;
                }

                match &owner_ty {
                    Type::Struct { name, fields } => {
                        if let Some(type_def_id) = type_def_id_for_nominal_name(name, type_symbols)
                            && is_external_opaque_access(
                                *caller_def_id,
                                type_def_id,
                                def_table,
                                def_owners,
                            )
                        {
                            let diag_name = def_table
                                .lookup_def(type_def_id)
                                .map(|def| def.name.clone())
                                .unwrap_or_else(|| compact_nominal_name(name));
                            errors.push(
                                TypeCheckErrorKind::OpaqueFieldAccess(
                                    diag_name,
                                    field.clone(),
                                    *span,
                                )
                                .into(),
                            );
                            covered_exprs.insert(*stmt_id);
                            continue;
                        }
                        if let Some(struct_field) = fields.iter().find(|f| f.name == *field) {
                            let _ = unifier.unify(assignee, &struct_field.ty);
                            if let Err(_) = solve_assignable(&value_ty, &struct_field.ty, unifier) {
                                let value_ty = canonicalize_type(unifier.apply(&value_ty));
                                if is_unresolved(&value_ty) {
                                    continue;
                                }
                                errors.push(
                                    TypeCheckErrorKind::AssignTypeMismatch(
                                        struct_field.ty.clone(),
                                        value_ty,
                                        *span,
                                    )
                                    .into(),
                                );
                                covered_exprs.insert(*stmt_id);
                            }
                        } else {
                            let _ = unifier.unify(assignee, &Type::Unknown);
                        }
                    }
                    ty if is_unresolved(ty) => {}
                    _ => {
                        errors.push(
                            TypeCheckErrorKind::InvalidStructFieldTarget(owner_ty, *span).into(),
                        );
                        covered_exprs.insert(*stmt_id);
                    }
                }
            }
        }
    }

    (errors, covered_exprs)
}

fn collect_index_nodes(obligations: &[ExprObligation]) -> HashMap<NodeId, Span> {
    let mut out = HashMap::new();
    for obligation in obligations {
        if let ExprObligation::ArrayIndex {
            index_nodes,
            index_spans,
            ..
        } = obligation
        {
            for (node_id, span) in index_nodes.iter().zip(index_spans.iter()) {
                out.insert(*node_id, *span);
            }
        }
    }
    out
}

fn collect_enum_payload_nodes(
    obligations: &[ExprObligation],
) -> HashMap<NodeId, (String, usize, Span)> {
    let mut out = HashMap::new();
    for obligation in obligations {
        if let ExprObligation::EnumVariantPayload {
            variant,
            payload_nodes,
            payload_spans,
            ..
        } = obligation
        {
            for (index, (node_id, span)) in
                payload_nodes.iter().zip(payload_spans.iter()).enumerate()
            {
                out.insert(*node_id, (variant.clone(), index, *span));
            }
        }
    }
    out
}

fn should_retry_post_call_expr_obligation(
    obligation: &ExprObligation,
    unifier: &TcUnifier,
) -> bool {
    match obligation {
        ExprObligation::Join { arms, result, .. } => {
            let result_ty = resolve_term(result, unifier);
            if is_unresolved(&result_ty) {
                return true;
            }
            arms.iter()
                .map(|arm| resolve_term(arm, unifier))
                .any(|arm_ty| is_unresolved(&arm_ty))
        }
        ExprObligation::StructField { target, result, .. } => {
            let owner_ty = peel_heap(resolve_term(target, unifier));
            let result_ty = resolve_term(result, unifier);
            is_unresolved(&owner_ty) || is_unresolved(&result_ty)
        }
        ExprObligation::TupleField { target, result, .. } => {
            let owner_ty = peel_heap(resolve_term(target, unifier));
            let result_ty = resolve_term(result, unifier);
            is_unresolved(&owner_ty) || is_unresolved(&result_ty)
        }
        ExprObligation::StructFieldAssign {
            target,
            assignee,
            value,
            ..
        } => {
            let owner_ty = peel_heap(resolve_term(target, unifier));
            let assignee_ty = resolve_term(assignee, unifier);
            let value_ty = resolve_term(value, unifier);
            is_unresolved(&owner_ty) || is_unresolved(&assignee_ty) || is_unresolved(&value_ty)
        }
        ExprObligation::SetElemType { elem_ty, .. } => {
            let elem_ty = resolve_term(elem_ty, unifier);
            is_unresolved(&elem_ty)
        }
        ExprObligation::Try {
            callable_def_id: _, ..
        } => true,
        _ => false,
    }
}

fn remap_index_unify_error(err: &TcUnifyError, span: Span) -> Option<TypeCheckError> {
    match err {
        TcUnifyError::Mismatch(left, right) => {
            if !is_int_like(left) && !is_unresolved(left) {
                return Some(TypeCheckErrorKind::IndexTypeNotInt(left.clone(), span).into());
            }
            if !is_int_like(right) && !is_unresolved(right) {
                return Some(TypeCheckErrorKind::IndexTypeNotInt(right.clone(), span).into());
            }
            None
        }
        TcUnifyError::CannotBindRigid(_, found) if !is_int_like(found) && !is_unresolved(found) => {
            Some(TypeCheckErrorKind::IndexTypeNotInt(found.clone(), span).into())
        }
        _ => None,
    }
}

fn remap_enum_payload_unify_error(
    err: &TcUnifyError,
    variant: &str,
    index: usize,
    span: Span,
) -> Option<TypeCheckError> {
    match err {
        TcUnifyError::Mismatch(expected, found)
            if !is_unresolved(expected) && !is_unresolved(found) =>
        {
            Some(
                TypeCheckErrorKind::EnumVariantPayloadTypeMismatch(
                    variant.to_string(),
                    index,
                    expected.clone(),
                    found.clone(),
                    span,
                )
                .into(),
            )
        }
        _ => None,
    }
}

fn check_unresolved_local_infer_vars(
    resolved_def_types: &HashMap<DefId, Type>,
    constraints: &[Constraint],
    pattern_obligations: &[crate::typecheck::constraints::PatternObligation],
    def_table: &DefTable,
    vars: &crate::typecheck::typesys::TypeVarStore,
    existing_errors: &[TypeCheckError],
) -> Vec<TypeCheckError> {
    let mut decl_spans = HashMap::new();
    for constraint in constraints {
        let reason = match constraint {
            Constraint::Eq { reason, .. } | Constraint::Assignable { reason, .. } => reason,
        };
        if let ConstraintReason::Decl(def_id, span) = reason {
            decl_spans.entry(*def_id).or_insert(*span);
        }
    }
    for obligation in pattern_obligations {
        if let crate::typecheck::constraints::PatternObligation::Bind { pattern, span, .. } =
            obligation
        {
            collect_pattern_bind_decl_spans(pattern, *span, &mut decl_spans);
        }
    }

    let mut errors = Vec::new();
    let blocking_spans = existing_errors
        .iter()
        .filter(|err| !matches!(err.kind(), TypeCheckErrorKind::UnknownType(_)))
        .map(TypeCheckError::span)
        .collect::<Vec<_>>();
    for (def_id, span) in decl_spans {
        let Some(def) = def_table.lookup_def(def_id) else {
            continue;
        };
        if !matches!(def.kind, DefKind::LocalVar { .. }) {
            continue;
        }
        let Some(ty) = resolved_def_types.get(&def_id) else {
            continue;
        };
        // Avoid cascaded "cannot infer" diagnostics when we already emitted a
        // concrete root-cause error touching this declaration site.
        if blocking_spans
            .iter()
            .any(|blocking| spans_overlap(*blocking, span) || spans_share_context(*blocking, span))
        {
            continue;
        }
        if has_unresolved_infer_var(ty, vars) {
            errors.push(TypeCheckErrorKind::UnknownType(span).into());
        }
    }
    errors
}

fn spans_overlap(a: Span, b: Span) -> bool {
    if a.start.offset > 0 && a.end.offset > 0 && b.start.offset > 0 && b.end.offset > 0 {
        return a.start.offset < b.end.offset && b.start.offset < a.end.offset;
    }

    // Fallback for synthesized/default spans where offsets may be zero.
    let a_starts_after_b = (a.start.line, a.start.column) >= (b.end.line, b.end.column);
    let b_starts_after_a = (b.start.line, b.start.column) >= (a.end.line, a.end.column);
    !(a_starts_after_b || b_starts_after_a)
}

fn spans_share_context(a: Span, b: Span) -> bool {
    // A frequent cascade pattern is: root-cause error under the RHS of a
    // declaration, followed by "cannot infer" on the LHS binding. They are
    // usually on the same source line but may not strictly overlap.
    (a.start.line <= b.start.line && b.start.line <= a.end.line)
        || (b.start.line <= a.start.line && a.start.line <= b.end.line)
}

fn collect_pattern_bind_decl_spans(
    pattern: &BindPattern,
    span: Span,
    out: &mut HashMap<DefId, Span>,
) {
    match &pattern.kind {
        BindPatternKind::Name { def_id, .. } => {
            out.entry(*def_id).or_insert(span);
        }
        BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
            for child in patterns {
                collect_pattern_bind_decl_spans(child, child.span, out);
            }
        }
        BindPatternKind::Struct { fields, .. } => {
            for field in fields {
                collect_pattern_bind_decl_spans(&field.pattern, field.span, out);
            }
        }
    }
}

fn has_unresolved_infer_var(ty: &Type, vars: &crate::typecheck::typesys::TypeVarStore) -> bool {
    ty.any(&|t| {
        matches!(t, Type::Var(var) if matches!(
            vars.kind(*var),
            Some(TypeVarKind::InferLocal) | Some(TypeVarKind::InferInt)
        ))
    })
}

fn check_pattern_obligations(
    obligations: &[crate::typecheck::constraints::PatternObligation],
    def_terms: &HashMap<DefId, Type>,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    type_symbols: &HashMap<String, DefId>,
    def_table: &DefTable,
    def_owners: &HashMap<DefId, ModuleId>,
    module: &crate::tree::resolved::Module,
) -> (Vec<TypeCheckError>, HashSet<NodeId>) {
    let mut errors = Vec::new();
    let mut covered = HashSet::new();

    for obligation in obligations {
        match obligation {
            crate::typecheck::constraints::PatternObligation::Bind {
                pattern_id,
                pattern,
                value_ty,
                caller_def_id,
                span,
            } => {
                let value_ty = resolve_term_for_diagnostics(value_ty, unifier);
                if let Some(error) = check_bind_pattern(
                    pattern,
                    &value_ty,
                    *span,
                    *caller_def_id,
                    type_symbols,
                    def_table,
                    def_owners,
                ) {
                    errors.push(error);
                    covered.insert(*pattern_id);
                }
            }
            crate::typecheck::constraints::PatternObligation::MatchArm {
                arm_id,
                pattern,
                scrutinee_ty,
                caller_def_id: _,
                ..
            } => {
                let scrutinee_ty = resolve_term(scrutinee_ty, unifier);
                bind_match_pattern_types(
                    pattern,
                    &scrutinee_ty,
                    def_terms,
                    unifier,
                    type_defs,
                    def_table,
                    module,
                    &mut errors,
                    &mut covered,
                    *arm_id,
                );
            }
        }
    }

    (errors, covered)
}

fn bind_match_pattern_types(
    pattern: &MatchPattern,
    scrutinee_ty: &Type,
    def_terms: &HashMap<DefId, Type>,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    def_table: &DefTable,
    module: &crate::tree::resolved::Module,
    errors: &mut Vec<TypeCheckError>,
    covered: &mut HashSet<NodeId>,
    pattern_id: NodeId,
) {
    match pattern {
        MatchPattern::Wildcard { .. } => {}
        MatchPattern::BoolLit { .. } => {
            if is_unresolved(scrutinee_ty) {
                let _ = unifier.unify(scrutinee_ty, &Type::Bool);
            }
        }
        MatchPattern::IntLit { .. } => {
            if is_unresolved(scrutinee_ty) {
                let int_var = Type::Var(unifier.vars_mut().fresh_infer_int());
                let _ = unifier.unify(scrutinee_ty, &int_var);
            }
        }
        MatchPattern::Binding { def_id, .. } => {
            if let Some(term) = def_terms.get(def_id) {
                let _ = unifier.unify(term, scrutinee_ty);
            }
        }
        MatchPattern::TypedBinding {
            id,
            def_id,
            ty_expr,
            span,
            ..
        } => {
            if let Ok(pat_ty) = resolve_type_expr(def_table, module, ty_expr) {
                let scrutinee_applied = unifier.apply(scrutinee_ty);
                if let Type::ErrorUnion { ok_ty, err_tys } = &scrutinee_applied {
                    let variants = std::iter::once(ok_ty.as_ref())
                        .chain(err_tys.iter())
                        .cloned()
                        .collect::<Vec<_>>();
                    let matches_union_variant = variants.iter().any(|variant_ty| {
                        !matches!(
                            type_assignable(variant_ty, &pat_ty),
                            TypeAssignability::Incompatible
                        )
                    });
                    if matches_union_variant {
                        if let Some(term) = def_terms.get(def_id) {
                            let _ = unifier.unify(term, &pat_ty);
                        }
                    } else if !is_unresolved(&pat_ty) {
                        let variant_names =
                            variants.iter().map(compact_type_name).collect::<Vec<_>>();
                        errors.push(
                            TypeCheckErrorKind::MatchTypedBindingTypeMismatch(
                                variant_names,
                                pat_ty.clone(),
                                *span,
                            )
                            .into(),
                        );
                        covered.insert(*id);
                        covered.insert(pattern_id);
                    }
                } else {
                    if let Some(term) = def_terms.get(def_id) {
                        let _ = unifier.unify(term, &pat_ty);
                    }
                }
            } else if let Some(term) = def_terms.get(def_id) {
                let _ = unifier.unify(term, scrutinee_ty);
            }
        }
        MatchPattern::Tuple { patterns, .. } => {
            if let Type::Tuple { field_tys } = scrutinee_ty {
                for (child, child_ty) in patterns.iter().zip(field_tys.iter()) {
                    bind_match_pattern_types(
                        child, child_ty, def_terms, unifier, type_defs, def_table, module, errors,
                        covered, pattern_id,
                    );
                }
            } else if is_unresolved(scrutinee_ty) {
                let inferred_fields = patterns
                    .iter()
                    .map(|_| Type::Var(unifier.vars_mut().fresh_infer_local()))
                    .collect::<Vec<_>>();
                let inferred_tuple = Type::Tuple {
                    field_tys: inferred_fields.clone(),
                };
                let _ = unifier.unify(scrutinee_ty, &inferred_tuple);
                for (child, child_ty) in patterns.iter().zip(inferred_fields.iter()) {
                    bind_match_pattern_types(
                        child, child_ty, def_terms, unifier, type_defs, def_table, module, errors,
                        covered, pattern_id,
                    );
                }
            }
        }
        MatchPattern::EnumVariant {
            id,
            enum_name,
            type_args,
            variant_name,
            bindings,
            ..
        } => {
            let owner_ty = peel_heap(scrutinee_ty.clone());
            let mut matched_variant = None;

            if let Type::Enum { variants, .. } = owner_ty.clone() {
                matched_variant = variants.iter().find(|v| v.name == *variant_name).cloned();
            } else if is_unresolved(&owner_ty) {
                let inferred_enum = resolve_pattern_enum_type(
                    *id, enum_name, type_args, type_defs, def_table, module, unifier,
                );
                if let Some(enum_ty) = inferred_enum {
                    let _ = unifier.unify(&owner_ty, &enum_ty);
                    if let Type::Enum { variants, .. } = unifier.apply(&owner_ty) {
                        matched_variant =
                            variants.iter().find(|v| v.name == *variant_name).cloned();
                    }
                }
            }

            if let Some(variant) = matched_variant {
                for (binding, payload_ty) in bindings.iter().zip(variant.payload.iter()) {
                    if let MatchPatternBinding::Named { def_id, .. } = binding
                        && let Some(term) = def_terms.get(def_id)
                    {
                        let _ = unifier.unify(term, payload_ty);
                    }
                }
            }
        }
    }
}

fn compact_type_name(ty: &Type) -> String {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name.clone(),
        Type::Int { signed, bits, .. } => format!("{}{}", if *signed { "i" } else { "u" }, bits),
        Type::Bool => "bool".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "string".to_string(),
        Type::Unit => "()".to_string(),
        _ => ty.to_string(),
    }
}

fn compact_nominal_name(name: &str) -> String {
    name.split('<').next().unwrap_or(name).trim().to_string()
}

fn type_def_id_for_nominal_name(
    name: &str,
    type_symbols: &HashMap<String, DefId>,
) -> Option<DefId> {
    let base = compact_nominal_name(name);
    type_symbols.get(&base).copied()
}

fn is_external_opaque_access(
    caller_def_id: Option<DefId>,
    owner_type_def_id: DefId,
    def_table: &DefTable,
    def_owners: &HashMap<DefId, ModuleId>,
) -> bool {
    if !def_table
        .lookup_def(owner_type_def_id)
        .is_some_and(|def| def.is_opaque())
    {
        return false;
    }
    let Some(caller_def_id) = caller_def_id else {
        return false;
    };
    let Some(caller_module_id) = def_owners.get(&caller_def_id) else {
        return false;
    };
    let Some(owner_module_id) = def_owners.get(&owner_type_def_id) else {
        return false;
    };
    caller_module_id != owner_module_id
}

fn is_def_accessible_from(
    caller_def_id: Option<DefId>,
    target_def_id: DefId,
    def_table: &DefTable,
    def_owners: &HashMap<DefId, ModuleId>,
) -> bool {
    if def_table
        .lookup_def(target_def_id)
        .is_some_and(|def| def.is_public())
    {
        return true;
    }

    let Some(caller_def_id) = caller_def_id else {
        return true;
    };
    let Some(caller_module_id) = def_owners.get(&caller_def_id) else {
        return true;
    };
    let Some(target_module_id) = def_owners.get(&target_def_id) else {
        return true;
    };
    caller_module_id == target_module_id
}

fn error_union_variant_names(ty: &Type) -> Option<Vec<String>> {
    let Type::ErrorUnion { ok_ty, err_tys } = ty else {
        return None;
    };
    let mut names = std::iter::once(ok_ty.as_ref())
        .chain(err_tys.iter())
        .map(compact_type_name)
        .collect::<Vec<_>>();
    names.sort();
    names.dedup();
    Some(names)
}

fn resolve_pattern_enum_type(
    pattern_id: NodeId,
    enum_name: &Option<String>,
    type_args: &[crate::tree::resolved::TypeExpr],
    type_defs: &HashMap<String, Type>,
    def_table: &DefTable,
    module: &crate::tree::resolved::Module,
    unifier: &mut TcUnifier,
) -> Option<Type> {
    if let Some(def_id) = def_table.lookup_node_def_id(pattern_id) {
        let type_def = module.type_def_by_id(def_id)?;
        let resolved_args = if type_args.is_empty() {
            type_def
                .type_params
                .iter()
                .map(|_| Type::Var(unifier.vars_mut().fresh_infer_local()))
                .collect::<Vec<_>>()
        } else {
            type_args
                .iter()
                .map(|arg| resolve_type_expr(def_table, module, arg))
                .collect::<Result<Vec<_>, _>>()
                .ok()?
        };

        let ty = resolve_type_def_with_args(def_table, module, def_id, &resolved_args).ok()?;
        return matches!(ty, Type::Enum { .. }).then_some(ty);
    }

    let enum_name = enum_name.as_ref()?;
    if let Some(ty) = type_defs.get(enum_name)
        && matches!(ty, Type::Enum { .. })
    {
        return Some(ty.clone());
    }

    // If resolver failed to attach a def id on the enum pattern, do not guess
    // via name lookup; leave this unresolved for later diagnostics.
    None
}

fn default_unresolved_int_vars(unifier: &mut TcUnifier) {
    let unresolved = unifier
        .vars()
        .unresolved_vars_by_kind(TypeVarKind::InferInt);
    for var in unresolved {
        unifier.vars_mut().bind(var, Type::sint(32));
    }
}

fn check_bind_pattern(
    pattern: &BindPattern,
    value_ty: &Type,
    span: Span,
    caller_def_id: Option<DefId>,
    type_symbols: &HashMap<String, DefId>,
    def_table: &DefTable,
    def_owners: &HashMap<DefId, ModuleId>,
) -> Option<TypeCheckError> {
    match &pattern.kind {
        BindPatternKind::Name { .. } => None,
        BindPatternKind::Tuple { patterns } => match value_ty {
            Type::Tuple { field_tys } => {
                if field_tys.len() != patterns.len() {
                    return Some(
                        TypeCheckErrorKind::TuplePatternLengthMismatch(
                            field_tys.len(),
                            patterns.len(),
                            span,
                        )
                        .into(),
                    );
                }
                for (child, child_ty) in patterns.iter().zip(field_tys.iter()) {
                    if let Some(error) = check_bind_pattern(
                        child,
                        child_ty,
                        child.span,
                        caller_def_id,
                        type_symbols,
                        def_table,
                        def_owners,
                    ) {
                        return Some(error);
                    }
                }
                None
            }
            ty if is_unresolved(ty) => None,
            _ => Some(
                TypeCheckErrorKind::PatternTypeMismatch(pattern.clone(), value_ty.clone(), span)
                    .into(),
            ),
        },
        BindPatternKind::Array { patterns } => match value_ty {
            Type::Array { elem_ty, dims } => {
                let expected_len = dims.first().copied().unwrap_or(0);
                if expected_len != patterns.len() {
                    return Some(
                        TypeCheckErrorKind::ArrayPatternLengthMismatch(
                            expected_len,
                            patterns.len(),
                            span,
                        )
                        .into(),
                    );
                }
                let child_ty = if dims.len() <= 1 {
                    (**elem_ty).clone()
                } else {
                    Type::Array {
                        elem_ty: elem_ty.clone(),
                        dims: dims[1..].to_vec(),
                    }
                };
                for child in patterns {
                    if let Some(error) = check_bind_pattern(
                        child,
                        &child_ty,
                        child.span,
                        caller_def_id,
                        type_symbols,
                        def_table,
                        def_owners,
                    ) {
                        return Some(error);
                    }
                }
                None
            }
            ty if is_unresolved(ty) => None,
            _ => Some(
                TypeCheckErrorKind::PatternTypeMismatch(pattern.clone(), value_ty.clone(), span)
                    .into(),
            ),
        },
        BindPatternKind::Struct { fields, .. } => match value_ty {
            Type::Struct {
                name,
                fields: struct_fields,
                ..
            } => {
                if let Some(type_def_id) = type_def_id_for_nominal_name(name, type_symbols)
                    && is_external_opaque_access(caller_def_id, type_def_id, def_table, def_owners)
                {
                    let diag_name = def_table
                        .lookup_def(type_def_id)
                        .map(|def| def.name.clone())
                        .unwrap_or_else(|| compact_nominal_name(name));
                    return Some(
                        TypeCheckErrorKind::OpaquePatternDestructure(diag_name, span).into(),
                    );
                }
                for field in fields {
                    let Some(struct_field) = struct_fields.iter().find(|f| f.name == field.name)
                    else {
                        continue;
                    };
                    if let Some(error) = check_bind_pattern(
                        &field.pattern,
                        &struct_field.ty,
                        field.span,
                        caller_def_id,
                        type_symbols,
                        def_table,
                        def_owners,
                    ) {
                        return Some(error);
                    }
                }
                None
            }
            ty if is_unresolved(ty) => None,
            _ => Some(
                TypeCheckErrorKind::PatternTypeMismatch(pattern.clone(), value_ty.clone(), span)
                    .into(),
            ),
        },
    }
}

fn op_span(obligation: &ExprObligation) -> Span {
    match obligation {
        ExprObligation::BinOp { span, .. }
        | ExprObligation::UnaryOp { span, .. }
        | ExprObligation::Join { span, .. }
        | ExprObligation::Try { span, .. }
        | ExprObligation::ArrayIndex { span, .. }
        | ExprObligation::Slice { span, .. }
        | ExprObligation::Range { span, .. }
        | ExprObligation::SetElemType { span, .. }
        | ExprObligation::ForIter { span, .. }
        | ExprObligation::EnumVariantPayload { span, .. }
        | ExprObligation::StructConstruct { span, .. }
        | ExprObligation::StructUpdate { span, .. }
        | ExprObligation::TupleField { span, .. }
        | ExprObligation::StructField { span, .. }
        | ExprObligation::StructFieldAssign { span, .. } => *span,
    }
}

fn first_non_int_operand(
    left: &Type,
    right: &Type,
    _expr_id: NodeId,
    span: Span,
) -> Option<TypeCheckError> {
    if !is_int_like(left) && !is_unresolved(left) {
        return Some(TypeCheckErrorKind::ArithOperandNotInt(left.clone(), span).into());
    }
    if !is_int_like(right) && !is_unresolved(right) {
        return Some(TypeCheckErrorKind::ArithOperandNotInt(right.clone(), span).into());
    }
    None
}

fn first_non_int_cmp_operand(left: &Type, right: &Type, span: Span) -> Option<TypeCheckError> {
    if !is_int_like(left) && !is_unresolved(left) {
        return Some(TypeCheckErrorKind::CmpOperandNotInt(left.clone(), span).into());
    }
    if !is_int_like(right) && !is_unresolved(right) {
        return Some(TypeCheckErrorKind::CmpOperandNotInt(right.clone(), span).into());
    }
    None
}

fn first_non_bool_operand(left: &Type, right: &Type, span: Span) -> Option<TypeCheckError> {
    if *left != Type::Bool && !is_unresolved(left) {
        return Some(TypeCheckErrorKind::LogicalOperandNotBoolean(left.clone(), span).into());
    }
    if *right != Type::Bool && !is_unresolved(right) {
        return Some(TypeCheckErrorKind::LogicalOperandNotBoolean(right.clone(), span).into());
    }
    None
}

fn resolve_term_for_diagnostics(ty: &Type, unifier: &TcUnifier) -> Type {
    default_infer_ints_for_diagnostics(resolve_term(ty, unifier), unifier.vars())
}

fn resolve_term(ty: &Type, unifier: &TcUnifier) -> Type {
    canonicalize_type(unifier.apply(ty))
}

fn default_infer_ints_for_diagnostics(
    ty: Type,
    vars: &crate::typecheck::typesys::TypeVarStore,
) -> Type {
    ty.map(&|t| match t {
        Type::Var(var) if matches!(vars.kind(var), Some(TypeVarKind::InferInt)) => Type::sint(32),
        other => other,
    })
}

fn is_int_like(ty: &Type) -> bool {
    matches!(ty, Type::Int { .. })
}

fn int_repr_compatible(from: &Type, to: &Type) -> bool {
    match (from, to) {
        (
            Type::Int {
                signed: from_signed,
                bits: from_bits,
                ..
            },
            Type::Int {
                signed: to_signed,
                bits: to_bits,
                ..
            },
        ) => from_signed == to_signed && from_bits == to_bits,
        _ => false,
    }
}

fn is_unresolved(ty: &Type) -> bool {
    ty.any(&|t| matches!(t, Type::Unknown | Type::Var(_)))
}

fn is_iterable(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Range { .. }
            | Type::Array { .. }
            | Type::DynArray { .. }
            | Type::Slice { .. }
            | Type::String
    )
}

fn is_len_target(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Array { .. }
            | Type::DynArray { .. }
            | Type::Set { .. }
            | Type::Slice { .. }
            | Type::String
    )
}

fn is_supported_set_elem_type(ty: &Type) -> bool {
    matches!(ty, Type::Int { .. } | Type::Bool | Type::Char)
}

fn peel_heap(ty: Type) -> Type {
    ty.peel_heap()
}

#[derive(Debug, Clone)]
struct ResolvedPropertyAccess {
    ty: Type,
    readable: bool,
    writable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PropertyResolution {
    Missing,
    Ambiguous,
    Private,
}

fn property_owner_name(ty: &Type) -> Option<&str> {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name.as_str()),
        Type::String => Some("string"),
        _ => None,
    }
}

fn lookup_property<'a>(
    property_sigs: &'a HashMap<String, HashMap<String, CollectedPropertySig>>,
    owner_ty: &Type,
    field: &str,
) -> Option<&'a CollectedPropertySig> {
    let owner = property_owner_name(owner_ty)?;
    property_sigs.get(owner).and_then(|props| props.get(field))
}

fn resolve_property_access(
    owner_ty: &Type,
    field: &str,
    property_sigs: &HashMap<String, HashMap<String, CollectedPropertySig>>,
    trait_sigs: &HashMap<String, CollectedTraitSig>,
    var_trait_bounds: &HashMap<TyVarId, Vec<String>>,
    caller_def_id: Option<DefId>,
    def_table: &DefTable,
    def_owners: &HashMap<DefId, ModuleId>,
) -> Result<ResolvedPropertyAccess, PropertyResolution> {
    if let Some(prop) = lookup_property(property_sigs, owner_ty, field) {
        let readable = prop.getter.is_some_and(|def_id| {
            is_def_accessible_from(caller_def_id, def_id, def_table, def_owners)
        });
        let writable = prop.setter.is_some_and(|def_id| {
            is_def_accessible_from(caller_def_id, def_id, def_table, def_owners)
        });
        if (prop.getter.is_some() || prop.setter.is_some()) && !readable && !writable {
            return Err(PropertyResolution::Private);
        }
        return Ok(ResolvedPropertyAccess {
            ty: prop.ty.clone(),
            readable,
            writable,
        });
    }

    let Type::Var(var) = owner_ty else {
        return Err(PropertyResolution::Missing);
    };

    let mut matched = None;
    for trait_name in var_trait_bounds
        .get(var)
        .into_iter()
        .flat_map(|names| names.iter())
    {
        let Some(trait_sig) = trait_sigs.get(trait_name) else {
            continue;
        };
        let Some(prop) = trait_sig.properties.get(field) else {
            continue;
        };

        if matched.is_some() {
            return Err(PropertyResolution::Ambiguous);
        }
        matched = Some(ResolvedPropertyAccess {
            ty: prop.ty.clone(),
            readable: prop.has_get,
            writable: prop.has_set,
        });
    }

    matched.ok_or(PropertyResolution::Missing)
}

fn iterable_elem_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Range { elem_ty } => Some((**elem_ty).clone()),
        Type::Array { elem_ty, .. } => Some((**elem_ty).clone()),
        Type::DynArray { elem_ty } => Some((**elem_ty).clone()),
        Type::Slice { elem_ty } => Some((**elem_ty).clone()),
        Type::String => Some(Type::Char),
        _ => None,
    }
}

fn canonicalize_type(ty: Type) -> Type {
    ty.map(&|t| match t {
        // Flatten nested arrays.
        Type::Array { elem_ty, dims } => match *elem_ty {
            Type::Array {
                elem_ty: inner_elem,
                dims: inner_dims,
            } => {
                let mut merged = dims;
                merged.extend(inner_dims);
                Type::Array {
                    elem_ty: inner_elem,
                    dims: merged,
                }
            }
            other => Type::Array {
                elem_ty: Box::new(other),
                dims,
            },
        },
        other => other,
    })
}

fn erase_refinements(ty: &Type) -> Type {
    ty.map_cloned(&|t| match t {
        Type::Int { signed, bits, .. } => Type::Int {
            signed,
            bits,
            bounds: None,
            nonzero: false,
        },
        other => other,
    })
}

fn unify_error_to_diag(err: TcUnifyError, span: Span) -> TypeCheckError {
    match err {
        TcUnifyError::Mismatch(expected, found) => {
            TypeCheckErrorKind::DeclTypeMismatch(expected, found, span).into()
        }
        TcUnifyError::CannotBindRigid(var, found) => {
            TypeCheckErrorKind::DeclTypeMismatch(Type::Var(var), found, span).into()
        }
        TcUnifyError::OccursCheckFailed(_, _) => TypeCheckErrorKind::UnknownType(span).into(),
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
                    .map(compact_type_name)
                    .collect::<Vec<_>>();
                variants.sort();
                variants.dedup();
                return TypeCheckErrorKind::ReturnNotInErrorUnion(variants, found, span).into();
            }
            TypeCheckErrorKind::ReturnTypeMismatch(expected, found, span).into()
        }
        TcUnifyError::CannotBindRigid(var, found) => {
            TypeCheckErrorKind::ReturnTypeMismatch(Type::Var(var), found, span).into()
        }
        TcUnifyError::OccursCheckFailed(_, _) => TypeCheckErrorKind::UnknownType(span).into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::ParsedContext;
    use crate::lexer::{LexError, Lexer, Token};
    use crate::parse::Parser;
    use crate::resolve::resolve;
    use crate::typecheck::{collect, constraints, engine::TypecheckEngine};

    fn resolve_source(source: &str) -> crate::context::ResolvedContext {
        let lexer = Lexer::new(source);
        let tokens = lexer
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .expect("Failed to tokenize");
        let mut parser = Parser::new(&tokens);
        let module = parser.parse().expect("Failed to parse");
        let id_gen = parser.into_id_gen();
        let ast_context = ParsedContext::new(module, id_gen);
        resolve(ast_context).expect("Failed to resolve")
    }

    #[test]
    fn test_solver_resolves_basic_local_types() {
        let source = r#"
            fn test() -> u64 {
                let x = 1;
                x
            }
        "#;

        let resolved = resolve_source(source);
        let mut engine = TypecheckEngine::new(resolved);
        collect::run(&mut engine).expect("collect pass failed");
        constraints::run(&mut engine).expect("constrain pass failed");
        run(&mut engine).expect("solve pass failed");

        assert!(!engine.state().solve.resolved_node_types.is_empty());
        assert!(!engine.state().solve.resolved_def_types.is_empty());
        assert_eq!(engine.state().solve.failed_constraints, 0);
        assert!(
            engine
                .state()
                .solve
                .resolved_def_types
                .values()
                .any(|ty| *ty == Type::uint(64))
        );
    }
}
