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

mod access_utils;
mod assignability;
mod calls;
mod collections;
mod constraint_checks;
mod control;
mod diag_utils;
mod expr_ops;
mod index;
mod joins;
mod linear;
mod nominal;
mod patterns;
mod term_utils;

#[cfg(test)]
pub(crate) fn test_solve_assignable(
    from: &Type,
    to: &Type,
    unifier: &mut TcUnifier,
) -> Result<(), TcUnifyError> {
    assignability::solve_assignable(from, to, unifier)
}

use std::collections::HashMap;
use std::collections::HashSet;

use crate::core::ast::NodeId;
use crate::core::ast::{BindPattern, BindPatternKind};
use crate::core::capsule::ModuleId;
use crate::core::context::ResolvedContext;
use crate::core::diag::Span;
use crate::core::plans::{
    ProtocolForPlanError, diagnose_protocol_iterable_type, plan_for_iterable_type_with_methods,
};
use crate::core::resolve::{DefId, DefKind, DefTable};
use crate::core::typecheck::capability::ensure_hashable;
use crate::core::typecheck::constraints::{
    CallObligation, ConstrainOutput, Constraint, ConstraintReason, ExprObligation,
    PatternObligation,
};
use crate::core::typecheck::engine::{
    CollectedCallableSig, CollectedPropertySig, CollectedTraitSig, TypecheckEngine,
};
use crate::core::typecheck::errors::{TEK, TypeCheckError};
use crate::core::typecheck::property_access;
use crate::core::typecheck::typesys::{TypeVarKind, TypeVarStore};
use crate::core::typecheck::unify::{TcUnifier, TcUnifyError};
use crate::core::types::{TyVarId, Type};

#[derive(Debug, Clone, Default)]
#[allow(dead_code)]
pub(crate) struct SolveOutput {
    pub(crate) resolved_node_types: HashMap<NodeId, Type>,
    pub(crate) resolved_def_types: HashMap<DefId, Type>,
    pub(crate) resolved_call_defs: HashMap<NodeId, DefId>,
    pub(crate) failed_constraints: usize,
}

type DeferredExprError = (NodeId, TcUnifyError, Span);
type DeferredPatternError = (NodeId, TcUnifyError, Span);

/// Pass 3: solve constraints and obligations.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let constrain = engine.state().constrain.clone();
    let vars = std::mem::take(engine.type_vars_mut());
    let mut unifier = TcUnifier::new(vars);

    let mut failed_constraints = 0usize;
    let mut non_expr_errors = Vec::new();
    let mut deferred_expr_errors = Vec::new();
    let mut deferred_pattern_errors = Vec::new();
    solve_strict_equalities(
        &constrain,
        &mut unifier,
        &mut failed_constraints,
        &mut deferred_expr_errors,
        &mut deferred_pattern_errors,
        &mut non_expr_errors,
    );
    apply_assignable_inference_pass(&constrain, &mut unifier);
    prepass_pattern_obligations(&constrain, &mut unifier, engine);

    let (mut expr_errors, mut covered_exprs, index_nodes, enum_payload_nodes) =
        solve_expr_stage(&constrain, &mut unifier, engine);
    let (mut call_errors, mut resolved_call_defs, unresolved_calls) =
        solve_call_stage(&constrain, &mut unifier, engine);

    retry_expr_stage(
        &constrain,
        &mut unifier,
        engine,
        &mut expr_errors,
        &mut covered_exprs,
    );
    apply_assignable_inference_pass(&constrain, &mut unifier);
    prepass_pattern_obligations(&constrain, &mut unifier, engine);

    retry_call_stage(
        unresolved_calls,
        &constrain,
        &mut unifier,
        engine,
        &mut call_errors,
        &mut resolved_call_defs,
        &mut expr_errors,
        &mut covered_exprs,
    );

    apply_final_assignability(
        &constrain,
        &mut unifier,
        &mut failed_constraints,
        &mut deferred_expr_errors,
        &mut deferred_pattern_errors,
        &mut non_expr_errors,
    );

    remap_deferred_expr_errors(
        deferred_expr_errors,
        &mut expr_errors,
        &covered_exprs,
        &index_nodes,
        &enum_payload_nodes,
    );

    let pattern_errors =
        solve_pattern_stage(&constrain, &mut unifier, engine, deferred_pattern_errors);

    let mut errors = Vec::new();
    errors.append(&mut call_errors);
    errors.extend(expr_errors);
    errors.extend(pattern_errors);
    errors.extend(non_expr_errors);

    // Apply local numeric defaulting after all inference/obligation solving.
    // Any still-unresolved integer-literal vars become i32.
    default_unresolved_int_vars(&mut unifier);

    let output = build_solve_output(&constrain, &unifier, resolved_call_defs, failed_constraints);

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

fn solve_strict_equalities(
    constrain: &ConstrainOutput,
    unifier: &mut TcUnifier,
    failed_constraints: &mut usize,
    deferred_expr_errors: &mut Vec<DeferredExprError>,
    deferred_pattern_errors: &mut Vec<DeferredPatternError>,
    non_expr_errors: &mut Vec<TypeCheckError>,
) {
    for constraint in constrain
        .constraints
        .iter()
        .filter(|constraint| matches!(constraint, Constraint::Eq { .. }))
    {
        constraint_checks::apply_constraint(
            constraint,
            unifier,
            failed_constraints,
            deferred_expr_errors,
            deferred_pattern_errors,
            non_expr_errors,
        );
    }
}

fn apply_assignable_inference_pass(constrain: &ConstrainOutput, unifier: &mut TcUnifier) {
    for constraint in constrain
        .constraints
        .iter()
        .filter(|constraint| matches!(constraint, Constraint::Assignable { .. }))
    {
        constraint_checks::apply_assignable_inference(constraint, unifier);
    }
}

fn prepass_pattern_obligations(
    constrain: &ConstrainOutput,
    unifier: &mut TcUnifier,
    engine: &TypecheckEngine,
) {
    let _ = patterns::check_pattern_obligations(
        &constrain.pattern_obligations,
        &constrain.def_terms,
        unifier,
        &engine.env().type_defs,
        &engine.env().type_symbols,
        &engine.context().def_table,
        &engine.context().def_owners,
        engine.context(),
        engine.is_partial_mode(),
    );
}

fn solve_expr_stage(
    constrain: &ConstrainOutput,
    unifier: &mut TcUnifier,
    engine: &TypecheckEngine,
) -> (
    Vec<TypeCheckError>,
    HashSet<NodeId>,
    HashMap<NodeId, Span>,
    HashMap<NodeId, (String, usize, Span)>,
) {
    let (expr_errors, covered_exprs) = check_expr_obligations(
        &constrain.expr_obligations,
        &constrain.def_terms,
        unifier,
        &engine.context().def_table,
        engine.context(),
        &engine.env().type_defs,
        &engine.env().type_symbols,
        &engine.context().def_owners,
        &engine.context().linear_index,
        &engine.env().property_sigs,
        &engine.env().method_sigs,
        &engine.env().trait_sigs,
        &constrain.var_trait_bounds,
    );
    let index_nodes = collect_index_nodes(&constrain.expr_obligations);
    let enum_payload_nodes = collect_enum_payload_nodes(&constrain.expr_obligations);
    (expr_errors, covered_exprs, index_nodes, enum_payload_nodes)
}

fn solve_call_stage(
    constrain: &ConstrainOutput,
    unifier: &mut TcUnifier,
    engine: &TypecheckEngine,
) -> (
    Vec<TypeCheckError>,
    HashMap<NodeId, DefId>,
    Vec<CallObligation>,
) {
    let mut call_errors = Vec::new();
    let mut resolved_call_defs = HashMap::new();
    let mut pending_calls = constrain.call_obligations.clone();
    let mut unresolved_calls = Vec::new();
    while !pending_calls.is_empty() {
        let prior_pending = pending_calls.len();
        let (mut round_errors, round_resolved, deferred) = calls::check_call_obligations(
            &pending_calls,
            unifier,
            &engine.env().func_sigs,
            &engine.env().method_sigs,
            &engine.env().property_sigs,
            &engine.env().trait_sigs,
            &engine.env().trait_impls,
            &constrain.var_trait_bounds,
            &engine.context().def_table,
            &engine.context().def_owners,
            true,
        );
        call_errors.append(&mut round_errors);
        resolved_call_defs.extend(round_resolved);

        if deferred.is_empty() {
            unresolved_calls.clear();
            break;
        }
        if deferred.len() == prior_pending {
            // No progress this round: keep legacy behavior for remaining
            // unresolved dynamic calls and let other obligations/late checks
            // decide whether diagnostics are needed.
            unresolved_calls = deferred;
            break;
        }
        pending_calls = deferred;
    }
    (call_errors, resolved_call_defs, unresolved_calls)
}

fn retry_expr_stage(
    constrain: &ConstrainOutput,
    unifier: &mut TcUnifier,
    engine: &TypecheckEngine,
    expr_errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) {
    let retry_expr_obligations = constrain
        .expr_obligations
        .iter()
        .filter(|obligation| should_retry_post_call_expr_obligation(obligation, unifier))
        .cloned()
        .collect::<Vec<_>>();
    if retry_expr_obligations.is_empty() {
        return;
    }

    let (mut retry_errors, retry_covered) = check_expr_obligations(
        &retry_expr_obligations,
        &constrain.def_terms,
        unifier,
        &engine.context().def_table,
        engine.context(),
        &engine.env().type_defs,
        &engine.env().type_symbols,
        &engine.context().def_owners,
        &engine.context().linear_index,
        &engine.env().property_sigs,
        &engine.env().method_sigs,
        &engine.env().trait_sigs,
        &constrain.var_trait_bounds,
    );
    expr_errors.append(&mut retry_errors);
    covered_exprs.extend(retry_covered);
}

fn retry_call_stage(
    mut pending_calls: Vec<CallObligation>,
    constrain: &ConstrainOutput,
    unifier: &mut TcUnifier,
    engine: &TypecheckEngine,
    call_errors: &mut Vec<TypeCheckError>,
    resolved_call_defs: &mut HashMap<NodeId, DefId>,
    expr_errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) {
    while !pending_calls.is_empty() {
        let prior_pending = pending_calls.len();
        let prior_call_state = call_state_signature(&pending_calls, unifier);
        let (mut retry_call_errors, retry_resolved, deferred) = calls::check_call_obligations(
            &pending_calls,
            unifier,
            &engine.env().func_sigs,
            &engine.env().method_sigs,
            &engine.env().property_sigs,
            &engine.env().trait_sigs,
            &engine.env().trait_impls,
            &constrain.var_trait_bounds,
            &engine.context().def_table,
            &engine.context().def_owners,
            true,
        );
        call_errors.append(&mut retry_call_errors);
        resolved_call_defs.extend(retry_resolved);

        prepass_pattern_obligations(constrain, unifier, engine);
        retry_expr_stage(constrain, unifier, engine, expr_errors, covered_exprs);
        apply_assignable_inference_pass(constrain, unifier);

        let next_call_state = call_state_signature(&deferred, unifier);
        let made_progress = next_call_state != prior_call_state;

        if deferred.is_empty() {
            break;
        }
        if deferred.len() == prior_pending && !made_progress {
            let (mut final_call_errors, final_resolved, _final_deferred) =
                calls::check_call_obligations(
                    &deferred,
                    unifier,
                    &engine.env().func_sigs,
                    &engine.env().method_sigs,
                    &engine.env().property_sigs,
                    &engine.env().trait_sigs,
                    &engine.env().trait_impls,
                    &constrain.var_trait_bounds,
                    &engine.context().def_table,
                    &engine.context().def_owners,
                    false,
                );
            call_errors.append(&mut final_call_errors);
            resolved_call_defs.extend(final_resolved);
            break;
        }
        pending_calls = deferred;
    }
}

fn call_state_signature(
    obligations: &[CallObligation],
    unifier: &TcUnifier,
) -> Vec<(NodeId, Option<Type>, Vec<Type>, Type)> {
    obligations
        .iter()
        .map(|obligation| {
            let receiver_ty = obligation
                .receiver
                .as_ref()
                .map(|term| term_utils::canonicalize_type(term_utils::resolve_term(term, unifier)));
            let arg_tys = obligation
                .arg_terms
                .iter()
                .map(|term| term_utils::canonicalize_type(term_utils::resolve_term(term, unifier)))
                .collect::<Vec<_>>();
            let ret_ty = term_utils::canonicalize_type(term_utils::resolve_term(
                &obligation.ret_ty,
                unifier,
            ));
            (obligation.call_node, receiver_ty, arg_tys, ret_ty)
        })
        .collect()
}

fn apply_final_assignability(
    constrain: &ConstrainOutput,
    unifier: &mut TcUnifier,
    failed_constraints: &mut usize,
    deferred_expr_errors: &mut Vec<DeferredExprError>,
    deferred_pattern_errors: &mut Vec<DeferredPatternError>,
    non_expr_errors: &mut Vec<TypeCheckError>,
) {
    for constraint in constrain
        .constraints
        .iter()
        .filter(|constraint| matches!(constraint, Constraint::Assignable { .. }))
    {
        constraint_checks::apply_constraint(
            constraint,
            unifier,
            failed_constraints,
            deferred_expr_errors,
            deferred_pattern_errors,
            non_expr_errors,
        );
    }
}

fn remap_deferred_expr_errors(
    deferred_expr_errors: Vec<DeferredExprError>,
    expr_errors: &mut Vec<TypeCheckError>,
    covered_exprs: &HashSet<NodeId>,
    index_nodes: &HashMap<NodeId, Span>,
    enum_payload_nodes: &HashMap<NodeId, (String, usize, Span)>,
) {
    let covered_expr_spans = expr_errors
        .iter()
        .map(TypeCheckError::span)
        .collect::<Vec<_>>();

    for (expr_id, err, span) in deferred_expr_errors {
        if let Some(index_span) = index_nodes.get(&expr_id)
            && let Some(index_error) = constraint_checks::remap_index_unify_error(&err, *index_span)
        {
            expr_errors.push(index_error);
            continue;
        }
        if let Some((variant, index, payload_span)) = enum_payload_nodes.get(&expr_id)
            && let Some(payload_error) = constraint_checks::remap_enum_payload_unify_error(
                &err,
                variant,
                *index,
                *payload_span,
            )
        {
            expr_errors.push(payload_error);
            continue;
        }
        if !covered_exprs.contains(&expr_id) && !covered_expr_spans.contains(&span) {
            expr_errors.push(constraint_checks::unify_error_to_diag(err, span));
        }
    }
}

fn solve_pattern_stage(
    constrain: &ConstrainOutput,
    unifier: &mut TcUnifier,
    engine: &TypecheckEngine,
    deferred_pattern_errors: Vec<DeferredPatternError>,
) -> Vec<TypeCheckError> {
    let (mut pattern_errors, covered_patterns) = patterns::check_pattern_obligations(
        &constrain.pattern_obligations,
        &constrain.def_terms,
        unifier,
        &engine.env().type_defs,
        &engine.env().type_symbols,
        &engine.context().def_table,
        &engine.context().def_owners,
        engine.context(),
        engine.is_partial_mode(),
    );
    for (pattern_id, err, span) in deferred_pattern_errors {
        if !covered_patterns.contains(&pattern_id) {
            pattern_errors.push(constraint_checks::unify_error_to_diag(err, span));
        }
    }
    pattern_errors
}

fn build_solve_output(
    constrain: &ConstrainOutput,
    unifier: &TcUnifier,
    resolved_call_defs: HashMap<NodeId, DefId>,
    failed_constraints: usize,
) -> SolveOutput {
    let mut output = SolveOutput {
        resolved_node_types: HashMap::new(),
        resolved_def_types: HashMap::new(),
        resolved_call_defs,
        failed_constraints,
    };

    for (node_id, term) in &constrain.node_terms {
        output
            .resolved_node_types
            .insert(*node_id, term_utils::canonicalize_type(unifier.apply(term)));
    }
    for (def_id, term) in &constrain.def_terms {
        output
            .resolved_def_types
            .insert(*def_id, term_utils::canonicalize_type(unifier.apply(term)));
    }

    output
}

fn check_expr_obligations(
    obligations: &[ExprObligation],
    def_terms: &HashMap<DefId, Type>,
    unifier: &mut TcUnifier,
    def_table: &DefTable,
    context: &ResolvedContext,
    type_defs: &HashMap<String, Type>,
    type_symbols: &HashMap<String, DefId>,
    def_owners: &HashMap<DefId, ModuleId>,
    linear_index: &crate::core::linear::LinearIndex,
    property_sigs: &HashMap<String, HashMap<String, CollectedPropertySig>>,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
    trait_sigs: &HashMap<String, CollectedTraitSig>,
    var_trait_bounds: &HashMap<TyVarId, Vec<String>>,
) -> (Vec<TypeCheckError>, HashSet<NodeId>) {
    let mut errors = Vec::new();
    let mut covered_exprs = HashSet::new();

    for obligation in obligations {
        if index::try_check_expr_obligation_index(
            obligation,
            unifier,
            &mut errors,
            &mut covered_exprs,
        ) {
            continue;
        }
        if collections::try_check_expr_obligation_collections(
            obligation,
            unifier,
            &mut errors,
            &mut covered_exprs,
        ) {
            continue;
        }
        if control::try_check_expr_obligation_control(
            obligation,
            def_terms,
            method_sigs,
            unifier,
            &mut errors,
            &mut covered_exprs,
        ) {
            continue;
        }
        if nominal::try_check_expr_obligation_nominal(
            obligation,
            unifier,
            def_table,
            context,
            type_defs,
            type_symbols,
            def_owners,
            linear_index,
            property_sigs,
            trait_sigs,
            var_trait_bounds,
            &mut errors,
            &mut covered_exprs,
        ) {
            continue;
        }
        if linear::try_check_expr_obligation_linear(
            obligation,
            unifier,
            type_defs,
            linear_index,
            &mut errors,
            &mut covered_exprs,
        ) {
            continue;
        }
        if expr_ops::try_check_expr_obligation_ops(
            obligation,
            unifier,
            &mut errors,
            &mut covered_exprs,
        ) {
            continue;
        }
        match obligation {
            ExprObligation::BinOp { .. } | ExprObligation::UnaryOp { .. } => {
                unreachable!("operator obligations are handled by solve::expr_ops");
            }
            ExprObligation::Join { .. } | ExprObligation::Try { .. } => {
                unreachable!("control obligations are handled by solve::control");
            }
            ExprObligation::ArrayIndex { .. }
            | ExprObligation::MapIndexAssign { .. }
            | ExprObligation::Slice { .. }
            | ExprObligation::Range { .. } => {
                unreachable!("index obligations are handled by solve::index");
            }
            ExprObligation::SetElemType { .. } | ExprObligation::MapKeyType { .. } => {
                unreachable!("collection obligations are handled by solve::collections");
            }
            ExprObligation::ForIter { .. } => {
                unreachable!("control obligations are handled by solve::control");
            }
            ExprObligation::EnumVariantPayload { .. }
            | ExprObligation::StructConstruct { .. }
            | ExprObligation::StructUpdate { .. }
            | ExprObligation::TupleField { .. }
            | ExprObligation::StructField { .. }
            | ExprObligation::StructFieldAssign { .. } => {
                unreachable!("nominal obligations are handled by solve::nominal");
            }
            ExprObligation::LinearMachineCreate { .. }
            | ExprObligation::LinearMachineLookup { .. }
            | ExprObligation::LinearMachineResume { .. }
            | ExprObligation::LinearSessionAction { .. }
            | ExprObligation::LinearMachineDeliver { .. }
            | ExprObligation::LinearMachineSend { .. } => {
                unreachable!("linear obligations are handled by solve::linear");
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
            let result_ty = term_utils::resolve_term(result, unifier);
            if term_utils::is_unresolved(&result_ty) {
                return true;
            }
            arms.iter()
                .map(|arm| term_utils::resolve_term(arm, unifier))
                .any(|arm_ty| term_utils::is_unresolved(&arm_ty))
        }
        ExprObligation::StructField { target, result, .. } => {
            let owner_ty = term_utils::peel_heap(term_utils::resolve_term(target, unifier));
            let result_ty = term_utils::resolve_term(result, unifier);
            term_utils::is_unresolved(&owner_ty) || term_utils::is_unresolved(&result_ty)
        }
        ExprObligation::TupleField { target, result, .. } => {
            let owner_ty = term_utils::peel_heap(term_utils::resolve_term(target, unifier));
            let result_ty = term_utils::resolve_term(result, unifier);
            term_utils::is_unresolved(&owner_ty) || term_utils::is_unresolved(&result_ty)
        }
        ExprObligation::StructFieldAssign {
            target,
            assignee,
            value,
            ..
        } => {
            let owner_ty = term_utils::peel_heap(term_utils::resolve_term(target, unifier));
            let assignee_ty = term_utils::resolve_term(assignee, unifier);
            let value_ty = term_utils::resolve_term(value, unifier);
            term_utils::is_unresolved(&owner_ty)
                || term_utils::is_unresolved(&assignee_ty)
                || term_utils::is_unresolved(&value_ty)
        }
        ExprObligation::SetElemType { elem_ty, .. } => {
            let elem_ty = term_utils::resolve_term(elem_ty, unifier);
            term_utils::is_unresolved(&elem_ty)
        }
        ExprObligation::MapKeyType { key_ty, .. } => {
            let key_ty = term_utils::resolve_term(key_ty, unifier);
            term_utils::is_unresolved(&key_ty)
        }
        ExprObligation::MapIndexAssign { target, .. } => {
            let owner_ty = term_utils::peel_heap(term_utils::resolve_term(target, unifier));
            term_utils::is_unresolved(&owner_ty)
        }
        ExprObligation::ForIter { iter, pattern, .. } => {
            let iter_ty = term_utils::resolve_term(iter, unifier);
            let pattern_ty = term_utils::resolve_term(pattern, unifier);
            term_utils::is_unresolved(&iter_ty) || term_utils::is_unresolved(&pattern_ty)
        }
        ExprObligation::ArrayIndex { target, result, .. } => {
            let owner_ty = term_utils::peel_heap(term_utils::resolve_term(target, unifier));
            let result_ty = term_utils::resolve_term(result, unifier);
            term_utils::is_unresolved(&owner_ty) || term_utils::is_unresolved(&result_ty)
        }
        ExprObligation::Slice { target, result, .. } => {
            let owner_ty = term_utils::peel_heap(term_utils::resolve_term(target, unifier));
            let result_ty = term_utils::resolve_term(result, unifier);
            term_utils::is_unresolved(&owner_ty) || term_utils::is_unresolved(&result_ty)
        }
        ExprObligation::Try {
            callable_def_id: _, ..
        } => true,
        ExprObligation::LinearMachineCreate {
            receiver, result, ..
        } => {
            // `create` depends on the receiver's hosted-machine handle type,
            // which may only become concrete after ordinary call solving
            // resolves the earlier `spawn()?` expression.
            let receiver_ty = term_utils::resolve_term(receiver, unifier);
            let result_ty = term_utils::resolve_term(result, unifier);
            term_utils::is_unresolved(&receiver_ty) || term_utils::is_unresolved(&result_ty)
        }
        ExprObligation::LinearMachineLookup {
            receiver,
            key_term,
            result,
            ..
        } => {
            let receiver_ty = term_utils::resolve_term(receiver, unifier);
            let key_ty = term_utils::resolve_term(key_term, unifier);
            let result_ty = term_utils::resolve_term(result, unifier);
            term_utils::is_unresolved(&receiver_ty)
                || term_utils::is_unresolved(&key_ty)
                || term_utils::is_unresolved(&result_ty)
        }
        ExprObligation::LinearMachineResume {
            receiver,
            key_term,
            result,
            ..
        } => {
            let receiver_ty = term_utils::resolve_term(receiver, unifier);
            let key_ty = term_utils::resolve_term(key_term, unifier);
            let result_ty = term_utils::resolve_term(result, unifier);
            term_utils::is_unresolved(&receiver_ty)
                || term_utils::is_unresolved(&key_ty)
                || term_utils::is_unresolved(&result_ty)
        }
        ExprObligation::LinearMachineDeliver {
            receiver,
            key_term,
            event_term,
            result,
            ..
        } => {
            let receiver_ty = term_utils::resolve_term(receiver, unifier);
            let key_ty = term_utils::resolve_term(key_term, unifier);
            let event_ty = term_utils::resolve_term(event_term, unifier);
            let result_ty = term_utils::resolve_term(result, unifier);
            term_utils::is_unresolved(&receiver_ty)
                || term_utils::is_unresolved(&key_ty)
                || term_utils::is_unresolved(&event_ty)
                || term_utils::is_unresolved(&result_ty)
        }
        ExprObligation::LinearMachineSend {
            target,
            payload_term,
            result,
            ..
        } => {
            let target_ty = term_utils::resolve_term(target, unifier);
            let payload_ty = term_utils::resolve_term(payload_term, unifier);
            let result_ty = term_utils::resolve_term(result, unifier);
            term_utils::is_unresolved(&target_ty)
                || term_utils::is_unresolved(&payload_ty)
                || term_utils::is_unresolved(&result_ty)
        }
        ExprObligation::LinearSessionAction { .. } => false,
        _ => false,
    }
}

fn check_unresolved_local_infer_vars(
    resolved_def_types: &HashMap<DefId, Type>,
    constraints: &[Constraint],
    pattern_obligations: &[PatternObligation],
    def_table: &DefTable,
    vars: &crate::core::typecheck::typesys::TypeVarStore,
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
        if let PatternObligation::Bind { pattern, span, .. } = obligation {
            collect_pattern_bind_decl_spans(pattern, *span, def_table, &mut decl_spans);
        }
    }

    let mut errors = Vec::new();
    let blocking_spans = existing_errors
        .iter()
        .filter(|err| !matches!(err.kind(), TEK::UnknownType))
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
            crate::core::typecheck::tc_push_error!(errors, span, TEK::UnknownType);
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
    def_table: &DefTable,
    out: &mut HashMap<DefId, Span>,
) {
    match &pattern.kind {
        BindPatternKind::Wildcard => {}
        BindPatternKind::Name { .. } => {
            out.entry(def_table.def_id(pattern.id)).or_insert(span);
        }
        BindPatternKind::Array { .. } | BindPatternKind::Tuple { .. } => {
            pattern.kind.for_each_child_pattern(|child| {
                collect_pattern_bind_decl_spans(child, child.span, def_table, out)
            })
        }
        BindPatternKind::Struct { fields, .. } => {
            for field in fields {
                collect_pattern_bind_decl_spans(&field.pattern, field.span, def_table, out);
            }
        }
    }
}

fn has_unresolved_infer_var(ty: &Type, vars: &TypeVarStore) -> bool {
    ty.any(&|t| {
        matches!(t, Type::Var(var) if matches!(
            vars.kind(*var),
            Some(TypeVarKind::InferLocal) | Some(TypeVarKind::InferInt)
        ))
    })
}

fn default_unresolved_int_vars(unifier: &mut TcUnifier) {
    let unresolved = unifier
        .vars()
        .unresolved_vars_by_kind(TypeVarKind::InferInt);
    for var in unresolved {
        unifier.vars_mut().bind(var, Type::sint(32));
    }
}

fn is_iterable(
    ty: &Type,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> bool {
    iterable_plan(ty, method_sigs).is_some()
}

fn iterable_plan(
    ty: &Type,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> Option<crate::core::plans::ForPlan> {
    plan_for_iterable_type_with_methods(ty, method_sigs)
}

fn iterable_protocol_error(
    ty: &Type,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> Option<ProtocolForPlanError> {
    diagnose_protocol_iterable_type(ty, method_sigs)
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
    match property_access::lookup(owner_ty, field, property_sigs, trait_sigs, var_trait_bounds) {
        Ok(prop) => {
            let readable = prop.getter_def.is_some_and(|def_id| {
                access_utils::is_def_accessible_from(caller_def_id, def_id, def_table, def_owners)
            }) || (prop.getter_def.is_none() && prop.has_get);
            let writable = prop.setter_def.is_some_and(|def_id| {
                access_utils::is_def_accessible_from(caller_def_id, def_id, def_table, def_owners)
            }) || (prop.setter_def.is_none() && prop.has_set);
            if (prop.has_get || prop.has_set) && !readable && !writable {
                return Err(PropertyResolution::Private);
            }
            Ok(ResolvedPropertyAccess {
                ty: prop.ty,
                readable,
                writable,
            })
        }
        Err(property_access::PropertyLookupError::Missing) => Err(PropertyResolution::Missing),
        Err(property_access::PropertyLookupError::Ambiguous) => Err(PropertyResolution::Ambiguous),
    }
}

fn iterable_elem_type(
    ty: &Type,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> Option<Type> {
    iterable_plan(ty, method_sigs).map(|plan| plan.item_ty)
}

#[cfg(test)]
#[path = "../../../tests/core/typecheck/solver/t_mod.rs"]
mod tests;
