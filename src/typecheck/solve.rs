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
use crate::resolve::{DefId, DefKind, DefTable};
use crate::tree::NodeId;
use crate::tree::resolved::{BindPattern, BindPatternKind, MatchPattern, MatchPatternBinding};
use crate::typecheck::constraints::{
    CallCallee, CallObligation, Constraint, ConstraintReason, ExprObligation, TyTerm,
};
use crate::typecheck::engine::{CollectedCallableSig, CollectedPropertySig, TypecheckEngine};
use crate::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::typecheck::type_map::{resolve_type_def_with_args, resolve_type_expr};
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

    // Stage C: expression obligations.
    let (mut expr_errors, covered_exprs) = check_expr_obligations(
        &constrain.expr_obligations,
        &mut unifier,
        &engine.env().type_defs,
        &engine.env().property_sigs,
    );
    let index_nodes = collect_index_nodes(&constrain.expr_obligations);
    let enum_payload_nodes = collect_enum_payload_nodes(&constrain.expr_obligations);
    let covered_expr_spans = expr_errors
        .iter()
        .map(TypeCheckError::span)
        .collect::<Vec<_>>();

    // Stage D: call obligations (overload/generic resolution).
    let (mut call_errors, resolved_call_defs) = check_call_obligations(
        &constrain.call_obligations,
        &mut unifier,
        &engine.env().func_sigs,
        &engine.env().method_sigs,
    );

    // Stage E: final assignability check (diagnostics-producing).
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

    // Stage F: pattern obligations and late unresolved-local checks.
    let (mut pattern_errors, covered_patterns) = check_pattern_obligations(
        &constrain.pattern_obligations,
        &constrain.def_terms,
        &mut unifier,
        &engine.env().type_defs,
        &engine.context().def_table,
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

    let mut output = SolveOutput {
        resolved_node_types: HashMap::new(),
        resolved_def_types: HashMap::new(),
        resolved_call_defs,
        failed_constraints,
    };

    for (node_id, term) in &constrain.node_terms {
        output.resolved_node_types.insert(
            *node_id,
            canonicalize_type(unifier.apply(&term_as_type(term))),
        );
    }
    for (def_id, term) in &constrain.def_terms {
        output.resolved_def_types.insert(
            *def_id,
            canonicalize_type(unifier.apply(&term_as_type(term))),
        );
    }

    errors.extend(check_unresolved_local_infer_vars(
        &output.resolved_def_types,
        &constrain.constraints,
        &constrain.pattern_obligations,
        &engine.context().def_table,
        unifier.vars(),
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
        let _ = solve_assignable_constraint(from, to, unifier);
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
    let result = match constraint {
        Constraint::Eq { left, right, .. } => unifier.unify(
            &canonicalize_type(term_as_type(left)),
            &canonicalize_type(term_as_type(right)),
        ),
        Constraint::Assignable { from, to, .. } => solve_assignable_constraint(from, to, unifier),
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
                non_expr_errors.push(unify_error_to_diag(err, reason_span(constraint)));
            }
        }
    }
}

fn term_as_type(term: &TyTerm) -> Type {
    match term {
        TyTerm::Concrete(ty) => ty.clone(),
        TyTerm::Var(var) => Type::Var(*var),
    }
}

fn solve_assignable_constraint(
    from: &TyTerm,
    to: &TyTerm,
    unifier: &mut TcUnifier,
) -> Result<(), TcUnifyError> {
    let from_raw = canonicalize_type(term_as_type(from));
    let to_raw = canonicalize_type(term_as_type(to));
    let from_applied = canonicalize_type(unifier.apply(&from_raw));
    let to_applied = canonicalize_type(unifier.apply(&to_raw));

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

fn solve_assignable_types(
    from: &Type,
    to: &Type,
    unifier: &mut TcUnifier,
) -> Result<(), TcUnifyError> {
    let from_raw = canonicalize_type(from.clone());
    let to_raw = canonicalize_type(to.clone());
    let from_applied = canonicalize_type(unifier.apply(&from_raw));
    let to_applied = canonicalize_type(unifier.apply(&to_raw));

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

fn check_call_obligations(
    obligations: &[CallObligation],
    unifier: &mut TcUnifier,
    func_sigs: &HashMap<String, Vec<CollectedCallableSig>>,
    method_sigs: &HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> (Vec<TypeCheckError>, HashMap<NodeId, DefId>) {
    let mut errors = Vec::new();
    let mut resolved_call_defs = HashMap::new();
    for obligation in obligations {
        if let CallCallee::Dynamic { .. } = &obligation.callee {
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
                        if let Err(_) = solve_assignable_types(&arg_ty, &param.ty, unifier) {
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
                    if let Err(err) = unifier.unify(&term_as_type(&obligation.ret_ty), &ret_ty) {
                        errors.push(unify_error_to_diag(err, obligation.span));
                    }
                }
            }
            continue;
        }

        let mut candidates: Vec<&CollectedCallableSig> = match &obligation.callee {
            CallCallee::NamedFunction { name, .. } => {
                named_call_candidates(name, obligation.arg_terms.len(), func_sigs)
            }
            CallCallee::Method { name } => {
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
                )
            }
            CallCallee::Dynamic { .. } => Vec::new(),
        };

        if candidates.is_empty() {
            let name = match &obligation.callee {
                CallCallee::NamedFunction { name, .. } => name.clone(),
                CallCallee::Method { name } => name.clone(),
                CallCallee::Dynamic { expr_id } => format!("<dynamic:{expr_id}>"),
            };
            errors.push(TypeCheckErrorKind::OverloadNoMatch(name, obligation.span).into());
            continue;
        }

        let mut best_choice: Option<(i32, TcUnifier, DefId)> = None;
        let mut first_error = None;
        for sig in candidates.drain(..) {
            let mut trial = unifier.clone();
            let (inst_params, inst_ret) = instantiate_sig(sig, &mut trial);
            let mut failed = false;
            let mut score = 0i32;
            for (index, (arg_term, param_ty)) in obligation
                .arg_terms
                .iter()
                .zip(inst_params.iter())
                .enumerate()
            {
                let arg_ty = resolve_term(arg_term, &trial);
                if let Err(_) = solve_assignable_types(&arg_ty, param_ty, &mut trial) {
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
            if let Err(err) = trial.unify(&term_as_type(&obligation.ret_ty), &inst_ret) {
                first_error.get_or_insert_with(|| unify_error_to_diag(err, obligation.span));
                continue;
            }
            match &best_choice {
                Some((best_score, _, _)) if score <= *best_score => {}
                _ => best_choice = Some((score, trial, sig.def_id)),
            }
        }

        if let Some((_, next, def_id)) = best_choice {
            *unifier = next;
            resolved_call_defs.insert(obligation.call_node, def_id);
        } else if let Some(err) = first_error {
            errors.push(err);
        } else {
            let name = match &obligation.callee {
                CallCallee::NamedFunction { name, .. } => name.clone(),
                CallCallee::Method { name } => name.clone(),
                CallCallee::Dynamic { expr_id } => format!("<dynamic:{expr_id}>"),
            };
            errors.push(TypeCheckErrorKind::OverloadNoMatch(name, obligation.span).into());
        }
    }
    (errors, resolved_call_defs)
}

fn named_call_candidates<'a>(
    name: &str,
    arity: usize,
    func_sigs: &'a HashMap<String, Vec<CollectedCallableSig>>,
) -> Vec<&'a CollectedCallableSig> {
    func_sigs
        .get(name)
        .map(|overloads| {
            overloads
                .iter()
                .filter(|sig| sig.params.len() == arity)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default()
}

fn method_call_candidates<'a>(
    method_name: &str,
    receiver_ty: Option<&Type>,
    arity: usize,
    method_sigs: &'a HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) -> Vec<&'a CollectedCallableSig> {
    let Some(receiver_ty) = receiver_ty else {
        return Vec::new();
    };
    let owner = match receiver_ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name.clone(),
        Type::String => "string".to_string(),
        _ => return Vec::new(),
    };
    let Some(by_name) = method_sigs.get(&owner) else {
        return Vec::new();
    };
    let Some(overloads) = by_name.get(method_name) else {
        return Vec::new();
    };
    overloads
        .iter()
        .filter(|sig| sig.params.len() == arity)
        .collect::<Vec<_>>()
}

fn instantiate_sig(sig: &CollectedCallableSig, unifier: &mut TcUnifier) -> (Vec<Type>, Type) {
    if sig.type_param_count == 0 {
        return (
            sig.params.iter().map(|param| param.ty.clone()).collect(),
            sig.ret_ty.clone(),
        );
    }
    let mut map = HashMap::new();
    for index in 0..sig.type_param_count {
        let fresh = unifier.vars_mut().fresh_infer_local();
        map.insert(TyVarId::new(index as u32), Type::Var(fresh));
    }
    let params = sig
        .params
        .iter()
        .map(|param| subst_type_vars(&param.ty, &map))
        .collect::<Vec<_>>();
    let ret = subst_type_vars(&sig.ret_ty, &map);
    (params, ret)
}

fn subst_type_vars(ty: &Type, map: &HashMap<TyVarId, Type>) -> Type {
    match ty {
        Type::Var(var) => map.get(var).cloned().unwrap_or(Type::Var(*var)),
        Type::Fn { params, ret_ty } => Type::Fn {
            params: params
                .iter()
                .map(|param| crate::types::FnParam {
                    mode: param.mode,
                    ty: subst_type_vars(&param.ty, map),
                })
                .collect(),
            ret_ty: Box::new(subst_type_vars(ret_ty, map)),
        },
        Type::Range { elem_ty } => Type::Range {
            elem_ty: Box::new(subst_type_vars(elem_ty, map)),
        },
        Type::Array { elem_ty, dims } => Type::Array {
            elem_ty: Box::new(subst_type_vars(elem_ty, map)),
            dims: dims.clone(),
        },
        Type::Tuple { field_tys } => Type::Tuple {
            field_tys: field_tys
                .iter()
                .map(|field_ty| subst_type_vars(field_ty, map))
                .collect(),
        },
        Type::Struct { name, fields } => Type::Struct {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|field| crate::types::StructField {
                    name: field.name.clone(),
                    ty: subst_type_vars(&field.ty, map),
                })
                .collect(),
        },
        Type::Enum { name, variants } => Type::Enum {
            name: name.clone(),
            variants: variants
                .iter()
                .map(|variant| crate::types::EnumVariant {
                    name: variant.name.clone(),
                    payload: variant
                        .payload
                        .iter()
                        .map(|payload_ty| subst_type_vars(payload_ty, map))
                        .collect(),
                })
                .collect(),
        },
        Type::Slice { elem_ty } => Type::Slice {
            elem_ty: Box::new(subst_type_vars(elem_ty, map)),
        },
        Type::Heap { elem_ty } => Type::Heap {
            elem_ty: Box::new(subst_type_vars(elem_ty, map)),
        },
        Type::Ref { mutable, elem_ty } => Type::Ref {
            mutable: *mutable,
            elem_ty: Box::new(subst_type_vars(elem_ty, map)),
        },
        other => other.clone(),
    }
}

fn reason_span(constraint: &Constraint) -> Span {
    let reason = match constraint {
        Constraint::Eq { reason, .. } => reason,
        Constraint::Assignable { reason, .. } => reason,
    };
    match reason {
        ConstraintReason::Expr(_, span)
        | ConstraintReason::Stmt(_, span)
        | ConstraintReason::Pattern(_, span)
        | ConstraintReason::Decl(_, span)
        | ConstraintReason::Return(_, span) => *span,
    }
}

enum ConstraintSubject {
    Expr(NodeId),
    Pattern(NodeId),
    Other,
}

fn reason_subject_id(constraint: &Constraint) -> ConstraintSubject {
    let reason = match constraint {
        Constraint::Eq { reason, .. } => reason,
        Constraint::Assignable { reason, .. } => reason,
    };
    match reason {
        ConstraintReason::Expr(expr_id, _) => ConstraintSubject::Expr(*expr_id),
        ConstraintReason::Pattern(pattern_id, _) => ConstraintSubject::Pattern(*pattern_id),
        _ => ConstraintSubject::Other,
    }
}

fn check_expr_obligations(
    obligations: &[ExprObligation],
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    property_sigs: &HashMap<String, HashMap<String, CollectedPropertySig>>,
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
                let left_ty = resolve_term(left, unifier);
                let right_ty = resolve_term(right, unifier);
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
                let operand_ty = resolve_term(operand, unifier);
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
                        let _ = unifier.unify(&term_as_type(result), &result_ty);
                    }
                    Type::Slice { elem_ty } => {
                        if indices.len() != 1 {
                            errors.push(
                                TypeCheckErrorKind::TooManyIndices(1, indices.len(), *span).into(),
                            );
                            covered_exprs.insert(*expr_id);
                            continue;
                        }
                        let _ = unifier.unify(&term_as_type(result), elem_ty);
                    }
                    Type::String => {
                        if indices.len() != 1 {
                            errors.push(
                                TypeCheckErrorKind::TooManyIndices(1, indices.len(), *span).into(),
                            );
                            covered_exprs.insert(*expr_id);
                            continue;
                        }
                        let _ = unifier.unify(&term_as_type(result), &Type::uint(8));
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
                            &term_as_type(result),
                            &Type::Slice {
                                elem_ty: Box::new(slice_elem_ty),
                            },
                        );
                    }
                    Type::Slice { elem_ty } => {
                        let _ = unifier.unify(
                            &term_as_type(result),
                            &Type::Slice {
                                elem_ty: elem_ty.clone(),
                            },
                        );
                    }
                    Type::String => {
                        let _ = unifier.unify(
                            &term_as_type(result),
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
                    &term_as_type(result),
                    &Type::Range {
                        elem_ty: Box::new(Type::uint(64)),
                    },
                );
            }
            ExprObligation::ForIter {
                stmt_id,
                iter,
                pattern,
                span,
            } => {
                let iter_ty = resolve_term(iter, unifier);
                if !is_iterable(&iter_ty) && !is_unresolved(&iter_ty) {
                    errors.push(TypeCheckErrorKind::ForIterNotIterable(iter_ty, *span).into());
                    covered_exprs.insert(*stmt_id);
                    continue;
                }

                if let Some(elem_ty) = iterable_elem_type(&iter_ty) {
                    let _ = unifier.unify(&term_as_type(pattern), &elem_ty);
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
            ExprObligation::StructUpdate {
                expr_id,
                target,
                fields,
                result,
                span,
            } => {
                let target_ty = resolve_term(target, unifier);
                match &target_ty {
                    Type::Struct {
                        fields: struct_fields,
                        ..
                    } => {
                        for (field_name, found_term) in fields {
                            let found_ty = resolve_term(found_term, unifier);
                            let Some(expected_field) =
                                struct_fields.iter().find(|field| field.name == *field_name)
                            else {
                                continue;
                            };
                            if matches!(
                                type_assignable(&found_ty, &expected_field.ty),
                                TypeAssignability::Incompatible
                            ) && !is_unresolved(&found_ty)
                            {
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
                        let _ = unifier.unify(&term_as_type(result), &target_ty);
                    }
                    ty if is_unresolved(ty) => {}
                    _ => {
                        errors.push(
                            TypeCheckErrorKind::InvalidStructUpdateTarget(target_ty, *span).into(),
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
                        let _ = unifier.unify(&term_as_type(result), &field_tys[*index]);
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
                span,
            } => {
                let target_ty = resolve_term(target, unifier);
                let owner_ty = peel_heap(target_ty.clone());
                if let Some(prop) = lookup_property(property_sigs, &owner_ty, field) {
                    if prop.getter.is_none() {
                        errors.push(
                            TypeCheckErrorKind::PropertyNotReadable(field.clone(), *span).into(),
                        );
                        covered_exprs.insert(*expr_id);
                        continue;
                    }
                    let _ = unifier.unify(&term_as_type(result), &prop.ty);
                    continue;
                }
                if field == "len" {
                    if is_len_target(&owner_ty) {
                        let _ = unifier.unify(&term_as_type(result), &Type::uint(64));
                        continue;
                    }
                }
                match &owner_ty {
                    Type::Struct { fields, .. } => {
                        if let Some(struct_field) = fields.iter().find(|f| f.name == *field) {
                            let _ = unifier.unify(&term_as_type(result), &struct_field.ty);
                        } else {
                            let _ = unifier.unify(&term_as_type(result), &Type::Unknown);
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
                target,
                field,
                assignee,
                value,
                span,
            } => {
                let target_ty = resolve_term(target, unifier);
                let value_ty = resolve_term(value, unifier);
                let owner_ty = peel_heap(target_ty.clone());

                if let Some(prop) = lookup_property(property_sigs, &owner_ty, field) {
                    if prop.setter.is_none() {
                        errors.push(
                            TypeCheckErrorKind::PropertyNotWritable(field.clone(), *span).into(),
                        );
                        covered_exprs.insert(*stmt_id);
                        continue;
                    }
                    let _ = unifier.unify(&term_as_type(assignee), &prop.ty);
                    if matches!(
                        type_assignable(&value_ty, &prop.ty),
                        TypeAssignability::Incompatible
                    ) && !is_unresolved(&value_ty)
                    {
                        errors.push(
                            TypeCheckErrorKind::AssignTypeMismatch(
                                prop.ty.clone(),
                                value_ty,
                                *span,
                            )
                            .into(),
                        );
                        covered_exprs.insert(*stmt_id);
                    }
                    continue;
                }

                if field == "len" && is_len_target(&owner_ty) {
                    errors
                        .push(TypeCheckErrorKind::PropertyNotWritable(field.clone(), *span).into());
                    covered_exprs.insert(*stmt_id);
                    continue;
                }

                match &owner_ty {
                    Type::Struct { fields, .. } => {
                        if let Some(struct_field) = fields.iter().find(|f| f.name == *field) {
                            let _ = unifier.unify(&term_as_type(assignee), &struct_field.ty);
                            if matches!(
                                type_assignable(&value_ty, &struct_field.ty),
                                TypeAssignability::Incompatible
                            ) && !is_unresolved(&value_ty)
                            {
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
                            let _ = unifier.unify(&term_as_type(assignee), &Type::Unknown);
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
        if has_unresolved_infer_var(ty, vars) {
            errors.push(TypeCheckErrorKind::UnknownType(span).into());
        }
    }
    errors
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
    match ty {
        Type::Var(var) => matches!(
            vars.kind(*var),
            Some(crate::typecheck::typesys::TypeVarKind::InferLocal)
        ),
        Type::Fn { params, ret_ty } => {
            params
                .iter()
                .any(|param| has_unresolved_infer_var(&param.ty, vars))
                || has_unresolved_infer_var(ret_ty, vars)
        }
        Type::Range { elem_ty }
        | Type::Array { elem_ty, .. }
        | Type::Slice { elem_ty }
        | Type::Heap { elem_ty }
        | Type::Ref { elem_ty, .. } => has_unresolved_infer_var(elem_ty, vars),
        Type::Tuple { field_tys } => field_tys
            .iter()
            .any(|field_ty| has_unresolved_infer_var(field_ty, vars)),
        Type::Struct { fields, .. } => fields
            .iter()
            .any(|field| has_unresolved_infer_var(&field.ty, vars)),
        Type::Enum { variants, .. } => variants.iter().any(|variant| {
            variant
                .payload
                .iter()
                .any(|payload_ty| has_unresolved_infer_var(payload_ty, vars))
        }),
        _ => false,
    }
}

fn check_pattern_obligations(
    obligations: &[crate::typecheck::constraints::PatternObligation],
    def_terms: &HashMap<DefId, TyTerm>,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    def_table: &DefTable,
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
                span,
            } => {
                let value_ty = resolve_term(value_ty, unifier);
                if let Some(error) = check_bind_pattern(pattern, &value_ty, *span) {
                    errors.push(error);
                    covered.insert(*pattern_id);
                }
            }
            crate::typecheck::constraints::PatternObligation::MatchArm {
                pattern,
                scrutinee_ty,
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
                );
            }
        }
    }

    (errors, covered)
}

fn bind_match_pattern_types(
    pattern: &MatchPattern,
    scrutinee_ty: &Type,
    def_terms: &HashMap<DefId, TyTerm>,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    def_table: &DefTable,
    module: &crate::tree::resolved::Module,
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
                let _ = unifier.unify(scrutinee_ty, &Type::uint(64));
            }
        }
        MatchPattern::Binding { def_id, .. } => {
            if let Some(term) = def_terms.get(def_id) {
                let _ = unifier.unify(&term_as_type(term), scrutinee_ty);
            }
        }
        MatchPattern::Tuple { patterns, .. } => {
            if let Type::Tuple { field_tys } = scrutinee_ty {
                for (child, child_ty) in patterns.iter().zip(field_tys.iter()) {
                    bind_match_pattern_types(
                        child, child_ty, def_terms, unifier, type_defs, def_table, module,
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
                        child, child_ty, def_terms, unifier, type_defs, def_table, module,
                    );
                }
            }
        }
        MatchPattern::EnumVariant {
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
                    enum_name, type_args, type_defs, def_table, module, unifier,
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
                        let _ = unifier.unify(&term_as_type(term), payload_ty);
                    }
                }
            }
        }
    }
}

fn resolve_pattern_enum_type(
    enum_name: &Option<String>,
    type_args: &[crate::tree::resolved::TypeExpr],
    type_defs: &HashMap<String, Type>,
    def_table: &DefTable,
    module: &crate::tree::resolved::Module,
    unifier: &mut TcUnifier,
) -> Option<Type> {
    let enum_name = enum_name.as_ref()?;

    if let Some(ty) = type_defs.get(enum_name)
        && matches!(ty, Type::Enum { .. })
    {
        return Some(ty.clone());
    }

    let def_id = def_table.lookup_type_def_id(enum_name)?;
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
    matches!(ty, Type::Enum { .. }).then_some(ty)
}

fn check_bind_pattern(
    pattern: &BindPattern,
    value_ty: &Type,
    span: Span,
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
                    if let Some(error) = check_bind_pattern(child, child_ty, child.span) {
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
                    if let Some(error) = check_bind_pattern(child, &child_ty, child.span) {
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
                fields: struct_fields,
                ..
            } => {
                for field in fields {
                    let Some(struct_field) = struct_fields.iter().find(|f| f.name == field.name)
                    else {
                        continue;
                    };
                    if let Some(error) =
                        check_bind_pattern(&field.pattern, &struct_field.ty, field.span)
                    {
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
        | ExprObligation::ArrayIndex { span, .. }
        | ExprObligation::Slice { span, .. }
        | ExprObligation::Range { span, .. }
        | ExprObligation::ForIter { span, .. }
        | ExprObligation::EnumVariantPayload { span, .. }
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

fn resolve_term(term: &TyTerm, unifier: &TcUnifier) -> Type {
    match term {
        TyTerm::Concrete(ty) => canonicalize_type(unifier.apply(ty)),
        TyTerm::Var(var) => canonicalize_type(unifier.apply(&Type::Var(*var))),
    }
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
    match ty {
        Type::Unknown | Type::Var(_) => true,
        Type::Fn { params, ret_ty } => {
            params.iter().any(|param| is_unresolved(&param.ty)) || is_unresolved(ret_ty)
        }
        Type::Range { elem_ty }
        | Type::Array { elem_ty, .. }
        | Type::Slice { elem_ty }
        | Type::Heap { elem_ty }
        | Type::Ref { elem_ty, .. } => is_unresolved(elem_ty),
        Type::Tuple { field_tys } => field_tys.iter().any(is_unresolved),
        Type::Struct { fields, .. } => fields.iter().any(|field| is_unresolved(&field.ty)),
        Type::Enum { variants, .. } => variants
            .iter()
            .any(|variant| variant.payload.iter().any(is_unresolved)),
        _ => false,
    }
}

fn is_iterable(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Range { .. } | Type::Array { .. } | Type::Slice { .. } | Type::String
    )
}

fn is_len_target(ty: &Type) -> bool {
    matches!(ty, Type::Array { .. } | Type::Slice { .. } | Type::String)
}

fn peel_heap(ty: Type) -> Type {
    ty.peel_heap()
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

fn iterable_elem_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Range { elem_ty } => Some((**elem_ty).clone()),
        Type::Array { elem_ty, .. } => Some((**elem_ty).clone()),
        Type::Slice { elem_ty } => Some((**elem_ty).clone()),
        Type::String => Some(Type::Char),
        _ => None,
    }
}

fn canonicalize_type(ty: Type) -> Type {
    match ty {
        Type::Fn { params, ret_ty } => Type::Fn {
            params: params
                .into_iter()
                .map(|param| crate::types::FnParam {
                    mode: param.mode,
                    ty: canonicalize_type(param.ty),
                })
                .collect(),
            ret_ty: Box::new(canonicalize_type(*ret_ty)),
        },
        Type::Range { elem_ty } => Type::Range {
            elem_ty: Box::new(canonicalize_type(*elem_ty)),
        },
        Type::Array { elem_ty, dims } => {
            let elem_ty = canonicalize_type(*elem_ty);
            if let Type::Array {
                elem_ty: inner_elem,
                dims: inner_dims,
            } = elem_ty
            {
                let mut merged_dims = dims;
                merged_dims.extend(inner_dims);
                Type::Array {
                    elem_ty: inner_elem,
                    dims: merged_dims,
                }
            } else {
                Type::Array {
                    elem_ty: Box::new(elem_ty),
                    dims,
                }
            }
        }
        Type::Tuple { field_tys } => Type::Tuple {
            field_tys: field_tys.into_iter().map(canonicalize_type).collect(),
        },
        Type::Struct { name, fields } => Type::Struct {
            name,
            fields: fields
                .into_iter()
                .map(|field| crate::types::StructField {
                    name: field.name,
                    ty: canonicalize_type(field.ty),
                })
                .collect(),
        },
        Type::Enum { name, variants } => Type::Enum {
            name,
            variants: variants
                .into_iter()
                .map(|variant| crate::types::EnumVariant {
                    name: variant.name,
                    payload: variant.payload.into_iter().map(canonicalize_type).collect(),
                })
                .collect(),
        },
        Type::Slice { elem_ty } => Type::Slice {
            elem_ty: Box::new(canonicalize_type(*elem_ty)),
        },
        Type::Heap { elem_ty } => Type::Heap {
            elem_ty: Box::new(canonicalize_type(*elem_ty)),
        },
        Type::Ref { mutable, elem_ty } => Type::Ref {
            mutable,
            elem_ty: Box::new(canonicalize_type(*elem_ty)),
        },
        other => other,
    }
}

fn erase_refinements(ty: &Type) -> Type {
    match ty {
        Type::Int { signed, bits, .. } => Type::Int {
            signed: *signed,
            bits: *bits,
            bounds: None,
            nonzero: false,
        },
        Type::Fn { params, ret_ty } => Type::Fn {
            params: params
                .iter()
                .map(|param| crate::types::FnParam {
                    mode: param.mode,
                    ty: erase_refinements(&param.ty),
                })
                .collect(),
            ret_ty: Box::new(erase_refinements(ret_ty)),
        },
        Type::Range { elem_ty } => Type::Range {
            elem_ty: Box::new(erase_refinements(elem_ty)),
        },
        Type::Array { elem_ty, dims } => Type::Array {
            elem_ty: Box::new(erase_refinements(elem_ty)),
            dims: dims.clone(),
        },
        Type::Tuple { field_tys } => Type::Tuple {
            field_tys: field_tys.iter().map(erase_refinements).collect(),
        },
        Type::Struct { name, fields } => Type::Struct {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|field| crate::types::StructField {
                    name: field.name.clone(),
                    ty: erase_refinements(&field.ty),
                })
                .collect(),
        },
        Type::Enum { name, variants } => Type::Enum {
            name: name.clone(),
            variants: variants
                .iter()
                .map(|variant| crate::types::EnumVariant {
                    name: variant.name.clone(),
                    payload: variant.payload.iter().map(erase_refinements).collect(),
                })
                .collect(),
        },
        Type::Slice { elem_ty } => Type::Slice {
            elem_ty: Box::new(erase_refinements(elem_ty)),
        },
        Type::Heap { elem_ty } => Type::Heap {
            elem_ty: Box::new(erase_refinements(elem_ty)),
        },
        Type::Ref { mutable, elem_ty } => Type::Ref {
            mutable: *mutable,
            elem_ty: Box::new(erase_refinements(elem_ty)),
        },
        other => other.clone(),
    }
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
