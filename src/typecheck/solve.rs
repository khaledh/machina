use std::collections::HashMap;
use std::collections::HashSet;

use crate::diag::Span;
use crate::resolve::DefId;
use crate::tree::NodeId;
use crate::tree::resolved::{BindPattern, BindPatternKind};
use crate::typecheck::constraints::{Constraint, ConstraintReason, ExprObligation, TyTerm};
use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::typecheck::unify::{TcUnifier, TcUnifyError};
use crate::types::{Type, TypeAssignability, type_assignable};

#[derive(Debug, Clone, Default)]
#[allow(dead_code)]
pub(crate) struct SolveOutput {
    pub(crate) resolved_node_types: HashMap<NodeId, Type>,
    pub(crate) resolved_def_types: HashMap<DefId, Type>,
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
    for constraint in &constrain.constraints {
        let result = match constraint {
            Constraint::Eq { left, right, .. } => unifier.unify(
                &canonicalize_type(term_as_type(left)),
                &canonicalize_type(term_as_type(right)),
            ),
            Constraint::Assignable { from, to, .. } => {
                solve_assignable_constraint(from, to, &mut unifier)
            }
        };
        if let Err(err) = result {
            failed_constraints += 1;
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

    let (mut expr_errors, covered_exprs) = check_expr_obligations(
        &constrain.expr_obligations,
        &unifier,
        &engine.env().type_defs,
    );
    let index_nodes = collect_index_nodes(&constrain.expr_obligations);
    let enum_payload_nodes = collect_enum_payload_nodes(&constrain.expr_obligations);
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

    let (mut pattern_errors, covered_patterns) =
        check_pattern_obligations(&constrain.pattern_obligations, &unifier);
    for (pattern_id, err, span) in deferred_pattern_errors {
        if !covered_patterns.contains(&pattern_id) {
            pattern_errors.push(unify_error_to_diag(err, span));
        }
    }

    let mut errors = Vec::new();
    errors.extend(expr_errors);
    errors.extend(pattern_errors);
    errors.extend(non_expr_errors);

    let mut output = SolveOutput {
        resolved_node_types: HashMap::new(),
        resolved_def_types: HashMap::new(),
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

    *engine.type_vars_mut() = unifier.vars().clone();
    engine.state_mut().solve = output;

    if errors.is_empty() {
        Ok(())
    } else {
        engine.state_mut().diags.extend(errors.clone());
        Err(errors)
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
            TypeAssignability::Incompatible => {
                Err(TcUnifyError::Mismatch(to_applied, from_applied))
            }
            _ => Ok(()),
        };
    }

    unifier.unify(&from_raw, &to_raw)
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
    unifier: &TcUnifier,
    type_defs: &HashMap<String, Type>,
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
                span,
                ..
            } => {
                let target_ty = resolve_term(target, unifier);
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

                match &target_ty {
                    Type::Array { dims, .. } => {
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
                        }
                    }
                    Type::Slice { .. } | Type::String => {}
                    ty if is_unresolved(ty) => {}
                    _ => {
                        errors.push(
                            TypeCheckErrorKind::InvalidIndexTargetType(target_ty, *span).into(),
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
                span,
                ..
            } => {
                let target_ty = resolve_term(target, unifier);
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

                match &target_ty {
                    Type::Array { dims, .. } => {
                        if dims.is_empty() {
                            errors.push(
                                TypeCheckErrorKind::SliceTargetZeroDimArray(target_ty, *span)
                                    .into(),
                            );
                            covered_exprs.insert(*expr_id);
                        }
                    }
                    Type::String => {}
                    ty if is_unresolved(ty) => {}
                    _ => {
                        errors.push(
                            TypeCheckErrorKind::SliceTargetNotArrayOrString(target_ty, *span)
                                .into(),
                        );
                        covered_exprs.insert(*expr_id);
                    }
                }
            }
            ExprObligation::Range { .. } => {}
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

                let pattern_ty = resolve_term(pattern, unifier);
                if let Some(elem_ty) = iterable_elem_type(&iter_ty)
                    && matches!(
                        type_assignable(&pattern_ty, &elem_ty),
                        TypeAssignability::Incompatible
                    )
                    && !is_unresolved(&pattern_ty)
                    && !is_unresolved(&elem_ty)
                {
                    errors.push(
                        TypeCheckErrorKind::DeclTypeMismatch(pattern_ty, elem_ty, *span).into(),
                    );
                    covered_exprs.insert(*stmt_id);
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
                                errors.push(
                                    TypeCheckErrorKind::UnknownStructField(
                                        field_name.clone(),
                                        *span,
                                    )
                                    .into(),
                                );
                                covered_exprs.insert(*expr_id);
                                break;
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

fn check_pattern_obligations(
    obligations: &[crate::typecheck::constraints::PatternObligation],
    unifier: &TcUnifier,
) -> (Vec<TypeCheckError>, HashSet<NodeId>) {
    let mut errors = Vec::new();
    let mut covered = HashSet::new();

    for obligation in obligations {
        if let crate::typecheck::constraints::PatternObligation::Bind {
            pattern_id,
            pattern,
            value_ty,
            span,
        } = obligation
        {
            let value_ty = resolve_term(value_ty, unifier);
            if let Some(error) = check_bind_pattern(pattern, &value_ty, *span) {
                errors.push(error);
                covered.insert(*pattern_id);
            }
        }
    }

    (errors, covered)
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
                        return Some(
                            TypeCheckErrorKind::PatternTypeMismatch(
                                pattern.clone(),
                                value_ty.clone(),
                                span,
                            )
                            .into(),
                        );
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
        | ExprObligation::StructUpdate { span, .. } => *span,
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

fn is_unresolved(ty: &Type) -> bool {
    matches!(ty, Type::Unknown | Type::Var(_))
}

fn is_iterable(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Range { .. } | Type::Array { .. } | Type::Slice { .. } | Type::String
    )
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
