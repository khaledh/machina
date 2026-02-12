//! Pattern obligation solving.
//!
//! This module contains both bind-pattern validation and match-pattern
//! inference/binding logic used by the solver pre-pass and late diagnostics.

use std::collections::{HashMap, HashSet};

use crate::core::capsule::ModuleId;
use crate::core::context::ResolvedContext;
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefTable};
use crate::core::tree::NodeId;
use crate::core::tree::resolved::{
    BindPattern, BindPatternKind, MatchPattern, MatchPatternBinding,
};
use crate::core::typecheck::constraints::PatternObligation;
use crate::core::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::core::typecheck::type_map::{resolve_type_def_with_args, resolve_type_expr};
use crate::core::typecheck::unify::TcUnifier;
use crate::core::types::{Type, TypeAssignability, type_assignable};

pub(super) fn check_pattern_obligations(
    obligations: &[PatternObligation],
    def_terms: &HashMap<DefId, Type>,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    type_symbols: &HashMap<String, DefId>,
    def_table: &DefTable,
    def_owners: &HashMap<DefId, ModuleId>,
    ctx: &ResolvedContext,
) -> (Vec<TypeCheckError>, HashSet<NodeId>) {
    let mut errors = Vec::new();
    let mut covered = HashSet::new();

    for obligation in obligations {
        match obligation {
            PatternObligation::Bind {
                pattern_id,
                pattern,
                value_ty,
                caller_def_id,
                span,
            } => {
                let value_ty = super::term_utils::resolve_term_for_diagnostics(value_ty, unifier);
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
            PatternObligation::MatchArm {
                arm_id,
                pattern,
                scrutinee_ty,
                caller_def_id: _,
                ..
            } => {
                let scrutinee_ty = super::term_utils::resolve_term(scrutinee_ty, unifier);
                bind_match_pattern_types(
                    pattern,
                    &scrutinee_ty,
                    def_terms,
                    unifier,
                    type_defs,
                    def_table,
                    ctx,
                    &mut errors,
                    &mut covered,
                    *arm_id,
                );
            }
        }
    }

    (errors, covered)
}

#[allow(clippy::too_many_arguments)]
fn bind_match_pattern_types(
    pattern: &MatchPattern,
    scrutinee_ty: &Type,
    def_terms: &HashMap<DefId, Type>,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    def_table: &DefTable,
    ctx: &ResolvedContext,
    errors: &mut Vec<TypeCheckError>,
    covered: &mut HashSet<NodeId>,
    pattern_id: NodeId,
) {
    match pattern {
        MatchPattern::Wildcard { .. } => {}
        MatchPattern::BoolLit { .. } => {
            if super::term_utils::is_unresolved(scrutinee_ty) {
                let _ = unifier.unify(scrutinee_ty, &Type::Bool);
            }
        }
        MatchPattern::IntLit { .. } => {
            if super::term_utils::is_unresolved(scrutinee_ty) {
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
            if let Ok(pat_ty) = resolve_type_expr(def_table, ctx, ty_expr) {
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
                    } else if !super::term_utils::is_unresolved(&pat_ty) {
                        let variant_names = variants
                            .iter()
                            .map(super::diag_utils::compact_type_name)
                            .collect::<Vec<_>>();
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
                } else if let Some(term) = def_terms.get(def_id) {
                    let _ = unifier.unify(term, &pat_ty);
                }
            } else if let Some(term) = def_terms.get(def_id) {
                let _ = unifier.unify(term, scrutinee_ty);
            }
        }
        MatchPattern::Tuple { patterns, .. } => {
            if let Type::Tuple { field_tys } = scrutinee_ty {
                for (child, child_ty) in patterns.iter().zip(field_tys.iter()) {
                    bind_match_pattern_types(
                        child, child_ty, def_terms, unifier, type_defs, def_table, ctx, errors,
                        covered, pattern_id,
                    );
                }
            } else if super::term_utils::is_unresolved(scrutinee_ty) {
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
                        child, child_ty, def_terms, unifier, type_defs, def_table, ctx, errors,
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
            let owner_ty = super::term_utils::peel_heap(scrutinee_ty.clone());
            let mut matched_variant = None;

            if let Type::Enum { variants, .. } = owner_ty.clone() {
                matched_variant = variants.iter().find(|v| v.name == *variant_name).cloned();
            } else if super::term_utils::is_unresolved(&owner_ty) {
                let inferred_enum = resolve_pattern_enum_type(
                    *id, enum_name, type_args, type_defs, def_table, ctx, unifier,
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

fn resolve_pattern_enum_type(
    pattern_id: NodeId,
    enum_name: &Option<String>,
    type_args: &[crate::core::tree::resolved::TypeExpr],
    type_defs: &HashMap<String, Type>,
    def_table: &DefTable,
    ctx: &ResolvedContext,
    unifier: &mut TcUnifier,
) -> Option<Type> {
    if let Some(def_id) = def_table.lookup_node_def_id(pattern_id) {
        let type_def = ctx.module.type_def_by_id(def_id)?;
        let resolved_args = if type_args.is_empty() {
            type_def
                .type_params
                .iter()
                .map(|_| Type::Var(unifier.vars_mut().fresh_infer_local()))
                .collect::<Vec<_>>()
        } else {
            type_args
                .iter()
                .map(|arg| resolve_type_expr(def_table, ctx, arg))
                .collect::<Result<Vec<_>, _>>()
                .ok()?
        };

        let ty = resolve_type_def_with_args(def_table, ctx, def_id, &resolved_args).ok()?;
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
            ty if super::term_utils::is_unresolved(ty) => None,
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
            ty if super::term_utils::is_unresolved(ty) => None,
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
                if let Some(type_def_id) =
                    super::access_utils::type_def_id_for_nominal_name(name, type_symbols)
                    && super::access_utils::is_external_opaque_access(
                        caller_def_id,
                        type_def_id,
                        def_table,
                        def_owners,
                    )
                {
                    let diag_name = def_table
                        .lookup_def(type_def_id)
                        .map(|def| def.name.clone())
                        .unwrap_or_else(|| super::diag_utils::compact_nominal_name(name));
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
            ty if super::term_utils::is_unresolved(ty) => None,
            _ => Some(
                TypeCheckErrorKind::PatternTypeMismatch(pattern.clone(), value_ty.clone(), span)
                    .into(),
            ),
        },
    }
}
