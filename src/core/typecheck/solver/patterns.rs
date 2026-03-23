//! Pattern obligation solving.
//!
//! This module contains both bind-pattern validation and match-pattern
//! inference/binding logic used by the solver pre-pass and late diagnostics.

use std::collections::{HashMap, HashSet};

use crate::core::ast::{
    BindPattern, BindPatternKind, Expr, ExprKind, MatchArm, MatchPattern, MatchPatternBinding,
    MethodItem, NodeId, TopLevelItem,
    visit::{Visitor, walk_expr},
};
use crate::core::capsule::ModuleId;
use crate::core::context::ResolvedContext;
use crate::core::diag::Span;
use crate::core::resolve::{DefId, DefTable};
use crate::core::typecheck::constraints::PatternObligation;
use crate::core::typecheck::errors::{TEK, TypeCheckError};
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
    resolved_call_defs: &HashMap<NodeId, DefId>,
    def_owners: &HashMap<DefId, ModuleId>,
    ctx: &ResolvedContext,
    allow_missing_def_ids: bool,
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
                let value_ty = super::term_utils::resolve_term(value_ty, unifier);
                bind_bind_pattern_types(pattern, &value_ty, def_terms, unifier, def_table);
                let value_ty = super::term_utils::resolve_term_for_diagnostics(&value_ty, unifier);
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
                scrutinee_expr_id,
                scrutinee_ty,
                prior_patterns,
                caller_def_id: _,
                ..
            } => {
                let scrutinee_ty = super::term_utils::resolve_term(scrutinee_ty, unifier);
                bind_match_pattern_types(
                    pattern,
                    &scrutinee_ty,
                    prior_patterns,
                    false,
                    def_terms,
                    unifier,
                    type_defs,
                    def_table,
                    resolved_call_defs,
                    ctx,
                    &mut errors,
                    &mut covered,
                    *arm_id,
                    *scrutinee_expr_id,
                    allow_missing_def_ids,
                );
            }
        }
    }

    (errors, covered)
}

fn bind_bind_pattern_types(
    pattern: &BindPattern,
    value_ty: &Type,
    def_terms: &HashMap<DefId, Type>,
    unifier: &mut TcUnifier,
    def_table: &DefTable,
) {
    match &pattern.kind {
        BindPatternKind::Wildcard => {}
        BindPatternKind::Name { .. } => {
            let def_id = def_table.def_id(pattern.id);
            if let Some(term) = def_terms.get(&def_id) {
                let _ = unifier.unify(term, value_ty);
            }
        }
        BindPatternKind::Tuple { patterns } => {
            if let Type::Tuple { field_tys } = value_ty {
                for (child, child_ty) in patterns.iter().zip(field_tys.iter()) {
                    bind_bind_pattern_types(child, child_ty, def_terms, unifier, def_table);
                }
            } else if super::term_utils::is_unresolved(value_ty) {
                let field_tys = patterns
                    .iter()
                    .map(|_| Type::Var(unifier.vars_mut().fresh_infer_local()))
                    .collect::<Vec<_>>();
                let tuple_ty = Type::Tuple {
                    field_tys: field_tys.clone(),
                };
                let _ = unifier.unify(value_ty, &tuple_ty);
                for (child, child_ty) in patterns.iter().zip(field_tys.iter()) {
                    bind_bind_pattern_types(child, child_ty, def_terms, unifier, def_table);
                }
            }
        }
        BindPatternKind::Array {
            prefix,
            rest,
            suffix,
        } => {
            let fixed_len = prefix.len() + suffix.len();
            match value_ty {
                Type::Array { elem_ty, dims } => {
                    let child_ty = if dims.len() <= 1 {
                        (**elem_ty).clone()
                    } else {
                        Type::Array {
                            elem_ty: elem_ty.clone(),
                            dims: dims[1..].to_vec(),
                        }
                    };
                    bind_array_pattern_types(
                        prefix,
                        rest.as_ref(),
                        suffix,
                        &child_ty,
                        def_terms,
                        unifier,
                        def_table,
                    );
                }
                Type::DynArray { elem_ty } => {
                    bind_array_pattern_types(
                        prefix,
                        rest.as_ref(),
                        suffix,
                        elem_ty,
                        def_terms,
                        unifier,
                        def_table,
                    );
                }
                ty if super::term_utils::is_unresolved(ty) && rest.is_none() => {
                    let elem_ty = Type::Var(unifier.vars_mut().fresh_infer_local());
                    let array_ty = Type::Array {
                        elem_ty: Box::new(elem_ty.clone()),
                        dims: vec![fixed_len],
                    };
                    let _ = unifier.unify(value_ty, &array_ty);
                    bind_array_pattern_types(
                        prefix, None, suffix, &elem_ty, def_terms, unifier, def_table,
                    );
                }
                _ => {}
            }
        }
        BindPatternKind::Struct { fields, .. } => {
            if let Type::Struct {
                fields: struct_fields,
                ..
            } = value_ty
            {
                for field in fields {
                    if let Some(struct_field) = struct_fields.iter().find(|f| f.name == field.name)
                    {
                        bind_bind_pattern_types(
                            &field.pattern,
                            &struct_field.ty,
                            def_terms,
                            unifier,
                            def_table,
                        );
                    }
                }
            }
        }
    }
}

fn bind_array_pattern_types(
    prefix: &[BindPattern],
    rest: Option<&crate::core::ast::ArrayRestBindPattern>,
    suffix: &[BindPattern],
    child_ty: &Type,
    def_terms: &HashMap<DefId, Type>,
    unifier: &mut TcUnifier,
    def_table: &DefTable,
) {
    for child in prefix.iter().chain(suffix.iter()) {
        bind_bind_pattern_types(child, child_ty, def_terms, unifier, def_table);
    }
    if let Some(rest) = rest
        && let Some(rest_pattern) = &rest.pattern
    {
        bind_bind_pattern_types(
            rest_pattern,
            &Type::Slice {
                elem_ty: Box::new(child_ty.clone()),
            },
            def_terms,
            unifier,
            def_table,
        );
    }
}

#[allow(clippy::too_many_arguments)]
fn bind_match_pattern_types(
    pattern: &MatchPattern,
    scrutinee_ty: &Type,
    prior_patterns: &[MatchPattern],
    allow_unresolved_scrutinee_binding: bool,
    def_terms: &HashMap<DefId, Type>,
    unifier: &mut TcUnifier,
    type_defs: &HashMap<String, Type>,
    def_table: &DefTable,
    resolved_call_defs: &HashMap<NodeId, DefId>,
    ctx: &ResolvedContext,
    errors: &mut Vec<TypeCheckError>,
    covered: &mut HashSet<NodeId>,
    pattern_id: NodeId,
    scrutinee_expr_id: NodeId,
    allow_missing_def_ids: bool,
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
        MatchPattern::Binding { id, .. } => {
            if let Some(def_id) = pattern_def_id(def_table, *id, allow_missing_def_ids)
                && let Some(term) = def_terms.get(&def_id)
            {
                if let Some(binding_ty) = union_binding_remainder_type(
                    scrutinee_ty,
                    prior_patterns,
                    def_table,
                    ctx,
                    unifier,
                ) {
                    let _ = unifier.unify(term, &binding_ty);
                } else if allow_unresolved_scrutinee_binding
                    || !super::term_utils::is_unresolved(scrutinee_ty)
                {
                    let _ = unifier.unify(term, scrutinee_ty);
                }
            }
        }
        MatchPattern::TypedBinding {
            id, ty_expr, span, ..
        } => {
            let def_id = pattern_def_id(def_table, *id, allow_missing_def_ids);
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
                        if let Some(def_id) = def_id
                            && let Some(term) = def_terms.get(&def_id)
                        {
                            let _ = unifier.unify(term, &pat_ty);
                        }
                    } else if !super::term_utils::is_unresolved(&pat_ty) {
                        if should_defer_forwarded_call_mismatch(
                            scrutinee_expr_id,
                            resolved_call_defs,
                            def_table,
                            ctx,
                        ) {
                            return;
                        }
                        let variant_names = variants
                            .iter()
                            .map(super::diag_utils::compact_type_name)
                            .collect::<Vec<_>>();
                        crate::core::typecheck::tc_push_error!(
                            errors,
                            *span,
                            TEK::MatchTypedBindingTypeMismatch(variant_names, pat_ty.clone(),)
                        );
                        covered.insert(*id);
                        covered.insert(pattern_id);
                    }
                } else if let Some(def_id) = def_id
                    && let Some(term) = def_terms.get(&def_id)
                {
                    let _ = unifier.unify(term, &pat_ty);
                }
            } else if let Some(def_id) = def_id
                && let Some(term) = def_terms.get(&def_id)
            {
                let _ = unifier.unify(term, scrutinee_ty);
            }
        }
        MatchPattern::Tuple { patterns, .. } => {
            if let Type::Tuple { field_tys } = scrutinee_ty {
                for (child, child_ty) in patterns.iter().zip(field_tys.iter()) {
                    bind_match_pattern_types(
                        child,
                        child_ty,
                        &[],
                        true,
                        def_terms,
                        unifier,
                        type_defs,
                        def_table,
                        resolved_call_defs,
                        ctx,
                        errors,
                        covered,
                        pattern_id,
                        scrutinee_expr_id,
                        allow_missing_def_ids,
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
                        child,
                        child_ty,
                        &[],
                        true,
                        def_terms,
                        unifier,
                        type_defs,
                        def_table,
                        resolved_call_defs,
                        ctx,
                        errors,
                        covered,
                        pattern_id,
                        scrutinee_expr_id,
                        allow_missing_def_ids,
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
                    if let MatchPatternBinding::Named { id, .. } = binding
                        && let Some(def_id) = pattern_def_id(def_table, *id, allow_missing_def_ids)
                        && let Some(term) = def_terms.get(&def_id)
                    {
                        let _ = unifier.unify(term, payload_ty);
                    }
                }
            }
        }
    }
}

fn should_defer_forwarded_call_mismatch(
    scrutinee_expr_id: NodeId,
    resolved_call_defs: &HashMap<NodeId, DefId>,
    def_table: &DefTable,
    ctx: &ResolvedContext,
) -> bool {
    let Some(def_id) = resolved_call_defs.get(&scrutinee_expr_id).copied() else {
        return false;
    };
    let Some(callable) = find_callable_def(&ctx.module, def_table, def_id) else {
        return false;
    };
    callable.is_generic && callable.has_direct_forwarding
}

struct CallablePatternInfo {
    is_generic: bool,
    has_direct_forwarding: bool,
}

fn find_callable_def(
    module: &crate::core::ast::Module,
    def_table: &DefTable,
    def_id: DefId,
) -> Option<CallablePatternInfo> {
    for item in &module.top_level_items {
        match item {
            TopLevelItem::FuncDef(func) if def_table.def_id(func.id) == def_id => {
                return Some(CallablePatternInfo {
                    is_generic: !func.sig.type_params.is_empty(),
                    has_direct_forwarding: expr_has_direct_forwarding(&func.body, def_table),
                });
            }
            TopLevelItem::MethodBlock(block) => {
                for method_item in &block.method_items {
                    if let MethodItem::Def(method) = method_item
                        && def_table.def_id(method.id) == def_id
                    {
                        let receiver_is_generic = block.type_args.iter().any(|type_arg| {
                            def_table
                                .lookup_node_def_id(type_arg.id)
                                .and_then(|type_arg_def_id| def_table.lookup_def(type_arg_def_id))
                                .is_some_and(|def| {
                                    matches!(
                                        def.kind,
                                        crate::core::resolve::DefKind::TypeParam { .. }
                                    )
                                })
                        });
                        return Some(CallablePatternInfo {
                            is_generic: receiver_is_generic || !method.sig.type_params.is_empty(),
                            has_direct_forwarding: expr_has_direct_forwarding(
                                &method.body,
                                def_table,
                            ),
                        });
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn expr_has_direct_forwarding(expr: &Expr, def_table: &DefTable) -> bool {
    struct Finder<'a> {
        def_table: &'a DefTable,
        found: bool,
    }

    impl Visitor for Finder<'_> {
        fn visit_expr(&mut self, expr: &Expr) {
            if self.found {
                return;
            }
            if let ExprKind::Match { arms, .. } = &expr.kind
                && arms
                    .iter()
                    .any(|arm| is_direct_forwarding_arm(arm, self.def_table))
            {
                self.found = true;
                return;
            }
            walk_expr(self, expr);
        }
    }

    let mut finder = Finder {
        def_table,
        found: false,
    };
    finder.visit_expr(expr);
    finder.found
}

fn is_direct_forwarding_arm(arm: &MatchArm, def_table: &DefTable) -> bool {
    if arm.patterns.len() != 1 {
        return false;
    }
    let MatchPattern::Binding { id, ident, .. } = &arm.patterns[0] else {
        return false;
    };
    let ExprKind::Var { ident: body_ident } = &arm.body.kind else {
        return false;
    };
    if ident != body_ident {
        return false;
    }
    match (
        def_table.lookup_node_def_id(*id),
        def_table.lookup_node_def_id(arm.body.id),
    ) {
        (Some(pattern_def), Some(body_def)) => pattern_def == body_def,
        _ => true,
    }
}

fn union_binding_remainder_type(
    scrutinee_ty: &Type,
    prior_patterns: &[MatchPattern],
    def_table: &DefTable,
    ctx: &ResolvedContext,
    unifier: &mut TcUnifier,
) -> Option<Type> {
    let scrutinee_applied = unifier.apply(scrutinee_ty);
    let Type::ErrorUnion { .. } = &scrutinee_applied else {
        return None;
    };

    let matched = prior_patterns
        .iter()
        .filter_map(|pattern| match pattern {
            MatchPattern::TypedBinding { ty_expr, .. } => {
                resolve_type_expr(def_table, ctx, ty_expr)
                    .ok()
                    .map(|ty| unifier.apply(&ty))
            }
            _ => None,
        })
        .collect::<Vec<_>>();

    scrutinee_applied.error_union_remainder_excluding(&matched)
}

fn pattern_def_id(
    def_table: &DefTable,
    node_id: NodeId,
    allow_missing_def_ids: bool,
) -> Option<DefId> {
    if allow_missing_def_ids {
        def_table.lookup_node_def_id(node_id)
    } else {
        Some(def_table.def_id(node_id))
    }
}

fn resolve_pattern_enum_type(
    pattern_id: NodeId,
    enum_name: &Option<String>,
    type_args: &[crate::core::ast::TypeExpr],
    type_defs: &HashMap<String, Type>,
    def_table: &DefTable,
    ctx: &ResolvedContext,
    unifier: &mut TcUnifier,
) -> Option<Type> {
    if let Some(def_id) = def_table.lookup_node_def_id(pattern_id) {
        let type_def = ctx.module.type_def_by_id(def_table, def_id)?;
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
        BindPatternKind::Wildcard => None,
        BindPatternKind::Name { .. } => None,
        BindPatternKind::Tuple { patterns } => match value_ty {
            Type::Tuple { field_tys } => {
                if field_tys.len() != patterns.len() {
                    return Some(
                        TEK::TuplePatternLengthMismatch(field_tys.len(), patterns.len()).at(span),
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
            _ => Some(TEK::PatternTypeMismatch(pattern.clone(), value_ty.clone()).at(span)),
        },
        BindPatternKind::Array {
            prefix,
            rest,
            suffix,
        } => match value_ty {
            Type::Array { elem_ty, dims } => {
                let expected_len = dims.first().copied().unwrap_or(0);
                let fixed_len = prefix.len() + suffix.len();
                if rest.is_some() {
                    if expected_len < fixed_len {
                        return Some(
                            TEK::ArrayPatternMinLengthMismatch(fixed_len, expected_len).at(span),
                        );
                    }
                } else if expected_len != fixed_len {
                    return Some(TEK::ArrayPatternLengthMismatch(expected_len, fixed_len).at(span));
                }

                let child_ty = if dims.len() <= 1 {
                    (**elem_ty).clone()
                } else {
                    Type::Array {
                        elem_ty: elem_ty.clone(),
                        dims: dims[1..].to_vec(),
                    }
                };
                if let Some(error) = check_array_bind_children(
                    prefix,
                    rest.as_ref(),
                    suffix,
                    &child_ty,
                    caller_def_id,
                    type_symbols,
                    def_table,
                    def_owners,
                ) {
                    return Some(error);
                }
                None
            }
            Type::DynArray { elem_ty } => {
                let child_ty = (**elem_ty).clone();
                check_array_bind_children(
                    prefix,
                    rest.as_ref(),
                    suffix,
                    &child_ty,
                    caller_def_id,
                    type_symbols,
                    def_table,
                    def_owners,
                )
            }
            ty if super::term_utils::is_unresolved(ty) => None,
            _ => Some(TEK::PatternTypeMismatch(pattern.clone(), value_ty.clone()).at(span)),
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
                    return Some(TEK::OpaquePatternDestructure(diag_name).at(span));
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
            _ => Some(TEK::PatternTypeMismatch(pattern.clone(), value_ty.clone()).at(span)),
        },
    }
}

#[allow(clippy::too_many_arguments)]
fn check_array_bind_children(
    prefix: &[BindPattern],
    rest: Option<&crate::core::ast::ArrayRestBindPattern>,
    suffix: &[BindPattern],
    child_ty: &Type,
    caller_def_id: Option<DefId>,
    type_symbols: &HashMap<String, DefId>,
    def_table: &DefTable,
    def_owners: &HashMap<DefId, ModuleId>,
) -> Option<TypeCheckError> {
    for child in prefix.iter().chain(suffix.iter()) {
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
    if let Some(rest) = rest
        && let Some(rest_pattern) = &rest.pattern
        && let Some(error) = check_bind_pattern(
            rest_pattern,
            &Type::Slice {
                elem_ty: Box::new(child_ty.clone()),
            },
            rest_pattern.span,
            caller_def_id,
            type_symbols,
            def_table,
            def_owners,
        )
    {
        return Some(error);
    }
    None
}
