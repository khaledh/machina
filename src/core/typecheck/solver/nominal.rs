//! Nominal/aggregate obligation solving.
//!
//! Handles obligations that depend on nominal type structure and visibility:
//! enum payload checks, struct construction/update, tuple/struct field access,
//! and property-backed field assignment.

use std::collections::{HashMap, HashSet};

use crate::core::capsule::ModuleId;
use crate::core::resolve::{DefId, DefTable};
use crate::core::tree::NodeId;
use crate::core::typecheck::builtin_methods;
use crate::core::typecheck::constraints::ExprObligation;
use crate::core::typecheck::engine::{CollectedPropertySig, CollectedTraitSig};
use crate::core::typecheck::errors::{TypeCheckError, TEK};
use crate::core::typecheck::unify::TcUnifier;
use crate::core::types::{TyVarId, Type, TypeAssignability, type_assignable};

#[allow(clippy::too_many_arguments)]
pub(super) fn try_check_expr_obligation_nominal(
    obligation: &ExprObligation,
    unifier: &mut TcUnifier,
    def_table: &DefTable,
    type_defs: &HashMap<String, Type>,
    type_symbols: &HashMap<String, DefId>,
    def_owners: &HashMap<DefId, ModuleId>,
    property_sigs: &HashMap<String, HashMap<String, CollectedPropertySig>>,
    trait_sigs: &HashMap<String, CollectedTraitSig>,
    var_trait_bounds: &HashMap<TyVarId, Vec<String>>,
    errors: &mut Vec<TypeCheckError>,
    covered_exprs: &mut HashSet<NodeId>,
) -> bool {
    match obligation {
        ExprObligation::EnumVariantPayload {
            expr_id,
            enum_name,
            variant,
            payload,
            span,
            ..
        } => {
            let Some(Type::Enum { variants, .. }) = type_defs.get(enum_name) else {
                return true;
            };
            let Some(expected_variant) = variants.iter().find(|v| v.name == *variant) else {
                return true;
            };
            if payload.len() != expected_variant.payload.len() {
                crate::core::typecheck::tc_push_error!(errors, *span, TEK::EnumVariantPayloadArityMismatch(
                        variant.clone(),
                        expected_variant.payload.len(),
                        payload.len(),
                    ));
                covered_exprs.insert(*expr_id);
                return true;
            }
            for (idx, (expected_ty, found_term)) in expected_variant
                .payload
                .iter()
                .zip(payload.iter())
                .enumerate()
            {
                let found_ty = super::term_utils::resolve_term(found_term, unifier);
                if matches!(
                    type_assignable(&found_ty, expected_ty),
                    TypeAssignability::Incompatible
                ) && !super::term_utils::is_unresolved(&found_ty)
                {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::EnumVariantPayloadTypeMismatch(
                            variant.clone(),
                            idx,
                            expected_ty.clone(),
                            found_ty,
                        ));
                    covered_exprs.insert(*expr_id);
                    break;
                }
            }
            true
        }
        ExprObligation::StructConstruct {
            expr_id,
            type_name,
            caller_def_id,
            span,
        } => {
            if let Some(type_def_id) =
                super::access_utils::type_def_id_for_nominal_name(type_name, type_symbols)
                && super::access_utils::is_external_opaque_access(
                    *caller_def_id,
                    type_def_id,
                    def_table,
                    def_owners,
                )
            {
                let diag_name = def_table
                    .lookup_def(type_def_id)
                    .map(|def| def.name.clone())
                    .unwrap_or_else(|| super::diag_utils::compact_nominal_name(type_name));
                crate::core::typecheck::tc_push_error!(errors, *span, TEK::OpaqueTypeConstruction(diag_name));
                covered_exprs.insert(*expr_id);
            }
            true
        }
        ExprObligation::StructUpdate {
            expr_id,
            target,
            fields,
            result,
            caller_def_id,
            span,
        } => {
            let target_ty = super::term_utils::resolve_term(target, unifier);
            let target_ty_for_diag = super::term_utils::default_infer_ints_for_diagnostics(
                target_ty.clone(),
                unifier.vars(),
            );
            match &target_ty_for_diag {
                Type::Struct {
                    name,
                    fields: struct_fields,
                } => {
                    if let Some(type_def_id) =
                        super::access_utils::type_def_id_for_nominal_name(name, type_symbols)
                        && super::access_utils::is_external_opaque_access(
                            *caller_def_id,
                            type_def_id,
                            def_table,
                            def_owners,
                        )
                    {
                        let diag_name = def_table
                            .lookup_def(type_def_id)
                            .map(|def| def.name.clone())
                            .unwrap_or_else(|| super::diag_utils::compact_nominal_name(name));
                        let field_name = fields
                            .first()
                            .map(|(field, _)| field.clone())
                            .unwrap_or_else(|| "<update>".to_string());
                        crate::core::typecheck::tc_push_error!(errors, *span, TEK::OpaqueFieldAccess(diag_name, field_name));
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    for (field_name, found_term) in fields {
                        let found_ty = super::term_utils::resolve_term(found_term, unifier);
                        let Some(expected_field) =
                            struct_fields.iter().find(|field| field.name == *field_name)
                        else {
                            continue;
                        };
                        if let Err(_) = super::assignability::solve_assignable(
                            &found_ty,
                            &expected_field.ty,
                            unifier,
                        ) {
                            let found_ty =
                                super::term_utils::canonicalize_type(unifier.apply(&found_ty));
                            if super::term_utils::is_unresolved(&found_ty) {
                                continue;
                            }
                            crate::core::typecheck::tc_push_error!(errors, *span, TEK::StructFieldTypeMismatch(
                                    field_name.clone(),
                                    expected_field.ty.clone(),
                                    found_ty,
                                ));
                            covered_exprs.insert(*expr_id);
                            break;
                        }
                    }
                    let _ = unifier.unify(result, &target_ty);
                }
                ty if super::term_utils::is_unresolved(ty) => {}
                _ => {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::InvalidStructUpdateTarget(target_ty_for_diag));
                    covered_exprs.insert(*expr_id);
                }
            }
            true
        }
        ExprObligation::TupleField {
            expr_id,
            target,
            index,
            result,
            span,
        } => {
            let target_ty = super::term_utils::resolve_term(target, unifier);
            let tuple_target_ty = super::term_utils::peel_heap(target_ty.clone());
            match &tuple_target_ty {
                Type::Tuple { field_tys } => {
                    if *index >= field_tys.len() {
                        crate::core::typecheck::tc_push_error!(errors, *span, TEK::TupleFieldOutOfBounds(field_tys.len(), *index));
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    let _ = unifier.unify(result, &field_tys[*index]);
                }
                ty if super::term_utils::is_unresolved(ty) => {}
                _ => {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::InvalidTupleFieldTarget(tuple_target_ty));
                    covered_exprs.insert(*expr_id);
                }
            }
            true
        }
        ExprObligation::StructField {
            expr_id,
            target,
            field,
            result,
            caller_def_id,
            span,
        } => {
            let target_ty = super::term_utils::resolve_term(target, unifier);
            let owner_ty = super::term_utils::peel_heap(target_ty.clone());
            match super::resolve_property_access(
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
                        crate::core::typecheck::tc_push_error!(errors, *span, TEK::PropertyNotReadable(field.clone()));
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    let _ = unifier.unify(result, &prop.ty);
                    return true;
                }
                Err(super::PropertyResolution::Ambiguous) => {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::OverloadAmbiguous(field.clone()));
                    covered_exprs.insert(*expr_id);
                    return true;
                }
                Err(super::PropertyResolution::Private) => {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::PropertyNotAccessible(field.clone()));
                    covered_exprs.insert(*expr_id);
                    return true;
                }
                Err(super::PropertyResolution::Missing) => {}
            }
            if let Some(prop) = builtin_methods::resolve_builtin_property(&owner_ty, field) {
                if !prop.readable {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::PropertyNotReadable(field.clone()));
                    covered_exprs.insert(*expr_id);
                    return true;
                }
                let _ = unifier.unify(result, &prop.ty);
                return true;
            }
            match &owner_ty {
                Type::Struct { name, fields } => {
                    if let Some(type_def_id) =
                        super::access_utils::type_def_id_for_nominal_name(name, type_symbols)
                        && super::access_utils::is_external_opaque_access(
                            *caller_def_id,
                            type_def_id,
                            def_table,
                            def_owners,
                        )
                    {
                        let diag_name = def_table
                            .lookup_def(type_def_id)
                            .map(|def| def.name.clone())
                            .unwrap_or_else(|| super::diag_utils::compact_nominal_name(name));
                        crate::core::typecheck::tc_push_error!(errors, *span, TEK::OpaqueFieldAccess(diag_name, field.clone()));
                        covered_exprs.insert(*expr_id);
                        return true;
                    }
                    if let Some(struct_field) = fields.iter().find(|f| f.name == *field) {
                        let _ = unifier.unify(result, &struct_field.ty);
                    } else {
                        let _ = unifier.unify(result, &Type::Unknown);
                    }
                }
                ty if super::term_utils::is_unresolved(ty) => {}
                _ => {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::InvalidStructFieldTarget(owner_ty));
                    covered_exprs.insert(*expr_id);
                }
            }
            true
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
            let target_ty = super::term_utils::resolve_term(target, unifier);
            let value_ty = super::term_utils::resolve_term(value, unifier);
            let owner_ty = super::term_utils::peel_heap(target_ty.clone());

            match super::resolve_property_access(
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
                        crate::core::typecheck::tc_push_error!(errors, *span, TEK::PropertyNotWritable(field.clone()));
                        covered_exprs.insert(*stmt_id);
                        return true;
                    }
                    let _ = unifier.unify(assignee, &prop.ty);
                    if let Err(_) =
                        super::assignability::solve_assignable(&value_ty, &prop.ty, unifier)
                    {
                        let value_ty =
                            super::term_utils::canonicalize_type(unifier.apply(&value_ty));
                        if super::term_utils::is_unresolved(&value_ty) {
                            return true;
                        }
                        crate::core::typecheck::tc_push_error!(errors, *span, TEK::AssignTypeMismatch(prop.ty, value_ty));
                        covered_exprs.insert(*stmt_id);
                    }
                    return true;
                }
                Err(super::PropertyResolution::Ambiguous) => {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::OverloadAmbiguous(field.clone()));
                    covered_exprs.insert(*stmt_id);
                    return true;
                }
                Err(super::PropertyResolution::Private) => {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::PropertyNotAccessible(field.clone()));
                    covered_exprs.insert(*stmt_id);
                    return true;
                }
                Err(super::PropertyResolution::Missing) => {}
            }

            if let Some(prop) = builtin_methods::resolve_builtin_property(&owner_ty, field) {
                if !prop.writable {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::PropertyNotWritable(field.clone()));
                    covered_exprs.insert(*stmt_id);
                    return true;
                }
                let _ = unifier.unify(assignee, &prop.ty);
                if let Err(_) = super::assignability::solve_assignable(&value_ty, &prop.ty, unifier)
                {
                    let value_ty = super::term_utils::canonicalize_type(unifier.apply(&value_ty));
                    if super::term_utils::is_unresolved(&value_ty) {
                        return true;
                    }
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::AssignTypeMismatch(prop.ty, value_ty));
                    covered_exprs.insert(*stmt_id);
                }
                return true;
            }

            match &owner_ty {
                Type::Struct { name, fields } => {
                    if let Some(type_def_id) =
                        super::access_utils::type_def_id_for_nominal_name(name, type_symbols)
                        && super::access_utils::is_external_opaque_access(
                            *caller_def_id,
                            type_def_id,
                            def_table,
                            def_owners,
                        )
                    {
                        let diag_name = def_table
                            .lookup_def(type_def_id)
                            .map(|def| def.name.clone())
                            .unwrap_or_else(|| super::diag_utils::compact_nominal_name(name));
                        crate::core::typecheck::tc_push_error!(errors, *span, TEK::OpaqueFieldAccess(diag_name, field.clone()));
                        covered_exprs.insert(*stmt_id);
                        return true;
                    }
                    if let Some(struct_field) = fields.iter().find(|f| f.name == *field) {
                        let _ = unifier.unify(assignee, &struct_field.ty);
                        if let Err(_) = super::assignability::solve_assignable(
                            &value_ty,
                            &struct_field.ty,
                            unifier,
                        ) {
                            let value_ty =
                                super::term_utils::canonicalize_type(unifier.apply(&value_ty));
                            if super::term_utils::is_unresolved(&value_ty) {
                                return true;
                            }
                            crate::core::typecheck::tc_push_error!(errors, *span, TEK::AssignTypeMismatch(
                                    struct_field.ty.clone(),
                                    value_ty,
                                ));
                            covered_exprs.insert(*stmt_id);
                        }
                    } else {
                        let _ = unifier.unify(assignee, &Type::Unknown);
                    }
                }
                ty if super::term_utils::is_unresolved(ty) => {}
                _ => {
                    crate::core::typecheck::tc_push_error!(errors, *span, TEK::InvalidStructFieldTarget(owner_ty));
                    covered_exprs.insert(*stmt_id);
                }
            }
            true
        }
        _ => false,
    }
}
