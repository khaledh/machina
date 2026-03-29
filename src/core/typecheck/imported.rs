//! Imported signature adapters for the collect pass.
//!
//! Imported aliases enter type checking as synthetic local defs backed by
//! source-keyed payloads from resolve. This module keeps the translation from
//! those imported payloads into collected callable/trait signatures in one
//! place so `collect.rs` can stay focused on local declarations.

use std::collections::HashMap;

use crate::core::context::ResolvedContext;
use crate::core::diag::Span;
use crate::core::resolve::{
    DefId, ImportedCallableSig, ImportedFacts, ImportedParamSig, ImportedResolvedMethodSig,
    ImportedTraitSig,
};
use crate::core::typecheck::engine::{
    CollectedCallableSig, CollectedParamSig, CollectedTraitMethodSig, CollectedTraitPropertySig,
    CollectedTraitSig,
};

pub(super) fn extend_imported_function_sigs(
    ctx: &ResolvedContext,
    imported_facts: &ImportedFacts,
    func_sigs: &mut HashMap<String, Vec<CollectedCallableSig>>,
) {
    // Imported callable aliases already have synthetic local defs in the
    // def-table; we only need to rehydrate their source-backed signatures.
    for (def_id, imported_sigs) in imported_facts.callable_entries() {
        let Some(def) = ctx.def_table.lookup_def(def_id) else {
            continue;
        };
        let out = func_sigs.entry(def.name.clone()).or_default();
        for imported in imported_sigs {
            out.push(imported_callable_sig_to_collected(def_id, imported));
        }
    }
}

pub(super) fn extend_imported_method_sigs(
    imported_facts: &ImportedFacts,
    method_sigs: &mut HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
) {
    for (owner, by_name) in imported_facts.method_entries() {
        let out = method_sigs.entry(owner.clone()).or_default();
        for (method_name, sigs) in by_name {
            let overloads = out.entry(method_name.clone()).or_default();
            for sig in sigs {
                overloads.push(imported_method_sig_to_collected(sig));
            }
        }
    }
}

pub(super) fn extend_imported_trait_sigs(
    ctx: &ResolvedContext,
    imported_facts: &ImportedFacts,
    trait_sigs: &mut HashMap<String, CollectedTraitSig>,
) {
    // Traits follow the same pattern: alias-local def id, source-keyed payload.
    for (def_id, imported) in imported_facts.trait_entries() {
        let Some(def) = ctx.def_table.lookup_def(def_id) else {
            continue;
        };
        trait_sigs.insert(
            def.name.clone(),
            imported_trait_sig_to_collected(def_id, imported),
        );
    }
}

fn imported_callable_sig_to_collected(
    def_id: DefId,
    imported: &ImportedCallableSig,
) -> CollectedCallableSig {
    CollectedCallableSig {
        def_id,
        self_ty: None,
        params: imported_params_to_collected(&imported.params),
        ret_ty: imported.ret_ty.clone(),
        type_param_count: imported.type_param_count,
        type_param_var_names: imported.type_param_var_names.clone(),
        type_param_bounds: imported.type_param_bounds.clone(),
        self_mode: None,
        impl_trait: None,
    }
}

fn imported_method_sig_to_collected(imported: &ImportedResolvedMethodSig) -> CollectedCallableSig {
    CollectedCallableSig {
        def_id: imported.def_id,
        self_ty: Some(imported.self_ty.clone()),
        params: imported_params_to_collected(&imported.params),
        ret_ty: imported.ret_ty.clone(),
        type_param_count: imported.type_param_count,
        type_param_var_names: imported.type_param_var_names.clone(),
        type_param_bounds: imported.type_param_bounds.clone(),
        self_mode: Some(imported.self_mode.clone()),
        impl_trait: None,
    }
}

fn imported_trait_sig_to_collected(
    def_id: DefId,
    imported: &ImportedTraitSig,
) -> CollectedTraitSig {
    let methods = imported
        .methods
        .iter()
        .map(|(name, method)| {
            (
                name.clone(),
                CollectedTraitMethodSig {
                    name: method.name.clone(),
                    params: imported_params_to_collected(&method.params),
                    ret_ty: method.ret_ty.clone(),
                    type_param_count: method.type_param_count,
                    type_param_bounds: method.type_param_bounds.clone(),
                    self_mode: method.self_mode.clone(),
                    span: Span::default(),
                },
            )
        })
        .collect();
    let properties = imported
        .properties
        .iter()
        .map(|(name, property)| {
            (
                name.clone(),
                CollectedTraitPropertySig {
                    name: property.name.clone(),
                    ty: property.ty.clone(),
                    has_get: property.has_get,
                    has_set: property.has_set,
                    span: Span::default(),
                },
            )
        })
        .collect();
    CollectedTraitSig {
        def_id,
        methods,
        properties,
        span: Span::default(),
    }
}

fn imported_params_to_collected(params: &[ImportedParamSig]) -> Vec<CollectedParamSig> {
    params
        .iter()
        .enumerate()
        .map(|(index, param)| CollectedParamSig {
            name: format!("arg{index}"),
            ty: param.ty.clone(),
            mode: param.mode.clone(),
            has_default: false,
        })
        .collect()
}
