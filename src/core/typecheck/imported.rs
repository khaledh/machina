//! Imported signature adapters for the collect pass.
//!
//! Imported aliases enter type checking as synthetic local defs backed by
//! source-keyed payloads from resolve. This module keeps the translation from
//! those imported payloads into collected callable/trait signatures in one
//! place so `collect.rs` can stay focused on local declarations.

use std::collections::{BTreeMap, HashMap};

use crate::core::diag::Span;
use crate::core::resolve::{
    DefId, ImportedCallableSig, ImportedFacts, ImportedParamSig, ImportedTraitSig,
};
use crate::core::typecheck::engine::{
    CollectedCallableSig, CollectedParamSig, CollectedTraitMethodSig, CollectedTraitPropertySig,
    CollectedTraitSig,
};

pub(super) fn extend_imported_function_sigs(
    ctx: &crate::core::context::ResolvedContext,
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

pub(super) fn extend_imported_trait_sigs(
    ctx: &crate::core::context::ResolvedContext,
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
        params: imported_params_to_collected(&imported.params),
        ret_ty: imported.ret_ty.clone(),
        type_param_count: 0,
        type_param_var_names: BTreeMap::new(),
        type_param_bounds: Vec::new(),
        self_mode: None,
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
        })
        .collect()
}
