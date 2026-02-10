//! Shared property lookup for nominal and trait-bound owners.
//!
//! This module resolves the structural presence of a property (type + getter/
//! setter availability) without applying access-control policy. Solver/finalize
//! can then layer their own policy decisions on top.

use std::collections::HashMap;

use crate::resolve::DefId;
use crate::typecheck::engine::{CollectedPropertySig, CollectedTraitSig, lookup_property};
use crate::types::{TyVarId, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PropertyLookupError {
    Missing,
    Ambiguous,
}

#[derive(Debug, Clone)]
pub(crate) struct PropertyLookupResult {
    pub(crate) ty: Type,
    pub(crate) has_get: bool,
    pub(crate) has_set: bool,
    pub(crate) getter_def: Option<DefId>,
    pub(crate) setter_def: Option<DefId>,
}

pub(crate) fn lookup(
    owner_ty: &Type,
    field: &str,
    property_sigs: &HashMap<String, HashMap<String, CollectedPropertySig>>,
    trait_sigs: &HashMap<String, CollectedTraitSig>,
    var_trait_bounds: &HashMap<TyVarId, Vec<String>>,
) -> Result<PropertyLookupResult, PropertyLookupError> {
    if let Some(prop) = lookup_property(property_sigs, owner_ty, field) {
        return Ok(PropertyLookupResult {
            ty: prop.ty.clone(),
            has_get: prop.getter.is_some(),
            has_set: prop.setter.is_some(),
            getter_def: prop.getter,
            setter_def: prop.setter,
        });
    }

    let Type::Var(var) = owner_ty else {
        return Err(PropertyLookupError::Missing);
    };

    let mut matched: Option<PropertyLookupResult> = None;
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
            return Err(PropertyLookupError::Ambiguous);
        }
        matched = Some(PropertyLookupResult {
            ty: prop.ty.clone(),
            has_get: prop.has_get,
            has_set: prop.has_set,
            getter_def: None,
            setter_def: None,
        });
    }

    matched.ok_or(PropertyLookupError::Missing)
}
