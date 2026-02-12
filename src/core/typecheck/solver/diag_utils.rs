//! Shared diagnostic-shaping helpers for solver modules.

use crate::core::types::Type;

pub(super) fn compact_type_name(ty: &Type) -> String {
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

pub(super) fn compact_nominal_name(name: &str) -> String {
    name.split('<').next().unwrap_or(name).trim().to_string()
}

pub(super) fn error_union_variant_names(ty: &Type) -> Option<Vec<String>> {
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

pub(super) fn map_key_not_found_type() -> Type {
    Type::Struct {
        name: "KeyNotFound".to_string(),
        fields: Vec::new(),
    }
}
