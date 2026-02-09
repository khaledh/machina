//! Internal capability checks used by typecheck diagnostics.
//!
//! V1 keeps these checks intentionally conservative and aligned with
//! currently-supported lowering/runtime behavior.

use crate::types::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Capability {
    Equatable,
    Hashable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CapabilityErrorPath {
    pub(crate) path: String,
    pub(crate) failing_ty: Type,
}

pub(crate) fn ensure_equatable(ty: &Type) -> Result<(), CapabilityErrorPath> {
    ensure_capability(ty, Capability::Equatable)
}

pub(crate) fn ensure_hashable(ty: &Type) -> Result<(), CapabilityErrorPath> {
    ensure_capability(ty, Capability::Hashable)
}

fn ensure_capability(ty: &Type, capability: Capability) -> Result<(), CapabilityErrorPath> {
    check_capability_at_path(ty, capability, "value")
}

fn check_capability_at_path(
    ty: &Type,
    capability: Capability,
    path: &str,
) -> Result<(), CapabilityErrorPath> {
    if supports_capability_leaf_v1(ty, capability) {
        return Ok(());
    }

    if matches!(ty, Type::Unknown | Type::Var(_)) {
        return Ok(());
    }

    match ty {
        Type::Unit => Ok(()),
        Type::Tuple { field_tys } => {
            if capability == Capability::Hashable {
                return Err(CapabilityErrorPath {
                    path: path.to_string(),
                    failing_ty: ty.clone(),
                });
            }
            for (index, field_ty) in field_tys.iter().enumerate() {
                let field_path = format!("{path}.{index}");
                check_capability_at_path(field_ty, capability, &field_path)?;
            }
            Ok(())
        }
        Type::Struct { fields, .. } => {
            if capability == Capability::Hashable {
                return Err(CapabilityErrorPath {
                    path: path.to_string(),
                    failing_ty: ty.clone(),
                });
            }
            for field in fields {
                let field_path = format!("{path}.{}", field.name);
                check_capability_at_path(&field.ty, capability, &field_path)?;
            }
            Ok(())
        }
        Type::Array { elem_ty, .. } => {
            if capability == Capability::Hashable {
                return Err(CapabilityErrorPath {
                    path: path.to_string(),
                    failing_ty: ty.clone(),
                });
            }
            let elem_path = format!("{path}[]");
            check_capability_at_path(elem_ty, capability, &elem_path)?;
            Ok(())
        }
        Type::Enum { variants, .. } => {
            if capability == Capability::Hashable {
                return Err(CapabilityErrorPath {
                    path: path.to_string(),
                    failing_ty: ty.clone(),
                });
            }
            for variant in variants {
                for (index, payload_ty) in variant.payload.iter().enumerate() {
                    let payload_path = format!("{path}::{}[{index}]", variant.name);
                    check_capability_at_path(payload_ty, capability, &payload_path)?;
                }
            }
            Ok(())
        }
        Type::Int { .. } | Type::Bool | Type::Char | Type::Unknown | Type::Var(_) => Ok(()),
        Type::Fn { .. }
        | Type::ErrorUnion { .. }
        | Type::String
        | Type::DynArray { .. }
        | Type::Set { .. }
        | Type::Map { .. }
        | Type::Slice { .. }
        | Type::Heap { .. }
        | Type::Ref { .. }
        | Type::Range { .. } => Err(CapabilityErrorPath {
            path: path.to_string(),
            failing_ty: ty.clone(),
        }),
    }
}

fn supports_capability_leaf_v1(ty: &Type, capability: Capability) -> bool {
    match capability {
        Capability::Equatable => {
            matches!(
                ty,
                Type::Int { .. } | Type::Bool | Type::Char | Type::String
            )
        }
        Capability::Hashable => matches!(ty, Type::Int { .. } | Type::Bool | Type::Char),
    }
}
