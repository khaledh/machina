//! Canonical nominal identity and structural view types.
//!
//! This module separates nominal identity (`NominalKey`) from structural data
//! (`StructView` / `EnumView`) so callers can reason about identity and
//! expansion state independently.

use crate::core::resolve::DefId;
use crate::core::types::Type;

/// Canonical identity for a nominal type definition instance.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct NominalKey {
    pub(crate) def_id: DefId,
    pub(crate) type_args: Vec<Type>,
}

impl NominalKey {
    pub(crate) fn new(def_id: DefId, type_args: Vec<Type>) -> Self {
        Self { def_id, type_args }
    }
}

/// Indicates whether a nominal view is fully expanded or a recursion boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ExpansionState {
    Shallow,
    Expanded,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StructFieldView {
    pub(crate) name: String,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StructView {
    pub(crate) key: NominalKey,
    pub(crate) name: String,
    pub(crate) fields: Vec<StructFieldView>,
    pub(crate) state: ExpansionState,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EnumVariantView {
    pub(crate) name: String,
    pub(crate) payload: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EnumView {
    pub(crate) key: NominalKey,
    pub(crate) name: String,
    pub(crate) variants: Vec<EnumVariantView>,
    pub(crate) state: ExpansionState,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TypeView {
    Struct(StructView),
    Enum(EnumView),
}
