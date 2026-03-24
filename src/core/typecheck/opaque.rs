//! Opaque iterable side tables.
//!
//! These tables let typecheck expose a protocol-facing type to source-level
//! users while preserving the concrete witness type in the main `TypeMap`.

use std::collections::HashMap;

use crate::core::ast::NodeId;
use crate::core::resolve::DefId;
use crate::core::types::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpaqueBinding {
    pub exposed_ty: Type,
    pub witness_ty: Type,
}

pub type OpaqueBindingMap = HashMap<DefId, OpaqueBinding>;
pub type ExposedTypeMap = HashMap<NodeId, Type>;
