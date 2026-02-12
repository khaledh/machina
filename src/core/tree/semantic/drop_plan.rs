//! Drop plans for SSA emission.
//!
//! These plans describe where drops should be emitted at scope exits and
//! control-flow edges, so lowering can focus on codegen.

use std::collections::HashMap;

use crate::core::resolve::DefId;
use crate::core::tree::NodeId;
use crate::core::types::TypeId;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DropGuard {
    /// Always emit a drop when leaving the scope.
    Always,
    /// Emit a drop only if the binding has been initialized.
    IfInitialized,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DropItem {
    pub def_id: DefId,
    pub ty: TypeId,
    pub guard: DropGuard,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DropScopePlan {
    pub drops: Vec<DropItem>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DropPlanMap {
    /// Drop actions to run when exiting a scope expression.
    scopes: HashMap<NodeId, DropScopePlan>,
    /// Drop depth to emit before a control-flow edge (break/continue/return).
    depths: HashMap<NodeId, usize>,
}

impl DropPlanMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_scope(&mut self, id: NodeId, scope: DropScopePlan) {
        self.scopes.insert(id, scope);
    }

    pub fn insert_depth(&mut self, id: NodeId, depth: usize) {
        self.depths.insert(id, depth);
    }

    pub fn scope_for(&self, id: NodeId) -> Option<&DropScopePlan> {
        self.scopes.get(&id)
    }

    pub fn depth_for(&self, id: NodeId) -> Option<usize> {
        self.depths.get(&id).copied()
    }
}
