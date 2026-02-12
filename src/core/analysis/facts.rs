//! Immutable facts with synthetic overlays.
//!
//! This module models the ownership contract used by query-based analysis:
//! base facts are immutable products from earlier stages, while later stages
//! attach synthetic facts in overlays without mutating the base.

use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

use crate::core::resolve::def_table::DefTable;
use crate::core::resolve::{Def, DefId};
use crate::core::tree::NodeId;
use crate::core::typecheck::type_map::TypeMap;
use crate::core::types::{Type, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntheticReason {
    NormalizeCoercion,
    ElaborateSyntheticNode,
    ClosureLowering,
    Other(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FactOrigin {
    Source,
    Synthetic {
        stage: String,
        reason: SyntheticReason,
    },
}

#[derive(Debug, Clone)]
struct OverlayValue<V> {
    value: V,
    origin: FactOrigin,
}

/// Read view over an immutable base map plus a mutable synthetic overlay.
#[derive(Debug, Clone)]
pub struct FactOverlayMap<K, V> {
    base: Arc<HashMap<K, V>>,
    overlay: HashMap<K, OverlayValue<V>>,
}

impl<K, V> FactOverlayMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Clone,
{
    pub fn new(base: HashMap<K, V>) -> Self {
        Self {
            base: Arc::new(base),
            overlay: HashMap::new(),
        }
    }

    pub fn with_shared_base(base: Arc<HashMap<K, V>>) -> Self {
        Self {
            base,
            overlay: HashMap::new(),
        }
    }

    pub fn base(&self) -> Arc<HashMap<K, V>> {
        Arc::clone(&self.base)
    }

    pub fn get(&self, key: &K) -> Option<V> {
        self.overlay
            .get(key)
            .map(|v| v.value.clone())
            .or_else(|| self.base.get(key).cloned())
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.overlay.contains_key(key) || self.base.contains_key(key)
    }

    pub fn origin(&self, key: &K) -> Option<FactOrigin> {
        if let Some(v) = self.overlay.get(key) {
            Some(v.origin.clone())
        } else if self.base.contains_key(key) {
            Some(FactOrigin::Source)
        } else {
            None
        }
    }

    pub fn insert_synthetic(
        &mut self,
        key: K,
        value: V,
        stage: impl Into<String>,
        reason: SyntheticReason,
    ) {
        self.overlay.insert(
            key,
            OverlayValue {
                value,
                origin: FactOrigin::Synthetic {
                    stage: stage.into(),
                    reason,
                },
            },
        );
    }

    pub fn clear_overlay(&mut self) {
        self.overlay.clear();
    }

    pub fn synthetic_len(&self) -> usize {
        self.overlay.len()
    }
}

/// Immutable-base wrapper for `TypeMap` with synthetic overlays.
///
/// Callers can read through `Deref<Target = TypeMap>`. Synthetic inserts are
/// routed through overlay methods that capture provenance.
#[derive(Debug, Clone)]
pub struct TypeMapOverlay {
    base: Arc<TypeMap>,
    working: TypeMap,
    base_def_ids: HashSet<DefId>,
    node_origin: HashMap<NodeId, FactOrigin>,
    def_origin: HashMap<DefId, FactOrigin>,
}

impl TypeMapOverlay {
    pub fn new(base: TypeMap) -> Self {
        let base = Arc::new(base);
        Self::with_shared_base(base)
    }

    pub fn with_shared_base(base: Arc<TypeMap>) -> Self {
        let base_def_ids = base.into_iter().map(|(def, _)| def.id).collect();
        Self {
            working: (*base).clone(),
            base,
            base_def_ids,
            node_origin: HashMap::new(),
            def_origin: HashMap::new(),
        }
    }

    pub fn insert_node_type(
        &mut self,
        node_id: NodeId,
        ty: Type,
        stage: impl Into<String>,
        reason: SyntheticReason,
    ) -> TypeId {
        self.node_origin.insert(
            node_id,
            FactOrigin::Synthetic {
                stage: stage.into(),
                reason,
            },
        );
        self.working.insert_node_type(node_id, ty)
    }

    pub fn insert_def_type(
        &mut self,
        def: Def,
        ty: Type,
        stage: impl Into<String>,
        reason: SyntheticReason,
    ) -> TypeId {
        self.def_origin.insert(
            def.id,
            FactOrigin::Synthetic {
                stage: stage.into(),
                reason,
            },
        );
        self.working.insert_def_type(def, ty)
    }

    pub fn origin_for_node(&self, node_id: NodeId) -> Option<FactOrigin> {
        if let Some(origin) = self.node_origin.get(&node_id) {
            Some(origin.clone())
        } else if self.base.lookup_node_type(node_id).is_some() {
            Some(FactOrigin::Source)
        } else {
            None
        }
    }

    pub fn origin_for_def(&self, def_id: DefId) -> Option<FactOrigin> {
        if let Some(origin) = self.def_origin.get(&def_id) {
            Some(origin.clone())
        } else if self.base_def_ids.contains(&def_id) {
            Some(FactOrigin::Source)
        } else {
            None
        }
    }

    pub fn synthetic_node_origins(&self) -> &HashMap<NodeId, FactOrigin> {
        &self.node_origin
    }

    pub fn synthetic_def_origins(&self) -> &HashMap<DefId, FactOrigin> {
        &self.def_origin
    }

    pub fn into_inner(self) -> TypeMap {
        self.working
    }
}

impl Deref for TypeMapOverlay {
    type Target = TypeMap;

    fn deref(&self) -> &Self::Target {
        &self.working
    }
}

/// Immutable-base wrapper for `DefTable` with synthetic defs.
#[derive(Debug, Clone)]
pub struct DefTableOverlay {
    base: Arc<DefTable>,
    working: DefTable,
    def_origin: HashMap<DefId, FactOrigin>,
}

impl DefTableOverlay {
    pub fn new(base: DefTable) -> Self {
        let base = Arc::new(base);
        Self::with_shared_base(base)
    }

    pub fn with_shared_base(base: Arc<DefTable>) -> Self {
        Self {
            working: (*base).clone(),
            base,
            def_origin: HashMap::new(),
        }
    }

    pub fn add_def(
        &mut self,
        name: String,
        kind: crate::core::resolve::DefKind,
        stage: impl Into<String>,
        reason: SyntheticReason,
    ) -> DefId {
        let def_id = self.working.add_def(name, kind);
        self.def_origin.insert(
            def_id,
            FactOrigin::Synthetic {
                stage: stage.into(),
                reason,
            },
        );
        def_id
    }

    pub fn origin_for_def(&self, def_id: DefId) -> Option<FactOrigin> {
        if let Some(origin) = self.def_origin.get(&def_id) {
            Some(origin.clone())
        } else if self.base.lookup_def(def_id).is_some() {
            Some(FactOrigin::Source)
        } else {
            None
        }
    }

    pub fn synthetic_def_origins(&self) -> &HashMap<DefId, FactOrigin> {
        &self.def_origin
    }

    pub fn into_inner(self) -> DefTable {
        self.working
    }
}

impl Deref for DefTableOverlay {
    type Target = DefTable;

    fn deref(&self) -> &Self::Target {
        &self.working
    }
}

#[cfg(test)]
#[path = "../../tests/analysis/t_facts.rs"]
mod tests;
