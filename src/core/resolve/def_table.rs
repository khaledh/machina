use std::collections::HashMap;
use std::fmt;

use crate::core::resolve::{Def, DefId, DefKind};
use crate::core::tree::NodeId;

pub struct DefTableBuilder {
    defs: Vec<Def>,
    node_def: HashMap<NodeId, DefId>,
    def_node: HashMap<DefId, NodeId>,
}

impl Default for DefTableBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl DefTableBuilder {
    pub fn new() -> Self {
        Self {
            defs: Vec::new(),
            node_def: HashMap::new(),
            def_node: HashMap::new(),
        }
    }

    pub fn record_def(&mut self, def: Def, node_id: NodeId) {
        self.node_def.insert(node_id, def.id);
        self.def_node.insert(def.id, node_id);
        self.defs.push(def);
    }

    pub fn record_use(&mut self, node_id: NodeId, def_id: DefId) {
        self.node_def.insert(node_id, def_id);
    }

    pub fn finish(self) -> (DefTable, NodeDefLookup) {
        let node_def = self.node_def;
        (
            DefTable {
                defs: self.defs,
                node_def: node_def.clone(),
                def_node: self.def_node,
            },
            NodeDefLookup { node_def },
        )
    }
}

// --- DefTable ---

#[derive(Debug, Clone)]
pub struct DefTable {
    defs: Vec<Def>,
    node_def: HashMap<NodeId, DefId>,
    def_node: HashMap<DefId, NodeId>,
}

impl DefTable {
    pub fn new(defs: Vec<Def>) -> Self {
        Self {
            defs,
            node_def: HashMap::new(),
            def_node: HashMap::new(),
        }
    }

    pub fn lookup_def(&self, def_id: DefId) -> Option<&Def> {
        self.defs.get(def_id.0 as usize)
    }

    pub fn lookup_node_def_id(&self, node_id: NodeId) -> Option<DefId> {
        self.node_def.get(&node_id).copied()
    }

    pub fn node_def_entries(&self) -> impl Iterator<Item = (NodeId, DefId)> + '_ {
        self.node_def
            .iter()
            .map(|(node_id, def_id)| (*node_id, *def_id))
    }

    pub fn lookup_def_node_id(&self, def_id: DefId) -> Option<NodeId> {
        self.def_node.get(&def_id).copied()
    }

    pub fn is_intrinsic(&self, def_id: DefId) -> bool {
        self.lookup_def(def_id)
            .is_some_and(|def| def.is_intrinsic())
    }

    pub fn lookup_type_def_id(&self, name: &str) -> Option<DefId> {
        self.defs
            .iter()
            .find(|def| def.name == name && matches!(def.kind, DefKind::TypeDef { .. }))
            .map(|def| def.id)
    }

    pub fn mark_nrvo_eligible(&mut self, def_id: DefId) {
        if let Some(def) = self.defs.iter_mut().find(|def| def.id == def_id)
            && let DefKind::LocalVar { nrvo_eligible, .. } = &mut def.kind
        {
            *nrvo_eligible = true;
        }
    }

    pub fn get_nrvo_eligible_defs(&self) -> Vec<&Def> {
        self.defs
            .iter()
            .filter(|def| {
                matches!(
                    def.kind,
                    DefKind::LocalVar {
                        nrvo_eligible: true,
                        ..
                    }
                )
            })
            .collect()
    }

    // For registering generated functions
    pub fn next_def_id(&self) -> DefId {
        DefId(self.defs.len() as u32)
    }

    pub fn add_def(&mut self, name: String, kind: DefKind) -> DefId {
        let id = self.next_def_id();
        self.defs.push(Def { id, name, kind });
        id
    }

    pub fn defs(&self) -> &[Def] {
        &self.defs
    }
}

impl IntoIterator for DefTable {
    type Item = Def;
    type IntoIter = std::vec::IntoIter<Def>;
    fn into_iter(self) -> Self::IntoIter {
        self.defs.into_iter()
    }
}

impl fmt::Display for DefTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Defs:")?;
        for def in self.defs.iter() {
            writeln!(f, "Def [{}] {}: {}", def.id, def.name, def.kind)?;
        }
        Ok(())
    }
}

// --- NodeDefLookup ---

pub struct NodeDefLookup {
    node_def: HashMap<NodeId, DefId>,
}

impl NodeDefLookup {
    pub fn new(node_def: HashMap<NodeId, DefId>) -> Self {
        Self { node_def }
    }

    pub fn lookup_node_def_id(&self, node_id: NodeId) -> Option<DefId> {
        self.node_def.get(&node_id).cloned()
    }
}

impl fmt::Display for NodeDefLookup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // sort def map by node id
        writeln!(f)?;
        writeln!(f, "Node -> Def:")?;
        let mut node_def = self.node_def.iter().collect::<Vec<(&NodeId, &DefId)>>();
        node_def.sort_by_key(|(node, _)| node.0);
        for (node, def) in node_def {
            writeln!(f, "Node [{}] -> Def [{}]", node, def)?;
        }
        Ok(())
    }
}
