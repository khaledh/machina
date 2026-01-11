use std::collections::HashMap;
use std::fmt;

use crate::ast::NodeId;
use crate::resolve::{Def, DefId, DefKind};

pub struct DefTableBuilder {
    defs: Vec<Def>,
    node_def: HashMap<NodeId, DefId>,
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
        }
    }

    pub fn record_def(&mut self, def: Def, node_id: NodeId) {
        self.node_def.insert(node_id, def.id);
        self.defs.push(def);
    }

    pub fn record_use(&mut self, node_id: NodeId, def_id: DefId) {
        self.node_def.insert(node_id, def_id);
    }

    pub fn finish(self) -> (DefTable, NodeDefLookup) {
        (
            DefTable { defs: self.defs },
            NodeDefLookup {
                node_def: self.node_def,
            },
        )
    }
}

// --- DefTable ---

#[derive(Debug, Clone)]
pub struct DefTable {
    defs: Vec<Def>,
}

impl DefTable {
    pub fn new(defs: Vec<Def>) -> Self {
        Self { defs }
    }

    pub fn lookup_def(&self, def_id: DefId) -> Option<&Def> {
        self.defs.get(def_id.0 as usize)
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
