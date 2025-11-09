use crate::ids::{DefId, NodeId};
use crate::types::Type;
use std::collections::HashMap;

mod resolution {
    use super::{DefId, HashMap, NodeId};

    pub struct ResolutionBuilder {
        node_to_def: HashMap<NodeId, DefId>,
        def_to_node: HashMap<DefId, NodeId>,
    }

    impl ResolutionBuilder {
        pub fn new() -> Self {
            Self {
                node_to_def: HashMap::new(),
                def_to_node: HashMap::new(),
            }
        }

        pub fn record_def(&mut self, def_id: DefId, node_id: NodeId) {
            self.def_to_node.insert(def_id, node_id);
        }

        pub fn record_use(&mut self, node_id: NodeId, def_id: DefId) {
            self.node_to_def.insert(node_id, def_id);
        }

        pub fn finish(self) -> ResolutionMap {
            ResolutionMap {
                node_to_def: self.node_to_def,
                def_to_node: self.def_to_node,
            }
        }
    }

    pub struct ResolutionMap {
        node_to_def: HashMap<NodeId, DefId>,
        def_to_node: HashMap<DefId, NodeId>,
    }

    impl ResolutionMap {
        pub fn get(&self, node: NodeId) -> Option<DefId> {
            self.node_to_def.get(&node).copied()
        }
    }
}

pub use resolution::{ResolutionBuilder, ResolutionMap};
