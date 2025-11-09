use crate::ids::{DefId, NodeId};
use crate::types::Type;
use std::collections::HashMap;

// -----------------------------------------------------------------------------
// Definition Resolution
// -----------------------------------------------------------------------------

mod def_resolution {
    use super::{DefId, HashMap, NodeId};

    pub struct DefMapBuilder {
        node_def: HashMap<NodeId, DefId>,
        def_node: HashMap<DefId, NodeId>,
    }

    impl DefMapBuilder {
        pub fn new() -> Self {
            Self {
                node_def: HashMap::new(),
                def_node: HashMap::new(),
            }
        }

        pub fn record_def(&mut self, def_id: DefId, node_id: NodeId) {
            self.def_node.insert(def_id, node_id);
            self.node_def.insert(node_id, def_id);
        }

        pub fn record_use(&mut self, node_id: NodeId, def_id: DefId) {
            self.node_def.insert(node_id, def_id);
        }

        pub fn finish(self) -> DefMap {
            DefMap {
                node_def: self.node_def,
                def_node: self.def_node,
            }
        }
    }

    pub struct DefMap {
        node_def: HashMap<NodeId, DefId>,
        def_node: HashMap<DefId, NodeId>,
    }

    impl DefMap {
        pub fn lookup_def(&self, node: NodeId) -> Option<DefId> {
            self.node_def.get(&node).copied()
        }
    }
}

pub use def_resolution::{DefMap, DefMapBuilder};
