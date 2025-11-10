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
            }
        }
    }

    #[derive(Clone)]
    pub struct DefMap {
        node_def: HashMap<NodeId, DefId>,
    }

    impl DefMap {
        pub fn lookup_def(&self, node: NodeId) -> Option<DefId> {
            self.node_def.get(&node).copied()
        }
    }
}

pub use def_resolution::{DefMap, DefMapBuilder};

// -----------------------------------------------------------------------------
// Type Resolution
// -----------------------------------------------------------------------------

mod type_resolution {
    use super::{DefId, HashMap, NodeId, Type};

    pub struct TypeMapBuilder {
        node_type: HashMap<NodeId, Type>,
        def_type: HashMap<DefId, Type>,
    }

    impl TypeMapBuilder {
        pub fn new() -> Self {
            Self {
                node_type: HashMap::new(),
                def_type: HashMap::new(),
            }
        }

        pub fn record_def_type(&mut self, def_id: DefId, typ: Type) {
            self.def_type.insert(def_id, typ);
        }

        pub fn record_node_type(&mut self, node_id: NodeId, typ: Type) {
            self.node_type.insert(node_id, typ);
        }

        pub fn lookup_def_type(&self, def_id: DefId) -> Option<Type> {
            self.def_type.get(&def_id).copied()
        }

        pub fn finish(self) -> TypeMap {
            TypeMap {
                node_type: self.node_type,
            }
        }
    }

    #[allow(unused)]
    #[derive(Clone)]
    pub struct TypeMap {
        node_type: HashMap<NodeId, Type>,
    }

    #[allow(unused)]
    impl TypeMap {
        pub fn lookup_node_type(&self, node: NodeId) -> Option<Type> {
            self.node_type.get(&node).copied()
        }
    }
}

pub use type_resolution::{TypeMap, TypeMapBuilder};
