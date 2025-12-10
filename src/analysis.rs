use crate::ids::{DefId, NodeId};
use crate::types::Type;
use std::collections::HashMap;

// -----------------------------------------------------------------------------
// Definition Resolution
// -----------------------------------------------------------------------------

mod def_resolution {
    use super::{DefId, HashMap, NodeId};
    use std::fmt;
    use std::hash::{Hash, Hasher};

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum DefKind {
        Func,
        LocalVar,
        Param { index: u32 },
    }

    impl fmt::Display for DefKind {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                DefKind::Func => write!(f, "Func"),
                DefKind::LocalVar => write!(f, "LocalVar"),
                DefKind::Param { index } => write!(f, "Param[{}]", index),
            }
        }
    }

    #[derive(Debug, Clone, Eq)]
    pub struct Def {
        pub id: DefId,
        pub name: String,
        pub kind: DefKind,
        // Whether it's safe to construct this def on the caller's stack directly
        // without copying from the callee's stack (applicable only to compound
        // return types).
        pub nrvo_eligible: bool,
    }

    impl Hash for Def {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.id.hash(state);
        }
    }

    impl PartialEq for Def {
        fn eq(&self, other: &Self) -> bool {
            self.id == other.id
        }
    }

    impl PartialOrd for Def {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.id.cmp(&other.id))
        }
    }

    impl fmt::Display for Def {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "Def [{}] {}: {}", self.id, self.name, self.kind)?;
            if self.nrvo_eligible {
                write!(f, " (NRVO eligible)")?;
            }
            Ok(())
        }
    }

    pub struct DefMapBuilder {
        defs: Vec<Def>,
        def_node: HashMap<DefId, NodeId>,
        node_def: HashMap<NodeId, DefId>,
    }

    impl DefMapBuilder {
        pub fn new() -> Self {
            Self {
                defs: Vec::new(),
                def_node: HashMap::new(),
                node_def: HashMap::new(),
            }
        }

        pub fn record_def(&mut self, def: Def, node_id: NodeId) {
            self.def_node.insert(def.id, node_id);
            self.node_def.insert(node_id, def.id);
            self.defs.push(def);
        }

        pub fn record_use(&mut self, node_id: NodeId, def_id: DefId) {
            self.node_def.insert(node_id, def_id);
        }

        pub fn finish(self) -> DefMap {
            DefMap {
                defs: self.defs,
                node_def: self.node_def,
            }
        }
    }

    #[derive(Clone)]
    pub struct DefMap {
        defs: Vec<Def>,
        node_def: HashMap<NodeId, DefId>,
    }

    impl DefMap {
        pub fn lookup_def(&self, node: NodeId) -> Option<&Def> {
            self.node_def
                .get(&node)
                .map(|def_id| &self.defs[def_id.0 as usize])
        }

        pub fn mark_nrvo_eligible(&mut self, def_id: DefId) {
            if let Some(def) = self.defs.iter_mut().find(|def| def.id == def_id) {
                def.nrvo_eligible = true;
            }
        }

        pub fn get_nrvo_eligible_defs(&self) -> Vec<&Def> {
            self.defs.iter().filter(|def| def.nrvo_eligible).collect()
        }
    }

    impl IntoIterator for DefMap {
        type Item = Def;
        type IntoIter = std::vec::IntoIter<Def>;
        fn into_iter(self) -> Self::IntoIter {
            self.defs.into_iter()
        }
    }

    impl fmt::Display for DefMap {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            writeln!(f, "Defs:")?;
            for def in self.defs.iter() {
                writeln!(f, "Def [{}] {}: {}", def.id, def.name, def.kind)?;
            }
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
}

pub use def_resolution::{Def, DefKind, DefMap, DefMapBuilder};

// -----------------------------------------------------------------------------
// Type Resolution
// -----------------------------------------------------------------------------

mod type_resolution {
    use super::{Def, HashMap, NodeId, Type};
    use std::fmt;

    pub struct TypeMapBuilder {
        node_type: HashMap<NodeId, Type>,
        def_type: HashMap<Def, Type>,
    }

    impl TypeMapBuilder {
        pub fn new() -> Self {
            Self {
                node_type: HashMap::new(),
                def_type: HashMap::new(),
            }
        }

        pub fn record_def_type(&mut self, def: Def, typ: Type) {
            self.def_type.insert(def, typ);
        }

        pub fn record_node_type(&mut self, node_id: NodeId, typ: Type) {
            self.node_type.insert(node_id, typ);
        }

        pub fn lookup_def_type(&self, def: &Def) -> Option<Type> {
            self.def_type.get(def).cloned()
        }

        pub fn finish(self) -> TypeMap {
            TypeMap {
                def_type: self.def_type,
                node_type: self.node_type,
            }
        }
    }

    #[allow(unused)]
    #[derive(Clone)]
    pub struct TypeMap {
        def_type: HashMap<Def, Type>,
        node_type: HashMap<NodeId, Type>,
    }

    #[allow(unused)]
    impl TypeMap {
        pub fn lookup_node_type(&self, node: NodeId) -> Option<Type> {
            self.node_type.get(&node).cloned()
        }
    }

    impl<'a> IntoIterator for &'a TypeMap {
        type Item = (&'a Def, &'a Type);
        type IntoIter = std::vec::IntoIter<(&'a Def, &'a Type)>;

        fn into_iter(self) -> Self::IntoIter {
            let mut items: Vec<_> = self.def_type.iter().collect();
            items.sort_by_key(|(def, _)| def.id);
            items.into_iter()
        }
    }

    impl fmt::Display for TypeMap {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            // sort by node id
            let mut node_type = self.node_type.iter().collect::<Vec<(&NodeId, &Type)>>();
            node_type.sort_by_key(|(node, _)| node.0);
            for (node, typ) in node_type {
                writeln!(f, "Node [{}] -> Type [{}]", node, typ)?;
            }
            Ok(())
        }
    }
}

pub use type_resolution::{TypeMap, TypeMapBuilder};
