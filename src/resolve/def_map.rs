use crate::ast::NodeId;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DefId(pub u32);

impl fmt::Display for DefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct DefIdGen {
    next_id: u32,
}

impl DefIdGen {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    pub fn new_id(&mut self) -> DefId {
        let id = DefId(self.next_id);
        self.next_id += 1;
        id
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefKind {
    TypeAlias {
        ty_expr: crate::ast::TypeExpr,
    },
    StructDef {
        fields: Vec<crate::ast::StructField>,
    },
    Func,
    LocalVar {
        nrvo_eligible: bool,
    },
    Param {
        index: u32,
    },
}

impl fmt::Display for DefKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DefKind::TypeAlias { ty_expr } => write!(f, "TypeAlias[{}]", ty_expr),
            DefKind::StructDef { fields } => {
                let field_names = fields
                    .iter()
                    .map(|field| field.name.as_str())
                    .collect::<Vec<_>>();
                write!(f, "StructDef[{}]", field_names.join(", "))
            }
            DefKind::Func => write!(f, "Func"),
            DefKind::LocalVar { nrvo_eligible } => {
                if *nrvo_eligible {
                    write!(f, "LocalVar (NRVO eligible)")
                } else {
                    write!(f, "LocalVar")
                }
            }
            DefKind::Param { index } => write!(f, "Param[{}]", index),
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub struct Def {
    pub id: DefId,
    pub name: String,
    pub kind: DefKind,
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
        write!(f, "Def [{}] {}: {}", self.id, self.name, self.kind)
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

#[derive(Debug, Clone)]
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
        if let Some(def) = self.defs.iter_mut().find(|def| def.id == def_id)
            && let DefKind::LocalVar { nrvo_eligible } = &mut def.kind
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
                        nrvo_eligible: true
                    }
                )
            })
            .collect()
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
