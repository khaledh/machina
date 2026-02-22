use std::collections::HashMap;
use std::fmt;
use std::path::{Path, PathBuf};

use crate::core::diag::Span;
use crate::core::resolve::{Def, DefId, DefKind};
use crate::core::tree::NodeId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefLocation {
    pub path: Option<PathBuf>,
    pub span: Span,
}

pub struct DefTableBuilder {
    defs: Vec<Def>,
    node_def: HashMap<NodeId, DefId>,
    def_node: HashMap<DefId, NodeId>,
    def_span: HashMap<DefId, Span>,
    def_location_overrides: HashMap<DefId, DefLocation>,
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
            def_span: HashMap::new(),
            def_location_overrides: HashMap::new(),
        }
    }

    pub fn record_def(&mut self, def: Def, node_id: NodeId, span: Span) {
        self.node_def.insert(node_id, def.id);
        self.def_node.insert(def.id, node_id);
        self.def_span.insert(def.id, span);
        self.defs.push(def);
    }

    pub fn record_use(&mut self, node_id: NodeId, def_id: DefId) {
        self.node_def.insert(node_id, def_id);
    }

    pub fn finish(self) -> DefTable {
        DefTable {
            defs: self.defs,
            node_def: self.node_def,
            def_node: self.def_node,
            def_span: self.def_span,
            def_location_overrides: self.def_location_overrides,
            source_path: None,
        }
    }
}

// --- DefTable ---

#[derive(Debug, Clone)]
pub struct DefTable {
    defs: Vec<Def>,
    node_def: HashMap<NodeId, DefId>,
    def_node: HashMap<DefId, NodeId>,
    def_span: HashMap<DefId, Span>,
    def_location_overrides: HashMap<DefId, DefLocation>,
    source_path: Option<PathBuf>,
}

impl DefTable {
    pub fn new(defs: Vec<Def>) -> Self {
        Self {
            defs,
            node_def: HashMap::new(),
            def_node: HashMap::new(),
            def_span: HashMap::new(),
            def_location_overrides: HashMap::new(),
            source_path: None,
        }
    }

    // --- Defs ---

    pub fn record_def_node(&mut self, def_id: DefId, node_id: NodeId, span: Span) {
        self.node_def.insert(node_id, def_id);
        self.def_node.insert(def_id, node_id);
        self.def_span.insert(def_id, span);
    }

    pub fn lookup_def(&self, def_id: DefId) -> Option<&Def> {
        self.defs.get(def_id.0 as usize)
    }

    pub fn lookup_def_node_id(&self, def_id: DefId) -> Option<NodeId> {
        self.def_node.get(&def_id).copied()
    }

    pub fn lookup_def_span(&self, def_id: DefId) -> Option<Span> {
        self.def_span.get(&def_id).copied()
    }

    pub fn set_def_location(&mut self, def_id: DefId, location: DefLocation) {
        self.def_location_overrides.insert(def_id, location);
    }

    pub fn lookup_def_location(&self, def_id: DefId) -> Option<DefLocation> {
        if let Some(loc) = self.def_location_overrides.get(&def_id) {
            return Some(loc.clone());
        }
        let span = self.lookup_def_span(def_id)?;
        Some(DefLocation {
            path: self.source_path.clone(),
            span,
        })
    }

    // --- Uses ---

    pub fn record_use(&mut self, node_id: NodeId, def_id: DefId) {
        self.node_def.insert(node_id, def_id);
    }

    pub fn lookup_node_def_id(&self, node_id: NodeId) -> Option<DefId> {
        self.node_def.get(&node_id).copied()
    }

    pub fn lookup_type_def_id(&self, type_name: &str) -> Option<DefId> {
        self.defs
            .iter()
            .find(|def| def.name == type_name && matches!(def.kind, DefKind::TypeDef { .. }))
            .map(|def| def.id)
    }

    pub fn def_id(&self, node_id: NodeId) -> DefId {
        self.node_def
            .get(&node_id)
            .copied()
            .unwrap_or_else(|| panic!("missing def_id for node {node_id}"))
    }

    // --- Source path ---

    pub fn source_path(&self) -> Option<&Path> {
        self.source_path.as_deref()
    }

    pub fn set_source_path(&mut self, source_path: Option<PathBuf>) {
        self.source_path = source_path;
    }

    // --- Other def properties ---

    pub fn is_intrinsic(&self, def_id: DefId) -> bool {
        self.lookup_def(def_id)
            .is_some_and(|def| def.is_intrinsic())
    }

    // --- NRVO eligibility ---

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

    // -- Bulk access ---

    pub fn defs(&self) -> &[Def] {
        &self.defs
    }

    pub fn node_def_entries(&self) -> impl Iterator<Item = (NodeId, DefId)> + '_ {
        self.node_def
            .iter()
            .map(|(node_id, def_id)| (*node_id, *def_id))
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn def_location_uses_override_when_present() {
        let def_id = DefId(0);
        let mut builder = DefTableBuilder::new();
        builder.record_def(
            Def {
                id: def_id,
                name: "f".to_string(),
                kind: DefKind::FuncDef {
                    attrs: crate::core::resolve::FuncAttrs::default(),
                },
            },
            NodeId(1),
            Span::default(),
        );
        let mut table = builder.finish();
        let source_path = PathBuf::from("main.mc");
        table.set_source_path(Some(source_path.clone()));

        let default_loc = table
            .lookup_def_location(def_id)
            .expect("expected default location");
        assert_eq!(default_loc.path.as_deref(), Some(source_path.as_path()));

        let override_loc = DefLocation {
            path: Some(PathBuf::from("std/prelude_decl.mc")),
            span: Span::default(),
        };
        table.set_def_location(def_id, override_loc.clone());

        let resolved_loc = table
            .lookup_def_location(def_id)
            .expect("expected override location");
        assert_eq!(resolved_loc, override_loc);
    }

    #[test]
    fn synthetic_def_without_span_has_no_location() {
        let mut table = DefTable::new(Vec::new());
        let def_id = table.add_def(
            "__synthetic".to_string(),
            DefKind::FuncDef {
                attrs: crate::core::resolve::FuncAttrs::default(),
            },
        );
        table.set_source_path(Some(PathBuf::from("main.mc")));
        assert!(table.lookup_def_location(def_id).is_none());
    }
}
