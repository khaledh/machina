//! Module graph helpers for incremental invalidation.
//!
//! The frontend already computes module edges (`module -> dependencies`).
//! This module provides reverse-closure utilities used by analysis invalidation.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::frontend::ModuleId;

#[derive(Debug, Clone, Default)]
pub struct ModuleGraph {
    /// Directed edges: `module -> dependencies`.
    edges: HashMap<ModuleId, Vec<ModuleId>>,
    /// Reverse edges: `dependency -> dependents`.
    reverse: HashMap<ModuleId, Vec<ModuleId>>,
}

impl ModuleGraph {
    pub fn new(edges: HashMap<ModuleId, Vec<ModuleId>>) -> Self {
        let mut reverse = HashMap::<ModuleId, Vec<ModuleId>>::new();
        for (module, deps) in &edges {
            for dep in deps {
                reverse.entry(*dep).or_default().push(*module);
            }
        }
        Self { edges, reverse }
    }

    pub fn dependencies_of(&self, module: ModuleId) -> &[ModuleId] {
        self.edges.get(&module).map_or(&[], Vec::as_slice)
    }

    pub fn dependents_of(&self, module: ModuleId) -> &[ModuleId] {
        self.reverse.get(&module).map_or(&[], Vec::as_slice)
    }

    /// Compute invalidation closure for changed modules:
    /// changed modules + all transitive dependents.
    pub fn invalidation_closure(&self, changed: &HashSet<ModuleId>) -> HashSet<ModuleId> {
        let mut out = HashSet::new();
        let mut q = VecDeque::new();
        for module in changed {
            q.push_back(*module);
        }
        while let Some(module) = q.pop_front() {
            if !out.insert(module) {
                continue;
            }
            for dependent in self.dependents_of(module) {
                q.push_back(*dependent);
            }
        }
        out
    }
}

#[cfg(test)]
#[path = "../../tests/analysis/t_module_graph.rs"]
mod tests;
