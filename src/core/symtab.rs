use std::collections::HashMap;

use crate::core::resolve::{DefId, DefTable};
use crate::core::tree::Module;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub def_names: HashMap<DefId, String>,
}

impl SymbolTable {
    pub fn new(module: &Module, def_table: &DefTable) -> Self {
        // Collect overloads
        let mut overloads: HashMap<String, Vec<DefId>> = HashMap::new();
        let mut fixed_names: HashMap<DefId, String> = HashMap::new();
        for callable in module.callables() {
            // During recovery (e.g., conflicting decl/def pairs), resolver may
            // intentionally leave some callables unmapped while still reporting diagnostics.
            let Some(def_id) = def_table.lookup_node_def_id(callable.id()) else {
                continue;
            };
            if let Some(def) = def_table.lookup_def(def_id)
                && let Some(link_name) = def.link_name()
            {
                fixed_names.insert(def_id, link_name.to_string());
                continue;
            }

            let name = callable.symbol_base_name();
            overloads.entry(name).or_default().push(def_id);
        }

        let mut def_names = fixed_names;

        // defs: suffix only if overloaded
        for (name, def_ids) in overloads {
            if def_ids.len() == 1 {
                def_names.insert(def_ids[0], name);
            } else {
                // Overloads get a stable in-module suffix so codegen emits unique symbols.
                for (index, def_id) in def_ids.iter().enumerate() {
                    def_names.insert(*def_id, format!("{name}${index}"));
                }
            }
        }

        Self { def_names }
    }

    pub fn register_generated_def(&mut self, def_id: DefId, name: String) {
        self.def_names.insert(def_id, name);
    }
}
