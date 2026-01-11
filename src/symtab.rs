use std::collections::HashMap;

use crate::hir::model::Module;
use crate::resolve::DefId;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub def_names: HashMap<DefId, String>,
}

impl SymbolTable {
    pub fn new(module: &Module) -> Self {
        // Collect overloads
        let mut overloads: HashMap<String, Vec<DefId>> = HashMap::new();
        for callable in module.callables() {
            let def_id = callable.def_id();
            let name = callable.symbol_base_name();
            overloads.entry(name).or_default().push(def_id);
        }

        let mut def_names = HashMap::new();

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
