use std::collections::HashMap;

use crate::ast::Module;
use crate::resolve::def_map::DefId;
use crate::resolve::def_map::{DefKind, DefMap};

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub func_ids: Vec<DefId>,
    pub def_names: HashMap<DefId, String>,
}

impl SymbolTable {
    pub fn new(module: &Module, def_map: &DefMap) -> Self {
        let mut extern_defs = Vec::new();

        // Collect extern defs for direct symbol names.
        for def in def_map.clone().into_iter() {
            match def.kind {
                DefKind::ExternFunc => extern_defs.push(def),
                _ => {}
            }
        }

        let mut func_ids = Vec::new();

        // Collect overloads
        let mut overloads: HashMap<String, Vec<DefId>> = HashMap::new();
        for callable in module.callables() {
            let def_id = def_map
                .lookup_node_def(callable.id())
                .unwrap_or_else(|| {
                    panic!("Callable {} not found in def_map", callable.name());
                })
                .id;
            func_ids.push(def_id);
            let name = callable.symbol_base_name();
            overloads.entry(name).or_default().push(def_id);
        }

        let mut def_names = HashMap::new();

        // externs: always plain
        for def in &extern_defs {
            def_names.insert(def.id, def.name.clone());
        }

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

        Self {
            func_ids,
            def_names,
        }
    }

    pub fn register_generated_def(&mut self, def_id: DefId, name: String) {
        self.func_ids.push(def_id);
        self.def_names.insert(def_id, name);
    }

    pub fn func_name(&self, index: usize) -> Option<&str> {
        let def_id = self.func_ids.get(index)?;
        self.def_names.get(def_id).map(|s| s.as_str())
    }
}
