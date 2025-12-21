use std::collections::HashMap;

use crate::analysis::{DefKind, DefMap};
use crate::ast::Module;
use crate::ids::DefId;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub func_ids: Vec<DefId>,
    pub def_names: HashMap<DefId, String>,
}

impl SymbolTable {
    pub fn new(module: &Module, def_map: &DefMap) -> Self {
        let func_ids = module
            .funcs()
            .iter()
            .map(|func| {
                def_map
                    .lookup_def(func.id)
                    .unwrap_or_else(|| {
                        panic!("Function {} not found in def_map", func.name);
                    })
                    .id
            })
            .collect();

        let def_names = def_map
            .clone()
            .into_iter()
            .filter(|def| matches!(def.kind, DefKind::Func))
            .map(|def| (def.id, def.name))
            .collect();

        Self {
            func_ids,
            def_names,
        }
    }

    pub fn func_name(&self, index: usize) -> Option<&str> {
        let def_id = self.func_ids.get(index)?;
        self.def_names.get(def_id).map(|s| s.as_str())
    }
}
