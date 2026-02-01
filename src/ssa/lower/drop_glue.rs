use std::collections::HashMap;

use crate::resolve::{DefId, DefTable};
use crate::ssa::lower::globals::GlobalArena;
use crate::ssa::lower::lowerer::FuncLowerer;
use crate::ssa::lower::{LoweredFunction, LoweringError};
use crate::ssa::model::ir::Terminator;
use crate::tree::NodeId;
use crate::tree::semantic as sem;
use crate::typeck::type_map::TypeMap;
use crate::types::Type;

/// Tracks drop-glue functions needed for recursive heap elements.
///
/// Recursive type definitions lower to shallow types for recursive occurrences.
/// When dropping a heap element whose type is shallow, we route the drop through
/// a generated drop-glue function to preserve recursive drop behavior without
/// inlining infinitely.
pub(super) struct DropGlueRegistry {
    next_def: u32,
    defs: HashMap<String, DefId>,
    tys: HashMap<String, Type>,
    full_tys: HashMap<String, Type>,
}

impl DropGlueRegistry {
    pub(super) fn new(def_table: &DefTable, full_tys: HashMap<String, Type>) -> Self {
        Self {
            next_def: def_table.next_def_id().0,
            defs: HashMap::new(),
            tys: HashMap::new(),
            full_tys,
        }
    }

    pub(super) fn def_id_for(&mut self, name: &str, ty: &Type) -> DefId {
        if let Some(def_id) = self.defs.get(name).copied() {
            return def_id;
        }

        let def_id = DefId(self.next_def);
        self.next_def += 1;
        self.defs.insert(name.to_string(), def_id);

        let full_ty = self
            .full_tys
            .get(name)
            .cloned()
            .unwrap_or_else(|| ty.clone());
        self.tys.insert(name.to_string(), full_ty);

        def_id
    }

    pub(super) fn take_glue_functions(
        &mut self,
        def_table: &DefTable,
        type_map: &TypeMap,
        globals: &mut GlobalArena,
    ) -> Result<Vec<LoweredFunction>, LoweringError> {
        let mut funcs = Vec::new();
        let empty_plans: HashMap<NodeId, sem::LoweringPlan> = HashMap::new();
        let mut pending: Vec<(String, Type)> = self.tys.drain().collect();
        while let Some((name, ty)) = pending.pop() {
            let def_id = self
                .defs
                .get(&name)
                .copied()
                .unwrap_or_else(|| panic!("ssa drop glue missing def for {}", name));
            let func_name = format!("__drop_{}", name);
            let mut lowerer = FuncLowerer::new_drop_glue(
                def_id,
                func_name,
                ty.clone(),
                def_table,
                type_map,
                &empty_plans,
                self,
                globals,
            );

            let entry = lowerer.builder.current_block();
            let param_ty = lowerer.param_tys[0];
            let arg = lowerer.builder.add_block_param(entry, param_ty);
            lowerer.drop_value_at_addr(arg, &ty)?;
            lowerer
                .builder
                .terminate(Terminator::Return { value: None });

            let (func, types) = lowerer.finish();
            funcs.push(LoweredFunction {
                func,
                types,
                globals: Vec::new(),
            });

            if !self.tys.is_empty() {
                pending.extend(self.tys.drain());
            }
        }

        Ok(funcs)
    }
}
