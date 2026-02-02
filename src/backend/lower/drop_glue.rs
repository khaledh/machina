use std::collections::HashMap;

use crate::backend::lower::globals::GlobalArena;
use crate::backend::lower::lowerer::FuncLowerer;
use crate::backend::lower::{LowerToIrError, LoweredFunction};
use crate::ir::ir::Terminator;
use crate::resolve::{DefId, DefTable};
use crate::tree::NodeId;
use crate::tree::resolved as res;
use crate::tree::semantic as sem;
use crate::typeck::type_map::{TypeMap, resolve_type_expr};
use crate::types::Type;

/// Tracks drop-glue functions needed for recursive heap elements.
///
/// Recursive type definitions lower to shallow types for recursive occurrences.
/// When dropping a heap element whose type is shallow, we route the drop through
/// a generated drop-glue function to preserve recursive drop behavior without
/// inlining infinitely.
pub(super) struct DropGlueRegistry {
    next_def: u32,
    defs: HashMap<DefId, DefId>,
    tys: HashMap<DefId, Type>,
    full_tys: HashMap<DefId, Type>,
}

impl DropGlueRegistry {
    pub(super) fn new(def_table: &DefTable) -> Self {
        Self {
            next_def: def_table.next_def_id().0,
            defs: HashMap::new(),
            tys: HashMap::new(),
            full_tys: HashMap::new(),
        }
    }

    pub(super) fn from_module(def_table: &DefTable, module: &sem::Module) -> Self {
        let mut full_tys = HashMap::new();
        for type_def in module.type_defs() {
            let type_expr = res::TypeExpr {
                id: type_def.id,
                kind: res::TypeExprKind::Named {
                    ident: type_def.name.clone(),
                    def_id: type_def.def_id,
                },
                span: type_def.span,
            };
            let ty = resolve_type_expr(def_table, module, &type_expr).unwrap_or_else(|err| {
                panic!("backend drop glue type def {}: {:?}", type_def.name, err)
            });
            full_tys.insert(type_def.def_id, ty);
        }
        Self {
            next_def: def_table.next_def_id().0,
            defs: HashMap::new(),
            tys: HashMap::new(),
            full_tys,
        }
    }

    pub(super) fn def_id_for(&mut self, name: &str, ty: &Type, def_table: &DefTable) -> DefId {
        let type_def_id = def_table
            .lookup_type_def_id(name)
            .unwrap_or_else(|| panic!("backend drop glue missing type def for {}", name));

        if let Some(def_id) = self.defs.get(&type_def_id).copied() {
            return def_id;
        }

        let def_id = DefId(self.next_def);
        self.next_def += 1;
        self.defs.insert(type_def_id, def_id);

        let full_ty = self
            .full_tys
            .get(&type_def_id)
            .cloned()
            .unwrap_or_else(|| ty.clone());
        self.tys.insert(type_def_id, full_ty);

        def_id
    }

    pub(super) fn take_glue_functions(
        &mut self,
        def_table: &DefTable,
        type_map: &TypeMap,
        globals: &mut GlobalArena,
        trace_drops: bool,
    ) -> Result<Vec<LoweredFunction>, LowerToIrError> {
        let mut funcs = Vec::new();
        let empty_plans: HashMap<NodeId, sem::LoweringPlan> = HashMap::new();
        let mut pending: Vec<(DefId, Type)> = self.tys.drain().collect();
        while let Some((type_def_id, ty)) = pending.pop() {
            let def_id =
                self.defs.get(&type_def_id).copied().unwrap_or_else(|| {
                    panic!("backend drop glue missing def for {:?}", type_def_id)
                });
            let type_def = def_table
                .lookup_def(type_def_id)
                .unwrap_or_else(|| panic!("backend drop glue missing type def {:?}", type_def_id));
            let func_name = format!("__drop_{}", type_def.name);
            let mut lowerer = FuncLowerer::new_drop_glue(
                def_id,
                func_name,
                ty.clone(),
                def_table,
                type_map,
                &empty_plans,
                self,
                globals,
                trace_drops,
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
