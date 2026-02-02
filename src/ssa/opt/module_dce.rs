//! Module-level dead function elimination for SSA.
//!
//! Keeps functions reachable from `main` via direct calls or through
//! explicitly taken function addresses.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::resolve::DefId;
use crate::ssa::lower::{LoweredFunction, LoweredModule};
use crate::ssa::model::ir::{Callee, ConstValue, Function, GlobalId, InstKind, Terminator};

/// Computes the set of reachable function DefIds for a module.
///
/// Reachability roots at any function named `main`. If no `main` exists,
/// all functions are considered reachable (library-style module).
pub fn reachable_def_ids(funcs: &[Function]) -> HashSet<DefId> {
    let mut index_by_def = HashMap::new();
    for (idx, func) in funcs.iter().enumerate() {
        index_by_def.insert(func.def_id, idx);
    }

    let mut worklist = VecDeque::new();
    for func in funcs {
        if func.name == "main" {
            worklist.push_back(func.def_id);
        }
    }

    if worklist.is_empty() {
        return funcs.iter().map(|func| func.def_id).collect();
    }

    let mut reachable = HashSet::new();
    while let Some(def_id) = worklist.pop_front() {
        if !reachable.insert(def_id) {
            continue;
        }

        let Some(&idx) = index_by_def.get(&def_id) else {
            continue;
        };
        let func = &funcs[idx];
        enqueue_references(func, &index_by_def, &mut worklist);
    }

    reachable
}

/// Removes unused globals and remaps global IDs to a dense range.
///
/// Returns true if any globals were removed.
pub fn prune_globals(module: &mut LoweredModule) -> bool {
    if module.globals.is_empty() {
        return false;
    }

    let used = collect_used_globals(&module.funcs);
    if used.len() == module.globals.len() {
        return false;
    }

    let mut used_ids: Vec<GlobalId> = used.into_iter().collect();
    used_ids.sort_by_key(|id| id.0);

    let mut remap = HashMap::new();
    for (new_idx, old_id) in used_ids.iter().enumerate() {
        remap.insert(*old_id, GlobalId(new_idx as u32));
    }

    let mut globals_by_id = HashMap::new();
    for global in &module.globals {
        globals_by_id.insert(global.id, global.clone());
    }

    let mut new_globals = Vec::with_capacity(used_ids.len());
    for old_id in used_ids {
        let Some(mut global) = globals_by_id.get(&old_id).cloned() else {
            continue;
        };
        global.id = *remap.get(&old_id).unwrap();
        new_globals.push(global);
    }
    module.globals = new_globals;

    for func in &mut module.funcs {
        remap_globals_in_func(&mut func.func, &remap);
        remap_globals_in_func_slice(func, &remap);
    }

    true
}

fn collect_used_globals(funcs: &[LoweredFunction]) -> HashSet<GlobalId> {
    let mut used = HashSet::new();
    for func in funcs {
        for block in &func.func.blocks {
            for inst in &block.insts {
                if let InstKind::Const {
                    value: ConstValue::GlobalAddr { id },
                } = &inst.kind
                {
                    used.insert(*id);
                }
            }

            if let Terminator::Switch { cases, .. } = &block.term {
                for case in cases {
                    if let ConstValue::GlobalAddr { id } = &case.value {
                        used.insert(*id);
                    }
                }
            }
        }
    }
    used
}

fn remap_globals_in_func(func: &mut Function, remap: &HashMap<GlobalId, GlobalId>) {
    for block in &mut func.blocks {
        for inst in &mut block.insts {
            if let InstKind::Const {
                value: ConstValue::GlobalAddr { id },
            } = &mut inst.kind
            {
                if let Some(new_id) = remap.get(id) {
                    *id = *new_id;
                }
            }
        }

        if let Terminator::Switch { cases, .. } = &mut block.term {
            for case in cases {
                if let ConstValue::GlobalAddr { id } = &mut case.value {
                    if let Some(new_id) = remap.get(id) {
                        *id = *new_id;
                    }
                }
            }
        }
    }
}

fn remap_globals_in_func_slice(func: &mut LoweredFunction, remap: &HashMap<GlobalId, GlobalId>) {
    func.globals.retain_mut(|global| {
        let Some(new_id) = remap.get(&global.id) else {
            return false;
        };
        global.id = *new_id;
        true
    });
}

fn enqueue_references(
    func: &Function,
    index_by_def: &HashMap<DefId, usize>,
    worklist: &mut VecDeque<DefId>,
) {
    let mut enqueue = |def: DefId| {
        if index_by_def.contains_key(&def) {
            worklist.push_back(def);
        }
    };

    for block in &func.blocks {
        for inst in &block.insts {
            match &inst.kind {
                InstKind::Call {
                    callee: Callee::Direct(def),
                    ..
                } => enqueue(*def),
                InstKind::Const {
                    value: ConstValue::FuncAddr { def },
                } => enqueue(*def),
                _ => {}
            }
        }

        if let Terminator::Switch { cases, .. } = &block.term {
            for case in cases {
                if let ConstValue::FuncAddr { def } = &case.value {
                    enqueue(*def);
                }
            }
        }
    }
}

#[cfg(test)]
#[path = "../../tests/ssa/opt/t_module_dce.rs"]
mod tests_module_dce;
