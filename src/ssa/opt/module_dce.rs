//! Module-level dead function elimination for SSA.
//!
//! Keeps functions reachable from `main` via direct calls or through
//! explicitly taken function addresses.

use std::collections::{HashMap, HashSet, VecDeque};

use crate::resolve::DefId;
use crate::ssa::model::ir::{Callee, ConstValue, Function, InstKind, Terminator};

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
