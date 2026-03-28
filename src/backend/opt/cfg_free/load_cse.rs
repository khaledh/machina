//! Eliminates repeated loads from the same pointer within a block.

use std::collections::{HashMap, HashSet};

use crate::backend::opt::Pass;
use crate::ir::{Function, InstKind, ValueId, replace_value_in_func};

/// Reuses the first load result when the same pointer is loaded again and no
/// side-effecting memory operation occurs in between.
pub struct LoadCse;

impl Pass for LoadCse {
    fn name(&self) -> &'static str {
        "backend-load-cse"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let mut replacements = Vec::new();
        let mut remove = HashSet::new();

        for (block_idx, block) in func.blocks.iter().enumerate() {
            let mut cached: HashMap<ValueId, ValueId> = HashMap::new();

            for (inst_idx, inst) in block.insts.iter().enumerate() {
                match &inst.kind {
                    InstKind::Load { ptr } => {
                        let Some(result) = &inst.result else {
                            continue;
                        };
                        if let Some(existing) = cached.get(ptr).copied() {
                            replacements.push((block_idx, inst_idx, result.id, existing));
                            remove.insert((block_idx, inst_idx));
                        } else {
                            cached.insert(*ptr, result.id);
                        }
                    }
                    InstKind::Store { .. }
                    | InstKind::MemCopy { .. }
                    | InstKind::MemSet { .. }
                    | InstKind::Call { .. }
                    | InstKind::Drop { .. } => {
                        cached.clear();
                    }
                    _ => {}
                }
            }
        }

        if replacements.is_empty() {
            return false;
        }

        for (block_idx, inst_idx, from, to) in &replacements {
            replace_value_in_func(func, *from, *to, Some((*block_idx, *inst_idx)));
        }

        for (block_idx, block) in func.blocks.iter_mut().enumerate() {
            if !remove.iter().any(|(b, _)| *b == block_idx) {
                continue;
            }
            let mut new_insts = Vec::with_capacity(block.insts.len());
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                if !remove.contains(&(block_idx, inst_idx)) {
                    new_insts.push(inst.clone());
                }
            }
            block.insts = new_insts;
        }

        true
    }
}
