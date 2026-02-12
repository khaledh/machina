//! Eliminates repeated field_addr within a block.

use std::collections::{HashMap, HashSet};

use crate::backend::opt::Pass;
use crate::ir::IrTypeId;
use crate::ir::{Function, InstKind, ValueId, replace_value_in_func};

/// Reuses earlier field_addr results when the base and field index match.
pub struct FieldAddrCse;

impl Pass for FieldAddrCse {
    fn name(&self) -> &'static str {
        "backend-field-addr-cse"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let mut replacements = Vec::new();
        let mut remove = HashSet::new();

        for (block_idx, block) in func.blocks.iter().enumerate() {
            let mut seen: HashMap<(ValueId, usize, IrTypeId), ValueId> = HashMap::new();

            for (inst_idx, inst) in block.insts.iter().enumerate() {
                let InstKind::FieldAddr { base, index } = &inst.kind else {
                    continue;
                };
                let Some(result) = &inst.result else {
                    continue;
                };

                let key = (*base, *index, result.ty);
                if let Some(existing) = seen.get(&key).copied() {
                    replacements.push((block_idx, inst_idx, result.id, existing));
                    remove.insert((block_idx, inst_idx));
                } else {
                    seen.insert(key, result.id);
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
