//! Eliminates redundant by-ref parameter copies into locals.

use std::collections::{HashMap, HashSet};

use crate::backend::opt::Pass;
use crate::backend::opt::dataflow::ptr_utils::{is_read_only_ptr, peel_ptr_cast};
use crate::ir::ir::{Function, InstKind, ValueId, for_each_inst_use, replace_value_in_func};

/// Drops MemCopy-to-local when the local is only read through derived pointers.
pub struct ByRefCopyElim;

impl Pass for ByRefCopyElim {
    fn name(&self) -> &'static str {
        "byref-copy-elim"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let Some(entry) = func.blocks.first() else {
            return false;
        };
        let param_ids: HashSet<ValueId> = entry.params.iter().map(|param| param.value.id).collect();

        let (def_inst, uses) = build_use_maps(func);
        let mut candidates = Vec::new();

        for (block_idx, block) in func.blocks.iter().enumerate() {
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                let InstKind::MemCopy { dst, src, .. } = &inst.kind else {
                    continue;
                };

                if !param_ids.contains(src) {
                    continue;
                }

                let dst_root = peel_ptr_cast(*dst, func, &def_inst);
                let Some((def_block, def_idx)) = def_inst.get(&dst_root) else {
                    continue;
                };
                let def = &func.blocks[*def_block].insts[*def_idx];
                if !matches!(def.kind, InstKind::AddrOfLocal { .. }) {
                    continue;
                }

                let ignore = Some((block_idx, inst_idx));
                if !is_read_only_ptr(*dst, func, &uses, ignore, &mut HashSet::new()) {
                    continue;
                }

                candidates.push((block_idx, inst_idx, *dst, *src));
            }
        }

        if candidates.is_empty() {
            return false;
        }

        let remove: HashSet<(usize, usize)> = candidates
            .iter()
            .map(|(block_idx, inst_idx, _, _)| (*block_idx, *inst_idx))
            .collect();

        for (block_idx, inst_idx, dst, src) in &candidates {
            replace_value_in_func(func, *dst, *src, Some((*block_idx, *inst_idx)));
        }

        for (block_idx, block) in func.blocks.iter_mut().enumerate() {
            let mut new_insts = Vec::with_capacity(block.insts.len());
            let mut pending_comments = Vec::new();
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                if !remove.contains(&(block_idx, inst_idx)) {
                    let mut kept = inst.clone();
                    if !pending_comments.is_empty() {
                        let mut combined = Vec::new();
                        combined.append(&mut pending_comments);
                        combined.append(&mut kept.comments);
                        kept.comments = combined;
                    }
                    new_insts.push(kept);
                } else if !inst.comments.is_empty() {
                    pending_comments.extend(inst.comments.iter().cloned());
                }
            }
            if !pending_comments.is_empty()
                && let Some(last) = new_insts.last_mut()
            {
                last.comments.extend(pending_comments);
            }
            block.insts = new_insts;
        }

        true
    }
}

fn build_use_maps(
    func: &Function,
) -> (
    HashMap<ValueId, (usize, usize)>,
    HashMap<ValueId, Vec<(usize, usize)>>,
) {
    let mut def_inst = HashMap::new();
    let mut uses: HashMap<ValueId, Vec<(usize, usize)>> = HashMap::new();

    for (block_idx, block) in func.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.insts.iter().enumerate() {
            if let Some(result) = &inst.result {
                def_inst.insert(result.id, (block_idx, inst_idx));
            }

            for_each_inst_use(&inst.kind, |value| {
                uses.entry(value).or_default().push((block_idx, inst_idx));
            });
        }
    }

    (def_inst, uses)
}
