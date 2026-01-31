//! Eliminates redundant stack-temp copies followed by memcpy.

use std::collections::{HashMap, HashSet};

use crate::ssa::model::ir::{
    Function, InstKind, ValueId, for_each_inst_use, replace_value_in_func,
};
use crate::ssa::opt::Pass;

/// Removes store+memcpy patterns that only shuttle data between locals.
pub struct StackTempCopyElim;

impl Pass for StackTempCopyElim {
    fn name(&self) -> &'static str {
        "stack-temp-copy-elim"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let (def_inst, uses) = build_maps(func);
        let mut candidates = Vec::new();

        for (block_idx, block) in func.blocks.iter().enumerate() {
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                let InstKind::MemCopy { dst, src, .. } = &inst.kind else {
                    continue;
                };

                let Some((dst_def_block, dst_def_idx)) = def_inst.get(dst) else {
                    continue;
                };
                let dst_def = &func.blocks[*dst_def_block].insts[*dst_def_idx];
                if !matches!(dst_def.kind, InstKind::AddrOfLocal { .. }) {
                    continue;
                }

                let Some((src_def_block, src_def_idx)) = def_inst.get(src) else {
                    continue;
                };
                let src_def = &func.blocks[*src_def_block].insts[*src_def_idx];
                if !matches!(src_def.kind, InstKind::AddrOfLocal { .. }) {
                    continue;
                }

                let Some((store_block, store_idx, _stored_val)) =
                    store_to_local_before(*src, block_idx, inst_idx, func, &uses)
                else {
                    continue;
                };

                if !source_stable_after(
                    *dst,
                    block_idx,
                    inst_idx,
                    func,
                    &uses,
                    Some((block_idx, inst_idx)),
                    &mut HashSet::new(),
                ) {
                    continue;
                }

                if !source_stable_after(
                    *src,
                    block_idx,
                    inst_idx,
                    func,
                    &uses,
                    Some((block_idx, inst_idx)),
                    &mut HashSet::new(),
                ) {
                    continue;
                }

                candidates.push((block_idx, inst_idx, *dst, *src, store_block, store_idx));
            }
        }

        if candidates.is_empty() {
            return false;
        }

        let mut remove: HashSet<(usize, usize)> = HashSet::new();
        for (block_idx, inst_idx, dst, src, store_block, store_idx) in &candidates {
            replace_value_in_func(func, *dst, *src, Some((*block_idx, *inst_idx)));
            remove.insert((*block_idx, *inst_idx));
            remove.insert((*store_block, *store_idx));
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

fn build_maps(
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

fn store_to_local_before(
    ptr: ValueId,
    block_idx: usize,
    inst_idx: usize,
    func: &Function,
    uses: &HashMap<ValueId, Vec<(usize, usize)>>,
) -> Option<(usize, usize, ValueId)> {
    let mut last_store = None;
    let Some(users) = uses.get(&ptr) else {
        return None;
    };
    for (use_block, use_idx) in users {
        if *use_block != block_idx || *use_idx >= inst_idx {
            continue;
        }
        let inst = &func.blocks[*use_block].insts[*use_idx];
        match inst.kind {
            InstKind::Store {
                ptr: store_ptr,
                value,
            } if store_ptr == ptr => {
                last_store = Some((*use_block, *use_idx, value));
            }
            InstKind::Load { .. } | InstKind::FieldAddr { .. } | InstKind::IndexAddr { .. } => {}
            _ => return None,
        }
    }
    last_store
}

fn source_stable_after(
    value: ValueId,
    block_idx: usize,
    inst_idx: usize,
    func: &Function,
    uses: &HashMap<ValueId, Vec<(usize, usize)>>,
    ignore: Option<(usize, usize)>,
    visiting: &mut HashSet<ValueId>,
) -> bool {
    if !visiting.insert(value) {
        return true;
    }

    let Some(users) = uses.get(&value) else {
        return true;
    };

    for (use_block, use_idx) in users {
        if Some((*use_block, *use_idx)) == ignore {
            continue;
        }

        let inst = &func.blocks[*use_block].insts[*use_idx];
        match &inst.kind {
            InstKind::Load { .. } => {}
            InstKind::FieldAddr { .. } | InstKind::IndexAddr { .. } => {
                let Some(result) = &inst.result else {
                    return false;
                };
                if !source_stable_after(
                    result.id, block_idx, inst_idx, func, uses, ignore, visiting,
                ) {
                    return false;
                }
            }
            InstKind::Store { .. }
            | InstKind::MemCopy { .. }
            | InstKind::MemSet { .. }
            | InstKind::Drop { .. }
            | InstKind::Call { .. } => {
                if *use_block != block_idx || *use_idx > inst_idx {
                    return false;
                }
            }
            _ => return false,
        }
    }

    true
}
