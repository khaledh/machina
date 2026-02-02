//! Eliminates redundant stack-temp copies followed by memcpy.
//!
//! Example:
//! ```
//! %t: ptr<u64[3]> = addr_of %l2
//! %v: u64[3] = load %src
//! store %t, %v
//! call @__rt_memcpy(%dst, %t, %len)
//! ```
//! becomes:
//! ```
//! call @__rt_memcpy(%dst, %src, %len)
//! ```

use std::collections::{HashMap, HashSet};

use crate::backend::opt::Pass;
use crate::backend::opt::dataflow::ptr_utils::{peel_ptr_cast, source_stable_after};
use crate::ir::{
    CastKind, Function, InstKind, LocalId, ValueId, for_each_inst_use, replace_value_in_func,
};

type DefUseMaps = (
    HashMap<ValueId, (usize, usize)>,
    HashMap<ValueId, Vec<(usize, usize)>>,
);

/// Removes store+memcpy patterns that only shuttle data between locals.
pub struct StackTempCopyElim;

impl Pass for StackTempCopyElim {
    fn name(&self) -> &'static str {
        "stack-temp-copy-elim"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let (def_inst, uses) = build_maps(func);
        let local_ptrs = collect_local_ptrs(func);
        let mut candidates = Vec::new();
        let mut direct_memcpy = Vec::new();

        for (block_idx, block) in func.blocks.iter().enumerate() {
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                let InstKind::MemCopy { dst, src, .. } = &inst.kind else {
                    continue;
                };

                // Only consider memcpy between locals (addr_of_local results).
                let dst_root = peel_ptr_cast(*dst, func, &def_inst);
                let src_root = peel_ptr_cast(*src, func, &def_inst);

                let Some((dst_def_block, dst_def_idx)) = def_inst.get(&dst_root) else {
                    continue;
                };
                let dst_def = &func.blocks[*dst_def_block].insts[*dst_def_idx];
                let dst_local = match dst_def.kind {
                    InstKind::AddrOfLocal { local } => local,
                    _ => continue,
                };

                let Some((src_def_block, src_def_idx)) = def_inst.get(&src_root) else {
                    continue;
                };
                let src_def = &func.blocks[*src_def_block].insts[*src_def_idx];
                if !matches!(src_def.kind, InstKind::AddrOfLocal { .. }) {
                    continue;
                }

                // Look for: store temp = load src_ptr; memcpy dst, temp
                let Some((store_block, store_idx, stored_val)) =
                    store_to_local_before(*src, block_idx, inst_idx, func, &uses)
                else {
                    continue;
                };

                // If the stored value is a load from another local, rewrite memcpy to use the
                // original pointer directly and drop the temp store.
                if let Some((load_block, load_idx, load_ptr)) =
                    load_source_of_value(stored_val, func, &def_inst)
                    && load_block == block_idx
                    && load_idx < store_idx
                    && store_idx < inst_idx
                    && !has_uses_between(load_ptr, block_idx, load_idx, inst_idx, &uses)
                {
                    direct_memcpy.push((
                        block_idx,
                        inst_idx,
                        *src,
                        load_ptr,
                        store_block,
                        store_idx,
                    ));
                    continue;
                }

                // Fall back to the existing elimination strategy: if both dst/src are stable
                // after the memcpy, we can just replace dst with src and remove memcpy+store.
                if local_ptrs
                    .get(&dst_local)
                    .is_some_and(|values| values.len() > 1)
                {
                    continue;
                }

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

        if candidates.is_empty() && direct_memcpy.is_empty() {
            return false;
        }

        let mut remove: HashSet<(usize, usize)> = HashSet::new();
        let mut rewrite: HashMap<(usize, usize), ValueId> = HashMap::new();

        // For direct-memcpy cases, rewrite the memcpy source and drop the temp store.
        for (block_idx, inst_idx, _src, new_src, store_block, store_idx) in &direct_memcpy {
            rewrite.insert((*block_idx, *inst_idx), *new_src);
            remove.insert((*store_block, *store_idx));
        }
        // For replace-value cases, substitute dst with src and remove the memcpy.
        // The temp store may still be needed to materialize the source value (e.g. call results).
        for (block_idx, inst_idx, dst, src, _store_block, _store_idx) in &candidates {
            replace_value_in_func(func, *dst, *src, Some((*block_idx, *inst_idx)));
            remove.insert((*block_idx, *inst_idx));
        }

        for (block_idx, block) in func.blocks.iter_mut().enumerate() {
            if !remove.iter().any(|(b, _)| *b == block_idx)
                && !rewrite.keys().any(|(b, _)| *b == block_idx)
            {
                continue;
            }
            // Apply in-place rewrites for memcpy sources.
            for (inst_idx, inst) in block.insts.iter_mut().enumerate() {
                if let Some(new_src) = rewrite.get(&(block_idx, inst_idx))
                    && let InstKind::MemCopy { src, .. } = &mut inst.kind
                {
                    *src = *new_src;
                }
            }
            if !remove.iter().any(|(b, _)| *b == block_idx) {
                continue;
            }
            // Drop the now-redundant store/memcpy instructions.
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

fn build_maps(func: &Function) -> DefUseMaps {
    // Build def/use maps so we can reason about local addresses within a block.
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

fn collect_local_ptrs(func: &Function) -> HashMap<LocalId, Vec<ValueId>> {
    let mut ptrs: HashMap<LocalId, Vec<ValueId>> = HashMap::new();
    for block in &func.blocks {
        for inst in &block.insts {
            if let InstKind::AddrOfLocal { local } = inst.kind
                && let Some(result) = &inst.result
            {
                ptrs.entry(local).or_default().push(result.id);
            }
        }
    }
    ptrs
}

fn load_source_of_value(
    value: ValueId,
    func: &Function,
    def_inst: &HashMap<ValueId, (usize, usize)>,
) -> Option<(usize, usize, ValueId)> {
    // If the value was defined by a load, return the loaded pointer and location.
    let (block_idx, inst_idx) = def_inst.get(&value).copied()?;
    let inst = func.blocks.get(block_idx)?.insts.get(inst_idx)?;
    match &inst.kind {
        InstKind::Load { ptr } => Some((block_idx, inst_idx, *ptr)),
        _ => None,
    }
}

fn has_uses_between(
    value: ValueId,
    block_idx: usize,
    start_idx: usize,
    end_idx: usize,
    uses: &HashMap<ValueId, Vec<(usize, usize)>>,
) -> bool {
    // Checks for any use of `value` between two instruction indices in the same block.
    let Some(users) = uses.get(&value) else {
        return false;
    };
    users
        .iter()
        .any(|(b, i)| *b == block_idx && *i > start_idx && *i < end_idx)
}

fn store_to_local_before(
    ptr: ValueId,
    block_idx: usize,
    inst_idx: usize,
    func: &Function,
    uses: &HashMap<ValueId, Vec<(usize, usize)>>,
) -> Option<(usize, usize, ValueId)> {
    // Find the last store into `ptr` before the current instruction in the same block.
    let mut last_store = None;
    let users = uses.get(&ptr)?;
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
            InstKind::Load { .. }
            | InstKind::FieldAddr { .. }
            | InstKind::IndexAddr { .. }
            | InstKind::Cast {
                kind: CastKind::PtrToPtr,
                ..
            } => {}
            _ => return None,
        }
    }
    last_store
}
