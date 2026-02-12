//! Eliminates redundant local-to-local MemCopy when the source stays stable.

use std::collections::{HashMap, HashSet};

use crate::backend::opt::Pass;
use crate::backend::opt::dataflow::ptr_utils::{
    is_read_only_ptr, peel_ptr_cast, source_stable_after,
};
use crate::ir::IrTypeId;
use crate::ir::{
    Function, InstKind, LocalId, Terminator, ValueId, for_each_inst_use, replace_value_in_func,
};

type ValueDefUseMaps = (
    HashMap<ValueId, IrTypeId>,
    HashMap<ValueId, (usize, usize)>,
    HashMap<ValueId, Vec<(usize, usize)>>,
);

/// Drops MemCopy between locals when the destination is read-only and the source is not written after.
pub struct LocalMemCopyElim;

impl Pass for LocalMemCopyElim {
    fn name(&self) -> &'static str {
        "local-memcpy-elim"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let (value_types, def_inst, uses) = build_maps(func);
        let local_ptrs = collect_local_ptrs(func);
        let param_ids = collect_param_ids(func);
        let term_uses = collect_term_uses(func);
        let memcpy_dsts = collect_memcpy_dsts(func, &def_inst);
        let mut candidates = Vec::new();

        for (block_idx, block) in func.blocks.iter().enumerate() {
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                let InstKind::MemCopy { dst, src, .. } = &inst.kind else {
                    continue;
                };

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

                let mut src_local = None;
                if let Some((src_def_block, src_def_idx)) = def_inst.get(&src_root) {
                    let src_def = &func.blocks[*src_def_block].insts[*src_def_idx];
                    if let InstKind::AddrOfLocal { local } = src_def.kind {
                        src_local = Some(local);
                    } else {
                        continue;
                    }
                } else if !param_ids.contains(&src_root) {
                    continue;
                }

                let Some(dst_ty) = value_types.get(dst) else {
                    continue;
                };
                let Some(src_ty) = value_types.get(src) else {
                    continue;
                };
                if dst_ty != src_ty {
                    continue;
                }

                if term_uses.contains(dst) || term_uses.contains(src) {
                    continue;
                }

                let ignore = Some((block_idx, inst_idx));
                let dst_ptrs = match local_ptrs.get(&dst_local) {
                    Some(ptrs) if !ptrs.is_empty() => ptrs.as_slice(),
                    _ => continue,
                };
                if dst_ptrs.iter().any(|ptr| term_uses.contains(ptr)) {
                    continue;
                }
                if dst_ptrs
                    .iter()
                    .any(|ptr| !is_read_only_ptr(*ptr, func, &uses, ignore, &mut HashSet::new()))
                {
                    continue;
                }

                let fallback = [src_root];
                let alias_ptrs = src_local
                    .and_then(|local| local_ptrs.get(&local).map(|values| values.as_slice()))
                    .unwrap_or(&fallback);
                if alias_ptrs.len() > 1 {
                    continue;
                }

                let mut dst_uses_valid = true;
                for ptr in dst_ptrs {
                    let Some(ptr_uses) = uses.get(ptr) else {
                        continue;
                    };
                    if ptr_uses
                        .iter()
                        .any(|(b, i)| *b != block_idx || *i < inst_idx)
                    {
                        dst_uses_valid = false;
                        break;
                    }
                }
                if !dst_uses_valid {
                    continue;
                }
                let mut stable = true;
                for alias in alias_ptrs {
                    if !source_stable_after(
                        *alias,
                        block_idx,
                        inst_idx,
                        func,
                        &uses,
                        ignore,
                        &mut HashSet::new(),
                    ) {
                        stable = false;
                        break;
                    }
                }
                if !stable {
                    continue;
                }

                candidates.push((block_idx, inst_idx, dst_local, src_local, src_root));
            }
        }

        if candidates.is_empty() {
            return false;
        }

        let mut final_candidates = candidates;
        loop {
            let candidate_dsts: HashSet<_> = final_candidates.iter().map(|c| c.2).collect();
            let mut changed = false;
            final_candidates.retain(|candidate| {
                let Some(src_local) = candidate.3 else {
                    return true;
                };
                let keep = !memcpy_dsts.contains(&src_local) || candidate_dsts.contains(&src_local);
                changed |= !keep;
                keep
            });
            if !changed {
                break;
            }
        }

        if final_candidates.is_empty() {
            return false;
        }

        let replacement_map = build_replacement_map(&final_candidates);
        let mut resolved_map = HashMap::new();
        for dst_local in replacement_map.keys() {
            let resolved = resolve_replacement(*dst_local, &replacement_map, func, &def_inst);
            resolved_map.insert(*dst_local, resolved);
        }

        let remove: HashSet<(usize, usize)> = final_candidates
            .iter()
            .map(|(block_idx, inst_idx, _, _, _)| (*block_idx, *inst_idx))
            .collect();

        for (block_idx, inst_idx, dst_local, _, _) in &final_candidates {
            let Some(src) = resolved_map.get(dst_local) else {
                continue;
            };
            if let Some(ptrs) = local_ptrs.get(dst_local) {
                for ptr in ptrs {
                    replace_value_in_func(func, *ptr, *src, Some((*block_idx, *inst_idx)));
                }
            }
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

fn build_maps(func: &Function) -> ValueDefUseMaps {
    let mut value_types = HashMap::new();
    let mut def_inst = HashMap::new();
    let mut uses: HashMap<ValueId, Vec<(usize, usize)>> = HashMap::new();

    if let Some(entry) = func.blocks.first() {
        for param in &entry.params {
            value_types.insert(param.value.id, param.value.ty);
        }
    }

    for (block_idx, block) in func.blocks.iter().enumerate() {
        for (inst_idx, inst) in block.insts.iter().enumerate() {
            if let Some(result) = &inst.result {
                value_types.insert(result.id, result.ty);
                def_inst.insert(result.id, (block_idx, inst_idx));
            }

            for_each_inst_use(&inst.kind, |value| {
                uses.entry(value).or_default().push((block_idx, inst_idx));
            });
        }
    }

    (value_types, def_inst, uses)
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

fn collect_param_ids(func: &Function) -> HashSet<ValueId> {
    let mut params = HashSet::new();
    if let Some(entry) = func.blocks.first() {
        for param in &entry.params {
            params.insert(param.value.id);
        }
    }
    params
}

fn collect_term_uses(func: &Function) -> HashSet<ValueId> {
    let mut uses = HashSet::new();
    for block in &func.blocks {
        match &block.term {
            Terminator::Br { args, .. } => {
                uses.extend(args.iter().cloned());
            }
            Terminator::CondBr {
                cond,
                then_args,
                else_args,
                ..
            } => {
                uses.insert(*cond);
                uses.extend(then_args.iter().cloned());
                uses.extend(else_args.iter().cloned());
            }
            Terminator::Switch {
                value,
                cases,
                default_args,
                ..
            } => {
                uses.insert(*value);
                for case in cases {
                    uses.extend(case.args.iter().cloned());
                }
                uses.extend(default_args.iter().cloned());
            }
            Terminator::Return { value } => {
                if let Some(value) = value {
                    uses.insert(*value);
                }
            }
            Terminator::Unreachable => {}
        }
    }
    uses
}

fn collect_memcpy_dsts(
    func: &Function,
    def_inst: &HashMap<ValueId, (usize, usize)>,
) -> HashSet<LocalId> {
    let mut locals = HashSet::new();
    for block in &func.blocks {
        for inst in &block.insts {
            let InstKind::MemCopy { dst, .. } = inst.kind else {
                continue;
            };
            let root = peel_ptr_cast(dst, func, def_inst);
            let Some((def_block, def_idx)) = def_inst.get(&root) else {
                continue;
            };
            let def = &func.blocks[*def_block].insts[*def_idx];
            if let InstKind::AddrOfLocal { local } = def.kind {
                locals.insert(local);
            }
        }
    }
    locals
}

fn build_replacement_map(
    candidates: &[(usize, usize, LocalId, Option<LocalId>, ValueId)],
) -> HashMap<LocalId, ValueId> {
    let mut map = HashMap::new();
    for (_, _, dst_local, _, src) in candidates {
        map.insert(*dst_local, *src);
    }
    map
}

fn resolve_replacement(
    local: LocalId,
    replacements: &HashMap<LocalId, ValueId>,
    func: &Function,
    def_inst: &HashMap<ValueId, (usize, usize)>,
) -> ValueId {
    let mut current = replacements
        .get(&local)
        .copied()
        .expect("missing local replacement");
    let mut seen = HashSet::new();

    loop {
        let root = peel_ptr_cast(current, func, def_inst);
        let Some((def_block, def_idx)) = def_inst.get(&root) else {
            break;
        };
        let def = &func.blocks[*def_block].insts[*def_idx];
        let InstKind::AddrOfLocal { local } = def.kind else {
            break;
        };

        if !seen.insert(local) {
            break;
        }

        let Some(next) = replacements.get(&local) else {
            break;
        };
        current = *next;
    }

    current
}
