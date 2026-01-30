//! Eliminates redundant local-to-local MemCopy when the source stays stable.

use std::collections::{HashMap, HashSet};

use crate::ssa::IrTypeId;
use crate::ssa::model::ir::{
    Function, InstKind, ValueId, for_each_inst_use, replace_value_in_func,
};
use crate::ssa::opt::Pass;

/// Drops MemCopy between locals when the destination is read-only and the source is not written after.
pub struct LocalMemCopyElim;

impl Pass for LocalMemCopyElim {
    fn name(&self) -> &'static str {
        "local-memcpy-elim"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let (value_types, def_inst, uses) = build_maps(func);
        let term_uses = collect_term_uses(func);
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
                if !is_read_only_ptr(*dst, func, &uses, ignore, &mut HashSet::new()) {
                    continue;
                }

                if !source_stable_after(
                    *src,
                    block_idx,
                    inst_idx,
                    func,
                    &uses,
                    ignore,
                    &mut HashSet::new(),
                ) {
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
    HashMap<ValueId, IrTypeId>,
    HashMap<ValueId, (usize, usize)>,
    HashMap<ValueId, Vec<(usize, usize)>>,
) {
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

fn collect_term_uses(func: &Function) -> HashSet<ValueId> {
    let mut uses = HashSet::new();
    for block in &func.blocks {
        match &block.term {
            crate::ssa::model::ir::Terminator::Br { args, .. } => {
                uses.extend(args.iter().cloned());
            }
            crate::ssa::model::ir::Terminator::CondBr {
                cond,
                then_args,
                else_args,
                ..
            } => {
                uses.insert(*cond);
                uses.extend(then_args.iter().cloned());
                uses.extend(else_args.iter().cloned());
            }
            crate::ssa::model::ir::Terminator::Switch {
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
            crate::ssa::model::ir::Terminator::Return { value } => {
                if let Some(value) = value {
                    uses.insert(*value);
                }
            }
            crate::ssa::model::ir::Terminator::Unreachable => {}
        }
    }
    uses
}

fn is_read_only_ptr(
    value: ValueId,
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

    for (block_idx, inst_idx) in users {
        if Some((*block_idx, *inst_idx)) == ignore {
            continue;
        }

        let inst = &func.blocks[*block_idx].insts[*inst_idx];
        match &inst.kind {
            InstKind::Load { .. } => {}
            InstKind::FieldAddr { .. } | InstKind::IndexAddr { .. } => {
                let Some(result) = &inst.result else {
                    return false;
                };
                if !is_read_only_ptr(result.id, func, uses, ignore, visiting) {
                    return false;
                }
            }
            _ => return false,
        }
    }

    true
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
