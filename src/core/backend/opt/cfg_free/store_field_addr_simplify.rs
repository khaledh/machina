//! Simplifies store-to-local followed by field-address extraction.

use std::collections::{HashMap, HashSet};

use crate::core::backend::opt::Pass;
use crate::core::ir::IrTypeId;
use crate::core::ir::{
    Function, InstKind, Terminator, ValueId, for_each_inst_use, replace_value_in_func,
};

type ValueDefUseMaps = (
    HashMap<ValueId, IrTypeId>,
    HashMap<ValueId, (usize, usize)>,
    HashMap<ValueId, Vec<(usize, usize)>>,
);

/// Eliminates local stores when the local is only used to take field addresses.
pub struct StoreFieldAddrSimplify;

impl Pass for StoreFieldAddrSimplify {
    fn name(&self) -> &'static str {
        "backend-store-field-addr-simplify"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let (value_types, def_inst, uses) = build_maps(func);
        let term_uses = collect_term_uses(func);
        let mut candidates = Vec::new();

        for (block_idx, block) in func.blocks.iter().enumerate() {
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                let InstKind::Store { ptr, value } = &inst.kind else {
                    continue;
                };

                let Some((def_block, def_idx)) = def_inst.get(ptr) else {
                    continue;
                };
                let def = &func.blocks[*def_block].insts[*def_idx];
                if !matches!(def.kind, InstKind::AddrOfLocal { .. }) {
                    continue;
                }

                let Some((val_block, val_idx)) = def_inst.get(value) else {
                    continue;
                };
                let val_def = &func.blocks[*val_block].insts[*val_idx];
                let InstKind::Load { ptr: src_ptr } = &val_def.kind else {
                    continue;
                };

                let Some(ptr_ty) = value_types.get(ptr) else {
                    continue;
                };
                let Some(src_ty) = value_types.get(src_ptr) else {
                    continue;
                };
                if ptr_ty != src_ty {
                    continue;
                }

                let ignore = Some((block_idx, inst_idx));
                if !is_read_only_ptr(*ptr, func, &uses, &term_uses, ignore, &mut HashSet::new()) {
                    continue;
                }

                candidates.push((block_idx, inst_idx, *ptr, *src_ptr));
            }
        }

        if candidates.is_empty() {
            return false;
        }

        let remove: HashSet<(usize, usize)> = candidates
            .iter()
            .map(|(block_idx, inst_idx, _, _)| (*block_idx, *inst_idx))
            .collect();

        for (block_idx, inst_idx, from, to) in &candidates {
            replace_value_in_func(func, *from, *to, Some((*block_idx, *inst_idx)));
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

fn is_read_only_ptr(
    value: ValueId,
    func: &Function,
    uses: &HashMap<ValueId, Vec<(usize, usize)>>,
    term_uses: &HashSet<ValueId>,
    ignore: Option<(usize, usize)>,
    visiting: &mut HashSet<ValueId>,
) -> bool {
    if !visiting.insert(value) {
        return true;
    }

    if term_uses.contains(&value) {
        return false;
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
                if !is_read_only_ptr(result.id, func, uses, term_uses, ignore, visiting) {
                    return false;
                }
            }
            _ => return false,
        }
    }

    true
}

fn collect_term_uses(func: &Function) -> HashSet<ValueId> {
    let mut uses = HashSet::new();
    for block in &func.blocks {
        match &block.term {
            Terminator::Br { args, .. } => {
                uses.extend(args.iter().copied());
            }
            Terminator::CondBr {
                cond,
                then_args,
                else_args,
                ..
            } => {
                uses.insert(*cond);
                uses.extend(then_args.iter().copied());
                uses.extend(else_args.iter().copied());
            }
            Terminator::Switch {
                value,
                cases,
                default_args,
                ..
            } => {
                uses.insert(*value);
                for case in cases {
                    uses.extend(case.args.iter().copied());
                }
                uses.extend(default_args.iter().copied());
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
