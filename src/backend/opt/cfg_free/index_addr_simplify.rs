//! Simplifies index_addr with constant zero offsets.

use std::collections::{HashMap, HashSet};

use crate::backend::IrTypeId;
use crate::backend::opt::Pass;
use crate::ir::ir::{
    CastKind, ConstValue, Function, InstKind, Terminator, ValueId, replace_value_in_func,
};

/// Eliminates `index_addr` when the index is constant zero, inserting a ptr cast if needed.
pub struct IndexAddrSimplify;

impl Pass for IndexAddrSimplify {
    fn name(&self) -> &'static str {
        "backend-index-addr-simplify"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let (value_types, const_zero) = collect_types_and_zeros(func);
        let mut candidates = Vec::new();

        for (block_idx, block) in func.blocks.iter().enumerate() {
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                let InstKind::IndexAddr { base, index } = inst.kind else {
                    continue;
                };
                if !const_zero.contains(&index) {
                    continue;
                }
                let Some(result) = &inst.result else {
                    continue;
                };
                let Some(base_ty) = value_types.get(&base) else {
                    continue;
                };
                if *base_ty == result.ty {
                    candidates.push((block_idx, inst_idx, result.id, base, false));
                    continue;
                }

                candidates.push((block_idx, inst_idx, result.id, base, true));
            }
        }

        if candidates.is_empty() {
            return false;
        }

        let remove: HashSet<(usize, usize)> = candidates
            .iter()
            .filter(|(_, _, _, _, needs_cast)| !*needs_cast)
            .map(|(block_idx, inst_idx, _, _, _)| (*block_idx, *inst_idx))
            .collect();

        for (block_idx, inst_idx, from, to, needs_cast) in &candidates {
            if *needs_cast {
                let inst = &mut func.blocks[*block_idx].insts[*inst_idx];
                inst.kind = InstKind::Cast {
                    kind: CastKind::PtrToPtr,
                    value: *to,
                    ty: inst.result.as_ref().expect("cast result").ty,
                };
                continue;
            }
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

fn collect_types_and_zeros(func: &Function) -> (HashMap<ValueId, IrTypeId>, HashSet<ValueId>) {
    let mut types = HashMap::new();
    let mut zeros = HashSet::new();

    for block in &func.blocks {
        for param in &block.params {
            types.insert(param.value.id, param.value.ty);
        }
    }

    for block in &func.blocks {
        for inst in &block.insts {
            if let Some(result) = &inst.result {
                types.insert(result.id, result.ty);
            }
            if let InstKind::Const {
                value: ConstValue::Int { value: 0, .. },
            } = &inst.kind
            {
                if let Some(result) = &inst.result {
                    zeros.insert(result.id);
                }
            }
        }
    }

    let incoming = build_incoming_args(func);
    let mut changed = true;
    while changed {
        changed = false;
        for (block_idx, block) in func.blocks.iter().enumerate() {
            for (param_idx, param) in block.params.iter().enumerate() {
                if zeros.contains(&param.value.id) {
                    continue;
                }
                let args = &incoming[block_idx][param_idx];
                if args.is_empty() {
                    continue;
                }
                if args.iter().all(|arg| zeros.contains(arg)) {
                    zeros.insert(param.value.id);
                    changed = true;
                }
            }
        }
    }

    (types, zeros)
}

fn build_incoming_args(func: &Function) -> Vec<Vec<Vec<ValueId>>> {
    let mut incoming: Vec<Vec<Vec<ValueId>>> = func
        .blocks
        .iter()
        .map(|block| vec![Vec::new(); block.params.len()])
        .collect();

    for block in &func.blocks {
        match &block.term {
            Terminator::Br { target, args } => {
                push_args(&mut incoming, *target, args);
            }
            Terminator::CondBr {
                then_bb,
                then_args,
                else_bb,
                else_args,
                ..
            } => {
                push_args(&mut incoming, *then_bb, then_args);
                push_args(&mut incoming, *else_bb, else_args);
            }
            Terminator::Switch {
                cases,
                default,
                default_args,
                ..
            } => {
                for case in cases {
                    push_args(&mut incoming, case.target, &case.args);
                }
                push_args(&mut incoming, *default, default_args);
            }
            Terminator::Return { .. } | Terminator::Unreachable => {}
        }
    }

    incoming
}

fn push_args(incoming: &mut [Vec<Vec<ValueId>>], target: crate::ir::ir::BlockId, args: &[ValueId]) {
    let slots = &mut incoming[target.index()];
    for (idx, value) in args.iter().enumerate() {
        if let Some(slot) = slots.get_mut(idx) {
            slot.push(*value);
        }
    }
}
