//! Simplifies store-to-local followed by field-address extraction.

use std::collections::{HashMap, HashSet};

use crate::ssa::IrTypeId;
use crate::ssa::model::ir::{Function, InstKind, Terminator, ValueId};
use crate::ssa::opt::Pass;

/// Eliminates local stores when the local is only used to take field addresses.
pub struct StoreFieldAddrSimplify;

impl Pass for StoreFieldAddrSimplify {
    fn name(&self) -> &'static str {
        "ssa-store-field-addr-simplify"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let (value_types, def_inst, uses) = build_maps(func);
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
                if !is_read_only_ptr(*ptr, func, &uses, ignore, &mut HashSet::new()) {
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

            for value in inst_uses(&inst.kind) {
                uses.entry(value).or_default().push((block_idx, inst_idx));
            }
        }
    }

    (value_types, def_inst, uses)
}

fn inst_uses(kind: &InstKind) -> Vec<ValueId> {
    match kind {
        InstKind::Const { .. } | InstKind::AddrOfLocal { .. } => Vec::new(),
        InstKind::BinOp { lhs, rhs, .. } | InstKind::Cmp { lhs, rhs, .. } => {
            vec![*lhs, *rhs]
        }
        InstKind::UnOp { value, .. }
        | InstKind::IntTrunc { value, .. }
        | InstKind::IntExtend { value, .. }
        | InstKind::Cast { value, .. }
        | InstKind::FieldAddr { base: value, .. }
        | InstKind::Load { ptr: value } => vec![*value],
        InstKind::IndexAddr { base, index } => vec![*base, *index],
        InstKind::Store { ptr, value } => vec![*ptr, *value],
        InstKind::MemCopy { dst, src, len } => vec![*dst, *src, *len],
        InstKind::MemSet { dst, byte, len } => vec![*dst, *byte, *len],
        InstKind::Call { callee, args } => {
            let mut values = Vec::with_capacity(args.len() + 1);
            if let crate::ssa::model::ir::Callee::Value(value) = callee {
                values.push(*value);
            }
            values.extend(args.iter().cloned());
            values
        }
        InstKind::Drop { ptr } => vec![*ptr],
    }
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

fn replace_value_in_func(
    func: &mut Function,
    from: ValueId,
    to: ValueId,
    ignore: Option<(usize, usize)>,
) {
    for (block_idx, block) in func.blocks.iter_mut().enumerate() {
        for (inst_idx, inst) in block.insts.iter_mut().enumerate() {
            if Some((block_idx, inst_idx)) == ignore {
                continue;
            }
            replace_value_in_inst(&mut inst.kind, from, to);
        }
        replace_value_in_term(&mut block.term, from, to);
    }
}

fn replace_value_in_inst(kind: &mut InstKind, from: ValueId, to: ValueId) {
    let replace = |value: &mut ValueId| {
        if *value == from {
            *value = to;
        }
    };

    match kind {
        InstKind::BinOp { lhs, rhs, .. } | InstKind::Cmp { lhs, rhs, .. } => {
            replace(lhs);
            replace(rhs);
        }
        InstKind::UnOp { value, .. }
        | InstKind::IntTrunc { value, .. }
        | InstKind::IntExtend { value, .. }
        | InstKind::Cast { value, .. }
        | InstKind::FieldAddr { base: value, .. }
        | InstKind::Load { ptr: value } => replace(value),
        InstKind::IndexAddr { base, index } => {
            replace(base);
            replace(index);
        }
        InstKind::Store { ptr, value } => {
            replace(ptr);
            replace(value);
        }
        InstKind::MemCopy { dst, src, len } => {
            replace(dst);
            replace(src);
            replace(len);
        }
        InstKind::MemSet { dst, byte, len } => {
            replace(dst);
            replace(byte);
            replace(len);
        }
        InstKind::Call { callee, args } => {
            if let crate::ssa::model::ir::Callee::Value(value) = callee {
                replace(value);
            }
            for arg in args {
                replace(arg);
            }
        }
        InstKind::Drop { ptr } => replace(ptr),
        InstKind::Const { .. } | InstKind::AddrOfLocal { .. } => {}
    }
}

fn replace_value_in_term(term: &mut Terminator, from: ValueId, to: ValueId) {
    let replace = |value: &mut ValueId| {
        if *value == from {
            *value = to;
        }
    };

    match term {
        Terminator::Br { args, .. } => {
            for value in args {
                replace(value);
            }
        }
        Terminator::CondBr {
            cond,
            then_args,
            else_args,
            ..
        } => {
            replace(cond);
            for value in then_args {
                replace(value);
            }
            for value in else_args {
                replace(value);
            }
        }
        Terminator::Switch {
            value,
            cases,
            default_args,
            ..
        } => {
            replace(value);
            for case in cases {
                for arg in &mut case.args {
                    replace(arg);
                }
            }
            for value in default_args {
                replace(value);
            }
        }
        Terminator::Return { value } => {
            if let Some(value) = value {
                replace(value);
            }
        }
        Terminator::Unreachable => {}
    }
}
