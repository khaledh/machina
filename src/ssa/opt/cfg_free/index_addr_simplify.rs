//! Simplifies index_addr with constant zero offsets.

use std::collections::{HashMap, HashSet};

use crate::ssa::IrTypeId;
use crate::ssa::model::ir::{ConstValue, Function, InstKind, Terminator, ValueId};
use crate::ssa::opt::Pass;

/// Eliminates `index_addr` when the index is constant zero and the pointer type is unchanged.
pub struct IndexAddrSimplify;

impl Pass for IndexAddrSimplify {
    fn name(&self) -> &'static str {
        "ssa-index-addr-simplify"
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
                if *base_ty != result.ty {
                    continue;
                }
                candidates.push((block_idx, inst_idx, result.id, base));
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

fn collect_types_and_zeros(func: &Function) -> (HashMap<ValueId, IrTypeId>, HashSet<ValueId>) {
    let mut types = HashMap::new();
    let mut zeros = HashSet::new();

    if let Some(entry) = func.blocks.first() {
        for param in &entry.params {
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

    (types, zeros)
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
