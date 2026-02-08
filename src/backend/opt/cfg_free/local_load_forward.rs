//! Forwards stores into subsequent loads from the same local address.

use std::collections::{HashMap, HashSet};

use crate::backend::opt::Pass;
use crate::ir::{Callee, Function, InstKind, Terminator, ValueId, replace_value_in_func};

/// Eliminates `load` from a local address when a dominating store is in the same block.
pub struct LocalLoadForward;

impl Pass for LocalLoadForward {
    fn name(&self) -> &'static str {
        "local-load-forward"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let safe_ptrs = collect_safe_addr_of_locals(func);
        if safe_ptrs.is_empty() {
            return false;
        }

        let mut replacements = Vec::new();
        let mut remove: HashSet<(usize, usize)> = HashSet::new();

        for (block_idx, block) in func.blocks.iter().enumerate() {
            let mut last_store: HashMap<ValueId, ValueId> = HashMap::new();

            for (inst_idx, inst) in block.insts.iter().enumerate() {
                match &inst.kind {
                    InstKind::Store { ptr, value } if safe_ptrs.contains(ptr) => {
                        last_store.insert(*ptr, *value);
                    }
                    InstKind::Load { ptr } if safe_ptrs.contains(ptr) => {
                        let Some(stored) = last_store.get(ptr).copied() else {
                            continue;
                        };
                        let Some(result) = &inst.result else {
                            continue;
                        };
                        replacements.push((block_idx, inst_idx, result.id, stored));
                        remove.insert((block_idx, inst_idx));
                    }
                    _ => {}
                }
            }
        }

        if replacements.is_empty() {
            return false;
        }

        let replacement_map: HashMap<ValueId, ValueId> = replacements
            .iter()
            .map(|(_, _, from, to)| (*from, *to))
            .collect();

        for (block_idx, inst_idx, from, to) in &replacements {
            let resolved = resolve_replacement_target(*to, &replacement_map);
            if resolved == *from {
                continue;
            }
            replace_value_in_func(func, *from, resolved, Some((*block_idx, *inst_idx)));
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

fn resolve_replacement_target(
    start: ValueId,
    replacement_map: &HashMap<ValueId, ValueId>,
) -> ValueId {
    let mut curr = start;
    let mut seen = HashSet::new();
    while let Some(next) = replacement_map.get(&curr).copied() {
        if !seen.insert(curr) {
            break;
        }
        curr = next;
    }
    curr
}

fn collect_safe_addr_of_locals(func: &Function) -> HashSet<ValueId> {
    let mut addr_ptrs = HashSet::new();
    for block in &func.blocks {
        for inst in &block.insts {
            if let InstKind::AddrOfLocal { .. } = inst.kind
                && let Some(result) = &inst.result
            {
                addr_ptrs.insert(result.id);
            }
        }
    }

    if addr_ptrs.is_empty() {
        return addr_ptrs;
    }

    let mut unsafe_ptrs = HashSet::new();

    for block in &func.blocks {
        for inst in &block.insts {
            match &inst.kind {
                InstKind::Load { ptr: _ } => {}
                InstKind::Store { ptr: _, value } => {
                    if addr_ptrs.contains(value) {
                        unsafe_ptrs.insert(*value);
                    }
                }
                InstKind::FieldAddr { base, .. } => {
                    if addr_ptrs.contains(base) {
                        unsafe_ptrs.insert(*base);
                    }
                }
                InstKind::IndexAddr { base, index } => {
                    if addr_ptrs.contains(base) {
                        unsafe_ptrs.insert(*base);
                    }
                    if addr_ptrs.contains(index) {
                        unsafe_ptrs.insert(*index);
                    }
                }
                InstKind::MemCopy { dst, src, len } => {
                    if addr_ptrs.contains(dst) {
                        unsafe_ptrs.insert(*dst);
                    }
                    if addr_ptrs.contains(src) {
                        unsafe_ptrs.insert(*src);
                    }
                    if addr_ptrs.contains(len) {
                        unsafe_ptrs.insert(*len);
                    }
                }
                InstKind::MemSet { dst, byte, len } => {
                    if addr_ptrs.contains(dst) {
                        unsafe_ptrs.insert(*dst);
                    }
                    if addr_ptrs.contains(byte) {
                        unsafe_ptrs.insert(*byte);
                    }
                    if addr_ptrs.contains(len) {
                        unsafe_ptrs.insert(*len);
                    }
                }
                InstKind::Call { callee, args } => {
                    if let Callee::Value(value) = callee
                        && addr_ptrs.contains(value)
                    {
                        unsafe_ptrs.insert(*value);
                    }
                    for arg in args {
                        if addr_ptrs.contains(arg) {
                            unsafe_ptrs.insert(*arg);
                        }
                    }
                }
                InstKind::Drop { ptr } => {
                    if addr_ptrs.contains(ptr) {
                        unsafe_ptrs.insert(*ptr);
                    }
                }
                InstKind::BinOp { lhs, rhs, .. } | InstKind::Cmp { lhs, rhs, .. } => {
                    if addr_ptrs.contains(lhs) {
                        unsafe_ptrs.insert(*lhs);
                    }
                    if addr_ptrs.contains(rhs) {
                        unsafe_ptrs.insert(*rhs);
                    }
                }
                InstKind::UnOp { value, .. }
                | InstKind::IntTrunc { value, .. }
                | InstKind::IntExtend { value, .. }
                | InstKind::Cast { value, .. } => {
                    if addr_ptrs.contains(value) {
                        unsafe_ptrs.insert(*value);
                    }
                }
                InstKind::Const { .. } | InstKind::AddrOfLocal { .. } => {}
            }
        }

        mark_term_uses(&block.term, &addr_ptrs, &mut unsafe_ptrs);
    }

    addr_ptrs.retain(|ptr| !unsafe_ptrs.contains(ptr));
    addr_ptrs
}

fn mark_term_uses(
    term: &Terminator,
    addr_ptrs: &HashSet<ValueId>,
    unsafe_ptrs: &mut HashSet<ValueId>,
) {
    let mut mark = |value: &ValueId| {
        if addr_ptrs.contains(value) {
            unsafe_ptrs.insert(*value);
        }
    };

    match term {
        Terminator::Br { args, .. } => {
            for value in args {
                mark(value);
            }
        }
        Terminator::CondBr {
            cond,
            then_args,
            else_args,
            ..
        } => {
            mark(cond);
            for value in then_args {
                mark(value);
            }
            for value in else_args {
                mark(value);
            }
        }
        Terminator::Switch {
            value,
            cases,
            default_args,
            ..
        } => {
            mark(value);
            for case in cases {
                for value in &case.args {
                    mark(value);
                }
            }
            for value in default_args {
                mark(value);
            }
        }
        Terminator::Return { value } => {
            if let Some(value) = value {
                mark(value);
            }
        }
        Terminator::Unreachable => {}
    }
}
