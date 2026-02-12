//! CFG-based SSA optimizations.

use std::collections::{HashMap, HashSet};

use crate::backend::analysis::cfg::Cfg;
use crate::backend::opt::Pass;
use crate::ir::{Block, BlockId, Callee, Function, InstKind, Terminator, ValueId};

pub struct CfgCleanup;

impl Pass for CfgCleanup {
    fn name(&self) -> &'static str {
        "backend-cfg-cleanup"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let mut changed = false;
        const MAX_ITERS: usize = 4;

        for _ in 0..MAX_ITERS {
            let mut iter_changed = false;

            if simplify_terminators(func) {
                iter_changed = true;
            }

            if remove_unreachable(func) {
                iter_changed = true;
            }

            if remove_empty_blocks(func) {
                iter_changed = true;
            }

            if merge_single_pred_blocks(func) {
                iter_changed = true;
            }

            if prune_unused_block_params(func) {
                iter_changed = true;
            }

            if iter_changed {
                renumber_blocks(func);
                changed = true;
            } else {
                break;
            }
        }

        changed
    }
}

pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl Default for PassManager {
    fn default() -> Self {
        Self::new()
    }
}

impl PassManager {
    pub fn new() -> Self {
        Self {
            passes: vec![Box::new(CfgCleanup)],
        }
    }

    pub fn run(&mut self, funcs: &mut [Function]) {
        for pass in &mut self.passes {
            for func in funcs.iter_mut() {
                pass.run(func);
            }
        }
    }
}

fn simplify_terminators(func: &mut Function) -> bool {
    let mut changed = false;

    for block in &mut func.blocks {
        match &mut block.term {
            Terminator::CondBr {
                then_bb,
                then_args,
                else_bb,
                else_args,
                ..
            } => {
                if then_bb == else_bb && then_args == else_args {
                    let target = *then_bb;
                    let args = then_args.clone();
                    block.term = Terminator::Br { target, args };
                    changed = true;
                }
            }
            Terminator::Switch {
                cases,
                default,
                default_args,
                ..
            } => {
                if cases.is_empty() {
                    let target = *default;
                    let args = default_args.clone();
                    block.term = Terminator::Br { target, args };
                    changed = true;
                    continue;
                }

                let all_same = cases
                    .iter()
                    .all(|case| case.target == *default && case.args == *default_args);
                if all_same {
                    let target = *default;
                    let args = default_args.clone();
                    block.term = Terminator::Br { target, args };
                    changed = true;
                }
            }
            Terminator::Br { .. } | Terminator::Return { .. } | Terminator::Unreachable => {}
        }
    }

    changed
}

fn remove_unreachable(func: &mut Function) -> bool {
    if func.blocks.is_empty() {
        return false;
    }

    let cfg = Cfg::new(func);
    let reachable: HashSet<BlockId> = cfg.postorder().into_iter().collect();
    let before = func.blocks.len();
    func.blocks.retain(|block| reachable.contains(&block.id));
    func.blocks.len() != before
}

fn remove_empty_blocks(func: &mut Function) -> bool {
    let mut changed = false;

    loop {
        let mut candidates: Vec<(BlockId, BlockId)> = Vec::new();
        let entry = func.blocks.first().map(|block| block.id);

        for block in &func.blocks {
            if Some(block.id) == entry {
                continue;
            }
            if let Some(target) = empty_br_target(block)
                && incoming_args_empty(func, block.id)
            {
                candidates.push((block.id, target));
            }
        }

        if candidates.is_empty() {
            break;
        }

        let mut map: HashMap<BlockId, BlockId> = candidates.into_iter().collect();
        for key in map.clone().keys().cloned().collect::<Vec<_>>() {
            let target = resolve_target(key, &map);
            map.insert(key, target);
        }

        for block in &mut func.blocks {
            rewrite_term_targets(&mut block.term, &map);
        }

        let before = func.blocks.len();
        func.blocks.retain(|block| !map.contains_key(&block.id));
        if func.blocks.len() != before {
            changed = true;
        }
    }

    changed
}

fn merge_single_pred_blocks(func: &mut Function) -> bool {
    let mut changed = false;

    loop {
        let mut pred_count: HashMap<BlockId, usize> = HashMap::new();
        let mut br_pred: HashMap<BlockId, BlockId> = HashMap::new();

        for block in &func.blocks {
            match &block.term {
                Terminator::Br { target, args } => {
                    *pred_count.entry(*target).or_insert(0) += 1;
                    if args.is_empty() {
                        br_pred.entry(*target).or_insert(block.id);
                    }
                }
                Terminator::CondBr {
                    then_bb, else_bb, ..
                } => {
                    *pred_count.entry(*then_bb).or_insert(0) += 1;
                    *pred_count.entry(*else_bb).or_insert(0) += 1;
                }
                Terminator::Switch { cases, default, .. } => {
                    *pred_count.entry(*default).or_insert(0) += 1;
                    for case in cases {
                        *pred_count.entry(case.target).or_insert(0) += 1;
                    }
                }
                Terminator::Return { .. } | Terminator::Unreachable => {}
            }
        }

        let mut candidate = None;
        for block in &func.blocks {
            if !block.params.is_empty() {
                continue;
            }
            let count = *pred_count.get(&block.id).unwrap_or(&0);
            if count == 1
                && let Some(pred) = br_pred.get(&block.id)
            {
                candidate = Some((*pred, block.id));
                break;
            }
        }

        let Some((pred, block_id)) = candidate else {
            break;
        };

        let pred_idx = func
            .blocks
            .iter()
            .position(|block| block.id == pred)
            .expect("missing merge predecessor");
        let block_idx = func
            .blocks
            .iter()
            .position(|block| block.id == block_id)
            .expect("missing merge block");

        let insts = func.blocks[block_idx].insts.clone();
        let term = func.blocks[block_idx].term.clone();
        func.blocks[pred_idx].insts.extend(insts);
        func.blocks[pred_idx].term = term;
        func.blocks.remove(block_idx);
        changed = true;
    }

    changed
}

fn prune_unused_block_params(func: &mut Function) -> bool {
    let mut changed = false;
    let mut updates: Vec<(BlockId, Vec<usize>)> = Vec::new();
    let mut used_values: HashSet<ValueId> = HashSet::new();

    for block in &func.blocks {
        for inst in &block.insts {
            collect_inst_uses(&inst.kind, &mut used_values);
        }
        collect_term_uses(&block.term, &mut used_values);
    }

    for block in &mut func.blocks {
        if block.id == BlockId(0) {
            continue;
        }
        if block.params.is_empty() {
            continue;
        }

        let mut used = HashSet::new();
        for inst in &block.insts {
            collect_inst_uses(&inst.kind, &mut used);
        }
        collect_term_uses(&block.term, &mut used);

        let mut removed = Vec::new();
        for (idx, param) in block.params.iter().enumerate() {
            if !used.contains(&param.value.id) && !used_values.contains(&param.value.id) {
                removed.push(idx);
            }
        }

        if removed.is_empty() {
            continue;
        }

        for idx in removed.iter().rev().copied() {
            block.params.remove(idx);
        }

        updates.push((block.id, removed));
        changed = true;
    }

    for (block_id, removed) in &updates {
        for other in &mut func.blocks {
            remove_args_for_target(&mut other.term, *block_id, removed);
        }
    }

    changed
}

fn remove_args_for_target(term: &mut Terminator, target: BlockId, removed: &[usize]) {
    match term {
        Terminator::Br {
            target: term_target,
            args,
        } => {
            if *term_target == target {
                remove_indices(args, removed);
            }
        }
        Terminator::CondBr {
            then_bb,
            then_args,
            else_bb,
            else_args,
            ..
        } => {
            if *then_bb == target {
                remove_indices(then_args, removed);
            }
            if *else_bb == target {
                remove_indices(else_args, removed);
            }
        }
        Terminator::Switch {
            cases,
            default,
            default_args,
            ..
        } => {
            if *default == target {
                remove_indices(default_args, removed);
            }
            for case in cases {
                if case.target == target {
                    remove_indices(&mut case.args, removed);
                }
            }
        }
        Terminator::Return { .. } | Terminator::Unreachable => {}
    }
}

fn remove_indices(args: &mut Vec<ValueId>, removed: &[usize]) {
    for idx in removed.iter().rev().copied() {
        args.remove(idx);
    }
}

fn collect_inst_uses(kind: &InstKind, used: &mut HashSet<ValueId>) {
    match kind {
        InstKind::Const { .. } | InstKind::AddrOfLocal { .. } => {}
        InstKind::BinOp { lhs, rhs, .. } | InstKind::Cmp { lhs, rhs, .. } => {
            used.insert(*lhs);
            used.insert(*rhs);
        }
        InstKind::UnOp { value, .. }
        | InstKind::IntTrunc { value, .. }
        | InstKind::IntExtend { value, .. }
        | InstKind::Cast { value, .. }
        | InstKind::FieldAddr { base: value, .. }
        | InstKind::Load { ptr: value } => {
            used.insert(*value);
        }
        InstKind::IndexAddr { base, index } => {
            used.insert(*base);
            used.insert(*index);
        }
        InstKind::Store { ptr, value } => {
            used.insert(*ptr);
            used.insert(*value);
        }
        InstKind::MemCopy { dst, src, len } => {
            used.insert(*dst);
            used.insert(*src);
            used.insert(*len);
        }
        InstKind::MemSet { dst, byte, len } => {
            used.insert(*dst);
            used.insert(*byte);
            used.insert(*len);
        }
        InstKind::Call { callee, args } => {
            if let Callee::Value(value) = callee {
                used.insert(*value);
            }
            used.extend(args.iter().copied());
        }
        InstKind::Drop { ptr } => {
            used.insert(*ptr);
        }
    }
}

fn collect_term_uses(term: &Terminator, used: &mut HashSet<ValueId>) {
    match term {
        Terminator::Br { args, .. } => {
            used.extend(args.iter().copied());
        }
        Terminator::CondBr {
            cond,
            then_args,
            else_args,
            ..
        } => {
            used.insert(*cond);
            used.extend(then_args.iter().copied());
            used.extend(else_args.iter().copied());
        }
        Terminator::Switch {
            value,
            cases,
            default_args,
            ..
        } => {
            used.insert(*value);
            for case in cases {
                used.extend(case.args.iter().copied());
            }
            used.extend(default_args.iter().copied());
        }
        Terminator::Return { value } => {
            if let Some(value) = value {
                used.insert(*value);
            }
        }
        Terminator::Unreachable => {}
    }
}

fn empty_br_target(block: &Block) -> Option<BlockId> {
    if !block.params.is_empty() || !block.insts.is_empty() {
        return None;
    }

    match &block.term {
        Terminator::Br { target, args } if args.is_empty() && *target != block.id => Some(*target),
        _ => None,
    }
}

fn incoming_args_empty(func: &Function, target: BlockId) -> bool {
    for block in &func.blocks {
        match &block.term {
            Terminator::Br {
                target: term_target,
                args,
            } => {
                if *term_target == target && !args.is_empty() {
                    return false;
                }
            }
            Terminator::CondBr {
                then_bb,
                then_args,
                else_bb,
                else_args,
                ..
            } => {
                if *then_bb == target && !then_args.is_empty() {
                    return false;
                }
                if *else_bb == target && !else_args.is_empty() {
                    return false;
                }
            }
            Terminator::Switch {
                cases,
                default,
                default_args,
                ..
            } => {
                if *default == target && !default_args.is_empty() {
                    return false;
                }
                for case in cases {
                    if case.target == target && !case.args.is_empty() {
                        return false;
                    }
                }
            }
            Terminator::Return { .. } | Terminator::Unreachable => {}
        }
    }

    true
}

fn resolve_target(start: BlockId, map: &HashMap<BlockId, BlockId>) -> BlockId {
    let mut current = start;
    let mut visited = HashSet::new();
    while let Some(next) = map.get(&current).copied() {
        if next == current || !visited.insert(current) {
            break;
        }
        current = next;
    }
    current
}

fn rewrite_term_targets(term: &mut Terminator, map: &HashMap<BlockId, BlockId>) {
    match term {
        Terminator::Br { target, .. } => {
            if let Some(mapped) = map.get(target) {
                *target = *mapped;
            }
        }
        Terminator::CondBr {
            then_bb, else_bb, ..
        } => {
            if let Some(mapped) = map.get(then_bb) {
                *then_bb = *mapped;
            }
            if let Some(mapped) = map.get(else_bb) {
                *else_bb = *mapped;
            }
        }
        Terminator::Switch { cases, default, .. } => {
            if let Some(mapped) = map.get(default) {
                *default = *mapped;
            }
            for case in cases {
                if let Some(mapped) = map.get(&case.target) {
                    case.target = *mapped;
                }
            }
        }
        Terminator::Return { .. } | Terminator::Unreachable => {}
    }
}

fn renumber_blocks(func: &mut Function) {
    let mut map = HashMap::new();
    for (idx, block) in func.blocks.iter_mut().enumerate() {
        let new_id = BlockId(idx as u32);
        map.insert(block.id, new_id);
        block.id = new_id;
    }

    for block in &mut func.blocks {
        rewrite_term_targets(&mut block.term, &map);
    }
}

#[cfg(test)]
#[path = "../../../../tests/backend/opt/cfg/mod.rs"]
mod tests;
