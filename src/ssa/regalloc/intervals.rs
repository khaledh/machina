//! Live interval construction for SSA values.

use std::collections::HashMap;

use crate::ssa::analysis::cfg::Cfg;
use crate::ssa::analysis::liveness::LiveMap;
use crate::ssa::model::ir::{Callee, Function, InstKind, Terminator, ValueId};

/// Half-open live interval [start, end) in instruction index space.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LiveInterval {
    pub start: u32,
    pub end: u32,
}

/// Live intervals keyed by SSA `ValueId`.
pub type LiveIntervalMap = HashMap<ValueId, LiveInterval>;

/// Build SSA live intervals using a linearized instruction index and block live-outs.
pub fn build_live_intervals(func: &Function, live_map: &LiveMap) -> LiveIntervalMap {
    let mut map = LiveIntervalMap::new();
    let mut block_end_idx = vec![0u32; func.blocks.len()];

    let mut block_index = HashMap::with_capacity(func.blocks.len());
    for (idx, block) in func.blocks.iter().enumerate() {
        block_index.insert(block.id, idx);
    }

    let cfg = Cfg::new(func);
    let mut inst_idx: u32 = 0;

    for block_id in cfg.rpo() {
        let idx = block_index[&block_id];
        let block = &func.blocks[idx];

        for param in &block.params {
            mark_def(param.value.id, inst_idx, &mut map);
        }

        for inst in &block.insts {
            for value in inst_uses(&inst.kind) {
                mark_use(value, inst_idx + 1, &mut map);
            }

            if let Some(result) = &inst.result {
                mark_def(result.id, inst_idx, &mut map);
            }

            inst_idx += 1;
        }

        for value in term_uses(&block.term) {
            mark_use(value, inst_idx + 1, &mut map);
        }

        inst_idx += 1; // Terminator slot.
        block_end_idx[idx] = inst_idx;
    }

    for (idx, live) in live_map.iter().enumerate() {
        for value in &live.live_out {
            let end = block_end_idx[idx] + 1;
            map.entry(*value)
                .and_modify(|iv| iv.end = iv.end.max(end))
                .or_insert(LiveInterval {
                    start: block_end_idx[idx],
                    end,
                });
        }
    }

    map
}

fn mark_use(value: ValueId, end: u32, map: &mut LiveIntervalMap) {
    map.entry(value)
        .and_modify(|iv| iv.end = iv.end.max(end))
        .or_insert(LiveInterval {
            start: end.saturating_sub(1),
            end,
        });
}

fn mark_def(value: ValueId, start: u32, map: &mut LiveIntervalMap) {
    map.entry(value)
        .and_modify(|iv| {
            if start < iv.start {
                iv.start = start;
            }
            iv.end = iv.end.max(start + 1);
        })
        .or_insert(LiveInterval {
            start,
            end: start + 1,
        });
}

fn inst_uses(kind: &InstKind) -> Vec<ValueId> {
    match kind {
        InstKind::Const { .. } | InstKind::AddrOfLocal { .. } => Vec::new(),
        InstKind::BinOp { lhs, rhs, .. } | InstKind::Cmp { lhs, rhs, .. } => {
            vec![*lhs, *rhs]
        }
        InstKind::UnOp { value, .. }
        | InstKind::Cast { value, .. }
        | InstKind::FieldAddr { base: value, .. }
        | InstKind::Load { ptr: value } => vec![*value],
        InstKind::IndexAddr { base, index } => vec![*base, *index],
        InstKind::Store { ptr, value } => vec![*ptr, *value],
        InstKind::MemCopy { dst, src, len } => vec![*dst, *src, *len],
        InstKind::MemSet { dst, byte, len } => vec![*dst, *byte, *len],
        InstKind::Call { callee, args } => {
            let mut values = Vec::with_capacity(args.len() + 1);
            if let Callee::Value(value) = callee {
                values.push(*value);
            }
            values.extend(args.iter().cloned());
            values
        }
        InstKind::Drop { ptr } => vec![*ptr],
    }
}

fn term_uses(term: &Terminator) -> Vec<ValueId> {
    match term {
        Terminator::Br { args, .. } => args.clone(),
        Terminator::CondBr {
            cond,
            then_args,
            else_args,
            ..
        } => {
            let mut values = Vec::with_capacity(1 + then_args.len() + else_args.len());
            values.push(*cond);
            values.extend(then_args.iter().cloned());
            values.extend(else_args.iter().cloned());
            values
        }
        Terminator::Switch {
            value,
            cases,
            default_args,
            ..
        } => {
            let mut values = Vec::new();
            values.push(*value);
            for case in cases {
                values.extend(case.args.iter().cloned());
            }
            values.extend(default_args.iter().cloned());
            values
        }
        Terminator::Return { value } => value.into_iter().cloned().collect(),
        Terminator::Unreachable => Vec::new(),
    }
}
