//! Live interval construction for SSA values.

use std::collections::HashMap;

use crate::backend::analysis::cfg::Cfg;
use crate::backend::analysis::liveness::LiveMap;
use crate::ir::IrTypeId;
use crate::ir::{Function, InstKind, Terminator, ValueId, for_each_inst_use};

/// Half-open live interval [start, end) in instruction index space.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LiveInterval {
    pub start: u32,
    pub end: u32,
}

/// Live intervals keyed by SSA `ValueId`.
pub type LiveIntervalMap = HashMap<ValueId, LiveInterval>;

/// Combined interval analysis results for SSA regalloc.
#[derive(Debug, Clone)]
pub struct IntervalAnalysis {
    pub intervals: LiveIntervalMap,
    pub call_positions: Vec<u32>,
    pub value_types: HashMap<ValueId, IrTypeId>,
    /// Entry block parameter values in source order.
    pub param_values: Vec<ValueId>,
    /// Values returned from the function, used for result-register precoloring.
    pub return_values: Vec<ValueId>,
}

/// Build SSA live intervals using a linearized instruction index and block live-outs.
pub fn build_live_intervals(func: &Function, live_map: &LiveMap) -> LiveIntervalMap {
    analyze(func, live_map).intervals
}

/// Analyze a function to build intervals, call positions, value type info,
/// and return values for precoloring.
pub fn analyze(func: &Function, live_map: &LiveMap) -> IntervalAnalysis {
    let mut map = LiveIntervalMap::new();
    let mut block_end_idx = vec![0u32; func.blocks.len()];
    let mut call_positions = Vec::new();
    let mut value_types = HashMap::new();
    let mut param_values = Vec::new();
    let mut return_values = Vec::new();

    let mut block_index = HashMap::with_capacity(func.blocks.len());
    for (idx, block) in func.blocks.iter().enumerate() {
        block_index.insert(block.id, idx);
    }

    if let Some(entry) = func.blocks.first() {
        for param in &entry.params {
            param_values.push(param.value.id);
        }
    }

    let cfg = Cfg::new(func);
    let mut inst_idx: u32 = 0;

    for block_id in cfg.rpo() {
        let idx = block_index[&block_id];
        let block = &func.blocks[idx];

        for param in &block.params {
            mark_def(param.value.id, inst_idx, &mut map);
            value_types.insert(param.value.id, param.value.ty);
        }

        for inst in &block.insts {
            for_each_inst_use(&inst.kind, |value| {
                mark_use(value, inst_idx + 1, &mut map);
            });

            if let Some(result) = &inst.result {
                mark_def(result.id, inst_idx, &mut map);
                value_types.insert(result.id, result.ty);
            }

            if matches!(inst.kind, InstKind::Call { .. } | InstKind::Drop { .. }) {
                call_positions.push(inst_idx);
            }

            inst_idx += 1;
        }

        for value in term_uses(&block.term) {
            mark_use(value, inst_idx + 1, &mut map);
        }

        // Track SSA values that flow into the function return.
        if let Terminator::Return { value: Some(value) } = block.term
            && !return_values.contains(&value)
        {
            return_values.push(value);
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

    IntervalAnalysis {
        intervals: map,
        call_positions,
        value_types,
        param_values,
        return_values,
    }
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
        Terminator::Return { value } => value.iter().cloned().collect(),
        Terminator::Unreachable => Vec::new(),
    }
}
