use std::collections::{HashMap, HashSet};

use crate::liveness::{LiveMap, collect_operand_uses, stmt_defs, stmt_uses};
use crate::mcir::types::{FuncBody, LocalId, LocalKind, Terminator};

// -- Live intervals computation ---

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LiveInterval {
    pub start: u32,
    pub end: u32,
}

pub type LiveIntervalMap = HashMap<LocalId, LiveInterval>;

pub(crate) fn build_live_intervals(body: &FuncBody, live_map: &LiveMap) -> LiveIntervalMap {
    let mut map = LiveIntervalMap::new();
    let mut block_last_inst_idx = vec![0u32; body.blocks.len()];
    let mut inst_idx: u32 = 0;

    for (i, block) in body.blocks.iter().enumerate() {
        for stmt in &block.stmts {
            let mut uses = HashSet::new();
            stmt_uses(stmt, &mut uses);
            for u in uses {
                map.entry(u)
                    .and_modify(|iv| iv.end = inst_idx + 1)
                    .or_insert(LiveInterval {
                        start: inst_idx,
                        end: inst_idx + 1,
                    });
            }

            let mut defs = HashSet::new();
            stmt_defs(stmt, &mut defs);
            for d in defs {
                map.entry(d)
                    .and_modify(|iv| iv.end = inst_idx + 1)
                    .or_insert(LiveInterval {
                        start: inst_idx,
                        end: inst_idx + 1,
                    });
            }

            inst_idx += 1;
        }

        if let Terminator::If { cond, .. } = &block.terminator {
            let mut uses = HashSet::new();
            collect_operand_uses(cond, &mut uses);
            for u in uses {
                map.entry(u)
                    .and_modify(|iv| iv.end = inst_idx + 1)
                    .or_insert(LiveInterval {
                        start: inst_idx,
                        end: inst_idx + 1,
                    });
            }
        }

        inst_idx += 1; // terminator slot
        block_last_inst_idx[i] = inst_idx;
    }

    for (i, live) in live_map.iter().enumerate() {
        for local in &live.live_out {
            map.entry(*local)
                .and_modify(|iv| iv.end = iv.end.max(block_last_inst_idx[i] + 1))
                .or_insert(LiveInterval {
                    start: block_last_inst_idx[i],
                    end: block_last_inst_idx[i] + 1,
                });
        }
    }

    // Ensure params (and aggregate return pointers) are live from entry.
    let ret_local = body.ret_local;
    let ret_ty = body.locals[ret_local.index()].ty;
    let ret_is_agg = body.types.get(ret_ty).is_aggregate();
    for (i, local) in body.locals.iter().enumerate() {
        let local_id = LocalId(i as u32);
        let is_param =
            matches!(local.kind, LocalKind::Param { .. }) || (ret_is_agg && local_id == ret_local);
        if is_param && let Some(iv) = map.get_mut(&local_id) {
            iv.start = 0;
        }
    }

    map
}

/// Format live intervals for human-readable output.
pub fn format_live_intervals(intervals: &LiveIntervalMap, func_name: &str) -> String {
    let mut out = String::new();
    out.push_str(&format!("Live Intervals ({}):\n", func_name));
    out.push_str("--------------------------------\n");
    let mut ids: Vec<_> = intervals.keys().collect();
    ids.sort_by_key(|id| id.0);
    for id in ids {
        let interval = intervals.get(id).unwrap();
        out.push_str(&format!(
            "  %t{}: [{}; {})\n",
            id.0, interval.start, interval.end
        ));
    }
    out.push_str("--------------------------------\n");
    out
}
