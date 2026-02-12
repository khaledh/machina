use std::collections::HashSet;

use crate::core::analysis::dataflow::solve_backward;
use crate::core::resolve::DefId;
use crate::core::tree::cfg::{TreeCfg, TreeCfgItem, TreeCfgNode, TreeCfgTerminator};
use crate::core::types::TypeId;

pub(crate) struct LivenessResult {
    pub(crate) live_in: Vec<HashSet<DefId>>,
    pub(crate) live_out: Vec<HashSet<DefId>>,
    pub(crate) live_after: Vec<Vec<HashSet<DefId>>>,
}

pub(crate) fn analyze_liveness<AT, CI>(
    cfg: &TreeCfg<'_, TypeId>,
    add_terminator_uses: AT,
    collect_item_defs_uses: CI,
) -> LivenessResult
where
    AT: Fn(&TreeCfgTerminator<'_, TypeId>, &mut HashSet<DefId>),
    CI: Fn(&TreeCfgItem<'_, TypeId>, &mut HashSet<DefId>, &mut HashSet<DefId>),
{
    let entry = HashSet::new();
    let bottom = HashSet::new();

    let analysis = solve_backward(
        cfg,
        entry,
        bottom,
        |states| {
            let mut out = HashSet::new();
            for state in states {
                out.extend(state.iter().copied());
            }
            out
        },
        |block_id, out_state| {
            compute_live_in(
                &cfg.nodes[block_id.0],
                out_state,
                &add_terminator_uses,
                &collect_item_defs_uses,
            )
        },
    );

    let live_in = analysis.in_map;
    let live_out = analysis.out_map;
    let live_after = cfg
        .nodes
        .iter()
        .enumerate()
        .map(|(idx, node)| {
            compute_live_after(
                node,
                &live_out[idx],
                &add_terminator_uses,
                &collect_item_defs_uses,
            )
        })
        .collect();

    LivenessResult {
        live_in,
        live_out,
        live_after,
    }
}

fn compute_live_in<AT, CI>(
    node: &TreeCfgNode<'_, TypeId>,
    live_out: &HashSet<DefId>,
    add_terminator_uses: &AT,
    collect_item_defs_uses: &CI,
) -> HashSet<DefId>
where
    AT: Fn(&TreeCfgTerminator<'_, TypeId>, &mut HashSet<DefId>),
    CI: Fn(&TreeCfgItem<'_, TypeId>, &mut HashSet<DefId>, &mut HashSet<DefId>),
{
    let mut live = live_out.clone();
    add_terminator_uses(&node.term, &mut live);
    for item in node.items.iter().rev() {
        apply_item_defs_uses(item, &mut live, collect_item_defs_uses);
    }
    live
}

fn compute_live_after<AT, CI>(
    node: &TreeCfgNode<'_, TypeId>,
    live_out: &HashSet<DefId>,
    add_terminator_uses: &AT,
    collect_item_defs_uses: &CI,
) -> Vec<HashSet<DefId>>
where
    AT: Fn(&TreeCfgTerminator<'_, TypeId>, &mut HashSet<DefId>),
    CI: Fn(&TreeCfgItem<'_, TypeId>, &mut HashSet<DefId>, &mut HashSet<DefId>),
{
    let mut live = live_out.clone();
    add_terminator_uses(&node.term, &mut live);
    let mut live_after = vec![HashSet::new(); node.items.len()];
    for (idx, item) in node.items.iter().enumerate().rev() {
        live_after[idx] = live.clone();
        apply_item_defs_uses(item, &mut live, collect_item_defs_uses);
    }
    live_after
}

fn apply_item_defs_uses<CI>(
    item: &TreeCfgItem<'_, TypeId>,
    live: &mut HashSet<DefId>,
    collect_item_defs_uses: &CI,
) where
    CI: Fn(&TreeCfgItem<'_, TypeId>, &mut HashSet<DefId>, &mut HashSet<DefId>),
{
    let mut defs = HashSet::new();
    let mut uses = HashSet::new();
    collect_item_defs_uses(item, &mut defs, &mut uses);

    for def in defs {
        live.remove(&def);
    }
    live.extend(uses);
}
