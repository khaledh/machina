//! Flow-sensitive tracking of which locals are bound to captured closures.
//!
//! This pass builds a map from local bindings to the captured bases of the
//! closure values currently stored in them. The result is used to determine
//! which captures are "active" at each program point.
use std::collections::HashMap;

use crate::core::analysis::dataflow::solve_forward;
use crate::core::resolve::DefId;
use crate::core::semck::closure::capture::{CaptureMode, ClosureCapture};
use crate::core::tree::cfg::{AstBlockId, TreeCfg, TreeCfgItem, TreeCfgNode};
use crate::core::tree::normalized::{BindPatternKind, Expr, ExprKind, StmtExprKind};
use crate::core::types::TypeId;

use super::{CaptureMap, ClosureBindings};

pub(super) struct ClosureBindingAnalysis {
    pub(super) in_map: Vec<ClosureBindings>,
}

pub(super) fn build_capture_map(
    captures: &HashMap<DefId, Vec<ClosureCapture>>,
) -> HashMap<DefId, CaptureMap> {
    let mut map = HashMap::new();
    for (closure_def, list) in captures {
        if list.is_empty() {
            continue;
        }
        // Store per-closure capture modes keyed by the captured base def.
        let mut capture_modes = HashMap::new();
        for capture in list {
            capture_modes.insert(capture.def_id, capture.mode);
        }
        map.insert(*closure_def, capture_modes);
    }
    map
}

pub(super) fn analyze_closure_bindings(
    cfg: &TreeCfg<'_, TypeId>,
    capture_map: &HashMap<DefId, CaptureMap>,
) -> ClosureBindingAnalysis {
    let entry_state = HashMap::new();
    let bottom = HashMap::new();

    // Flow-sensitive map: local -> captured bases for closures assigned so far.
    let analysis = solve_forward(
        cfg,
        AstBlockId(0),
        entry_state,
        bottom,
        merge_closure_bindings,
        |block_id, in_state| apply_block_bindings(&cfg.nodes[block_id.0], in_state, capture_map),
    );

    ClosureBindingAnalysis {
        in_map: analysis.in_map,
    }
}

fn merge_closure_bindings(states: &[ClosureBindings]) -> ClosureBindings {
    let mut merged = HashMap::new();
    for state in states {
        for (closure_def, captures) in state {
            let entry = merged.entry(*closure_def).or_insert_with(HashMap::new);
            for (base_def, mode) in captures {
                entry
                    .entry(*base_def)
                    .and_modify(|cap_mode| {
                        *cap_mode = merge_mode(*cap_mode, *mode);
                    })
                    .or_insert(*mode);
            }
        }
    }
    merged
}

fn merge_mode(current: CaptureMode, incoming: CaptureMode) -> CaptureMode {
    use CaptureMode::*;
    // Mut-borrow is most restrictive, then imm-borrow; move captures don't
    // impose borrow conflicts, so they only win if both sides are move.
    match (current, incoming) {
        (MutBorrow, _) | (_, MutBorrow) => MutBorrow,
        (ImmBorrow, _) | (_, ImmBorrow) => ImmBorrow,
        (Move, Move) => Move,
    }
}

fn apply_block_bindings(
    node: &TreeCfgNode<'_, TypeId>,
    in_state: &ClosureBindings,
    capture_map: &HashMap<DefId, CaptureMap>,
) -> ClosureBindings {
    let mut state = in_state.clone();
    for item in &node.items {
        apply_item_bindings(&mut state, item, capture_map);
    }
    state
}

pub(super) fn apply_item_bindings(
    state: &mut ClosureBindings,
    item: &TreeCfgItem<'_, TypeId>,
    capture_map: &HashMap<DefId, CaptureMap>,
) {
    let TreeCfgItem::Stmt(stmt) = item else {
        return;
    };

    // Only assignments/bindings affect which locals hold captured closures.
    match &stmt.kind {
        StmtExprKind::LetBind { pattern, value, .. }
        | StmtExprKind::VarBind { pattern, value, .. } => {
            if let BindPatternKind::Name { def_id, .. } = &pattern.kind {
                if let Some(captures) = closure_value_captures(value, state, capture_map) {
                    state.insert(*def_id, captures);
                } else {
                    state.remove(def_id);
                }
            }
        }
        StmtExprKind::Assign {
            assignee, value, ..
        } => {
            if let ExprKind::Var { def_id, .. } = assignee.kind {
                if let Some(captures) = closure_value_captures(value, state, capture_map) {
                    state.insert(def_id, captures);
                } else {
                    state.remove(&def_id);
                }
            }
        }
        _ => {}
    }
}

fn closure_value_captures(
    expr: &Expr,
    state: &ClosureBindings,
    capture_map: &HashMap<DefId, CaptureMap>,
) -> Option<CaptureMap> {
    match &expr.kind {
        ExprKind::Closure { def_id, .. } => capture_map.get(def_id).cloned(),
        ExprKind::Var { def_id, .. } => state.get(def_id).cloned(),
        ExprKind::Move { expr }
        | ExprKind::ImplicitMove { expr }
        | ExprKind::Coerce { expr, .. } => closure_value_captures(expr, state, capture_map),
        _ => None,
    }
}
