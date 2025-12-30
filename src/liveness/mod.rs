//! Liveness analysis for MCIR.
//!
//! This stage runs after MCIR optimization and before register allocation.

use std::collections::HashSet;

use crate::context::{LivenessContext, OptimizedMcirContext};
use crate::mcir::types::{
    BasicBlock, BlockId, FuncBody, LocalId, Operand, Place, PlaceAny, Projection, Rvalue,
    Statement, Terminator,
};

/// Run liveness analysis for an optimized MCIR context.
pub fn analyze(ctx: OptimizedMcirContext) -> LivenessContext {
    let mut live_maps = Vec::new();
    for body in &ctx.func_bodies {
        live_maps.push(LivenessAnalysis::new(body).analyze());
    }
    ctx.with_liveness(live_maps)
}

// --- Def/use extraction ---

pub(crate) fn collect_operand_uses(op: &Operand, out: &mut HashSet<LocalId>) {
    match op {
        Operand::Copy(p) | Operand::Move(p) => collect_place_uses(p, out),
        Operand::Const(_) => {}
    }
}

fn collect_place_uses<K>(place: &Place<K>, out: &mut HashSet<LocalId>) {
    out.insert(place.base());
    for proj in place.projections() {
        if let Projection::Index { index } = proj {
            collect_operand_uses(index, out);
        }
    }
}

// For LHS: only use base if there are projections.
fn collect_place_lhs_uses<K>(place: &Place<K>, out: &mut HashSet<LocalId>) {
    if !place.projections().is_empty() {
        out.insert(place.base());
    }
    for proj in place.projections() {
        if let Projection::Index { index } = proj {
            collect_operand_uses(index, out);
        }
    }
}

fn collect_place_any_uses(place: &PlaceAny, out: &mut HashSet<LocalId>) {
    match place {
        PlaceAny::Scalar(p) => collect_place_uses(p, out),
        PlaceAny::Aggregate(p) => collect_place_uses(p, out),
    }
}

fn collect_place_any_lhs_uses(place: &PlaceAny, out: &mut HashSet<LocalId>) {
    match place {
        PlaceAny::Scalar(p) => collect_place_lhs_uses(p, out),
        PlaceAny::Aggregate(p) => collect_place_lhs_uses(p, out),
    }
}

fn collect_rvalue_uses(rv: &Rvalue, out: &mut HashSet<LocalId>) {
    match rv {
        Rvalue::Use(op) => collect_operand_uses(op, out),
        Rvalue::BinOp { lhs, rhs, .. } => {
            collect_operand_uses(lhs, out);
            collect_operand_uses(rhs, out);
        }
        Rvalue::UnOp { arg, .. } => collect_operand_uses(arg, out),
        Rvalue::AddrOf(place) => collect_place_any_uses(place, out),
    }
}

pub(crate) fn stmt_defs(stmt: &Statement, out: &mut HashSet<LocalId>) {
    match stmt {
        Statement::Comment(_) => {}
        Statement::CopyScalar { dst, .. } => {
            out.insert(dst.base());
        }
        Statement::CopyAggregate { dst, .. } => {
            out.insert(dst.base());
        }
        Statement::Call { dst, .. } => {
            if let Some(dst) = dst {
                out.insert(match dst {
                    PlaceAny::Scalar(p) => p.base(),
                    PlaceAny::Aggregate(p) => p.base(),
                });
            }
        }
    }
}

pub(crate) fn stmt_uses(stmt: &Statement, out: &mut HashSet<LocalId>) {
    match stmt {
        Statement::Comment(_) => {}
        Statement::CopyScalar { dst, src } => {
            collect_place_lhs_uses(dst, out);
            collect_rvalue_uses(src, out);
        }
        Statement::CopyAggregate { dst, src } => {
            collect_place_lhs_uses(dst, out);
            collect_place_uses(src, out);
        }
        Statement::Call { dst, args, .. } => {
            if let Some(dst) = dst {
                collect_place_any_lhs_uses(dst, out);
            }
            for arg in args {
                collect_place_any_uses(arg, out);
            }
        }
    }
}

// -- Gen/Kill sets computation ---

struct GenKillSet {
    gen_set: HashSet<LocalId>,
    kill_set: HashSet<LocalId>,
}

fn gen_kill_for_block(block: &BasicBlock) -> GenKillSet {
    let mut gen_set = HashSet::new();
    let mut kill_set = HashSet::new();

    for stmt in &block.stmts {
        let mut uses = HashSet::new();
        stmt_uses(stmt, &mut uses);
        for u in uses {
            if !kill_set.contains(&u) {
                gen_set.insert(u);
            }
        }

        let mut defs = HashSet::new();
        stmt_defs(stmt, &mut defs);
        for d in defs {
            kill_set.insert(d);
        }
    }

    match &block.terminator {
        Terminator::If { cond, .. } => {
            let mut uses = HashSet::new();
            collect_operand_uses(cond, &mut uses);
            for u in uses {
                if !kill_set.contains(&u) {
                    gen_set.insert(u);
                }
            }
        }
        Terminator::Switch { discr, .. } => {
            let mut uses = HashSet::new();
            collect_operand_uses(discr, &mut uses);
            for u in uses {
                if !kill_set.contains(&u) {
                    gen_set.insert(u);
                }
            }
        }
        Terminator::Return
        | Terminator::Goto(_)
        | Terminator::Unreachable
        | Terminator::Unterminated => {}
    }

    GenKillSet { gen_set, kill_set }
}

// -- Liveness analysis ---

#[derive(Debug, Clone)]
pub struct LiveSet {
    pub live_in: HashSet<LocalId>,
    pub live_out: HashSet<LocalId>,
}

impl LiveSet {
    pub fn new() -> Self {
        Self {
            live_in: HashSet::new(),
            live_out: HashSet::new(),
        }
    }
}

pub type LiveMap = Vec<LiveSet>;

fn compute_succs(body: &FuncBody) -> Vec<Vec<BlockId>> {
    let mut succs: Vec<Vec<BlockId>> = vec![vec![]; body.blocks.len()];
    for (i, block) in body.blocks.iter().enumerate() {
        let v = &mut succs[i];
        match &block.terminator {
            Terminator::Goto(target) => v.push(*target),
            Terminator::If {
                then_bb, else_bb, ..
            } => {
                v.push(*then_bb);
                v.push(*else_bb);
            }
            Terminator::Switch { cases, default, .. } => {
                for case in cases {
                    v.push(case.target);
                }
                v.push(*default);
            }
            Terminator::Return | Terminator::Unreachable | Terminator::Unterminated => {}
        }
    }
    succs
}

pub struct LivenessAnalysis<'a> {
    pub body: &'a FuncBody,
}

impl<'a> LivenessAnalysis<'a> {
    pub fn new(body: &'a FuncBody) -> Self {
        Self { body }
    }

    pub fn analyze(&self) -> LiveMap {
        let mut gen_kill = Vec::with_capacity(self.body.blocks.len());
        for block in &self.body.blocks {
            gen_kill.push(gen_kill_for_block(block));
        }

        let mut live_map = vec![LiveSet::new(); self.body.blocks.len()];
        let succs = compute_succs(self.body);

        let mut changed = true;
        while changed {
            changed = false;
            for i in 0..self.body.blocks.len() {
                let mut new_live_out = HashSet::new();
                for succ in &succs[i] {
                    new_live_out.extend(live_map[succ.index()].live_in.iter().cloned());
                }

                if live_map[i].live_out != new_live_out {
                    live_map[i].live_out = new_live_out;
                    changed = true;
                }

                let gen_set = &gen_kill[i].gen_set;
                let kill_set = &gen_kill[i].kill_set;
                let diff = live_map[i].live_out.difference(kill_set).cloned().collect();
                let new_live_in = gen_set.union(&diff).cloned().collect();

                if live_map[i].live_in != new_live_in {
                    live_map[i].live_in = new_live_in;
                    changed = true;
                }
            }
        }

        live_map
    }
}

/// Format a liveness map for human-readable output.
pub fn format_liveness_map(live_map: &LiveMap, func_name: &str) -> String {
    let mut out = String::new();
    out.push_str(&format!("Live Map ({}):\n", func_name));
    out.push_str("--------------------------------\n");
    for (bb_idx, live) in live_map.iter().enumerate() {
        out.push_str(&format!("  bb{}:\n", bb_idx));
        out.push_str("    live_in: ");
        out.push_str(&format_live_set(&live.live_in));
        out.push('\n');
        out.push_str("    live_out: ");
        out.push_str(&format_live_set(&live.live_out));
        out.push('\n');
    }
    out.push_str("--------------------------------\n");
    out
}

#[cfg(test)]
#[path = "../tests/t_liveness.rs"]
mod tests;

fn format_live_set(set: &HashSet<LocalId>) -> String {
    let mut ids: Vec<_> = set.iter().map(|l| l.0).collect();
    ids.sort();
    let mut out = String::new();
    out.push('[');
    for (idx, id) in ids.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(&format!("%t{}", id));
    }
    out.push(']');
    out
}
