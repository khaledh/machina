use std::collections::{HashMap, HashSet};

use crate::mcir::types::{
    BasicBlock, BlockId, Body, LocalId, Operand, Place, PlaceAny, Projection, Rvalue, Statement,
    Terminator,
};

// --- Def/use extraction ---

fn collect_operand_uses(op: &Operand, out: &mut HashSet<LocalId>) {
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

fn stmt_defs(stmt: &Statement, out: &mut HashSet<LocalId>) {
    match stmt {
        Statement::AssignScalar { dst, .. } => {
            out.insert(dst.base());
        }
        Statement::InitAggregate { dst, .. } => {
            out.insert(dst.base());
        }
        Statement::CopyAggregate { dst, .. } => {
            out.insert(dst.base());
        }
        Statement::Call { dst, .. } => {
            out.insert(match dst {
                PlaceAny::Scalar(p) => p.base(),
                PlaceAny::Aggregate(p) => p.base(),
            });
        }
    }
}

fn stmt_uses(stmt: &Statement, out: &mut HashSet<LocalId>) {
    match stmt {
        Statement::AssignScalar { dst, src } => {
            collect_place_lhs_uses(dst, out);
            collect_rvalue_uses(src, out);
        }
        Statement::InitAggregate { dst, fields } => {
            collect_place_lhs_uses(dst, out);
            for field in fields {
                collect_operand_uses(field, out);
            }
        }
        Statement::CopyAggregate { dst, src } => {
            collect_place_lhs_uses(dst, out);
            collect_place_uses(src, out);
        }
        Statement::Call { dst, args, .. } => {
            collect_place_any_lhs_uses(dst, out);
            for arg in args {
                collect_place_any_uses(arg, out);
            }
        }
    }
}

// -- Gen/Kill sets computation ---

pub struct GenKillSet {
    pub gen_set: HashSet<LocalId>,
    pub kill_set: HashSet<LocalId>,
}

pub fn gen_kill_for_block(block: &BasicBlock) -> GenKillSet {
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
        Terminator::Return | Terminator::Goto(_) | Terminator::Unterminated => {}
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

fn compute_succs(body: &Body) -> Vec<Vec<BlockId>> {
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
            Terminator::Return | Terminator::Unterminated => {}
        }
    }
    succs
}

pub struct LivenessAnalysis<'a> {
    pub body: &'a Body,
}

impl<'a> LivenessAnalysis<'a> {
    pub fn new(body: &'a Body) -> Self {
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

// -- Live intervals computation ---

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LiveInterval {
    pub start: u32,
    pub end: u32,
}

pub type LiveIntervalMap = HashMap<LocalId, LiveInterval>;

pub(crate) fn build_live_intervals(body: &Body, live_map: &LiveMap) -> LiveIntervalMap {
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

    map
}
