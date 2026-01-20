//! Liveness analysis for MCIR.
//!
//! This stage runs after MCIR optimization and before register allocation.

use std::collections::{HashMap, HashSet};

use crate::analysis::dataflow::solve_backward;
use crate::context::{LivenessContext, OptimizedMcirContext};
use crate::mcir::cfg::McirCfg;
use crate::mcir::types::{
    BasicBlock, Callee, FuncBody, LocalId, Operand, Place, PlaceAny, Projection, Rvalue, Statement,
    Terminator, TyKind,
};

/// Run liveness analysis for an optimized MCIR context.
pub fn analyze(ctx: OptimizedMcirContext) -> LivenessContext {
    let mut live_maps = Vec::new();
    for body in &ctx.funcs {
        live_maps.push(LivenessAnalysis::new(&body.body).analyze());
    }
    ctx.with_liveness(live_maps)
}

// --- Def/use extraction ---

pub(crate) type AliasMap = HashMap<LocalId, HashSet<LocalId>>;

pub(crate) fn collect_operand_uses(op: &Operand, out: &mut HashSet<LocalId>, alias_map: &AliasMap) {
    match op {
        Operand::Copy(p) | Operand::Move(p) => collect_place_uses(p, out, alias_map),
        Operand::Const(_) => {}
    }
}

fn collect_place_uses<K>(place: &Place<K>, out: &mut HashSet<LocalId>, alias_map: &AliasMap) {
    insert_with_alias(place.base(), out, alias_map);
    for proj in place.projections() {
        if let Projection::Index { index } = proj {
            collect_operand_uses(index, out, alias_map);
        }
    }
}

// For LHS: only use base if there are projections.
fn collect_place_lhs_uses<K>(place: &Place<K>, out: &mut HashSet<LocalId>, alias_map: &AliasMap) {
    if !place.projections().is_empty() {
        insert_with_alias(place.base(), out, alias_map);
    }
    for proj in place.projections() {
        if let Projection::Index { index } = proj {
            collect_operand_uses(index, out, alias_map);
        }
    }
}

fn collect_place_any_uses(place: &PlaceAny, out: &mut HashSet<LocalId>, alias_map: &AliasMap) {
    match place {
        PlaceAny::Scalar(p) => collect_place_uses(p, out, alias_map),
        PlaceAny::Aggregate(p) => collect_place_uses(p, out, alias_map),
    }
}

fn collect_place_any_lhs_uses(place: &PlaceAny, out: &mut HashSet<LocalId>, alias_map: &AliasMap) {
    match place {
        PlaceAny::Scalar(p) => collect_place_lhs_uses(p, out, alias_map),
        PlaceAny::Aggregate(p) => collect_place_lhs_uses(p, out, alias_map),
    }
}

fn collect_rvalue_uses(rv: &Rvalue, out: &mut HashSet<LocalId>, alias_map: &AliasMap) {
    match rv {
        Rvalue::Use(op) => collect_operand_uses(op, out, alias_map),
        Rvalue::BinOp { lhs, rhs, .. } => {
            collect_operand_uses(lhs, out, alias_map);
            collect_operand_uses(rhs, out, alias_map);
        }
        Rvalue::UnOp { arg, .. } => collect_operand_uses(arg, out, alias_map),
        Rvalue::AddrOf(place) => collect_place_any_uses(place, out, alias_map),
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
        Statement::MemSet { dst, .. } => {
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

pub(crate) fn stmt_uses(stmt: &Statement, out: &mut HashSet<LocalId>, alias_map: &AliasMap) {
    match stmt {
        Statement::Comment(_) => {}
        Statement::CopyScalar { dst, src } => {
            collect_place_lhs_uses(dst, out, alias_map);
            collect_rvalue_uses(src, out, alias_map);
        }
        Statement::CopyAggregate { dst, src } => {
            collect_place_lhs_uses(dst, out, alias_map);
            collect_place_uses(src, out, alias_map);
        }
        Statement::MemSet { dst, value, .. } => {
            collect_place_lhs_uses(dst, out, alias_map);
            collect_operand_uses(value, out, alias_map);
        }
        Statement::Call { dst, callee, args } => {
            if let Some(dst) = dst {
                collect_place_any_lhs_uses(dst, out, alias_map);
            }
            if let Callee::Value(operand) = callee {
                collect_operand_uses(operand, out, alias_map);
            }
            for arg in args {
                collect_place_any_uses(arg, out, alias_map);
            }
        }
    }
}

// -- Gen/Kill sets computation ---

struct GenKillSet {
    gen_set: HashSet<LocalId>,
    kill_set: HashSet<LocalId>,
}

fn gen_kill_for_block(block: &BasicBlock, body: &FuncBody, alias_map: &AliasMap) -> GenKillSet {
    let mut gen_set = HashSet::new();
    let mut kill_set = HashSet::new();

    for stmt in &block.stmts {
        let mut uses = HashSet::new();
        stmt_uses(stmt, &mut uses, alias_map);
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
            collect_operand_uses(cond, &mut uses, alias_map);
            for u in uses {
                if !kill_set.contains(&u) {
                    gen_set.insert(u);
                }
            }
        }
        Terminator::Switch { discr, .. } => {
            let mut uses = HashSet::new();
            collect_operand_uses(discr, &mut uses, alias_map);
            for u in uses {
                if !kill_set.contains(&u) {
                    gen_set.insert(u);
                }
            }
        }
        Terminator::Return => {
            // Return has an implicit use of the return local; keep scalar returns live.
            let ret_local = body.ret_local;
            let ret_ty = body.locals[ret_local.index()].ty;
            let ret_kind = body.types.kind(ret_ty);
            if body.types.get(ret_ty).is_scalar()
                && !matches!(ret_kind, TyKind::Unit)
                && !kill_set.contains(&ret_local)
            {
                gen_set.insert(ret_local);
            }
        }

        Terminator::Goto(_) | Terminator::Unreachable | Terminator::Unterminated => {}
    }

    GenKillSet { gen_set, kill_set }
}

// -- Liveness analysis ---

#[derive(Debug, Clone)]
pub struct LiveSet {
    pub live_in: HashSet<LocalId>,
    pub live_out: HashSet<LocalId>,
}

impl Default for LiveSet {
    fn default() -> Self {
        Self::new()
    }
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

pub struct LivenessAnalysis<'a> {
    pub body: &'a FuncBody,
    alias_map: AliasMap,
}

impl<'a> LivenessAnalysis<'a> {
    pub fn new(body: &'a FuncBody) -> Self {
        Self {
            body,
            alias_map: build_alias_map(body),
        }
    }

    pub fn analyze(&self) -> LiveMap {
        let mut gen_kill = Vec::with_capacity(self.body.blocks.len());
        for block in &self.body.blocks {
            gen_kill.push(gen_kill_for_block(block, self.body, &self.alias_map));
        }

        let cfg = McirCfg::new(self.body);

        let empty = HashSet::new();
        let result = solve_backward(
            &cfg,
            empty.clone(), // entry_state for exits: live_out = {}
            empty,         // bottom = {}
            |states| {
                let mut out = HashSet::new();
                for s in states {
                    out.extend(s.iter().cloned());
                }
                out
            },
            |block_id, out_state| {
                let idx = block_id.index();
                let gen_set = &gen_kill[idx].gen_set;
                let kill_set = &gen_kill[idx].kill_set;

                let diff = out_state.difference(kill_set).cloned().collect();
                gen_set.union(&diff).cloned().collect()
            },
        );

        result
            .in_map
            .into_iter()
            .zip(result.out_map)
            .map(|(live_in, live_out)| LiveSet { live_in, live_out })
            .collect()
    }
}

fn insert_with_alias(local: LocalId, out: &mut HashSet<LocalId>, alias_map: &AliasMap) {
    out.insert(local);
    if let Some(bases) = alias_map.get(&local) {
        out.extend(bases.iter().copied());
    }
}

pub(crate) fn build_alias_map(body: &FuncBody) -> AliasMap {
    let mut alias_map: AliasMap = HashMap::new();
    let mut changed = true;
    while changed {
        changed = false;
        for block in &body.blocks {
            for stmt in &block.stmts {
                let Statement::CopyScalar { dst, src } = stmt else {
                    continue;
                };
                let dst_local = dst.base();
                match src {
                    Rvalue::AddrOf(place) => {
                        let base = match place {
                            PlaceAny::Scalar(p) => p.base(),
                            PlaceAny::Aggregate(p) => p.base(),
                        };
                        changed |= insert_alias(&mut alias_map, dst_local, base);
                    }
                    Rvalue::Use(Operand::Copy(place)) | Rvalue::Use(Operand::Move(place)) => {
                        let src_local = place.base();
                        if let Some(bases) = alias_map.get(&src_local).cloned() {
                            for base in bases {
                                changed |= insert_alias(&mut alias_map, dst_local, base);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    alias_map
}

fn insert_alias(alias_map: &mut AliasMap, dst: LocalId, base: LocalId) -> bool {
    let entry = alias_map.entry(dst).or_default();
    entry.insert(base)
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

#[cfg(test)]
#[path = "../tests/liveness/t_liveness.rs"]
mod tests;
