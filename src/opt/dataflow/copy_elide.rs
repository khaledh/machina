use std::collections::{HashMap, HashSet};

use crate::analysis::dataflow::{DataflowResult, solve_forward};
use crate::context::{LivenessContext, OptimizedMcirContext};
use crate::liveness::{LiveMap, collect_operand_uses, stmt_defs, stmt_uses};
use crate::mcir::cfg::McirCfg;
use crate::mcir::types::{
    BasicBlock, FuncBody, LocalId, Operand, Place, PlaceAny, Rvalue, Statement, Terminator,
};

/// Elide aggregate copies when the source is dead and we can safely reuse its storage.
///
/// This is a CFG-aware pass:
/// - We consider full aggregate copies (no projections).
/// - We require the source to be dead immediately after the copy.
/// - We avoid locals whose address is taken (e.g., slices / aggregate call args).
/// - Renames are propagated across the CFG using a must-available dataflow.
pub fn elide_last_use_copies(ctx: LivenessContext) -> OptimizedMcirContext {
    let LivenessContext {
        mut func_bodies,
        live_maps,
        symbols,
        globals,
    } = ctx;

    for (body, live_map) in func_bodies.iter_mut().zip(live_maps.iter()) {
        elide_in_body(body, live_map);
    }

    OptimizedMcirContext {
        func_bodies,
        symbols,
        globals,
    }
}

fn elide_in_body(body: &mut FuncBody, live_map: &LiveMap) {
    // If a local's address escapes, we conservatively avoid eliding copies
    // that would change its identity.
    let addr_taken = collect_addr_taken(body);

    // Per-statement live-after info drives last-use checks.
    let live_after_by_block = compute_live_after_by_block(body, live_map);
    // Compute rename availability across the CFG (must-available on all paths).
    let analysis = analyze_renames(body, &live_after_by_block, &addr_taken);

    let mut used_locals = HashSet::new();
    let mut rewritten_blocks = Vec::with_capacity(body.blocks.len());
    let mut elide_flags = Vec::with_capacity(body.blocks.len());

    for (block_idx, block) in body.blocks.iter().enumerate() {
        let live_after = &live_after_by_block[block_idx];
        // Apply in-map renames while tracking which locals are actually used.
        let (rewritten, flags) = rewrite_block(
            block,
            &analysis.in_map[block_idx],
            live_after,
            &addr_taken,
            &mut used_locals,
        );
        rewritten_blocks.push(rewritten);
        elide_flags.push(flags);
    }

    // Drop elided copies whose destination is now unused after rewriting.
    for (block_idx, block) in rewritten_blocks.iter_mut().enumerate() {
        let mut new_stmts = Vec::with_capacity(block.stmts.len());
        for (idx, stmt) in block.stmts.drain(..).enumerate() {
            if let Some(dst) = elide_flags[block_idx][idx]
                && !used_locals.contains(&dst)
            {
                continue;
            }
            new_stmts.push(stmt);
        }
        block.stmts = new_stmts;
    }

    body.blocks = rewritten_blocks;
}

fn compute_live_after(block: &BasicBlock, live_out: &HashSet<LocalId>) -> Vec<HashSet<LocalId>> {
    // Standard backward dataflow over a single block to compute live-after sets.
    let mut live = live_out.clone();
    let mut live_after = vec![HashSet::new(); block.stmts.len()];

    for (idx, stmt) in block.stmts.iter().enumerate().rev() {
        live_after[idx] = live.clone();

        let mut defs = HashSet::new();
        stmt_defs(stmt, &mut defs);

        let mut uses = HashSet::new();
        stmt_uses(stmt, &mut uses);

        for def in defs {
            live.remove(&def);
        }
        live.extend(uses);
    }

    live_after
}

fn compute_live_after_by_block(body: &FuncBody, live_map: &LiveMap) -> Vec<Vec<HashSet<LocalId>>> {
    body.blocks
        .iter()
        .enumerate()
        .map(|(idx, block)| compute_live_after(block, &live_map[idx].live_out))
        .collect()
}

fn analyze_renames(
    body: &FuncBody,
    live_after_by_block: &[Vec<HashSet<LocalId>>],
    addr_taken: &HashSet<LocalId>,
) -> DataflowResult<RenameMap> {
    // Forward must-analysis: a rename is available only if all preds agree.
    let cfg = McirCfg::new(body);

    solve_forward(
        &cfg,
        body.entry,
        RenameMap::new(), // entry state
        RenameMap::new(), // bottom
        meet_rename_maps,
        |block_id, in_map| {
            let block = &body.blocks[block_id.index()];
            transfer_block(
                block,
                in_map,
                &live_after_by_block[block_id.index()],
                addr_taken,
            )
        },
    )
}

fn is_elidable_copy(
    dst: &Place<crate::mcir::types::Aggregate>,
    src: &Place<crate::mcir::types::Aggregate>,
    live_after: &HashSet<LocalId>,
    addr_taken: &HashSet<LocalId>,
) -> bool {
    // Only full aggregate copies (no projections) are eligible.
    if !dst.projections().is_empty() || !src.projections().is_empty() {
        return false;
    }

    let dst_local = dst.base();
    let src_local = src.base();

    if dst_local == src_local {
        return false;
    }

    // Require last-use: source must be dead immediately after the copy.
    if live_after.contains(&src_local) {
        return false;
    }

    // Avoid address-taken locals to prevent aliasing bugs.
    if addr_taken.contains(&src_local) || addr_taken.contains(&dst_local) {
        return false;
    }

    true
}

type RenameMap = HashMap<LocalId, LocalId>;

fn meet_rename_maps(preds: &[RenameMap]) -> RenameMap {
    if preds.is_empty() {
        return RenameMap::new();
    }

    // Keep only renames that are identical across all predecessors.
    let mut out = preds[0].clone();
    out.retain(|dst, src| preds.iter().all(|m| m.get(dst) == Some(src)));
    out
}

fn transfer_block(
    block: &BasicBlock,
    in_map: &RenameMap,
    live_after: &[HashSet<LocalId>],
    addr_taken: &HashSet<LocalId>,
) -> RenameMap {
    let mut rename = in_map.clone();

    for (idx, stmt) in block.stmts.iter().enumerate() {
        // If a last-use copy is safe to elide, add a rename for later uses.
        if let Statement::CopyAggregate { dst, src } = stmt
            && is_elidable_copy(dst, src, &live_after[idx], addr_taken)
        {
            let dst_local = dst.base();
            let src_local = resolve_rename(&rename, src.base());
            clear_rename_for_def(&mut rename, dst_local);
            rename.insert(dst_local, src_local);
            continue;
        }

        // Any full definition kills the mapping for that local.
        if let Some(def_local) = def_local_full(stmt) {
            clear_rename_for_def(&mut rename, def_local);
        }
        // Any write to the base invalidates it as a rename source.
        if let Some(def_local) = def_local_base(stmt) {
            clear_rename_for_src(&mut rename, def_local);
        }
    }

    rename
}

fn resolve_rename(map: &HashMap<LocalId, LocalId>, mut local: LocalId) -> LocalId {
    // Follow rename chains (dst -> src -> src2) with cycle protection.
    let mut visited = HashSet::new();
    while let Some(next) = map.get(&local) {
        if *next == local || !visited.insert(local) {
            break;
        }
        local = *next;
    }
    local
}

fn clear_rename_for_def(rename: &mut HashMap<LocalId, LocalId>, def: LocalId) {
    // If we define a local, discard any rename for it or pointing to it.
    rename.remove(&def);
    rename.retain(|_, src| *src != def);
}

fn clear_rename_for_src(rename: &mut HashMap<LocalId, LocalId>, def: LocalId) {
    // If a local is written, it can no longer serve as a safe rename source.
    rename.retain(|_, src| *src != def);
}

fn def_local_full(stmt: &Statement) -> Option<LocalId> {
    // Only treat full-place definitions as kills for rename tracking.
    match stmt {
        Statement::Comment(_) => None,
        Statement::CopyScalar { dst, .. } => def_place_full(dst),
        Statement::CopyAggregate { dst, .. } => def_place_full(dst),
        Statement::MemSet { dst, .. } => def_place_full(dst),
        Statement::Call { dst, .. } => dst.as_ref().and_then(def_place_any_full),
    }
}

fn def_local_base(stmt: &Statement) -> Option<LocalId> {
    match stmt {
        Statement::Comment(_) => None,
        Statement::CopyScalar { dst, .. } => Some(dst.base()),
        Statement::CopyAggregate { dst, .. } => Some(dst.base()),
        Statement::MemSet { dst, .. } => Some(dst.base()),
        Statement::Call { dst, .. } => dst.as_ref().map(place_any_base),
    }
}

fn def_place_any_full(place: &PlaceAny) -> Option<LocalId> {
    match place {
        PlaceAny::Scalar(p) => def_place_full(p),
        PlaceAny::Aggregate(p) => def_place_full(p),
    }
}

fn def_place_full<K>(place: &Place<K>) -> Option<LocalId> {
    if place.projections().is_empty() {
        Some(place.base())
    } else {
        None
    }
}

fn place_any_base(place: &PlaceAny) -> LocalId {
    match place {
        PlaceAny::Scalar(p) => p.base(),
        PlaceAny::Aggregate(p) => p.base(),
    }
}

fn rewrite_block(
    block: &BasicBlock,
    in_map: &RenameMap,
    live_after: &[HashSet<LocalId>],
    addr_taken: &HashSet<LocalId>,
    used_locals: &mut HashSet<LocalId>,
) -> (BasicBlock, Vec<Option<LocalId>>) {
    let mut rename = in_map.clone();
    let mut new_stmts = Vec::with_capacity(block.stmts.len());
    let mut elide_flags = Vec::with_capacity(block.stmts.len());

    for (idx, stmt) in block.stmts.iter().enumerate() {
        // Rewrite uses before deciding whether we can elide this copy.
        let stmt = rewrite_stmt_uses(stmt, &rename);
        let mut elide_dst = None;

        if let Statement::CopyAggregate { dst, src } = &stmt
            && is_elidable_copy(dst, src, &live_after[idx], addr_taken)
        {
            let dst_local = dst.base();
            let src_local = resolve_rename(&rename, src.base());
            clear_rename_for_def(&mut rename, dst_local);
            rename.insert(dst_local, src_local);
            elide_dst = Some(dst_local);
        }

        if elide_dst.is_none()
            && let Some(def_local) = def_local_full(&stmt)
        {
            clear_rename_for_def(&mut rename, def_local);
        }

        if let Some(def_local) = def_local_base(&stmt) {
            clear_rename_for_src(&mut rename, def_local);
        }

        // Track which locals are still referenced after rewriting.
        stmt_uses(&stmt, used_locals);
        new_stmts.push(stmt);
        elide_flags.push(elide_dst);
    }

    let terminator = rewrite_terminator(&block.terminator, &rename);
    collect_terminator_uses(&terminator, used_locals);

    (
        BasicBlock {
            stmts: new_stmts,
            terminator,
        },
        elide_flags,
    )
}

fn rewrite_stmt_uses(stmt: &Statement, rename: &HashMap<LocalId, LocalId>) -> Statement {
    // Apply rename mapping to uses; defs are only rewritten when projected.
    match stmt {
        Statement::Comment(_) => stmt.clone(),
        Statement::CopyScalar { dst, src } => Statement::CopyScalar {
            dst: rewrite_place_for_def(dst, rename),
            src: rewrite_rvalue(src, rename),
        },
        Statement::CopyAggregate { dst, src } => Statement::CopyAggregate {
            dst: rewrite_place_for_def(dst, rename),
            src: rewrite_place_for_use(src, rename),
        },
        Statement::MemSet { dst, value, len } => Statement::MemSet {
            dst: rewrite_place_for_def(dst, rename),
            value: rewrite_operand(value, rename),
            len: *len,
        },
        Statement::Call { dst, callee, args } => Statement::Call {
            dst: dst.as_ref().map(|p| rewrite_place_any_for_def(p, rename)),
            callee: callee.clone(),
            args: args
                .iter()
                .map(|p| rewrite_place_any_for_use(p, rename))
                .collect(),
        },
    }
}

fn rewrite_terminator(terminator: &Terminator, rename: &HashMap<LocalId, LocalId>) -> Terminator {
    match terminator {
        Terminator::If {
            cond,
            then_bb,
            else_bb,
        } => Terminator::If {
            cond: rewrite_operand(cond, rename),
            then_bb: *then_bb,
            else_bb: *else_bb,
        },
        Terminator::Switch {
            discr,
            cases,
            default,
        } => Terminator::Switch {
            discr: rewrite_operand(discr, rename),
            cases: cases.clone(),
            default: *default,
        },
        Terminator::Return => Terminator::Return,
        Terminator::Goto(target) => Terminator::Goto(*target),
        Terminator::Unreachable => Terminator::Unreachable,
        Terminator::Unterminated => Terminator::Unterminated,
    }
}

fn rewrite_rvalue(rv: &Rvalue, rename: &HashMap<LocalId, LocalId>) -> Rvalue {
    match rv {
        Rvalue::Use(op) => Rvalue::Use(rewrite_operand(op, rename)),
        Rvalue::BinOp { op, lhs, rhs } => Rvalue::BinOp {
            op: *op,
            lhs: rewrite_operand(lhs, rename),
            rhs: rewrite_operand(rhs, rename),
        },
        Rvalue::UnOp { op, arg } => Rvalue::UnOp {
            op: *op,
            arg: rewrite_operand(arg, rename),
        },
        Rvalue::AddrOf(place) => Rvalue::AddrOf(rewrite_place_any_for_use(place, rename)),
    }
}

fn rewrite_operand(op: &Operand, rename: &HashMap<LocalId, LocalId>) -> Operand {
    match op {
        Operand::Copy(place) => Operand::Copy(rewrite_place_for_use(place, rename)),
        Operand::Move(place) => Operand::Move(rewrite_place_for_use(place, rename)),
        Operand::Const(value) => Operand::Const(value.clone()),
    }
}

fn rewrite_place_any_for_use(place: &PlaceAny, rename: &HashMap<LocalId, LocalId>) -> PlaceAny {
    match place {
        PlaceAny::Scalar(p) => PlaceAny::Scalar(rewrite_place_for_use(p, rename)),
        PlaceAny::Aggregate(p) => PlaceAny::Aggregate(rewrite_place_for_use(p, rename)),
    }
}

fn rewrite_place_any_for_def(place: &PlaceAny, rename: &HashMap<LocalId, LocalId>) -> PlaceAny {
    match place {
        PlaceAny::Scalar(p) => PlaceAny::Scalar(rewrite_place_for_def(p, rename)),
        PlaceAny::Aggregate(p) => PlaceAny::Aggregate(rewrite_place_for_def(p, rename)),
    }
}

fn rewrite_place_for_use<K>(place: &Place<K>, rename: &HashMap<LocalId, LocalId>) -> Place<K> {
    // Substitute base local while preserving projections and type.
    let base = resolve_rename(rename, place.base());
    Place::new(base, place.ty(), place.projections().to_vec())
}

fn rewrite_place_for_def<K>(place: &Place<K>, rename: &HashMap<LocalId, LocalId>) -> Place<K> {
    // Full definitions keep their base; projections still get use-side renaming.
    if place.projections().is_empty() {
        Place::new(place.base(), place.ty(), Vec::new())
    } else {
        rewrite_place_for_use(place, rename)
    }
}

fn collect_addr_taken(body: &FuncBody) -> HashSet<LocalId> {
    // Conservative: any addr_of or aggregate call arg counts as address-taken.
    let mut addr_taken = HashSet::new();

    for block in &body.blocks {
        for stmt in &block.stmts {
            match stmt {
                Statement::Comment(_) => {}
                Statement::CopyScalar {
                    src: Rvalue::AddrOf(place),
                    ..
                } => {
                    addr_taken.insert(place_base(place));
                }
                Statement::Call { args, .. } => {
                    for arg in args {
                        if let PlaceAny::Aggregate(place) = arg {
                            addr_taken.insert(place.base());
                        }
                    }
                }
                _ => {}
            }
        }
    }

    addr_taken
}

fn place_base(place: &PlaceAny) -> LocalId {
    match place {
        PlaceAny::Scalar(p) => p.base(),
        PlaceAny::Aggregate(p) => p.base(),
    }
}

fn collect_terminator_uses(terminator: &Terminator, used: &mut HashSet<LocalId>) {
    match terminator {
        Terminator::If { cond, .. } => collect_operand_uses(cond, used),
        Terminator::Switch { discr, .. } => collect_operand_uses(discr, used),
        Terminator::Return
        | Terminator::Goto(_)
        | Terminator::Unreachable
        | Terminator::Unterminated => {}
    }
}
