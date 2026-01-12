use std::collections::{HashMap, HashSet};

use crate::analysis::dataflow::solve_backward;
use crate::ast::cfg::{HirCfg, HirCfgNode, HirItem, HirTerminator};
use crate::ast::visit::{Visitor, walk_expr};
use crate::context::ElaboratedContext;
use crate::resolve::DefId;
use crate::sir::model::{BindPattern, BindPatternKind, Expr, ExprKind, StmtExpr, StmtExprKind};
use crate::types::TypeId;

// ============================================================================
// Public API
// ============================================================================

pub struct AstLiveness {
    #[allow(dead_code)]
    pub live_in: Vec<HashSet<DefId>>,
    #[allow(dead_code)]
    pub live_out: Vec<HashSet<DefId>>,
    pub live_after: Vec<Vec<HashSet<DefId>>>,
}

/// Compute liveness for heap-owned locals on the AST CFG. This keeps the analysis
/// lightweight and scoped to the move-checker (we only track heap uses).
pub fn analyze(cfg: &HirCfg<'_, TypeId>, ctx: &ElaboratedContext) -> AstLiveness {
    let entry = HashSet::new();
    let bottom = HashSet::new();

    // Backward analysis: live-in = uses ∪ (live-out − defs).
    let analysis = solve_backward(
        cfg,
        entry,
        bottom,
        |states| {
            // May-liveness: a value is live if any successor needs it.
            let mut out = HashSet::new();
            for state in states {
                out.extend(state.iter().cloned());
            }
            out
        },
        |block_id, out_state| compute_live_in(ctx, &cfg.nodes[block_id.0], out_state),
    );

    let live_in = analysis.in_map;
    let live_out = analysis.out_map;
    let live_after = cfg
        .nodes
        .iter()
        .enumerate()
        .map(|(idx, node)| compute_live_after(ctx, node, &live_out[idx]))
        .collect();

    AstLiveness {
        live_in,
        live_out,
        live_after,
    }
}

/// Count heap-owned variable uses within one AST item. This lets move_check
/// avoid implicit moves when a single item uses the same binding multiple times.
pub(crate) fn heap_use_counts_for_item(
    item: &HirItem<'_, TypeId>,
    ctx: &ElaboratedContext,
) -> HashMap<DefId, usize> {
    let mut counts = HashMap::new();
    match item {
        HirItem::Stmt(stmt) => collect_stmt_uses(stmt, ctx, &mut counts),
        HirItem::Expr(expr) => collect_expr_uses(expr, ctx, &mut counts),
    }
    counts
}

// ============================================================================
// Heap Use Accumulator Trait
// ============================================================================

trait HeapUseAccumulator {
    fn record(&mut self, def_id: DefId);
}

impl HeapUseAccumulator for HashSet<DefId> {
    fn record(&mut self, def_id: DefId) {
        self.insert(def_id);
    }
}

impl HeapUseAccumulator for HashMap<DefId, usize> {
    fn record(&mut self, def_id: DefId) {
        *self.entry(def_id).or_insert(0) += 1;
    }
}

// ============================================================================
// Dataflow Helpers
// ============================================================================

fn compute_live_in(
    ctx: &ElaboratedContext,
    node: &HirCfgNode<'_, TypeId>,
    live_out: &HashSet<DefId>,
) -> HashSet<DefId> {
    let mut live = live_out.clone();
    // Terminator conditions (if) are evaluated after items in the block.
    add_terminator_uses(&node.term, ctx, &mut live);
    for item in node.items.iter().rev() {
        apply_item_defs_uses(item, ctx, &mut live);
    }
    live
}

/// Per-item live-after sets are used to detect last-use sites inside a block.
fn compute_live_after(
    ctx: &ElaboratedContext,
    node: &HirCfgNode<'_, TypeId>,
    live_out: &HashSet<DefId>,
) -> Vec<HashSet<DefId>> {
    let mut live = live_out.clone();
    add_terminator_uses(&node.term, ctx, &mut live);
    let mut live_after = vec![HashSet::new(); node.items.len()];
    for (idx, item) in node.items.iter().enumerate().rev() {
        live_after[idx] = live.clone();
        apply_item_defs_uses(item, ctx, &mut live);
    }
    live_after
}

fn apply_item_defs_uses(
    item: &HirItem<'_, TypeId>,
    ctx: &ElaboratedContext,
    live: &mut HashSet<DefId>,
) {
    let mut defs = HashSet::new();
    let mut uses = HashSet::new();
    collect_item_defs_uses(item, ctx, &mut defs, &mut uses);

    // Standard liveness transfer: kill defs, then add uses.
    for def in defs {
        live.remove(&def);
    }
    live.extend(uses);
}

fn add_terminator_uses(
    term: &HirTerminator<'_, TypeId>,
    ctx: &ElaboratedContext,
    uses: &mut HashSet<DefId>,
) {
    if let HirTerminator::If { cond, .. } = term {
        collect_expr_uses(cond, ctx, uses);
    }
}

// ============================================================================
// Def/Use Collection
// ============================================================================

fn collect_item_defs_uses(
    item: &HirItem<'_, TypeId>,
    ctx: &ElaboratedContext,
    defs: &mut HashSet<DefId>,
    uses: &mut HashSet<DefId>,
) {
    match item {
        HirItem::Stmt(stmt) => collect_stmt_defs_uses(stmt, ctx, defs, uses),
        HirItem::Expr(expr) => collect_expr_uses(expr, ctx, uses),
    }
}

fn collect_stmt_defs_uses(
    stmt: &StmtExpr,
    ctx: &ElaboratedContext,
    defs: &mut HashSet<DefId>,
    uses: &mut HashSet<DefId>,
) {
    // Collect uses via the generic helper.
    collect_stmt_uses(stmt, ctx, uses);
    // Collect defs separately (only relevant for bindings and assignments).
    match &stmt.kind {
        StmtExprKind::LetBind { pattern, .. } | StmtExprKind::VarBind { pattern, .. } => {
            collect_pattern_defs(pattern, ctx, defs);
        }
        StmtExprKind::VarDecl { .. } => {}
        StmtExprKind::Assign { assignee, .. } => {
            collect_assignee_defs(assignee, ctx, defs);
        }
        StmtExprKind::While { .. } | StmtExprKind::For { .. } => {}
    }
}

fn collect_pattern_defs(pattern: &BindPattern, ctx: &ElaboratedContext, defs: &mut HashSet<DefId>) {
    match &pattern.kind {
        BindPatternKind::Name { def_id, .. } => add_def_if_heap(*def_id, ctx, defs),
        BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
            for pattern in patterns {
                collect_pattern_defs(pattern, ctx, defs);
            }
        }
        BindPatternKind::Struct { fields, .. } => {
            for field in fields {
                collect_pattern_defs(&field.pattern, ctx, defs);
            }
        }
    }
}

fn collect_assignee_defs(assignee: &Expr, ctx: &ElaboratedContext, defs: &mut HashSet<DefId>) {
    // Only a plain variable counts as a definition; projections are treated as uses.
    if let ExprKind::Var { def_id, .. } = assignee.kind {
        add_def_if_heap(def_id, ctx, defs);
    }
}

/// Only treat heap-owned locals as tracked defs.
fn add_def_if_heap(def_id: DefId, ctx: &ElaboratedContext, defs: &mut HashSet<DefId>) {
    let Some(def) = ctx.def_table.lookup_def(def_id) else {
        return;
    };
    let Some(ty) = ctx.type_map.lookup_def_type(def) else {
        return;
    };
    if ty.is_heap() {
        defs.insert(def_id);
    }
}

// ============================================================================
// Generic Use Collection
// ============================================================================

fn collect_stmt_uses<A: HeapUseAccumulator>(stmt: &StmtExpr, ctx: &ElaboratedContext, acc: &mut A) {
    match &stmt.kind {
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            collect_expr_uses(value, ctx, acc);
        }
        StmtExprKind::VarDecl { .. } => {}
        StmtExprKind::Assign { assignee, value } => {
            collect_expr_uses(value, ctx, acc);
            collect_assignee_uses(assignee, ctx, acc);
        }
        StmtExprKind::While { cond, body } => {
            collect_expr_uses(cond, ctx, acc);
            collect_expr_uses(body, ctx, acc);
        }
        StmtExprKind::For { iter, body, .. } => {
            collect_expr_uses(iter, ctx, acc);
            collect_expr_uses(body, ctx, acc);
        }
    }
}

fn collect_assignee_uses<A: HeapUseAccumulator>(
    assignee: &Expr,
    ctx: &ElaboratedContext,
    acc: &mut A,
) {
    match &assignee.kind {
        ExprKind::Var { .. } => {}
        ExprKind::StructField { target, .. } | ExprKind::TupleField { target, .. } => {
            collect_expr_uses(target, ctx, acc);
        }
        ExprKind::ArrayIndex { target, indices } => {
            collect_expr_uses(target, ctx, acc);
            for index in indices {
                collect_expr_uses(index, ctx, acc);
            }
        }
        ExprKind::Slice { target, start, end } => {
            collect_expr_uses(target, ctx, acc);
            if let Some(start) = start {
                collect_expr_uses(start, ctx, acc);
            }
            if let Some(end) = end {
                collect_expr_uses(end, ctx, acc);
            }
        }
        _ => collect_expr_uses(assignee, ctx, acc),
    }
}

fn collect_expr_uses<A: HeapUseAccumulator>(expr: &Expr, ctx: &ElaboratedContext, acc: &mut A) {
    let mut collector = HeapUseCollector { ctx, acc };
    collector.visit_expr(expr);
}

// ============================================================================
// Generic Visitor
// ============================================================================

struct HeapUseCollector<'a, A> {
    ctx: &'a ElaboratedContext,
    acc: &'a mut A,
}

impl<A: HeapUseAccumulator> Visitor<DefId, TypeId> for HeapUseCollector<'_, A> {
    fn visit_expr(&mut self, expr: &Expr) {
        if let Some(def_id) = heap_use_def(expr, self.ctx) {
            self.acc.record(def_id);
        }
        walk_expr(self, expr);
    }
}

/// Returns the DefId if `expr` is a plain heap variable read.
fn heap_use_def(expr: &Expr, ctx: &ElaboratedContext) -> Option<DefId> {
    let ExprKind::Var { def_id, .. } = expr.kind else {
        return None;
    };
    if !ctx.type_map.type_table().get(expr.ty).is_heap() {
        return None;
    }
    Some(def_id)
}
