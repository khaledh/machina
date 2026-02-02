//! Slice borrow checking: prevents mutation/move of a base while slices into it exist.
//!
//! Rule: once a slice is stored in a local, its base cannot be mutated or moved
//! while that slice local is still live.
//!
//! Example violation:
//! ```mc
//! let s = arr[0..5];  // s borrows arr
//! arr[0] = 42;        // ERROR: arr is borrowed by s
//! ```

use std::collections::{HashMap, HashSet};

use crate::analysis::dataflow::solve_forward;
use crate::context::NormalizedContext;
use crate::resolve::DefId;
use crate::semck::SemCheckError;
use crate::semck::liveness_util;
use crate::tree::cfg::{
    AstBlockId, TreeCfg, TreeCfgBuilder, TreeCfgItem, TreeCfgNode, TreeCfgTerminator,
};
use crate::tree::normalized::{
    BindPattern, BindPatternKind, CallArg, Expr, ExprKind, FuncDef, ParamMode, StmtExpr,
    StmtExprKind,
};
use crate::tree::visit::{Visitor, walk_expr};
use crate::types::{Type, TypeId};

pub(super) fn check(ctx: &NormalizedContext) -> Vec<SemCheckError> {
    let mut errors = Vec::new();

    for func_def in ctx.module.func_defs() {
        check_func_def(ctx, func_def, &mut errors);
    }

    errors
}

/// Two dataflow passes:
/// 1. Forward: track which bases each slice local borrows from
/// 2. Backward: compute liveness of slice locals
///
/// Then walk each block, checking for conflicts where a borrowed base is
/// mutated/moved while a slice borrowing it is still live.
fn check_func_def(ctx: &NormalizedContext, func_def: &FuncDef, errors: &mut Vec<SemCheckError>) {
    let cfg = TreeCfgBuilder::<TypeId>::new().build_from_expr(&func_def.body);
    let liveness = analyze_slice_liveness(&cfg, ctx);
    let bindings = analyze_slice_bindings(&cfg, ctx);

    for (block_idx, node) in cfg.nodes.iter().enumerate() {
        let mut state = bindings.in_map[block_idx].clone();

        for (item_idx, item) in node.items.iter().enumerate() {
            // Active slices = live after this item + used in this item.
            let mut active_slices = liveness.live_after[block_idx][item_idx].clone();
            collect_item_slice_uses(item, ctx, &mut active_slices);

            if !active_slices.is_empty() {
                let borrowed_bases = borrowed_bases_for_active(&active_slices, &state);
                if !borrowed_bases.is_empty() {
                    check_item_for_conflicts(ctx, item, &borrowed_bases, errors);
                }
            }

            // Advance the flow-sensitive binding state for the next item.
            apply_item_bindings(&mut state, item, ctx);
        }

        if let TreeCfgTerminator::If { cond, .. } = &node.term {
            let mut active_slices = liveness.live_out[block_idx].clone();
            collect_expr_slice_uses(cond, ctx, &mut active_slices);

            if !active_slices.is_empty() {
                let borrowed_bases = borrowed_bases_for_active(&active_slices, &state);
                if !borrowed_bases.is_empty() {
                    let mut visitor = BorrowConflictVisitor::new(ctx, &borrowed_bases, errors);
                    visitor.visit_expr(cond);
                }
            }
        }
    }
}

fn borrowed_bases_for_active(
    active_slices: &HashSet<DefId>,
    state: &SliceBindings,
) -> HashSet<DefId> {
    let mut borrowed = HashSet::new();
    for slice_def in active_slices {
        if let Some(bases) = state.get(slice_def) {
            borrowed.extend(bases.iter().copied());
        }
    }
    borrowed
}

fn check_item_for_conflicts(
    ctx: &NormalizedContext,
    item: &TreeCfgItem<'_, TypeId>,
    borrowed_bases: &HashSet<DefId>,
    errors: &mut Vec<SemCheckError>,
) {
    match item {
        TreeCfgItem::Stmt(stmt) => match &stmt.kind {
            StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
                let mut visitor = BorrowConflictVisitor::new(ctx, borrowed_bases, errors);
                visitor.visit_expr(value);
            }
            StmtExprKind::VarDecl { .. } => {}
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                check_write_target(ctx, assignee, borrowed_bases, errors);
                let mut visitor = BorrowConflictVisitor::new(ctx, borrowed_bases, errors);
                visitor.visit_expr(value);
            }
            StmtExprKind::While { cond, body } => {
                let mut visitor = BorrowConflictVisitor::new(ctx, borrowed_bases, errors);
                visitor.visit_expr(cond);
                visitor.visit_expr(body);
            }
            StmtExprKind::For { iter, body, .. } => {
                let mut visitor = BorrowConflictVisitor::new(ctx, borrowed_bases, errors);
                visitor.visit_expr(iter);
                visitor.visit_expr(body);
            }
            StmtExprKind::Break | StmtExprKind::Continue => {}
            StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    let mut visitor = BorrowConflictVisitor::new(ctx, borrowed_bases, errors);
                    visitor.visit_expr(value);
                }
            }
        },
        TreeCfgItem::Expr(expr) => {
            let mut visitor = BorrowConflictVisitor::new(ctx, borrowed_bases, errors);
            visitor.visit_expr(expr);
        }
    }
}

fn check_write_target(
    ctx: &NormalizedContext,
    expr: &Expr,
    borrowed_bases: &HashSet<DefId>,
    errors: &mut Vec<SemCheckError>,
) {
    if let Some(def) = base_def_id(expr, ctx)
        && borrowed_bases.contains(&def)
    {
        errors.push(SemCheckError::SliceBorrowConflict(expr.span));
    }
}

// ============================================================================
// Flow-sensitive slice bindings (slice local -> base defs)
// ============================================================================

/// Maps each slice local to the set of bases it may borrow from.
/// A slice can borrow multiple bases if assigned conditionally.
type SliceBindings = HashMap<DefId, HashSet<DefId>>;

struct SliceBindingAnalysis {
    in_map: Vec<SliceBindings>,
}

/// Forward dataflow: track slice -> base bindings through the CFG.
fn analyze_slice_bindings(
    cfg: &TreeCfg<'_, TypeId>,
    ctx: &NormalizedContext,
) -> SliceBindingAnalysis {
    let entry_state = HashMap::new();
    let bottom = HashMap::new();

    let analysis = solve_forward(
        cfg,
        AstBlockId(0),
        entry_state,
        bottom,
        merge_slice_bindings,
        |block_id, in_state| apply_block_bindings(&cfg.nodes[block_id.0], in_state, ctx),
    );

    SliceBindingAnalysis {
        in_map: analysis.in_map,
    }
}

/// Meet = union: if a slice borrows a base on any path, track it.
fn merge_slice_bindings(states: &[SliceBindings]) -> SliceBindings {
    let mut merged = HashMap::new();
    for state in states {
        for (slice_def, bases) in state {
            merged
                .entry(*slice_def)
                .or_insert_with(HashSet::new)
                .extend(bases.iter().copied());
        }
    }
    merged
}

fn apply_block_bindings(
    node: &TreeCfgNode<'_, TypeId>,
    in_state: &SliceBindings,
    ctx: &NormalizedContext,
) -> SliceBindings {
    let mut state = in_state.clone();
    for item in &node.items {
        apply_item_bindings(&mut state, item, ctx);
    }
    state
}

fn apply_item_bindings(
    state: &mut SliceBindings,
    item: &TreeCfgItem<'_, TypeId>,
    ctx: &NormalizedContext,
) {
    let TreeCfgItem::Stmt(stmt) = item else {
        return;
    };

    match &stmt.kind {
        StmtExprKind::LetBind { pattern, value, .. }
        | StmtExprKind::VarBind { pattern, value, .. } => {
            if let BindPatternKind::Name { def_id, .. } = &pattern.kind
                && let Some(def) = ctx.def_table.lookup_def(*def_id)
                && matches!(ctx.type_map.lookup_def_type(def), Some(Type::Slice { .. }))
            {
                let bases = slice_bases_for_value(value, state, ctx);
                update_slice_binding(state, *def_id, bases);
            }
        }
        StmtExprKind::Assign {
            assignee, value, ..
        } => {
            if let ExprKind::Var { def_id, .. } = assignee.kind
                && let Some(def) = ctx.def_table.lookup_def(def_id)
                && matches!(ctx.type_map.lookup_def_type(def), Some(Type::Slice { .. }))
            {
                let bases = slice_bases_for_value(value, state, ctx);
                update_slice_binding(state, def_id, bases);
            }
        }
        _ => {}
    }
}

fn update_slice_binding(state: &mut SliceBindings, slice_def: DefId, bases: HashSet<DefId>) {
    // No known bases means the slice no longer borrows anything we can track.
    if bases.is_empty() {
        state.remove(&slice_def);
    } else {
        state.insert(slice_def, bases);
    }
}

/// Determine which bases a value expression borrows from.
fn slice_bases_for_value(
    expr: &Expr,
    state: &SliceBindings,
    ctx: &NormalizedContext,
) -> HashSet<DefId> {
    match &expr.kind {
        ExprKind::Slice { target, .. } => slice_bases_for_slice_target(target, state, ctx),
        ExprKind::Coerce { expr, .. } => slice_bases_for_slice_target(expr, state, ctx),
        // Copy of existing slice: inherit its borrowed bases.
        ExprKind::Var { .. } | ExprKind::Move { .. } => {
            if let Some(slice_def) = slice_def_from_expr(expr, ctx) {
                return state.get(&slice_def).cloned().unwrap_or_default();
            }
            HashSet::new()
        }
        ExprKind::If {
            then_body,
            else_body,
            ..
        } => {
            // Merge bases across branches (flow-sensitive but conservative).
            let mut bases = slice_bases_for_value(then_body, state, ctx);
            bases.extend(slice_bases_for_value(else_body, state, ctx));
            bases
        }
        ExprKind::Block { tail, .. } => {
            // Only the tail expression contributes a value.
            if let Some(tail) = tail {
                return slice_bases_for_value(tail, state, ctx);
            }
            HashSet::new()
        }
        _ => HashSet::new(),
    }
}

/// If target is itself a slice, inherit its bases; otherwise use the base variable.
fn slice_bases_for_slice_target(
    target: &Expr,
    state: &SliceBindings,
    ctx: &NormalizedContext,
) -> HashSet<DefId> {
    // Subslicing another slice: inherit the original's bases.
    if let Some(slice_def) = slice_def_from_expr(target, ctx) {
        return state.get(&slice_def).cloned().unwrap_or_default();
    }

    if let Some(base) = base_def_id(target, ctx) {
        let mut bases = HashSet::new();
        bases.insert(base);
        return bases;
    }

    HashSet::new()
}

fn slice_def_from_expr(expr: &Expr, ctx: &NormalizedContext) -> Option<DefId> {
    match &expr.kind {
        ExprKind::Var { def_id, .. } => {
            let ty = ctx.type_map.type_table().get(expr.ty);
            if !matches!(ty, Type::Slice { .. }) {
                return None;
            }
            Some(*def_id)
        }
        ExprKind::Move { expr } => slice_def_from_expr(expr, ctx),
        ExprKind::Coerce { expr, .. } => slice_def_from_expr(expr, ctx),
        _ => None,
    }
}

// ============================================================================
// Liveness for slice locals (used for last-use borrow release)
// ============================================================================

struct SliceLiveness {
    live_out: Vec<HashSet<DefId>>,
    live_after: Vec<Vec<HashSet<DefId>>>,
}

/// Backward dataflow: compute which slice locals are live at each point.
fn analyze_slice_liveness(cfg: &TreeCfg<'_, TypeId>, ctx: &NormalizedContext) -> SliceLiveness {
    let analysis = liveness_util::analyze_liveness(
        cfg,
        |term, uses| add_terminator_uses(term, ctx, uses),
        |item, defs, uses| collect_item_defs_uses(item, ctx, defs, uses),
    );

    SliceLiveness {
        live_out: analysis.live_out,
        live_after: analysis.live_after,
    }
}

/// Add slice uses from the block terminator (if condition).
fn add_terminator_uses(
    term: &TreeCfgTerminator<'_, TypeId>,
    ctx: &NormalizedContext,
    uses: &mut HashSet<DefId>,
) {
    if let TreeCfgTerminator::If { cond, .. } = term {
        collect_expr_slice_uses(cond, ctx, uses);
    }
}

/// Collect defs and uses of slice locals from an item.
fn collect_item_defs_uses(
    item: &TreeCfgItem<'_, TypeId>,
    ctx: &NormalizedContext,
    defs: &mut HashSet<DefId>,
    uses: &mut HashSet<DefId>,
) {
    match item {
        TreeCfgItem::Stmt(stmt) => collect_stmt_defs_uses(stmt, ctx, defs, uses),
        TreeCfgItem::Expr(expr) => collect_expr_slice_uses(expr, ctx, uses),
    }
}

/// Collect defs and uses of slice locals from a statement.
fn collect_stmt_defs_uses(
    stmt: &StmtExpr,
    ctx: &NormalizedContext,
    defs: &mut HashSet<DefId>,
    uses: &mut HashSet<DefId>,
) {
    collect_stmt_slice_uses(stmt, ctx, uses);
    match &stmt.kind {
        StmtExprKind::LetBind { pattern, .. } | StmtExprKind::VarBind { pattern, .. } => {
            collect_pattern_defs(pattern, ctx, defs);
        }
        StmtExprKind::VarDecl { .. } => {}
        StmtExprKind::Assign { assignee, .. } => {
            collect_assignee_defs(assignee, ctx, defs);
        }
        StmtExprKind::While { .. } | StmtExprKind::For { .. } => {}
        StmtExprKind::Break | StmtExprKind::Continue => {}
        StmtExprKind::Return { .. } => {}
    }
}

/// Collect slice-typed definitions from a pattern.
fn collect_pattern_defs(pattern: &BindPattern, ctx: &NormalizedContext, defs: &mut HashSet<DefId>) {
    match &pattern.kind {
        BindPatternKind::Name { def_id, .. } => add_def_if_slice(*def_id, ctx, defs),
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

/// Collect slice-typed definitions from an assignment target.
fn collect_assignee_defs(assignee: &Expr, ctx: &NormalizedContext, defs: &mut HashSet<DefId>) {
    if let ExprKind::Var { def_id, .. } = assignee.kind {
        add_def_if_slice(def_id, ctx, defs);
    }
}

/// Add a DefId to the set if it has slice type.
fn add_def_if_slice(def_id: DefId, ctx: &NormalizedContext, defs: &mut HashSet<DefId>) {
    let Some(def) = ctx.def_table.lookup_def(def_id) else {
        return;
    };
    let Some(ty) = ctx.type_map.lookup_def_type(def) else {
        return;
    };
    if matches!(ty, Type::Slice { .. }) {
        defs.insert(def_id);
    }
}

/// Collect slice local uses from an item.
fn collect_item_slice_uses(
    item: &TreeCfgItem<'_, TypeId>,
    ctx: &NormalizedContext,
    uses: &mut HashSet<DefId>,
) {
    match item {
        TreeCfgItem::Stmt(stmt) => collect_stmt_slice_uses(stmt, ctx, uses),
        TreeCfgItem::Expr(expr) => collect_expr_slice_uses(expr, ctx, uses),
    }
}

/// Collect slice local uses from a statement.
fn collect_stmt_slice_uses(stmt: &StmtExpr, ctx: &NormalizedContext, uses: &mut HashSet<DefId>) {
    match &stmt.kind {
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            collect_expr_slice_uses(value, ctx, uses);
        }
        StmtExprKind::VarDecl { .. } => {}
        StmtExprKind::Assign {
            assignee, value, ..
        } => {
            collect_expr_slice_uses(value, ctx, uses);
            collect_assignee_uses(assignee, ctx, uses);
        }
        StmtExprKind::While { cond, body } => {
            collect_expr_slice_uses(cond, ctx, uses);
            collect_expr_slice_uses(body, ctx, uses);
        }
        StmtExprKind::For { iter, body, .. } => {
            collect_expr_slice_uses(iter, ctx, uses);
            collect_expr_slice_uses(body, ctx, uses);
        }
        StmtExprKind::Break | StmtExprKind::Continue => {}
        StmtExprKind::Return { value } => {
            if let Some(value) = value {
                collect_expr_slice_uses(value, ctx, uses);
            }
        }
    }
}

/// Collect slice uses from an assignment target (not the assigned-to var itself).
fn collect_assignee_uses(assignee: &Expr, ctx: &NormalizedContext, uses: &mut HashSet<DefId>) {
    match &assignee.kind {
        // Assigning to a var doesn't use it.
        ExprKind::Var { .. } => {}
        ExprKind::StructField { target, .. } | ExprKind::TupleField { target, .. } => {
            collect_expr_slice_uses(target, ctx, uses);
        }
        ExprKind::ArrayIndex { target, indices } => {
            collect_expr_slice_uses(target, ctx, uses);
            for index in indices {
                collect_expr_slice_uses(index, ctx, uses);
            }
        }
        ExprKind::Slice { target, start, end } => {
            collect_expr_slice_uses(target, ctx, uses);
            if let Some(start) = start {
                collect_expr_slice_uses(start, ctx, uses);
            }
            if let Some(end) = end {
                collect_expr_slice_uses(end, ctx, uses);
            }
        }
        _ => collect_expr_slice_uses(assignee, ctx, uses),
    }
}

/// Walk an expression and collect all slice-typed variable uses.
fn collect_expr_slice_uses(expr: &Expr, ctx: &NormalizedContext, uses: &mut HashSet<DefId>) {
    let mut collector = SliceUseCollector { ctx, uses };
    collector.visit_expr(expr);
}

/// Visitor that collects slice-typed variable uses.
struct SliceUseCollector<'a> {
    ctx: &'a NormalizedContext,
    uses: &'a mut HashSet<DefId>,
}

impl Visitor<DefId, TypeId> for SliceUseCollector<'_> {
    fn visit_expr(&mut self, expr: &Expr) {
        if let Some(def_id) = slice_use_def(expr, self.ctx) {
            self.uses.insert(def_id);
        }
        walk_expr(self, expr);
    }
}

/// If expr is a slice-typed variable use, return its DefId.
fn slice_use_def(expr: &Expr, ctx: &NormalizedContext) -> Option<DefId> {
    let ExprKind::Var { def_id, .. } = expr.kind else {
        return None;
    };
    let ty = ctx.type_map.type_table().get(expr.ty);
    if !matches!(ty, Type::Slice { .. }) {
        return None;
    }
    Some(def_id)
}

// ============================================================================
// Borrowed base conflict checks
// ============================================================================

/// Visitor that reports errors for moves or mutating calls on borrowed bases.
struct BorrowConflictVisitor<'a> {
    ctx: &'a NormalizedContext,
    borrowed_bases: &'a HashSet<DefId>,
    errors: &'a mut Vec<SemCheckError>,
}

impl<'a> BorrowConflictVisitor<'a> {
    fn new(
        ctx: &'a NormalizedContext,
        borrowed_bases: &'a HashSet<DefId>,
        errors: &'a mut Vec<SemCheckError>,
    ) -> Self {
        Self {
            ctx,
            borrowed_bases,
            errors,
        }
    }

    /// Check call arguments: error if passing a borrowed base to inout/out/sink.
    fn check_call(&mut self, call: &Expr, args: &[CallArg], receiver: Option<&Expr>) {
        let Some(sig) = self.ctx.call_sigs.get(&call.id) else {
            return;
        };

        if let (Some(receiver_param), Some(receiver)) = (sig.receiver.as_ref(), receiver)
            && matches!(
                receiver_param.mode,
                ParamMode::InOut | ParamMode::Out | ParamMode::Sink
            )
            && let Some(def) = base_def_id(receiver, self.ctx)
            && self.borrowed_bases.contains(&def)
        {
            self.errors
                .push(SemCheckError::SliceBorrowConflict(receiver.span));
        }

        for (param, arg) in sig.params.iter().zip(args) {
            let arg_expr = &arg.expr;
            // Only mutating modes conflict.
            if !matches!(
                param.mode,
                ParamMode::InOut | ParamMode::Out | ParamMode::Sink
            ) {
                continue;
            }
            if let Some(def) = base_def_id(arg_expr, self.ctx)
                && self.borrowed_bases.contains(&def)
            {
                self.errors
                    .push(SemCheckError::SliceBorrowConflict(arg.span));
            }
        }
    }
}

impl Visitor<DefId, TypeId> for BorrowConflictVisitor<'_> {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            // Moving a borrowed base is a conflict.
            ExprKind::Move { expr: inner } => {
                if let Some(def) = base_def_id(inner, self.ctx)
                    && self.borrowed_bases.contains(&def)
                {
                    self.errors
                        .push(SemCheckError::SliceBorrowConflict(expr.span));
                }
            }
            ExprKind::Call { args, .. } => {
                self.check_call(expr, args, None);
            }
            ExprKind::MethodCall { callee, args, .. } => {
                self.check_call(expr, args, Some(callee));
            }
            _ => {}
        }
        walk_expr(self, expr);
    }
}

// ============================================================================
// Utilities
// ============================================================================

/// Extract the base variable's DefId from an lvalue expression.
/// E.g., `arr[0].field` -> DefId of `arr`.
fn base_def_id(expr: &Expr, ctx: &NormalizedContext) -> Option<DefId> {
    match &expr.kind {
        ExprKind::Var { def_id, .. } => Some(*def_id),
        ExprKind::ArrayIndex { target, .. }
        | ExprKind::TupleField { target, .. }
        | ExprKind::StructField { target, .. }
        | ExprKind::Slice { target, .. }
        | ExprKind::Coerce { expr: target, .. } => base_def_id(target, ctx),
        ExprKind::Move { expr } => base_def_id(expr, ctx),
        _ => None,
    }
}
