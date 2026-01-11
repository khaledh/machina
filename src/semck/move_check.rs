//! Move checking: tracks ownership transfer and prevents use-after-move.
//!
//! Semantics:
//! - `move x` is only valid if x is a plain variable (no projections).
//! - Move tracking applies to compound types and heap-owned values (`Type::is_move_tracked()`).
//! - Any use of a moved variable is an error.
//! - Re-assigning a variable (`x = ...`) clears its moved status.
//! - Heap values require explicit `move` unless it's their last use (implicit move).

use std::collections::{HashMap, HashSet};

use crate::analysis::dataflow::solve_forward;
use crate::ast::cfg::{AstBlockId, HirCfgBuilder, HirCfgNode, HirItem, HirTerminator};
use crate::context::TypeCheckedContext;
use crate::hir::model::{
    BindPattern, BindPatternKind, Expr, ExprKind, FuncDef, NodeId, ParamMode, StmtExpr,
    StmtExprKind,
};
use crate::hir::visit::{Visitor, walk_expr};
use crate::resolve::{DefId, DefKind};
use crate::semck::SemCheckError;
use crate::semck::ast_liveness::{self, AstLiveness};

pub struct MoveCheckResult {
    pub errors: Vec<SemCheckError>,
    /// Nodes where an implicit move was inserted (last-use of heap values).
    pub implicit_moves: HashSet<NodeId>,
}

/// Run move checking and collect implicit moves for last-use heap values.
pub fn check(ctx: &TypeCheckedContext) -> MoveCheckResult {
    let mut errors = Vec::new();
    let mut implicit_moves = HashSet::new();
    for func_def in ctx.module.func_defs() {
        check_func_def(func_def, ctx, &mut errors, &mut implicit_moves);
    }
    MoveCheckResult {
        errors,
        implicit_moves,
    }
}

fn check_func_def(
    func_def: &FuncDef,
    ctx: &TypeCheckedContext,
    errors: &mut Vec<SemCheckError>,
    implicit_moves: &mut HashSet<NodeId>,
) {
    let cfg = HirCfgBuilder::new().build_from_expr(&func_def.body);

    // Precompute heap liveness for last-use detection (implicit moves).
    let liveness = ast_liveness::analyze(&cfg, ctx);

    // Sink params own their value and can be moved from.
    let mut sink_params = HashSet::new();
    for param in &func_def.sig.params {
        if param.mode == ParamMode::Sink {
            sink_params.insert(param.ident);
        }
    }

    // State = set of DefIds that have been moved.
    // Entry = empty (nothing moved yet). Bottom = empty (safe default).
    let empty = HashSet::new();
    solve_forward(
        &cfg,
        AstBlockId(0),
        empty.clone(),
        empty,
        |states| {
            // Meet = union: if moved on ANY path, treat as moved (conservative).
            // This is the opposite of def_init which uses intersection.
            let mut out = HashSet::new();
            for s in states {
                out.extend(s.iter().cloned());
            }
            out
        },
        |block_id, in_state| {
            // Move checking is forward: we need to catch use-after-move in order.
            let mut visitor = MoveVisitor::new(
                ctx,
                in_state.clone(),
                sink_params.clone(),
                errors,
                implicit_moves,
                &liveness,
            );
            visitor.visit_cfg_node(&cfg.nodes[block_id.0], block_id);
            visitor.moved
        },
    );
}

/// Walks expressions checking for use-after-move and tracking moved variables.
struct MoveVisitor<'a> {
    ctx: &'a TypeCheckedContext,
    /// Variables that have been moved and cannot be used.
    moved: HashSet<DefId>,
    /// Sink params can be moved from (they own the value).
    sink_params: HashSet<DefId>,
    /// Collects nodes that get implicit moves (last-use of heap values).
    implicit_moves: &'a mut HashSet<NodeId>,
    /// Precomputed liveness for detecting last-use sites.
    liveness: &'a AstLiveness,
    /// Live heap vars after the current item (for last-use detection).
    current_live_after: Option<HashSet<DefId>>,
    /// How many times each heap var is used in the current item.
    current_use_counts: Option<HashMap<DefId, usize>>,
    /// True when inside a borrow context (in/inout args) - skips move requirements.
    borrow_context: bool,
    errors: &'a mut Vec<SemCheckError>,
}

impl<'a> MoveVisitor<'a> {
    fn new(
        ctx: &'a TypeCheckedContext,
        moved: HashSet<DefId>,
        sink_params: HashSet<DefId>,
        errors: &'a mut Vec<SemCheckError>,
        implicit_moves: &'a mut HashSet<NodeId>,
        liveness: &'a AstLiveness,
    ) -> Self {
        Self {
            ctx,
            moved,
            sink_params,
            errors,
            implicit_moves,
            liveness,
            current_live_after: None,
            current_use_counts: None,
            borrow_context: false,
        }
    }

    /// Process a CFG block: check each item with its liveness context.
    fn visit_cfg_node(&mut self, node: &HirCfgNode<'_>, block_id: AstBlockId) {
        // Precompute per-item info for implicit move detection:
        // - use_counts: how many times each heap var is used in each item
        // - live_after: which heap vars are live after each item
        let item_use_counts = node
            .items
            .iter()
            .map(|item| ast_liveness::heap_use_counts_for_item(item, self.ctx))
            .collect::<Vec<_>>();
        let live_after = &self.liveness.live_after[block_id.0];

        for (idx, item) in node.items.iter().enumerate() {
            // Set context for implicit move detection in this item.
            self.current_live_after = Some(live_after[idx].clone());
            self.current_use_counts = Some(item_use_counts[idx].clone());
            match item {
                HirItem::Stmt(stmt) => self.visit_stmt_expr(stmt),
                HirItem::Expr(expr) => self.visit_expr(expr),
            }
        }
        self.current_live_after = None;
        self.current_use_counts = None;

        // Check terminator condition (if any).
        match &node.term {
            HirTerminator::If { cond, .. } => self.visit_expr(cond),
            HirTerminator::Goto(_) | HirTerminator::End => {}
        }
    }

    /// Process `move x`: validate target and mark as moved.
    fn handle_move_target(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Var(def_id) => {
                let Some(def) = self.ctx.def_table.lookup_def(*def_id) else {
                    return;
                };
                // Params can only be moved if they're sink params (owned).
                if matches!(def.kind, DefKind::Param { .. }) {
                    if !self.sink_params.contains(def_id) {
                        self.errors.push(SemCheckError::MoveFromParam(expr.span));
                        return;
                    }
                }
                let Some(ty) = self.ctx.type_map.lookup_node_type(expr.id) else {
                    return;
                };
                // Only track moves for types that need ownership tracking.
                if ty.is_move_tracked() {
                    self.moved.insert(*def_id);
                }
            }
            // `move x.field` or `move arr[i]` not allowed - must move whole variable.
            _ => self
                .errors
                .push(SemCheckError::InvalidMoveTarget(expr.span)),
        }
    }

    /// Execute `f` in a borrow context where heap move requirements are relaxed.
    /// Used for `in`/`inout` arguments which borrow rather than consume.
    fn with_borrow_context(&mut self, f: impl FnOnce(&mut Self)) {
        let prev = self.borrow_context;
        self.borrow_context = true;
        f(self);
        self.borrow_context = prev;
    }

    /// Error if using a variable that has already been moved.
    fn check_use(&mut self, expr: &Expr) {
        if let ExprKind::Var(def_id) = expr.kind
            && let Some(def) = self.ctx.def_table.lookup_def(def_id)
            && self.moved.contains(&def_id)
        {
            self.errors
                .push(SemCheckError::UseAfterMove(def.name.clone(), expr.span));
        }
    }

    /// For heap-owned values: require explicit `move` unless this is the last use.
    /// Last-use detection: not live after this item AND only used once in this item.
    fn check_heap_move_required(&mut self, expr: &Expr) {
        if let Some(ty) = self.ctx.type_map.lookup_node_type(expr.id)
            && ty.is_heap()
        {
            let ExprKind::Var(def_id) = expr.kind else {
                return;
            };
            let Some(def) = self.ctx.def_table.lookup_def(def_id) else {
                return;
            };
            if matches!(def.kind, DefKind::Param { .. }) {
                // Allow moving from sink params only.
                if !self.sink_params.contains(&def_id) {
                    self.errors.push(SemCheckError::MoveFromParam(expr.span));
                    return;
                }
            }
            let Some(ref live_after) = self.current_live_after else {
                self.errors
                    .push(SemCheckError::OwnedMoveRequired(expr.span));
                return;
            };
            let use_count = self
                .current_use_counts
                .as_ref()
                .and_then(|counts| counts.get(&def_id))
                .copied()
                .unwrap_or(0);
            // Require explicit move if:
            // - Used multiple times in same item (e.g., `f(p, p)`) - ambiguous which moves
            // - Live after this item - can't implicitly consume something still needed
            if use_count > 1 || live_after.contains(&def_id) {
                self.errors
                    .push(SemCheckError::OwnedMoveRequired(expr.span));
                return;
            }

            // Last-use and only use in this item: implicit move is safe.
            self.implicit_moves.insert(expr.id);
            self.moved.insert(def_id);
        }
    }

    /// Descend through projections to the base variable, checking use-after-move.
    /// Projections (field access, indexing) borrow the base, so no move required.
    fn visit_place_base(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Var(_) => {
                // Place projections are treated as borrows of the base.
                self.check_use(expr);
            }
            ExprKind::StructField { target, .. } => {
                self.visit_place_base(target);
            }
            ExprKind::TupleField { target, .. } => {
                self.visit_place_base(target);
            }
            ExprKind::ArrayIndex { target, indices } => {
                self.visit_place_base(target);
                for index in indices {
                    self.visit_expr(index);
                }
            }
            _ => self.visit_expr(expr),
        }
    }

    /// Clear moved status for variables bound by a pattern.
    /// Called on let/var bindings and reassignments to "revive" the variable.
    fn clear_pattern_defs(&mut self, pattern: &BindPattern) {
        match &pattern.kind {
            BindPatternKind::Name(def_id) => {
                self.moved.remove(def_id);
            }
            BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
                for p in patterns {
                    self.clear_pattern_defs(p);
                }
            }
            BindPatternKind::Struct { fields, .. } => {
                for f in fields {
                    self.clear_pattern_defs(&f.pattern);
                }
            }
        }
    }

    fn visit_out_arg(&mut self, arg: &Expr) {
        match &arg.kind {
            ExprKind::Var(def_id) => {
                // Out args are reinitialized by the callee.
                self.moved.remove(def_id);
            }
            ExprKind::StructField { target, .. } => {
                self.visit_place_base(target);
            }
            ExprKind::TupleField { target, .. } => {
                self.visit_place_base(target);
            }
            ExprKind::ArrayIndex { target, indices } => {
                self.visit_place_base(target);
                for index in indices {
                    self.visit_expr(index);
                }
            }
            ExprKind::Slice { target, start, end } => {
                self.visit_place_base(target);
                if let Some(start) = start {
                    self.visit_expr(start);
                }
                if let Some(end) = end {
                    self.visit_expr(end);
                }
            }
            _ => self.visit_expr(arg),
        }
    }
}

impl<'a> Visitor for MoveVisitor<'a> {
    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { pattern, value, .. }
            | StmtExprKind::VarBind { pattern, value, .. } => {
                self.visit_expr(value);
                self.clear_pattern_defs(pattern);
            }
            StmtExprKind::VarDecl { .. } => {}
            StmtExprKind::Assign { assignee, value } => {
                self.visit_expr(value);
                if let ExprKind::Var(def_id) = assignee.kind {
                    // Reassigning a variable clears its moved status.
                    self.moved.remove(&def_id);
                } else {
                    // For projections (x.field = ...), check the base is usable.
                    self.visit_expr(assignee);
                }
            }
            StmtExprKind::While { cond, .. } => {
                self.visit_expr(cond);
            }
            StmtExprKind::For { iter, .. } => {
                self.visit_expr(iter);
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Move { expr } => {
                self.handle_move_target(expr);
            }
            ExprKind::Var(_) => {
                self.check_use(expr);
                // Heap values need explicit move (or implicit if last-use).
                // Skip if we're in a borrow context (in/inout arg).
                if !self.borrow_context {
                    self.check_heap_move_required(expr);
                }
            }
            ExprKind::ArrayIndex { target, indices } => {
                self.visit_place_base(target);
                for index in indices {
                    self.visit_expr(index);
                }
            }
            ExprKind::TupleField { target, .. } => {
                self.visit_place_base(target);
            }
            ExprKind::StructField { target, .. } => {
                self.visit_place_base(target);
            }
            // If/Match bodies are handled as separate CFG blocks; only check cond/scrutinee here.
            ExprKind::If { cond, .. } => {
                self.visit_expr(cond);
            }
            ExprKind::Match { scrutinee, .. } => {
                self.visit_place_base(scrutinee);
            }
            ExprKind::Call { callee, args } => {
                self.visit_expr(callee);
                // Check args based on param mode: in/inout borrow, sink consumes.
                if let Some(sig) = self.ctx.type_map.lookup_call_sig(expr.id) {
                    for (param, arg) in sig.params.iter().zip(args) {
                        let arg_expr = &arg.expr;
                        match param.mode {
                            ParamMode::In | ParamMode::InOut => {
                                self.with_borrow_context(|this| this.visit_expr(arg_expr));
                            }
                            ParamMode::Out => {
                                self.visit_out_arg(arg_expr);
                            }
                            ParamMode::Sink => {
                                self.handle_move_target(arg_expr);
                            }
                        }
                    }
                } else {
                    for arg in args {
                        self.visit_expr(&arg.expr);
                    }
                }
            }

            ExprKind::MethodCall { callee, args, .. } => {
                if let Some(sig) = self.ctx.type_map.lookup_call_sig(expr.id) {
                    if let Some(receiver) = sig.receiver.as_ref() {
                        match receiver.mode {
                            ParamMode::In | ParamMode::InOut => {
                                self.with_borrow_context(|this| this.visit_expr(callee));
                            }
                            ParamMode::Out => {
                                self.visit_out_arg(callee);
                            }
                            ParamMode::Sink => {
                                self.handle_move_target(callee);
                            }
                        }
                    } else {
                        self.visit_expr(callee);
                    }

                    for (param, arg) in sig.params.iter().zip(args) {
                        let arg_expr = &arg.expr;
                        match param.mode {
                            ParamMode::In | ParamMode::InOut => {
                                self.with_borrow_context(|this| this.visit_expr(arg_expr));
                            }
                            ParamMode::Out => {
                                self.visit_out_arg(arg_expr);
                            }
                            ParamMode::Sink => {
                                self.handle_move_target(arg_expr);
                            }
                        }
                    }
                } else {
                    self.visit_expr(callee);
                    for arg in args {
                        self.visit_expr(&arg.expr);
                    }
                }
            }
            _ => walk_expr(self, expr),
        }
    }
}
