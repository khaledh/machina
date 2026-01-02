//! Semantics
// - `move x` only valid if x is a plain variable (no projections).
// - Move tracks compound types and heap-owned values (`Type::is_move_tracked()`).
// - Any use of a moved var is an error.
// - Re‑assigning the var (`x = ...`) clears moved status.
// - `inout` use counts as use (already checked elsewhere).

use std::collections::{HashMap, HashSet};

use crate::analysis::dataflow::solve_forward;
use crate::ast::cfg::{AstBlockId, AstCfgBuilder, AstCfgNode, AstItem, AstTerminator};
use crate::ast::{
    Expr, ExprKind, Function, FunctionParamMode, Pattern, PatternKind, StmtExpr, StmtExprKind,
};
use crate::ast::{Visitor, walk_expr};
use crate::context::TypeCheckedContext;
use crate::resolve::def_map::{DefId, DefKind};
use crate::semck::SemCheckError;
use crate::semck::util::lookup_call_sig;
use crate::semck::ast_liveness::{self, AstLiveness};

pub struct MoveCheckResult {
    pub errors: Vec<SemCheckError>,
    pub implicit_moves: HashSet<crate::ast::NodeId>,
}

// Run move checking and collect implicit moves for last-use heap values.
pub fn check(ctx: &TypeCheckedContext) -> MoveCheckResult {
    let mut errors = Vec::new();
    let mut implicit_moves = HashSet::new();
    for func in ctx.module.funcs() {
        check_func(func, ctx, &mut errors, &mut implicit_moves);
    }
    MoveCheckResult {
        errors,
        implicit_moves,
    }
}

fn check_func(
    func: &Function,
    ctx: &TypeCheckedContext,
    errors: &mut Vec<SemCheckError>,
    implicit_moves: &mut HashSet<crate::ast::NodeId>,
) {
    let cfg = AstCfgBuilder::new().build_from_expr(&func.body);
    // Precompute heap liveness so we can detect last-use sites inside blocks.
    let liveness = ast_liveness::analyze(&cfg, ctx);

    let empty = HashSet::new();
    solve_forward(
        &cfg,
        AstBlockId(0), // entry block is block 0 in builder
        empty.clone(), // entry state
        empty,         // bottom
        |states| {
            // Conservative: once moved on any path, treat it as moved.
            let mut out = HashSet::new();
            for s in states {
                out.extend(s.iter().cloned());
            }
            out
        },
        |block_id, in_state| {
            // Move checking is forward: we need to catch use-after-move in order.
            let mut visitor =
                MoveVisitor::new(ctx, in_state.clone(), errors, implicit_moves, &liveness);
            visitor.visit_cfg_node(&cfg.nodes[block_id.0], block_id);
            visitor.moved
        },
    );
}

struct MoveVisitor<'a> {
    ctx: &'a TypeCheckedContext,
    moved: HashSet<DefId>,
    errors: &'a mut Vec<SemCheckError>,
    implicit_moves: &'a mut HashSet<crate::ast::NodeId>,
    liveness: &'a AstLiveness,
    current_live_after: Option<HashSet<DefId>>,
    current_use_counts: Option<HashMap<DefId, usize>>,
    borrow_context: bool,
}

impl<'a> MoveVisitor<'a> {
    fn new(
        ctx: &'a TypeCheckedContext,
        moved: HashSet<DefId>,
        errors: &'a mut Vec<SemCheckError>,
        implicit_moves: &'a mut HashSet<crate::ast::NodeId>,
        liveness: &'a AstLiveness,
    ) -> Self {
        Self {
            ctx,
            moved,
            errors,
            implicit_moves,
            liveness,
            current_live_after: None,
            current_use_counts: None,
            borrow_context: false,
        }
    }

    fn visit_cfg_node(&mut self, node: &AstCfgNode<'_>, block_id: AstBlockId) {
        // Count per-item heap uses so we don't implicitly move if an item
        // references the same binding multiple times.
        let item_use_counts = node
            .items
            .iter()
            .map(|item| ast_liveness::heap_use_counts_for_item(item, self.ctx))
            .collect::<Vec<_>>();
        let live_after = &self.liveness.live_after[block_id.0];

        for (idx, item) in node.items.iter().enumerate() {
            self.current_live_after = Some(live_after[idx].clone());
            self.current_use_counts = Some(item_use_counts[idx].clone());
            match item {
                AstItem::Stmt(stmt) => self.visit_stmt_expr(stmt),
                AstItem::Expr(expr) => self.visit_expr(expr),
            }
        }
        self.current_live_after = None;
        self.current_use_counts = None;
        match &node.term {
            AstTerminator::If { cond, .. } => self.visit_expr(cond),
            AstTerminator::Goto(_) | AstTerminator::End => {}
        }
    }

    fn handle_move_target(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Var(_) => {
                let Some(def) = self.ctx.def_map.lookup_def(expr.id) else {
                    return;
                };
                if matches!(def.kind, DefKind::Param { .. }) {
                    self.errors.push(SemCheckError::MoveFromParam(expr.span));
                    return;
                }
                let Some(ty) = self.ctx.type_map.lookup_node_type(expr.id) else {
                    return;
                };
                if ty.is_move_tracked() {
                    self.moved.insert(def.id);
                }
            }
            _ => self
                .errors
                .push(SemCheckError::InvalidMoveTarget(expr.span)),
        }
    }

    fn with_borrow_context(&mut self, f: impl FnOnce(&mut Self)) {
        let prev = self.borrow_context;
        self.borrow_context = true;
        f(self);
        self.borrow_context = prev;
    }

    fn check_use(&mut self, expr: &Expr) {
        if let Some(def) = self.ctx.def_map.lookup_def(expr.id)
            && self.moved.contains(&def.id)
        {
            self.errors
                .push(SemCheckError::UseAfterMove(def.name.clone(), expr.span));
        }
    }

    fn check_heap_move_required(&mut self, expr: &Expr) {
        if let Some(ty) = self.ctx.type_map.lookup_node_type(expr.id)
            && ty.is_heap()
        {
            let Some(def) = self.ctx.def_map.lookup_def(expr.id) else {
                return;
            };
            if matches!(def.kind, DefKind::Param { .. }) {
                self.errors
                    .push(SemCheckError::MoveFromParam(expr.span));
                return;
            }
            let Some(ref live_after) = self.current_live_after else {
                self.errors
                    .push(SemCheckError::OwnedMoveRequired(expr.span));
                return;
            };
            let use_count = self
                .current_use_counts
                .as_ref()
                .and_then(|counts| counts.get(&def.id))
                .copied()
                .unwrap_or(0);
            // If a statement uses the same binding multiple times (e.g., `f(p, p)`),
            // we require an explicit move to avoid partial-move ambiguity.
            if use_count > 1 || live_after.contains(&def.id) {
                self.errors
                    .push(SemCheckError::OwnedMoveRequired(expr.span));
                return;
            }

            // Last-use in this item; record an implicit move.
            self.implicit_moves.insert(expr.id);
            self.moved.insert(def.id);
        }
    }

    // Treat projections as borrowing from the base, so only check use-after-move
    // on the base variable and avoid requiring an explicit move.
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

    fn clear_pattern_defs(&mut self, pattern: &Pattern) {
        match &pattern.kind {
            PatternKind::Ident { .. } => {
                if let Some(def) = self.ctx.def_map.lookup_def(pattern.id) {
                    self.moved.remove(&def.id);
                }
            }
            PatternKind::Array { patterns } | PatternKind::Tuple { patterns } => {
                for p in patterns {
                    self.clear_pattern_defs(p);
                }
            }
            PatternKind::Struct { fields, .. } => {
                for f in fields {
                    self.clear_pattern_defs(&f.pattern);
                }
            }
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
            StmtExprKind::Assign { assignee, value } => {
                self.visit_expr(value);
                if let ExprKind::Var(_) = assignee.kind {
                    if let Some(def) = self.ctx.def_map.lookup_def(assignee.id) {
                        self.moved.remove(&def.id);
                    }
                } else {
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
                // Direct heap usage still requires explicit move for ownership transfer.
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
            // If/Match require special treatment at CFG level
            ExprKind::If { cond, .. } => {
                self.visit_expr(cond);
            }
            ExprKind::Match { scrutinee, .. } => {
                self.visit_place_base(scrutinee);
            }
            ExprKind::Call { callee, args } => {
                self.visit_expr(callee);
                if let Some(sig) = lookup_call_sig(expr, self.ctx) {
                    for (param, arg) in sig.params.iter().zip(args) {
                        match param.mode {
                            FunctionParamMode::In | FunctionParamMode::Inout => {
                                self.with_borrow_context(|this| this.visit_expr(arg));
                            }
                        }
                    }
                } else {
                    for arg in args {
                        self.visit_expr(arg);
                    }
                }
            }
            _ => walk_expr(self, expr),
        }
    }
}
