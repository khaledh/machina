//! Semantics
// - `move x` only valid if x is a plain variable (no projections).
// - Move tracks compound types and heap-owned values (`Type::is_move_tracked()`).
// - Any use of a moved var is an error.
// - Re‑assigning the var (`x = ...`) clears moved status.
// - `inout` use counts as use (already checked elsewhere).

use std::collections::HashSet;

use crate::analysis::dataflow::solve_forward;
use crate::ast::cfg::{AstBlockId, AstCfgBuilder, AstCfgNode, AstItem, AstTerminator};
use crate::ast::{Expr, ExprKind, Function, Pattern, PatternKind, StmtExpr, StmtExprKind};
use crate::ast::{Visitor, walk_expr};
use crate::context::TypeCheckedContext;
use crate::resolve::def_map::DefId;
use crate::semck::SemCheckError;

pub fn check(ctx: &TypeCheckedContext) -> Vec<SemCheckError> {
    let mut errors = Vec::new();
    for func in ctx.module.funcs() {
        check_func(func, ctx, &mut errors);
    }
    errors
}

fn check_func(func: &Function, ctx: &TypeCheckedContext, errors: &mut Vec<SemCheckError>) {
    let cfg = AstCfgBuilder::new().build_from_expr(&func.body);

    let empty = HashSet::new();
    solve_forward(
        &cfg,
        AstBlockId(0), // entry block is block 0 in builder
        empty.clone(), // entry state
        empty,         // bottom
        |states| {
            let mut out = HashSet::new();
            for s in states {
                out.extend(s.iter().cloned());
            }
            out
        },
        |block_id, in_state| {
            let mut visitor = MoveVisitor::new(ctx, in_state.clone(), errors);
            visitor.visit_cfg_node(&cfg.nodes[block_id.0]);
            visitor.moved
        },
    );
}

struct MoveVisitor<'a> {
    ctx: &'a TypeCheckedContext,
    moved: HashSet<DefId>,
    errors: &'a mut Vec<SemCheckError>,
}

impl<'a> MoveVisitor<'a> {
    fn new(
        ctx: &'a TypeCheckedContext,
        moved: HashSet<DefId>,
        errors: &'a mut Vec<SemCheckError>,
    ) -> Self {
        Self { ctx, moved, errors }
    }

    fn visit_cfg_node(&mut self, node: &AstCfgNode<'_>) {
        for item in &node.items {
            match item {
                AstItem::Stmt(stmt) => self.visit_stmt_expr(stmt),
                AstItem::Expr(expr) => self.visit_expr(expr),
            }
        }
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
            self.errors
                .push(SemCheckError::OwnedMoveRequired(expr.span));
        }
    }

    // Treat projections as borrowing from the base, so only check use-after-move
    // on the base variable and avoid requiring an explicit move.
    fn visit_place_base(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Var(_) => {
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
                self.check_heap_move_required(expr);
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
            _ => walk_expr(self, expr),
        }
    }
}
