//! Def/use collection helpers for closure borrow analysis.
//!
//! These walkers are intentionally small and local to closure borrow checking,
//! so the analysis can reason about which bindings are defined or referenced
//! without pulling in the full type checker.
use std::collections::HashSet;

use crate::resolve::DefId;
use crate::tree::normalized::{
    BindPattern, BindPatternKind, Expr, ExprKind, MatchPattern, MatchPatternBinding, StmtExpr,
    StmtExprKind,
};
use crate::tree::visit::{Visitor, walk_bind_pattern, walk_expr, walk_match_pattern};
use crate::types::TypeId;

pub(super) fn collect_bind_pattern_defs(pattern: &BindPattern, defs: &mut HashSet<DefId>) {
    // Bind patterns introduce new locals; collect all def_ids.
    let mut collector = BindPatternDefCollector { defs };
    collector.visit_bind_pattern(pattern);
}

pub(super) fn collect_match_pattern_defs(pattern: &MatchPattern, defs: &mut HashSet<DefId>) {
    // Match patterns can also bind locals (via enum bindings).
    let mut collector = MatchPatternDefCollector { defs };
    collector.visit_match_pattern(pattern);
}

pub(super) fn collect_assignee_defs(assignee: &Expr, defs: &mut HashSet<DefId>) {
    if let ExprKind::Var { def_id, .. } = assignee.kind {
        defs.insert(def_id);
    }
}

pub(super) fn collect_item_var_uses(
    item: &crate::tree::cfg::TreeCfgItem<'_, TypeId>,
    uses: &mut HashSet<DefId>,
) {
    match item {
        crate::tree::cfg::TreeCfgItem::Stmt(stmt) => collect_stmt_var_uses(stmt, uses),
        crate::tree::cfg::TreeCfgItem::Expr(expr) => collect_expr_var_uses(expr, uses),
    }
}

pub(super) fn collect_stmt_var_uses(stmt: &StmtExpr, uses: &mut HashSet<DefId>) {
    // Record all variable uses appearing in a statement.
    match &stmt.kind {
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            collect_expr_var_uses(value, uses);
        }
        StmtExprKind::VarDecl { .. } => {}
        StmtExprKind::Assign {
            assignee, value, ..
        } => {
            collect_expr_var_uses(value, uses);
            collect_assignee_uses(assignee, uses);
        }
        StmtExprKind::While { cond, body } => {
            collect_expr_var_uses(cond, uses);
            collect_expr_var_uses(body, uses);
        }
        StmtExprKind::For { iter, body, .. } => {
            collect_expr_var_uses(iter, uses);
            collect_expr_var_uses(body, uses);
        }
        StmtExprKind::Break | StmtExprKind::Continue => {}
        StmtExprKind::Return { value } => {
            if let Some(value) = value {
                collect_expr_var_uses(value, uses);
            }
        }
    }
}

pub(super) fn collect_assignee_uses(assignee: &Expr, uses: &mut HashSet<DefId>) {
    // Assignments use indices/fields but not the base binding itself.
    match &assignee.kind {
        ExprKind::Var { .. } => {}
        ExprKind::StructField { target, .. } | ExprKind::TupleField { target, .. } => {
            collect_expr_var_uses(target, uses);
        }
        ExprKind::ArrayIndex { target, indices } => {
            collect_expr_var_uses(target, uses);
            for index in indices {
                collect_expr_var_uses(index, uses);
            }
        }
        ExprKind::Slice { target, start, end } => {
            collect_expr_var_uses(target, uses);
            if let Some(start) = start {
                collect_expr_var_uses(start, uses);
            }
            if let Some(end) = end {
                collect_expr_var_uses(end, uses);
            }
        }
        _ => collect_expr_var_uses(assignee, uses),
    }
}

pub(super) fn collect_expr_var_uses(expr: &Expr, uses: &mut HashSet<DefId>) {
    // Walk expressions and collect Var uses, skipping nested closures.
    let mut collector = VarUseCollector { uses };
    collector.visit_expr(expr);
}

pub(super) fn lvalue_base_def_id(expr: &Expr) -> Option<DefId> {
    // Strip projections/coercions to reach the base variable.
    match &expr.kind {
        ExprKind::Var { def_id, .. } => Some(*def_id),
        ExprKind::ArrayIndex { target, .. }
        | ExprKind::TupleField { target, .. }
        | ExprKind::StructField { target, .. }
        | ExprKind::Slice { target, .. }
        | ExprKind::Coerce { expr: target, .. }
        | ExprKind::Move { expr: target }
        | ExprKind::ImplicitMove { expr: target }
        | ExprKind::Deref { expr: target }
        | ExprKind::AddrOf { expr: target } => lvalue_base_def_id(target),
        _ => None,
    }
}

struct BindPatternDefCollector<'a> {
    defs: &'a mut HashSet<DefId>,
}

impl Visitor<DefId, TypeId> for BindPatternDefCollector<'_> {
    fn visit_bind_pattern(&mut self, pattern: &BindPattern) {
        if let BindPatternKind::Name { def_id, .. } = &pattern.kind {
            self.defs.insert(*def_id);
        }
        walk_bind_pattern(self, pattern);
    }
}

struct MatchPatternDefCollector<'a> {
    defs: &'a mut HashSet<DefId>,
}

impl Visitor<DefId, TypeId> for MatchPatternDefCollector<'_> {
    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        if let MatchPattern::EnumVariant { bindings, .. } = pattern {
            for binding in bindings {
                self.visit_match_pattern_binding(binding);
            }
        }
        walk_match_pattern(self, pattern);
    }

    fn visit_match_pattern_binding(&mut self, binding: &MatchPatternBinding) {
        if let MatchPatternBinding::Named { def_id, .. } = binding {
            self.defs.insert(*def_id);
        }
    }
}

struct VarUseCollector<'a> {
    uses: &'a mut HashSet<DefId>,
}

impl Visitor<DefId, TypeId> for VarUseCollector<'_> {
    fn visit_match_arm(&mut self, arm: &crate::tree::normalized::MatchArm) {
        let mut defs = HashSet::new();
        collect_match_pattern_defs(&arm.pattern, &mut defs);
        self.visit_expr(&arm.body);
        for def_id in defs {
            self.uses.remove(&def_id);
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Var { def_id, .. } = expr.kind {
            self.uses.insert(def_id);
        }
        if matches!(expr.kind, ExprKind::Closure { .. }) {
            return;
        }
        walk_expr(self, expr);
    }
}
