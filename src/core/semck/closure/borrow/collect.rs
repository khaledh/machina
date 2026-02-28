//! Def/use collection helpers for closure borrow analysis.
//!
//! These walkers are intentionally small and local to closure borrow checking,
//! so the analysis can reason about which bindings are defined or referenced
//! without pulling in the full type checker.
use std::collections::HashSet;

use crate::core::resolve::{DefId, DefTable};
use crate::core::tree::cfg::CfgItem;
use crate::core::tree::visit::{Visitor, walk_bind_pattern, walk_expr, walk_match_pattern};
use crate::core::tree::{
    BindPattern, BindPatternKind, Expr, ExprKind, MatchArm, MatchPattern, MatchPatternBinding,
    StmtExpr, StmtExprKind,
};

pub(super) fn collect_bind_pattern_defs(
    pattern: &BindPattern,
    def_table: &DefTable,
    defs: &mut HashSet<DefId>,
) {
    // Bind patterns introduce new locals; collect all def_ids.
    let mut collector = BindPatternDefCollector { defs, def_table };
    collector.visit_bind_pattern(pattern);
}

pub(super) fn collect_match_pattern_defs(
    pattern: &MatchPattern,
    def_table: &DefTable,
    defs: &mut HashSet<DefId>,
) {
    // Match patterns can also bind locals (via enum bindings).
    let mut collector = MatchPatternDefCollector { defs, def_table };
    collector.visit_match_pattern(pattern);
}

pub(super) fn collect_assignee_defs(
    assignee: &Expr,
    def_table: &DefTable,
    defs: &mut HashSet<DefId>,
) {
    if let ExprKind::Var { .. } = assignee.kind {
        defs.insert(def_table.def_id(assignee.id));
    }
}

pub(super) fn collect_item_var_uses(
    item: &CfgItem<'_>,
    def_table: &DefTable,
    uses: &mut HashSet<DefId>,
) {
    match item {
        CfgItem::Stmt(stmt) => collect_stmt_var_uses(stmt, def_table, uses),
        CfgItem::Expr(expr) => collect_expr_var_uses(expr, def_table, uses),
    }
}

pub(super) fn collect_stmt_var_uses(
    stmt: &StmtExpr,
    def_table: &DefTable,
    uses: &mut HashSet<DefId>,
) {
    // Record all variable uses appearing in a statement.
    match &stmt.kind {
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            collect_expr_var_uses(value, def_table, uses);
        }
        StmtExprKind::VarDecl { .. } => {}
        StmtExprKind::Assign {
            assignee, value, ..
        }
        | StmtExprKind::CompoundAssign {
            assignee, value, ..
        } => {
            collect_expr_var_uses(value, def_table, uses);
            collect_assignee_uses(assignee, def_table, uses);
        }
        StmtExprKind::While { cond, body } => {
            collect_expr_var_uses(cond, def_table, uses);
            collect_expr_var_uses(body, def_table, uses);
        }
        StmtExprKind::For { iter, body, .. } => {
            collect_expr_var_uses(iter, def_table, uses);
            collect_expr_var_uses(body, def_table, uses);
        }
        StmtExprKind::Defer { value } => {
            collect_expr_var_uses(value, def_table, uses);
        }
        StmtExprKind::Using { value, body, .. } => {
            collect_expr_var_uses(value, def_table, uses);
            collect_expr_var_uses(body, def_table, uses);
        }
        StmtExprKind::Break | StmtExprKind::Continue => {}
        StmtExprKind::Return { value } => {
            if let Some(value) = value {
                collect_expr_var_uses(value, def_table, uses);
            }
        }
    }
}

pub(super) fn collect_assignee_uses(
    assignee: &Expr,
    def_table: &DefTable,
    uses: &mut HashSet<DefId>,
) {
    // Assignments use indices/fields but not the base binding itself.
    match &assignee.kind {
        ExprKind::Var { .. } => {}
        ExprKind::StructField { target, .. } | ExprKind::TupleField { target, .. } => {
            collect_expr_var_uses(target, def_table, uses);
        }
        ExprKind::ArrayIndex { target, indices } => {
            collect_expr_var_uses(target, def_table, uses);
            for index in indices {
                collect_expr_var_uses(index, def_table, uses);
            }
        }
        ExprKind::Slice { target, start, end } => {
            collect_expr_var_uses(target, def_table, uses);
            if let Some(start) = start {
                collect_expr_var_uses(start, def_table, uses);
            }
            if let Some(end) = end {
                collect_expr_var_uses(end, def_table, uses);
            }
        }
        _ => collect_expr_var_uses(assignee, def_table, uses),
    }
}

pub(super) fn collect_expr_var_uses(expr: &Expr, def_table: &DefTable, uses: &mut HashSet<DefId>) {
    // Walk expressions and collect Var uses, skipping nested closures.
    let mut collector = VarUseCollector { uses, def_table };
    collector.visit_expr(expr);
}

pub(super) fn lvalue_base_def_id(expr: &Expr, def_table: &DefTable) -> Option<DefId> {
    // Strip projections/coercions to reach the base variable.
    match &expr.kind {
        ExprKind::Var { .. } => Some(def_table.def_id(expr.id)),
        ExprKind::ArrayIndex { target, .. }
        | ExprKind::TupleField { target, .. }
        | ExprKind::StructField { target, .. }
        | ExprKind::Slice { target, .. }
        | ExprKind::Coerce { expr: target, .. }
        | ExprKind::Move { expr: target }
        | ExprKind::ImplicitMove { expr: target }
        | ExprKind::Deref { expr: target }
        | ExprKind::AddrOf { expr: target } => lvalue_base_def_id(target, def_table),
        _ => None,
    }
}

struct BindPatternDefCollector<'a> {
    defs: &'a mut HashSet<DefId>,
    def_table: &'a DefTable,
}

impl Visitor for BindPatternDefCollector<'_> {
    fn visit_bind_pattern(&mut self, pattern: &BindPattern) {
        if let BindPatternKind::Name { .. } = &pattern.kind {
            self.defs.insert(self.def_table.def_id(pattern.id));
        }
        walk_bind_pattern(self, pattern);
    }
}

struct MatchPatternDefCollector<'a> {
    defs: &'a mut HashSet<DefId>,
    def_table: &'a DefTable,
}

impl Visitor for MatchPatternDefCollector<'_> {
    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        if let MatchPattern::Binding { id, .. } | MatchPattern::TypedBinding { id, .. } = pattern {
            self.defs.insert(self.def_table.def_id(*id));
        }
        if let MatchPattern::EnumVariant { bindings, .. } = pattern {
            for binding in bindings {
                self.visit_match_pattern_binding(binding);
            }
        }
        walk_match_pattern(self, pattern);
    }

    fn visit_match_pattern_binding(&mut self, binding: &MatchPatternBinding) {
        if let MatchPatternBinding::Named { id, .. } = binding {
            self.defs.insert(self.def_table.def_id(*id));
        }
    }
}

struct VarUseCollector<'a> {
    uses: &'a mut HashSet<DefId>,
    def_table: &'a DefTable,
}

impl Visitor for VarUseCollector<'_> {
    fn visit_match_arm(&mut self, arm: &MatchArm) {
        let mut defs = HashSet::new();
        collect_match_pattern_defs(&arm.pattern, self.def_table, &mut defs);
        self.visit_expr(&arm.body);
        for def_id in defs {
            self.uses.remove(&def_id);
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Var { .. } = expr.kind {
            self.uses.insert(self.def_table.def_id(expr.id));
        }
        if matches!(expr.kind, ExprKind::Closure { .. }) {
            return;
        }
        walk_expr(self, expr);
    }
}
