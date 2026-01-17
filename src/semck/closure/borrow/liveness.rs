//! Liveness for closure-bound locals.
//!
//! Computes which local bindings (that may hold captured closures) are live at
//! each CFG point so borrow conflicts are only enforced while the closure is
//! still potentially used.
use std::collections::HashSet;

use crate::resolve::DefId;
use crate::tree::cfg::{TreeCfg, TreeCfgItem, TreeCfgTerminator};
use crate::tree::normalized::{StmtExpr, StmtExprKind};
use crate::types::TypeId;

use super::collect::{
    collect_assignee_defs, collect_bind_pattern_defs, collect_expr_var_uses, collect_stmt_var_uses,
};
use crate::semck::liveness_util;

pub(super) struct ClosureLiveness {
    pub(super) live_out: Vec<HashSet<DefId>>,
    pub(super) live_after: Vec<Vec<HashSet<DefId>>>,
}

pub(super) fn analyze_closure_liveness(cfg: &TreeCfg<'_, TypeId>) -> ClosureLiveness {
    let analysis = liveness_util::analyze_liveness(
        cfg,
        |term, uses| add_terminator_uses(term, uses),
        |item, defs, uses| collect_item_defs_uses(item, defs, uses),
    );

    ClosureLiveness {
        live_out: analysis.live_out,
        live_after: analysis.live_after,
    }
}

fn add_terminator_uses(term: &TreeCfgTerminator<'_, TypeId>, uses: &mut HashSet<DefId>) {
    if let TreeCfgTerminator::If { cond, .. } = term {
        collect_expr_var_uses(cond, uses);
    }
}

fn collect_item_defs_uses(
    item: &TreeCfgItem<'_, TypeId>,
    defs: &mut HashSet<DefId>,
    uses: &mut HashSet<DefId>,
) {
    match item {
        TreeCfgItem::Stmt(stmt) => collect_stmt_defs_uses(stmt, defs, uses),
        TreeCfgItem::Expr(expr) => collect_expr_var_uses(expr, uses),
    }
}

fn collect_stmt_defs_uses(stmt: &StmtExpr, defs: &mut HashSet<DefId>, uses: &mut HashSet<DefId>) {
    collect_stmt_var_uses(stmt, uses);
    match &stmt.kind {
        StmtExprKind::LetBind { pattern, .. } | StmtExprKind::VarBind { pattern, .. } => {
            collect_bind_pattern_defs(pattern, defs);
        }
        StmtExprKind::VarDecl { .. } => {}
        StmtExprKind::Assign { assignee, .. } => {
            collect_assignee_defs(assignee, defs);
        }
        StmtExprKind::While { .. } => {}
        StmtExprKind::For { pattern, .. } => {
            collect_bind_pattern_defs(pattern, defs);
        }
        StmtExprKind::Break | StmtExprKind::Continue => {}
        StmtExprKind::Return { .. } => {}
    }
}
