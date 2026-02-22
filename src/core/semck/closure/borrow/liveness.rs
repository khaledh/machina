//! Liveness for closure-bound locals.
//!
//! Computes which local bindings (that may hold captured closures) are live at
//! each CFG point so borrow conflicts are only enforced while the closure is
//! still potentially used.
use std::collections::HashSet;

use crate::core::resolve::{DefId, DefTable};
use crate::core::tree::cfg::{Cfg, CfgItem, CfgTerminator};
use crate::core::tree::{StmtExpr, StmtExprKind};

use super::collect::{
    collect_assignee_defs, collect_bind_pattern_defs, collect_expr_var_uses, collect_stmt_var_uses,
};
use crate::core::semck::liveness_util;

pub(super) struct ClosureLiveness {
    pub(super) live_out: Vec<HashSet<DefId>>,
    pub(super) live_after: Vec<Vec<HashSet<DefId>>>,
}

pub(super) fn analyze_closure_liveness(cfg: &Cfg<'_>, def_table: &DefTable) -> ClosureLiveness {
    let analysis = liveness_util::analyze_liveness(
        cfg,
        |term, uses| add_terminator_uses(term, def_table, uses),
        |item, defs, uses| collect_item_defs_uses(item, def_table, defs, uses),
    );

    ClosureLiveness {
        live_out: analysis.live_out,
        live_after: analysis.live_after,
    }
}

fn add_terminator_uses(term: &CfgTerminator<'_>, def_table: &DefTable, uses: &mut HashSet<DefId>) {
    if let CfgTerminator::If { cond, .. } = term {
        collect_expr_var_uses(cond, def_table, uses);
    }
}

fn collect_item_defs_uses(
    item: &CfgItem<'_>,
    def_table: &DefTable,
    defs: &mut HashSet<DefId>,
    uses: &mut HashSet<DefId>,
) {
    match item {
        CfgItem::Stmt(stmt) => collect_stmt_defs_uses(stmt, def_table, defs, uses),
        CfgItem::Expr(expr) => collect_expr_var_uses(expr, def_table, uses),
    }
}

fn collect_stmt_defs_uses(
    stmt: &StmtExpr,
    def_table: &DefTable,
    defs: &mut HashSet<DefId>,
    uses: &mut HashSet<DefId>,
) {
    collect_stmt_var_uses(stmt, def_table, uses);
    match &stmt.kind {
        StmtExprKind::LetBind { pattern, .. } | StmtExprKind::VarBind { pattern, .. } => {
            collect_bind_pattern_defs(pattern, def_table, defs);
        }
        StmtExprKind::VarDecl { .. } => {}
        StmtExprKind::Assign { assignee, .. } | StmtExprKind::CompoundAssign { assignee, .. } => {
            collect_assignee_defs(assignee, def_table, defs);
        }
        StmtExprKind::While { .. } => {}
        StmtExprKind::For { pattern, .. } => {
            collect_bind_pattern_defs(pattern, def_table, defs);
        }
        StmtExprKind::Break | StmtExprKind::Continue => {}
        StmtExprKind::Return { .. } => {}
    }
}
