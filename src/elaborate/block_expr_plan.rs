//! Block expression planning for SSA lowering.
//!
//! This pass classifies statement-position expressions as either linear
//! (safe to lower without introducing control flow) or branching.

use crate::elaborate::elaborator::Elaborator;
use crate::tree::semantic as sem;

impl<'a> Elaborator<'a> {
    pub(super) fn record_block_expr_plan(&mut self, expr: &sem::ValueExpr) {
        let plan = classify_block_expr(expr);
        self.block_expr_plans.insert(expr.id, plan);
    }
}

fn classify_block_expr(expr: &sem::ValueExpr) -> sem::BlockExprPlan {
    if is_linear_value_expr(expr) {
        sem::BlockExprPlan::Linear
    } else {
        sem::BlockExprPlan::Branching
    }
}

fn is_linear_value_expr(expr: &sem::ValueExpr) -> bool {
    match &expr.kind {
        sem::ValueExprKind::Block { items, tail } => {
            items.iter().all(is_linear_block_item)
                && tail.as_deref().map(is_linear_value_expr).unwrap_or(true)
        }
        sem::ValueExprKind::UnitLit
        | sem::ValueExprKind::IntLit(_)
        | sem::ValueExprKind::BoolLit(_)
        | sem::ValueExprKind::CharLit(_) => true,
        sem::ValueExprKind::UnaryOp { expr, .. } => is_linear_value_expr(expr),
        sem::ValueExprKind::BinOp { left, right, .. } => {
            is_linear_value_expr(left) && is_linear_value_expr(right)
        }
        sem::ValueExprKind::Load { .. } => true,
        sem::ValueExprKind::Call { callee, args } => {
            is_linear_value_expr(callee) && args.iter().all(is_linear_call_arg)
        }
        sem::ValueExprKind::MethodCall { receiver, args, .. } => {
            is_linear_method_receiver(receiver) && args.iter().all(is_linear_call_arg)
        }
        sem::ValueExprKind::ClosureRef { .. } => true,
        _ => false,
    }
}

fn is_linear_call_arg(arg: &sem::CallArg) -> bool {
    match arg {
        sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
            // Only value-style arguments can remain in the linear subset.
            is_linear_value_expr(expr)
        }
        sem::CallArg::InOut { .. } | sem::CallArg::Out { .. } => false,
    }
}

fn is_linear_method_receiver(receiver: &sem::MethodReceiver) -> bool {
    match receiver {
        sem::MethodReceiver::ValueExpr(expr) => is_linear_value_expr(expr),
        sem::MethodReceiver::PlaceExpr(_) => false,
    }
}

fn is_linear_block_item(item: &sem::BlockItem) -> bool {
    match item {
        sem::BlockItem::Stmt(stmt) => is_linear_stmt(stmt),
        sem::BlockItem::Expr(expr) => is_linear_value_expr(expr),
    }
}

fn is_linear_stmt(stmt: &sem::StmtExpr) -> bool {
    match &stmt.kind {
        sem::StmtExprKind::LetBind { .. }
        | sem::StmtExprKind::VarBind { .. }
        | sem::StmtExprKind::VarDecl { .. }
        | sem::StmtExprKind::Assign { .. }
        | sem::StmtExprKind::Return { .. } => true,
        sem::StmtExprKind::While { .. }
        | sem::StmtExprKind::For { .. }
        | sem::StmtExprKind::Break
        | sem::StmtExprKind::Continue => false,
    }
}
