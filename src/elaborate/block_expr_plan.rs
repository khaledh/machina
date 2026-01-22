//! Block expression planning for SSA lowering.
//!
//! This pass classifies statement-position expressions as either linear
//! (safe to lower without introducing control flow) or branching.

use crate::elaborate::elaborator::Elaborator;
use crate::tree::{NodeId, semantic as sem};

impl<'a> Elaborator<'a> {
    pub(super) fn record_block_expr_plan(&mut self, expr: &sem::ValueExpr) {
        let plan = self.classify_block_expr(expr);
        self.block_expr_plans.insert(expr.id, plan);
    }

    fn classify_block_expr(&self, expr: &sem::ValueExpr) -> sem::BlockExprPlan {
        if self.is_linear_value_expr(expr) {
            sem::BlockExprPlan::Linear
        } else {
            sem::BlockExprPlan::Branching
        }
    }

    fn is_linear_value_expr(&self, expr: &sem::ValueExpr) -> bool {
        match &expr.kind {
            sem::ValueExprKind::Block { items, tail } => {
                items.iter().all(|item| self.is_linear_block_item(item))
                    && tail
                        .as_deref()
                        .map(|tail| self.is_linear_value_expr(tail))
                        .unwrap_or(true)
            }
            sem::ValueExprKind::UnitLit
            | sem::ValueExprKind::IntLit(_)
            | sem::ValueExprKind::BoolLit(_)
            | sem::ValueExprKind::CharLit(_) => true,
            sem::ValueExprKind::UnaryOp { expr, .. } => self.is_linear_value_expr(expr),
            sem::ValueExprKind::BinOp { left, right, .. } => {
                self.is_linear_value_expr(left) && self.is_linear_value_expr(right)
            }
            sem::ValueExprKind::Load { .. } => true,
            sem::ValueExprKind::Call { callee, args } => {
                self.is_linear_value_expr(callee)
                    && args.iter().all(|arg| self.is_linear_call_arg(arg))
                    && self.is_linear_call_plan(expr.id, false)
            }
            sem::ValueExprKind::MethodCall { receiver, args, .. } => {
                self.is_linear_method_receiver(receiver)
                    && args.iter().all(|arg| self.is_linear_call_arg(arg))
                    && self.is_linear_call_plan(expr.id, true)
            }
            sem::ValueExprKind::ClosureRef { .. } => true,
            _ => false,
        }
    }

    fn is_linear_call_arg(&self, arg: &sem::CallArg) -> bool {
        match arg {
            sem::CallArg::In { expr, .. } | sem::CallArg::Sink { expr, .. } => {
                // Only value-style arguments can remain in the linear subset.
                self.is_linear_value_expr(expr)
            }
            sem::CallArg::InOut { .. } | sem::CallArg::Out { .. } => false,
        }
    }

    fn is_linear_method_receiver(&self, receiver: &sem::MethodReceiver) -> bool {
        match receiver {
            sem::MethodReceiver::ValueExpr(expr) => self.is_linear_value_expr(expr),
            sem::MethodReceiver::PlaceExpr(_) => false,
        }
    }

    fn is_linear_block_item(&self, item: &sem::BlockItem) -> bool {
        match item {
            sem::BlockItem::Stmt(stmt) => Self::is_linear_stmt(stmt),
            sem::BlockItem::Expr(expr) => self.is_linear_value_expr(expr),
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

    fn is_linear_call_plan(&self, call_id: NodeId, has_receiver: bool) -> bool {
        let Some(plan) = self.type_map.lookup_call_plan(call_id) else {
            return false;
        };

        if plan.has_receiver != has_receiver {
            return false;
        }

        if !matches!(plan.target, sem::CallTarget::Direct(_)) {
            return false;
        }

        plan.args
            .iter()
            .all(|arg| matches!(arg, sem::ArgLowering::Direct(_)))
    }
}
