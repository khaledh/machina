//! Call and operator obligation collection.

use super::*;

impl<'a> ConstraintCollector<'a> {
    pub(super) fn collect_call(&mut self, call_expr: &Expr, callee: &Expr, args: &[CallArg]) {
        let callee_ty = self.collect_expr(callee, None);
        let arg_terms = args
            .iter()
            .map(|arg| self.collect_expr(&arg.expr, None))
            .collect::<Vec<_>>();
        let ret_ty = self.node_term(call_expr.id);
        let callee_kind = match &callee.kind {
            ExprKind::Var { ident } => {
                if let Some(def_id) = self.lookup_def_id(callee.id)
                    && let Some(def) = self.ctx.def_table.lookup_def(def_id)
                {
                    if matches!(def.kind, DefKind::FuncDef { .. } | DefKind::FuncDecl { .. }) {
                        CallCallee::NamedFunction {
                            def_id,
                            name: ident.clone(),
                        }
                    } else {
                        CallCallee::Dynamic { expr_id: callee.id }
                    }
                } else {
                    CallCallee::Dynamic { expr_id: callee.id }
                }
            }
            _ => CallCallee::Dynamic { expr_id: callee.id },
        };

        self.out.call_obligations.push(CallObligation {
            call_node: call_expr.id,
            span: call_expr.span,
            caller_def_id: self.current_callable_def_id(),
            callee: callee_kind,
            callee_ty: Some(callee_ty),
            receiver: None,
            arg_terms,
            ret_ty,
        });
    }

    pub(super) fn collect_binop_constraints(&mut self, expr: &Expr, left_ty: Type, right_ty: Type) {
        let expr_ty = self.node_term(expr.id);
        let ExprKind::BinOp { op, .. } = &expr.kind else {
            return;
        };
        self.out.expr_obligations.push(ExprObligation::BinOp {
            expr_id: expr.id,
            op: *op,
            left: left_ty.clone(),
            right: right_ty.clone(),
            result: expr_ty.clone(),
            span: expr.span,
        });
        match op {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Mod
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::BitAnd
            | BinaryOp::Shl
            | BinaryOp::Shr => {
                self.push_eq(
                    left_ty.clone(),
                    right_ty,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
                self.push_eq(expr_ty, left_ty, ConstraintReason::Expr(expr.id, expr.span));
            }
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::LtEq
            | BinaryOp::GtEq => {
                self.push_eq(
                    left_ty,
                    right_ty,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
                self.push_eq(
                    expr_ty,
                    Type::Bool,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                self.push_eq(
                    left_ty,
                    Type::Bool,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
                self.push_eq(
                    right_ty,
                    Type::Bool,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
                self.push_eq(
                    expr_ty,
                    Type::Bool,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
        }
    }
}
