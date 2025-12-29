use super::*;

/// AST visitor with default traversal helpers.
///
/// Implement the methods you care about (e.g. `visit_expr`) and call the
/// corresponding `walk_*` function to recurse into children.
/// Example:
/// ```rust
/// use machina::ast::{walk_expr, Expr, Visitor};
///
/// struct MyVisitor;
/// impl Visitor for MyVisitor {
///     fn visit_expr(&mut self, expr: &Expr) {
///         // pre-visit logic here
///         walk_expr(self, expr);
///         // post-visit logic here
///     }
/// }
/// ```
pub trait Visitor {
    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr)
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        walk_stmt_expr(self, stmt)
    }

    fn visit_block_item(&mut self, item: &BlockItem) {
        walk_block_item(self, item)
    }

    fn visit_function(&mut self, func: &Function) {
        walk_function(self, func)
    }
}

pub fn walk_function<V: Visitor + ?Sized>(v: &mut V, func: &Function) {
    v.visit_expr(&func.body);
}

pub fn walk_block_item<V: Visitor + ?Sized>(v: &mut V, item: &BlockItem) {
    match item {
        BlockItem::Stmt(stmt) => v.visit_stmt_expr(stmt),
        BlockItem::Expr(expr) => v.visit_expr(expr),
    }
}

pub fn walk_stmt_expr<V: Visitor + ?Sized>(v: &mut V, stmt: &StmtExpr) {
    match &stmt.kind {
        StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
            v.visit_expr(value);
        }
        StmtExprKind::Assign { assignee, value } => {
            v.visit_expr(assignee);
            v.visit_expr(value);
        }
        StmtExprKind::While { cond, body } => {
            v.visit_expr(cond);
            v.visit_expr(body);
        }
        StmtExprKind::For { iter, body, .. } => {
            v.visit_expr(iter);
            v.visit_expr(body);
        }
    }
}

pub fn walk_expr<V: Visitor + ?Sized>(v: &mut V, expr: &Expr) {
    match &expr.kind {
        ExprKind::Block { items, tail } => {
            for item in items {
                v.visit_block_item(item);
            }
            if let Some(tail) = tail {
                v.visit_expr(tail);
            }
        }
        ExprKind::IntLit(_)
        | ExprKind::BoolLit(_)
        | ExprKind::CharLit(_)
        | ExprKind::StringLit { .. }
        | ExprKind::UnitLit
        | ExprKind::Var(_)
        | ExprKind::Range { .. } => {}
        ExprKind::ArrayLit { elems, .. } => {
            for elem in elems {
                v.visit_expr(elem);
            }
        }
        ExprKind::TupleLit(fields) => {
            for field in fields {
                v.visit_expr(field);
            }
        }
        ExprKind::StructLit { fields, .. } => {
            for field in fields {
                v.visit_expr(&field.value);
            }
        }
        ExprKind::EnumVariant { payload, .. } => {
            for expr in payload {
                v.visit_expr(expr);
            }
        }
        ExprKind::StructUpdate { target, fields } => {
            v.visit_expr(target);
            for field in fields {
                v.visit_expr(&field.value);
            }
        }
        ExprKind::BinOp { left, right, .. } => {
            v.visit_expr(left);
            v.visit_expr(right);
        }
        ExprKind::UnaryOp { expr, .. } => {
            v.visit_expr(expr);
        }
        ExprKind::ArrayIndex { target, indices } => {
            v.visit_expr(target);
            for index in indices {
                v.visit_expr(index);
            }
        }
        ExprKind::TupleField { target, .. } => {
            v.visit_expr(target);
        }
        ExprKind::StructField { target, .. } => {
            v.visit_expr(target);
        }
        ExprKind::If {
            cond,
            then_body,
            else_body,
        } => {
            v.visit_expr(cond);
            v.visit_expr(then_body);
            v.visit_expr(else_body);
        }
        ExprKind::Slice { target, start, end } => {
            v.visit_expr(target);
            if let Some(start) = start {
                v.visit_expr(start);
            }
            if let Some(end) = end {
                v.visit_expr(end);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            v.visit_expr(scrutinee);
            for arm in arms {
                v.visit_expr(&arm.body);
            }
        }
        ExprKind::Call { callee, args } => {
            v.visit_expr(callee);
            for arg in args {
                v.visit_expr(arg);
            }
        }
    }
}
