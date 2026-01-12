use crate::ast::stage::HirDef;
use crate::ast::visit::{Visitor, walk_expr, walk_stmt_expr};
use crate::context::TypeCheckedContext;
use crate::hir::model::{ArrayLitInit, Expr, ExprKind, FuncDef, StmtExpr, StmtExprKind};
use crate::semck::SemCheckError;
use crate::types::Type;

pub(super) fn check(ctx: &TypeCheckedContext) -> Vec<SemCheckError> {
    let mut checker = SliceEscapeChecker::new(ctx);
    checker.visit_module(&ctx.module);
    checker.errors
}

struct SliceEscapeChecker<'a> {
    ctx: &'a TypeCheckedContext,
    errors: Vec<SemCheckError>,
}

impl<'a> SliceEscapeChecker<'a> {
    fn new(ctx: &'a TypeCheckedContext) -> Self {
        Self {
            ctx,
            errors: Vec::new(),
        }
    }

    fn is_slice_expr(&self, expr: &Expr) -> bool {
        matches!(
            self.ctx.type_map.lookup_node_type(expr.id),
            Some(Type::Slice { .. })
        )
    }

    fn record_slice_store(&mut self, span: crate::diag::Span) {
        self.errors.push(SemCheckError::SliceEscapeStore(span));
    }

    fn record_slice_return(&mut self, span: crate::diag::Span) {
        self.errors.push(SemCheckError::SliceEscapeReturn(span));
    }

    fn check_slice_value(&mut self, expr: &Expr) {
        if self.is_slice_expr(expr) {
            self.record_slice_store(expr.span);
        }
    }

    fn return_expr<'b>(&self, func_def: &'b FuncDef) -> Option<&'b Expr> {
        match &func_def.body.kind {
            ExprKind::Block { tail, .. } => tail.as_deref(),
            _ => Some(&func_def.body),
        }
    }
}

impl Visitor<HirDef> for SliceEscapeChecker<'_> {
    fn visit_func_def(&mut self, func_def: &FuncDef) {
        self.visit_expr(&func_def.body);

        if let Some(ret_expr) = self.return_expr(func_def)
            && self.is_slice_expr(ret_expr)
        {
            self.record_slice_return(ret_expr.span);
        }
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        if let StmtExprKind::Assign { assignee, value } = &stmt.kind
            && self.is_slice_expr(value)
            && !matches!(assignee.kind, ExprKind::Var { .. })
        {
            self.record_slice_store(value.span);
        }

        walk_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::ArrayLit { init, .. } => match init {
                ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.check_slice_value(elem);
                    }
                }
                ArrayLitInit::Repeat(elem, _) => self.check_slice_value(elem),
            },
            ExprKind::TupleLit(fields) => {
                for field in fields {
                    self.check_slice_value(field);
                }
            }
            ExprKind::StructLit { fields, .. } => {
                for field in fields {
                    self.check_slice_value(&field.value);
                }
            }
            ExprKind::StructUpdate { fields, .. } => {
                for field in fields {
                    self.check_slice_value(&field.value);
                }
            }
            ExprKind::EnumVariant { payload, .. } => {
                for elem in payload {
                    self.check_slice_value(elem);
                }
            }
            _ => {}
        }

        walk_expr(self, expr);
    }
}
