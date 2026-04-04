use crate::core::ast::visit::{Visitor, walk_expr, walk_func_def, walk_method_def, walk_stmt_expr};
use crate::core::ast::{
    ArrayLitInit, ClosureDef, Expr, ExprKind, FuncDecl, FuncDef, MethodDecl, MethodDef, StaticDef,
    StmtExpr, StmtExprKind,
};
use crate::core::context::NormalizedContext;
use crate::core::semck::{SEK, SemCheckError};
use crate::core::types::Type;

pub(super) fn check(ctx: &NormalizedContext) -> Vec<SemCheckError> {
    let mut checker = BorrowEscapeChecker::new(ctx);
    checker.visit_module(&ctx.module);
    checker.errors
}

struct BorrowEscapeChecker<'a> {
    ctx: &'a NormalizedContext,
    errors: Vec<SemCheckError>,
}

impl<'a> BorrowEscapeChecker<'a> {
    fn new(ctx: &'a NormalizedContext) -> Self {
        Self {
            ctx,
            errors: Vec::new(),
        }
    }

    fn callable_return_contains_borrow(&self, def_node_id: crate::core::ast::NodeId) -> bool {
        self.lookup_def_type(def_node_id)
            .and_then(|ty| match ty {
                Type::Fn { ret_ty, .. } => Some(ret_ty.contains_borrow()),
                _ => None,
            })
            .unwrap_or(false)
    }

    fn static_contains_borrow(&self, static_def: &StaticDef) -> bool {
        self.lookup_def_type(static_def.id)
            .is_some_and(|ty| ty.contains_borrow())
    }

    fn lookup_def_type(&self, def_node_id: crate::core::ast::NodeId) -> Option<Type> {
        let def_id = self.ctx.def_table.lookup_node_def_id(def_node_id)?;
        let def = self.ctx.def_table.lookup_def(def_id)?;
        self.ctx.type_map.lookup_def_type(def)
    }

    fn expr_contains_borrow(&self, expr: &Expr) -> bool {
        self.ctx
            .type_map
            .lookup_node_type(expr.id)
            .is_some_and(|ty| ty.contains_borrow())
    }

    fn record_store_if_borrow(&mut self, expr: &Expr) {
        if self.expr_contains_borrow(expr) {
            self.errors.push(SEK::BorrowEscapeStore.at(expr.span));
        }
    }

    fn record_callable_return_if_needed(
        &mut self,
        def_node_id: crate::core::ast::NodeId,
        span: crate::core::diag::Span,
    ) {
        if self.callable_return_contains_borrow(def_node_id) {
            self.errors.push(SEK::BorrowEscapeReturn.at(span));
        }
    }
}

impl Visitor for BorrowEscapeChecker<'_> {
    fn visit_func_decl(&mut self, func_decl: &FuncDecl) {
        self.record_callable_return_if_needed(func_decl.id, func_decl.sig.ret_ty_expr.span);
    }

    fn visit_func_def(&mut self, func_def: &FuncDef) {
        self.record_callable_return_if_needed(func_def.id, func_def.sig.ret_ty_expr.span);
        walk_func_def(self, func_def);
    }

    fn visit_method_decl(&mut self, method_decl: &MethodDecl) {
        self.record_callable_return_if_needed(method_decl.id, method_decl.sig.ret_ty_expr.span);
    }

    fn visit_method_def(&mut self, method_def: &MethodDef) {
        self.record_callable_return_if_needed(method_def.id, method_def.sig.ret_ty_expr.span);
        walk_method_def(self, method_def);
    }

    fn visit_closure_def(&mut self, closure_def: &ClosureDef) {
        self.record_callable_return_if_needed(closure_def.id, closure_def.sig.return_ty.span);
    }

    fn visit_static_def(&mut self, static_def: &StaticDef) {
        if self.static_contains_borrow(static_def) {
            let span = static_def
                .ty
                .as_ref()
                .map(|ty| ty.span)
                .unwrap_or(static_def.span);
            self.errors.push(SEK::BorrowEscapeStatic.at(span));
        }
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        if let StmtExprKind::Assign {
            assignee, value, ..
        } = &stmt.kind
            && self.expr_contains_borrow(value)
            && !matches!(assignee.kind, ExprKind::Var { .. })
        {
            self.errors.push(SEK::BorrowEscapeStore.at(value.span));
        }

        walk_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::ArrayLit { init, .. } => match init {
                ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.record_store_if_borrow(elem);
                    }
                }
                ArrayLitInit::Repeat(elem, _) => self.record_store_if_borrow(elem),
            },
            ExprKind::TupleLit(fields) => {
                for field in fields {
                    self.record_store_if_borrow(field);
                }
            }
            ExprKind::StructLit { fields, .. } => {
                for field in fields {
                    self.record_store_if_borrow(&field.value);
                }
            }
            ExprKind::StructUpdate { fields, .. } => {
                for field in fields {
                    self.record_store_if_borrow(&field.value);
                }
            }
            ExprKind::EnumVariant { payload, .. } => {
                for elem in payload {
                    self.record_store_if_borrow(elem);
                }
            }
            _ => {}
        }

        walk_expr(self, expr);
    }
}
