use crate::ast::{
    BinaryOp, Decl, Expr, ExprKind, Function, StmtExpr, StmtExprKind, TypeExpr, TypeExprKind,
    UnaryOp, Visitor, walk_expr, walk_stmt_expr,
};
use crate::context::TypeCheckedContext;
use crate::semck::SemCheckError;
use crate::semck::util::lookup_call_sig;
use crate::typeck::type_map::resolve_type_expr;
use crate::types::Type;

pub(super) fn check(ctx: &TypeCheckedContext) -> Vec<SemCheckError> {
    let mut checker = ValueChecker::new(ctx);
    checker.check_module();
    checker.errors
}

struct ValueChecker<'a> {
    ctx: &'a TypeCheckedContext,
    errors: Vec<SemCheckError>,
    current_return_ty: Option<Type>,
}

impl<'a> ValueChecker<'a> {
    fn new(ctx: &'a TypeCheckedContext) -> Self {
        Self {
            ctx,
            errors: Vec::new(),
            current_return_ty: None,
        }
    }

    fn check_module(&mut self) {
        for decl in &self.ctx.module.decls {
            if let Decl::TypeDecl(decl) = decl {
                self.check_type_decl(decl);
            }
        }
        for decl in &self.ctx.module.decls {
            if let crate::ast::Decl::FunctionDecl(decl) = decl {
                self.check_function_sig(&decl.sig);
            }
        }
        for func in self.ctx.module.funcs() {
            self.check_function_sig(&func.sig);
            self.visit_func(func);
        }
    }

    /// Validate that a literal fits within a target integer type's bounds.
    fn check_int_range(&mut self, value: i128, min: i128, max_excl: i128, span: crate::diag::Span) {
        if value < min || value >= max_excl {
            self.errors
                .push(SemCheckError::ValueOutOfRange(value, min, max_excl, span));
        }
    }

    fn check_range_value(&mut self, value: u64, min: u64, max: u64, span: crate::diag::Span) {
        if value < min || value >= max {
            self.errors.push(SemCheckError::ValueOutOfRange(
                value as i128,
                min as i128,
                max as i128,
                span,
            ));
        }
    }

    fn check_type_expr(&mut self, ty: &TypeExpr) {
        match &ty.kind {
            TypeExprKind::Range { min, max } => {
                if min >= max {
                    self.errors
                        .push(SemCheckError::InvalidRangeBounds(*min, *max, ty.span));
                }
            }
            TypeExprKind::Array { elem_ty, .. } => self.check_type_expr(elem_ty),
            TypeExprKind::Tuple { fields } => {
                for field in fields {
                    self.check_type_expr(field);
                }
            }
            TypeExprKind::Slice { elem_ty } => self.check_type_expr(elem_ty),
            TypeExprKind::Heap { elem_ty } => self.check_type_expr(elem_ty),
            TypeExprKind::Named(_) => {}
        }
    }

    fn check_function_sig(&mut self, sig: &crate::ast::FunctionSig) {
        for param in &sig.params {
            self.check_type_expr(&param.typ);
        }
        self.check_type_expr(&sig.return_type);
    }

    fn check_type_decl(&mut self, decl: &crate::ast::TypeDecl) {
        match &decl.kind {
            crate::ast::TypeDeclKind::Alias { aliased_ty } => self.check_type_expr(aliased_ty),
            crate::ast::TypeDeclKind::Struct { fields } => {
                for field in fields {
                    self.check_type_expr(&field.ty);
                }
            }
            crate::ast::TypeDeclKind::Enum { variants } => {
                for variant in variants {
                    for payload_ty in &variant.payload {
                        self.check_type_expr(payload_ty);
                    }
                }
            }
        }
    }

    fn resolve_type(&self, ty: &TypeExpr) -> Option<Type> {
        resolve_type_expr(&self.ctx.def_map, ty).ok()
    }

    fn check_range_binding_value(&mut self, value: &Expr, ty: &Type) {
        let Type::Range { min, max } = ty else {
            return;
        };
        if let Some(lit_value) = int_lit_value(value) {
            if lit_value < 0 {
                self.errors.push(SemCheckError::ValueOutOfRange(
                    lit_value,
                    *min as i128,
                    *max as i128,
                    value.span,
                ));
            } else {
                self.check_range_value(lit_value as u64, *min, *max, value.span);
            }
        }
    }
}

impl Visitor for ValueChecker<'_> {
    fn visit_func(&mut self, func: &Function) {
        self.current_return_ty = self.resolve_type(&func.sig.return_type);
        walk_expr(self, &func.body);

        let ret_expr = match &func.body.kind {
            ExprKind::Block {
                tail: Some(tail), ..
            } => Some(tail.as_ref()),
            ExprKind::Block { tail: None, .. } => None,
            _ => Some(&func.body),
        };
        if let (Some(Type::Range { min, max }), Some(ret_expr)) =
            (&self.current_return_ty, ret_expr)
            && let Some(value) = int_lit_value(ret_expr)
        {
            if value < 0 {
                self.errors.push(SemCheckError::ValueOutOfRange(
                    value,
                    *min as i128,
                    *max as i128,
                    ret_expr.span,
                ));
            } else {
                self.check_range_value(value as u64, *min, *max, ret_expr.span);
            }
        }

        self.current_return_ty = None;
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { decl_ty, value, .. }
            | StmtExprKind::VarBind { decl_ty, value, .. } => {
                if let Some(ty) = decl_ty {
                    self.check_type_expr(ty);
                    if let Some(resolved_ty) = self.resolve_type(ty) {
                        self.check_range_binding_value(value, &resolved_ty);
                    }
                }
            }
            StmtExprKind::VarDecl { decl_ty, .. } => {
                self.check_type_expr(decl_ty);
            }
            StmtExprKind::Assign { assignee, value } => {
                if let Some(assignee_ty) = self.ctx.type_map.lookup_node_type(assignee.id) {
                    self.check_range_binding_value(value, &assignee_ty);
                }
            }
            _ => {}
        }

        walk_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::IntLit(value) => {
                // Enforce integer literal ranges based on the resolved type.
                if let Some(ty) = self.ctx.type_map.lookup_node_type(expr.id)
                    && let Type::Int { signed, bits } = ty
                {
                    let min = if signed {
                        -(1i128 << (bits as u32 - 1))
                    } else {
                        0
                    };
                    let max_excl = if signed {
                        1i128 << (bits as u32 - 1)
                    } else {
                        1i128 << (bits as u32)
                    };
                    self.check_int_range(*value as i128, min, max_excl, expr.span);
                }
            }
            ExprKind::Range { start, end } => {
                // Range bounds must be ordered (start < end).
                if start >= end {
                    self.errors
                        .push(SemCheckError::InvalidRangeBounds(*start, *end, expr.span));
                }
            }
            ExprKind::UnaryOp {
                op: UnaryOp::Neg, ..
            } => {
                if let Some(lit_value) = int_lit_value(expr)
                    && let Some(ty) = self.ctx.type_map.lookup_node_type(expr.id)
                    && let Type::Int { signed, bits } = ty
                {
                    let min = if signed {
                        -(1i128 << (bits as u32 - 1))
                    } else {
                        0
                    };
                    let max_excl = if signed {
                        1i128 << (bits as u32 - 1)
                    } else {
                        1i128 << (bits as u32)
                    };
                    self.check_int_range(lit_value, min, max_excl, expr.span);
                }
            }
            ExprKind::BinOp {
                op: BinaryOp::Div | BinaryOp::Mod,
                right,
                ..
            } => {
                // Reject constant division by zero early.
                if matches!(right.kind, ExprKind::IntLit(0)) {
                    self.errors.push(SemCheckError::DivisionByZero(right.span));
                }
            }
            ExprKind::ArrayLit {
                elem_ty: Some(elem_ty),
                ..
            } => {
                self.check_type_expr(elem_ty);
            }
            ExprKind::Call { args, .. } => {
                let param_tys = lookup_call_sig(expr, self.ctx).map(|sig| {
                    sig.params
                        .iter()
                        .map(|param| self.resolve_type(&param.typ))
                        .collect::<Vec<_>>()
                });
                if let Some(param_tys) = param_tys {
                    for (arg, param_ty) in args.iter().zip(param_tys) {
                        if let Some(param_ty) = param_ty {
                            self.check_range_binding_value(&arg.expr, &param_ty);
                        }
                    }
                }
            }
            _ => {}
        }

        walk_expr(self, expr);
    }
}

fn int_lit_value(expr: &Expr) -> Option<i128> {
    match &expr.kind {
        ExprKind::IntLit(value) => Some(*value as i128),
        ExprKind::UnaryOp {
            op: UnaryOp::Neg,
            expr,
        } => match expr.kind {
            ExprKind::IntLit(value) => Some(-(value as i128)),
            _ => None,
        },
        _ => None,
    }
}
