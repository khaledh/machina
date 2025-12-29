use crate::ast::{
    BinaryOp, Expr, ExprKind, Function, StmtExpr, StmtExprKind, StringTag, TypeExpr, TypeExprKind,
    Visitor, walk_expr, walk_stmt_expr,
};
use crate::context::TypeCheckedContext;
use crate::semck::SemCheckError;
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
            match decl {
                crate::ast::Decl::TypeDecl(decl) => self.check_type_decl(decl),
                _ => {}
            }
        }
        for decl in &self.ctx.module.decls {
            if let crate::ast::Decl::FunctionDecl(decl) = decl {
                self.check_function_sig(&decl.sig);
            }
        }
        for func in self.ctx.module.funcs() {
            self.check_function_sig(&func.sig);
            self.visit_function(func);
        }
    }

    /// Validate that a literal fits within a target integer type's bounds.
    fn check_int_range(
        &mut self,
        value: u64,
        min: u64,
        max_excl: u64,
        span: crate::diag::Span,
    ) {
        if value < min || value >= max_excl {
            self.errors
                .push(SemCheckError::ValueOutOfRange(value, min, max_excl, span));
        }
    }

    fn check_range_value(
        &mut self,
        value: u64,
        min: u64,
        max: u64,
        span: crate::diag::Span,
    ) {
        if value < min || value >= max {
            self.errors
                .push(SemCheckError::ValueOutOfRange(value, min, max, span));
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
        if let ExprKind::IntLit(lit_value) = value.kind {
            self.check_range_value(lit_value, *min, *max, value.span);
        }
    }

    fn lookup_call_sig(&self, call_expr: &Expr) -> Option<&crate::ast::FunctionSig> {
        let def_id = self.ctx.type_map.lookup_call_def(call_expr.id)?;
        for func in self.ctx.module.funcs() {
            let def = self.ctx.def_map.lookup_def(func.id)?;
            if def.id == def_id {
                return Some(&func.sig);
            }
        }
        for decl in self.ctx.module.func_decls() {
            let def = self.ctx.def_map.lookup_def(decl.id)?;
            if def.id == def_id {
                return Some(&decl.sig);
            }
        }
        None
    }
}

impl Visitor for ValueChecker<'_> {
    fn visit_function(&mut self, func: &Function) {
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
        {
            if let ExprKind::IntLit(value) = ret_expr.kind {
                self.check_range_value(value, *min, *max, ret_expr.span);
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
                if let Some(ty) = self.ctx.type_map.lookup_node_type(expr.id) {
                    match ty {
                        Type::UInt8 => {
                            self.check_int_range(*value, 0, u8::MAX as u64 + 1, expr.span)
                        }
                        Type::UInt32 => {
                            self.check_int_range(*value, 0, u32::MAX as u64 + 1, expr.span)
                        }
                        Type::UInt64 => {}
                        _ => {}
                    }
                }
            }
            ExprKind::Range { start, end } => {
                // Range bounds must be ordered (start < end).
                if start >= end {
                    self.errors
                        .push(SemCheckError::InvalidRangeBounds(*start, *end, expr.span));
                }
            }
            ExprKind::BinOp {
                op: BinaryOp::Div,
                right,
                ..
            } => {
                // Reject constant division by zero early.
                if matches!(right.kind, ExprKind::IntLit(0)) {
                    self.errors.push(SemCheckError::DivisionByZero(right.span));
                }
            }
            ExprKind::ArrayIndex { target, .. } => {
                // String indexing is only allowed on ASCII literals for now.
                if let Some(Type::String) = self.ctx.type_map.lookup_node_type(target.id) {
                    if !matches!(
                        target.kind,
                        ExprKind::StringLit {
                            tag: StringTag::Ascii,
                            ..
                        }
                    ) {
                        self.errors
                            .push(SemCheckError::StringIndexNonAscii(target.span));
                    }
                }
            }
            ExprKind::ArrayLit {
                elem_ty: Some(elem_ty),
                ..
            } => {
                self.check_type_expr(elem_ty);
            }
            ExprKind::Call { args, .. } => {
                let param_tys = self.lookup_call_sig(expr).map(|sig| {
                    sig.params
                        .iter()
                        .map(|param| self.resolve_type(&param.typ))
                        .collect::<Vec<_>>()
                });
                if let Some(param_tys) = param_tys {
                    for (arg, param_ty) in args.iter().zip(param_tys.into_iter()) {
                        if let Some(param_ty) = param_ty {
                            self.check_range_binding_value(arg, &param_ty);
                        }
                    }
                }
            }
            _ => {}
        }

        walk_expr(self, expr);
    }
}
