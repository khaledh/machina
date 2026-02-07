use crate::context::NormalizedContext;
use crate::diag::Span;
use crate::resolve::DefId;
use crate::semck::SemCheckError;
use crate::tree::RefinementKind;
use crate::tree::normalized::{
    BinaryOp, BindPatternKind, Expr, ExprKind, FuncDef, FunctionSig, StmtExpr, StmtExprKind,
    TypeDef, TypeDefKind, TypeExpr, TypeExprKind, UnaryOp,
};
use crate::tree::visit::{Visitor, walk_expr, walk_stmt_expr};
use crate::typecheck::type_map::resolve_type_expr;
use crate::types::{Type, TypeId};
use std::collections::HashMap;

pub(super) fn check(ctx: &NormalizedContext) -> Vec<SemCheckError> {
    let mut checker = ValueChecker::new(ctx);
    checker.check_module();
    checker.errors
}

struct ValueChecker<'a> {
    ctx: &'a NormalizedContext,
    errors: Vec<SemCheckError>,
    return_stack: Vec<Type>,
    const_env: Vec<HashMap<DefId, i128>>,
}

impl<'a> ValueChecker<'a> {
    fn new(ctx: &'a NormalizedContext) -> Self {
        Self {
            ctx,
            errors: Vec::new(),
            return_stack: Vec::new(),
            const_env: Vec::new(),
        }
    }

    fn push_return_ty(&mut self, ty: Type) {
        self.return_stack.push(ty);
    }

    fn pop_return_ty(&mut self) {
        self.return_stack.pop();
    }

    fn current_return_ty(&self) -> Option<&Type> {
        self.return_stack.last()
    }

    fn push_const_scope(&mut self) {
        self.const_env.push(HashMap::new());
    }

    fn pop_const_scope(&mut self) {
        self.const_env.pop();
    }

    fn set_const(&mut self, def_id: DefId, value: Option<i128>) {
        let Some(scope) = self.const_env.last_mut() else {
            return;
        };
        match value {
            Some(value) => {
                scope.insert(def_id, value);
            }
            None => {
                scope.remove(&def_id);
            }
        }
    }

    fn lookup_const(&self, def_id: DefId) -> Option<i128> {
        for scope in self.const_env.iter().rev() {
            if let Some(value) = scope.get(&def_id) {
                return Some(*value);
            }
        }
        None
    }

    fn const_int_value(&self, expr: &Expr) -> Option<i128> {
        match &expr.kind {
            ExprKind::IntLit(value) => Some(*value as i128),
            ExprKind::UnaryOp {
                op: UnaryOp::Neg,
                expr,
            } => self.const_int_value(expr).map(|value| -value),
            ExprKind::Var { def_id, .. } => self.lookup_const(*def_id),
            ExprKind::Move { expr } => self.const_int_value(expr),
            _ => None,
        }
    }

    fn check_module(&mut self) {
        for type_def in &self.ctx.module.type_defs() {
            self.check_type_def(type_def);
        }
        for func_decl in &self.ctx.module.func_decls() {
            self.check_function_sig(&func_decl.sig);
        }
        for func_def in self.ctx.module.func_defs() {
            self.check_function_sig(&func_def.sig);
            self.visit_func_def(func_def);
        }
    }

    /// Validate that a literal fits within a target integer type's bounds.
    fn check_int_range(&mut self, value: i128, min: i128, max_excl: i128, span: Span) {
        if value < min || value >= max_excl {
            self.errors
                .push(SemCheckError::ValueOutOfRange(value, min, max_excl, span));
        }
    }

    fn check_int_refinements(&mut self, value: i128, ty: &Type, span: Span) {
        let Type::Int {
            bounds, nonzero, ..
        } = ty
        else {
            return;
        };
        if let Some(bounds) = bounds {
            self.check_int_range(value, bounds.min, bounds.max_excl, span);
        }
        if *nonzero && value == 0 {
            self.errors
                .push(SemCheckError::ValueNotNonZero(value, span));
        }
    }

    fn check_type_expr(&mut self, ty: &TypeExpr) {
        self.run_type_rules(ty);
        match &ty.kind {
            TypeExprKind::Infer => {}
            TypeExprKind::Refined { base_ty_expr, .. } => {
                self.check_type_expr(base_ty_expr);
            }
            TypeExprKind::Array { elem_ty_expr, .. } => self.check_type_expr(elem_ty_expr),
            TypeExprKind::Tuple { field_ty_exprs } => {
                for field in field_ty_exprs {
                    self.check_type_expr(field);
                }
            }
            TypeExprKind::Slice { elem_ty_expr } => self.check_type_expr(elem_ty_expr),
            TypeExprKind::Heap { elem_ty_expr } => self.check_type_expr(elem_ty_expr),
            TypeExprKind::Ref { elem_ty_expr, .. } => self.check_type_expr(elem_ty_expr),
            TypeExprKind::Named { type_args, .. } => {
                for arg in type_args {
                    self.check_type_expr(arg);
                }
            }
            TypeExprKind::Fn {
                params,
                ret_ty_expr,
            } => {
                for param in params {
                    self.check_type_expr(&param.ty_expr);
                }
                self.check_type_expr(ret_ty_expr);
            }
        }
    }

    fn check_function_sig(&mut self, sig: &FunctionSig) {
        for param in &sig.params {
            self.check_type_expr(&param.typ);
        }
        self.check_type_expr(&sig.ret_ty_expr);
    }

    fn check_type_def(&mut self, def: &TypeDef) {
        match &def.kind {
            TypeDefKind::Alias { aliased_ty } => self.check_type_expr(aliased_ty),
            TypeDefKind::Struct { fields } => {
                for field in fields {
                    self.check_type_expr(&field.ty);
                }
            }
            TypeDefKind::Enum { variants } => {
                for variant in variants {
                    for payload_ty in &variant.payload {
                        self.check_type_expr(payload_ty);
                    }
                }
            }
        }
    }

    fn resolve_type(&self, ty: &TypeExpr) -> Option<Type> {
        resolve_type_expr(&self.ctx.def_table, &self.ctx.module, ty).ok()
    }

    fn check_range_binding_value(&mut self, value: &Expr, ty: &Type) {
        if let Some(const_value) = self.const_int_value(value) {
            self.check_int_refinements(const_value, ty, value.span);
        }
    }

    fn check_return_value_range(&mut self, expr: &Expr) {
        let Some(return_ty) = self.current_return_ty().cloned() else {
            return;
        };
        if let Some(value) = int_lit_value(expr) {
            self.check_int_refinements(value, &return_ty, expr.span);
        }
    }

    fn run_type_rules(&mut self, ty: &TypeExpr) {
        self.rule_refined_type(ty);
    }

    fn run_expr_rules(&mut self, expr: &Expr) {
        self.rule_int_literal_bounds(expr);
        self.rule_range_bounds(expr);
        self.rule_div_by_zero(expr);
        self.rule_array_lit_elem_ty(expr);
        self.rule_call_arg_ranges(expr);
    }

    fn run_stmt_rules(&mut self, stmt: &StmtExpr) {
        self.rule_stmt_type_exprs(stmt);
        self.rule_stmt_range_bindings(stmt);
        self.rule_stmt_const_env(stmt);
        self.rule_stmt_return_ranges(stmt);
    }

    fn rule_refined_type(&mut self, ty: &TypeExpr) {
        let TypeExprKind::Refined { refinements, .. } = &ty.kind else {
            return;
        };
        for refinement in refinements {
            if let RefinementKind::Bounds { min, max } = refinement
                && min >= max
            {
                self.errors
                    .push(SemCheckError::InvalidRangeBounds(*min, *max, ty.span));
            }
        }
    }

    fn rule_int_literal_bounds(&mut self, expr: &Expr) {
        let Some(lit_value) = int_lit_value(expr) else {
            return;
        };
        let Type::Int { signed, bits, .. } = *self.ctx.type_map.type_table().get(expr.ty) else {
            return;
        };
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

    fn rule_range_bounds(&mut self, expr: &Expr) {
        let ExprKind::Range { start, end } = &expr.kind else {
            return;
        };
        let (Some(start), Some(end)) = (self.const_int_value(start), self.const_int_value(end))
        else {
            return;
        };
        if start < 0 || end < 0 {
            return;
        }
        if start >= end {
            self.errors
                .push(SemCheckError::InvalidRangeBounds(start, end, expr.span));
        }
    }

    fn rule_div_by_zero(&mut self, expr: &Expr) {
        let ExprKind::BinOp {
            op: BinaryOp::Div | BinaryOp::Mod,
            right,
            ..
        } = &expr.kind
        else {
            return;
        };
        if self.const_int_value(right) == Some(0) {
            self.errors.push(SemCheckError::DivisionByZero(right.span));
        }
    }

    fn rule_array_lit_elem_ty(&mut self, expr: &Expr) {
        let ExprKind::ArrayLit {
            elem_ty: Some(elem_ty),
            ..
        } = &expr.kind
        else {
            return;
        };
        self.check_type_expr(elem_ty);
    }

    fn rule_call_arg_ranges(&mut self, expr: &Expr) {
        let (ExprKind::Call { args, .. } | ExprKind::MethodCall { args, .. }) = &expr.kind else {
            return;
        };
        let Some(sig) = self.ctx.call_sigs.get(&expr.id) else {
            return;
        };
        for (arg, param) in args.iter().zip(sig.params.iter()) {
            self.check_range_binding_value(&arg.expr, &param.ty);
        }
    }

    fn rule_stmt_type_exprs(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { decl_ty, .. } | StmtExprKind::VarBind { decl_ty, .. } => {
                if let Some(ty) = decl_ty {
                    self.check_type_expr(ty);
                }
            }
            StmtExprKind::VarDecl { decl_ty, .. } => {
                self.check_type_expr(decl_ty);
            }
            _ => {}
        }
    }

    fn rule_stmt_range_bindings(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { decl_ty, value, .. }
            | StmtExprKind::VarBind { decl_ty, value, .. } => {
                if let Some(ty) = decl_ty
                    && let Some(resolved_ty) = self.resolve_type(ty)
                {
                    self.check_range_binding_value(value, &resolved_ty);
                }
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                let assignee_ty = self.ctx.type_map.type_table().get(assignee.ty);
                self.check_range_binding_value(value, assignee_ty);
            }
            _ => {}
        }
    }

    fn rule_stmt_const_env(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind { pattern, value, .. }
            | StmtExprKind::VarBind { pattern, value, .. } => {
                if let BindPatternKind::Name { def_id, .. } = &pattern.kind {
                    let const_value = self.const_int_value(value);
                    self.set_const(*def_id, const_value);
                }
            }
            StmtExprKind::VarDecl { def_id, .. } => {
                self.set_const(*def_id, None);
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                if let ExprKind::Var { def_id, .. } = assignee.kind {
                    let const_value = self.const_int_value(value);
                    self.set_const(def_id, const_value);
                }
            }
            _ => {}
        }
    }

    fn rule_stmt_return_ranges(&mut self, stmt: &StmtExpr) {
        let StmtExprKind::Return { value: Some(value) } = &stmt.kind else {
            return;
        };
        self.check_return_value_range(value);
    }
}

impl Visitor<DefId, TypeId> for ValueChecker<'_> {
    fn visit_func_def(&mut self, func_def: &FuncDef) {
        let return_ty = self
            .resolve_type(&func_def.sig.ret_ty_expr)
            .unwrap_or(Type::Unknown);
        self.push_return_ty(return_ty);
        self.push_const_scope();
        walk_expr(self, &func_def.body);

        let ret_expr = match &func_def.body.kind {
            ExprKind::Block {
                tail: Some(tail), ..
            } => Some(tail.as_ref()),
            ExprKind::Block { tail: None, .. } => None,
            _ => Some(&func_def.body),
        };
        if let Some(ret_expr) = ret_expr {
            self.check_return_value_range(ret_expr);
        }

        self.pop_const_scope();
        self.pop_return_ty();
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        self.run_stmt_rules(stmt);
        walk_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Closure {
            return_ty, body, ..
        } = &expr.kind
        {
            let resolved_return_ty = self.resolve_type(return_ty).unwrap_or(Type::Unknown);
            self.push_return_ty(resolved_return_ty);
            self.push_const_scope();
            walk_expr(self, expr);

            let body_expr = body.as_ref();
            let ret_expr = match &body_expr.kind {
                ExprKind::Block {
                    tail: Some(tail), ..
                } => Some(tail.as_ref()),
                ExprKind::Block { tail: None, .. } => None,
                _ => Some(body_expr),
            };
            if let Some(ret_expr) = ret_expr {
                self.check_return_value_range(ret_expr);
            }

            self.pop_const_scope();
            self.pop_return_ty();
            return;
        }

        if matches!(expr.kind, ExprKind::Block { .. }) {
            self.push_const_scope();
            walk_expr(self, expr);
            self.pop_const_scope();
            return;
        }
        self.run_expr_rules(expr);
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
