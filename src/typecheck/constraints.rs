use std::collections::HashMap;

use crate::diag::Span;
use crate::resolve::{DefId, DefKind};
use crate::tree::NodeId;
use crate::tree::resolved::{
    BinaryOp, BindPattern, BindPatternKind, BlockItem, CallArg, Expr, ExprKind, FuncDef, MatchArm,
    MatchPattern, MethodDef, MethodSig, StmtExpr, StmtExprKind, StructFieldBindPattern, TypeExpr,
    TypeParam, UnaryOp,
};
use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::errors::TypeCheckError;
use crate::typeck::type_map::resolve_type_expr_with_params;
use crate::types::{TyVarId, Type};

use super::typesys::TypeVarStore;

/// Type term used by constraints before solving.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TyTerm {
    Concrete(Type),
    Var(TyVarId),
}

/// Canonical constraints emitted by AST traversal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    Eq {
        left: TyTerm,
        right: TyTerm,
        reason: ConstraintReason,
    },
    Assignable {
        from: TyTerm,
        to: TyTerm,
        reason: ConstraintReason,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ConstraintReason {
    Expr(NodeId, Span),
    Stmt(NodeId, Span),
    Pattern(NodeId, Span),
    Decl(DefId, Span),
    Return(NodeId, Span),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CallCallee {
    NamedFunction { def_id: DefId, name: String },
    Method { name: String },
    Dynamic { expr_id: NodeId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallObligation {
    pub(crate) call_node: NodeId,
    pub(crate) span: Span,
    pub(crate) callee: CallCallee,
    pub(crate) receiver: Option<TyTerm>,
    pub(crate) arg_terms: Vec<TyTerm>,
    pub(crate) ret_ty: TyTerm,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) enum PatternObligation {
    Bind {
        pattern_id: NodeId,
        pattern: BindPattern,
        value_ty: TyTerm,
        span: Span,
    },
    MatchArm {
        arm_id: NodeId,
        pattern: MatchPattern,
        scrutinee_ty: TyTerm,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ControlFact {
    EnterCallable {
        def_id: DefId,
        return_ty: TyTerm,
        span: Span,
    },
    ExitCallable {
        def_id: DefId,
        span: Span,
    },
    Return {
        stmt_id: NodeId,
        has_value: bool,
        expected_return_ty: Option<TyTerm>,
        loop_depth: usize,
        span: Span,
    },
    Break {
        stmt_id: NodeId,
        loop_depth: usize,
        span: Span,
    },
    Continue {
        stmt_id: NodeId,
        loop_depth: usize,
        span: Span,
    },
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ConstrainOutput {
    pub(crate) constraints: Vec<Constraint>,
    pub(crate) call_obligations: Vec<CallObligation>,
    pub(crate) pattern_obligations: Vec<PatternObligation>,
    pub(crate) control_facts: Vec<ControlFact>,
    pub(crate) node_terms: HashMap<NodeId, TyTerm>,
    pub(crate) def_terms: HashMap<DefId, TyTerm>,
}

#[derive(Debug, Clone)]
struct ControlContext {
    return_ty: TyTerm,
    loop_depth: usize,
}

struct ConstraintCollector<'a> {
    ctx: &'a crate::context::ResolvedContext,
    type_defs: &'a HashMap<String, Type>,
    vars: &'a mut TypeVarStore,
    out: &'a mut ConstrainOutput,
    control_stack: Vec<ControlContext>,
    type_param_stack: Vec<HashMap<DefId, TyVarId>>,
}

impl<'a> ConstraintCollector<'a> {
    fn new(
        ctx: &'a crate::context::ResolvedContext,
        type_defs: &'a HashMap<String, Type>,
        vars: &'a mut TypeVarStore,
        out: &'a mut ConstrainOutput,
    ) -> Self {
        Self {
            ctx,
            type_defs,
            vars,
            out,
            control_stack: Vec::new(),
            type_param_stack: Vec::new(),
        }
    }

    fn collect_module(&mut self) {
        for func_def in self.ctx.module.func_defs() {
            self.collect_func_def(func_def);
        }
        for method_block in self.ctx.module.method_blocks() {
            for method_item in &method_block.method_items {
                let MethodDef { sig, .. } = match method_item {
                    crate::tree::resolved::MethodItem::Decl(_) => continue,
                    crate::tree::resolved::MethodItem::Def(method_def) => method_def,
                };
                self.collect_method_def(&method_block.type_name, method_item, sig);
            }
        }
    }

    fn collect_func_def(&mut self, func_def: &FuncDef) {
        self.with_type_params(&func_def.sig.type_params, |this| {
            let ret_ty = this.resolve_type_in_scope(&func_def.sig.ret_ty_expr);
            let return_term = ret_ty
                .map(TyTerm::Concrete)
                .unwrap_or_else(|_| this.fresh_var_term());
            this.enter_callable(func_def.def_id, return_term.clone(), func_def.span);

            for param in &func_def.sig.params {
                let def_term = this.def_term(param.def_id);
                if let Ok(ty) = this.resolve_type_in_scope(&param.typ) {
                    this.push_eq(
                        def_term.clone(),
                        TyTerm::Concrete(ty),
                        ConstraintReason::Decl(param.def_id, param.span),
                    );
                }
                let node_term = this.node_term(param.id);
                this.push_eq(
                    node_term,
                    def_term,
                    ConstraintReason::Decl(param.def_id, param.span),
                );
            }

            let body_ty = this.collect_expr(&func_def.body, Some(return_term.clone()));
            this.push_assignable(
                body_ty,
                return_term,
                ConstraintReason::Expr(func_def.body.id, func_def.body.span),
            );
            this.exit_callable(func_def.def_id, func_def.span);
        });
    }

    fn collect_method_def(
        &mut self,
        type_name: &str,
        method_item: &crate::tree::resolved::MethodItem,
        sig: &MethodSig,
    ) {
        let method_def_id = match method_item {
            crate::tree::resolved::MethodItem::Decl(method_decl) => method_decl.def_id,
            crate::tree::resolved::MethodItem::Def(method_def) => method_def.def_id,
        };
        let method_span = match method_item {
            crate::tree::resolved::MethodItem::Decl(method_decl) => method_decl.span,
            crate::tree::resolved::MethodItem::Def(method_def) => method_def.span,
        };

        self.with_type_params(&sig.type_params, |this| {
            let ret_ty = this.resolve_type_in_scope(&sig.ret_ty_expr);
            let return_term = ret_ty
                .map(TyTerm::Concrete)
                .unwrap_or_else(|_| this.fresh_var_term());
            this.enter_callable(method_def_id, return_term.clone(), method_span);

            let self_term = this.def_term(sig.self_param.def_id);
            if let Some(self_ty) = this.type_defs.get(type_name).cloned() {
                this.push_eq(
                    self_term.clone(),
                    TyTerm::Concrete(self_ty),
                    ConstraintReason::Decl(sig.self_param.def_id, sig.self_param.span),
                );
            }
            let self_node = this.node_term(sig.self_param.id);
            this.push_eq(
                self_node,
                self_term,
                ConstraintReason::Decl(sig.self_param.def_id, sig.self_param.span),
            );

            for param in &sig.params {
                let def_term = this.def_term(param.def_id);
                if let Ok(ty) = this.resolve_type_in_scope(&param.typ) {
                    this.push_eq(
                        def_term.clone(),
                        TyTerm::Concrete(ty),
                        ConstraintReason::Decl(param.def_id, param.span),
                    );
                }
                let node_term = this.node_term(param.id);
                this.push_eq(
                    node_term,
                    def_term,
                    ConstraintReason::Decl(param.def_id, param.span),
                );
            }

            if let crate::tree::resolved::MethodItem::Def(method_def) = method_item {
                let body_ty = this.collect_expr(&method_def.body, Some(return_term.clone()));
                this.push_assignable(
                    body_ty,
                    return_term,
                    ConstraintReason::Expr(method_def.body.id, method_def.body.span),
                );
            }

            this.exit_callable(method_def_id, method_span);
        });
    }

    fn with_type_params<F>(&mut self, type_params: &[TypeParam], f: F)
    where
        F: FnOnce(&mut Self),
    {
        if type_params.is_empty() {
            f(self);
            return;
        }

        let mapping = type_params
            .iter()
            .enumerate()
            .map(|(index, param)| (param.def_id, TyVarId::new(index as u32)))
            .collect::<HashMap<_, _>>();

        for (def_id, var) in &mapping {
            self.vars.register_rigid_param(*var, *def_id);
        }

        self.type_param_stack.push(mapping);
        f(self);
        self.type_param_stack.pop();
    }

    fn current_type_params(&self) -> Option<&HashMap<DefId, TyVarId>> {
        self.type_param_stack.last()
    }

    fn resolve_type_in_scope(&self, ty_expr: &TypeExpr) -> Result<Type, TypeCheckError> {
        resolve_type_expr_with_params(
            &self.ctx.def_table,
            &self.ctx.module,
            ty_expr,
            self.current_type_params(),
        )
    }

    fn collect_expr(&mut self, expr: &Expr, expected: Option<TyTerm>) -> TyTerm {
        let expr_ty = self.node_term(expr.id);
        if let Some(expected) = expected.clone() {
            self.push_eq(
                expr_ty.clone(),
                expected,
                ConstraintReason::Expr(expr.id, expr.span),
            );
        }

        match &expr.kind {
            ExprKind::UnitLit => self.push_eq(
                expr_ty.clone(),
                TyTerm::Concrete(Type::Unit),
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::IntLit(_) => {
                if expected.is_none() {
                    self.push_eq(
                        expr_ty.clone(),
                        TyTerm::Concrete(Type::uint(64)),
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                }
            }
            ExprKind::BoolLit(_) => self.push_eq(
                expr_ty.clone(),
                TyTerm::Concrete(Type::Bool),
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::CharLit(_) => self.push_eq(
                expr_ty.clone(),
                TyTerm::Concrete(Type::Char),
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::StringLit { .. } | ExprKind::StringFmt { .. } => self.push_eq(
                expr_ty.clone(),
                TyTerm::Concrete(Type::String),
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::HeapAlloc { expr: inner }
            | ExprKind::Move { expr: inner }
            | ExprKind::Coerce { expr: inner, .. }
            | ExprKind::ImplicitMove { expr: inner }
            | ExprKind::AddrOf { expr: inner }
            | ExprKind::Deref { expr: inner } => {
                let inner_ty = self.collect_expr(inner, None);
                self.push_eq(
                    expr_ty.clone(),
                    inner_ty,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::Var { def_id, .. } => {
                let def_ty = self.def_term(*def_id);
                self.push_eq(
                    expr_ty.clone(),
                    def_ty,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::Block { items, tail } => {
                for item in items {
                    self.collect_block_item(item);
                }
                if let Some(tail) = tail {
                    let tail_ty = self.collect_expr(tail, expected.clone());
                    self.push_eq(
                        expr_ty.clone(),
                        tail_ty,
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                } else {
                    if let Some(expected) = expected.clone() {
                        if self.block_has_explicit_return(items) {
                            self.push_eq(
                                expr_ty.clone(),
                                expected,
                                ConstraintReason::Expr(expr.id, expr.span),
                            );
                        } else {
                            self.push_eq(
                                expr_ty.clone(),
                                TyTerm::Concrete(Type::Unit),
                                ConstraintReason::Expr(expr.id, expr.span),
                            );
                        }
                    } else {
                        self.push_eq(
                            expr_ty.clone(),
                            TyTerm::Concrete(Type::Unit),
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                    }
                }
            }
            ExprKind::TupleLit(fields) => {
                for field in fields {
                    self.collect_expr(field, None);
                }
            }
            ExprKind::ArrayLit { init, .. } => match init {
                crate::tree::resolved::ArrayLitInit::Elems(elems) => {
                    for elem in elems {
                        self.collect_expr(elem, None);
                    }
                }
                crate::tree::resolved::ArrayLitInit::Repeat(value, _) => {
                    self.collect_expr(value, None);
                }
            },
            ExprKind::StructLit { fields, .. } => {
                for field in fields {
                    self.collect_expr(&field.value, None);
                }
            }
            ExprKind::EnumVariant { payload, .. } => {
                for item in payload {
                    self.collect_expr(item, None);
                }
            }
            ExprKind::StructUpdate { target, fields } => {
                self.collect_expr(target, None);
                for field in fields {
                    self.collect_expr(&field.value, None);
                }
            }
            ExprKind::ArrayIndex { target, indices } => {
                self.collect_expr(target, None);
                for index in indices {
                    self.collect_expr(index, Some(TyTerm::Concrete(Type::uint(64))));
                }
            }
            ExprKind::TupleField { target, .. } | ExprKind::StructField { target, .. } => {
                self.collect_expr(target, None);
            }
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.collect_expr(cond, Some(TyTerm::Concrete(Type::Bool)));
                let then_ty = self.collect_expr(then_body, expected.clone());
                let else_ty = self.collect_expr(else_body, expected.clone());
                self.push_eq(
                    then_ty.clone(),
                    else_ty,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
                self.push_eq(
                    expr_ty.clone(),
                    then_ty,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::Range { start, end } => {
                self.collect_expr(start, Some(TyTerm::Concrete(Type::uint(64))));
                self.collect_expr(end, Some(TyTerm::Concrete(Type::uint(64))));
                self.push_eq(
                    expr_ty.clone(),
                    TyTerm::Concrete(Type::Range {
                        elem_ty: Box::new(Type::uint(64)),
                    }),
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::Slice { target, start, end } => {
                self.collect_expr(target, None);
                if let Some(start) = start {
                    self.collect_expr(start, Some(TyTerm::Concrete(Type::uint(64))));
                }
                if let Some(end) = end {
                    self.collect_expr(end, Some(TyTerm::Concrete(Type::uint(64))));
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                let scrutinee_ty = self.collect_expr(scrutinee, None);
                for arm in arms {
                    self.out
                        .pattern_obligations
                        .push(PatternObligation::MatchArm {
                            arm_id: arm.id,
                            pattern: arm.pattern.clone(),
                            scrutinee_ty: scrutinee_ty.clone(),
                            span: arm.span,
                        });
                    let arm_ty = self.collect_match_arm(arm, expected.clone());
                    self.push_eq(
                        expr_ty.clone(),
                        arm_ty,
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                }
            }
            ExprKind::Call { callee, args } => {
                self.collect_call(expr, callee, args);
            }
            ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => {
                let receiver_ty = self.collect_expr(callee, None);
                let arg_terms = args
                    .iter()
                    .map(|arg| self.collect_expr(&arg.expr, None))
                    .collect::<Vec<_>>();
                self.out.call_obligations.push(CallObligation {
                    call_node: expr.id,
                    span: expr.span,
                    callee: CallCallee::Method {
                        name: method_name.clone(),
                    },
                    receiver: Some(receiver_ty),
                    arg_terms,
                    ret_ty: expr_ty.clone(),
                });
            }
            ExprKind::BinOp { left, right, .. } => {
                let left_ty = self.collect_expr(left, None);
                let right_ty = self.collect_expr(right, None);
                self.collect_binop_constraints(expr, left_ty, right_ty);
            }
            ExprKind::UnaryOp { op, expr: inner } => {
                let inner_ty = self.collect_expr(inner, None);
                match op {
                    UnaryOp::Neg | UnaryOp::BitNot => {
                        self.push_eq(
                            expr_ty.clone(),
                            inner_ty,
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                    }
                    UnaryOp::LogicalNot => {
                        self.push_eq(
                            inner_ty,
                            TyTerm::Concrete(Type::Bool),
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                        self.push_eq(
                            expr_ty.clone(),
                            TyTerm::Concrete(Type::Bool),
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                    }
                }
            }
            ExprKind::Closure {
                def_id,
                params,
                return_ty,
                body,
                ..
            } => {
                let fn_ty = self.collect_closure_signature(params, return_ty);
                if let Some(fn_ty) = fn_ty.clone() {
                    self.push_eq(
                        expr_ty.clone(),
                        TyTerm::Concrete(fn_ty),
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                }

                let return_term = fn_ty
                    .as_ref()
                    .and_then(|ty| match ty {
                        Type::Fn { ret_ty, .. } => Some(TyTerm::Concrete((**ret_ty).clone())),
                        _ => None,
                    })
                    .unwrap_or_else(|| self.fresh_var_term());

                self.enter_callable(*def_id, return_term.clone(), expr.span);
                for param in params {
                    let param_term = self.def_term(param.def_id);
                    if let Ok(param_ty) = self.resolve_type_in_scope(&param.typ) {
                        self.push_eq(
                            param_term.clone(),
                            TyTerm::Concrete(param_ty),
                            ConstraintReason::Decl(param.def_id, param.span),
                        );
                    }
                    let node_term = self.node_term(param.id);
                    self.push_eq(
                        node_term,
                        param_term,
                        ConstraintReason::Decl(param.def_id, param.span),
                    );
                }
                let body_ty = self.collect_expr(body, Some(return_term.clone()));
                self.push_assignable(
                    body_ty,
                    return_term,
                    ConstraintReason::Expr(body.id, body.span),
                );
                self.exit_callable(*def_id, expr.span);
            }
        }

        expr_ty
    }

    fn collect_match_arm(&mut self, arm: &MatchArm, expected: Option<TyTerm>) -> TyTerm {
        self.collect_match_pattern_bindings(&arm.pattern);
        self.collect_expr(&arm.body, expected)
    }

    fn collect_match_pattern_bindings(&mut self, pattern: &MatchPattern) {
        match pattern {
            MatchPattern::Binding {
                def_id, id, span, ..
            } => {
                let bind_term = self.def_term(*def_id);
                let node_term = self.node_term(*id);
                self.push_eq(node_term, bind_term, ConstraintReason::Pattern(*id, *span));
            }
            MatchPattern::Tuple { patterns, .. } => {
                for pattern in patterns {
                    self.collect_match_pattern_bindings(pattern);
                }
            }
            MatchPattern::EnumVariant { bindings, .. } => {
                for binding in bindings {
                    if let crate::tree::resolved::MatchPatternBinding::Named {
                        def_id,
                        id,
                        span,
                        ..
                    } = binding
                    {
                        let bind_term = self.def_term(*def_id);
                        let node_term = self.node_term(*id);
                        self.push_eq(node_term, bind_term, ConstraintReason::Pattern(*id, *span));
                    }
                }
            }
            _ => {}
        }
    }

    fn collect_call(&mut self, call_expr: &Expr, callee: &Expr, args: &[CallArg]) {
        self.collect_expr(callee, None);
        let arg_terms = args
            .iter()
            .map(|arg| self.collect_expr(&arg.expr, None))
            .collect::<Vec<_>>();
        let ret_ty = self.node_term(call_expr.id);
        let callee_kind = match &callee.kind {
            ExprKind::Var { def_id, ident } => {
                if let Some(def) = self.ctx.def_table.lookup_def(*def_id) {
                    if matches!(def.kind, DefKind::FuncDef { .. } | DefKind::FuncDecl { .. }) {
                        CallCallee::NamedFunction {
                            def_id: *def_id,
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
            callee: callee_kind,
            receiver: None,
            arg_terms,
            ret_ty,
        });
    }

    fn collect_binop_constraints(&mut self, expr: &Expr, left_ty: TyTerm, right_ty: TyTerm) {
        let expr_ty = self.node_term(expr.id);
        let ExprKind::BinOp { op, .. } = &expr.kind else {
            return;
        };
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
                    TyTerm::Concrete(Type::Bool),
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                self.push_eq(
                    left_ty,
                    TyTerm::Concrete(Type::Bool),
                    ConstraintReason::Expr(expr.id, expr.span),
                );
                self.push_eq(
                    right_ty,
                    TyTerm::Concrete(Type::Bool),
                    ConstraintReason::Expr(expr.id, expr.span),
                );
                self.push_eq(
                    expr_ty,
                    TyTerm::Concrete(Type::Bool),
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
        }
    }

    fn collect_block_item(&mut self, item: &BlockItem) {
        match item {
            BlockItem::Stmt(stmt) => self.collect_stmt(stmt),
            BlockItem::Expr(expr) => {
                let _ = self.collect_expr(expr, None);
            }
        }
    }

    fn collect_stmt(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            }
            | StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                let expected_decl_ty = decl_ty
                    .as_ref()
                    .and_then(|decl_ty| self.resolve_type_in_scope(decl_ty).ok())
                    .map(TyTerm::Concrete);
                let value_ty = self.collect_expr(value, expected_decl_ty.clone());
                if let Some(decl_ty) = expected_decl_ty.clone() {
                    self.push_assignable(
                        value_ty.clone(),
                        decl_ty,
                        ConstraintReason::Stmt(stmt.id, stmt.span),
                    );
                }
                self.collect_bind_pattern(pattern, value_ty);
            }
            StmtExprKind::VarDecl {
                def_id, decl_ty, ..
            } => {
                let def_term = self.def_term(*def_id);
                if let Ok(ty) = self.resolve_type_in_scope(decl_ty) {
                    self.push_eq(
                        def_term,
                        TyTerm::Concrete(ty),
                        ConstraintReason::Decl(*def_id, stmt.span),
                    );
                }
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                let lhs_ty = self.collect_expr(assignee, None);
                let rhs_ty = self.collect_expr(value, Some(lhs_ty.clone()));
                self.push_assignable(rhs_ty, lhs_ty, ConstraintReason::Stmt(stmt.id, stmt.span));
            }
            StmtExprKind::While { cond, body } => {
                self.collect_expr(cond, Some(TyTerm::Concrete(Type::Bool)));
                self.enter_loop();
                self.collect_expr(body, Some(TyTerm::Concrete(Type::Unit)));
                self.exit_loop();
            }
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                self.collect_expr(iter, None);
                let pattern_ty = self.fresh_var_term();
                self.collect_bind_pattern(pattern, pattern_ty);
                self.enter_loop();
                self.collect_expr(body, Some(TyTerm::Concrete(Type::Unit)));
                self.exit_loop();
            }
            StmtExprKind::Break => {
                self.out.control_facts.push(ControlFact::Break {
                    stmt_id: stmt.id,
                    loop_depth: self.current_loop_depth(),
                    span: stmt.span,
                });
            }
            StmtExprKind::Continue => {
                self.out.control_facts.push(ControlFact::Continue {
                    stmt_id: stmt.id,
                    loop_depth: self.current_loop_depth(),
                    span: stmt.span,
                });
            }
            StmtExprKind::Return { value } => {
                let expected_return = self.current_return_ty();
                if let Some(value) = value {
                    let value_ty = self.collect_expr(value, expected_return.clone());
                    if let Some(expected_return) = expected_return.clone() {
                        self.push_assignable(
                            value_ty,
                            expected_return,
                            ConstraintReason::Return(stmt.id, stmt.span),
                        );
                    }
                }
                self.out.control_facts.push(ControlFact::Return {
                    stmt_id: stmt.id,
                    has_value: value.is_some(),
                    expected_return_ty: expected_return,
                    loop_depth: self.current_loop_depth(),
                    span: stmt.span,
                });
            }
        }
    }

    fn collect_bind_pattern(&mut self, pattern: &BindPattern, value_ty: TyTerm) {
        self.out.pattern_obligations.push(PatternObligation::Bind {
            pattern_id: pattern.id,
            pattern: pattern.clone(),
            value_ty: value_ty.clone(),
            span: pattern.span,
        });

        match &pattern.kind {
            BindPatternKind::Name { def_id, .. } => {
                let bind_ty = self.def_term(*def_id);
                let node_ty = self.node_term(pattern.id);
                self.push_assignable(
                    value_ty,
                    bind_ty.clone(),
                    ConstraintReason::Pattern(pattern.id, pattern.span),
                );
                self.push_eq(
                    node_ty,
                    bind_ty,
                    ConstraintReason::Pattern(pattern.id, pattern.span),
                );
            }
            BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
                for child in patterns {
                    self.collect_bind_pattern(child, value_ty.clone());
                }
            }
            BindPatternKind::Struct { fields, .. } => {
                for StructFieldBindPattern { pattern, .. } in fields {
                    self.collect_bind_pattern(pattern, value_ty.clone());
                }
            }
        }
    }

    fn node_term(&mut self, node_id: NodeId) -> TyTerm {
        if let Some(term) = self.out.node_terms.get(&node_id) {
            return term.clone();
        }
        let term = self.fresh_var_term();
        self.out.node_terms.insert(node_id, term.clone());
        term
    }

    fn def_term(&mut self, def_id: DefId) -> TyTerm {
        if let Some(term) = self.out.def_terms.get(&def_id) {
            return term.clone();
        }
        let term = self.fresh_var_term();
        self.out.def_terms.insert(def_id, term.clone());
        term
    }

    fn fresh_var_term(&mut self) -> TyTerm {
        TyTerm::Var(self.vars.fresh_infer_local())
    }

    fn push_eq(&mut self, left: TyTerm, right: TyTerm, reason: ConstraintReason) {
        self.out.constraints.push(Constraint::Eq {
            left,
            right,
            reason,
        });
    }

    fn push_assignable(&mut self, from: TyTerm, to: TyTerm, reason: ConstraintReason) {
        self.out
            .constraints
            .push(Constraint::Assignable { from, to, reason });
    }

    fn enter_callable(&mut self, def_id: DefId, return_ty: TyTerm, span: Span) {
        self.out.control_facts.push(ControlFact::EnterCallable {
            def_id,
            return_ty: return_ty.clone(),
            span,
        });
        self.control_stack.push(ControlContext {
            return_ty,
            loop_depth: 0,
        });
    }

    fn exit_callable(&mut self, def_id: DefId, span: Span) {
        self.out
            .control_facts
            .push(ControlFact::ExitCallable { def_id, span });
        let _ = self.control_stack.pop();
    }

    fn current_return_ty(&self) -> Option<TyTerm> {
        self.control_stack.last().map(|ctx| ctx.return_ty.clone())
    }

    fn current_loop_depth(&self) -> usize {
        self.control_stack
            .last()
            .map(|ctx| ctx.loop_depth)
            .unwrap_or(0)
    }

    fn enter_loop(&mut self) {
        if let Some(ctx) = self.control_stack.last_mut() {
            ctx.loop_depth += 1;
        }
    }

    fn exit_loop(&mut self) {
        if let Some(ctx) = self.control_stack.last_mut() {
            ctx.loop_depth = ctx.loop_depth.saturating_sub(1);
        }
    }

    fn collect_closure_signature(
        &self,
        params: &[crate::tree::resolved::Param],
        return_ty: &TypeExpr,
    ) -> Option<Type> {
        let params = params
            .iter()
            .map(|param| {
                resolve_type_expr_with_params(
                    &self.ctx.def_table,
                    &self.ctx.module,
                    &param.typ,
                    self.current_type_params(),
                )
                .map(|ty| crate::types::FnParam {
                    mode: match param.mode {
                        crate::tree::ParamMode::In => crate::types::FnParamMode::In,
                        crate::tree::ParamMode::InOut => crate::types::FnParamMode::InOut,
                        crate::tree::ParamMode::Out => crate::types::FnParamMode::Out,
                        crate::tree::ParamMode::Sink => crate::types::FnParamMode::Sink,
                    },
                    ty,
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .ok()?;
        let ret_ty = resolve_type_expr_with_params(
            &self.ctx.def_table,
            &self.ctx.module,
            return_ty,
            self.current_type_params(),
        )
        .ok()?;
        Some(Type::Fn {
            params,
            ret_ty: Box::new(ret_ty),
        })
    }

    fn block_has_explicit_return(&self, items: &[BlockItem]) -> bool {
        items.iter().any(|item| match item {
            BlockItem::Stmt(stmt) => self.stmt_has_return(stmt),
            BlockItem::Expr(expr) => self.expr_has_return(expr),
        })
    }

    fn stmt_has_return(&self, stmt: &StmtExpr) -> bool {
        match &stmt.kind {
            StmtExprKind::Return { .. } => true,
            StmtExprKind::While { body, .. } | StmtExprKind::For { body, .. } => {
                self.expr_has_return(body)
            }
            StmtExprKind::LetBind { value, .. }
            | StmtExprKind::VarBind { value, .. }
            | StmtExprKind::Assign { value, .. } => self.expr_has_return(value),
            _ => false,
        }
    }

    fn expr_has_return(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Block { items, tail } => {
                self.block_has_explicit_return(items)
                    || tail
                        .as_ref()
                        .is_some_and(|tail_expr| self.expr_has_return(tail_expr))
            }
            ExprKind::If {
                then_body,
                else_body,
                ..
            } => self.expr_has_return(then_body) || self.expr_has_return(else_body),
            ExprKind::Match { arms, .. } => arms.iter().any(|arm| self.expr_has_return(&arm.body)),
            // Do not recurse into nested closures; their returns are local to them.
            ExprKind::Closure { .. } => false,
            _ => false,
        }
    }
}

/// Pass 2: collect typing constraints from AST traversal.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let ctx = engine.context().clone();
    let type_defs = engine.env().type_defs.clone();
    let mut output = ConstrainOutput::default();

    {
        let vars = engine.type_vars_mut();
        let mut collector = ConstraintCollector::new(&ctx, &type_defs, vars, &mut output);
        collector.collect_module();
    }

    engine.state_mut().constrain = output;

    if engine.state().diags.is_empty() {
        Ok(())
    } else {
        Err(engine.state().diags.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::ParsedContext;
    use crate::lexer::{LexError, Lexer, Token};
    use crate::parse::Parser;
    use crate::resolve::resolve;
    use crate::typecheck::{collect, engine::TypecheckEngine};

    fn resolve_source(source: &str) -> crate::context::ResolvedContext {
        let lexer = Lexer::new(source);
        let tokens = lexer
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .expect("Failed to tokenize");
        let mut parser = Parser::new(&tokens);
        let module = parser.parse().expect("Failed to parse");
        let id_gen = parser.into_id_gen();
        let ast_context = ParsedContext::new(module, id_gen);
        resolve(ast_context).expect("Failed to resolve")
    }

    fn collect_constraints(source: &str) -> ConstrainOutput {
        let resolved = resolve_source(source);
        let mut engine = TypecheckEngine::new(resolved);
        collect::run(&mut engine).expect("collect pass failed");
        run(&mut engine).expect("constrain pass failed");
        engine.state().constrain.clone()
    }

    #[test]
    fn test_collect_constraints_for_let_binding() {
        let source = r#"
            fn test() -> u64 {
                let x = 1;
                x
            }
        "#;

        let out = collect_constraints(source);
        assert!(!out.constraints.is_empty());
        assert!(
            out.pattern_obligations
                .iter()
                .any(|ob| matches!(ob, PatternObligation::Bind { .. }))
        );
    }

    #[test]
    fn test_collect_call_obligation() {
        let source = r#"
            fn id(x: u64) -> u64 { x }

            fn test() -> u64 {
                id(1)
            }
        "#;

        let out = collect_constraints(source);
        assert_eq!(out.call_obligations.len(), 1);
        assert!(matches!(
            out.call_obligations[0].callee,
            CallCallee::NamedFunction { .. }
        ));
    }

    #[test]
    fn test_collect_control_facts_for_loop_flow() {
        let source = r#"
            fn test() -> u64 {
                while true {
                    break;
                }
                0
            }
        "#;

        let out = collect_constraints(source);
        assert!(
            out.control_facts
                .iter()
                .any(|fact| matches!(fact, ControlFact::Break { loop_depth: d, .. } if *d > 0))
        );
    }
}
