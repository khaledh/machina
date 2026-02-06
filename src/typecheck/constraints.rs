//! Pass 2 of the type checker: constraint and obligation collection.
//!
//! This pass walks resolved AST bodies and emits:
//! - structural constraints (`Eq`, `Assignable`),
//! - expression/call/pattern obligations that require semantic interpretation,
//! - control-flow facts (`return`, `break`, `continue`),
//! - node/def-to-type-term anchors used by later phases.
//!
//! The collector is intentionally syntax-directed and mostly diagnostic-free.
//! Solver/validator phases interpret these facts and produce user-facing errors.

use std::collections::HashMap;

use crate::diag::Span;
use crate::resolve::{DefId, DefKind};
use crate::tree::NodeId;
use crate::tree::resolved::{
    BinaryOp, BindPattern, BindPatternKind, BlockItem, CallArg, Expr, ExprKind, FuncDef, MatchArm,
    MatchPattern, MethodSig, StmtExpr, StmtExprKind, StringFmtSegment, StructFieldBindPattern,
    TypeExpr, TypeParam, UnaryOp,
};
use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::errors::TypeCheckError;
use crate::typecheck::type_map::{resolve_type_def_with_args, resolve_type_expr_with_params};
use crate::types::{TyVarId, Type};

use super::typesys::TypeVarStore;

/// Canonical constraints emitted by AST traversal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Constraint {
    Eq {
        left: Type,
        right: Type,
        reason: ConstraintReason,
    },
    Assignable {
        from: Type,
        to: Type,
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
#[allow(dead_code)]
pub(crate) enum ExprObligation {
    BinOp {
        expr_id: NodeId,
        op: BinaryOp,
        left: Type,
        right: Type,
        result: Type,
        span: Span,
    },
    UnaryOp {
        expr_id: NodeId,
        op: UnaryOp,
        operand: Type,
        result: Type,
        span: Span,
    },
    ArrayIndex {
        expr_id: NodeId,
        target: Type,
        indices: Vec<Type>,
        index_nodes: Vec<NodeId>,
        index_spans: Vec<Span>,
        result: Type,
        span: Span,
    },
    Slice {
        expr_id: NodeId,
        target: Type,
        start: Option<Type>,
        end: Option<Type>,
        result: Type,
        span: Span,
    },
    Range {
        expr_id: NodeId,
        start: Type,
        end: Type,
        result: Type,
        span: Span,
    },
    ForIter {
        stmt_id: NodeId,
        iter: Type,
        pattern: Type,
        span: Span,
    },
    EnumVariantPayload {
        expr_id: NodeId,
        enum_name: String,
        variant: String,
        payload: Vec<Type>,
        payload_nodes: Vec<NodeId>,
        payload_spans: Vec<Span>,
        span: Span,
    },
    StructUpdate {
        expr_id: NodeId,
        target: Type,
        fields: Vec<(String, Type)>,
        result: Type,
        span: Span,
    },
    TupleField {
        expr_id: NodeId,
        target: Type,
        index: usize,
        result: Type,
        span: Span,
    },
    StructField {
        expr_id: NodeId,
        target: Type,
        field: String,
        result: Type,
        span: Span,
    },
    StructFieldAssign {
        stmt_id: NodeId,
        target: Type,
        field: String,
        assignee: Type,
        value: Type,
        span: Span,
    },
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
    pub(crate) callee_ty: Option<Type>,
    pub(crate) receiver: Option<Type>,
    pub(crate) arg_terms: Vec<Type>,
    pub(crate) ret_ty: Type,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) enum PatternObligation {
    Bind {
        pattern_id: NodeId,
        pattern: BindPattern,
        value_ty: Type,
        span: Span,
    },
    MatchArm {
        arm_id: NodeId,
        pattern: MatchPattern,
        scrutinee_ty: Type,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ControlFact {
    EnterCallable {
        def_id: DefId,
        return_ty: Type,
        span: Span,
    },
    ExitCallable {
        def_id: DefId,
        span: Span,
    },
    Return {
        stmt_id: NodeId,
        has_value: bool,
        expected_return_ty: Option<Type>,
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
    pub(crate) expr_obligations: Vec<ExprObligation>,
    pub(crate) call_obligations: Vec<CallObligation>,
    pub(crate) pattern_obligations: Vec<PatternObligation>,
    pub(crate) control_facts: Vec<ControlFact>,
    pub(crate) node_terms: HashMap<NodeId, Type>,
    pub(crate) def_terms: HashMap<DefId, Type>,
}

#[derive(Debug, Clone)]
struct ControlContext {
    return_ty: Type,
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
        // Declarations first so callable defs are available when encountered by
        // later expressions in the same module.
        for func_decl in self.ctx.module.func_decls() {
            self.collect_func_decl(func_decl);
        }
        for func_def in self.ctx.module.func_defs() {
            self.collect_func_def(func_def);
        }
        for method_block in self.ctx.module.method_blocks() {
            for method_item in &method_block.method_items {
                let sig = match method_item {
                    crate::tree::resolved::MethodItem::Decl(method_decl) => &method_decl.sig,
                    crate::tree::resolved::MethodItem::Def(method_def) => &method_def.sig,
                };
                self.collect_method_def(&method_block.type_name, method_item, sig);
            }
        }
    }

    fn collect_func_decl(&mut self, func_decl: &crate::tree::resolved::FuncDecl) {
        self.with_type_params(&func_decl.sig.type_params, |this| {
            if let Some(fn_ty) = this.collect_function_signature(&func_decl.sig) {
                let def_term = this.def_term(func_decl.def_id);
                this.push_eq(
                    def_term,
                    fn_ty,
                    ConstraintReason::Decl(func_decl.def_id, func_decl.span),
                );
            }
        });
    }

    fn collect_func_def(&mut self, func_def: &FuncDef) {
        self.with_type_params(&func_def.sig.type_params, |this| {
            if let Some(fn_ty) = this.collect_function_signature(&func_def.sig) {
                let def_term = this.def_term(func_def.def_id);
                this.push_eq(
                    def_term,
                    fn_ty,
                    ConstraintReason::Decl(func_def.def_id, func_def.span),
                );
            }
            let ret_ty = this.resolve_type_in_scope(&func_def.sig.ret_ty_expr);
            let return_term = ret_ty.unwrap_or_else(|_| this.fresh_var_term());
            let func_node_term = this.node_term(func_def.id);
            this.push_eq(
                func_node_term,
                return_term.clone(),
                ConstraintReason::Decl(func_def.def_id, func_def.span),
            );
            this.enter_callable(func_def.def_id, return_term.clone(), func_def.span);

            for param in &func_def.sig.params {
                let def_term = this.def_term(param.def_id);
                if let Ok(ty) = this.resolve_type_in_scope(&param.typ) {
                    this.push_eq(
                        def_term.clone(),
                        ty,
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
            if let Some(fn_ty) = this.collect_method_signature(type_name, sig) {
                let def_term = this.def_term(method_def_id);
                this.push_eq(
                    def_term,
                    fn_ty,
                    ConstraintReason::Decl(method_def_id, method_span),
                );
            }
            let ret_ty = this.resolve_type_in_scope(&sig.ret_ty_expr);
            let return_term = ret_ty.unwrap_or_else(|_| this.fresh_var_term());
            if let crate::tree::resolved::MethodItem::Def(method_def) = method_item {
                let method_node_term = this.node_term(method_def.id);
                this.push_eq(
                    method_node_term,
                    return_term.clone(),
                    ConstraintReason::Decl(method_def.def_id, method_def.span),
                );
            }
            this.enter_callable(method_def_id, return_term.clone(), method_span);

            let self_term = this.def_term(sig.self_param.def_id);
            if let Some(self_ty) = this.type_defs.get(type_name).cloned() {
                this.push_eq(
                    self_term.clone(),
                    self_ty,
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
                        ty,
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

    fn resolve_named_type_instance(&mut self, name: &str, type_args: &[TypeExpr]) -> Option<Type> {
        let def_id = self.ctx.def_table.lookup_type_def_id(name)?;
        let type_def = self.ctx.module.type_def_by_id(def_id)?;
        let args = if type_args.is_empty() && type_def.type_params.is_empty() {
            Vec::new()
        } else if type_args.is_empty() {
            type_def
                .type_params
                .iter()
                .map(|_| Type::Var(self.vars.fresh_infer_local()))
                .collect::<Vec<_>>()
        } else {
            let mut out = Vec::with_capacity(type_args.len());
            for arg in type_args {
                out.push(self.resolve_type_in_scope(arg).ok()?);
            }
            out
        };
        resolve_type_def_with_args(&self.ctx.def_table, &self.ctx.module, def_id, &args).ok()
    }

    fn collect_expr(&mut self, expr: &Expr, expected: Option<Type>) -> Type {
        let expr_ty = self.node_term(expr.id);

        // Push contextual expected-type information into the expression graph
        // for non-place expressions. Place-like forms (`var`, `block`) are
        // handled by their own structural equalities below.
        if let Some(expected) = expected.clone()
            && !matches!(expr.kind, ExprKind::Var { .. } | ExprKind::Block { .. })
        {
            self.push_assignable(
                expr_ty.clone(),
                expected,
                ConstraintReason::Expr(expr.id, expr.span),
            );
        }

        match &expr.kind {
            ExprKind::UnitLit => self.push_eq(
                expr_ty.clone(),
                Type::Unit,
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::IntLit(_) => {
                // Keep integer literals polymorphic during solving. If they
                // remain unconstrained, solve defaults this var to i32.
                let lit_ty = self.fresh_int_var_term();
                self.push_eq(
                    expr_ty.clone(),
                    lit_ty,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::BoolLit(_) => self.push_eq(
                expr_ty.clone(),
                Type::Bool,
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::CharLit(_) => self.push_eq(
                expr_ty.clone(),
                Type::Char,
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::StringLit { .. } => self.push_eq(
                expr_ty.clone(),
                Type::String,
                ConstraintReason::Expr(expr.id, expr.span),
            ),
            ExprKind::StringFmt { segments } => {
                // Collect interpolated segment expression terms so solver can
                // validate supported formatting operand types later.
                for segment in segments {
                    if let StringFmtSegment::Expr { expr, .. } = segment {
                        self.collect_expr(expr, None);
                    }
                }
                self.push_eq(
                    expr_ty.clone(),
                    Type::String,
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::HeapAlloc { expr: inner } => {
                let inner_ty = self.collect_expr(inner, None);
                self.push_eq(
                    expr_ty.clone(),
                    Type::Heap {
                        elem_ty: Box::new(inner_ty),
                    },
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::Move { expr: inner }
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
                                Type::Unit,
                                ConstraintReason::Expr(expr.id, expr.span),
                            );
                        }
                    } else {
                        self.push_eq(
                            expr_ty.clone(),
                            Type::Unit,
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                    }
                }
            }
            ExprKind::TupleLit(fields) => {
                let mut field_terms = Vec::with_capacity(fields.len());
                for field in fields {
                    field_terms.push(self.collect_expr(field, None));
                }
                self.push_eq(
                    expr_ty.clone(),
                    Type::Tuple {
                        field_tys: field_terms,
                    },
                    ConstraintReason::Expr(expr.id, expr.span),
                );
            }
            ExprKind::ArrayLit { init, .. } => match init {
                crate::tree::resolved::ArrayLitInit::Elems(elems) => {
                    let elem_term = if let ExprKind::ArrayLit {
                        elem_ty: Some(explicit_elem_ty),
                        ..
                    } = &expr.kind
                    {
                        self.resolve_type_in_scope(explicit_elem_ty)
                            .unwrap_or_else(|_| self.fresh_var_term())
                    } else {
                        self.fresh_var_term()
                    };
                    for elem in elems {
                        let value_ty = self.collect_expr(elem, Some(elem_term.clone()));
                        self.push_assignable(
                            value_ty,
                            elem_term.clone(),
                            ConstraintReason::Expr(elem.id, elem.span),
                        );
                    }
                    self.push_eq(
                        expr_ty.clone(),
                        array_type_from_elem(&elem_term, elems.len()),
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                }
                crate::tree::resolved::ArrayLitInit::Repeat(value, count) => {
                    let elem_term = if let ExprKind::ArrayLit {
                        elem_ty: Some(explicit_elem_ty),
                        ..
                    } = &expr.kind
                    {
                        self.resolve_type_in_scope(explicit_elem_ty)
                            .unwrap_or_else(|_| self.fresh_var_term())
                    } else {
                        self.fresh_var_term()
                    };
                    let value_ty = self.collect_expr(value, Some(elem_term.clone()));
                    self.push_assignable(
                        value_ty,
                        elem_term.clone(),
                        ConstraintReason::Expr(value.id, value.span),
                    );
                    self.push_eq(
                        expr_ty.clone(),
                        array_type_from_elem(&elem_term, *count as usize),
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                }
            },
            ExprKind::StructLit {
                name,
                type_args,
                fields,
            } => {
                // Resolve nominal struct type (including generic instantiation)
                // when possible, then type fields against known field types.
                let known_struct = self
                    .type_defs
                    .get(name)
                    .cloned()
                    .or_else(|| self.resolve_named_type_instance(name, type_args));
                if let Some(Type::Struct {
                    fields: struct_fields,
                    ..
                }) = known_struct.as_ref()
                {
                    self.push_eq(
                        expr_ty.clone(),
                        known_struct.clone().expect("known struct type"),
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                    for field in fields {
                        let expected = struct_fields
                            .iter()
                            .find(|f| f.name == field.name)
                            .map(|f| f.ty.clone());
                        self.collect_expr(&field.value, expected);
                    }
                } else {
                    for field in fields {
                        self.collect_expr(&field.value, None);
                    }
                }
            }
            ExprKind::EnumVariant {
                enum_name,
                type_args,
                variant,
                payload,
            } => {
                // Resolve nominal enum type (including generic instantiation),
                // and collect payload terms with per-position expected types.
                let known_enum = self
                    .type_defs
                    .get(enum_name)
                    .cloned()
                    .or_else(|| self.resolve_named_type_instance(enum_name, type_args));
                let variant_payload_tys = if let Some(Type::Enum { variants, .. }) = &known_enum {
                    variants
                        .iter()
                        .find(|v| v.name == *variant)
                        .map(|v| v.payload.clone())
                } else {
                    None
                };
                let mut payload_terms = Vec::with_capacity(payload.len());
                let mut payload_nodes = Vec::with_capacity(payload.len());
                let mut payload_spans = Vec::with_capacity(payload.len());
                for (index, item) in payload.iter().enumerate() {
                    payload_nodes.push(item.id);
                    payload_spans.push(item.span);
                    let expected = variant_payload_tys
                        .as_ref()
                        .and_then(|tys| tys.get(index))
                        .cloned();
                    payload_terms.push(self.collect_expr(item, expected));
                }
                if let Some(enum_ty) = known_enum {
                    self.push_eq(
                        expr_ty.clone(),
                        enum_ty,
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                }
                self.out
                    .expr_obligations
                    .push(ExprObligation::EnumVariantPayload {
                        expr_id: expr.id,
                        enum_name: enum_name.clone(),
                        variant: variant.clone(),
                        payload: payload_terms,
                        payload_nodes,
                        payload_spans,
                        span: expr.span,
                    });
            }
            ExprKind::StructUpdate { target, fields } => {
                let target_ty = self.collect_expr(target, None);
                let mut field_terms = Vec::with_capacity(fields.len());
                for field in fields {
                    let value_ty = self.collect_expr(&field.value, None);
                    field_terms.push((field.name.clone(), value_ty));
                }
                self.out
                    .expr_obligations
                    .push(ExprObligation::StructUpdate {
                        expr_id: expr.id,
                        target: target_ty,
                        fields: field_terms,
                        result: expr_ty.clone(),
                        span: expr.span,
                    });
            }
            ExprKind::ArrayIndex { target, indices } => {
                let target_ty = self.collect_expr(target, None);
                let mut index_terms = Vec::with_capacity(indices.len());
                let mut index_nodes = Vec::with_capacity(indices.len());
                let mut index_spans = Vec::with_capacity(indices.len());
                for index in indices {
                    index_nodes.push(index.id);
                    index_spans.push(index.span);
                    index_terms.push(self.collect_expr(index, Some(Type::uint(64))));
                }
                self.out.expr_obligations.push(ExprObligation::ArrayIndex {
                    expr_id: expr.id,
                    target: target_ty,
                    indices: index_terms,
                    index_nodes,
                    index_spans,
                    result: expr_ty.clone(),
                    span: expr.span,
                });
            }
            ExprKind::TupleField { target, index } => {
                let target_ty = self.collect_expr(target, None);
                self.out.expr_obligations.push(ExprObligation::TupleField {
                    expr_id: expr.id,
                    target: target_ty,
                    index: *index,
                    result: expr_ty.clone(),
                    span: expr.span,
                });
            }
            ExprKind::StructField { target, field } => {
                let target_ty = self.collect_expr(target, None);
                self.out.expr_obligations.push(ExprObligation::StructField {
                    expr_id: expr.id,
                    target: target_ty,
                    field: field.clone(),
                    result: expr_ty.clone(),
                    span: expr.span,
                });
            }
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.collect_expr(cond, Some(Type::Bool));
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
                let start_ty = self.collect_expr(start, Some(Type::uint(64)));
                let end_ty = self.collect_expr(end, Some(Type::uint(64)));
                self.push_eq(
                    expr_ty.clone(),
                    Type::Range {
                        elem_ty: Box::new(Type::uint(64)),
                    },
                    ConstraintReason::Expr(expr.id, expr.span),
                );
                self.out.expr_obligations.push(ExprObligation::Range {
                    expr_id: expr.id,
                    start: start_ty,
                    end: end_ty,
                    result: expr_ty.clone(),
                    span: expr.span,
                });
            }
            ExprKind::Slice { target, start, end } => {
                let target_ty = self.collect_expr(target, None);
                let start_ty = start
                    .as_ref()
                    .map(|start| self.collect_expr(start, Some(Type::uint(64))));
                let end_ty = end
                    .as_ref()
                    .map(|end| self.collect_expr(end, Some(Type::uint(64))));
                self.out.expr_obligations.push(ExprObligation::Slice {
                    expr_id: expr.id,
                    target: target_ty,
                    start: start_ty,
                    end: end_ty,
                    result: expr_ty.clone(),
                    span: expr.span,
                });
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
                // Calls are represented as obligations because overload/generic
                // resolution needs solver-time type information.
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
                    callee_ty: None,
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
                let inner_expected = match op {
                    UnaryOp::Neg | UnaryOp::BitNot => expected.clone(),
                    UnaryOp::LogicalNot => None,
                };
                let inner_ty = self.collect_expr(inner, inner_expected);
                self.out.expr_obligations.push(ExprObligation::UnaryOp {
                    expr_id: expr.id,
                    op: *op,
                    operand: inner_ty.clone(),
                    result: expr_ty.clone(),
                    span: expr.span,
                });
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
                            Type::Bool,
                            ConstraintReason::Expr(expr.id, expr.span),
                        );
                        self.push_eq(
                            expr_ty.clone(),
                            Type::Bool,
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
                        fn_ty,
                        ConstraintReason::Expr(expr.id, expr.span),
                    );
                }
                let closure_def_ty = self.def_term(*def_id);
                self.push_eq(
                    closure_def_ty,
                    expr_ty.clone(),
                    ConstraintReason::Decl(*def_id, expr.span),
                );

                let return_term = fn_ty
                    .as_ref()
                    .and_then(|ty| match ty {
                        Type::Fn { ret_ty, .. } => Some((**ret_ty).clone()),
                        _ => None,
                    })
                    .unwrap_or_else(|| self.fresh_var_term());

                self.enter_callable(*def_id, return_term.clone(), expr.span);
                for param in params {
                    let param_term = self.def_term(param.def_id);
                    if let Ok(param_ty) = self.resolve_type_in_scope(&param.typ) {
                        self.push_eq(
                            param_term.clone(),
                            param_ty,
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

    fn collect_match_arm(&mut self, arm: &MatchArm, expected: Option<Type>) -> Type {
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
        let callee_ty = self.collect_expr(callee, None);
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
            callee_ty: Some(callee_ty),
            receiver: None,
            arg_terms,
            ret_ty,
        });
    }

    fn collect_binop_constraints(&mut self, expr: &Expr, left_ty: Type, right_ty: Type) {
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
                // For explicitly typed declarations, bind pattern variables to
                // the declared type (not the raw value type) so refinements are
                // preserved and runtime checks can be emitted downstream.
                let expected_decl_ty = decl_ty
                    .as_ref()
                    .and_then(|decl_ty| self.resolve_type_in_scope(decl_ty).ok());
                let value_ty = self.collect_expr(value, expected_decl_ty.clone());
                if let Some(decl_ty) = expected_decl_ty.clone() {
                    self.push_assignable(
                        value_ty.clone(),
                        decl_ty.clone(),
                        ConstraintReason::Stmt(stmt.id, stmt.span),
                    );
                    self.collect_bind_pattern(pattern, decl_ty);
                } else {
                    self.collect_bind_pattern(pattern, value_ty);
                }
            }
            StmtExprKind::VarDecl {
                def_id, decl_ty, ..
            } => {
                let def_term = self.def_term(*def_id);
                if let Ok(ty) = self.resolve_type_in_scope(decl_ty) {
                    self.push_eq(def_term, ty, ConstraintReason::Decl(*def_id, stmt.span));
                }
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                // Property/field assignment has dedicated obligations so setter
                // accessibility and field-level typing can be checked precisely.
                if let ExprKind::StructField { target, field } = &assignee.kind {
                    let target_ty = self.collect_expr(target, None);
                    let assignee_ty = self.node_term(assignee.id);
                    let value_ty = self.collect_expr(value, None);
                    self.out
                        .expr_obligations
                        .push(ExprObligation::StructFieldAssign {
                            stmt_id: stmt.id,
                            target: target_ty,
                            field: field.clone(),
                            assignee: assignee_ty,
                            value: value_ty,
                            span: stmt.span,
                        });
                } else {
                    let lhs_ty = self.collect_expr(assignee, None);
                    let rhs_ty = self.collect_expr(value, Some(lhs_ty.clone()));
                    self.push_assignable(
                        rhs_ty,
                        lhs_ty,
                        ConstraintReason::Stmt(stmt.id, stmt.span),
                    );
                }
            }
            StmtExprKind::While { cond, body } => {
                self.collect_expr(cond, Some(Type::Bool));
                self.enter_loop();
                self.collect_expr(body, Some(Type::Unit));
                self.exit_loop();
            }
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                let iter_ty = self.collect_expr(iter, None);
                let pattern_ty = self.fresh_var_term();
                self.collect_bind_pattern(pattern, pattern_ty.clone());
                self.out.expr_obligations.push(ExprObligation::ForIter {
                    stmt_id: stmt.id,
                    iter: iter_ty,
                    pattern: pattern_ty,
                    span: stmt.span,
                });
                self.enter_loop();
                self.collect_expr(body, Some(Type::Unit));
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
                    let value_expected = if matches!(expected_return, Some(Type::Unit)) {
                        None
                    } else {
                        expected_return.clone()
                    };
                    let value_ty = self.collect_expr(value, value_expected);
                    if let Some(expected_return) = expected_return.clone() {
                        if !matches!(expected_return, Type::Unit) {
                            self.push_assignable(
                                value_ty,
                                expected_return,
                                ConstraintReason::Return(stmt.id, stmt.span),
                            );
                        }
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

    fn collect_bind_pattern(&mut self, pattern: &BindPattern, value_ty: Type) {
        // Record a generic bind obligation for structural pattern diagnostics.
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
                self.push_eq(
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
            BindPatternKind::Tuple { patterns } => {
                let field_terms = patterns
                    .iter()
                    .map(|_| self.fresh_var_term())
                    .collect::<Vec<_>>();
                self.push_eq(
                    value_ty.clone(),
                    Type::Tuple {
                        field_tys: field_terms.clone(),
                    },
                    ConstraintReason::Pattern(pattern.id, pattern.span),
                );
                for (child, child_term) in patterns.iter().zip(field_terms.into_iter()) {
                    self.collect_bind_pattern(child, child_term);
                }
            }
            BindPatternKind::Array { patterns } => {
                let elem_term = self.fresh_var_term();
                self.push_eq(
                    value_ty.clone(),
                    Type::Array {
                        elem_ty: Box::new(elem_term.clone()),
                        dims: vec![patterns.len()],
                    },
                    ConstraintReason::Pattern(pattern.id, pattern.span),
                );
                for child in patterns {
                    self.collect_bind_pattern(child, elem_term.clone());
                }
            }
            BindPatternKind::Struct {
                name: type_name,
                fields,
            } => {
                if let Some(Type::Struct {
                    fields: struct_fields,
                    ..
                }) = self.type_defs.get(type_name)
                {
                    self.push_assignable(
                        value_ty.clone(),
                        Type::Struct {
                            name: type_name.clone(),
                            fields: struct_fields.clone(),
                        },
                        ConstraintReason::Pattern(pattern.id, pattern.span),
                    );
                    for StructFieldBindPattern {
                        name,
                        pattern: child,
                        ..
                    } in fields
                    {
                        if let Some(field_ty) = struct_fields
                            .iter()
                            .find(|field| field.name == *name)
                            .map(|field| field.ty.clone())
                        {
                            self.collect_bind_pattern(child, field_ty);
                        }
                    }
                } else {
                    for StructFieldBindPattern { pattern: child, .. } in fields {
                        let child_ty = self.fresh_var_term();
                        self.collect_bind_pattern(child, child_ty);
                    }
                }
            }
        }
    }

    fn node_term(&mut self, node_id: NodeId) -> Type {
        if let Some(term) = self.out.node_terms.get(&node_id) {
            return term.clone();
        }
        let term = self.fresh_var_term();
        self.out.node_terms.insert(node_id, term.clone());
        term
    }

    fn def_term(&mut self, def_id: DefId) -> Type {
        if let Some(term) = self.out.def_terms.get(&def_id) {
            return term.clone();
        }
        let term = self.fresh_var_term();
        self.out.def_terms.insert(def_id, term.clone());
        term
    }

    fn fresh_var_term(&mut self) -> Type {
        Type::Var(self.vars.fresh_infer_local())
    }

    fn fresh_int_var_term(&mut self) -> Type {
        Type::Var(self.vars.fresh_infer_int())
    }

    fn push_eq(&mut self, left: Type, right: Type, reason: ConstraintReason) {
        self.out.constraints.push(Constraint::Eq {
            left,
            right,
            reason,
        });
    }

    fn push_assignable(&mut self, from: Type, to: Type, reason: ConstraintReason) {
        self.out
            .constraints
            .push(Constraint::Assignable { from, to, reason });
    }

    fn enter_callable(&mut self, def_id: DefId, return_ty: Type, span: Span) {
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

    fn current_return_ty(&self) -> Option<Type> {
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
        let params = self.resolve_fn_params(params)?;
        self.resolve_fn_type(params, return_ty)
    }

    fn collect_function_signature(&self, sig: &crate::tree::resolved::FunctionSig) -> Option<Type> {
        let params = self.resolve_fn_params(&sig.params)?;
        self.resolve_fn_type(params, &sig.ret_ty_expr)
    }

    fn collect_method_signature(&self, type_name: &str, sig: &MethodSig) -> Option<Type> {
        let self_ty = self.type_defs.get(type_name).cloned()?;
        let tail_params = self.resolve_fn_params(&sig.params)?;
        let mut params = Vec::with_capacity(sig.params.len() + 1);
        params.push(crate::types::FnParam {
            mode: map_param_mode(sig.self_param.mode.clone()),
            ty: self_ty,
        });
        params.extend(tail_params);
        self.resolve_fn_type(params, &sig.ret_ty_expr)
    }

    fn resolve_fn_params(
        &self,
        params: &[crate::tree::resolved::Param],
    ) -> Option<Vec<crate::types::FnParam>> {
        params
            .iter()
            .map(|param| {
                self.resolve_type_in_scope(&param.typ)
                    .ok()
                    .map(|ty| crate::types::FnParam {
                        mode: map_param_mode(param.mode.clone()),
                        ty,
                    })
            })
            .collect::<Option<Vec<_>>>()
    }

    fn resolve_fn_type(
        &self,
        params: Vec<crate::types::FnParam>,
        return_ty: &TypeExpr,
    ) -> Option<Type> {
        let ret_ty = self.resolve_type_in_scope(return_ty).ok()?;
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

fn map_param_mode(mode: crate::tree::ParamMode) -> crate::types::FnParamMode {
    match mode {
        crate::tree::ParamMode::In => crate::types::FnParamMode::In,
        crate::tree::ParamMode::InOut => crate::types::FnParamMode::InOut,
        crate::tree::ParamMode::Out => crate::types::FnParamMode::Out,
        crate::tree::ParamMode::Sink => crate::types::FnParamMode::Sink,
    }
}

fn array_type_from_elem(elem_ty: &Type, len: usize) -> Type {
    match elem_ty.clone() {
        Type::Array { elem_ty, dims } => {
            let mut merged_dims = Vec::with_capacity(dims.len() + 1);
            merged_dims.push(len);
            merged_dims.extend(dims);
            Type::Array {
                elem_ty,
                dims: merged_dims,
            }
        }
        elem_ty => Type::Array {
            elem_ty: Box::new(elem_ty),
            dims: vec![len],
        },
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

    #[test]
    fn test_collect_expr_obligations() {
        let source = r#"
            fn test() -> u64 {
                let arr = [1, 2, 3];
                let x = (arr[0] + 1) * 2;
                for item in arr {
                    if item > 0 {
                        break;
                    }
                }
                x
            }
        "#;

        let out = collect_constraints(source);
        assert!(
            out.expr_obligations
                .iter()
                .any(|ob| matches!(ob, ExprObligation::ArrayIndex { .. }))
        );
        assert!(
            out.expr_obligations
                .iter()
                .any(|ob| matches!(ob, ExprObligation::BinOp { .. }))
        );
        assert!(
            out.expr_obligations
                .iter()
                .any(|ob| matches!(ob, ExprObligation::ForIter { .. }))
        );
    }
}
