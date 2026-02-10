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
use crate::typecheck::type_map::{
    resolve_return_type_expr_with_params, resolve_type_def_with_args, resolve_type_expr_with_params,
};
use crate::types::{TyVarId, Type};

use super::typesys::TypeVarStore;
use super::utils::fn_param_mode;

mod calls;
mod decls;
mod expr;
mod patterns;
mod signatures;
mod stmt;
mod type_instances;

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
    Try {
        expr_id: NodeId,
        operand: Type,
        result: Type,
        expected_return_ty: Option<Type>,
        callable_def_id: Option<DefId>,
        span: Span,
    },
    Join {
        expr_id: NodeId,
        arms: Vec<Type>,
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
    SetElemType {
        expr_id: NodeId,
        elem_ty: Type,
        span: Span,
    },
    MapKeyType {
        expr_id: NodeId,
        key_ty: Type,
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
        caller_def_id: Option<DefId>,
        span: Span,
    },
    StructConstruct {
        expr_id: NodeId,
        type_name: String,
        caller_def_id: Option<DefId>,
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
        caller_def_id: Option<DefId>,
        span: Span,
    },
    StructFieldAssign {
        stmt_id: NodeId,
        assignee_expr_id: NodeId,
        target: Type,
        field: String,
        assignee: Type,
        value: Type,
        caller_def_id: Option<DefId>,
        span: Span,
    },
    MapIndexAssign {
        stmt_id: NodeId,
        target: Type,
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
    pub(crate) caller_def_id: Option<DefId>,
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
        caller_def_id: Option<DefId>,
        span: Span,
    },
    MatchArm {
        arm_id: NodeId,
        pattern: MatchPattern,
        scrutinee_ty: Type,
        caller_def_id: Option<DefId>,
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
    pub(crate) var_trait_bounds: HashMap<TyVarId, Vec<String>>,
}

#[derive(Debug, Clone)]
struct ControlContext {
    def_id: DefId,
    return_ty: Type,
    loop_depth: usize,
}

#[derive(Debug, Clone)]
struct ClosureSigInfo {
    fn_ty: Type,
    param_tys: Vec<Type>,
    ret_ty: Type,
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
            .map(|param| (param.def_id, self.vars.fresh_rigid_param(param.def_id)))
            .collect::<HashMap<_, _>>();

        for param in type_params {
            if let Some(bound) = &param.bound
                && let Some(var) = mapping.get(&param.def_id)
            {
                self.out
                    .var_trait_bounds
                    .entry(*var)
                    .or_default()
                    .push(bound.name.clone());
            }
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

    fn resolve_return_type_in_scope(&self, ty_expr: &TypeExpr) -> Result<Type, TypeCheckError> {
        resolve_return_type_expr_with_params(
            &self.ctx.def_table,
            &self.ctx.module,
            ty_expr,
            self.current_type_params(),
        )
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
            def_id,
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

    fn current_callable_def_id(&self) -> Option<DefId> {
        self.control_stack.last().map(|ctx| ctx.def_id)
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

fn nominal_base_name(ty: &Type) -> Option<&str> {
    let name = match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name.as_str(),
        _ => return None,
    };
    Some(name.split('<').next().unwrap_or(name).trim())
}

fn fn_type_return(ty: &Type) -> Option<Type> {
    let Type::Fn { ret_ty, .. } = ty else {
        return None;
    };
    Some((**ret_ty).clone())
}

fn is_synthesized_missing_else(then_body: &Expr, else_body: &Expr) -> bool {
    then_body.span == else_body.span
        && matches!(
            else_body.kind,
            ExprKind::Block {
                items: ref block_items,
                tail: None
            } if block_items.is_empty()
        )
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
