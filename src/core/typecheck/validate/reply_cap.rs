use std::collections::HashMap;

use crate::core::analysis::dataflow::{DataflowGraph, solve_forward};
use crate::core::resolve::DefId;
use crate::core::tree::cfg::{AstBlockId, TreeCfgBuilder, TreeCfgItem, TreeCfgNode};
use crate::core::tree::visit::{self, Visitor};
use crate::core::tree::{ExprKind, MethodItem, NodeId};
use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::{TEK, TypeCheckError};
use crate::core::typecheck::type_map::resolve_type_expr;
use crate::core::types::{Type, TypeAssignability, type_assignable};

pub(super) fn check_reply_cap_usage(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
    let mut errors = Vec::new();
    let node_types = &engine.state().solve.resolved_node_types;

    let mut outside_collector = ReplyOutsideHandlerCollector::default();
    outside_collector.visit_module(&engine.context().module);
    errors.extend(outside_collector.errors);

    for method_block in engine.context().module.method_blocks() {
        if !method_block.type_name.starts_with("__ts_") {
            continue;
        }
        for method_item in &method_block.method_items {
            let MethodItem::Def(method_def) = method_item else {
                continue;
            };
            if !method_def.sig.name.starts_with("__ts_on_") {
                continue;
            }
            let cap_params = collect_handler_reply_caps(
                &engine.context().def_table,
                &engine.context().module,
                method_def,
            );
            errors.extend(check_handler_reply_calls(
                method_def,
                &cap_params,
                node_types,
            ));
            errors.extend(check_handler_reply_cap_linearity(method_def, &cap_params));
        }
    }

    errors
}

#[derive(Default)]
struct ReplyOutsideHandlerCollector {
    errors: Vec<TypeCheckError>,
    in_typestate_method_block: bool,
    in_typestate_handler: bool,
}

impl Visitor<DefId, ()> for ReplyOutsideHandlerCollector {
    fn visit_method_block(&mut self, method_block: &crate::core::tree::resolved::MethodBlock) {
        let prev = self.in_typestate_method_block;
        self.in_typestate_method_block = method_block.type_name.starts_with("__ts_");
        visit::walk_method_block(self, method_block);
        self.in_typestate_method_block = prev;
    }

    fn visit_method_def(&mut self, method_def: &crate::core::tree::resolved::MethodDef) {
        let prev = self.in_typestate_handler;
        self.in_typestate_handler =
            self.in_typestate_method_block && method_def.sig.name.starts_with("__ts_on_");
        visit::walk_method_def(self, method_def);
        self.in_typestate_handler = prev;
    }

    fn visit_expr(&mut self, expr: &crate::core::tree::resolved::Expr) {
        if matches!(expr.kind, ExprKind::Reply { .. }) && !self.in_typestate_handler {
            self.errors
                .push(TEK::ReplyOutsideHandler.at(expr.span).into());
        }
        visit::walk_expr(self, expr);
    }
}

#[derive(Clone, Debug)]
struct ReplyCapParam {
    def_id: DefId,
    name: String,
    response_tys: Vec<Type>,
    span: crate::core::diag::Span,
}

fn collect_handler_reply_caps(
    def_table: &crate::core::resolve::DefTable,
    module: &crate::core::tree::resolved::Module,
    method_def: &crate::core::tree::resolved::MethodDef,
) -> Vec<ReplyCapParam> {
    let mut caps = Vec::new();
    for param in &method_def.sig.params {
        let Ok(param_ty) = resolve_type_expr(def_table, module, &param.typ) else {
            continue;
        };
        if let Type::ReplyCap { response_tys } = param_ty {
            caps.push(ReplyCapParam {
                def_id: param.def_id,
                name: param.ident.clone(),
                response_tys,
                span: param.span,
            });
        }
    }
    caps
}

#[derive(Clone, Debug)]
struct ReplySite {
    span: crate::core::diag::Span,
    cap_node: NodeId,
    cap_span: crate::core::diag::Span,
    cap_def_id: Option<DefId>,
    value_node: NodeId,
}

fn check_handler_reply_calls(
    method_def: &crate::core::tree::resolved::MethodDef,
    cap_params: &[ReplyCapParam],
    node_types: &HashMap<NodeId, Type>,
) -> Vec<TypeCheckError> {
    let mut errors = Vec::new();
    let cap_params_by_id: HashMap<DefId, &ReplyCapParam> =
        cap_params.iter().map(|cap| (cap.def_id, cap)).collect();
    let mut sites = Vec::new();
    collect_reply_sites_from_expr(&method_def.body, &mut sites);

    for site in sites {
        let Some(cap_ty) = node_types.get(&site.cap_node) else {
            continue;
        };
        let Type::ReplyCap { .. } = cap_ty else {
            crate::core::typecheck::tc_push_error!(
                errors,
                site.cap_span,
                TEK::ReplyCapExpected(cap_ty.clone())
            );
            continue;
        };

        let Some(cap_def_id) = site.cap_def_id else {
            crate::core::typecheck::tc_push_error!(
                errors,
                site.cap_span,
                TEK::ReplyCapParamRequired
            );
            continue;
        };
        let Some(cap_param) = cap_params_by_id.get(&cap_def_id).copied() else {
            crate::core::typecheck::tc_push_error!(
                errors,
                site.cap_span,
                TEK::ReplyCapParamRequired
            );
            continue;
        };

        let Some(value_ty) = node_types.get(&site.value_node) else {
            continue;
        };
        let allowed = cap_param
            .response_tys
            .iter()
            .any(|expected| type_assignable(value_ty, expected) != TypeAssignability::Incompatible);
        if !allowed {
            crate::core::typecheck::tc_push_error!(
                errors,
                site.span,
                TEK::ReplyPayloadNotAllowed(value_ty.clone(), cap_param.response_tys.clone(),)
            );
        }
    }

    errors
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ReplyCapFlowState {
    Available,
    Consumed,
    MaybeConsumed,
    InvalidDoubleConsume,
}

fn check_handler_reply_cap_linearity(
    method_def: &crate::core::tree::resolved::MethodDef,
    cap_params: &[ReplyCapParam],
) -> Vec<TypeCheckError> {
    if cap_params.is_empty() {
        return Vec::new();
    }

    let cfg = TreeCfgBuilder::new().build_from_expr(&method_def.body);
    if cfg.num_nodes() == 0 {
        return Vec::new();
    }

    let reachable = reachable_cfg_nodes(&cfg, AstBlockId(0));
    let mut errors = Vec::new();
    for cap in cap_params {
        let consumes_by_node: Vec<Vec<crate::core::diag::Span>> = cfg
            .nodes
            .iter()
            .map(|node| collect_reply_consume_spans_for_cap(node, cap.def_id))
            .collect();

        let dataflow = solve_forward(
            &cfg,
            AstBlockId(0),
            ReplyCapFlowState::Available,
            ReplyCapFlowState::Available,
            meet_reply_cap_state,
            |node, in_state| {
                let mut state = *in_state;
                for _ in 0..consumes_by_node[node.0].len() {
                    state = apply_reply_cap_consume(state);
                }
                state
            },
        );

        for (idx, spans) in consumes_by_node.iter().enumerate() {
            if !reachable[idx] {
                continue;
            }
            let mut state = dataflow.in_map[idx];
            for span in spans {
                if matches!(
                    state,
                    ReplyCapFlowState::Consumed
                        | ReplyCapFlowState::MaybeConsumed
                        | ReplyCapFlowState::InvalidDoubleConsume
                ) {
                    crate::core::typecheck::tc_push_error!(
                        errors,
                        *span,
                        TEK::ReplyCapConsumedMultipleTimes(cap.name.clone())
                    );
                    state = ReplyCapFlowState::InvalidDoubleConsume;
                } else {
                    state = apply_reply_cap_consume(state);
                }
            }
        }

        let mut missing_on_some_path = false;
        for idx in 0..cfg.num_nodes() {
            if !reachable[idx] {
                continue;
            }
            let node = AstBlockId(idx);
            if !cfg.succs(node).is_empty() {
                continue;
            }
            if !matches!(
                dataflow.out_map[idx],
                ReplyCapFlowState::Consumed | ReplyCapFlowState::InvalidDoubleConsume
            ) {
                missing_on_some_path = true;
                break;
            }
        }
        if missing_on_some_path {
            crate::core::typecheck::tc_push_error!(
                errors,
                cap.span,
                TEK::ReplyCapMustBeConsumed(cap.name.clone())
            );
        }
    }

    errors
}

fn collect_reply_consume_spans_for_cap(
    node: &TreeCfgNode<'_>,
    cap_def_id: DefId,
) -> Vec<crate::core::diag::Span> {
    let mut out = Vec::new();
    for item in &node.items {
        let mut sites = Vec::new();
        match item {
            TreeCfgItem::Stmt(stmt) => collect_reply_sites_from_stmt(stmt, &mut sites),
            TreeCfgItem::Expr(expr) => collect_reply_sites_from_expr(expr, &mut sites),
        }
        out.extend(
            sites
                .into_iter()
                .filter(|site| site.cap_def_id == Some(cap_def_id))
                .map(|site| site.span),
        );
    }
    out
}

fn meet_reply_cap_state(states: &[ReplyCapFlowState]) -> ReplyCapFlowState {
    let mut has_available = false;
    let mut has_consumed = false;
    let mut has_maybe = false;
    for state in states {
        match state {
            ReplyCapFlowState::InvalidDoubleConsume => {
                return ReplyCapFlowState::InvalidDoubleConsume;
            }
            ReplyCapFlowState::Available => has_available = true,
            ReplyCapFlowState::Consumed => has_consumed = true,
            ReplyCapFlowState::MaybeConsumed => has_maybe = true,
        }
    }
    if has_maybe || (has_available && has_consumed) {
        ReplyCapFlowState::MaybeConsumed
    } else if has_consumed {
        ReplyCapFlowState::Consumed
    } else {
        ReplyCapFlowState::Available
    }
}

fn apply_reply_cap_consume(state: ReplyCapFlowState) -> ReplyCapFlowState {
    match state {
        ReplyCapFlowState::Available => ReplyCapFlowState::Consumed,
        ReplyCapFlowState::Consumed
        | ReplyCapFlowState::MaybeConsumed
        | ReplyCapFlowState::InvalidDoubleConsume => ReplyCapFlowState::InvalidDoubleConsume,
    }
}

fn reachable_cfg_nodes(cfg: &crate::core::tree::cfg::TreeCfg<'_>, entry: AstBlockId) -> Vec<bool> {
    let mut reachable = vec![false; cfg.num_nodes()];
    if cfg.num_nodes() == 0 {
        return reachable;
    }

    let mut stack = vec![entry];
    while let Some(node) = stack.pop() {
        if reachable[node.0] {
            continue;
        }
        reachable[node.0] = true;
        stack.extend(cfg.succs(node).iter().copied());
    }
    reachable
}

fn collect_reply_sites_from_stmt(
    stmt: &crate::core::tree::resolved::StmtExpr,
    out: &mut Vec<ReplySite>,
) {
    let mut collector = ReplySiteCollector { out };
    collector.visit_stmt_expr(stmt);
}

fn collect_reply_sites_from_expr(
    expr: &crate::core::tree::resolved::Expr,
    out: &mut Vec<ReplySite>,
) {
    let mut collector = ReplySiteCollector { out };
    collector.visit_expr(expr);
}

struct ReplySiteCollector<'a> {
    out: &'a mut Vec<ReplySite>,
}

impl Visitor<DefId, ()> for ReplySiteCollector<'_> {
    fn visit_expr(&mut self, expr: &crate::core::tree::resolved::Expr) {
        if let ExprKind::Reply { cap, value } = &expr.kind {
            let cap_def_id = match &cap.kind {
                ExprKind::Var { def_id, .. } => Some(*def_id),
                _ => None,
            };
            self.out.push(ReplySite {
                span: expr.span,
                cap_node: cap.id,
                cap_span: cap.span,
                cap_def_id,
                value_node: value.id,
            });
        }
        visit::walk_expr(self, expr);
    }
}
