//! Protocol/typestate progression fact extraction for semck.
//!
//! This module builds structured progression facts once per semcheck run, so
//! later progression validators can consume indexed facts instead of re-scanning
//! typestate handlers and rebuilding CFG/event views ad hoc.

use std::collections::HashMap;

use crate::core::analysis::dataflow::DataflowGraph;
use crate::core::context::{
    ProtocolHandlerProgressionFact, ProtocolProgressionCfg, ProtocolProgressionEmit,
    ProtocolProgressionEvent, ProtocolProgressionFacts, ProtocolProgressionReturnState,
    ProtocolProgressionState, ProtocolProgressionStateKey, SemCheckNormalizedContext,
};
use crate::core::tree::cfg::{AstBlockId, TreeCfgBuilder, TreeCfgItem};
use crate::core::tree::normalized::{Expr, ExprKind, MethodItem, StmtExprKind};
use crate::core::typecheck::type_map::resolve_type_expr;
use crate::core::types::{Type, TypeId};

pub(super) fn extract(ctx: &SemCheckNormalizedContext) -> ProtocolProgressionFacts {
    let mut out = ProtocolProgressionFacts::default();

    for method_block in ctx.module.method_blocks() {
        let Some((typestate_name, state_name)) =
            parse_typestate_and_state_from_generated_state(&method_block.type_name)
        else {
            continue;
        };
        let Some(bindings) = ctx.protocol_index.typestate_bindings.get(&typestate_name) else {
            continue;
        };

        for method_item in &method_block.method_items {
            let MethodItem::Def(method_def) = method_item else {
                continue;
            };
            if !method_def.sig.name.starts_with("__ts_on_") {
                continue;
            }
            let Some(selector_param) = method_def.sig.params.first() else {
                continue;
            };
            let Ok(selector_ty) =
                resolve_type_expr(&ctx.def_table, &ctx.module, &selector_param.typ)
            else {
                continue;
            };

            let base_cfg = build_handler_cfg(ctx, &typestate_name, &method_def.body);
            for binding in bindings {
                let peer_role_by_field: HashMap<&str, &str> = binding
                    .peer_role_bindings
                    .iter()
                    .map(|peer| (peer.field_name.as_str(), peer.role_name.as_str()))
                    .collect();
                let cfg = bind_cfg_roles(base_cfg.clone(), &peer_role_by_field);

                let fact = ProtocolHandlerProgressionFact {
                    handler_def_id: method_def.def_id,
                    typestate_name: typestate_name.clone(),
                    entry_state: ProtocolProgressionState {
                        protocol_name: binding.protocol_name.clone(),
                        role_name: binding.role_name.clone(),
                        state_name: state_name.clone(),
                    },
                    selector_ty: selector_ty.clone(),
                    cfg,
                    span: method_def.sig.span,
                };

                let fact_idx = out.handlers.len();
                out.handlers.push(fact);
                out.by_handler_def
                    .entry(method_def.def_id)
                    .or_default()
                    .push(fact_idx);
                out.by_state
                    .entry(ProtocolProgressionStateKey {
                        typestate_name: typestate_name.clone(),
                        protocol_name: binding.protocol_name.clone(),
                        role_name: binding.role_name.clone(),
                        state_name: state_name.clone(),
                    })
                    .or_default()
                    .push(fact_idx);
            }
        }
    }

    out
}

fn build_handler_cfg(
    ctx: &SemCheckNormalizedContext,
    typestate_name: &str,
    body: &Expr,
) -> ProtocolProgressionCfg {
    let cfg = TreeCfgBuilder::<TypeId>::new().build_from_expr(body);
    let succs = (0..cfg.num_nodes())
        .map(|idx| cfg.succs(AstBlockId(idx)).iter().map(|n| n.0).collect())
        .collect::<Vec<Vec<usize>>>();
    let exit_blocks = (0..cfg.num_nodes())
        .filter(|idx| cfg.succs(AstBlockId(*idx)).is_empty())
        .collect::<Vec<usize>>();

    let mut node_events = HashMap::<usize, Vec<ProtocolProgressionEvent>>::new();
    for (idx, node) in cfg.nodes.iter().enumerate() {
        let mut events = Vec::new();
        for item in &node.items {
            collect_item_events(ctx, typestate_name, item, &mut events);
        }
        if !events.is_empty() {
            node_events.insert(idx, events);
        }
    }

    ProtocolProgressionCfg {
        entry_block: 0,
        succs,
        exit_blocks,
        node_events,
    }
}

fn collect_item_events(
    ctx: &SemCheckNormalizedContext,
    typestate_name: &str,
    item: &TreeCfgItem<'_, TypeId>,
    out: &mut Vec<ProtocolProgressionEvent>,
) {
    match item {
        TreeCfgItem::Stmt(stmt) => match &stmt.kind {
            StmtExprKind::LetBind { value, .. } | StmtExprKind::VarBind { value, .. } => {
                if let Some(emit) = emit_event(ctx, value) {
                    out.push(ProtocolProgressionEvent::Emit(emit));
                }
            }
            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                if let Some(emit) = emit_event(ctx, assignee) {
                    out.push(ProtocolProgressionEvent::Emit(emit));
                }
                if let Some(emit) = emit_event(ctx, value) {
                    out.push(ProtocolProgressionEvent::Emit(emit));
                }
            }
            StmtExprKind::Return {
                value: Some(expr), ..
            } => {
                if let Some(emit) = emit_event(ctx, expr) {
                    out.push(ProtocolProgressionEvent::Emit(emit));
                }
                if let Some(to_state_name) = return_state_name(typestate_name, expr) {
                    out.push(ProtocolProgressionEvent::ReturnState(
                        ProtocolProgressionReturnState {
                            to_state_name: Some(to_state_name),
                            span: expr.span,
                        },
                    ));
                }
            }
            _ => {}
        },
        TreeCfgItem::Expr(expr) => {
            if let Some(emit) = emit_event(ctx, expr) {
                out.push(ProtocolProgressionEvent::Emit(emit));
            }
            if let Some(to_state_name) = return_state_name(typestate_name, expr) {
                out.push(ProtocolProgressionEvent::ReturnState(
                    ProtocolProgressionReturnState {
                        to_state_name: Some(to_state_name),
                        span: expr.span,
                    },
                ));
            }
        }
    }
}

fn emit_event(ctx: &SemCheckNormalizedContext, expr: &Expr) -> Option<ProtocolProgressionEmit> {
    let ExprKind::Emit { kind } = &expr.kind else {
        return None;
    };
    let (to, payload, is_request) = match kind {
        crate::core::tree::EmitKind::Send { to, payload } => (to, payload, false),
        crate::core::tree::EmitKind::Request { to, payload, .. } => (to, payload, true),
    };
    let payload_ty = ctx.type_map.lookup_node_type(payload.id)?;
    let request_response_tys = if is_request {
        match ctx.type_map.lookup_node_type(expr.id) {
            Some(Type::Pending { response_tys }) => response_tys.clone(),
            _ => Vec::new(),
        }
    } else {
        Vec::new()
    };

    Some(ProtocolProgressionEmit {
        payload_ty,
        to_field_name: emit_destination_field_name(to),
        to_role_name: None,
        is_request,
        request_response_tys,
        span: expr.span,
    })
}

fn bind_cfg_roles(
    mut cfg: ProtocolProgressionCfg,
    peer_role_by_field: &HashMap<&str, &str>,
) -> ProtocolProgressionCfg {
    for events in cfg.node_events.values_mut() {
        for event in events {
            let ProtocolProgressionEvent::Emit(emit) = event else {
                continue;
            };
            emit.to_role_name = emit
                .to_field_name
                .as_deref()
                .and_then(|field| peer_role_by_field.get(field).copied())
                .map(ToString::to_string);
        }
    }
    cfg
}

fn emit_destination_field_name(expr: &Expr) -> Option<String> {
    let ExprKind::StructField { target, field } = &expr.kind else {
        return None;
    };
    let ExprKind::Var { ident, .. } = &target.kind else {
        return None;
    };
    if ident == "self" {
        Some(field.clone())
    } else {
        None
    }
}

fn return_state_name(typestate_name: &str, expr: &Expr) -> Option<String> {
    let ExprKind::StructLit { name, .. } = &expr.kind else {
        return None;
    };
    let (lit_typestate_name, state_name) = parse_typestate_and_state_from_generated_state(name)?;
    if lit_typestate_name == typestate_name {
        Some(state_name)
    } else {
        None
    }
}

fn parse_typestate_and_state_from_generated_state(type_name: &str) -> Option<(String, String)> {
    let rest = type_name.strip_prefix("__ts_")?;
    let (typestate_name, state_name) = rest.rsplit_once('_')?;
    Some((typestate_name.to_string(), state_name.to_string()))
}
