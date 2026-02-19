//! Typestate handler lowering helpers.
//!
//! This module owns:
//! - cloning parsed type/param nodes with fresh ids,
//! - lowering `on ...` handlers to synthetic methods, and
//! - handler command sugar rewrites (`send/request/reply`).

use crate::core::diag::Span;
use crate::core::tree::NodeIdGen;
use crate::core::tree::parsed::{
    self, CallArg, Expr, ExprKind, FuncDef, TypeExpr, TypeExprKind, TypestateOnHandler,
};
use crate::core::tree::visit_mut::{self, VisitorMut};
use crate::core::tree::{CallArgMode, InitInfo, ParamMode};

use super::{MACHINE_TARGET_ID_HELPER_FN, unit_expr};

pub(super) fn clone_type_expr_with_new_ids(ty: &TypeExpr, node_id_gen: &mut NodeIdGen) -> TypeExpr {
    let kind = match &ty.kind {
        TypeExprKind::Infer => TypeExprKind::Infer,
        TypeExprKind::Union { variants } => TypeExprKind::Union {
            variants: variants
                .iter()
                .map(|variant| clone_type_expr_with_new_ids(variant, node_id_gen))
                .collect(),
        },
        TypeExprKind::Named {
            ident,
            def_id,
            type_args,
        } => TypeExprKind::Named {
            ident: ident.clone(),
            def_id: *def_id,
            type_args: type_args
                .iter()
                .map(|arg| clone_type_expr_with_new_ids(arg, node_id_gen))
                .collect(),
        },
        TypeExprKind::Refined {
            base_ty_expr,
            refinements,
        } => TypeExprKind::Refined {
            base_ty_expr: Box::new(clone_type_expr_with_new_ids(base_ty_expr, node_id_gen)),
            refinements: refinements.clone(),
        },
        TypeExprKind::Array { elem_ty_expr, dims } => TypeExprKind::Array {
            elem_ty_expr: Box::new(clone_type_expr_with_new_ids(elem_ty_expr, node_id_gen)),
            dims: dims.clone(),
        },
        TypeExprKind::DynArray { elem_ty_expr } => TypeExprKind::DynArray {
            elem_ty_expr: Box::new(clone_type_expr_with_new_ids(elem_ty_expr, node_id_gen)),
        },
        TypeExprKind::Tuple { field_ty_exprs } => TypeExprKind::Tuple {
            field_ty_exprs: field_ty_exprs
                .iter()
                .map(|expr| clone_type_expr_with_new_ids(expr, node_id_gen))
                .collect(),
        },
        TypeExprKind::Slice { elem_ty_expr } => TypeExprKind::Slice {
            elem_ty_expr: Box::new(clone_type_expr_with_new_ids(elem_ty_expr, node_id_gen)),
        },
        TypeExprKind::Heap { elem_ty_expr } => TypeExprKind::Heap {
            elem_ty_expr: Box::new(clone_type_expr_with_new_ids(elem_ty_expr, node_id_gen)),
        },
        TypeExprKind::Ref {
            mutable,
            elem_ty_expr,
        } => TypeExprKind::Ref {
            mutable: *mutable,
            elem_ty_expr: Box::new(clone_type_expr_with_new_ids(elem_ty_expr, node_id_gen)),
        },
        TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => TypeExprKind::Fn {
            params: params
                .iter()
                .map(|param| parsed::FnTypeParam {
                    mode: param.mode.clone(),
                    ty_expr: clone_type_expr_with_new_ids(&param.ty_expr, node_id_gen),
                })
                .collect(),
            ret_ty_expr: Box::new(clone_type_expr_with_new_ids(ret_ty_expr, node_id_gen)),
        },
    };

    TypeExpr {
        id: node_id_gen.new_id(),
        kind,
        span: ty.span,
    }
}

pub(super) fn clone_param_with_new_ids(
    param: &parsed::Param,
    node_id_gen: &mut NodeIdGen,
) -> parsed::Param {
    parsed::Param {
        id: node_id_gen.new_id(),
        ident: param.ident.clone(),
        def_id: (),
        typ: clone_type_expr_with_new_ids(&param.typ, node_id_gen),
        mode: param.mode.clone(),
        span: param.span,
    }
}

pub(super) fn lower_handler_to_method_source(
    handler: &TypestateOnHandler,
    state_name: &str,
    next_index: &mut usize,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let provenance_site_suffix = handler
        .provenance
        .as_ref()
        .and_then(|provenance| provenance.request_site_label.as_deref())
        .map(|label| format!("__site_{label}"))
        .unwrap_or_default();
    // Preserve source-level provenance labels in generated handler symbols so
    // downstream machine-plan synthesis can recover deterministic site filters.
    let name = format!("__ts_on_{}{}", *next_index, provenance_site_suffix);
    *next_index += 1;
    let mut params = vec![parsed::Param {
        id: node_id_gen.new_id(),
        ident: "__event".to_string(),
        def_id: (),
        typ: handler.selector_ty.clone(),
        mode: ParamMode::In,
        span: handler.selector_ty.span,
    }];

    if handler.provenance.is_some() {
        // `for RequestType(binding)` handlers need response correlation id at
        // runtime so lowering can recover the originating request payload.
        // We model this via a hidden `Pending<Selector>` parameter.
        params.push(parsed::Param {
            id: node_id_gen.new_id(),
            ident: "__pending".to_string(),
            def_id: (),
            typ: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: "Pending".to_string(),
                    def_id: (),
                    type_args: vec![handler.selector_ty.clone()],
                },
                span: handler.selector_ty.span,
            },
            mode: ParamMode::In,
            span: handler.selector_ty.span,
        });
        if let Some(provenance) = &handler.provenance {
            params.push(provenance.param.clone());
        }
    }
    params.extend(handler.params.clone());
    let mut body = handler.body.clone();
    rewrite_handler_command_sugar(&mut body, node_id_gen);
    let mut ret_ty_expr =
        rewrite_handler_return_type(&handler.ret_ty_expr, state_name, node_id_gen);
    if handler_return_uses_stay(&handler.ret_ty_expr) {
        inject_self_tail_for_stay(&mut body, node_id_gen);
    }
    FuncDef {
        id: handler.id,
        def_id: (),
        attrs: Vec::new(),
        sig: parsed::FunctionSig {
            name,
            type_params: Vec::new(),
            params,
            ret_ty_expr: {
                // Keep node IDs unique after rewriting `stay` -> concrete state.
                ret_ty_expr.id = node_id_gen.new_id();
                ret_ty_expr
            },
            span: handler.span,
        },
        body,
        span: handler.span,
    }
}

fn type_expr_is_stay(ty: &TypeExpr) -> bool {
    matches!(
        &ty.kind,
        TypeExprKind::Named {
            ident,
            type_args,
            ..
        } if ident == "stay" && type_args.is_empty()
    )
}

fn rewrite_handler_return_type(
    ret_ty_expr: &TypeExpr,
    state_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> TypeExpr {
    fn concrete_state_ty(state_name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
        TypeExpr {
            id: node_id_gen.new_id(),
            kind: TypeExprKind::Named {
                ident: state_name.to_string(),
                def_id: (),
                type_args: Vec::new(),
            },
            span,
        }
    }

    if type_expr_is_stay(ret_ty_expr) {
        return concrete_state_ty(state_name, node_id_gen, ret_ty_expr.span);
    }

    let TypeExprKind::Union { variants } = &ret_ty_expr.kind else {
        return ret_ty_expr.clone();
    };
    let Some(first) = variants.first() else {
        return ret_ty_expr.clone();
    };
    if !type_expr_is_stay(first) {
        return ret_ty_expr.clone();
    }

    let mut rewritten = ret_ty_expr.clone();
    let TypeExprKind::Union { variants } = &mut rewritten.kind else {
        return rewritten;
    };
    variants[0] = concrete_state_ty(state_name, node_id_gen, first.span);
    rewritten
}

fn handler_return_uses_stay(ret_ty_expr: &TypeExpr) -> bool {
    if type_expr_is_stay(ret_ty_expr) {
        return true;
    }
    let TypeExprKind::Union { variants } = &ret_ty_expr.kind else {
        return false;
    };
    variants.first().is_some_and(type_expr_is_stay)
}

fn inject_self_tail_for_stay(body: &mut Expr, node_id_gen: &mut NodeIdGen) {
    let ExprKind::Block { tail, .. } = &mut body.kind else {
        return;
    };
    if tail.is_none() {
        *tail = Some(Box::new(Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Var {
                ident: "self".to_string(),
                def_id: (),
            },
            ty: (),
            span: body.span,
        }));
    }
}

fn rewrite_handler_command_sugar(body: &mut Expr, node_id_gen: &mut NodeIdGen) {
    let mut rewriter = HandlerCommandSugarRewriter { node_id_gen };
    rewriter.visit_expr(body);
}

struct HandlerCommandSugarRewriter<'a> {
    node_id_gen: &'a mut NodeIdGen,
}

impl HandlerCommandSugarRewriter<'_> {
    fn wrap_machine_target_id(&mut self, target: Expr) -> Expr {
        let span = target.span;
        Expr {
            id: self.node_id_gen.new_id(),
            kind: ExprKind::Call {
                callee: Box::new(Expr {
                    id: self.node_id_gen.new_id(),
                    kind: ExprKind::Var {
                        ident: MACHINE_TARGET_ID_HELPER_FN.to_string(),
                        def_id: (),
                    },
                    ty: (),
                    span,
                }),
                args: vec![CallArg {
                    mode: CallArgMode::Default,
                    expr: target,
                    init: InitInfo::default(),
                    span,
                }],
            },
            ty: (),
            span,
        }
    }
}

impl VisitorMut<()> for HandlerCommandSugarRewriter<'_> {
    fn visit_expr(&mut self, expr: &mut Expr) {
        visit_mut::walk_expr(self, expr);

        if let ExprKind::MethodCall {
            callee,
            method_name,
            args,
        } = &mut expr.kind
        {
            // Capability-style sugar inside handlers:
            //   cap.reply(payload)  ==>  reply(cap, payload)
            if method_name == "reply"
                && args.len() == 1
                && args.iter().all(|arg| arg.mode == CallArgMode::Default)
            {
                let value = std::mem::take(args)
                    .pop()
                    .expect("reply method arg shape checked by typestate handler sugar rewrite")
                    .expr;
                let cap =
                    std::mem::replace(callee, Box::new(unit_expr(self.node_id_gen, expr.span)));
                expr.kind = ExprKind::Reply {
                    cap,
                    value: Box::new(value),
                };
                return;
            }
            // Destination-handle sugar inside handlers:
            //   dst.send(payload)     ==> emit Send(to: machine_target_id(dst), payload)
            //   dst.request(payload)  ==> emit Request(to: machine_target_id(dst), payload)
            if (method_name == "send" || method_name == "request")
                && args.len() == 1
                && args.iter().all(|arg| arg.mode == CallArgMode::Default)
            {
                let payload = std::mem::take(args)
                    .pop()
                    .expect(
                        "send/request method arg shape checked by typestate handler sugar rewrite",
                    )
                    .expr;
                let dst =
                    std::mem::replace(callee, Box::new(unit_expr(self.node_id_gen, expr.span)));
                let to = self.wrap_machine_target_id(*dst);
                let kind = if method_name == "send" {
                    parsed::EmitKind::Send {
                        to: Box::new(to),
                        payload: Box::new(payload),
                    }
                } else {
                    parsed::EmitKind::Request {
                        to: Box::new(to),
                        payload: Box::new(payload),
                        request_site_label: None,
                    }
                };
                expr.kind = ExprKind::Emit { kind };
            }
            return;
        }

        let ExprKind::Call { callee, args } = &mut expr.kind else {
            return;
        };
        let ExprKind::Var { ident, .. } = &callee.kind else {
            return;
        };
        let Some((command, request_site_label)) = parse_handler_command_name(ident) else {
            return;
        };
        if args.len() != 2 || !args.iter().all(|arg| arg.mode == CallArgMode::Default) {
            return;
        }

        let mut call_args = std::mem::take(args);
        let payload = call_args
            .pop()
            .expect("call arg shape checked by typestate handler sugar rewrite")
            .expr;
        let to = call_args
            .pop()
            .expect("call arg shape checked by typestate handler sugar rewrite")
            .expr;
        let to = self.wrap_machine_target_id(to);
        let kind = if command == "send" {
            parsed::EmitKind::Send {
                to: Box::new(to),
                payload: Box::new(payload),
            }
        } else {
            parsed::EmitKind::Request {
                to: Box::new(to),
                payload: Box::new(payload),
                request_site_label,
            }
        };
        expr.kind = ExprKind::Emit { kind };
    }
}

fn parse_handler_command_name(ident: &str) -> Option<(&str, Option<String>)> {
    if ident == "send" {
        return Some(("send", None));
    }
    if ident == "request" {
        return Some(("request", None));
    }
    // Labeled request sugar is parsed as synthetic callee `request:label(...)`.
    let (command, label) = ident.split_once(':')?;
    if command != "request" || label.is_empty() {
        return None;
    }
    Some(("request", Some(label.to_string())))
}
