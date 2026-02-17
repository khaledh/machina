use super::*;
use super::ast_build::{call_expr, int_expr, let_bind_stmt, var_expr};

pub(super) fn rewrite_machines_entrypoint(module: &mut Module, node_id_gen: &mut NodeIdGen) -> bool {
    let mut opted_in = false;
    for item in &mut module.top_level_items {
        let TopLevelItem::FuncDef(func) = item else {
            continue;
        };
        let has_machines_attr = func
            .attrs
            .iter()
            .any(|attr| attr.name == "machines" && attr.args.is_empty());
        if !has_machines_attr || func.sig.name != "main" {
            continue;
        }

        opted_in = true;
        wrap_main_with_managed_runtime(func, node_id_gen);
    }
    opted_in
}

fn wrap_main_with_managed_runtime(main: &mut FuncDef, node_id_gen: &mut NodeIdGen) {
    let span = main.body.span;
    let original_body = std::mem::replace(
        &mut main.body,
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::UnitLit,
            ty: (),
            span,
        },
    );
    let bootstrap_call = call_expr(MANAGED_RUNTIME_BOOTSTRAP_FN, Vec::new(), node_id_gen, span);
    let shutdown_call = call_expr(MANAGED_RUNTIME_SHUTDOWN_FN, Vec::new(), node_id_gen, span);
    let step_call = call_expr(
        MANAGED_RUNTIME_STEP_FN,
        vec![var_expr("__mc_rt", node_id_gen, span)],
        node_id_gen,
        span,
    );
    let step_status_is_did_work = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::BinOp {
            left: Box::new(var_expr("__mc_step_status", node_id_gen, span)),
            op: crate::core::tree::BinaryOp::Eq,
            right: Box::new(int_expr(1, node_id_gen, span)),
        },
        ty: (),
        span,
    };
    let runtime_available_cond = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::BinOp {
            left: Box::new(var_expr("__mc_rt", node_id_gen, span)),
            op: crate::core::tree::BinaryOp::Ne,
            right: Box::new(int_expr(0, node_id_gen, span)),
        },
        ty: (),
        span,
    };
    // Auto-drive policy for `@machines`:
    // keep stepping while runtime reports "did work", and stop once it reaches
    // idle or faulted. We still return the user's main result.
    let auto_drive_if = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::If {
            cond: Box::new(runtime_available_cond),
            then_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: vec![parsed::BlockItem::Stmt(StmtExpr {
                        id: node_id_gen.new_id(),
                        kind: StmtExprKind::While {
                            cond: Box::new(Expr {
                                id: node_id_gen.new_id(),
                                kind: ExprKind::BoolLit(true),
                                ty: (),
                                span,
                            }),
                            body: Box::new(Expr {
                                id: node_id_gen.new_id(),
                                kind: ExprKind::Block {
                                    items: vec![
                                        parsed::BlockItem::Stmt(let_bind_stmt(
                                            "__mc_step_status",
                                            step_call,
                                            node_id_gen,
                                            span,
                                        )),
                                        parsed::BlockItem::Expr(Expr {
                                            id: node_id_gen.new_id(),
                                            kind: ExprKind::If {
                                                cond: Box::new(step_status_is_did_work),
                                                then_body: Box::new(Expr {
                                                    id: node_id_gen.new_id(),
                                                    kind: ExprKind::Block {
                                                        items: vec![parsed::BlockItem::Stmt(
                                                            StmtExpr {
                                                                id: node_id_gen.new_id(),
                                                                kind: StmtExprKind::Continue,
                                                                ty: (),
                                                                span,
                                                            },
                                                        )],
                                                        tail: None,
                                                    },
                                                    ty: (),
                                                    span,
                                                }),
                                                else_body: Box::new(Expr {
                                                    id: node_id_gen.new_id(),
                                                    kind: ExprKind::Block {
                                                        items: vec![parsed::BlockItem::Stmt(
                                                            StmtExpr {
                                                                id: node_id_gen.new_id(),
                                                                kind: StmtExprKind::Break,
                                                                ty: (),
                                                                span,
                                                            },
                                                        )],
                                                        tail: None,
                                                    },
                                                    ty: (),
                                                    span,
                                                }),
                                            },
                                            ty: (),
                                            span,
                                        }),
                                    ],
                                    tail: None,
                                },
                                ty: (),
                                span,
                            }),
                        },
                        ty: (),
                        span,
                    })],
                    tail: None,
                },
                ty: (),
                span,
            }),
            else_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: Vec::new(),
                    tail: None,
                },
                ty: (),
                span,
            }),
        },
        ty: (),
        span,
    };

    main.body = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items: vec![
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_rt",
                    bootstrap_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_main_result",
                    original_body,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Expr(auto_drive_if),
                parsed::BlockItem::Expr(shutdown_call),
            ],
            tail: Some(Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Var {
                    ident: "__mc_main_result".to_string(),
                    def_id: (),
                },
                ty: (),
                span,
            })),
        },
        ty: (),
        span,
    };
}
