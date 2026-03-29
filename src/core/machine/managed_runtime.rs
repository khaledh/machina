//! Shared managed-runtime entrypoint rewriting for programs that declare machines.
//!
//! Hosted linear machines rely on auto-bootstrap / auto-drive / shutdown
//! wrapper for `main()`.

use crate::core::ast::{
    BinaryOp, BindPattern, BindPatternKind, BlockItem, CallArg, CallArgMode, Expr, ExprKind,
    FuncDef, InitInfo, Module, NodeIdGen, StmtExpr, StmtExprKind, TopLevelItem,
};
use crate::core::diag::Span;
use crate::core::machine::runtime_intrinsics::ensure_u64_runtime_intrinsics;

const MANAGED_RUNTIME_CURRENT_FN: &str = "__mc_machine_runtime_managed_current_u64";

const MANAGED_RUNTIME_BOOTSTRAP_FN: &str = "__mc_machine_runtime_managed_bootstrap_u64";
const MANAGED_RUNTIME_SHUTDOWN_FN: &str = "__mc_machine_runtime_managed_shutdown_u64";
const MANAGED_RUNTIME_STEP_FN: &str = "__mc_machine_runtime_step_u64";

pub(crate) fn ensure_managed_runtime_intrinsics(module: &mut Module, node_id_gen: &mut NodeIdGen) {
    ensure_u64_runtime_intrinsics(
        module,
        node_id_gen,
        &[
            (MANAGED_RUNTIME_BOOTSTRAP_FN, &[]),
            (MANAGED_RUNTIME_CURRENT_FN, &[]),
            (MANAGED_RUNTIME_SHUTDOWN_FN, &[]),
            (MANAGED_RUNTIME_STEP_FN, &["runtime"]),
        ],
    );
}

pub(crate) fn rewrite_machines_entrypoint(
    module: &mut Module,
    runtime_requested: bool,
    node_id_gen: &mut NodeIdGen,
) -> bool {
    for item in &mut module.top_level_items {
        let TopLevelItem::FuncDef(func) = item else {
            continue;
        };
        if func.sig.name != "main" {
            continue;
        }

        if !runtime_requested {
            return false;
        }

        wrap_main_with_managed_runtime(func, node_id_gen);
        return true;
    }
    false
}

fn wrap_main_with_managed_runtime(main: &mut FuncDef, node_id_gen: &mut NodeIdGen) {
    let span = main.body.span;
    let original_body = std::mem::replace(
        &mut main.body,
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::UnitLit,
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
            op: crate::core::ast::BinaryOp::Eq,
            right: Box::new(int_expr(1, node_id_gen, span)),
        },
        span,
    };
    let runtime_available_cond = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::BinOp {
            left: Box::new(var_expr("__mc_rt", node_id_gen, span)),
            op: BinaryOp::Ne,
            right: Box::new(int_expr(0, node_id_gen, span)),
        },
        span,
    };
    let auto_drive_if = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::If {
            cond: Box::new(runtime_available_cond),
            then_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: vec![BlockItem::Stmt(StmtExpr {
                        id: node_id_gen.new_id(),
                        kind: StmtExprKind::While {
                            cond: Box::new(Expr {
                                id: node_id_gen.new_id(),
                                kind: ExprKind::BoolLit(true),
                                span,
                            }),
                            body: Box::new(Expr {
                                id: node_id_gen.new_id(),
                                kind: ExprKind::Block {
                                    items: vec![
                                        BlockItem::Stmt(let_bind_stmt(
                                            "__mc_step_status",
                                            step_call,
                                            node_id_gen,
                                            span,
                                        )),
                                        BlockItem::Expr(Expr {
                                            id: node_id_gen.new_id(),
                                            kind: ExprKind::If {
                                                cond: Box::new(step_status_is_did_work),
                                                then_body: Box::new(Expr {
                                                    id: node_id_gen.new_id(),
                                                    kind: ExprKind::Block {
                                                        items: vec![BlockItem::Stmt(StmtExpr {
                                                            id: node_id_gen.new_id(),
                                                            kind: StmtExprKind::Continue,
                                                            span,
                                                        })],
                                                        tail: None,
                                                    },
                                                    span,
                                                }),
                                                else_body: Box::new(Expr {
                                                    id: node_id_gen.new_id(),
                                                    kind: ExprKind::Block {
                                                        items: vec![BlockItem::Stmt(StmtExpr {
                                                            id: node_id_gen.new_id(),
                                                            kind: StmtExprKind::Break,
                                                            span,
                                                        })],
                                                        tail: None,
                                                    },
                                                    span,
                                                }),
                                            },
                                            span,
                                        }),
                                    ],
                                    tail: None,
                                },
                                span,
                            }),
                        },
                        span,
                    })],
                    tail: None,
                },
                span,
            }),
            else_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: Vec::new(),
                    tail: None,
                },
                span,
            }),
        },
        span,
    };

    main.body = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items: vec![
                BlockItem::Stmt(let_bind_stmt("__mc_rt", bootstrap_call, node_id_gen, span)),
                BlockItem::Stmt(let_bind_stmt(
                    "__mc_main_result",
                    original_body,
                    node_id_gen,
                    span,
                )),
                BlockItem::Expr(auto_drive_if),
                BlockItem::Expr(shutdown_call),
            ],
            tail: Some(Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Var {
                    ident: "__mc_main_result".to_string(),
                },
                span,
            })),
        },
        span,
    };
}

fn var_expr(name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Var {
            ident: name.to_string(),
        },
        span,
    }
}

fn int_expr(value: u64, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::IntLit(value),
        span,
    }
}

fn call_expr(callee_name: &str, args: Vec<Expr>, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Call {
            callee: Box::new(var_expr(callee_name, node_id_gen, span)),
            args: args
                .into_iter()
                .map(|expr| CallArg {
                    label: None,
                    mode: CallArgMode::Default,
                    expr,
                    init: InitInfo::default(),
                    span,
                })
                .collect(),
        },
        span,
    }
}

fn let_bind_stmt(ident: &str, value: Expr, node_id_gen: &mut NodeIdGen, span: Span) -> StmtExpr {
    StmtExpr {
        id: node_id_gen.new_id(),
        kind: StmtExprKind::LetBind {
            pattern: BindPattern {
                id: node_id_gen.new_id(),
                kind: BindPatternKind::Name {
                    ident: ident.to_string(),
                },
                span,
            },
            decl_ty: None,
            value: Box::new(value),
        },
        span,
    }
}
