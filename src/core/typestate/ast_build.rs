use super::*;

// Shared AST construction helpers for typestate desugaring modules.
// Keeping these centralized avoids subtle divergence in generated node shapes.

pub(super) fn var_expr(name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Var {
            ident: name.to_string(),
            def_id: (),
        },
        ty: (),
        span,
    }
}

pub(super) fn int_expr(value: u64, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::IntLit(value),
        ty: (),
        span,
    }
}

pub(super) fn tuple_field_expr(
    target: Expr,
    index: usize,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::TupleField {
            target: Box::new(target),
            index,
        },
        ty: (),
        span,
    }
}

pub(super) fn self_field_expr(field: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::StructField {
            target: Box::new(var_expr("self", node_id_gen, span)),
            field: field.to_string(),
        },
        ty: (),
        span,
    }
}

pub(super) fn call_expr(
    callee_name: &str,
    args: Vec<Expr>,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Call {
            callee: Box::new(var_expr(callee_name, node_id_gen, span)),
            args: args
                .into_iter()
                .map(|expr| CallArg {
                    mode: CallArgMode::Default,
                    expr,
                    init: InitInfo::default(),
                    span,
                })
                .collect(),
        },
        ty: (),
        span,
    }
}

pub(super) fn let_bind_stmt(
    ident: &str,
    value: Expr,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> StmtExpr {
    StmtExpr {
        id: node_id_gen.new_id(),
        kind: StmtExprKind::LetBind {
            pattern: BindPattern {
                id: node_id_gen.new_id(),
                kind: BindPatternKind::Name {
                    ident: ident.to_string(),
                    def_id: (),
                },
                span,
            },
            decl_ty: None,
            value: Box::new(value),
        },
        ty: (),
        span,
    }
}

pub(super) fn return_stmt(value: Expr, node_id_gen: &mut NodeIdGen, span: Span) -> StmtExpr {
    StmtExpr {
        id: node_id_gen.new_id(),
        kind: StmtExprKind::Return {
            value: Some(Box::new(value)),
        },
        ty: (),
        span,
    }
}
