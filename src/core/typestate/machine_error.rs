use super::ast_build::{int_expr, return_stmt, var_expr};
use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum MachineErrorKind {
    SpawnFailed,
    BindFailed,
    StartFailed,
    RuntimeUnavailable,
    Unknown,
    NotRunning,
    MailboxFull,
    RequestFailed,
}

pub(super) fn machine_error_type_def(node_id_gen: &mut NodeIdGen) -> TopLevelItem {
    let span = Span::default();
    TopLevelItem::TypeDef(TypeDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        name: MACHINE_ERROR_TYPE_NAME.to_string(),
        type_params: Vec::new(),
        kind: TypeDefKind::Enum {
            variants: vec![
                EnumDefVariant {
                    id: node_id_gen.new_id(),
                    name: MACHINE_ERROR_VARIANT_SPAWN_FAILED.to_string(),
                    payload: Vec::new(),
                    span,
                },
                EnumDefVariant {
                    id: node_id_gen.new_id(),
                    name: MACHINE_ERROR_VARIANT_BIND_FAILED.to_string(),
                    payload: Vec::new(),
                    span,
                },
                EnumDefVariant {
                    id: node_id_gen.new_id(),
                    name: MACHINE_ERROR_VARIANT_START_FAILED.to_string(),
                    payload: Vec::new(),
                    span,
                },
                EnumDefVariant {
                    id: node_id_gen.new_id(),
                    name: MACHINE_ERROR_VARIANT_MANAGED_RUNTIME_UNAVAILABLE.to_string(),
                    payload: Vec::new(),
                    span,
                },
                EnumDefVariant {
                    id: node_id_gen.new_id(),
                    name: MACHINE_ERROR_VARIANT_UNKNOWN.to_string(),
                    payload: Vec::new(),
                    span,
                },
                EnumDefVariant {
                    id: node_id_gen.new_id(),
                    name: MACHINE_ERROR_VARIANT_NOT_RUNNING.to_string(),
                    payload: Vec::new(),
                    span,
                },
                EnumDefVariant {
                    id: node_id_gen.new_id(),
                    name: MACHINE_ERROR_VARIANT_MAILBOX_FULL.to_string(),
                    payload: Vec::new(),
                    span,
                },
                EnumDefVariant {
                    id: node_id_gen.new_id(),
                    name: MACHINE_ERROR_VARIANT_REQUEST_FAILED.to_string(),
                    payload: Vec::new(),
                    span,
                },
            ],
        },
        span,
    })
}

fn machine_error_variant_for_kind(kind: MachineErrorKind) -> &'static str {
    match kind {
        MachineErrorKind::SpawnFailed => MACHINE_ERROR_VARIANT_SPAWN_FAILED,
        MachineErrorKind::BindFailed => MACHINE_ERROR_VARIANT_BIND_FAILED,
        MachineErrorKind::StartFailed => MACHINE_ERROR_VARIANT_START_FAILED,
        MachineErrorKind::RuntimeUnavailable => MACHINE_ERROR_VARIANT_MANAGED_RUNTIME_UNAVAILABLE,
        MachineErrorKind::Unknown => MACHINE_ERROR_VARIANT_UNKNOWN,
        MachineErrorKind::NotRunning => MACHINE_ERROR_VARIANT_NOT_RUNNING,
        MachineErrorKind::MailboxFull => MACHINE_ERROR_VARIANT_MAILBOX_FULL,
        MachineErrorKind::RequestFailed => MACHINE_ERROR_VARIANT_REQUEST_FAILED,
    }
}

pub(super) fn machine_error_variant_expr(
    variant_name: &str,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::EnumVariant {
            enum_name: MACHINE_ERROR_TYPE_NAME.to_string(),
            type_args: Vec::new(),
            variant: variant_name.to_string(),
            payload: Vec::new(),
        },
        ty: (),
        span,
    }
}

pub(super) fn return_machine_error_if_eq(
    value_var: &str,
    expected: u64,
    error_kind: MachineErrorKind,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    let cond = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::BinOp {
            left: Box::new(var_expr(value_var, node_id_gen, span)),
            op: crate::core::tree::BinaryOp::Eq,
            right: Box::new(int_expr(expected, node_id_gen, span)),
        },
        ty: (),
        span,
    };
    let then_body = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items: vec![parsed::BlockItem::Stmt(return_stmt(
                machine_error_variant_expr(
                    machine_error_variant_for_kind(error_kind),
                    node_id_gen,
                    span,
                ),
                node_id_gen,
                span,
            ))],
            tail: None,
        },
        ty: (),
        span,
    };
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::If {
            cond: Box::new(cond),
            then_body: Box::new(then_body),
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
    }
}

pub(super) fn return_machine_error_if_zero(
    value_var: &str,
    error_kind: MachineErrorKind,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    return_machine_error_if_eq(value_var, 0, error_kind, node_id_gen, span)
}

pub(super) fn send_status_error_items(
    status_var: &str,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Vec<parsed::BlockItem> {
    vec![
        parsed::BlockItem::Expr(return_machine_error_if_eq(
            status_var,
            1,
            MachineErrorKind::Unknown,
            node_id_gen,
            span,
        )),
        parsed::BlockItem::Expr(return_machine_error_if_eq(
            status_var,
            2,
            MachineErrorKind::NotRunning,
            node_id_gen,
            span,
        )),
        parsed::BlockItem::Expr(return_machine_error_if_eq(
            status_var,
            3,
            MachineErrorKind::MailboxFull,
            node_id_gen,
            span,
        )),
    ]
}
