use super::ast_build::{
    call_expr, int_expr, let_bind_stmt, self_field_expr, tuple_field_expr, var_expr,
};
use super::*;

// Descriptor for generating a handle method (send or request variant).
struct HandleMethodSpec<'a> {
    name: &'a str,
    params: Vec<Param>,
    ret_ty_expr: TypeExpr,
    // Items to insert before the runtime call (e.g. payload pack).
    pre_call_items: Vec<BlockItem>,
    // The runtime function to call.
    runtime_fn: &'a str,
    // Arguments to pass to the runtime function.
    runtime_args: Vec<Expr>,
    // Name of the variable bound to the runtime call result.
    result_var: &'a str,
    // Items to insert after the runtime call (status/error checks).
    post_call_items: Vec<BlockItem>,
    // Tail expression of the method body block.
    tail: Expr,
}

fn build_handle_method(spec: HandleMethodSpec<'_>, node_id_gen: &mut NodeIdGen) -> MethodDef {
    let span = Span::default();

    let mut items = runtime_current_or_error_items(node_id_gen, span);
    items.extend(spec.pre_call_items);
    items.push(BlockItem::Stmt(let_bind_stmt(
        spec.result_var,
        call_expr(spec.runtime_fn, spec.runtime_args, node_id_gen, span),
        node_id_gen,
        span,
    )));
    items.extend(spec.post_call_items);

    MethodDef {
        id: node_id_gen.new_id(),
        attrs: Vec::new(),
        sig: MethodSig {
            name: spec.name.to_string(),
            type_params: Vec::new(),
            self_param: SelfParam {
                id: node_id_gen.new_id(),
                mode: ParamMode::In,
                span,
            },
            params: spec.params,
            ret_ty_expr: spec.ret_ty_expr,
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items,
                tail: Some(Box::new(spec.tail)),
            },
            span,
        },
        span,
    }
}

fn runtime_current_or_error_items(node_id_gen: &mut NodeIdGen, span: Span) -> Vec<BlockItem> {
    vec![
        BlockItem::Stmt(let_bind_stmt(
            "__mc_rt",
            call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
            node_id_gen,
            span,
        )),
        BlockItem::Expr(return_machine_error_if_zero(
            "__mc_rt",
            MachineErrorKind::RuntimeUnavailable,
            node_id_gen,
            span,
        )),
    ]
}

fn machine_send_result_union_type(node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
    TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Union {
            variants: vec![
                named_type_expr("()", node_id_gen, span),
                named_type_expr(MACHINE_ERROR_TYPE_NAME, node_id_gen, span),
            ],
        },
        span,
    }
}

fn machine_request_result_union_type(node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
    TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Union {
            variants: vec![
                named_type_expr("u64", node_id_gen, span),
                named_type_expr(MACHINE_ERROR_TYPE_NAME, node_id_gen, span),
            ],
        },
        span,
    }
}

fn u64_param(name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Param {
    Param {
        id: node_id_gen.new_id(),
        ident: name.to_string(),
        typ: u64_type_expr(node_id_gen, span),
        mode: ParamMode::In,
        span,
    }
}

fn typed_payload_pack_items(node_id_gen: &mut NodeIdGen, span: Span) -> Vec<BlockItem> {
    vec![BlockItem::Stmt(let_bind_stmt(
        "__mc_packed",
        call_expr(
            "__mc_machine_payload_pack",
            vec![var_expr("payload", node_id_gen, span)],
            node_id_gen,
            span,
        ),
        node_id_gen,
        span,
    ))]
}

fn packed_field(index: usize, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    tuple_field_expr(
        var_expr("__mc_packed", node_id_gen, span),
        index,
        node_id_gen,
        span,
    )
}

fn build_generic_send_method(node_id_gen: &mut NodeIdGen) -> MethodDef {
    let span = Span::default();
    build_handle_method(
        HandleMethodSpec {
            name: "send",
            params: vec![
                u64_param("kind", node_id_gen, span),
                u64_param("payload0", node_id_gen, span),
                u64_param("payload1", node_id_gen, span),
            ],
            ret_ty_expr: machine_send_result_union_type(node_id_gen, span),
            pre_call_items: Vec::new(),
            runtime_fn: "__mc_machine_runtime_send_u64",
            runtime_args: vec![
                var_expr("__mc_rt", node_id_gen, span),
                self_field_expr("_id", node_id_gen, span),
                var_expr("kind", node_id_gen, span),
                var_expr("payload0", node_id_gen, span),
                var_expr("payload1", node_id_gen, span),
            ],
            result_var: "__mc_status",
            post_call_items: send_status_error_items("__mc_status", node_id_gen, span),
            tail: unit_expr(node_id_gen, span),
        },
        node_id_gen,
    )
}

fn build_typed_send_method(spec: &TypedSendSpec, node_id_gen: &mut NodeIdGen) -> MethodDef {
    let span = Span::default();
    build_handle_method(
        HandleMethodSpec {
            name: "send",
            params: vec![Param {
                id: node_id_gen.new_id(),
                ident: "payload".to_string(),
                typ: clone_type_expr_with_new_ids(&spec.selector_ty, node_id_gen),
                mode: ParamMode::In,
                span,
            }],
            ret_ty_expr: machine_send_result_union_type(node_id_gen, span),
            pre_call_items: typed_payload_pack_items(node_id_gen, span),
            runtime_fn: "__mc_machine_runtime_send_u64",
            runtime_args: vec![
                var_expr("__mc_rt", node_id_gen, span),
                self_field_expr("_id", node_id_gen, span),
                int_expr(spec.kind, node_id_gen, span),
                packed_field(0, node_id_gen, span),
                packed_field(1, node_id_gen, span),
            ],
            result_var: "__mc_status",
            post_call_items: send_status_error_items("__mc_status", node_id_gen, span),
            tail: unit_expr(node_id_gen, span),
        },
        node_id_gen,
    )
}

fn build_generic_request_method(node_id_gen: &mut NodeIdGen) -> MethodDef {
    let span = Span::default();
    build_handle_method(
        HandleMethodSpec {
            name: "request",
            params: vec![
                u64_param("dst", node_id_gen, span),
                u64_param("kind", node_id_gen, span),
                u64_param("payload0", node_id_gen, span),
                u64_param("payload1", node_id_gen, span),
            ],
            ret_ty_expr: machine_request_result_union_type(node_id_gen, span),
            pre_call_items: Vec::new(),
            runtime_fn: "__mc_machine_runtime_request_u64",
            runtime_args: vec![
                var_expr("__mc_rt", node_id_gen, span),
                self_field_expr("_id", node_id_gen, span),
                var_expr("dst", node_id_gen, span),
                var_expr("kind", node_id_gen, span),
                var_expr("payload0", node_id_gen, span),
                var_expr("payload1", node_id_gen, span),
            ],
            result_var: "__mc_pending_id",
            post_call_items: vec![BlockItem::Expr(return_machine_error_if_zero(
                "__mc_pending_id",
                MachineErrorKind::RequestFailed,
                node_id_gen,
                span,
            ))],
            tail: var_expr("__mc_pending_id", node_id_gen, span),
        },
        node_id_gen,
    )
}

fn build_typed_request_method(spec: &TypedRequestSpec, node_id_gen: &mut NodeIdGen) -> MethodDef {
    let span = Span::default();
    build_handle_method(
        HandleMethodSpec {
            name: "request",
            params: vec![
                u64_param("dst", node_id_gen, span),
                Param {
                    id: node_id_gen.new_id(),
                    ident: "payload".to_string(),
                    typ: clone_type_expr_with_new_ids(&spec.payload_ty, node_id_gen),
                    mode: ParamMode::In,
                    span,
                },
            ],
            ret_ty_expr: machine_request_result_union_type(node_id_gen, span),
            pre_call_items: typed_payload_pack_items(node_id_gen, span),
            runtime_fn: "__mc_machine_runtime_request_u64",
            runtime_args: vec![
                var_expr("__mc_rt", node_id_gen, span),
                self_field_expr("_id", node_id_gen, span),
                var_expr("dst", node_id_gen, span),
                int_expr(spec.kind, node_id_gen, span),
                packed_field(0, node_id_gen, span),
                packed_field(1, node_id_gen, span),
            ],
            result_var: "__mc_pending_id",
            post_call_items: vec![BlockItem::Expr(return_machine_error_if_zero(
                "__mc_pending_id",
                MachineErrorKind::RequestFailed,
                node_id_gen,
                span,
            ))],
            tail: var_expr("__mc_pending_id", node_id_gen, span),
        },
        node_id_gen,
    )
}

pub(super) fn machine_handle_method_block(
    handle_type_name: &str,
    typed_send_specs: &[TypedSendSpec],
    typed_request_specs: &[TypedRequestSpec],
    node_id_gen: &mut NodeIdGen,
) -> TopLevelItem {
    let span = Span::default();

    let mut method_items = vec![MethodItem::Def(build_generic_send_method(node_id_gen))];
    method_items.extend(
        typed_send_specs
            .iter()
            .map(|spec| MethodItem::Def(build_typed_send_method(spec, node_id_gen))),
    );
    method_items.push(MethodItem::Def(build_generic_request_method(node_id_gen)));
    method_items.extend(
        typed_request_specs
            .iter()
            .map(|spec| MethodItem::Def(build_typed_request_method(spec, node_id_gen))),
    );

    TopLevelItem::MethodBlock(MethodBlock {
        id: node_id_gen.new_id(),
        type_name: handle_type_name.to_string(),
        trait_name: None,
        method_items,
        span,
    })
}

// Helper: let-bind + error-check pattern used repeatedly in lower_spawn_func.
fn let_bind_with_error_check(
    var_name: &str,
    value: Expr,
    error_kind: MachineErrorKind,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> [BlockItem; 2] {
    [
        BlockItem::Stmt(let_bind_stmt(var_name, value, node_id_gen, span)),
        BlockItem::Expr(return_machine_error_if_zero(
            var_name,
            error_kind,
            node_id_gen,
            span,
        )),
    ]
}

pub(super) fn lower_spawn_func(
    spawn_name: &str,
    handle_type_name: &str,
    descriptor_id_helper_name: &str,
    ctor: &FuncDef,
    state_name_map: &HashMap<String, String>,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = ctor.span;
    let params: Vec<_> = ctor
        .sig
        .params
        .iter()
        .map(|param| clone_param_with_new_ids(param, node_id_gen))
        .collect();

    let mut state_names: Vec<String> = state_name_map.values().cloned().collect();
    state_names.sort();
    let initial_state_name = match &ctor.sig.ret_ty_expr.kind {
        TypeExprKind::Named { ident, .. } => ident.clone(),
        TypeExprKind::Union { variants } => variants
            .first()
            .and_then(|variant| match &variant.kind {
                TypeExprKind::Named { ident, .. } => Some(ident.clone()),
                _ => None,
            })
            .unwrap_or_else(|| state_names.first().cloned().unwrap_or_default()),
        _ => state_names.first().cloned().unwrap_or_default(),
    };
    let initial_state_tag = state_names
        .iter()
        .position(|name| name == &initial_state_name)
        .map(|idx| idx as u64 + 1)
        .unwrap_or(1);

    let ctor_call_args: Vec<Expr> = ctor
        .sig
        .params
        .iter()
        .map(|param| var_expr(&param.ident, node_id_gen, span))
        .collect();

    let mut items = Vec::new();

    // Runtime acquisition + error check.
    items.extend(let_bind_with_error_check(
        "__mc_rt",
        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
        MachineErrorKind::RuntimeUnavailable,
        node_id_gen,
        span,
    ));

    // Constructor call (keep value live for side effects).
    items.push(BlockItem::Stmt(let_bind_stmt(
        "__mc_initial_state",
        call_expr(&ctor.sig.name, ctor_call_args, node_id_gen, span),
        node_id_gen,
        span,
    )));

    // Spawn + error check.
    items.extend(let_bind_with_error_check(
        "__mc_machine_id",
        call_expr(
            "__mc_machine_runtime_spawn_u64",
            vec![
                var_expr("__mc_rt", node_id_gen, span),
                Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::IntLit(MANAGED_RUNTIME_DEFAULT_MAILBOX_CAP),
                    span,
                },
            ],
            node_id_gen,
            span,
        ),
        MachineErrorKind::SpawnFailed,
        node_id_gen,
        span,
    ));

    // Descriptor bind + error check.
    items.push(BlockItem::Stmt(let_bind_stmt(
        "__mc_descriptor_id",
        call_expr(descriptor_id_helper_name, Vec::new(), node_id_gen, span),
        node_id_gen,
        span,
    )));
    items.extend(let_bind_with_error_check(
        "__mc_bind_status",
        call_expr(
            "__mc_machine_runtime_bind_descriptor_u64",
            vec![
                var_expr("__mc_rt", node_id_gen, span),
                var_expr("__mc_machine_id", node_id_gen, span),
                var_expr("__mc_descriptor_id", node_id_gen, span),
                Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::IntLit(initial_state_tag),
                    span,
                },
            ],
            node_id_gen,
            span,
        ),
        MachineErrorKind::BindFailed,
        node_id_gen,
        span,
    ));

    // Pack + set state + error check.
    items.push(BlockItem::Stmt(let_bind_stmt(
        "__mc_initial_state_packed",
        call_expr(
            "__mc_machine_payload_pack",
            vec![var_expr("__mc_initial_state", node_id_gen, span)],
            node_id_gen,
            span,
        ),
        node_id_gen,
        span,
    )));
    items.extend(let_bind_with_error_check(
        "__mc_set_state_status",
        call_expr(
            "__mc_machine_runtime_set_state_u64",
            vec![
                var_expr("__mc_rt", node_id_gen, span),
                var_expr("__mc_machine_id", node_id_gen, span),
                Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::TupleField {
                        target: Box::new(var_expr("__mc_initial_state_packed", node_id_gen, span)),
                        index: 0,
                    },
                    span,
                },
            ],
            node_id_gen,
            span,
        ),
        MachineErrorKind::BindFailed,
        node_id_gen,
        span,
    ));

    // Start + error check.
    items.extend(let_bind_with_error_check(
        "__mc_start_status",
        call_expr(
            "__mc_machine_runtime_start_u64",
            vec![
                var_expr("__mc_rt", node_id_gen, span),
                var_expr("__mc_machine_id", node_id_gen, span),
            ],
            node_id_gen,
            span,
        ),
        MachineErrorKind::StartFailed,
        node_id_gen,
        span,
    ));

    let body = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items,
            tail: Some(Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::StructLit {
                    name: handle_type_name.to_string(),
                    type_args: Vec::new(),
                    fields: vec![StructLitField {
                        id: node_id_gen.new_id(),
                        name: "_id".to_string(),
                        value: var_expr("__mc_machine_id", node_id_gen, span),
                        span,
                    }],
                },
                span,
            })),
        },
        span,
    };

    FuncDef {
        id: node_id_gen.new_id(),
        attrs: Vec::new(),
        sig: FunctionSig {
            name: spawn_name.to_string(),
            type_params: Vec::new(),
            params,
            ret_ty_expr: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Union {
                    variants: vec![
                        named_type_expr(handle_type_name, node_id_gen, span),
                        named_type_expr(MACHINE_ERROR_TYPE_NAME, node_id_gen, span),
                    ],
                },
                span,
            },
            span,
        },
        body,
        span,
    }
}
