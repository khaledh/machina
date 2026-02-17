use super::ast_build::{
    call_expr, int_expr, let_bind_stmt, self_field_expr, tuple_field_expr, var_expr,
};
use super::*;

pub(super) fn machine_handle_method_block(
    handle_type_name: &str,
    typed_send_specs: &[TypedSendSpec],
    typed_request_specs: &[TypedRequestSpec],
    node_id_gen: &mut NodeIdGen,
) -> TopLevelItem {
    let span = Span::default();

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

    let send_method = MethodDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        sig: MethodSig {
            name: "send".to_string(),
            type_params: Vec::new(),
            self_param: SelfParam {
                id: node_id_gen.new_id(),
                def_id: (),
                mode: ParamMode::In,
                span,
            },
            params: vec![
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "kind".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "payload0".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "payload1".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
            ],
            ret_ty_expr: machine_send_result_union_type(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    parsed::BlockItem::Stmt(let_bind_stmt(
                        "__mc_rt",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_machine_error_if_zero(
                        "__mc_rt",
                        MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Stmt(let_bind_stmt(
                        "__mc_status",
                        call_expr(
                            "__mc_machine_runtime_send_u64",
                            vec![
                                var_expr("__mc_rt", node_id_gen, span),
                                self_field_expr("_id", node_id_gen, span),
                                var_expr("kind", node_id_gen, span),
                                var_expr("payload0", node_id_gen, span),
                                var_expr("payload1", node_id_gen, span),
                            ],
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_machine_error_if_eq(
                        "__mc_status",
                        1,
                        MACHINE_UNKNOWN_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_machine_error_if_eq(
                        "__mc_status",
                        2,
                        MACHINE_NOT_RUNNING_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_machine_error_if_eq(
                        "__mc_status",
                        3,
                        MACHINE_MAILBOX_FULL_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                ],
                tail: Some(Box::new(unit_expr(node_id_gen, span))),
            },
            ty: (),
            span,
        },
        span,
    };

    let typed_send_methods: Vec<MethodItem> = typed_send_specs
        .iter()
        .map(|spec| {
            MethodItem::Def(MethodDef {
                id: node_id_gen.new_id(),
                def_id: (),
                attrs: Vec::new(),
                sig: MethodSig {
                    name: "send".to_string(),
                    type_params: Vec::new(),
                    self_param: SelfParam {
                        id: node_id_gen.new_id(),
                        def_id: (),
                        mode: ParamMode::In,
                        span,
                    },
                    params: vec![parsed::Param {
                        id: node_id_gen.new_id(),
                        ident: "payload".to_string(),
                        def_id: (),
                        typ: clone_type_expr_with_new_ids(&spec.selector_ty, node_id_gen),
                        mode: ParamMode::In,
                        span,
                    }],
                    ret_ty_expr: machine_send_result_union_type(node_id_gen, span),
                    span,
                },
                body: Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::Block {
                        items: vec![
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_rt",
                                call_expr(
                                    MANAGED_RUNTIME_CURRENT_FN,
                                    Vec::new(),
                                    node_id_gen,
                                    span,
                                ),
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Expr(return_machine_error_if_zero(
                                "__mc_rt",
                                MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                            // Pack typed payload into runtime ABI words:
                            // - payload0: heap box pointer
                            // - payload1: payload layout id
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_packed",
                                call_expr(
                                    "__mc_machine_payload_pack",
                                    vec![var_expr("payload", node_id_gen, span)],
                                    node_id_gen,
                                    span,
                                ),
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_status",
                                call_expr(
                                    "__mc_machine_runtime_send_u64",
                                    vec![
                                        var_expr("__mc_rt", node_id_gen, span),
                                        self_field_expr("_id", node_id_gen, span),
                                        int_expr(spec.kind, node_id_gen, span),
                                        tuple_field_expr(
                                            var_expr("__mc_packed", node_id_gen, span),
                                            0,
                                            node_id_gen,
                                            span,
                                        ),
                                        tuple_field_expr(
                                            var_expr("__mc_packed", node_id_gen, span),
                                            1,
                                            node_id_gen,
                                            span,
                                        ),
                                    ],
                                    node_id_gen,
                                    span,
                                ),
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Expr(return_machine_error_if_eq(
                                "__mc_status",
                                1,
                                MACHINE_UNKNOWN_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Expr(return_machine_error_if_eq(
                                "__mc_status",
                                2,
                                MACHINE_NOT_RUNNING_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Expr(return_machine_error_if_eq(
                                "__mc_status",
                                3,
                                MACHINE_MAILBOX_FULL_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                        ],
                        tail: Some(Box::new(unit_expr(node_id_gen, span))),
                    },
                    ty: (),
                    span,
                },
                span,
            })
        })
        .collect();

    let request_method = MethodDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        sig: MethodSig {
            name: "request".to_string(),
            type_params: Vec::new(),
            self_param: SelfParam {
                id: node_id_gen.new_id(),
                def_id: (),
                mode: ParamMode::In,
                span,
            },
            params: vec![
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "dst".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "kind".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "payload0".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "payload1".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
            ],
            ret_ty_expr: machine_request_result_union_type(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    parsed::BlockItem::Stmt(let_bind_stmt(
                        "__mc_rt",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_machine_error_if_zero(
                        "__mc_rt",
                        MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Stmt(let_bind_stmt(
                        "__mc_pending_id",
                        call_expr(
                            "__mc_machine_runtime_request_u64",
                            vec![
                                var_expr("__mc_rt", node_id_gen, span),
                                self_field_expr("_id", node_id_gen, span),
                                var_expr("dst", node_id_gen, span),
                                var_expr("kind", node_id_gen, span),
                                var_expr("payload0", node_id_gen, span),
                                var_expr("payload1", node_id_gen, span),
                            ],
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_machine_error_if_zero(
                        "__mc_pending_id",
                        MACHINE_REQUEST_FAILED_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                ],
                tail: Some(Box::new(var_expr("__mc_pending_id", node_id_gen, span))),
            },
            ty: (),
            span,
        },
        span,
    };

    let typed_request_methods: Vec<MethodItem> = typed_request_specs
        .iter()
        .map(|spec| {
            MethodItem::Def(MethodDef {
                id: node_id_gen.new_id(),
                def_id: (),
                attrs: Vec::new(),
                sig: MethodSig {
                    name: "request".to_string(),
                    type_params: Vec::new(),
                    self_param: SelfParam {
                        id: node_id_gen.new_id(),
                        def_id: (),
                        mode: ParamMode::In,
                        span,
                    },
                    params: vec![
                        parsed::Param {
                            id: node_id_gen.new_id(),
                            ident: "dst".to_string(),
                            def_id: (),
                            typ: u64_type_expr(node_id_gen, span),
                            mode: ParamMode::In,
                            span,
                        },
                        parsed::Param {
                            id: node_id_gen.new_id(),
                            ident: "payload".to_string(),
                            def_id: (),
                            typ: clone_type_expr_with_new_ids(&spec.payload_ty, node_id_gen),
                            mode: ParamMode::In,
                            span,
                        },
                    ],
                    ret_ty_expr: machine_request_result_union_type(node_id_gen, span),
                    span,
                },
                body: Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::Block {
                        items: vec![
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_rt",
                                call_expr(
                                    MANAGED_RUNTIME_CURRENT_FN,
                                    Vec::new(),
                                    node_id_gen,
                                    span,
                                ),
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Expr(return_machine_error_if_zero(
                                "__mc_rt",
                                MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                            // Pack typed payload into runtime ABI words:
                            // - payload0: heap box pointer
                            // - payload1: payload layout id
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_packed",
                                call_expr(
                                    "__mc_machine_payload_pack",
                                    vec![var_expr("payload", node_id_gen, span)],
                                    node_id_gen,
                                    span,
                                ),
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_pending_id",
                                call_expr(
                                    "__mc_machine_runtime_request_u64",
                                    vec![
                                        var_expr("__mc_rt", node_id_gen, span),
                                        self_field_expr("_id", node_id_gen, span),
                                        var_expr("dst", node_id_gen, span),
                                        int_expr(spec.kind, node_id_gen, span),
                                        tuple_field_expr(
                                            var_expr("__mc_packed", node_id_gen, span),
                                            0,
                                            node_id_gen,
                                            span,
                                        ),
                                        tuple_field_expr(
                                            var_expr("__mc_packed", node_id_gen, span),
                                            1,
                                            node_id_gen,
                                            span,
                                        ),
                                    ],
                                    node_id_gen,
                                    span,
                                ),
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Expr(return_machine_error_if_zero(
                                "__mc_pending_id",
                                MACHINE_REQUEST_FAILED_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                        ],
                        tail: Some(Box::new(var_expr("__mc_pending_id", node_id_gen, span))),
                    },
                    ty: (),
                    span,
                },
                span,
            })
        })
        .collect();

    let mut method_items = vec![MethodItem::Def(send_method)];
    method_items.extend(typed_send_methods);
    method_items.push(MethodItem::Def(request_method));
    method_items.extend(typed_request_methods);

    TopLevelItem::MethodBlock(MethodBlock {
        id: node_id_gen.new_id(),
        type_name: handle_type_name.to_string(),
        trait_name: None,
        method_items,
        span,
    })
}

pub(super) fn lower_spawn_func(
    spawn_name: &str,
    handle_type_name: &str,
    descriptor_id_helper_name: &str,
    ctor: &FuncDef,
    state_name_map: &HashMap<String, String>,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    // V1 managed constructor flow:
    // 1) evaluate `new(...)` to enforce mirror-forward constructor contract,
    // 2) allocate/bind/start a runtime machine slot,
    // 3) return a machine handle wrapper on success.
    let span = ctor.span;
    let mut params = Vec::new();
    params.extend(
        ctor.sig
            .params
            .iter()
            .map(|param| clone_param_with_new_ids(param, node_id_gen)),
    );

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
    let ctor_call = call_expr(&ctor.sig.name, ctor_call_args, node_id_gen, span);
    let runtime_current_call = call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span);
    let spawn_call = call_expr(
        "__mc_machine_runtime_spawn_u64",
        vec![
            var_expr("__mc_rt", node_id_gen, span),
            Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::IntLit(MANAGED_RUNTIME_DEFAULT_MAILBOX_CAP),
                ty: (),
                span,
            },
        ],
        node_id_gen,
        span,
    );
    let descriptor_id_call = call_expr(descriptor_id_helper_name, Vec::new(), node_id_gen, span);
    let bind_call = call_expr(
        "__mc_machine_runtime_bind_descriptor_u64",
        vec![
            var_expr("__mc_rt", node_id_gen, span),
            var_expr("__mc_machine_id", node_id_gen, span),
            var_expr("__mc_descriptor_id", node_id_gen, span),
            Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::IntLit(initial_state_tag),
                ty: (),
                span,
            },
        ],
        node_id_gen,
        span,
    );
    let pack_initial_state_call = call_expr(
        "__mc_machine_payload_pack",
        vec![var_expr("__mc_initial_state", node_id_gen, span)],
        node_id_gen,
        span,
    );
    let set_state_call = call_expr(
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
                ty: (),
                span,
            },
        ],
        node_id_gen,
        span,
    );
    let start_call = call_expr(
        "__mc_machine_runtime_start_u64",
        vec![
            var_expr("__mc_rt", node_id_gen, span),
            var_expr("__mc_machine_id", node_id_gen, span),
        ],
        node_id_gen,
        span,
    );

    let body = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items: vec![
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_rt",
                    runtime_current_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Expr(return_machine_error_if_zero(
                    "__mc_rt",
                    MACHINE_SPAWN_FAILED_TYPE_NAME,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_initial_state",
                    ctor_call,
                    node_id_gen,
                    span,
                )),
                // Keep the constructor value live so side effects and argument
                // evaluation semantics match direct `new(...)` usage.
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_machine_id",
                    spawn_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Expr(return_machine_error_if_zero(
                    "__mc_machine_id",
                    MACHINE_SPAWN_FAILED_TYPE_NAME,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_descriptor_id",
                    descriptor_id_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_bind_status",
                    bind_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Expr(return_machine_error_if_zero(
                    "__mc_bind_status",
                    MACHINE_BIND_FAILED_TYPE_NAME,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_initial_state_packed",
                    pack_initial_state_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_set_state_status",
                    set_state_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Expr(return_machine_error_if_zero(
                    "__mc_set_state_status",
                    MACHINE_BIND_FAILED_TYPE_NAME,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_start_status",
                    start_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Expr(return_machine_error_if_zero(
                    "__mc_start_status",
                    MACHINE_START_FAILED_TYPE_NAME,
                    node_id_gen,
                    span,
                )),
            ],
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
                ty: (),
                span,
            })),
        },
        ty: (),
        span,
    };

    FuncDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        sig: parsed::FunctionSig {
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
