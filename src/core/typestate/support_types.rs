use super::*;

pub(super) fn ensure_machine_support_types(module: &mut Module, node_id_gen: &mut NodeIdGen) {
    let existing: HashSet<String> = module
        .top_level_items
        .iter()
        .filter_map(|item| match item {
            TopLevelItem::TypeDef(def) => Some(def.name.clone()),
            _ => None,
        })
        .collect();

    let mut prepend = Vec::new();
    if !existing.contains(MACHINE_HANDLE_TYPE_NAME) {
        prepend.push(machine_handle_type_def(node_id_gen));
    }
    if !existing.contains(MACHINE_ERROR_TYPE_NAME) {
        prepend.push(machine_error_type_def(node_id_gen));
    }
    if !existing.contains(MACHINE_SPAWN_FAILED_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_SPAWN_FAILED_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_BIND_FAILED_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_BIND_FAILED_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_START_FAILED_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_START_FAILED_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_UNKNOWN_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_UNKNOWN_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_NOT_RUNNING_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_NOT_RUNNING_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_MAILBOX_FULL_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_MAILBOX_FULL_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_REQUEST_FAILED_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_REQUEST_FAILED_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !module_has_callable_param_type(module, MACHINE_TARGET_ID_HELPER_FN, "u64") {
        prepend.push(machine_target_id_u64_helper_def(node_id_gen));
    }
    if !module_has_callable_param_type(
        module,
        MACHINE_TARGET_ID_HELPER_FN,
        MACHINE_HANDLE_TYPE_NAME,
    ) {
        prepend.push(machine_target_id_handle_helper_def(
            MACHINE_HANDLE_TYPE_NAME,
            node_id_gen,
        ));
    }

    let has_machine_methods = module.top_level_items.iter().any(|item| {
        matches!(item, TopLevelItem::MethodBlock(block) if block.type_name == MACHINE_HANDLE_TYPE_NAME)
    });
    if !has_machine_methods {
        prepend.push(machine_handle_method_block(
            MACHINE_HANDLE_TYPE_NAME,
            &[],
            &[],
            node_id_gen,
        ));
    }

    if prepend.is_empty() {
        return;
    }

    let mut items = Vec::with_capacity(prepend.len() + module.top_level_items.len());
    items.extend(prepend);
    items.extend(std::mem::take(&mut module.top_level_items));
    module.top_level_items = items;
}

pub(super) fn ensure_machine_runtime_intrinsics(module: &mut Module, node_id_gen: &mut NodeIdGen) {
    let existing_callables: HashSet<String> = module
        .top_level_items
        .iter()
        .filter_map(|item| match item {
            TopLevelItem::FuncDecl(decl) => Some(decl.sig.name.clone()),
            TopLevelItem::FuncDef(def) => Some(def.sig.name.clone()),
            _ => None,
        })
        .collect();

    let span = Span::default();
    let mut append = Vec::new();
    let mut push_decl = |name: &str, param_names: &[&str]| {
        if existing_callables.contains(name) {
            return;
        }
        // These declarations keep typestate-managed spawn lowering self-contained
        // even when prelude runtime declarations are not present in the source
        // module under test.
        append.push(TopLevelItem::FuncDecl(FuncDecl {
            id: node_id_gen.new_id(),
            def_id: (),
            attrs: Vec::new(),
            sig: parsed::FunctionSig {
                name: name.to_string(),
                type_params: Vec::new(),
                params: param_names
                    .iter()
                    .map(|param_name| parsed::Param {
                        id: node_id_gen.new_id(),
                        ident: (*param_name).to_string(),
                        def_id: (),
                        typ: u64_type_expr(node_id_gen, span),
                        mode: ParamMode::In,
                        span,
                    })
                    .collect(),
                ret_ty_expr: u64_type_expr(node_id_gen, span),
                span,
            },
            span,
        }));
    };

    push_decl(MANAGED_RUNTIME_BOOTSTRAP_FN, &[]);
    push_decl(MANAGED_RUNTIME_CURRENT_FN, &[]);
    push_decl(MANAGED_RUNTIME_SHUTDOWN_FN, &[]);
    push_decl(
        "__mc_machine_runtime_spawn_u64",
        &["runtime", "mailbox_cap"],
    );
    push_decl(
        "__mc_machine_runtime_bind_descriptor_u64",
        &[
            "runtime",
            "machine_id",
            "descriptor_id",
            "initial_state_tag",
        ],
    );
    push_decl("__mc_machine_runtime_start_u64", &["runtime", "machine_id"]);
    push_decl(
        "__mc_machine_runtime_set_state_u64",
        &["runtime", "machine_id", "state_word"],
    );
    push_decl("__mc_machine_runtime_step_u64", &["runtime"]);
    push_decl(
        "__mc_machine_runtime_send_u64",
        &["runtime", "dst", "kind", "payload0", "payload1"],
    );
    push_decl(
        "__mc_machine_runtime_request_u64",
        &["runtime", "src", "dst", "kind", "payload0", "payload1"],
    );
    if !existing_callables.contains("__mc_machine_payload_pack") {
        append.push(TopLevelItem::FuncDecl(FuncDecl {
            id: node_id_gen.new_id(),
            def_id: (),
            attrs: Vec::new(),
            sig: parsed::FunctionSig {
                name: "__mc_machine_payload_pack".to_string(),
                type_params: vec![parsed::TypeParam {
                    id: node_id_gen.new_id(),
                    ident: "T".to_string(),
                    bound: None,
                    def_id: (),
                    span,
                }],
                params: vec![parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "payload".to_string(),
                    def_id: (),
                    typ: TypeExpr {
                        id: node_id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: "T".to_string(),
                            def_id: (),
                            type_args: Vec::new(),
                        },
                        span,
                    },
                    mode: ParamMode::In,
                    span,
                }],
                // Returns `(payload0_ptr_word, payload_layout_id)`.
                ret_ty_expr: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Tuple {
                        field_ty_exprs: vec![
                            u64_type_expr(node_id_gen, span),
                            u64_type_expr(node_id_gen, span),
                        ],
                    },
                    span,
                },
                span,
            },
            span,
        }));
    }

    module.top_level_items.extend(append);
}

pub(super) fn machine_handle_type_def(node_id_gen: &mut NodeIdGen) -> TopLevelItem {
    machine_handle_named_type_def(MACHINE_HANDLE_TYPE_NAME, node_id_gen)
}

pub(super) fn machine_handle_named_type_def(
    type_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> TopLevelItem {
    TopLevelItem::TypeDef(TypeDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        name: type_name.to_string(),
        type_params: Vec::new(),
        kind: TypeDefKind::Struct {
            fields: vec![StructDefField {
                id: node_id_gen.new_id(),
                name: "_id".to_string(),
                ty: u64_type_expr(node_id_gen, Span::default()),
                span: Span::default(),
            }],
        },
        span: Span::default(),
    })
}

pub(super) fn empty_struct_type_def(name: &str, node_id_gen: &mut NodeIdGen) -> TopLevelItem {
    TopLevelItem::TypeDef(TypeDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        name: name.to_string(),
        type_params: Vec::new(),
        kind: TypeDefKind::Struct { fields: Vec::new() },
        span: Span::default(),
    })
}

pub(super) fn machine_target_id_u64_helper_def(node_id_gen: &mut NodeIdGen) -> TopLevelItem {
    let span = Span::default();
    TopLevelItem::FuncDef(FuncDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        sig: parsed::FunctionSig {
            name: MACHINE_TARGET_ID_HELPER_FN.to_string(),
            type_params: Vec::new(),
            params: vec![parsed::Param {
                id: node_id_gen.new_id(),
                ident: "dst".to_string(),
                def_id: (),
                typ: u64_type_expr(node_id_gen, span),
                mode: ParamMode::In,
                span,
            }],
            ret_ty_expr: u64_type_expr(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: Vec::new(),
                tail: Some(Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::Var {
                        ident: "dst".to_string(),
                        def_id: (),
                    },
                    ty: (),
                    span,
                })),
            },
            ty: (),
            span,
        },
        span,
    })
}

pub(super) fn machine_target_id_handle_helper_def(
    handle_type_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> TopLevelItem {
    let span = Span::default();
    TopLevelItem::FuncDef(FuncDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        sig: parsed::FunctionSig {
            name: MACHINE_TARGET_ID_HELPER_FN.to_string(),
            type_params: Vec::new(),
            params: vec![parsed::Param {
                id: node_id_gen.new_id(),
                ident: "dst".to_string(),
                def_id: (),
                typ: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: handle_type_name.to_string(),
                        def_id: (),
                        type_args: Vec::new(),
                    },
                    span,
                },
                mode: ParamMode::In,
                span,
            }],
            ret_ty_expr: u64_type_expr(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: Vec::new(),
                tail: Some(Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::StructField {
                        target: Box::new(Expr {
                            id: node_id_gen.new_id(),
                            kind: ExprKind::Var {
                                ident: "dst".to_string(),
                                def_id: (),
                            },
                            ty: (),
                            span,
                        }),
                        field: "_id".to_string(),
                    },
                    ty: (),
                    span,
                })),
            },
            ty: (),
            span,
        },
        span,
    })
}

pub(super) fn module_has_callable_param_type(
    module: &Module,
    name: &str,
    first_param_ty_name: &str,
) -> bool {
    let matches_sig = |sig: &parsed::FunctionSig| {
        sig.name == name
            && sig.params.first().is_some_and(|param| {
                matches!(
                    &param.typ.kind,
                    TypeExprKind::Named {
                        ident,
                        type_args,
                        ..
                    } if ident == first_param_ty_name && type_args.is_empty()
                )
            })
    };

    module.top_level_items.iter().any(|item| match item {
        TopLevelItem::FuncDecl(decl) => matches_sig(&decl.sig),
        TopLevelItem::FuncDef(def) => matches_sig(&def.sig),
        _ => false,
    })
}
