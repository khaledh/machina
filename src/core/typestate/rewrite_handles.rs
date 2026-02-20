use super::*;

pub(super) fn rewrite_typed_machine_handle_refs(
    module: &mut Module,
    handle_by_typestate: &HashMap<String, String>,
) {
    struct TypedMachineHandleRewriter<'a> {
        // Source typestate name -> concrete generated handle type name.
        handle_by_typestate: &'a HashMap<String, String>,
    }

    impl VisitorMut<()> for TypedMachineHandleRewriter<'_> {
        fn visit_stmt_expr(&mut self, stmt: &mut StmtExpr) {
            // `decl_ty` in let/var is not traversed by generic walk; handle
            // those explicitly so `Machine<Typestate>` annotations rewrite.
            match &mut stmt.kind {
                StmtExprKind::LetBind {
                    decl_ty: Some(decl_ty),
                    ..
                }
                | StmtExprKind::VarBind {
                    decl_ty: Some(decl_ty),
                    ..
                } => self.visit_type_expr(decl_ty),
                StmtExprKind::VarDecl { decl_ty, .. } => self.visit_type_expr(decl_ty),
                _ => {}
            }
            visit_mut::walk_stmt_expr(self, stmt);
        }

        fn visit_type_expr(&mut self, ty: &mut TypeExpr) {
            if let TypeExprKind::Named {
                ident, type_args, ..
            } = &mut ty.kind
                && ident == MACHINE_HANDLE_TYPE_NAME
                && type_args.len() == 1
                && let Some(type_arg) = type_args.first()
                && let TypeExprKind::Named {
                    ident: ts_name,
                    type_args: ts_args,
                    ..
                } = &type_arg.kind
                && ts_args.is_empty()
                && let Some(handle_name) = self.handle_by_typestate.get(ts_name)
            {
                *ident = handle_name.clone();
                type_args.clear();
            }
            visit_mut::walk_type_expr(self, ty);
        }

        fn visit_expr(&mut self, expr: &mut Expr) {
            if let ExprKind::StructLit {
                name, type_args, ..
            } = &mut expr.kind
                && name == MACHINE_HANDLE_TYPE_NAME
                && type_args.len() == 1
                && let Some(type_arg) = type_args.first()
                && let TypeExprKind::Named {
                    ident: ts_name,
                    type_args: ts_args,
                    ..
                } = &type_arg.kind
                && ts_args.is_empty()
                && let Some(handle_name) = self.handle_by_typestate.get(ts_name)
            {
                *name = handle_name.clone();
                type_args.clear();
            }
            visit_mut::walk_expr(self, expr);
        }
    }

    let mut rewriter = TypedMachineHandleRewriter {
        handle_by_typestate,
    };
    rewriter.visit_module(module);
}

pub(super) fn rewrite_machine_request_method_destinations(
    module: &mut Module,
    node_id_gen: &mut NodeIdGen,
) {
    use std::collections::HashSet;

    fn is_machine_handle_type_expr(ty: &TypeExpr) -> bool {
        matches!(
            &ty.kind,
            TypeExprKind::Named {
                ident,
                type_args,
                ..
            } if type_args.is_empty()
                && (ident == MACHINE_HANDLE_TYPE_NAME || ident.starts_with("__mc_machine_handle_"))
        )
    }

    fn expr_is_spawn_handle(expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Try { fallible_expr, .. } => expr_is_spawn_handle(fallible_expr),
            ExprKind::Call { callee, .. } => matches!(
                &callee.kind,
                ExprKind::Var { ident, .. } if ident.starts_with("__ts_spawn_")
            ),
            ExprKind::EnumVariant {
                enum_name, variant, ..
            } => !enum_name.is_empty() && variant == "spawn",
            _ => false,
        }
    }

    fn collect_machine_bindings_from_stmt(stmt: &StmtExpr, out: &mut HashSet<String>) {
        match &stmt.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            }
            | StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                collect_machine_bindings_from_expr(value, out);
                let BindPatternKind::Name { ident, .. } = &pattern.kind else {
                    return;
                };
                let typed_machine = decl_ty.as_ref().is_some_and(is_machine_handle_type_expr);
                if typed_machine || expr_is_spawn_handle(value) {
                    out.insert(ident.clone());
                }
            }
            StmtExprKind::VarDecl { ident, decl_ty, .. } => {
                if is_machine_handle_type_expr(decl_ty) {
                    out.insert(ident.clone());
                }
            }
            StmtExprKind::Assign {
                assignee, value, ..
            }
            | StmtExprKind::CompoundAssign {
                assignee, value, ..
            } => {
                collect_machine_bindings_from_expr(assignee, out);
                collect_machine_bindings_from_expr(value, out);
            }
            StmtExprKind::While { cond, body } => {
                collect_machine_bindings_from_expr(cond, out);
                collect_machine_bindings_from_expr(body, out);
            }
            StmtExprKind::For { iter, body, .. } => {
                collect_machine_bindings_from_expr(iter, out);
                collect_machine_bindings_from_expr(body, out);
            }
            StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    collect_machine_bindings_from_expr(value, out);
                }
            }
            StmtExprKind::Break | StmtExprKind::Continue => {}
        }
    }

    fn collect_machine_bindings_from_expr(expr: &Expr, out: &mut HashSet<String>) {
        match &expr.kind {
            ExprKind::Block { items, tail } => {
                for item in items {
                    match item {
                        parsed::BlockItem::Stmt(stmt) => {
                            collect_machine_bindings_from_stmt(stmt, out)
                        }
                        parsed::BlockItem::Expr(inner) => {
                            collect_machine_bindings_from_expr(inner, out)
                        }
                    }
                }
                if let Some(tail) = tail {
                    collect_machine_bindings_from_expr(tail, out);
                }
            }
            ExprKind::If {
                then_body,
                else_body,
                ..
            } => {
                collect_machine_bindings_from_expr(then_body, out);
                collect_machine_bindings_from_expr(else_body, out);
            }
            ExprKind::Match { arms, .. } => {
                for arm in arms {
                    collect_machine_bindings_from_expr(&arm.body, out);
                }
            }
            _ => {}
        }
    }

    struct RequestDstExprRewriter<'a> {
        node_id_gen: &'a mut NodeIdGen,
        machine_locals: &'a HashSet<String>,
    }

    impl RequestDstExprRewriter<'_> {
        fn wrap_target_id(&mut self, target: Expr) -> Expr {
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

    impl VisitorMut<()> for RequestDstExprRewriter<'_> {
        fn visit_expr(&mut self, expr: &mut Expr) {
            visit_mut::walk_expr(self, expr);

            let ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } = &mut expr.kind
            else {
                return;
            };
            if method_name != "request" || (args.len() != 2 && args.len() != 4) {
                return;
            }
            let ExprKind::Var { ident, .. } = &callee.kind else {
                return;
            };
            if !self.machine_locals.contains(ident) {
                return;
            }
            let Some(first_arg) = args.first_mut() else {
                return;
            };
            if first_arg.mode != CallArgMode::Default {
                return;
            }
            if matches!(
                &first_arg.expr.kind,
                ExprKind::Call { callee, .. }
                    if matches!(
                        callee.kind,
                        ExprKind::Var {
                            ref ident,
                            ..
                        } if ident == MACHINE_TARGET_ID_HELPER_FN
                    )
            ) {
                return;
            }

            let original = std::mem::replace(
                &mut first_arg.expr,
                Expr {
                    id: self.node_id_gen.new_id(),
                    kind: ExprKind::UnitLit,
                    ty: (),
                    span: first_arg.span,
                },
            );
            first_arg.expr = self.wrap_target_id(original);
        }
    }

    fn rewrite_body_expr(body: &mut Expr, params: &[parsed::Param], node_id_gen: &mut NodeIdGen) {
        let mut machine_locals = HashSet::new();
        for param in params {
            if is_machine_handle_type_expr(&param.typ) {
                machine_locals.insert(param.ident.clone());
            }
        }
        collect_machine_bindings_from_expr(body, &mut machine_locals);
        let mut rewriter = RequestDstExprRewriter {
            node_id_gen,
            machine_locals: &machine_locals,
        };
        rewriter.visit_expr(body);
    }

    for item in &mut module.top_level_items {
        match item {
            TopLevelItem::FuncDef(def) => {
                rewrite_body_expr(&mut def.body, &def.sig.params, node_id_gen)
            }
            TopLevelItem::MethodBlock(block) => {
                for item in &mut block.method_items {
                    if let MethodItem::Def(def) = item {
                        rewrite_body_expr(&mut def.body, &def.sig.params, node_id_gen);
                    }
                }
            }
            _ => {}
        }
    }
}
