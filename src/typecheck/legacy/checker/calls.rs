use super::*;
use crate::typecheck::legacy::overloads;

impl TypeChecker {
    fn check_call_arg_types(
        &mut self,
        args: &[CallArg],
        param_types: &[Type],
    ) -> Result<(), TypeCheckError> {
        for (i, arg) in args.iter().enumerate() {
            let param_ty = &param_types[i];
            let arg_ty = self.check_expr(&arg.expr, Expected::Exact(param_ty))?;
            match self.check_assignable_to(&arg.expr, &arg_ty, param_ty) {
                Ok(()) => continue,
                Err(_) => {
                    if arg.mode != CallArgMode::Move
                        && arg.mode != CallArgMode::Out
                        && array_to_slice_assignable(&arg_ty, param_ty)
                    {
                        continue;
                    }
                    let span = arg.span;
                    return Err(TypeCheckErrorKind::ArgTypeMismatch(
                        i + 1,
                        param_ty.clone(),
                        arg_ty.clone(),
                        span,
                    )
                    .into());
                }
            }
        }
        Ok(())
    }

    pub(super) fn check_call(
        &mut self,
        call_expr: &Expr,
        callee: &Expr,
        args: &[CallArg],
        expected: Expected<'_>,
    ) -> Result<Type, TypeCheckError> {
        if let ExprKind::Var { ident, def_id } = &callee.kind
            && let Some(def) = self.ctx.def_table.lookup_def(*def_id)
            && matches!(def.kind, DefKind::EnumVariantName)
        {
            if let Expected::Exact(expected_enum) = expected {
                if let Type::Enum { .. } = expected_enum {
                    self.type_map_builder
                        .record_node_type(callee.id, expected_enum.clone());
                }
            }
            let payload = args.iter().map(|arg| &arg.expr).collect::<Vec<_>>();
            return self.check_unqualified_enum_variant(ident, &payload, expected, call_expr.span);
        }

        if let ExprKind::Var { def_id, .. } = &callee.kind
            && let Some(def) = self.ctx.def_table.lookup_def(*def_id)
            && matches!(def.kind, DefKind::FuncDef { .. } | DefKind::FuncDecl { .. })
        {
            let name = def.name.clone();
            return self.check_named_call(&name, call_expr, callee, args, expected);
        }

        self.check_expr_call(call_expr, callee, args)
    }

    fn check_expr_call(
        &mut self,
        call_expr: &Expr,
        callee: &Expr,
        args: &[CallArg],
    ) -> Result<Type, TypeCheckError> {
        let callee_ty = self.check_expr(callee, Expected::Unknown)?;
        let Type::Fn { params, ret_ty } = callee_ty else {
            return Err(TypeCheckErrorKind::InvalidCallee(callee.kind.clone(), callee.span).into());
        };

        if params.len() != args.len() {
            return Err(self.err_arg_count_mismatch(
                "<fn>",
                params.len(),
                args.len(),
                call_expr.span,
            ));
        }

        let param_types = params
            .iter()
            .map(|param| param.ty.clone())
            .collect::<Vec<_>>();
        let params = params
            .iter()
            .map(|param| CallParam {
                mode: match param.mode {
                    FnParamMode::In => ParamMode::In,
                    FnParamMode::InOut => ParamMode::InOut,
                    FnParamMode::Out => ParamMode::Out,
                    FnParamMode::Sink => ParamMode::Sink,
                },
                ty: param.ty.clone(),
            })
            .collect::<Vec<_>>();

        self.type_map_builder.record_call_sig(
            call_expr.id,
            CallSig {
                def_id: None,
                receiver: None,
                params,
            },
        );

        self.check_call_arg_types(args, &param_types)?;

        Ok(*ret_ty)
    }

    fn check_named_call(
        &mut self,
        name: &str,
        call_expr: &Expr,
        callee: &Expr,
        args: &[CallArg],
        expected: Expected<'_>,
    ) -> Result<Type, TypeCheckError> {
        // Get the function overloads
        let Some(overloads) = self.func_sigs.get(name).cloned() else {
            return Err(self.err_unknown_type(callee.span));
        };

        self.check_named_call_common(name, callee, call_expr, args, &overloads, false, expected)
    }

    pub(super) fn check_method_call(
        &mut self,
        method_name: &str,
        call_expr: &Expr,
        callee: &Expr,
        args: &[CallArg],
        expected: Expected<'_>,
    ) -> Result<Type, TypeCheckError> {
        let callee_ty = self.check_expr(callee, Expected::Unknown)?;
        let view = self.view_type(&callee_ty);

        if method_name == "len" {
            if !args.is_empty() {
                return Err(self.err_arg_count_mismatch("len", 0, args.len(), call_expr.span));
            }
            if !self.is_place_expr(callee) {
                return Err(self.err_len_target_not_lvalue(callee.span));
            }
            if view.is_len_target() {
                return Ok(Type::uint(64));
            }
            return Err(self.err_invalid_struct_field_target(callee_ty, callee.span));
        }

        // Check that the callee type is a struct or enum
        let type_name = if let Some((name, _)) = view.as_struct() {
            name.to_string()
        } else if let Some((name, _)) = view.as_enum() {
            name.to_string()
        } else if view.is_string() {
            "string".to_string()
        } else {
            return Err(self.err_invalid_struct_field_target(callee_ty, callee.span));
        };

        // Get a map of method name to overloads
        let Some(type_methods) = self.method_sigs.get(&type_name) else {
            return Err(self.err_unknown_type(call_expr.span));
        };

        // Get the overloads for the method
        let Some(overloads) = type_methods.get(method_name).cloned() else {
            return Err(self.err_unknown_type(call_expr.span));
        };

        // Format the method name as "type::method"
        let name = format!("{}::{}", type_name, method_name);

        self.check_named_call_common(&name, callee, call_expr, args, &overloads, true, expected)
    }

    fn lookup_method_self_mode(&self, def_id: DefId) -> ParamMode {
        for block in self.ctx.module.method_blocks() {
            for method_item in &block.method_items {
                match method_item {
                    MethodItem::Decl(method_decl) => {
                        if method_decl.def_id == def_id {
                            return method_decl.sig.self_param.mode.clone();
                        }
                    }
                    MethodItem::Def(method_def) => {
                        if method_def.def_id == def_id {
                            return method_def.sig.self_param.mode.clone();
                        }
                    }
                }
            }
        }
        panic!("compiler bug: method self mode not found for def {def_id}");
    }

    fn check_named_call_common(
        &mut self,
        name: &str,
        callee: &Expr,
        call_expr: &Expr,
        args: &[CallArg],
        overloads: &[OverloadSig],
        is_method: bool,
        expected: Expected<'_>,
    ) -> Result<Type, TypeCheckError> {
        let callee_ty = self.check_expr(callee, Expected::Unknown)?;
        let arg_types = self.visit_call_args(args)?;

        let fallback_param_types = overloads::single_arity_param_types(overloads, arg_types.len());
        let expected_ty = expected.as_option();
        let allow_infer = expected_ty.map_or(false, Self::type_has_infer_vars);
        let mut fresh_infer = || self.new_infer_var();

        let resolution = match overloads::resolve_call(
            name,
            args,
            &arg_types,
            overloads,
            expected_ty,
            allow_infer,
            call_expr.span,
            &mut fresh_infer,
        ) {
            Ok(resolved) => resolved,
            Err(err) => {
                if matches!(err.kind(), TypeCheckErrorKind::OverloadNoMatch(_, _))
                    && let Some(param_types) = &fallback_param_types
                {
                    self.check_call_arg_types(args, &param_types)?
                }
                return Err(err);
            }
        };

        let overloads::CallResolution {
            def_id,
            param_types,
            param_modes,
            ret_type,
            inst,
        } = resolution;
        if let Some(inst) = inst {
            self.type_map_builder
                .record_generic_inst(call_expr.id, inst);
        }

        let mut receiver = None;
        if is_method {
            let self_mode = self.lookup_method_self_mode(def_id);
            receiver = Some(CallParam {
                mode: self_mode,
                ty: callee_ty.clone(),
            });
        }

        let params = param_modes
            .iter()
            .cloned()
            .zip(param_types.iter().cloned())
            .map(|(mode, ty)| CallParam { mode, ty })
            .collect::<Vec<_>>();

        self.type_map_builder.record_call_sig(
            call_expr.id,
            CallSig {
                def_id: Some(def_id),
                receiver,
                params,
            },
        );

        self.check_call_arg_types(args, &param_types)?;

        Ok(ret_type)
    }
}
