use super::*;

impl TypeChecker {
    fn single_arity_param_types(overloads: &[OverloadSig], arg_count: usize) -> Option<Vec<Type>> {
        let mut matches = overloads.iter().filter(|sig| sig.params.len() == arg_count);
        let sig = matches.next()?;
        if matches.next().is_some() {
            return None;
        }
        Some(sig.params.iter().map(|param| param.ty.clone()).collect())
    }

    fn check_call_arg_types(
        &mut self,
        args: &[CallArg],
        param_types: &[Type],
    ) -> Result<(), TypeCheckError> {
        for (i, arg) in args.iter().enumerate() {
            let param_ty = &param_types[i];
            let arg_ty = self.visit_expr(&arg.expr, Some(param_ty))?;
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
        expected: Option<&Type>,
    ) -> Result<Type, TypeCheckError> {
        if let ExprKind::Var { ident, def_id } = &callee.kind
            && let Some(def) = self.ctx.def_table.lookup_def(*def_id)
            && matches!(def.kind, DefKind::EnumVariantName)
        {
            if let Some(expected_enum @ Type::Enum { .. }) = expected {
                self.type_map_builder
                    .record_node_type(callee.id, expected_enum.clone());
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
        let callee_ty = self.visit_expr(callee, None)?;
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
        expected: Option<&Type>,
    ) -> Result<Type, TypeCheckError> {
        // Get the function overloads
        let Some(overloads) = self.func_sigs.get(name).cloned() else {
            return Err(TypeCheckErrorKind::UnknownType(callee.span).into());
        };

        self.check_named_call_common(name, callee, call_expr, args, &overloads, false, expected)
    }

    pub(super) fn check_method_call(
        &mut self,
        method_name: &str,
        call_expr: &Expr,
        callee: &Expr,
        args: &[CallArg],
        expected: Option<&Type>,
    ) -> Result<Type, TypeCheckError> {
        let callee_ty = self.visit_expr(callee, None)?;
        let view = self.view_type(&callee_ty);

        if method_name == "len" {
            if !args.is_empty() {
                return Err(self.err_arg_count_mismatch("len", 0, args.len(), call_expr.span));
            }
            if !self.is_place_expr(callee) {
                return Err(TypeCheckErrorKind::LenTargetNotLvalue(callee.span).into());
            }
            if view.as_array().is_some() || view.as_slice().is_some() || view.is_string() {
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
            return Err(TypeCheckErrorKind::UnknownType(call_expr.span).into());
        };

        // Get the overloads for the method
        let Some(overloads) = type_methods.get(method_name).cloned() else {
            return Err(TypeCheckErrorKind::UnknownType(call_expr.span).into());
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
        expected: Option<&Type>,
    ) -> Result<Type, TypeCheckError> {
        let callee_ty = self.visit_expr(callee, None)?;
        let arg_types = self.visit_call_args(args)?;

        let (resolved, fallback_param_types) = {
            let fallback_param_types = Self::single_arity_param_types(overloads, arg_types.len());
            // If no overload matches the arity and all overloads share the same arity,
            // report a count mismatch instead of a generic overload error.
            if !overloads
                .iter()
                .any(|sig| sig.params.len() == arg_types.len())
            {
                let mut counts = HashSet::new();
                for sig in overloads {
                    counts.insert(sig.params.len());
                }
                if counts.len() == 1 {
                    let expected = *counts.iter().next().unwrap();
                    return Err(self.err_arg_count_mismatch(
                        name,
                        expected,
                        arg_types.len(),
                        call_expr.span,
                    ));
                }
            }

            let mut generic_overloads = Vec::new();
            let mut concrete_overloads = Vec::new();
            for sig in overloads {
                if Self::sig_has_type_vars(sig) {
                    generic_overloads.push(sig);
                } else {
                    concrete_overloads.push(sig);
                }
            }

            let resolved = if !concrete_overloads.is_empty() {
                let concrete = concrete_overloads.into_iter().cloned().collect::<Vec<_>>();
                match OverloadResolver::new(name, args, &arg_types, call_expr.span)
                    .resolve(&concrete)
                {
                    Ok(resolved) => Ok((
                        resolved.def_id,
                        resolved
                            .sig
                            .params
                            .iter()
                            .map(|param| param.ty.clone())
                            .collect::<Vec<_>>(),
                        resolved
                            .sig
                            .params
                            .iter()
                            .map(|param| param.mode.clone())
                            .collect::<Vec<_>>(),
                        resolved.sig.ret_ty.clone(),
                    )),
                    Err(err) => {
                        if matches!(err.kind(), TypeCheckErrorKind::OverloadNoMatch(_, _))
                            && !generic_overloads.is_empty()
                        {
                            let inst = self.resolve_generic_overload(
                                name,
                                args,
                                &arg_types,
                                &generic_overloads,
                                expected,
                                call_expr.span,
                            )?;
                            let sig = generic_overloads
                                .iter()
                                .find(|sig| sig.def_id == inst.def_id)
                                .ok_or_else(|| {
                                    TypeCheckError::from(TypeCheckErrorKind::OverloadNoMatch(
                                        name.to_string(),
                                        call_expr.span,
                                    ))
                                })?;
                            let (param_types, param_modes, ret_type) =
                                Self::apply_inst_to_sig(sig, &inst);
                            self.type_map_builder
                                .record_generic_inst(call_expr.id, inst);
                            Ok((sig.def_id, param_types, param_modes, ret_type))
                        } else {
                            Err(err)
                        }
                    }
                }
            } else if !generic_overloads.is_empty() {
                let inst = self.resolve_generic_overload(
                    name,
                    args,
                    &arg_types,
                    &generic_overloads,
                    expected,
                    call_expr.span,
                )?;
                let sig = generic_overloads
                    .iter()
                    .find(|sig| sig.def_id == inst.def_id)
                    .ok_or_else(|| {
                        TypeCheckError::from(TypeCheckErrorKind::OverloadNoMatch(
                            name.to_string(),
                            call_expr.span,
                        ))
                    })?;
                let (param_types, param_modes, ret_type) = Self::apply_inst_to_sig(sig, &inst);
                self.type_map_builder
                    .record_generic_inst(call_expr.id, inst);
                Ok((sig.def_id, param_types, param_modes, ret_type))
            } else {
                OverloadResolver::new(name, args, &arg_types, call_expr.span)
                    .resolve(overloads)
                    .map(|resolved| {
                        let param_types = resolved
                            .sig
                            .params
                            .iter()
                            .map(|param| param.ty.clone())
                            .collect::<Vec<_>>();
                        let param_modes = resolved
                            .sig
                            .params
                            .iter()
                            .map(|param| param.mode.clone())
                            .collect::<Vec<_>>();
                        let ret_type = resolved.sig.ret_ty.clone();
                        (resolved.def_id, param_types, param_modes, ret_type)
                    })
            };
            (resolved, fallback_param_types)
        };

        let (def_id, param_types, param_modes, ret_type) = match resolved {
            Ok(resolved) => resolved,
            Err(err) => {
                if matches!(err.kind(), TypeCheckErrorKind::OverloadNoMatch(_, _))
                    && let Some(param_types) = fallback_param_types
                {
                    self.check_call_arg_types(args, &param_types)?
                }
                return Err(err);
            }
        };

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

    fn resolve_generic_overload(
        &mut self,
        name: &str,
        args: &[CallArg],
        arg_types: &[Type],
        overloads: &[&OverloadSig],
        expected: Option<&Type>,
        call_span: Span,
    ) -> Result<GenericInst, TypeCheckError> {
        let mut candidates = Vec::new();
        let mut range_err: Option<TypeCheckError> = None;
        let allow_infer = expected.map_or(false, Self::type_has_infer_vars);

        let mut unifier_for_sig = |sig: &OverloadSig| -> Result<Option<Unifier>, TypeCheckError> {
            let mut unifier = Unifier::new();
            for ((arg, arg_ty), param) in args.iter().zip(arg_types).zip(sig.params.iter()) {
                let param_ty = &param.ty;
                let mut arg_ty_for_unify = arg_ty.clone();

                if arg.mode != CallArgMode::Move
                    && arg.mode != CallArgMode::Out
                    && matches!(param_ty, Type::Slice { .. })
                    && let Some(item) = arg_ty.array_item_type()
                {
                    arg_ty_for_unify = Type::Slice {
                        elem_ty: Box::new(item),
                    };
                }

                if Self::type_has_vars(param_ty) {
                    if unifier.unify(param_ty, &arg_ty_for_unify).is_err() {
                        return Ok(None);
                    }
                    continue;
                }

                match value_assignable(&arg.expr, arg_ty, param_ty) {
                    ValueAssignability::Assignable(assignability) => {
                        if matches!(assignability, TypeAssignability::Incompatible) {
                            return Ok(None);
                        }
                    }
                    ValueAssignability::ValueOutOfRange { value, min, max } => {
                        range_err.get_or_insert(
                            TypeCheckErrorKind::ValueOutOfRange(value, min, max, arg.span).into(),
                        );
                        return Ok(None);
                    }
                    ValueAssignability::ValueNotNonZero { value } => {
                        return Err(TypeCheckErrorKind::ValueNotNonZero(value, arg.span).into());
                    }
                    ValueAssignability::Incompatible => {
                        if arg.mode != CallArgMode::Move
                            && arg.mode != CallArgMode::Out
                            && array_to_slice_assignable(arg_ty, param_ty)
                        {
                            continue;
                        }
                        return Ok(None);
                    }
                }
            }
            if let Some(expected_ty) = expected {
                if Self::type_has_vars(&sig.ret_ty) {
                    if unifier.unify(&sig.ret_ty, expected_ty).is_err() {
                        return Ok(None);
                    }
                }
            }
            Ok(Some(unifier))
        };

        for sig in overloads {
            if sig.params.len() != arg_types.len() {
                continue;
            }

            let Some(unifier) = unifier_for_sig(sig)? else {
                continue;
            };

            let mut type_args = (0..sig.type_param_count)
                .map(|index| unifier.apply(&Type::Var(TyVarId::new(index as u32))))
                .collect::<Vec<_>>();

            if allow_infer {
                let mut replacements = HashMap::new();
                type_args = type_args
                    .iter()
                    .map(|ty| {
                        self.replace_param_vars_with_infer(
                            ty,
                            sig.type_param_count,
                            &mut replacements,
                        )
                    })
                    .collect();
            }

            if type_args
                .iter()
                .any(|ty| Self::type_contains_param_var(ty, sig.type_param_count))
            {
                continue;
            }

            candidates.push(GenericInst {
                def_id: sig.def_id,
                type_args,
                call_span,
            });
        }

        if candidates.is_empty() {
            return Err(range_err.unwrap_or_else(|| {
                TypeCheckErrorKind::OverloadNoMatch(name.to_string(), call_span).into()
            }));
        }

        if candidates.len() != 1 {
            return Err(TypeCheckErrorKind::OverloadAmbiguous(name.to_string(), call_span).into());
        }

        Ok(candidates.pop().unwrap())
    }
}
