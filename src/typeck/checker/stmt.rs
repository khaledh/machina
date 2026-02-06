use super::*;

impl TypeChecker {
    fn record_infer_bindings(&mut self, pattern: &BindPattern) {
        let mut bindings = Vec::new();
        self.collect_bindings(pattern, &mut |def_id, span| {
            bindings.push((def_id, span));
        });
        let Some(ctx) = &mut self.infer_ctx else {
            return;
        };
        for (def_id, span) in bindings {
            ctx.bindings.entry(def_id).or_insert(span);
        }
    }

    fn collect_bindings<F>(&self, pattern: &BindPattern, f: &mut F)
    where
        F: FnMut(DefId, Span),
    {
        match &pattern.kind {
            BindPatternKind::Name { def_id, .. } => f(*def_id, pattern.span),
            BindPatternKind::Array { patterns } | BindPatternKind::Tuple { patterns } => {
                for child in patterns {
                    self.collect_bindings(child, f);
                }
            }
            BindPatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.collect_bindings(&field.pattern, f);
                }
            }
        }
    }

    fn check_bind_pattern(
        &mut self,
        pattern: &BindPattern,
        value_ty: &Type,
    ) -> Result<(), TypeCheckError> {
        match &pattern.kind {
            BindPatternKind::Name { def_id, .. } => {
                // Record this identifier's type
                if let Some(def) = self.ctx.def_table.lookup_def(*def_id) {
                    self.type_map_builder
                        .record_def_type(def.clone(), value_ty.clone());
                }
                Ok(())
            }
            BindPatternKind::Array { patterns } => {
                // Value must be an array
                match value_ty {
                    Type::Array { dims, .. } => {
                        // Check the pattern has the right number of elements
                        if patterns.len() != dims[0] {
                            return Err(TypeCheckErrorKind::ArrayPatternLengthMismatch(
                                dims[0],
                                patterns.len(),
                                pattern.span,
                            )
                            .into());
                        }

                        // Determine the subtype for each pattern element
                        let sub_ty = value_ty
                            .array_item_type()
                            .unwrap_or_else(|| panic!("compiler bug: empty array dims"));

                        // Recursively type check each sub-pattern
                        for pattern in patterns {
                            self.check_bind_pattern(pattern, &sub_ty)?;
                        }
                        Ok(())
                    }
                    _ => Err(TypeCheckErrorKind::PatternTypeMismatch(
                        pattern.clone(),
                        value_ty.clone(),
                        pattern.span,
                    )
                    .into()),
                }
            }
            BindPatternKind::Tuple { patterns } => match value_ty {
                Type::Tuple { field_tys } => {
                    if patterns.len() != field_tys.len() {
                        return Err(TypeCheckErrorKind::TuplePatternLengthMismatch(
                            field_tys.len(),
                            patterns.len(),
                            pattern.span,
                        )
                        .into());
                    }

                    // Recursively type check each sub-pattern
                    for (pattern, field) in patterns.iter().zip(field_tys) {
                        self.check_bind_pattern(pattern, field)?;
                    }
                    Ok(())
                }
                _ => Err(TypeCheckErrorKind::PatternTypeMismatch(
                    pattern.clone(),
                    value_ty.clone(),
                    pattern.span,
                )
                .into()),
            },
            BindPatternKind::Struct { name, fields } => {
                let Type::Struct { name: ty_name, .. } = value_ty else {
                    return Err(TypeCheckErrorKind::PatternTypeMismatch(
                        pattern.clone(),
                        value_ty.clone(),
                        pattern.span,
                    )
                    .into());
                };

                // Check that the struct type name matches
                if ty_name != name {
                    return Err(TypeCheckErrorKind::PatternTypeMismatch(
                        pattern.clone(),
                        value_ty.clone(),
                        pattern.span,
                    )
                    .into());
                }

                // Check each field pattern
                for field in fields {
                    // Type check the field
                    if let Some(expected_ty) = self
                        .resolve_struct_field(value_ty, &field.name)
                        .map(|f| &f.ty)
                    {
                        self.check_bind_pattern(&field.pattern, expected_ty)?;
                    }
                }

                Ok(())
            }
        }
    }

    pub(super) fn check_binding(
        &mut self,
        pattern: &BindPattern,
        decl_ty: &Option<TypeExpr>,
        value: &Expr,
    ) -> Result<Type, TypeCheckError> {
        // resolve the declaration type (if present)
        let expected_ty = decl_ty
            .as_ref()
            .map(|ty_expr| self.resolve_type_expr_in_scope(ty_expr))
            .transpose()?;
        let infer_binding = expected_ty.is_none();

        let mut value_ty = if let Some(expected_ty) = &expected_ty {
            let mut value_ty = self.visit_expr(value, Some(expected_ty))?;
            self.check_assignable_to(value, &value_ty, expected_ty)?;
            value_ty = expected_ty.clone();
            if matches!(&value.kind, ExprKind::ArrayLit { .. })
                && matches!(value_ty, Type::Array { .. })
            {
                self.type_map_builder
                    .record_node_type(value.id, value_ty.clone());
            }
            value_ty
        } else {
            let infer_var = Type::Var(self.new_infer_var());
            let value_ty = self.visit_expr(value, Some(&infer_var))?;
            let (_infer_ty, value_ty, _unified) = self.unify_infer_types(&infer_var, &value_ty);
            value_ty
        };

        value_ty = self.apply_infer(&value_ty);
        self.check_bind_pattern(pattern, &value_ty)?;
        if infer_binding {
            self.record_infer_bindings(pattern);
        }

        Ok(Type::Unit)
    }

    pub(super) fn check_assignable_to(
        &mut self,
        from_value: &Expr,
        from_ty: &Type,
        to_ty: &Type,
    ) -> Result<(), TypeCheckError> {
        let from_ty = self.apply_infer(from_ty);
        let to_ty = self.apply_infer(to_ty);
        let had_infer = Self::type_has_infer_vars(&from_ty) || Self::type_has_infer_vars(&to_ty);
        let (from_ty, to_ty, unified) = self.unify_infer_types(&from_ty, &to_ty);

        if had_infer {
            if !unified {
                return Err(
                    TypeCheckErrorKind::DeclTypeMismatch(to_ty, from_ty, from_value.span).into(),
                );
            }
            if Self::type_has_infer_vars(&from_ty) || Self::type_has_infer_vars(&to_ty) {
                return Ok(());
            }
        }

        match type_assignable(&from_ty, &to_ty) {
            TypeAssignability::Incompatible => {
                Err(TypeCheckErrorKind::DeclTypeMismatch(to_ty, from_ty, from_value.span).into())
            }
            _ => Ok(()),
        }
    }

    pub(super) fn check_assign(
        &mut self,
        assignee: &Expr,
        value: &Expr,
    ) -> Result<Type, TypeCheckError> {
        // Reject string index assignment (for now)
        if let ExprKind::ArrayIndex { target, .. } = &assignee.kind {
            let target_ty = self.visit_expr(target, None)?;
            if target_ty == Type::String {
                return Err(TypeCheckErrorKind::StringIndexAssign(assignee.span).into());
            }
        }

        if let ExprKind::StructField { target, field } = &assignee.kind {
            let receiver_ty = self.visit_expr(target, None)?;
            let view = self.view_type(&receiver_ty);
            if let Some(type_name) = self.property_owner_name(view.ty()) {
                // Property assignment is treated as a setter call.
                let prop_info = self
                    .property_sigs
                    .get(&type_name)
                    .and_then(|props| props.get(field))
                    .map(|prop| (prop.setter, prop.ty.clone()));
                if let Some((setter, prop_ty)) = prop_info {
                    let Some(setter) = setter else {
                        return Err(self.err_property_not_writable(field.clone(), assignee.span));
                    };
                    let rhs_type = self.visit_expr(value, Some(&prop_ty))?;
                    self.check_assignable_to(value, &rhs_type, &prop_ty)?;
                    self.record_property_call_sig(
                        assignee.id,
                        setter,
                        receiver_ty,
                        ParamMode::InOut,
                        vec![prop_ty.clone()],
                    );
                    self.type_map_builder
                        .record_node_type(assignee.id, Type::Unit);
                    return Ok(Type::Unit);
                }
            }
            if field == "len"
                && (view.as_array().is_some() || view.as_slice().is_some() || view.is_string())
            {
                return Err(self.err_property_not_writable(field.clone(), assignee.span));
            }
        }

        let lhs_type = self.visit_expr(assignee, None)?;
        let rhs_type = self.visit_expr(value, Some(&lhs_type))?;

        match self.check_assignable_to(value, &rhs_type, &lhs_type) {
            Ok(()) => Ok(Type::Unit),
            Err(_) => {
                Err(
                    TypeCheckErrorKind::AssignTypeMismatch(lhs_type, rhs_type, assignee.span)
                        .into(),
                )
            }
        }
    }

    pub(super) fn check_while(&mut self, cond: &Expr, body: &Expr) -> Result<Type, TypeCheckError> {
        let cond_type = self.visit_expr(cond, None)?;
        if cond_type != Type::Bool {
            return Err(TypeCheckErrorKind::CondNotBoolean(cond_type, cond.span).into());
        }

        self.enter_loop();
        let result = self.visit_expr(body, None);
        self.exit_loop();
        let _ = result?;
        Ok(Type::Unit)
    }

    pub(super) fn check_for(
        &mut self,
        pattern: &BindPattern,
        iter: &Expr,
        body: &Expr,
    ) -> Result<Type, TypeCheckError> {
        let iter_ty = self.visit_expr(iter, None)?;
        let item_ty = self.iterable_item_type(&iter_ty, iter.span)?;

        // Loop variable's type is the item type of the iterable
        self.check_bind_pattern(pattern, &item_ty)?;

        // Type check body
        self.enter_loop();
        let result = self.visit_expr(body, None);
        self.exit_loop();
        let _ = result?;

        Ok(Type::Unit)
    }

    fn iterable_item_type(&self, iter_ty: &Type, span: Span) -> Result<Type, TypeCheckError> {
        match iter_ty {
            Type::Range { elem_ty } => Ok((**elem_ty).clone()),
            Type::Array { dims, .. } => {
                if dims.is_empty() {
                    return Err(
                        TypeCheckErrorKind::ForIterNotIterable(iter_ty.clone(), span).into(),
                    );
                }
                Ok(iter_ty
                    .array_item_type()
                    .unwrap_or_else(|| panic!("compiler bug: empty array dims")))
            }
            Type::Slice { elem_ty } => Ok((**elem_ty).clone()),
            _ => Err(TypeCheckErrorKind::ForIterNotIterable(iter_ty.clone(), span).into()),
        }
    }
}
