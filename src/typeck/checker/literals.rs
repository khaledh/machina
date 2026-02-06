use super::*;

impl TypeChecker {
    pub(super) fn check_struct_lit(
        &mut self,
        name: &String,
        type_args: &[TypeExpr],
        fields: &[StructLitField],
        node_id: NodeId,
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        if type_args.is_empty() && self.infer_ctx.is_some() {
            if let Some(def_id) = self.ctx.def_table.lookup_type_def_id(name) {
                let (type_params, is_struct) = match self.ctx.module.type_def_by_id(def_id) {
                    Some(type_def) => (
                        type_def.type_params.clone(),
                        matches!(type_def.kind, TypeDefKind::Struct { .. }),
                    ),
                    None => (Vec::new(), false),
                };
                if is_struct && !type_params.is_empty() {
                    let type_args = self.infer_type_args(&type_params);
                    let struct_ty = resolve_type_def_with_args(
                        &self.ctx.def_table,
                        &self.ctx.module,
                        def_id,
                        &type_args,
                    )?;
                    let Type::Struct { .. } = &struct_ty else {
                        for field in fields {
                            let _ = self.visit_expr(&field.value, None)?;
                        }
                        return Ok(Type::Unknown);
                    };

                    for field in fields {
                        let Some(expected) = self.resolve_struct_field(&struct_ty, &field.name)
                        else {
                            let _ = self.visit_expr(&field.value, None)?;
                            continue;
                        };
                        let actual_ty = self.visit_expr(&field.value, Some(&expected.ty))?;
                        let (expected_ty, actual_ty, ok) =
                            self.unify_expected_actual(&expected.ty, &actual_ty);
                        if !ok {
                            return Err(TypeCheckErrorKind::StructFieldTypeMismatch(
                                field.name.clone(),
                                expected_ty,
                                actual_ty,
                                field.span,
                            )
                            .into());
                        }
                    }

                    let struct_ty = self.apply_infer(&struct_ty);
                    return Ok(struct_ty);
                }
            }
        }

        let struct_ty = match self.resolve_named_type_expr(name, type_args, node_id, span)? {
            Some(ty) => ty,
            None => {
                for field in fields {
                    let _ = self.visit_expr(&field.value, None)?;
                }
                return Ok(Type::Unknown);
            }
        };
        let Type::Struct { .. } = &struct_ty else {
            for field in fields {
                let _ = self.visit_expr(&field.value, None)?;
            }
            return Ok(Type::Unknown);
        };

        for field in fields {
            let actual_ty = self.visit_expr(&field.value, None)?;
            if let Some(expected) = self.resolve_struct_field(&struct_ty, &field.name)
                && actual_ty != expected.ty
            {
                return Err(TypeCheckErrorKind::StructFieldTypeMismatch(
                    field.name.clone(),
                    expected.ty.clone(),
                    actual_ty,
                    field.span,
                )
                .into());
            }
        }

        Ok(struct_ty)
    }

    pub(super) fn check_enum_variant(
        &mut self,
        enum_name: &String,
        type_args: &[TypeExpr],
        variant_name: &String,
        payload: &[Expr],
        node_id: NodeId,
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        if type_args.is_empty() && self.infer_ctx.is_some() {
            if let Some(def_id) = self.ctx.def_table.lookup_type_def_id(enum_name) {
                let (type_params, is_enum) = match self.ctx.module.type_def_by_id(def_id) {
                    Some(type_def) => (
                        type_def.type_params.clone(),
                        matches!(type_def.kind, TypeDefKind::Enum { .. }),
                    ),
                    None => (Vec::new(), false),
                };
                if is_enum && !type_params.is_empty() {
                    let type_args = self.infer_type_args(&type_params);
                    let enum_ty = resolve_type_def_with_args(
                        &self.ctx.def_table,
                        &self.ctx.module,
                        def_id,
                        &type_args,
                    )?;
                    let Type::Enum { .. } = &enum_ty else {
                        for expr in payload {
                            let _ = self.visit_expr(expr, None)?;
                        }
                        return Ok(Type::Unknown);
                    };

                    let Some(variant_ty) = self.resolve_enum_variant(&enum_ty, variant_name) else {
                        for expr in payload {
                            let _ = self.visit_expr(expr, None)?;
                        }
                        let enum_ty = self.apply_infer(&enum_ty);
                        return Ok(enum_ty);
                    };

                    if payload.len() != variant_ty.payload.len() {
                        for expr in payload {
                            let _ = self.visit_expr(expr, None)?;
                        }
                        let enum_ty = self.apply_infer(&enum_ty);
                        return Ok(enum_ty);
                    }

                    for (i, (payload_expr, payload_ty)) in
                        payload.iter().zip(variant_ty.payload.iter()).enumerate()
                    {
                        let actual_ty = self.visit_expr(payload_expr, Some(payload_ty))?;
                        let (expected_ty, actual_ty, ok) =
                            self.unify_expected_actual(payload_ty, &actual_ty);
                        if !ok {
                            return Err(TypeCheckErrorKind::EnumVariantPayloadTypeMismatch(
                                variant_name.clone(),
                                i,
                                expected_ty,
                                actual_ty,
                                payload_expr.span,
                            )
                            .into());
                        }
                    }

                    let enum_ty = self.apply_infer(&enum_ty);
                    return Ok(enum_ty);
                }
            }
        }

        // Lookup the type
        let enum_ty = match self.resolve_named_type_expr(enum_name, type_args, node_id, span)? {
            Some(ty) => ty,
            None => {
                for expr in payload {
                    let _ = self.visit_expr(expr, None)?;
                }
                return Ok(Type::Unknown);
            }
        };

        let Type::Enum { .. } = &enum_ty else {
            for expr in payload {
                let _ = self.visit_expr(expr, None)?;
            }
            return Ok(Type::Unknown);
        };

        // Get the variant
        let Some(variant_ty) = self.resolve_enum_variant(&enum_ty, variant_name) else {
            for expr in payload {
                let _ = self.visit_expr(expr, None)?;
            }
            return Ok(enum_ty.clone());
        };

        if payload.len() != variant_ty.payload.len() {
            for expr in payload {
                let _ = self.visit_expr(expr, None)?;
            }
            return Ok(enum_ty.clone());
        }

        // Type check each payload element
        for (i, (payload_expr, payload_ty)) in
            payload.iter().zip(variant_ty.payload.iter()).enumerate()
        {
            let actual_ty = self.visit_expr(payload_expr, None)?;
            if actual_ty != *payload_ty {
                return Err(TypeCheckErrorKind::EnumVariantPayloadTypeMismatch(
                    variant_name.clone(),
                    i,
                    payload_ty.clone(),
                    actual_ty,
                    payload_expr.span,
                )
                .into());
            }
        }

        Ok(enum_ty.clone())
    }
}
