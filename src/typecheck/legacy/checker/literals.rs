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
                    let Some(struct_def) = self
                        .resolve_type_def(&struct_ty)
                        .and_then(|def| def.as_struct())
                    else {
                        for field in fields {
                            let _ = self.check_expr(&field.value, Expected::Unknown)?;
                        }
                        return Ok(Type::Unknown);
                    };

                    for field in fields {
                        let Some(expected) = struct_def.field(&field.name) else {
                            let _ = self.check_expr(&field.value, Expected::Unknown)?;
                            continue;
                        };
                        let actual_ty =
                            self.check_expr(&field.value, Expected::Exact(&expected.ty))?;
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
                    let _ = self.check_expr(&field.value, Expected::Unknown)?;
                }
                return Ok(Type::Unknown);
            }
        };
        let Some(struct_def) = self
            .resolve_type_def(&struct_ty)
            .and_then(|def| def.as_struct())
        else {
            for field in fields {
                let _ = self.check_expr(&field.value, Expected::Unknown)?;
            }
            return Ok(Type::Unknown);
        };

        for field in fields {
            let actual_ty = self.check_expr(&field.value, Expected::Unknown)?;
            if let Some(expected) = struct_def.field(&field.name)
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
                    let Some(enum_def) = self
                        .resolve_type_def(&enum_ty)
                        .and_then(|def| def.as_enum())
                    else {
                        for expr in payload {
                            let _ = self.check_expr(expr, Expected::Unknown)?;
                        }
                        return Ok(Type::Unknown);
                    };

                    let Some(variant_ty) = enum_def.variant(variant_name) else {
                        for expr in payload {
                            let _ = self.check_expr(expr, Expected::Unknown)?;
                        }
                        let enum_ty = self.apply_infer(&enum_ty);
                        return Ok(enum_ty);
                    };

                    self.check_enum_variant_payload(
                        variant_name,
                        variant_ty.payload(),
                        &payload.iter().collect::<Vec<_>>(),
                        span,
                        Expected::Unknown,
                    )?;

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
                    let _ = self.check_expr(expr, Expected::Unknown)?;
                }
                return Ok(Type::Unknown);
            }
        };

        let Some(enum_def) = self
            .resolve_type_def(&enum_ty)
            .and_then(|def| def.as_enum())
        else {
            for expr in payload {
                let _ = self.check_expr(expr, Expected::Unknown)?;
            }
            return Ok(Type::Unknown);
        };

        // Get the variant
        let Some(variant_ty) = enum_def.variant(variant_name) else {
            for expr in payload {
                let _ = self.check_expr(expr, Expected::Unknown)?;
            }
            return Ok(enum_ty.clone());
        };

        self.check_enum_variant_payload(
            variant_name,
            variant_ty.payload(),
            &payload.iter().collect::<Vec<_>>(),
            span,
            Expected::Unknown,
        )?;

        Ok(enum_ty.clone())
    }
}
