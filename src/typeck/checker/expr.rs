use super::*;

impl TypeChecker {
    pub(super) fn check_array_lit(
        &mut self,
        elem_ty_expr: Option<&TypeExpr>,
        init: &ArrayLitInit,
        expected: Expected<'_>,
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        let len = self.array_lit_len(init, span)?;

        // Resolve the element type
        let elem_ty = if let Some(elem_ty_expr) = elem_ty_expr {
            self.resolve_type_expr_in_scope(elem_ty_expr)?
        } else if let Some(Type::Array {
            elem_ty: expected_elem_ty,
            dims: expected_dims,
        }) = expected.as_option()
        {
            // When the expected array has multiple dimensions, each element is itself
            // an array of the remaining dimensions.
            if expected_dims.len() > 1 {
                Type::Array {
                    elem_ty: expected_elem_ty.clone(),
                    dims: expected_dims[1..].to_vec(),
                }
            } else {
                expected_elem_ty.as_ref().clone()
            }
        } else {
            match init {
                ArrayLitInit::Elems(elems) => self.check_expr(&elems[0], Expected::Unknown)?,
                ArrayLitInit::Repeat(expr, _) => self.check_expr(expr, Expected::Unknown)?,
            }
        };

        // Type check the elements
        self.check_array_lit_init(init, &elem_ty)?;

        // Build the array type
        Ok(self.build_array_type_from_elem(len, elem_ty))
    }

    fn array_lit_len(&self, init: &ArrayLitInit, span: Span) -> Result<usize, TypeCheckError> {
        match init {
            ArrayLitInit::Elems(elems) if elems.is_empty() => {
                Err(TypeCheckErrorKind::EmptyArrayLiteral(span).into())
            }
            ArrayLitInit::Repeat(_, 0) => Err(TypeCheckErrorKind::EmptyArrayLiteral(span).into()),
            ArrayLitInit::Elems(elems) => Ok(elems.len()),
            ArrayLitInit::Repeat(_, count) => Ok(*count as usize),
        }
    }

    fn check_array_lit_init(
        &mut self,
        init: &ArrayLitInit,
        elem_ty: &Type,
    ) -> Result<(), TypeCheckError> {
        let exprs: Vec<&Expr> = match init {
            ArrayLitInit::Elems(elems) => elems.iter().collect(),
            ArrayLitInit::Repeat(expr, _) => vec![expr],
        };

        let actual_types = self.visit_array_lit_init(init, Some(elem_ty))?;
        for (expr, this_ty) in exprs.into_iter().zip(actual_types) {
            self.check_assignable_to(expr, &this_ty, elem_ty)?;
        }
        Ok(())
    }

    fn build_array_type_from_elem(&self, len: usize, elem_ty: Type) -> Type {
        // Build the array type, flattening nested array dimensions.
        match elem_ty {
            Type::Array {
                elem_ty: inner_elem_ty,
                dims: inner_dims,
            } => {
                let mut new_dims = vec![len];
                new_dims.extend(inner_dims);
                Type::Array {
                    elem_ty: inner_elem_ty,
                    dims: new_dims,
                }
            }
            _ => Type::Array {
                elem_ty: Box::new(elem_ty),
                dims: vec![len],
            },
        }
    }

    pub(super) fn check_array_index(
        &mut self,
        elem_ty: &Type,
        dims: &[usize],
        indices: &[Expr],
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        // Check we don't have more indices than dimensions
        if indices.len() > dims.len() {
            return Err(TypeCheckErrorKind::TooManyIndices(dims.len(), indices.len(), span).into());
        }

        // type check each index
        for index in indices {
            let index_type = self.check_expr(index, Expected::Unknown)?;
            if index_type != Type::uint(64) {
                return Err(self.err_index_type_not_int(index_type, index.span));
            }
        }

        // Determine result type
        if indices.len() == dims.len() {
            // Fully indexed, return the element type
            Ok(elem_ty.clone())
        } else {
            // Partially indexed, return array with the remaining dimensions
            Ok(Type::Array {
                elem_ty: Box::new(elem_ty.clone()),
                dims: dims[indices.len()..].to_vec(),
            })
        }
    }

    pub(super) fn check_slice(
        &mut self,
        target: &Expr,
        start: &Option<Box<Expr>>,
        end: &Option<Box<Expr>>,
    ) -> Result<Type, TypeCheckError> {
        let target_ty = self.check_expr(target, Expected::Unknown)?;
        let view = self.view_type(&target_ty);

        // Type check start and end (must be u64)
        if let Some(start) = start {
            let ty = self.check_expr(start, Expected::Unknown)?;
            if ty != Type::uint(64) {
                return Err(self.err_index_type_not_int(ty, start.span));
            }
        }
        if let Some(end) = end {
            let ty = self.check_expr(end, Expected::Unknown)?;
            if ty != Type::uint(64) {
                return Err(self.err_index_type_not_int(ty, end.span));
            }
        }

        // Slices are allowed only for arrays and strings.
        if let Some((elem_ty, dims)) = view.as_array() {
            if dims.is_empty() {
                return Err(
                    TypeCheckErrorKind::SliceTargetZeroDimArray(target_ty, target.span).into(),
                );
            }
            let slice_elem_ty = if dims.len() == 1 {
                elem_ty.clone()
            } else {
                Type::Array {
                    elem_ty: Box::new(elem_ty.clone()),
                    dims: dims[1..].to_vec(),
                }
            };
            return Ok(Type::Slice {
                elem_ty: Box::new(slice_elem_ty),
            });
        }
        if let Some(elem_ty) = view.as_slice() {
            return Ok(Type::Slice {
                elem_ty: Box::new(elem_ty.clone()),
            });
        }
        if view.is_string() {
            return Ok(Type::Slice {
                elem_ty: Box::new(Type::uint(8)),
            });
        }

        Err(TypeCheckErrorKind::SliceTargetNotArrayOrString(target_ty, target.span).into())
    }

    pub(super) fn check_string_index(
        &mut self,
        indices: &[Expr],
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        // Check we have exactly one index
        if indices.len() > 1 {
            return Err(TypeCheckErrorKind::TooManyIndices(1, indices.len(), span).into());
        }

        let index_ty = self.check_expr(&indices[0], Expected::Unknown)?;
        if index_ty != Type::uint(64) {
            return Err(self.err_index_type_not_int(index_ty, indices[0].span));
        }

        Ok(Type::uint(8))
    }

    pub(super) fn check_slice_index(
        &mut self,
        elem_ty: &Type,
        indices: &[Expr],
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        // Slices are 1D, so only a single index is allowed.
        if indices.len() != 1 {
            return Err(TypeCheckErrorKind::TooManyIndices(1, indices.len(), span).into());
        }

        let index_ty = self.check_expr(&indices[0], Expected::Unknown)?;
        if index_ty != Type::uint(64) {
            return Err(self.err_index_type_not_int(index_ty, indices[0].span));
        }

        Ok(elem_ty.clone())
    }

    pub(super) fn check_tuple_lit(&mut self, fields: &[Expr]) -> Result<Type, TypeCheckError> {
        if fields.is_empty() {
            return Err(TypeCheckErrorKind::EmptyTupleLiteral(Span::default()).into());
        }

        // Type check each field
        let field_tys = self.visit_exprs(fields)?;

        Ok(Type::Tuple { field_tys })
    }

    pub(super) fn check_tuple_field_access(
        &mut self,
        target: &Expr,
        index: usize,
    ) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.check_expr(target, Expected::Unknown)?;
        let view = self.view_type(&target_ty);

        match view.as_tuple() {
            Some(field_tys) => {
                let index_usize = index;
                if index_usize >= field_tys.len() {
                    return Err(TypeCheckErrorKind::TupleFieldOutOfBounds(
                        field_tys.len(),
                        index,
                        target.span,
                    )
                    .into());
                }

                Ok(field_tys[index_usize].clone())
            }
            None => Err(TypeCheckErrorKind::InvalidTupleFieldTarget(target_ty, target.span).into()),
        }
    }

    pub(super) fn check_field_access(
        &mut self,
        expr_id: NodeId,
        target: &Expr,
        field: &str,
    ) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.check_expr(target, Expected::Unknown)?;
        let view = self.view_type(&target_ty);

        if let Some(prop) = self.lookup_property_sig(view.ty(), field) {
            let getter = prop.getter;
            let prop_ty = prop.ty.clone();
            let Some(getter) = getter else {
                return Err(self.err_property_not_readable(field, target.span));
            };
            self.record_property_call_sig(expr_id, getter, target_ty, ParamMode::In, Vec::new());
            return Ok(prop_ty);
        }

        if field == "len" {
            if !self.is_place_expr(target) {
                return Err(self.err_len_target_not_lvalue(target.span));
            }
            if view.is_len_target() {
                self.type_map_builder.record_call_sig(
                    expr_id,
                    CallSig {
                        def_id: None,
                        receiver: None,
                        params: Vec::new(),
                    },
                );
                return Ok(Type::uint(64));
            }
        }

        if view.as_struct().is_some() {
            return match self.resolve_struct_field(view.ty(), field) {
                Some(field) => Ok(field.ty.clone()),
                None => Ok(Type::Unknown),
            };
        }

        Err(self.err_invalid_struct_field_target(target_ty, target.span))
    }

    pub(super) fn check_struct_update(
        &mut self,
        target: &Expr,
        fields: &[StructUpdateField],
    ) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.check_expr(target, Expected::Unknown)?;
        let Some(struct_ty) = self.resolve_struct_type_for_update(target_ty.clone()) else {
            return Err(
                TypeCheckErrorKind::InvalidStructUpdateTarget(target_ty, target.span).into(),
            );
        };

        for field in fields {
            let actual_ty = self.check_expr(&field.value, Expected::Unknown)?;
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

    pub(super) fn check_var_ref(
        &mut self,
        def_id: DefId,
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        match self
            .ctx
            .def_table
            .lookup_def(def_id)
            .and_then(|def| self.type_map_builder.lookup_def_type(def))
        {
            Some(def_type) => Ok(def_type.clone()),
            None => Err(self.err_unknown_type(span)),
        }
    }

    pub(super) fn is_place_expr(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Var { .. }
            | ExprKind::Deref { .. }
            | ExprKind::ArrayIndex { .. }
            | ExprKind::TupleField { .. }
            | ExprKind::StructField { .. } => true,
            ExprKind::Move { expr } => self.is_place_expr(expr),
            _ => false,
        }
    }
}
