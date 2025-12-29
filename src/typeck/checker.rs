use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::context::ResolvedContext;
use crate::diagnostics::Span;
use crate::resolve::def_map::DefId;
use crate::type_rel::{TypeAssignability, type_assignable};
use crate::types::{EnumVariant, StructField, Type};

use super::errors::TypeCheckError;
use super::overloads::{FuncOverloadResolver, FuncOverloadSig, FuncParamSig};
use super::type_map::{TypeMap, TypeMapBuilder, resolve_type_expr};

pub struct TypeChecker {
    context: ResolvedContext,
    type_map_builder: TypeMapBuilder,
    func_sigs: HashMap<String, Vec<FuncOverloadSig>>,
    type_decls: HashMap<String, Type>,
    errors: Vec<TypeCheckError>,
}

impl TypeChecker {
    pub fn new(context: ResolvedContext) -> Self {
        Self {
            context,
            type_map_builder: TypeMapBuilder::new(),
            func_sigs: HashMap::new(),
            type_decls: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn check(&mut self) -> Result<TypeMap, Vec<TypeCheckError>> {
        self.populate_function_symbols()?;
        self.populate_type_symbols()?;

        // Create split borrows so we can iterate immutably over functions while
        // mutably updating the builder & errors.
        let mut checker = Checker {
            context: &self.context,
            func_sigs: &self.func_sigs,
            type_decls: &self.type_decls,
            builder: &mut self.type_map_builder,
            errors: &mut self.errors,
        };

        for function in self.context.module.funcs() {
            if checker.type_check_function(function).is_err() {
                break;
            }
        }

        if self.errors.is_empty() {
            // Workaround for moving out of a field: replace with new one, then finish the old
            let builder = std::mem::replace(&mut self.type_map_builder, TypeMapBuilder::new());
            Ok(builder.finish())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn insert_func_overload(
        &mut self,
        def_id: DefId,
        sig: FunctionSig,
    ) -> Result<(), Vec<TypeCheckError>> {
        let params = sig
            .params
            .iter()
            .map(|p| {
                let ty = resolve_type_expr(&self.context.def_map, &p.typ)?;
                Ok(FuncParamSig {
                    name: p.name.clone(),
                    ty,
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| vec![e])?;

        let return_type =
            resolve_type_expr(&self.context.def_map, &sig.return_type).map_err(|e| vec![e])?;

        self.func_sigs
            .entry(sig.name.clone())
            .or_default()
            .push(FuncOverloadSig {
                def_id,
                params,
                return_type,
            });

        Ok(())
    }

    fn populate_function_symbols(&mut self) -> Result<(), Vec<TypeCheckError>> {
        let mut overloads = Vec::new();

        // Func decls
        for decl in self.context.module.func_decls() {
            let def_id = self.context.def_map.lookup_def(decl.id).unwrap().id;
            overloads.push((def_id, decl.sig.clone()));
        }

        // Funcs
        for func in self.context.module.funcs() {
            let def_id = self.context.def_map.lookup_def(func.id).unwrap().id;
            overloads.push((def_id, func.sig.clone()));
        }

        for (def_id, sig) in overloads {
            self.insert_func_overload(def_id, sig)?;
        }

        Ok(())
    }

    fn populate_type_symbols(&mut self) -> Result<(), Vec<TypeCheckError>> {
        for type_decl in self.context.module.type_decls() {
            match &type_decl.kind {
                TypeDeclKind::Alias { aliased_ty } => {
                    // Resolve the aliased type
                    let ty = resolve_type_expr(&self.context.def_map, aliased_ty)
                        .map_err(|e| vec![e])?;

                    self.type_decls.insert(type_decl.name.clone(), ty);
                }

                TypeDeclKind::Struct { fields } => {
                    // Resolve each struct field type
                    let struct_fields = fields
                        .iter()
                        .map(|f| {
                            let field_ty = resolve_type_expr(&self.context.def_map, &f.ty)
                                .map_err(|e| vec![e])?;
                            Ok(StructField {
                                name: f.name.clone(),
                                ty: field_ty,
                            })
                        })
                        .collect::<Result<_, Vec<TypeCheckError>>>()?;

                    // Create the struct type
                    let ty = Type::Struct {
                        name: type_decl.name.clone(),
                        fields: struct_fields,
                    };

                    self.type_decls.insert(type_decl.name.clone(), ty);
                }

                TypeDeclKind::Enum { variants } => {
                    // Collect the enum variant names + payload types
                    let mut enum_variants = Vec::new();
                    for variant in variants {
                        let payload = variant
                            .payload
                            .iter()
                            .map(|ty_expr| resolve_type_expr(&self.context.def_map, ty_expr))
                            .collect::<Result<Vec<Type>, _>>()
                            .map_err(|e| vec![e])?;

                        enum_variants.push(EnumVariant {
                            name: variant.name.clone(),
                            payload,
                        });
                    }

                    // Create the enum type
                    let ty = Type::Enum {
                        name: type_decl.name.clone(),
                        variants: enum_variants,
                    };

                    self.type_decls.insert(type_decl.name.clone(), ty);
                }
            }
        }
        Ok(())
    }
}

// Internal helper used during checking to avoid self-borrow conflicts. It holds
// split borrows into the owning TypeChecker fields so we can iterate over
// functions (&context.module.funcs) while still mutably updating the builder
// and error vec. This removes the need for moving the functions Vec out of the
// context (previous mem::take workaround) and keeps borrow scopes minimal.
struct Checker<'c, 'b> {
    context: &'c ResolvedContext,
    func_sigs: &'c HashMap<String, Vec<FuncOverloadSig>>,
    type_decls: &'c HashMap<String, Type>,
    builder: &'b mut TypeMapBuilder,
    errors: &'b mut Vec<TypeCheckError>,
}

impl<'c, 'b> Checker<'c, 'b> {
    fn lookup_def_type(&self, node: NodeId) -> Option<Type> {
        self.context
            .def_map
            .lookup_def(node)
            .and_then(|def| self.builder.lookup_def_type(def))
    }

    fn type_check_function(&mut self, function: &Function) -> Result<Type, Vec<TypeCheckError>> {
        // Lookup the function by def id and find the matching overload
        let func_def_id = self.context.def_map.lookup_def(function.id).unwrap().id;
        let overloads = self.func_sigs.get(&function.sig.name).expect(&format!(
            "Function {} not found in func_sigs",
            function.sig.name
        ));
        let func_sig = overloads
            .iter()
            .find(|sig| sig.def_id == func_def_id)
            .expect(&format!(
                "Overload for function {} not found",
                function.sig.name
            ));

        // Record param types
        for (param, param_sig) in function.sig.params.iter().zip(func_sig.params.iter()) {
            match self.context.def_map.lookup_def(param.id) {
                Some(def) => {
                    self.builder
                        .record_def_type(def.clone(), param_sig.ty.clone());
                    self.builder
                        .record_node_type(param.id, param_sig.ty.clone());
                }
                None => panic!("Parameter {} not found in def_map", param.name),
            }
        }

        // type check body
        if let Err(e) = self.type_check_expr(&function.body) {
            self.errors.push(e);
            return Err(self.errors.clone());
        }

        // check return type
        let ret_expr = match &function.body.kind {
            ExprKind::Block {
                tail: Some(tail), ..
            } => tail.as_ref(),
            _ => &function.body,
        };
        let ret_ty = match self.type_check_expr_with_expected(ret_expr, Some(&func_sig.return_type))
        {
            Ok(ty) => ty,
            Err(e) => {
                self.errors.push(e);
                return Err(self.errors.clone());
            }
        };

        match self.check_assignable_to(ret_expr, &ret_ty, &func_sig.return_type) {
            Ok(()) => {}
            Err(_) => {
                self.errors.push(TypeCheckError::FuncReturnTypeMismatch(
                    func_sig.return_type.clone(),
                    ret_ty.clone(),
                    ret_expr.span,
                ));
            }
        }

        // record return type
        self.builder.record_node_type(function.id, ret_ty.clone());
        if self.errors.is_empty() {
            Ok(ret_ty)
        } else {
            Err(self.errors.clone())
        }
    }

    fn type_check_array_lit(
        &mut self,
        elem_ty_expr: Option<&TypeExpr>,
        elems: &[Expr],
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        if elems.is_empty() {
            return Err(TypeCheckError::EmptyArrayLiteral(span));
        }

        let elem_ty = if let Some(elem_ty_expr) = elem_ty_expr {
            let elem_ty = resolve_type_expr(&self.context.def_map, elem_ty_expr)?;
            for elem in elems {
                let this_ty = self.type_check_expr_with_expected(elem, Some(&elem_ty))?;
                self.check_assignable_to(elem, &this_ty, &elem_ty)?;
            }
            elem_ty
        } else {
            // all elements must have the same type
            let elem_ty = self.type_check_expr(&elems[0])?;
            for elem in &elems[1..] {
                let this_ty = self.type_check_expr(elem)?;
                if this_ty != elem_ty {
                    return Err(TypeCheckError::ArrayElementTypeMismatch(
                        elem_ty, this_ty, elem.span,
                    ));
                }
            }
            elem_ty
        };

        // Build dimensions vector
        let array_ty = match elem_ty {
            // If elements are arrays, prepend this dimension to their dimensions
            Type::Array {
                elem_ty: inner_elem_ty,
                dims: inner_dims,
            } => {
                let mut new_dims = vec![elems.len()];
                new_dims.extend(inner_dims);
                Ok(Type::Array {
                    elem_ty: inner_elem_ty,
                    dims: new_dims,
                })
            }
            _ => Ok(Type::Array {
                elem_ty: Box::new(elem_ty),
                dims: vec![elems.len()],
            }),
        }?;

        Ok(array_ty)
    }

    fn type_check_array_lit_with_expected(
        &mut self,
        elems: &[Expr],
        expected: &Type,
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        if elems.is_empty() {
            return Err(TypeCheckError::EmptyArrayLiteral(span));
        }

        let Type::Array {
            elem_ty: expected_elem_ty,
            dims: expected_dims,
        } = expected
        else {
            return self.type_check_array_lit(None, elems, span);
        };

        // When the expected array has multiple dimensions, each element is itself
        // an array of the remaining dimensions. Use that as the expected element type.
        let elem_expected_ty = if expected_dims.len() > 1 {
            Type::Array {
                elem_ty: expected_elem_ty.clone(),
                dims: expected_dims[1..].to_vec(),
            }
        } else {
            expected_elem_ty.as_ref().clone()
        };

        // Check each element against the expected element type; we don't infer
        // element type from the first element when a declared type exists.
        for elem in elems {
            let this_ty = self.type_check_expr_with_expected(elem, Some(&elem_expected_ty))?;
            self.check_assignable_to(elem, &this_ty, &elem_expected_ty)?;
        }

        // Build dimensions vector
        let array_ty = match &elem_expected_ty {
            // If elements are arrays, prepend this dimension to their dimensions
            Type::Array {
                elem_ty: inner_elem_ty,
                dims: inner_dims,
            } => {
                let mut new_dims = vec![elems.len()];
                new_dims.extend(inner_dims);
                Type::Array {
                    elem_ty: inner_elem_ty.clone(),
                    dims: new_dims,
                }
            }
            _ => Type::Array {
                elem_ty: Box::new(elem_expected_ty),
                dims: vec![elems.len()],
            },
        };

        Ok(array_ty)
    }

    fn type_check_array_index(
        &mut self,
        elem_ty: &Type,
        dims: &[usize],
        indices: &[Expr],
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        // Check we don't have more indices than dimensions
        if indices.len() > dims.len() {
            return Err(TypeCheckError::TooManyIndices(
                dims.len(),
                indices.len(),
                span,
            ));
        }

        // type check each index
        for index in indices {
            let index_type = self.type_check_expr(index)?;
            if index_type != Type::UInt64 {
                return Err(TypeCheckError::IndexTypeNotInt(index_type, index.span));
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

    fn type_check_slice(
        &mut self,
        target: &Expr,
        start: &Option<Box<Expr>>,
        end: &Option<Box<Expr>>,
    ) -> Result<Type, TypeCheckError> {
        let target_ty = self.type_check_expr(target)?;
        let (elem_ty, dims) = match target_ty {
            Type::Array { elem_ty, dims } => (elem_ty, dims),
            other => return Err(TypeCheckError::SliceTargetNotArray(other, target.span)),
        };

        // Restrict slices to 1-D arrays (for now)
        if dims.len() != 1 {
            return Err(TypeCheckError::SliceTargetNot1DArray(
                Type::Array { elem_ty, dims },
                target.span,
            ));
        }

        // Type check start and end (must be u64)
        if let Some(start) = start {
            let ty = self.type_check_expr(start)?;
            if ty != Type::UInt64 {
                return Err(TypeCheckError::IndexTypeNotInt(ty, start.span));
            }
        }
        if let Some(end) = end {
            let ty = self.type_check_expr(end)?;
            if ty != Type::UInt64 {
                return Err(TypeCheckError::IndexTypeNotInt(ty, end.span));
            }
        }

        Ok(Type::Slice { elem_ty })
    }

    fn type_check_string_index(
        &mut self,
        indices: &[Expr],
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        // Check we have exactly one index
        if indices.len() > 1 {
            return Err(TypeCheckError::TooManyIndices(1, indices.len(), span));
        }

        let index_ty = self.type_check_expr(&indices[0])?;
        if index_ty != Type::UInt64 {
            return Err(TypeCheckError::IndexTypeNotInt(index_ty, indices[0].span));
        }

        Ok(Type::Char)
    }

    fn type_check_tuple_lit(&mut self, fields: &[Expr]) -> Result<Type, TypeCheckError> {
        if fields.is_empty() {
            return Err(TypeCheckError::EmptyTupleLiteral(fields[0].span));
        }

        // Type check each field
        let mut field_types = Vec::new();
        for field in fields {
            let field_ty = self.type_check_expr(field)?;
            field_types.push(field_ty);
        }

        Ok(Type::Tuple {
            fields: field_types,
        })
    }

    fn type_check_tuple_field_access(
        &mut self,
        target: &Expr,
        index: usize,
    ) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.type_check_expr(target)?;
        match target_ty {
            Type::Tuple { fields } => {
                let index_usize = index;
                if index_usize >= fields.len() {
                    return Err(TypeCheckError::TupleFieldOutOfBounds(
                        fields.len(),
                        index,
                        target.span,
                    ));
                }

                Ok(fields[index_usize].clone())
            }
            _ => Err(TypeCheckError::InvalidTupleFieldTarget(
                target_ty,
                target.span,
            )),
        }
    }

    fn type_check_struct_lit(
        &mut self,
        name: &String,
        fields: &[StructLitField],
    ) -> Result<Type, TypeCheckError> {
        let Some(struct_ty) = self.type_decls.get(name) else {
            for field in fields {
                let _ = self.type_check_expr(&field.value)?;
            }
            return Ok(Type::Unknown);
        };
        let Type::Struct {
            fields: struct_fields,
            ..
        } = struct_ty
        else {
            for field in fields {
                let _ = self.type_check_expr(&field.value)?;
            }
            return Ok(Type::Unknown);
        };

        for field in fields {
            let actual_ty = self.type_check_expr(&field.value)?;
            if let Some(expected) = struct_fields.iter().find(|f| f.name == field.name) {
                if actual_ty != expected.ty {
                    return Err(TypeCheckError::StructFieldTypeMismatch(
                        field.name.clone(),
                        expected.ty.clone(),
                        actual_ty,
                        field.span,
                    ));
                }
            }
        }

        Ok(struct_ty.clone())
    }

    fn type_check_field_access(
        &mut self,
        target: &Expr,
        field: &str,
    ) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.type_check_expr(target)?;
        match target_ty {
            Type::Struct { fields, .. } => match fields.iter().find(|f| f.name == field) {
                Some(field) => Ok(field.ty.clone()),
                None => Ok(Type::Unknown),
            },
            _ => Err(TypeCheckError::InvalidStructFieldTarget(
                target_ty,
                target.span,
            )),
        }
    }

    fn type_check_enum_variant(
        &mut self,
        enum_name: &String,
        variant_name: &String,
        payload: &[Expr],
    ) -> Result<Type, TypeCheckError> {
        // Lookup the type
        let Some(enum_ty) = self.type_decls.get(enum_name) else {
            for expr in payload {
                let _ = self.type_check_expr(expr)?;
            }
            return Ok(Type::Unknown);
        };

        let Type::Enum { variants, .. } = enum_ty else {
            for expr in payload {
                let _ = self.type_check_expr(expr)?;
            }
            return Ok(Type::Unknown);
        };

        // Get the variant
        let Some(variant_ty) = variants.iter().find(|v| v.name == *variant_name) else {
            for expr in payload {
                let _ = self.type_check_expr(expr)?;
            }
            return Ok(enum_ty.clone());
        };

        if payload.len() != variant_ty.payload.len() {
            for expr in payload {
                let _ = self.type_check_expr(expr)?;
            }
            return Ok(enum_ty.clone());
        }

        // Type check each payload element
        for (i, (payload_expr, payload_ty)) in
            payload.iter().zip(variant_ty.payload.iter()).enumerate()
        {
            let actual_ty = self.type_check_expr(payload_expr)?;
            if actual_ty != *payload_ty {
                return Err(TypeCheckError::EnumVariantPayloadTypeMismatch(
                    variant_name.clone(),
                    i,
                    payload_ty.clone(),
                    actual_ty,
                    payload_expr.span,
                ));
            }
        }

        Ok(enum_ty.clone())
    }

    fn type_check_struct_update(
        &mut self,
        target: &Expr,
        fields: &[StructUpdateField],
    ) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.type_check_expr(target)?;
        let struct_ty = match &target_ty {
            Type::Struct { .. } => target_ty.clone(),
            _ => {
                return Err(TypeCheckError::InvalidStructUpdateTarget(
                    target_ty,
                    target.span,
                ));
            }
        };

        let Type::Struct {
            fields: struct_fields,
            ..
        } = &struct_ty
        else {
            return Ok(Type::Unknown);
        };

        for field in fields {
            let actual_ty = self.type_check_expr(&field.value)?;
            if let Some(expected) = struct_fields.iter().find(|f| f.name == field.name) {
                if actual_ty != expected.ty {
                    return Err(TypeCheckError::StructFieldTypeMismatch(
                        field.name.clone(),
                        expected.ty.clone(),
                        actual_ty,
                        field.span,
                    ));
                }
            }
        }

        Ok(struct_ty)
    }

    fn type_check_block(
        &mut self,
        items: &Vec<BlockItem>,
        tail: &Option<&Expr>,
    ) -> Result<Type, TypeCheckError> {
        for item in items {
            match item {
                BlockItem::Stmt(stmt) => {
                    self.type_check_stmt_expr(stmt)?;
                }
                BlockItem::Expr(expr) => {
                    self.type_check_expr(expr)?;
                }
            }
        }

        match tail {
            Some(expr) => self.type_check_expr(expr),
            None => Ok(Type::Unit),
        }
    }

    fn type_check_pattern(
        &mut self,
        pattern: &Pattern,
        value_ty: &Type,
    ) -> Result<(), TypeCheckError> {
        match &pattern.kind {
            PatternKind::Ident { .. } => {
                // Record this identifier's type
                if let Some(def) = self.context.def_map.lookup_def(pattern.id) {
                    self.builder.record_def_type(def.clone(), value_ty.clone());
                }
                Ok(())
            }
            PatternKind::Array { patterns } => {
                // Value must be an array
                match value_ty {
                    Type::Array { elem_ty, dims } => {
                        // Check the pattern has the right number of elements
                        if patterns.len() != dims[0] {
                            return Err(TypeCheckError::ArrayPatternLengthMismatch(
                                dims[0],
                                patterns.len(),
                                pattern.span,
                            ));
                        }

                        // Determine the sub-type for each pattern element
                        let sub_ty = if dims.len() == 1 {
                            // 1D array: each element has the element type
                            (**elem_ty).clone()
                        } else {
                            // Multi-dim array: each element is an array with the remaining dims
                            Type::Array {
                                elem_ty: elem_ty.clone(),
                                dims: dims[1..].to_vec(),
                            }
                        };

                        // Recursively type check each sub-pattern
                        for pattern in patterns {
                            self.type_check_pattern(pattern, &sub_ty)?;
                        }
                        Ok(())
                    }
                    _ => Err(TypeCheckError::PatternTypeMismatch(
                        pattern.clone(),
                        value_ty.clone(),
                        pattern.span,
                    )),
                }
            }
            PatternKind::Tuple { patterns } => {
                match value_ty {
                    Type::Tuple { fields } => {
                        if patterns.len() != fields.len() {
                            return Err(TypeCheckError::TuplePatternLengthMismatch(
                                fields.len(),
                                patterns.len(),
                                pattern.span,
                            ));
                        }

                        // Recursively type check each sub-pattern
                        for (pattern, field) in patterns.iter().zip(fields) {
                            self.type_check_pattern(pattern, field)?;
                        }
                        Ok(())
                    }
                    _ => Err(TypeCheckError::PatternTypeMismatch(
                        pattern.clone(),
                        value_ty.clone(),
                        pattern.span,
                    )),
                }
            }
            PatternKind::Struct { name, fields } => {
                let Type::Struct {
                    name: ty_name,
                    fields: struct_fields,
                } = value_ty
                else {
                    return Err(TypeCheckError::PatternTypeMismatch(
                        pattern.clone(),
                        value_ty.clone(),
                        pattern.span,
                    ));
                };

                // Check that the struct type name matches
                if ty_name != name {
                    return Err(TypeCheckError::PatternTypeMismatch(
                        pattern.clone(),
                        value_ty.clone(),
                        pattern.span,
                    ));
                }

                // Check each field pattern
                for field in fields {
                    // Type check the field
                    if let Some(expected_ty) = struct_fields
                        .iter()
                        .find(|f| f.name == field.name)
                        .map(|f| &f.ty)
                    {
                        self.type_check_pattern(&field.pattern, expected_ty)?;
                    }
                }

                Ok(())
            }
        }
    }

    fn type_check_binding(
        &mut self,
        pattern: &Pattern,
        decl_ty: &Option<TypeExpr>,
        value: &Expr,
    ) -> Result<Type, TypeCheckError> {
        // resolve the declaration type (if present)
        let expected_ty = decl_ty
            .as_ref()
            .map(|ty_expr| resolve_type_expr(&self.context.def_map, ty_expr))
            .transpose()?;

        let mut value_ty = self.type_check_expr_with_expected(value, expected_ty.as_ref())?;

        if let Some(decl_ty) = &expected_ty {
            self.check_assignable_to(value, &value_ty, decl_ty)?;
            value_ty = decl_ty.clone();
            if matches!(&value.kind, ExprKind::ArrayLit { .. })
                && matches!(value_ty, Type::Array { .. })
            {
                self.builder.record_node_type(value.id, value_ty.clone());
            }
        }

        self.type_check_pattern(pattern, &value_ty)?;

        Ok(Type::Unit)
    }

    fn check_assignable_to(
        &mut self,
        from_value: &Expr,
        from_ty: &Type,
        to_ty: &Type,
    ) -> Result<(), TypeCheckError> {
        match type_assignable(from_ty, to_ty) {
            TypeAssignability::Incompatible => Err(TypeCheckError::DeclTypeMismatch(
                to_ty.clone(),
                from_ty.clone(),
                from_value.span,
            )),
            _ => Ok(()),
        }
    }

    fn type_check_assign(&mut self, assignee: &Expr, value: &Expr) -> Result<Type, TypeCheckError> {
        let lhs_type = self.type_check_expr(assignee)?;
        let rhs_type = self.type_check_expr_with_expected(value, Some(&lhs_type))?;

        match self.check_assignable_to(value, &rhs_type, &lhs_type) {
            Ok(()) => Ok(Type::Unit),
            Err(_) => Err(TypeCheckError::AssignTypeMismatch(
                lhs_type,
                rhs_type,
                assignee.span,
            )),
        }
    }

    fn type_check_var_ref(&mut self, var_ref_expr: &Expr) -> Result<Type, TypeCheckError> {
        match self.lookup_def_type(var_ref_expr.id) {
            Some(def_type) => Ok(def_type.clone()),
            None => Err(TypeCheckError::UnknownType(var_ref_expr.span)),
        }
    }

    fn type_check_call(
        &mut self,
        call_expr: &Expr,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<Type, TypeCheckError> {
        let name = match &callee.kind {
            ExprKind::Var(name) => name,
            _ => {
                for arg in args {
                    let _ = self.type_check_expr(arg)?;
                }
                return Ok(Type::Unknown);
            }
        };

        // Compute argument types first to avoid holding an immutable borrow of self.funcs
        let mut arg_types = Vec::new();
        for arg in args {
            let ty = self.type_check_expr(arg)?;
            arg_types.push(ty);
        }
        // Get overload set for the function
        let Some(overloads) = self.func_sigs.get(name) else {
            return Err(TypeCheckError::UnknownType(callee.span));
        };
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
                return Err(TypeCheckError::ArgCountMismatch(
                    name.to_string(),
                    expected,
                    arg_types.len(),
                    call_expr.span,
                ));
            }
        }

        let resolved =
            FuncOverloadResolver::new(name, args, &arg_types, call_expr.span).resolve(overloads)?;

        self.builder.record_call_def(call_expr.id, resolved.def_id);
        // Check argument types against the resolved overload, re-typing with expectations
        for (i, arg) in args.iter().enumerate() {
            let param_ty = &resolved.sig.params[i].ty;
            let arg_ty = self.type_check_expr_with_expected(arg, Some(param_ty))?;
            match self.check_assignable_to(arg, &arg_ty, param_ty) {
                Ok(()) => continue,
                Err(_) => {
                    let span = arg.span;
                    return Err(TypeCheckError::ArgTypeMismatch(
                        i + 1,
                        param_ty.clone(),
                        arg_ty.clone(),
                        span,
                    ));
                }
            }
        }
        Ok(resolved.sig.return_type.clone())
    }

    fn type_check_if(
        &mut self,
        cond: &Expr,
        then_body: &Expr,
        else_body: &Expr,
    ) -> Result<Type, TypeCheckError> {
        let cond_type = self.type_check_expr(cond)?;
        if cond_type != Type::Bool {
            return Err(TypeCheckError::CondNotBoolean(cond_type, cond.span));
        }

        let then_type = self.type_check_expr(then_body)?;
        let else_type = self.type_check_expr(else_body)?;
        if then_type != else_type {
            // create a span that covers both the then and else bodies so the
            // diagnostic highlights the whole region
            let span = Span::merge_all(vec![then_body.span, else_body.span]);
            return Err(TypeCheckError::ThenElseTypeMismatch(
                then_type, else_type, span,
            ));
        }

        Ok(then_type)
    }

    fn type_check_while(&mut self, cond: &Expr, body: &Expr) -> Result<Type, TypeCheckError> {
        let cond_type = self.type_check_expr(cond)?;
        if cond_type != Type::Bool {
            return Err(TypeCheckError::CondNotBoolean(cond_type, cond.span));
        }

        let _ = self.type_check_expr(body)?;
        Ok(Type::Unit)
    }

    fn type_check_for(
        &mut self,
        pattern: &Pattern,
        iter: &Expr,
        body: &Expr,
    ) -> Result<Type, TypeCheckError> {
        let iter_ty = self.type_check_expr(iter)?;
        let item_ty = self.iterable_item_type(&iter_ty, iter.span)?;

        // Loop variable's type is the item type of the iterable
        self.type_check_pattern(pattern, &item_ty)?;

        // Type check body
        let _ = self.type_check_expr(body)?;

        Ok(Type::Unit)
    }

    fn type_check_match(
        &mut self,
        scrutinee: &Expr,
        arms: &[MatchArm],
    ) -> Result<Type, TypeCheckError> {
        let scrutinee_ty = self.type_check_expr(scrutinee)?;

        let (enum_name, variants) = match scrutinee_ty {
            Type::Enum { name, variants } => (Some(name.clone()), variants),
            _ => (None, Vec::new()),
        };
        let mut arm_ty: Option<Type> = None;

        for arm in arms {
            if let Some(enum_name) = &enum_name {
                self.type_check_match_pattern(enum_name, &variants, &arm.pattern)?;
            }

            let body_ty = self.type_check_expr(&arm.body)?;
            if let Some(expected_ty) = &arm_ty {
                if body_ty != *expected_ty {
                    return Err(TypeCheckError::MatchArmTypeMismatch(
                        expected_ty.clone(),
                        body_ty,
                        arm.span,
                    ));
                }
            } else {
                arm_ty = Some(body_ty);
            }
        }

        Ok(arm_ty.unwrap_or(Type::Unit))
    }

    fn type_check_match_pattern(
        &mut self,
        enum_name: &String,
        variants: &[EnumVariant],
        pattern: &MatchPattern,
    ) -> Result<(), TypeCheckError> {
        match pattern {
            MatchPattern::Wildcard { .. } => Ok(()),

            MatchPattern::EnumVariant {
                enum_name: pat_enum_name,
                variant_name,
                bindings,
                ..
            } => {
                if let Some(pat_enum_name) = pat_enum_name
                    && pat_enum_name != enum_name
                {
                    return Ok(());
                }

                if let Some(variant) = variants.iter().find(|v| v.name == *variant_name) {
                    if bindings.len() == variant.payload.len() {
                        for (binding, ty) in bindings.iter().zip(variant.payload.iter()) {
                            match self.context.def_map.lookup_def(binding.id) {
                                Some(def) => {
                                    self.builder.record_def_type(def.clone(), ty.clone());
                                    self.builder.record_node_type(binding.id, ty.clone());
                                }
                                None => panic!(
                                    "compiler bug: binding [{}] not found in def_map",
                                    binding.id
                                ),
                            }
                        }
                    } else {
                        for binding in bindings {
                            if let Some(def) = self.context.def_map.lookup_def(binding.id) {
                                self.builder.record_def_type(def.clone(), Type::Unknown);
                                self.builder.record_node_type(binding.id, Type::Unknown);
                            }
                        }
                    }
                } else {
                    for binding in bindings {
                        if let Some(def) = self.context.def_map.lookup_def(binding.id) {
                            self.builder.record_def_type(def.clone(), Type::Unknown);
                            self.builder.record_node_type(binding.id, Type::Unknown);
                        }
                    }
                }

                Ok(())
            }
        }
    }

    fn type_check_bin_op(
        &mut self,
        left: &Expr,
        op: &BinaryOp,
        right: &Expr,
    ) -> Result<Type, TypeCheckError> {
        let left_type = self.type_check_expr(left)?;
        let right_type = self.type_check_expr(right)?;

        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                if left_type != Type::UInt64 || right_type != Type::UInt64 {
                    let span = Span::merge_all(vec![left.span, right.span]);
                    return Err(TypeCheckError::ArithTypeMismatch(
                        left_type, right_type, span,
                    ));
                }
                Ok(Type::UInt64)
            }
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::LtEq
            | BinaryOp::GtEq => {
                let span = Span::merge_all(vec![left.span, right.span]);
                if left_type != right_type {
                    Err(TypeCheckError::CmpTypeMismatch(left_type, right_type, span))
                } else if !left_type.is_scalar() {
                    Err(TypeCheckError::CmpNonScalar(left_type, span))
                } else {
                    Ok(Type::Bool)
                }
            }
        }
    }

    fn type_check_stmt_expr(&mut self, stmt: &StmtExpr) -> Result<Type, TypeCheckError> {
        let ty = match &stmt.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => self.type_check_binding(pattern, decl_ty, value)?,

            StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => self.type_check_binding(pattern, decl_ty, value)?,

            StmtExprKind::Assign { assignee, value } => self.type_check_assign(assignee, value)?,

            StmtExprKind::While { cond, body } => self.type_check_while(cond, body)?,

            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => self.type_check_for(pattern, iter, body)?,
        };

        self.builder.record_node_type(stmt.id, ty.clone());
        Ok(ty)
    }

    fn type_check_expr_with_expected(
        &mut self,
        expr: &Expr,
        expected: Option<&Type>,
    ) -> Result<Type, TypeCheckError> {
        match (&expr.kind, expected) {
            (ExprKind::IntLit(_), Some(expected_ty))
                if matches!(expected_ty, Type::UInt8 | Type::UInt32 | Type::UInt64) =>
            {
                self.builder.record_node_type(expr.id, expected_ty.clone());
                Ok(expected_ty.clone())
            }
            (
                ExprKind::ArrayLit {
                    elem_ty: None,
                    elems,
                },
                Some(expected_ty @ Type::Array { .. }),
            ) => {
                let ty = self.type_check_array_lit_with_expected(elems, expected_ty, expr.span)?;
                self.builder.record_node_type(expr.id, ty.clone());
                Ok(ty)
            }
            _ => self.type_check_expr(expr),
        }
    }

    fn type_check_expr(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
        let result = match &expr.kind {
            ExprKind::IntLit(_) => Ok(Type::UInt64),

            ExprKind::BoolLit(_) => Ok(Type::Bool),

            ExprKind::CharLit(_) => Ok(Type::Char),

            ExprKind::StringLit { .. } => Ok(Type::String),

            ExprKind::UnitLit => Ok(Type::Unit),

            ExprKind::ArrayLit { elem_ty, elems } => {
                self.type_check_array_lit(elem_ty.as_ref(), elems, expr.span)
            }

            ExprKind::ArrayIndex { target, indices } => {
                let target_ty = self.type_check_expr(target)?;
                match target_ty {
                    Type::Array { elem_ty, dims } => {
                        self.type_check_array_index(&elem_ty, &dims, indices, target.span)
                    }
                    Type::String => self.type_check_string_index(indices, target.span),
                    _ => {
                        return Err(TypeCheckError::InvalidIndexTargetType(
                            target_ty,
                            target.span,
                        ));
                    }
                }
            }

            ExprKind::TupleLit(fields) => self.type_check_tuple_lit(fields),

            ExprKind::TupleField { target, index } => {
                self.type_check_tuple_field_access(target, *index)
            }

            ExprKind::StructLit { name, fields } => self.type_check_struct_lit(name, fields),

            ExprKind::StructField { target, field } => self.type_check_field_access(target, field),

            ExprKind::StructUpdate { target, fields } => {
                self.type_check_struct_update(target, fields)
            }

            ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
            } => self.type_check_enum_variant(enum_name, variant, payload),

            ExprKind::BinOp { left, op, right } => self.type_check_bin_op(left, op, right),

            ExprKind::UnaryOp { expr, .. } => self.type_check_expr(expr),

            ExprKind::Block { items, tail } => self.type_check_block(items, &tail.as_deref()),

            ExprKind::Var(_) => self.type_check_var_ref(expr),

            ExprKind::Call { callee, args } => self.type_check_call(expr, callee, args),

            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => self.type_check_if(cond, then_body, else_body),

            ExprKind::Range { start, end } => Ok(Type::Range {
                min: *start,
                max: *end,
            }),

            ExprKind::Slice { target, start, end } => self.type_check_slice(target, start, end),

            ExprKind::Match { scrutinee, arms } => self.type_check_match(scrutinee, arms),
        };

        result.map(|ty| {
            self.builder.record_node_type(expr.id, ty.clone());
            ty.clone()
        })
    }

    // --- Helper functions ---

    fn iterable_item_type(&self, iter_ty: &Type, span: Span) -> Result<Type, TypeCheckError> {
        match iter_ty {
            Type::Range { .. } => Ok(Type::UInt64),
            Type::Array { elem_ty, dims } => {
                if dims.is_empty() {
                    return Err(TypeCheckError::ForIterNotIterable(iter_ty.clone(), span));
                }
                if dims.len() == 1 {
                    Ok((**elem_ty).clone())
                } else {
                    Ok(Type::Array {
                        elem_ty: Box::new((**elem_ty).clone()),
                        dims: dims[1..].to_vec(),
                    })
                }
            }
            _ => Err(TypeCheckError::ForIterNotIterable(iter_ty.clone(), span)),
        }
    }
}
