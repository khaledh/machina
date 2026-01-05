use std::collections::{HashMap, HashSet};

use crate::ast::AstFolder;
use crate::ast::fold::{walk_expr, walk_if};
use crate::ast::*;
use crate::context::ResolvedContext;
use crate::diag::Span;
use crate::resolve::def_map::DefId;
use crate::types::{EnumVariant, StructField, Type};
use crate::types::{TypeAssignability, type_assignable};

use super::errors::{TypeCheckError, TypeCheckErrorKind};
use super::overloads::{FuncOverloadResolver, FuncOverloadSig, FuncParamSig};
use super::type_map::{TypeMap, TypeMapBuilder, resolve_type_expr};

pub struct TypeChecker {
    context: ResolvedContext,
    type_map_builder: TypeMapBuilder,
    func_sigs: HashMap<String, Vec<FuncOverloadSig>>,
    method_sigs: HashMap<String, HashMap<String, Vec<FuncOverloadSig>>>,
    type_decls: HashMap<String, Type>,
    errors: Vec<TypeCheckError>,
    halted: bool,
}

impl TypeChecker {
    pub fn new(context: ResolvedContext) -> Self {
        Self {
            context,
            type_map_builder: TypeMapBuilder::new(),
            func_sigs: HashMap::new(),
            method_sigs: HashMap::new(),
            type_decls: HashMap::new(),
            errors: Vec::new(),
            halted: false,
        }
    }

    pub fn check(&mut self) -> Result<TypeMap, Vec<TypeCheckError>> {
        self.populate_type_symbols()?;
        self.populate_function_symbols()?;
        self.populate_method_symbols()?;

        self.halted = false;
        let module = self.context.module.clone();
        let _ = self.visit_module(&module);

        if self.errors.is_empty() {
            let builder = std::mem::take(&mut self.type_map_builder);
            Ok(builder.finish())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
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

    fn populate_method_symbols(&mut self) -> Result<(), Vec<TypeCheckError>> {
        for method_block in self.context.module.method_blocks() {
            let type_name = method_block.type_name.clone();
            for method in &method_block.methods {
                let def_id = self.context.def_map.lookup_def(method.id).unwrap().id;
                let params = self.build_param_sigs(&method.sig.params)?;
                let return_type = self.resolve_return_type(&method.sig.return_type)?;

                self.method_sigs
                    .entry(type_name.clone())
                    .or_default()
                    .entry(method.sig.name.clone())
                    .or_default()
                    .push(FuncOverloadSig {
                        def_id,
                        params,
                        return_type,
                    });
            }
        }

        Ok(())
    }

    fn insert_func_overload(
        &mut self,
        def_id: DefId,
        sig: FunctionSig,
    ) -> Result<(), Vec<TypeCheckError>> {
        let params = self.build_param_sigs(&sig.params)?;
        let return_type = self.resolve_return_type(&sig.return_type)?;

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

    fn build_param_sigs(
        &self,
        params: &[FunctionParam],
    ) -> Result<Vec<FuncParamSig>, Vec<TypeCheckError>> {
        params
            .iter()
            .map(|param| {
                let ty = resolve_type_expr(&self.context.def_map, &param.typ)?;
                Ok(FuncParamSig {
                    name: param.name.clone(),
                    ty,
                    mode: param.mode.clone(),
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| vec![e])
    }

    fn resolve_return_type(&self, return_type: &TypeExpr) -> Result<Type, Vec<TypeCheckError>> {
        resolve_type_expr(&self.context.def_map, return_type).map_err(|e| vec![e])
    }

    fn lookup_def_type(&self, node: NodeId) -> Option<Type> {
        self.context
            .def_map
            .lookup_def(node)
            .and_then(|def| self.type_map_builder.lookup_def_type(def))
    }

    fn expand_shallow_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Struct { name, fields } if fields.is_empty() => self
                .type_decls
                .get(name)
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            Type::Enum { name, variants } if variants.is_empty() => self
                .type_decls
                .get(name)
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            _ => ty.clone(),
        }
    }

    fn check_function(&mut self, function: &Function) -> Result<Type, Vec<TypeCheckError>> {
        // Lookup the function by def id and find the matching overload.
        let func_def_id = self.context.def_map.lookup_def(function.id).unwrap().id;
        let (param_types, return_type) = {
            let overloads = self.func_sigs.get(&function.sig.name).unwrap_or_else(|| {
                panic!(
                    "compiler bug: function {} not found in func_sigs",
                    function.sig.name
                )
            });
            let func_sig = overloads
                .iter()
                .find(|sig| sig.def_id == func_def_id)
                .unwrap_or_else(|| {
                    panic!(
                        "compiler bug: overload for function {} not found",
                        function.sig.name
                    )
                });
            (
                func_sig
                    .params
                    .iter()
                    .map(|param| param.ty.clone())
                    .collect::<Vec<Type>>(),
                func_sig.return_type.clone(),
            )
        };

        // Record param types
        for (param, param_ty) in function.sig.params.iter().zip(param_types.iter()) {
            match self.context.def_map.lookup_def(param.id) {
                Some(def) => {
                    self.type_map_builder
                        .record_def_type(def.clone(), param_ty.clone());
                    self.type_map_builder
                        .record_node_type(param.id, param_ty.clone());
                }
                None => panic!("Parameter {} not found in def_map", param.name),
            }
        }

        let body_ty = match self.visit_expr(&function.body, Some(&return_type)) {
            Ok(ty) => ty,
            Err(e) => {
                self.errors.push(e);
                return Err(self.errors.clone());
            }
        };

        let return_span = self.function_return_span(&function.body);
        if matches!(
            type_assignable(&body_ty, &return_type),
            TypeAssignability::Incompatible
        ) {
            self.errors.push(
                TypeCheckErrorKind::DeclTypeMismatch(
                    return_type.clone(),
                    body_ty.clone(),
                    return_span,
                )
                .into(),
            );
            return Err(self.errors.clone());
        }

        // record return type
        self.type_map_builder
            .record_node_type(function.id, body_ty.clone());
        if self.errors.is_empty() {
            Ok(body_ty)
        } else {
            Err(self.errors.clone())
        }
    }

    fn check_method(
        &mut self,
        method_block: &MethodBlock,
        method: &Method,
    ) -> Result<Type, Vec<TypeCheckError>> {
        let self_ty = match self.type_decls.get(&method_block.type_name) {
            Some(ty) => ty.clone(),
            None => {
                self.errors
                    .push(TypeCheckErrorKind::UnknownType(method_block.span).into());
                return Err(self.errors.clone());
            }
        };

        let return_type = match self.resolve_return_type(&method.sig.return_type) {
            Ok(ty) => ty,
            Err(errs) => {
                self.errors.extend(errs);
                return Err(self.errors.clone());
            }
        };

        let param_sigs = match self.build_param_sigs(&method.sig.params) {
            Ok(params) => params,
            Err(errs) => {
                self.errors.extend(errs);
                return Err(self.errors.clone());
            }
        };
        let param_types = param_sigs
            .iter()
            .map(|param| param.ty.clone())
            .collect::<Vec<_>>();

        match self.context.def_map.lookup_def(method.sig.self_param.id) {
            Some(def) => {
                self.type_map_builder
                    .record_def_type(def.clone(), self_ty.clone());
                self.type_map_builder
                    .record_node_type(method.sig.self_param.id, self_ty.clone());
            }
            None => panic!("self parameter not found in def_map"),
        }

        for (param, param_ty) in method.sig.params.iter().zip(param_types.iter()) {
            match self.context.def_map.lookup_def(param.id) {
                Some(def) => {
                    self.type_map_builder
                        .record_def_type(def.clone(), param_ty.clone());
                    self.type_map_builder
                        .record_node_type(param.id, param_ty.clone());
                }
                None => panic!("Parameter {} not found in def_map", param.name),
            }
        }

        let body_ty = match self.visit_expr(&method.body, Some(&return_type)) {
            Ok(ty) => ty,
            Err(e) => {
                self.errors.push(e);
                return Err(self.errors.clone());
            }
        };

        let return_span = self.function_return_span(&method.body);
        if matches!(
            type_assignable(&body_ty, &return_type),
            TypeAssignability::Incompatible
        ) {
            self.errors.push(
                TypeCheckErrorKind::DeclTypeMismatch(
                    return_type.clone(),
                    body_ty.clone(),
                    return_span,
                )
                .into(),
            );
            return Err(self.errors.clone());
        }

        self.type_map_builder
            .record_node_type(method.id, body_ty.clone());

        if self.errors.is_empty() {
            Ok(body_ty)
        } else {
            Err(self.errors.clone())
        }
    }

    fn function_return_span(&self, body: &Expr) -> Span {
        match &body.kind {
            ExprKind::Block { items, tail } => {
                if let Some(tail) = tail {
                    return tail.span;
                }
                if let Some(last) = items.last() {
                    return match last {
                        BlockItem::Stmt(stmt) => stmt.span,
                        BlockItem::Expr(expr) => expr.span,
                    };
                }
                body.span
            }
            _ => body.span,
        }
    }

    fn check_array_lit(
        &mut self,
        elem_ty_expr: Option<&TypeExpr>,
        init: &ArrayLitInit,
        expected: Option<&Type>,
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        let len = self.array_lit_len(init, span)?;

        // Resolve the element type
        let elem_ty = if let Some(elem_ty_expr) = elem_ty_expr {
            resolve_type_expr(&self.context.def_map, elem_ty_expr)?
        } else if let Some(Type::Array {
            elem_ty: expected_elem_ty,
            dims: expected_dims,
        }) = expected
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
                ArrayLitInit::Elems(elems) => self.visit_expr(&elems[0], None)?,
                ArrayLitInit::Repeat(expr, _) => self.visit_expr(expr, None)?,
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

    fn check_array_index(
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
            let index_type = self.visit_expr(index, None)?;
            if index_type != Type::uint(64) {
                return Err(TypeCheckErrorKind::IndexTypeNotInt(index_type, index.span).into());
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

    fn check_slice(
        &mut self,
        target: &Expr,
        start: &Option<Box<Expr>>,
        end: &Option<Box<Expr>>,
    ) -> Result<Type, TypeCheckError> {
        let target_ty = self.visit_expr(target, None)?;

        // Type check start and end (must be u64)
        if let Some(start) = start {
            let ty = self.visit_expr(start, None)?;
            if ty != Type::uint(64) {
                return Err(TypeCheckErrorKind::IndexTypeNotInt(ty, start.span).into());
            }
        }
        if let Some(end) = end {
            let ty = self.visit_expr(end, None)?;
            if ty != Type::uint(64) {
                return Err(TypeCheckErrorKind::IndexTypeNotInt(ty, end.span).into());
            }
        }

        // Slices are allowed only for arrays and strings.
        match target_ty {
            Type::Array { .. } => {
                let Some(slice_elem_ty) = target_ty.array_item_type() else {
                    return Err(TypeCheckErrorKind::SliceTargetZeroDimArray(
                        target_ty,
                        target.span,
                    )
                    .into());
                };

                Ok(Type::Slice {
                    elem_ty: Box::new(slice_elem_ty),
                })
            }
            Type::Slice { elem_ty } => Ok(Type::Slice { elem_ty }),
            Type::String => Ok(Type::Slice {
                elem_ty: Box::new(Type::uint(8)),
            }),
            other => {
                Err(TypeCheckErrorKind::SliceTargetNotArrayOrString(other, target.span).into())
            }
        }
    }

    fn check_string_index(&mut self, indices: &[Expr], span: Span) -> Result<Type, TypeCheckError> {
        // Check we have exactly one index
        if indices.len() > 1 {
            return Err(TypeCheckErrorKind::TooManyIndices(1, indices.len(), span).into());
        }

        let index_ty = self.visit_expr(&indices[0], None)?;
        if index_ty != Type::uint(64) {
            return Err(TypeCheckErrorKind::IndexTypeNotInt(index_ty, indices[0].span).into());
        }

        Ok(Type::uint(8))
    }

    fn check_slice_index(
        &mut self,
        elem_ty: &Type,
        indices: &[Expr],
        span: Span,
    ) -> Result<Type, TypeCheckError> {
        // Slices are 1D, so only a single index is allowed.
        if indices.len() != 1 {
            return Err(TypeCheckErrorKind::TooManyIndices(1, indices.len(), span).into());
        }

        let index_ty = self.visit_expr(&indices[0], None)?;
        if index_ty != Type::uint(64) {
            return Err(TypeCheckErrorKind::IndexTypeNotInt(index_ty, indices[0].span).into());
        }

        Ok(elem_ty.clone())
    }

    fn check_tuple_lit(&mut self, fields: &[Expr]) -> Result<Type, TypeCheckError> {
        if fields.is_empty() {
            return Err(TypeCheckErrorKind::EmptyTupleLiteral(fields[0].span).into());
        }

        // Type check each field
        let field_types = self.visit_exprs(fields)?;

        Ok(Type::Tuple {
            fields: field_types,
        })
    }

    fn check_tuple_field_access(
        &mut self,
        target: &Expr,
        index: usize,
    ) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.visit_expr(target, None)?;
        let mut peeled_ty = target_ty.clone();
        while let Type::Heap { elem_ty } = peeled_ty {
            peeled_ty = *elem_ty;
        }
        peeled_ty = self.expand_shallow_type(&peeled_ty);

        match peeled_ty {
            Type::Tuple { fields } => {
                let index_usize = index;
                if index_usize >= fields.len() {
                    return Err(TypeCheckErrorKind::TupleFieldOutOfBounds(
                        fields.len(),
                        index,
                        target.span,
                    )
                    .into());
                }

                Ok(fields[index_usize].clone())
            }
            _ => Err(TypeCheckErrorKind::InvalidTupleFieldTarget(target_ty, target.span).into()),
        }
    }

    fn check_struct_lit(
        &mut self,
        name: &String,
        fields: &[StructLitField],
    ) -> Result<Type, TypeCheckError> {
        let struct_ty = match self.type_decls.get(name) {
            Some(ty) => ty.clone(),
            None => {
                for field in fields {
                    let _ = self.visit_expr(&field.value, None)?;
                }
                return Ok(Type::Unknown);
            }
        };
        let Type::Struct {
            fields: struct_fields,
            ..
        } = &struct_ty
        else {
            for field in fields {
                let _ = self.visit_expr(&field.value, None)?;
            }
            return Ok(Type::Unknown);
        };

        for field in fields {
            let actual_ty = self.visit_expr(&field.value, None)?;
            if let Some(expected) = struct_fields.iter().find(|f| f.name == field.name)
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

    fn check_field_access(&mut self, target: &Expr, field: &str) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.visit_expr(target, None)?;
        let mut peeled_ty = target_ty.clone();
        while let Type::Heap { elem_ty } = peeled_ty {
            peeled_ty = *elem_ty;
        }
        peeled_ty = self.expand_shallow_type(&peeled_ty);

        match peeled_ty {
            Type::Struct { fields, .. } => match fields.iter().find(|f| f.name == field) {
                Some(field) => Ok(field.ty.clone()),
                None => Ok(Type::Unknown),
            },
            _ => Err(TypeCheckErrorKind::InvalidStructFieldTarget(target_ty, target.span).into()),
        }
    }

    fn check_enum_variant(
        &mut self,
        enum_name: &String,
        variant_name: &String,
        payload: &[Expr],
    ) -> Result<Type, TypeCheckError> {
        // Lookup the type
        let enum_ty = match self.type_decls.get(enum_name) {
            Some(ty) => ty.clone(),
            None => {
                for expr in payload {
                    let _ = self.visit_expr(expr, None)?;
                }
                return Ok(Type::Unknown);
            }
        };

        let Type::Enum { variants, .. } = &enum_ty else {
            for expr in payload {
                let _ = self.visit_expr(expr, None)?;
            }
            return Ok(Type::Unknown);
        };

        // Get the variant
        let Some(variant_ty) = variants.iter().find(|v| v.name == *variant_name) else {
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

        Ok(enum_ty)
    }

    fn check_struct_update(
        &mut self,
        target: &Expr,
        fields: &[StructUpdateField],
    ) -> Result<Type, TypeCheckError> {
        // Type check target
        let target_ty = self.visit_expr(target, None)?;
        let struct_ty = match &target_ty {
            Type::Struct { .. } => target_ty.clone(),
            _ => {
                return Err(
                    TypeCheckErrorKind::InvalidStructUpdateTarget(target_ty, target.span).into(),
                );
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
            let actual_ty = self.visit_expr(&field.value, None)?;
            if let Some(expected) = struct_fields.iter().find(|f| f.name == field.name)
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

    fn check_pattern(&mut self, pattern: &Pattern, value_ty: &Type) -> Result<(), TypeCheckError> {
        match &pattern.kind {
            PatternKind::Ident { .. } => {
                // Record this identifier's type
                if let Some(def) = self.context.def_map.lookup_def(pattern.id) {
                    self.type_map_builder
                        .record_def_type(def.clone(), value_ty.clone());
                }
                Ok(())
            }
            PatternKind::Array { patterns } => {
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
                            self.check_pattern(pattern, &sub_ty)?;
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
            PatternKind::Tuple { patterns } => {
                match value_ty {
                    Type::Tuple { fields } => {
                        if patterns.len() != fields.len() {
                            return Err(TypeCheckErrorKind::TuplePatternLengthMismatch(
                                fields.len(),
                                patterns.len(),
                                pattern.span,
                            )
                            .into());
                        }

                        // Recursively type check each sub-pattern
                        for (pattern, field) in patterns.iter().zip(fields) {
                            self.check_pattern(pattern, field)?;
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
            PatternKind::Struct { name, fields } => {
                let Type::Struct {
                    name: ty_name,
                    fields: struct_fields,
                } = value_ty
                else {
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
                    if let Some(expected_ty) = struct_fields
                        .iter()
                        .find(|f| f.name == field.name)
                        .map(|f| &f.ty)
                    {
                        self.check_pattern(&field.pattern, expected_ty)?;
                    }
                }

                Ok(())
            }
        }
    }

    fn check_binding(
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

        let mut value_ty = self.visit_expr(value, expected_ty.as_ref())?;

        if let Some(decl_ty) = &expected_ty {
            self.check_assignable_to(value, &value_ty, decl_ty)?;
            value_ty = decl_ty.clone();
            if matches!(&value.kind, ExprKind::ArrayLit { .. })
                && matches!(value_ty, Type::Array { .. })
            {
                self.type_map_builder
                    .record_node_type(value.id, value_ty.clone());
            }
        }

        self.check_pattern(pattern, &value_ty)?;

        Ok(Type::Unit)
    }

    fn check_assignable_to(
        &mut self,
        from_value: &Expr,
        from_ty: &Type,
        to_ty: &Type,
    ) -> Result<(), TypeCheckError> {
        match type_assignable(from_ty, to_ty) {
            TypeAssignability::Incompatible => Err(TypeCheckErrorKind::DeclTypeMismatch(
                to_ty.clone(),
                from_ty.clone(),
                from_value.span,
            )
            .into()),
            _ => Ok(()),
        }
    }

    fn check_assign(&mut self, assignee: &Expr, value: &Expr) -> Result<Type, TypeCheckError> {
        // Reject string index assignment (for now)
        if let ExprKind::ArrayIndex { target, .. } = &assignee.kind {
            let target_ty = self.visit_expr(target, None)?;
            if target_ty == Type::String {
                return Err(TypeCheckErrorKind::StringIndexAssign(assignee.span).into());
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

    fn check_var_ref(&mut self, var_ref_expr: &Expr) -> Result<Type, TypeCheckError> {
        match self.lookup_def_type(var_ref_expr.id) {
            Some(def_type) => Ok(def_type.clone()),
            None => Err(TypeCheckErrorKind::UnknownType(var_ref_expr.span).into()),
        }
    }

    fn single_arity_param_types(
        overloads: &[FuncOverloadSig],
        arg_count: usize,
    ) -> Option<Vec<Type>> {
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

    fn check_call(
        &mut self,
        call_expr: &Expr,
        callee: &Expr,
        args: &[CallArg],
    ) -> Result<Type, TypeCheckError> {
        let name = match &callee.kind {
            ExprKind::Var(name) => name,
            _ => {
                let _ = self.visit_call(callee, args)?;
                return Ok(Type::Unknown);
            }
        };

        // Compute argument types first to avoid holding an immutable borrow of self.funcs
        let arg_types = self.visit_call_args(args)?;
        // Get overload set for the function
        let (resolved, fallback_param_types) = {
            let Some(overloads) = self.func_sigs.get(name) else {
                return Err(TypeCheckErrorKind::UnknownType(callee.span).into());
            };
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
                    return Err(TypeCheckErrorKind::ArgCountMismatch(
                        name.to_string(),
                        expected,
                        arg_types.len(),
                        call_expr.span,
                    )
                    .into());
                }
            }

            let resolved = FuncOverloadResolver::new(name, args, &arg_types, call_expr.span)
                .resolve(overloads)
                .map(|resolved| {
                    let param_types = resolved
                        .sig
                        .params
                        .iter()
                        .map(|param| param.ty.clone())
                        .collect::<Vec<_>>();
                    let return_type = resolved.sig.return_type.clone();
                    (resolved.def_id, param_types, return_type)
                });
            (resolved, fallback_param_types)
        };

        let (def_id, param_types, return_type) = match resolved {
            Ok(resolved) => resolved,
            Err(err) => {
                if matches!(err.kind(), TypeCheckErrorKind::FuncOverloadNoMatch(_, _)) {
                    if let Some(param_types) = fallback_param_types {
                        if let Err(err) = self.check_call_arg_types(args, &param_types) {
                            return Err(err);
                        }
                    }
                }
                return Err(err);
            }
        };

        self.type_map_builder.record_call_def(call_expr.id, def_id);
        self.check_call_arg_types(args, &param_types)?;
        Ok(return_type)
    }

    fn check_method_call(
        &mut self,
        call_expr: &Expr,
        target: &Expr,
        method: &str,
        args: &[CallArg],
    ) -> Result<Type, TypeCheckError> {
        let target_ty = self.visit_expr(target, None)?;
        let mut peeled_ty = target_ty.clone();
        while let Type::Heap { elem_ty } = peeled_ty {
            peeled_ty = *elem_ty;
        }
        peeled_ty = self.expand_shallow_type(&peeled_ty);

        let type_name = match peeled_ty {
            Type::Struct { name, .. } | Type::Enum { name, .. } => name,
            _ => {
                return Err(
                    TypeCheckErrorKind::InvalidStructFieldTarget(target_ty, target.span).into(),
                );
            }
        };

        let arg_types = self.visit_call_args(args)?;
        let Some(type_methods) = self.method_sigs.get(&type_name) else {
            return Err(TypeCheckErrorKind::UnknownType(call_expr.span).into());
        };
        let Some(overloads) = type_methods.get(method) else {
            return Err(TypeCheckErrorKind::UnknownType(call_expr.span).into());
        };
        let (resolved, fallback_param_types) = {
            let name = format!("{}::{}", type_name, method);
            let fallback_param_types = Self::single_arity_param_types(overloads, arg_types.len());
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
                    return Err(TypeCheckErrorKind::ArgCountMismatch(
                        name,
                        expected,
                        arg_types.len(),
                        call_expr.span,
                    )
                    .into());
                }
            }

            let resolved = FuncOverloadResolver::new(&name, args, &arg_types, call_expr.span)
                .resolve(overloads)
                .map(|resolved| {
                    let param_types = resolved
                        .sig
                        .params
                        .iter()
                        .map(|param| param.ty.clone())
                        .collect::<Vec<_>>();
                    let return_type = resolved.sig.return_type.clone();
                    (resolved.def_id, param_types, return_type)
                });
            (resolved, fallback_param_types)
        };

        let (def_id, param_types, return_type) = match resolved {
            Ok(resolved) => resolved,
            Err(err) => {
                if matches!(err.kind(), TypeCheckErrorKind::FuncOverloadNoMatch(_, _)) {
                    if let Some(param_types) = fallback_param_types {
                        if let Err(err) = self.check_call_arg_types(args, &param_types) {
                            return Err(err);
                        }
                    }
                }
                return Err(err);
            }
        };

        self.type_map_builder.record_call_def(call_expr.id, def_id);
        self.check_call_arg_types(args, &param_types)?;

        Ok(return_type)
    }

    fn check_while(&mut self, cond: &Expr, body: &Expr) -> Result<Type, TypeCheckError> {
        let cond_type = self.visit_expr(cond, None)?;
        if cond_type != Type::Bool {
            return Err(TypeCheckErrorKind::CondNotBoolean(cond_type, cond.span).into());
        }

        let _ = self.visit_expr(body, None)?;
        Ok(Type::Unit)
    }

    fn check_for(
        &mut self,
        pattern: &Pattern,
        iter: &Expr,
        body: &Expr,
    ) -> Result<Type, TypeCheckError> {
        let iter_ty = self.visit_expr(iter, None)?;
        let item_ty = self.iterable_item_type(&iter_ty, iter.span)?;

        // Loop variable's type is the item type of the iterable
        self.check_pattern(pattern, &item_ty)?;

        // Type check body
        let _ = self.visit_expr(body, None)?;

        Ok(Type::Unit)
    }

    fn iterable_item_type(&self, iter_ty: &Type, span: Span) -> Result<Type, TypeCheckError> {
        match iter_ty {
            Type::Range { .. } => Ok(Type::uint(64)),
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
            _ => Err(TypeCheckErrorKind::ForIterNotIterable(iter_ty.clone(), span).into()),
        }
    }

    fn check_match(&mut self, scrutinee: &Expr, arms: &[MatchArm]) -> Result<Type, TypeCheckError> {
        let scrutinee_ty = self.visit_expr(scrutinee, None)?;
        let mut peeled_ty = scrutinee_ty;
        while let Type::Heap { elem_ty } = peeled_ty {
            peeled_ty = *elem_ty;
        }
        let peeled_ty = self.expand_shallow_type(&peeled_ty);
        let (enum_name, variants) = match peeled_ty {
            Type::Enum { name, variants } => (Some(name.clone()), variants),
            _ => (None, Vec::new()),
        };
        let mut arm_ty: Option<Type> = None;

        self.visit_match_arms(arms, |this, arm| {
            this.check_match_arm(arm, &enum_name, &variants, &mut arm_ty)
        })?;

        Ok(arm_ty.unwrap_or(Type::Unit))
    }

    fn check_match_arm(
        &mut self,
        arm: &MatchArm,
        enum_name: &Option<String>,
        variants: &[EnumVariant],
        arm_ty: &mut Option<Type>,
    ) -> Result<(), TypeCheckError> {
        if let Some(enum_name) = enum_name {
            self.check_match_pattern(enum_name, variants, &arm.pattern)?;
        }

        let body_ty = self.visit_match_arm(arm)?;
        if let Some(expected_ty) = arm_ty {
            if body_ty != *expected_ty {
                return Err(TypeCheckErrorKind::MatchArmTypeMismatch(
                    expected_ty.clone(),
                    body_ty,
                    arm.span,
                )
                .into());
            }
        } else {
            *arm_ty = Some(body_ty);
        }

        Ok(())
    }

    fn check_match_pattern(
        &mut self,
        enum_name: &String,
        variants: &[EnumVariant],
        pattern: &MatchPattern,
    ) -> Result<(), TypeCheckError> {
        match pattern {
            MatchPattern::Wildcard { .. } => Ok(()),
            MatchPattern::BoolLit { .. } => Ok(()),

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
                            if let MatchPatternBinding::Named { id, .. } = binding {
                                match self.context.def_map.lookup_def(*id) {
                                    Some(def) => {
                                        self.type_map_builder
                                            .record_def_type(def.clone(), ty.clone());
                                        self.type_map_builder.record_node_type(*id, ty.clone());
                                    }
                                    None => panic!(
                                        "compiler bug: binding [{}] not found in def_map",
                                        id
                                    ),
                                }
                            }
                        }
                    } else {
                        for binding in bindings {
                            if let MatchPatternBinding::Named { id, .. } = binding {
                                if let Some(def) = self.context.def_map.lookup_def(*id) {
                                    self.type_map_builder
                                        .record_def_type(def.clone(), Type::Unknown);
                                    self.type_map_builder.record_node_type(*id, Type::Unknown);
                                }
                            }
                        }
                    }
                } else {
                    for binding in bindings {
                        if let MatchPatternBinding::Named { id, .. } = binding {
                            if let Some(def) = self.context.def_map.lookup_def(*id) {
                                self.type_map_builder
                                    .record_def_type(def.clone(), Type::Unknown);
                                self.type_map_builder.record_node_type(*id, Type::Unknown);
                            }
                        }
                    }
                }

                Ok(())
            }
        }
    }
}

impl AstFolder for TypeChecker {
    type Error = TypeCheckError;
    type Output = Type;
    type Input = Type;

    fn visit_func(&mut self, func: &Function) -> Result<Type, TypeCheckError> {
        if self.halted {
            return Ok(Type::Unit);
        }

        if self.check_function(func).is_err() {
            self.halted = true;
        }

        Ok(Type::Unit)
    }

    fn visit_method_block(
        &mut self,
        method_block: &MethodBlock,
    ) -> Result<Vec<Type>, TypeCheckError> {
        if self.halted {
            return Ok(Vec::new());
        }

        let mut outputs = Vec::new();
        for method in &method_block.methods {
            if self.check_method(method_block, method).is_err() {
                self.halted = true;
                break;
            }
            outputs.push(Type::Unit);
        }

        Ok(outputs)
    }

    fn visit_if(
        &mut self,
        cond: &Expr,
        then_body: &Expr,
        else_body: &Expr,
    ) -> Result<(Type, Type, Type), TypeCheckError> {
        let (cond_type, then_type, else_type) = walk_if(self, cond, then_body, else_body)?;
        if cond_type != Type::Bool {
            return Err(TypeCheckErrorKind::CondNotBoolean(cond_type, cond.span).into());
        }

        if then_type != else_type {
            // create a span that covers both the then and else bodies so the
            // diagnostic highlights the whole region
            let span = Span::merge_all(vec![then_body.span, else_body.span]);
            return Err(
                TypeCheckErrorKind::ThenElseTypeMismatch(then_type, else_type, span).into(),
            );
        }

        Ok((cond_type, then_type, else_type))
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) -> Result<Type, TypeCheckError> {
        let ty = match &stmt.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => self.check_binding(pattern, decl_ty, value)?,

            StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => self.check_binding(pattern, decl_ty, value)?,

            StmtExprKind::VarDecl { decl_ty, .. } => {
                let ty = resolve_type_expr(&self.context.def_map, decl_ty)?;
                if let Some(def) = self.context.def_map.lookup_def(stmt.id) {
                    self.type_map_builder.record_def_type(def.clone(), ty);
                }
                Type::Unit
            }

            StmtExprKind::Assign { assignee, value } => self.check_assign(assignee, value)?,

            StmtExprKind::While { cond, body } => self.check_while(cond, body)?,

            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => self.check_for(pattern, iter, body)?,
        };

        self.type_map_builder.record_node_type(stmt.id, ty.clone());
        Ok(ty)
    }

    fn visit_expr(&mut self, expr: &Expr, expected: Option<&Type>) -> Result<Type, TypeCheckError> {
        let result = match (&expr.kind, expected) {
            (ExprKind::IntLit(_), Some(expected_ty)) if expected_ty.is_int() => {
                Ok(expected_ty.clone())
            }

            (
                ExprKind::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: operand,
                },
                Some(expected_ty @ Type::Int { signed: true, .. }),
            ) if matches!(operand.kind, ExprKind::IntLit(_)) => {
                let _ = self.visit_expr(operand, Some(expected_ty))?;
                Ok(expected_ty.clone())
            }

            _ => match &expr.kind {
                ExprKind::IntLit(_) => Ok(Type::uint(64)),

                ExprKind::BoolLit(_) => Ok(Type::Bool),

                ExprKind::CharLit(_) => Ok(Type::Char),

                ExprKind::StringLit { .. } => Ok(Type::String),

                ExprKind::UnitLit => Ok(Type::Unit),

                ExprKind::HeapAlloc { expr } => {
                    let inner_expected = match expected {
                        Some(Type::Heap { elem_ty }) => Some(elem_ty.as_ref()),
                        _ => None,
                    };
                    let elem_ty = self.visit_expr(expr, inner_expected)?;
                    Ok(Type::Heap {
                        elem_ty: Box::new(elem_ty),
                    })
                }

                ExprKind::StringFmt { segments } => {
                    for segment in segments {
                        if let StringFmtSegment::Expr { expr, span } = segment {
                            let ty = walk_expr(self, expr)?;
                            if !ty.is_int() && ty != Type::String {
                                return Err(TypeCheckErrorKind::StringFmtExprUnsupportedType(
                                    ty, *span,
                                )
                                .into());
                            }
                        }
                    }
                    Ok(Type::String)
                }

                ExprKind::ArrayLit { elem_ty, init } => {
                    self.check_array_lit(elem_ty.as_ref(), init, expected, expr.span)
                }

                ExprKind::ArrayIndex { target, indices } => {
                    let target_ty = self.visit_expr(target, None)?;
                    let mut peeled_ty = target_ty.clone();
                    while let Type::Heap { elem_ty } = peeled_ty {
                        peeled_ty = *elem_ty;
                    }

                    match peeled_ty {
                        Type::Array { elem_ty, dims } => {
                            self.check_array_index(elem_ty.as_ref(), &dims, indices, target.span)
                        }
                        Type::Slice { elem_ty } => {
                            self.check_slice_index(elem_ty.as_ref(), indices, target.span)
                        }
                        Type::String => self.check_string_index(indices, target.span),
                        _ => {
                            return Err(TypeCheckErrorKind::InvalidIndexTargetType(
                                target_ty,
                                target.span,
                            )
                            .into());
                        }
                    }
                }

                ExprKind::TupleLit(fields) => self.check_tuple_lit(fields),

                ExprKind::TupleField { target, index } => {
                    self.check_tuple_field_access(target, *index)
                }

                ExprKind::StructLit { name, fields } => self.check_struct_lit(name, fields),

                ExprKind::StructField { target, field } => self.check_field_access(target, field),

                ExprKind::StructUpdate { target, fields } => {
                    self.check_struct_update(target, fields)
                }

                ExprKind::EnumVariant {
                    enum_name,
                    variant,
                    payload,
                } => self.check_enum_variant(enum_name, variant, payload),

                ExprKind::BinOp { left, op, right } => {
                    let (left_type, right_type) = self.visit_binary_expr(left, right)?;
                    match op {
                        // Arithmetic operators
                        BinaryOp::Add
                        | BinaryOp::Sub
                        | BinaryOp::Mul
                        | BinaryOp::Div
                        | BinaryOp::Mod
                        | BinaryOp::BitOr
                        | BinaryOp::BitXor
                        | BinaryOp::BitAnd
                        | BinaryOp::Shl
                        | BinaryOp::Shr => {
                            if !left_type.is_int() {
                                return Err(TypeCheckErrorKind::ArithOperandNotInt(
                                    left_type, left.span,
                                )
                                .into());
                            }
                            if !right_type.is_int() {
                                return Err(TypeCheckErrorKind::ArithOperandNotInt(
                                    right_type, right.span,
                                )
                                .into());
                            }
                            if left_type != right_type {
                                let span = Span::merge_all(vec![left.span, right.span]);
                                return Err(TypeCheckErrorKind::ArithTypeMismatch(
                                    left_type, right_type, span,
                                )
                                .into());
                            }
                            Ok(left_type)
                        }

                        // Comparison operators
                        BinaryOp::Eq
                        | BinaryOp::Ne
                        | BinaryOp::Lt
                        | BinaryOp::Gt
                        | BinaryOp::LtEq
                        | BinaryOp::GtEq => {
                            if !left_type.is_int() {
                                return Err(TypeCheckErrorKind::CmpOperandNotInt(
                                    left_type, left.span,
                                )
                                .into());
                            }
                            if !right_type.is_int() {
                                return Err(TypeCheckErrorKind::CmpOperandNotInt(
                                    right_type, right.span,
                                )
                                .into());
                            }
                            Ok(Type::Bool)
                        }

                        // Logical operators
                        BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                            if left_type != Type::Bool {
                                return Err(TypeCheckErrorKind::LogicalOperandNotBoolean(
                                    left_type, left.span,
                                )
                                .into());
                            }
                            if right_type != Type::Bool {
                                return Err(TypeCheckErrorKind::LogicalOperandNotBoolean(
                                    right_type, right.span,
                                )
                                .into());
                            }
                            Ok(Type::Bool)
                        }
                    }
                }

                ExprKind::UnaryOp { op, expr } => {
                    let ty = walk_expr(self, expr)?;
                    match op {
                        UnaryOp::Neg => {
                            if !ty.is_int() {
                                return Err(TypeCheckErrorKind::NegationOperandNotInt(
                                    ty, expr.span,
                                )
                                .into());
                            }
                            Ok(ty)
                        }
                        UnaryOp::LogicalNot => {
                            if ty != Type::Bool {
                                return Err(TypeCheckErrorKind::LogicalOperandNotBoolean(
                                    ty, expr.span,
                                )
                                .into());
                            }
                            Ok(Type::Bool)
                        }
                        UnaryOp::BitNot => {
                            if !ty.is_int() {
                                return Err(
                                    TypeCheckErrorKind::ArithOperandNotInt(ty, expr.span).into()
                                );
                            }
                            Ok(ty)
                        }
                    }
                }

                ExprKind::Move { expr } => walk_expr(self, expr),

                ExprKind::Block { items, tail } => {
                    for item in items {
                        let _ = self.visit_block_item(item)?;
                    }

                    let tail_ty = self.visit_block_tail(tail.as_deref(), expected)?;

                    Ok(tail_ty.unwrap_or(Type::Unit))
                }

                ExprKind::Var(_) => self.check_var_ref(expr),

                ExprKind::MethodCall {
                    target,
                    method,
                    args,
                } => self.check_method_call(expr, target, method, args),

                ExprKind::Call { callee, args } => self.check_call(expr, callee, args),

                ExprKind::If {
                    cond,
                    then_body,
                    else_body,
                } => {
                    let (_cond, then_type, _else_type) =
                        self.visit_if(cond, then_body, else_body)?;
                    Ok(then_type)
                }

                ExprKind::Range { start, end } => Ok(Type::Range {
                    min: *start,
                    max: *end,
                }),

                ExprKind::Slice { target, start, end } => self.check_slice(target, start, end),

                ExprKind::Match { scrutinee, arms } => self.check_match(scrutinee, arms),
            },
        };

        result.map(|ty| {
            self.type_map_builder.record_node_type(expr.id, ty.clone());
            ty.clone()
        })
    }
}
