use std::collections::{HashMap, HashSet};

use crate::ast::{
    BinaryOp, Expr, ExprKind, Function, NodeId, Pattern, PatternKind, StructLitField, TypeDeclKind,
    TypeExpr,
};
use crate::context::ResolvedContext;
use crate::diagnostics::Span;
use crate::types::{StructField, Type};

use super::errors::TypeCheckError;
use super::type_map::{TypeMap, TypeMapBuilder, resolve_type_expr};

struct FuncSig {
    params: Vec<Type>,
    return_type: Type,
}

pub struct TypeChecker {
    context: ResolvedContext,
    type_map_builder: TypeMapBuilder,
    func_sigs: HashMap<String, FuncSig>,
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

    fn populate_function_symbols(&mut self) -> Result<(), Vec<TypeCheckError>> {
        for function in self.context.module.funcs() {
            let params = function
                .params
                .iter()
                .map(|p| resolve_type_expr(&self.context.def_map, &p.typ))
                .collect::<Result<Vec<Type>, _>>()
                .map_err(|e| vec![e])?;

            let return_type = resolve_type_expr(&self.context.def_map, &function.return_type)
                .map_err(|e| vec![e])?;

            self.func_sigs.insert(
                function.name.clone(),
                FuncSig {
                    params,
                    return_type,
                },
            );
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
    func_sigs: &'c HashMap<String, FuncSig>,
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
        // Get the resolved function signature
        let func_sig = self
            .func_sigs
            .get(&function.name)
            .expect("Function not found in func_sigs");

        // Record param types
        for (param, param_ty) in function.params.iter().zip(func_sig.params.iter()) {
            match self.context.def_map.lookup_def(param.id) {
                Some(def) => {
                    self.builder.record_def_type(def.clone(), param_ty.clone());
                    self.builder.record_node_type(param.id, param_ty.clone());
                }
                None => panic!("Parameter {} not found in def_map", param.name),
            }
        }

        // type check body
        let ret_ty = match self.type_check_expr(&function.body) {
            Ok(ty) => ty,
            Err(e) => {
                self.errors.push(e);
                return Err(self.errors.clone());
            }
        };

        // check return type
        if ret_ty != func_sig.return_type {
            // get the span of the last expression in the body
            let span = match &function.body.kind {
                ExprKind::Block(body) => body.last().unwrap().span,
                _ => function.body.span,
            };
            self.errors.push(TypeCheckError::FuncReturnTypeMismatch(
                func_sig.return_type.clone(),
                ret_ty.clone(),
                span,
            ));
        }

        // record return type
        self.builder.record_node_type(function.id, ret_ty.clone());
        if self.errors.is_empty() {
            Ok(ret_ty)
        } else {
            Err(self.errors.clone())
        }
    }

    fn type_check_array_lit(&mut self, elems: &[Expr]) -> Result<Type, TypeCheckError> {
        if elems.is_empty() {
            return Err(TypeCheckError::EmptyArrayLiteral(elems[0].span));
        }

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

    fn type_check_index(
        &mut self,
        target: &Expr,
        indices: &[Expr],
    ) -> Result<Type, TypeCheckError> {
        // type check target
        let target_ty = self.type_check_expr(target)?;
        let (elem_ty, dims) = match target_ty {
            Type::Array { elem_ty, dims } => (elem_ty, dims),
            _ => {
                return Err(TypeCheckError::InvalidIndexTargetType(
                    target_ty,
                    target.span,
                ));
            }
        };

        // Check we don't have more indices than dimensions
        if indices.len() > dims.len() {
            return Err(TypeCheckError::TooManyIndices(
                dims.len(),
                indices.len(),
                target.span,
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
            Ok(*elem_ty)
        } else {
            // Partially indexed, return array with the remaining dimensions
            Ok(Type::Array {
                elem_ty,
                dims: dims[indices.len()..].to_vec(),
            })
        }
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
        expr: &Expr,
        name: &String,
        fields: &[StructLitField],
    ) -> Result<Type, TypeCheckError> {
        // Lookup the struct type
        let struct_ty = self
            .type_decls
            .get(name)
            .ok_or_else(|| TypeCheckError::UnknownStructType(name.clone(), expr.span))?;

        let mut seen_fields = HashSet::new();

        // Type check each field
        for field in fields {
            // Check that the field is defined in the struct
            if !struct_ty.has_field(&field.name) {
                return Err(TypeCheckError::UnknownStructField(
                    field.name.clone(),
                    expr.span,
                ));
            }

            // Check for duplicate fields
            if !seen_fields.insert(&field.name) {
                return Err(TypeCheckError::DuplicateStructField(
                    field.name.clone(),
                    expr.span,
                ));
            }

            // Type check the field
            let expected_ty = struct_ty.struct_field_type(&field.name);
            let actual_ty = self.type_check_expr(&field.value)?;

            if actual_ty != expected_ty {
                return Err(TypeCheckError::StructFieldTypeMismatch(
                    field.name.clone(),
                    expected_ty,
                    actual_ty,
                    field.span,
                ));
            }
        }

        // Check for missing fields
        let mut missing_fields = Vec::new();
        match struct_ty {
            Type::Struct { fields, .. } => {
                for field in fields {
                    if !seen_fields.contains(&field.name) {
                        missing_fields.push(field.name.clone());
                    }
                }
            }
            _ => panic!("Expected struct type"),
        }
        if !missing_fields.is_empty() {
            return Err(TypeCheckError::StructFieldsMissing(
                missing_fields.join(", "),
                expr.span,
            ));
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
                None => Err(TypeCheckError::UnknownStructField(
                    field.to_string(),
                    target.span,
                )),
            },
            _ => Err(TypeCheckError::InvalidStructFieldTarget(
                target_ty,
                target.span,
            )),
        }
    }

    fn type_check_block(&mut self, body: &Vec<Expr>) -> Result<Type, TypeCheckError> {
        let mut last_type = Type::Unit;
        for expr in body {
            last_type = self.type_check_expr(expr)?;
        }
        Ok(last_type)
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
        }
    }

    fn type_check_let(
        &mut self,
        pattern: &Pattern,
        decl_ty: &Option<TypeExpr>,
        value: &Expr,
    ) -> Result<Type, TypeCheckError> {
        // type check value
        let expr_type = self.type_check_expr(value)?;

        // check declaration type (if present)
        if let Some(decl_ty) = decl_ty {
            let decl_ty = resolve_type_expr(&self.context.def_map, decl_ty)?;
            if expr_type != decl_ty {
                return Err(TypeCheckError::DeclTypeMismatch(
                    decl_ty, expr_type, value.span,
                ));
            }
        }

        // type check pattern
        self.type_check_pattern(pattern, &expr_type)?;

        Ok(Type::Unit)
    }

    fn type_check_var(
        &mut self,
        pattern: &Pattern,
        decl_ty: &Option<TypeExpr>,
        value: &Expr,
    ) -> Result<Type, TypeCheckError> {
        // type check value
        let expr_type = self.type_check_expr(value)?;

        // check declaration type (if present)
        if let Some(decl_ty) = decl_ty {
            let decl_ty = resolve_type_expr(&self.context.def_map, decl_ty)?;
            if expr_type != decl_ty {
                return Err(TypeCheckError::DeclTypeMismatch(
                    decl_ty, expr_type, value.span,
                ));
            }
        }

        // type check pattern
        self.type_check_pattern(pattern, &expr_type)?;

        Ok(Type::Unit)
    }

    fn type_check_assign(&mut self, assignee: &Expr, value: &Expr) -> Result<Type, TypeCheckError> {
        let lhs_type = self.type_check_expr(assignee)?;
        let rhs_type = self.type_check_expr(value)?;

        if lhs_type != rhs_type {
            return Err(TypeCheckError::AssignTypeMismatch(
                lhs_type,
                rhs_type,
                assignee.span,
            ));
        }

        Ok(Type::Unit)
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
                return Err(TypeCheckError::InvalidCallee(
                    callee.kind.clone(),
                    callee.span,
                ));
            }
        };

        // Compute argument types first to avoid holding an immutable borrow of self.funcs
        let mut arg_types = Vec::new();
        for arg in args {
            let ty = self.type_check_expr(arg)?;
            arg_types.push(ty);
        }
        // Get function signature
        let Some(func_sig) = self.func_sigs.get(name) else {
            return Err(TypeCheckError::UnknownType(callee.span));
        };
        // Check number of arguments
        if arg_types.len() != func_sig.params.len() {
            return Err(TypeCheckError::ArgCountMismatch(
                name.to_string(),
                func_sig.params.len(),
                arg_types.len(),
                call_expr.span,
            ));
        }
        // Check argument types
        for (i, arg_type) in arg_types.iter().enumerate() {
            if arg_type != &func_sig.params[i] {
                let span = args[i].span;
                return Err(TypeCheckError::ArgTypeMismatch(
                    i + 1,
                    func_sig.params[i].clone(),
                    arg_type.clone(),
                    span,
                ));
            }
        }
        Ok(func_sig.return_type.clone())
    }

    fn type_check_if(
        &mut self,
        cond: &Expr,
        then_body: &Expr,
        else_body: &Expr,
    ) -> Result<Type, TypeCheckError> {
        let cond_type = self.type_check_expr(cond)?;
        if cond_type != Type::Bool {
            Err(TypeCheckError::CondNotBoolean(cond_type, cond.span))
        } else {
            let then_type = self.type_check_expr(then_body)?;
            let else_type = self.type_check_expr(else_body)?;
            if then_type != else_type {
                // create a span that covers both the then and else bodies so the
                // diagnostic highlights the whole region
                let then_span = match &then_body.kind {
                    ExprKind::Block(body) => body.last().unwrap().span,
                    _ => then_body.span,
                };
                let else_span = match &else_body.kind {
                    ExprKind::Block(body) => body.last().unwrap().span,
                    _ => else_body.span,
                };
                let span = Span::merge_all(vec![then_span, else_span]);
                Err(TypeCheckError::ThenElseTypeMismatch(
                    then_type, else_type, span,
                ))
            } else {
                Ok(then_type)
            }
        }
    }

    fn type_check_while(&mut self, cond: &Expr, body: &Expr) -> Result<Type, TypeCheckError> {
        let cond_type = self.type_check_expr(cond)?;
        if cond_type != Type::Bool {
            Err(TypeCheckError::CondNotBoolean(cond_type, cond.span))
        } else {
            let _ = self.type_check_expr(body)?;
            Ok(Type::Unit)
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
                    Err(TypeCheckError::ArithTypeMismatch(
                        left_type, right_type, span,
                    ))
                } else {
                    Ok(Type::UInt64)
                }
            }
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::LtEq
            | BinaryOp::GtEq => {
                if left_type != right_type {
                    let span = Span::merge_all(vec![left.span, right.span]);
                    Err(TypeCheckError::CmpTypeMismatch(left_type, right_type, span))
                } else {
                    Ok(Type::Bool)
                }
            }
        }
    }

    fn type_check_expr(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
        let result = match &expr.kind {
            ExprKind::UInt64Lit(_) => Ok(Type::UInt64),

            ExprKind::BoolLit(_) => Ok(Type::Bool),

            ExprKind::UnitLit => Ok(Type::Unit),

            ExprKind::ArrayLit(elems) => self.type_check_array_lit(elems),

            ExprKind::ArrayIndex { target, indices } => self.type_check_index(target, indices),

            ExprKind::TupleLit(fields) => self.type_check_tuple_lit(fields),

            ExprKind::TupleField { target, index } => {
                self.type_check_tuple_field_access(target, *index)
            }

            ExprKind::StructLit { name, fields } => self.type_check_struct_lit(expr, name, fields),

            ExprKind::StructField { target, field } => self.type_check_field_access(target, field),

            ExprKind::BinOp { left, op, right } => self.type_check_bin_op(left, op, right),

            ExprKind::UnaryOp { expr, .. } => self.type_check_expr(expr),

            ExprKind::Block(body) => self.type_check_block(body),

            ExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => self.type_check_let(pattern, decl_ty, value),

            ExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => self.type_check_var(pattern, decl_ty, value),

            ExprKind::Assign { value, assignee } => self.type_check_assign(assignee, value),

            ExprKind::Var(_) => self.type_check_var_ref(expr),

            ExprKind::Call { callee, args } => self.type_check_call(expr, callee, args),

            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => self.type_check_if(cond, then_body, else_body),

            ExprKind::While { cond, body } => self.type_check_while(cond, body),
        };

        result.map(|ty| {
            self.builder.record_node_type(expr.id, ty.clone());
            ty.clone()
        })
    }
}
