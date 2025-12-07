use crate::analysis::{TypeMap, TypeMapBuilder};
use crate::ast::{BinaryOp, Expr, ExprKind, Function};
use crate::context::{ResolvedContext, TypeCheckedContext};
use crate::diagnostics::Span;
use crate::ids::NodeId;
use crate::types::Type;
use std::collections::HashMap;
use thiserror::Error;

struct FuncSig {
    params: Vec<Type>,
    return_type: Type,
}

#[derive(Debug, Clone, Error)]
pub enum TypeCheckError {
    #[error("Function return type mismatch: expected {0}, found {1}")]
    FuncReturnTypeMismatch(Type, Type, Span),

    #[error("Invalid types for arithmetic operation: {0} != {1}")]
    ArithTypeMismatch(Type, Type, Span),

    #[error("Invalid types for comparison operation: {0} != {1}")]
    CmpTypeMismatch(Type, Type, Span),

    #[error("Condition must be a boolean, found {0}")]
    CondNotBoolean(Type, Span),

    #[error("Then and else branches have different types: {0} != {1}")]
    ThenElseTypeMismatch(Type, Type, Span),

    #[error("Type mismatch in assignment: lhs type {0} != rhs type {1}")]
    AssignTypeMismatch(Type, Type, Span),

    #[error("Invalid argument count for function {0}: expected {1}, found {2}")]
    ArgCountMismatch(String, usize, usize, Span),

    #[error("Type mismatch in argument {0} for function {1}: expected {2}, found {3}")]
    ArgTypeMismatch(usize, String, Type, Type, Span),

    #[error("Invalid callee. Expected a function name, found: {0:?}")]
    InvalidCallee(ExprKind, Span),

    #[error("Empty array literals are unsupported: {0}")]
    EmptyArrayLiteral(Span),

    #[error("Array element type mismatch: expected {0}, found {1}")]
    ArrayElementTypeMismatch(Type, Type, Span),

    #[error("Index must be an integer, found {0}")]
    IndexTypeNotInt(Type, Span),

    #[error("Index on non-array type: {0}")]
    InvalidIndexTargetType(Type, Span),

    #[error("Type cannot be inferred")]
    UnknownType(Span),
}

impl TypeCheckError {
    pub fn span(&self) -> Span {
        match self {
            TypeCheckError::FuncReturnTypeMismatch(_, _, span) => *span,
            TypeCheckError::ArithTypeMismatch(_, _, span) => *span,
            TypeCheckError::CmpTypeMismatch(_, _, span) => *span,
            TypeCheckError::CondNotBoolean(_, span) => *span,
            TypeCheckError::ThenElseTypeMismatch(_, _, span) => *span,
            TypeCheckError::AssignTypeMismatch(_, _, span) => *span,
            TypeCheckError::ArgCountMismatch(_, _, _, span) => *span,
            TypeCheckError::ArgTypeMismatch(_, _, _, _, span) => *span,
            TypeCheckError::InvalidCallee(_, span) => *span,
            TypeCheckError::EmptyArrayLiteral(span) => *span,
            TypeCheckError::ArrayElementTypeMismatch(_, _, span) => *span,
            TypeCheckError::IndexTypeNotInt(_, span) => *span,
            TypeCheckError::InvalidIndexTargetType(_, span) => *span,
            TypeCheckError::UnknownType(span) => *span,
        }
    }
}

pub struct TypeChecker {
    context: ResolvedContext,
    type_map_builder: TypeMapBuilder,
    func_sigs: HashMap<String, FuncSig>,
    errors: Vec<TypeCheckError>,
}

// Internal helper used during checking to avoid self-borrow conflicts. It holds
// split borrows into the owning TypeChecker fields so we can iterate over
// functions (&context.module.funcs) while still mutably updating the builder
// and error vec. This removes the need for moving the functions Vec out of the
// context (previous mem::take workaround) and keeps borrow scopes minimal.
struct Checker<'c, 'b> {
    context: &'c ResolvedContext,
    func_sigs: &'c HashMap<String, FuncSig>,
    builder: &'b mut TypeMapBuilder,
    errors: &'b mut Vec<TypeCheckError>,
}

impl TypeChecker {
    pub fn new(context: ResolvedContext) -> Self {
        Self {
            context,
            type_map_builder: TypeMapBuilder::new(),
            func_sigs: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn check(&mut self) -> Result<TypeMap, Vec<TypeCheckError>> {
        self.populate_function_symbols();

        // Create split borrows so we can iterate immutably over functions while
        // mutably updating the builder & errors.
        let mut checker = Checker {
            context: &self.context,
            func_sigs: &self.func_sigs,
            builder: &mut self.type_map_builder,
            errors: &mut self.errors,
        };

        for function in &self.context.module.funcs {
            if let Err(_) = checker.type_check_function(function) {
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

    fn populate_function_symbols(&mut self) {
        for function in &self.context.module.funcs {
            self.func_sigs.insert(
                function.name.clone(),
                FuncSig {
                    params: function.params.iter().map(|p| p.typ.clone()).collect(),
                    return_type: function.return_type.clone(),
                },
            );
        }
    }
}

impl<'c, 'b> Checker<'c, 'b> {
    fn lookup_def_type(&self, node: NodeId) -> Option<Type> {
        self.context
            .def_map
            .lookup_def(node)
            .and_then(|def| self.builder.lookup_def_type(def))
    }

    fn type_check_function(&mut self, function: &Function) -> Result<Type, Vec<TypeCheckError>> {
        // record param types
        for param in &function.params {
            if let Some(def) = self.context.def_map.lookup_def(param.id) {
                self.builder.record_def_type(def.clone(), param.typ.clone());
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
        if ret_ty != Type::Unknown && ret_ty != function.return_type {
            // get the span of the last expression in the body
            let span = match &function.body.kind {
                ExprKind::Block(body) => body.last().unwrap().span,
                _ => function.body.span,
            };
            self.errors.push(TypeCheckError::FuncReturnTypeMismatch(
                function.return_type.clone(),
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

    fn type_check_array_lit(&mut self, elems: &Vec<Expr>) -> Result<Type, TypeCheckError> {
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

        Ok(Type::Array {
            elem_ty: Box::new(elem_ty),
            len: elems.len(),
        })
    }

    fn type_check_index(&mut self, target: &Expr, index: &Expr) -> Result<Type, TypeCheckError> {
        // type check target
        let target_ty = self.type_check_expr(target)?;
        let elem_ty = match target_ty {
            Type::Array { elem_ty, .. } => elem_ty,
            _ => {
                return Err(TypeCheckError::InvalidIndexTargetType(
                    target_ty,
                    target.span,
                ));
            }
        };

        // type check index
        let index_type = self.type_check_expr(index)?;
        if index_type != Type::UInt64 {
            return Err(TypeCheckError::IndexTypeNotInt(index_type, index.span));
        }

        Ok(*elem_ty)
    }

    fn type_check_block(&mut self, body: &Vec<Expr>) -> Result<Type, TypeCheckError> {
        let mut last_type = Type::Unit;
        for expr in body {
            last_type = self.type_check_expr(expr)?;
        }
        Ok(last_type)
    }

    fn type_check_let(&mut self, let_expr: &Expr, value: &Expr) -> Result<Type, TypeCheckError> {
        match self.context.def_map.lookup_def(let_expr.id) {
            Some(def) => {
                let expr_type = self.type_check_expr(value)?;
                self.builder.record_def_type(def.clone(), expr_type);
                Ok(Type::Unit)
            }
            None => Err(TypeCheckError::UnknownType(let_expr.span)),
        }
    }

    fn type_check_var(&mut self, var_expr: &Expr, value: &Expr) -> Result<Type, TypeCheckError> {
        match self.context.def_map.lookup_def(var_expr.id) {
            Some(def) => {
                let expr_type = self.type_check_expr(value)?;
                self.builder.record_def_type(def.clone(), expr_type);
                Ok(Type::Unit)
            }
            None => Err(TypeCheckError::UnknownType(var_expr.span)),
        }
    }

    fn type_check_assign(&mut self, assignee: &Expr, value: &Expr) -> Result<Type, TypeCheckError> {
        let lhs_type = self.type_check_expr(assignee)?;
        let rhs_type = self.type_check_expr(value)?;

        // lhs_type or rhs_type are never Unknown now because type_check_expr returns Err instead
        // or propagates Err.

        if lhs_type != rhs_type {
            Err(TypeCheckError::AssignTypeMismatch(
                lhs_type,
                rhs_type,
                assignee.span,
            ))
        } else {
            Ok(Type::Unit)
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
        args: &Vec<Expr>,
    ) -> Result<Type, TypeCheckError> {
        let name = match &callee.kind {
            ExprKind::VarRef(name) => name,
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
                    name.to_string(),
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

            ExprKind::Index { target, index } => self.type_check_index(target, index),

            ExprKind::BinOp { left, op, right } => self.type_check_bin_op(left, op, right),

            ExprKind::UnaryOp { expr, .. } => self.type_check_expr(expr),

            ExprKind::Block(body) => self.type_check_block(body),

            ExprKind::Let { value, .. } => self.type_check_let(expr, value),

            ExprKind::Var { value, .. } => self.type_check_var(expr, value),

            ExprKind::Assign { value, assignee } => self.type_check_assign(assignee, value),

            ExprKind::VarRef(_) => self.type_check_var_ref(expr),

            ExprKind::Call { callee, args } => self.type_check_call(expr, callee, args),

            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => self.type_check_if(&cond, &then_body, &else_body),

            ExprKind::While { cond, body } => self.type_check_while(&cond, &body),
        };

        result.map(|ty| {
            self.builder.record_node_type(expr.id, ty.clone());
            ty.clone()
        })
    }
}

pub fn type_check(context: ResolvedContext) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    let mut type_checker = TypeChecker::new(context.clone());
    let type_map = type_checker.check()?;
    let type_checked_context = context.with_type_map(type_map);
    Ok(type_checked_context)
}
