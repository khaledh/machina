use std::collections::HashMap;
use std::fmt;
use thiserror::Error;

use crate::analysis::{Def, DefKind, DefMap, DefMapBuilder};
use crate::ast;
use crate::ast::{
    Decl, ExprKind, Function, Module, PatternKind, StructField, TypeDecl, TypeDeclKind, TypeExpr,
    TypeExprKind,
};
use crate::context::{AstContext, ResolvedContext};
use crate::diagnostics::Span;
use crate::ids::{DefId, DefIdGen, NodeId};
use crate::types::BUILTIN_TYPES;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    Var { is_mutable: bool },
    Func,
    TypeAlias { ty_expr: TypeExpr },
    StructDef { fields: Vec<StructField> },
}

impl fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolKind::Var { .. } => write!(f, "var"),
            SymbolKind::Func => write!(f, "func"),
            SymbolKind::TypeAlias { ty_expr } => write!(f, "type_alias[{}]", ty_expr),
            SymbolKind::StructDef { fields } => {
                let field_names = fields
                    .iter()
                    .map(|field| field.name.as_str())
                    .collect::<Vec<_>>();
                write!(f, "struct_def[{}]", field_names.join(", "))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Symbol {
    def_id: DefId,
    name: String,
    kind: SymbolKind,
}

#[derive(Clone, Debug)]
pub struct Scope {
    defs: HashMap<String, Symbol>,
}

#[derive(Clone, Debug, Error)]
pub enum ResolveError {
    #[error("Variable already defined in current scope: {0}")]
    VarAlreadyDefined(String, Span),

    #[error("Undefined variable: {0}")]
    VarUndefined(String, Span),

    #[error("Cannot assign to immutable variable: {0}")]
    VarImmutable(String, Span),

    #[error("Undefined function: {0}")]
    FuncUndefined(String, Span),

    #[error("Invalid assignment target. Expected an l-value, found: {0:?}")]
    InvalidAssignmentTarget(ExprKind, Span),

    #[error("Invalid callee. Expected a function name, found: {0:?}")]
    InvalidCallee(ExprKind, Span),

    #[error("Expected '{0}' to be a type, found {1}")]
    ExpectedType(String, SymbolKind, Span),

    #[error("Undefined type: {0}")]
    TypeUndefined(String, Span),

    #[error("Undefined struct: {0}")]
    StructUndefined(String, Span),
}

impl ResolveError {
    pub fn span(&self) -> Span {
        match self {
            ResolveError::VarAlreadyDefined(_, span) => *span,
            ResolveError::VarUndefined(_, span) => *span,
            ResolveError::VarImmutable(_, span) => *span,
            ResolveError::FuncUndefined(_, span) => *span,
            ResolveError::InvalidAssignmentTarget(_, span) => *span,
            ResolveError::InvalidCallee(_, span) => *span,
            ResolveError::ExpectedType(_, _, span) => *span,
            ResolveError::TypeUndefined(_, span) => *span,
            ResolveError::StructUndefined(_, span) => *span,
        }
    }
}

pub struct SymbolResolver {
    scopes: Vec<Scope>,
    errors: Vec<ResolveError>,
    def_id_gen: DefIdGen,
    def_map_builder: DefMapBuilder,
}

impl SymbolResolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope {
                defs: HashMap::new(),
            }],
            errors: Vec::new(),
            def_id_gen: DefIdGen::new(),
            def_map_builder: DefMapBuilder::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scope {
            defs: HashMap::new(),
        });
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn with_scope<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.enter_scope();
        f(self);
        self.exit_scope();
    }

    fn insert_symbol(&mut self, name: &str, symbol: Symbol) {
        self.scopes
            .last_mut()
            .unwrap()
            .defs
            .insert(name.to_string(), symbol);
    }

    fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.defs.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    fn lookup_symbol_direct(&self, name: &str) -> Option<&Symbol> {
        self.scopes.last().unwrap().defs.get(name)
    }

    fn map_symbol_kind_to_def_kind(kind: SymbolKind) -> DefKind {
        match kind {
            SymbolKind::TypeAlias { ty_expr } => DefKind::TypeAlias { ty_expr },
            SymbolKind::StructDef { fields } => DefKind::StructDef { fields },
            SymbolKind::Func => DefKind::Func,
            SymbolKind::Var { .. } => DefKind::LocalVar,
        }
    }

    fn add_built_in_symbol(&mut self, name: &str, kind: SymbolKind) {
        let def_id = self.def_id_gen.new_id();
        let def = Def {
            id: def_id,
            name: name.to_string(),
            kind: Self::map_symbol_kind_to_def_kind(kind.clone()),
            nrvo_eligible: false,
        };
        self.def_map_builder.record_def(def, NodeId(0));
        self.insert_symbol(
            name,
            Symbol {
                def_id,
                name: name.to_string(),
                kind,
            },
        );
    }

    fn populate_decls(&mut self, module: &Module) {
        self.populate_type_decls(&module.type_decls());
        self.populate_funcs(&module.funcs());
    }

    fn populate_type_decls(&mut self, type_decls: &[&TypeDecl]) {
        for &type_decl in type_decls {
            let def_id = self.def_id_gen.new_id();

            // Map type decl kind to a (def kind, symbol kind) pair
            let (def_kind, symbol_kind) = match &type_decl.kind {
                TypeDeclKind::Alias { aliased_ty } => (
                    DefKind::TypeAlias {
                        ty_expr: aliased_ty.clone(),
                    },
                    SymbolKind::TypeAlias {
                        ty_expr: aliased_ty.clone(),
                    },
                ),
                TypeDeclKind::Struct { fields } => (
                    DefKind::StructDef {
                        fields: fields.clone(),
                    },
                    SymbolKind::StructDef {
                        fields: fields.clone(),
                    },
                ),
            };

            // Create a new Def
            let def = Def {
                id: def_id,
                name: type_decl.name.clone(),
                kind: def_kind,
                nrvo_eligible: false,
            };

            // Record the def
            self.def_map_builder.record_def(def, type_decl.id);

            // Insert the symbol
            self.insert_symbol(
                &type_decl.name,
                Symbol {
                    def_id,
                    name: type_decl.name.clone(),
                    kind: symbol_kind,
                },
            );
        }
    }

    fn populate_funcs(&mut self, funcs: &[&Function]) {
        for &func in funcs {
            let def_id = self.def_id_gen.new_id();
            let def = Def {
                id: def_id,
                name: func.name.clone(),
                kind: DefKind::Func,
                nrvo_eligible: false,
            };
            self.def_map_builder.record_def(def, func.id);
            self.insert_symbol(
                &func.name,
                Symbol {
                    def_id,
                    name: func.name.clone(),
                    kind: SymbolKind::Func,
                },
            );
        }
    }

    pub fn resolve(&mut self, module: &Module) -> Result<DefMap, Vec<ResolveError>> {
        self.with_scope(|checker| {
            // global scope
            // add built-in types
            for ty in BUILTIN_TYPES {
                checker.add_built_in_symbol(
                    &ty.to_string(),
                    SymbolKind::TypeAlias {
                        ty_expr: TypeExpr {
                            id: NodeId(0),
                            kind: TypeExprKind::Named(ty.to_string()),
                            span: Span::default(),
                        },
                    },
                );
            }
            checker.populate_decls(module);
            for decl in &module.decls {
                match decl {
                    Decl::TypeDecl(type_decl) => checker.check_type_decl(type_decl),
                    Decl::Function(function) => checker.check_function(function),
                }
            }
        });

        if self.errors.is_empty() {
            let def_map =
                std::mem::replace(&mut self.def_map_builder, DefMapBuilder::new()).finish();
            Ok(def_map)
        } else {
            Err(self.errors.clone())
        }
    }

    fn check_type_decl(&mut self, type_decl: &TypeDecl) {
        match &type_decl.kind {
            TypeDeclKind::Alias { aliased_ty } => {
                // resolve the aliased type expr
                self.check_type_expr(aliased_ty);
            }
            TypeDeclKind::Struct { fields } => {
                // resolve each struct field type expr
                for field in fields {
                    self.check_type_expr(&field.ty);
                }
            }
        }
    }

    fn check_function(&mut self, function: &ast::Function) {
        // resolve return type
        self.check_type_expr(&function.return_type);

        self.with_scope(|checker| {
            // add parameters to scope
            for (index, param) in function.params.iter().enumerate() {
                // resolve param type
                checker.check_type_expr(&param.typ);

                let def_id = checker.def_id_gen.new_id();
                let def = Def {
                    id: def_id,
                    name: param.name.clone(),
                    kind: DefKind::Param {
                        index: index as u32,
                    },
                    nrvo_eligible: false,
                };
                checker.def_map_builder.record_def(def, param.id);
                checker.insert_symbol(
                    &param.name,
                    Symbol {
                        def_id,
                        name: param.name.clone(),
                        kind: SymbolKind::Var { is_mutable: false },
                    },
                );
            }
            // check function body
            checker.check_expr(&function.body);
        });
    }

    fn check_lvalue_mutability(&mut self, expr: &ast::Expr) {
        match &expr.kind {
            ExprKind::Var(name) => {
                match self.lookup_symbol(name) {
                    Some(symbol) => {
                        match symbol.kind {
                            SymbolKind::Var { is_mutable: true } => {
                                // Mutable: ok
                                self.def_map_builder.record_use(expr.id, symbol.def_id);
                            }
                            SymbolKind::Var { is_mutable: false } => {
                                // Immutable: error
                                self.def_map_builder.record_use(expr.id, symbol.def_id);
                                self.errors
                                    .push(ResolveError::VarImmutable(name.clone(), expr.span));
                            }
                            _ => {
                                self.errors
                                    .push(ResolveError::VarUndefined(name.clone(), expr.span));
                            }
                        }
                    }
                    None => {
                        self.errors
                            .push(ResolveError::VarUndefined(name.clone(), expr.span));
                    }
                }
            }
            ExprKind::ArrayIndex { target, indices } => {
                // Recursively check the target. If target is mutable, then target[index] is mutable.
                self.check_lvalue_mutability(target);
                for index in indices {
                    self.check_expr(index);
                }
            }
            ExprKind::TupleField { target, .. } => {
                self.check_lvalue_mutability(target);
            }
            ExprKind::StructField { target, .. } => {
                self.check_lvalue_mutability(target);
            }
            _ => {
                self.errors.push(ResolveError::InvalidAssignmentTarget(
                    expr.kind.clone(),
                    expr.span,
                ));
            }
        }
    }

    fn check_pattern(&mut self, pattern: &ast::Pattern, is_mutable: bool) {
        match &pattern.kind {
            PatternKind::Ident { name } => {
                if self.lookup_symbol_direct(name).is_some() {
                    self.errors.push(ResolveError::VarAlreadyDefined(
                        name.to_string(),
                        pattern.span,
                    ));
                } else {
                    let def_id = self.def_id_gen.new_id();
                    let def = Def {
                        id: def_id,
                        name: name.to_string(),
                        kind: DefKind::LocalVar,
                        nrvo_eligible: false,
                    };
                    self.def_map_builder.record_def(def, pattern.id);
                    self.insert_symbol(
                        name,
                        Symbol {
                            def_id,
                            name: name.to_string(),
                            kind: SymbolKind::Var { is_mutable },
                        },
                    );
                }
            }
            PatternKind::Array { patterns } => {
                // Recursively check each sub-pattern
                for pattern in patterns {
                    self.check_pattern(pattern, is_mutable);
                }
            }
            PatternKind::Tuple { patterns } => {
                // Recursively check each sub-pattern
                for pattern in patterns {
                    self.check_pattern(pattern, is_mutable);
                }
            }
        }
    }

    fn check_type_expr(&mut self, type_expr: &ast::TypeExpr) {
        match &type_expr.kind {
            ast::TypeExprKind::Named(name) => match self.lookup_symbol(name) {
                Some(Symbol {
                    def_id,
                    kind: SymbolKind::TypeAlias { .. },
                    ..
                }) => {
                    self.def_map_builder.record_use(type_expr.id, *def_id);
                }
                Some(Symbol {
                    def_id,
                    kind: SymbolKind::StructDef { .. },
                    ..
                }) => {
                    self.def_map_builder.record_use(type_expr.id, *def_id);
                }
                Some(Symbol { kind, .. }) => {
                    self.errors.push(ResolveError::ExpectedType(
                        name.clone(),
                        kind.clone(),
                        type_expr.span,
                    ));
                }
                None => self
                    .errors
                    .push(ResolveError::TypeUndefined(name.clone(), type_expr.span)),
            },
            ast::TypeExprKind::Array { elem_ty, .. } => {
                self.check_type_expr(elem_ty);
            }
            ast::TypeExprKind::Tuple { fields } => {
                for field in fields {
                    self.check_type_expr(field);
                }
            }
        }
    }

    fn check_expr(&mut self, expr: &ast::Expr) {
        match &expr.kind {
            ExprKind::UInt64Lit(_) | ast::ExprKind::BoolLit(_) | ast::ExprKind::UnitLit => {}

            ExprKind::ArrayLit(elems) => {
                for elem in elems {
                    self.check_expr(elem);
                }
            }

            ExprKind::ArrayIndex { target, indices } => {
                self.check_expr(target);
                for index in indices {
                    self.check_expr(index);
                }
            }

            ExprKind::TupleLit(fields) => {
                for field in fields {
                    self.check_expr(field);
                }
            }

            ExprKind::TupleField { target, .. } => {
                self.check_expr(target);
            }

            ExprKind::StructLit { name, fields } => {
                // Resolve the struct name
                match self.lookup_symbol(name) {
                    Some(Symbol {
                        def_id,
                        kind: SymbolKind::StructDef { .. },
                        ..
                    }) => {
                        self.def_map_builder.record_use(expr.id, *def_id);
                    }
                    _ => self
                        .errors
                        .push(ResolveError::StructUndefined(name.clone(), expr.span)),
                }

                // Resolve each field value
                for field in fields {
                    self.check_expr(&field.value);
                }
            }

            ExprKind::StructField { target, .. } => {
                self.check_expr(target);
            }

            ExprKind::BinOp { left, right, .. } => {
                self.check_expr(left);
                self.check_expr(right);
            }

            ExprKind::UnaryOp { expr, .. } => {
                self.check_expr(expr);
            }

            ExprKind::Block(body) => {
                self.with_scope(|checker| {
                    for expr in body {
                        checker.check_expr(expr);
                    }
                });
            }

            ExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => {
                // Check the value first before introducing the lhs symbol(s) into the scope.
                self.check_expr(value);
                if let Some(decl_ty) = decl_ty {
                    self.check_type_expr(&decl_ty);
                }
                self.check_pattern(pattern, false);
            }

            ExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                // Check the value first before introducing the lhs symbol(s) into the scope.
                self.check_expr(value);
                if let Some(decl_ty) = decl_ty {
                    self.check_type_expr(&decl_ty);
                }
                self.check_pattern(pattern, true);
            }

            ExprKind::Var(name) => match self.lookup_symbol(name) {
                Some(symbol) => self.def_map_builder.record_use(expr.id, symbol.def_id),
                None => self
                    .errors
                    .push(ResolveError::VarUndefined(name.to_string(), expr.span)),
            },

            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.check_expr(cond);
                self.check_expr(then_body);
                self.check_expr(else_body);
            }

            ExprKind::Assign { assignee, value } => {
                self.check_lvalue_mutability(assignee);
                self.check_expr(value);
            }

            ExprKind::While { cond, body } => {
                self.check_expr(cond);
                self.check_expr(body);
            }

            ExprKind::Call { callee, args } => {
                // For now, callee must be a Var to a function.
                // In the future, this can be generalized.
                match &callee.kind {
                    ExprKind::Var(name) => match self.lookup_symbol(name) {
                        Some(symbol) if symbol.kind == SymbolKind::Func => {
                            self.def_map_builder.record_use(callee.id, symbol.def_id);
                            for arg in args {
                                self.check_expr(arg);
                            }
                        }
                        _ => self
                            .errors
                            .push(ResolveError::FuncUndefined(name.to_string(), callee.span)),
                    },
                    _ => self.errors.push(ResolveError::InvalidCallee(
                        callee.kind.clone(),
                        callee.span,
                    )),
                }
            }
        }
    }
}

pub fn resolve(ast_context: AstContext) -> Result<ResolvedContext, Vec<ResolveError>> {
    let mut resolver = SymbolResolver::new();
    let def_map = resolver.resolve(&ast_context.module)?;
    let resolved_context = ast_context.with_def_map(def_map);
    Ok(resolved_context)
}
