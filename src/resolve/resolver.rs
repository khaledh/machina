use std::collections::{HashMap, HashSet};

use crate::ast;
use crate::ast::NodeId;
use crate::ast::{
    BlockItem, Decl, ExprKind, Function, FunctionDecl, MatchPattern, Module, PatternKind,
    StmtExprKind, TypeDecl, TypeDeclKind, TypeExpr, TypeExprKind,
};
use crate::context::{AstContext, ResolvedContext};
use crate::diag::Span;
use crate::resolve::def_map::{Def, DefId, DefIdGen, DefKind, DefMap, DefMapBuilder};
use crate::resolve::errors::ResolveError;
use crate::resolve::symbols::SymbolKind;
use crate::resolve::symbols::{Scope, Symbol};
use crate::types::BUILTIN_TYPES;

pub struct SymbolResolver {
    scopes: Vec<Scope>,
    errors: Vec<ResolveError>,
    def_id_gen: DefIdGen,
    def_map_builder: DefMapBuilder,
    func_decl_names: HashSet<String>,
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
            func_decl_names: HashSet::new(),
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

    fn insert_symbol(&mut self, name: &str, symbol: Symbol, span: Span) {
        let scope = self.scopes.last_mut().unwrap();
        match scope.defs.get_mut(name) {
            None => {
                scope.defs.insert(name.to_string(), symbol);
            }
            Some(existing) => match (&mut existing.kind, symbol.kind) {
                (
                    SymbolKind::Func { overloads },
                    SymbolKind::Func {
                        overloads: new_overloads,
                    },
                ) => {
                    overloads.extend(new_overloads);
                }
                _ => {
                    self.errors
                        .push(ResolveError::SymbolAlreadyDefined(name.to_string(), span));
                }
            },
        }
    }

    fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.defs.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    fn map_symbol_kind_to_def_kind(kind: &SymbolKind) -> DefKind {
        match kind {
            SymbolKind::TypeAlias { ty_expr, .. } => DefKind::TypeAlias {
                ty_expr: ty_expr.clone(),
            },
            SymbolKind::StructDef { fields, .. } => DefKind::StructDef {
                fields: fields.clone(),
            },
            SymbolKind::Func { .. } => DefKind::Func,
            SymbolKind::Var { .. } => DefKind::LocalVar {
                nrvo_eligible: false,
            },
            SymbolKind::EnumDef { variants, .. } => DefKind::EnumDef {
                variants: variants.clone(),
            },
        }
    }

    fn add_built_in_symbol<F>(&mut self, name: &str, kind_fn: F)
    where
        F: FnOnce(DefId) -> SymbolKind,
    {
        let def_id = self.def_id_gen.new_id();
        let kind = kind_fn(def_id);
        let def = Def {
            id: def_id,
            name: name.to_string(),
            kind: Self::map_symbol_kind_to_def_kind(&kind),
        };
        self.def_map_builder.record_def(def, NodeId(0));
        self.insert_symbol(
            name,
            Symbol {
                name: name.to_string(),
                kind,
            },
            Span::default(),
        );
    }

    fn populate_decls(&mut self, module: &Module) {
        self.populate_type_decls(&module.type_decls());
        self.populate_func_decls(&module.func_decls());
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
                        def_id,
                        ty_expr: aliased_ty.clone(),
                    },
                ),
                TypeDeclKind::Struct { fields } => (
                    DefKind::StructDef {
                        fields: fields.clone(),
                    },
                    SymbolKind::StructDef {
                        def_id,
                        fields: fields.clone(),
                    },
                ),
                TypeDeclKind::Enum { variants } => (
                    DefKind::EnumDef {
                        variants: variants.clone(),
                    },
                    SymbolKind::EnumDef {
                        def_id,
                        variants: variants.clone(),
                    },
                ),
            };

            // Create a new Def
            let def = Def {
                id: def_id,
                name: type_decl.name.clone(),
                kind: def_kind,
            };

            // Record the def
            self.def_map_builder.record_def(def, type_decl.id);

            // Insert the symbol
            self.insert_symbol(
                &type_decl.name,
                Symbol {
                    name: type_decl.name.clone(),
                    kind: symbol_kind,
                },
                type_decl.span,
            );
        }
    }

    fn populate_func_decls(&mut self, func_decls: &[&FunctionDecl]) {
        for &func_decl in func_decls {
            // Check if the function decl name is already defined
            let name = func_decl.sig.name.clone();
            if self.lookup_symbol(&name).is_some() || self.func_decl_names.contains(&name) {
                self.errors
                    .push(ResolveError::SymbolAlreadyDefined(name, func_decl.span));
                continue;
            }
            self.func_decl_names.insert(name);

            // Create and record the def
            let def_id = self.def_id_gen.new_id();
            let def = Def {
                id: def_id,
                name: func_decl.sig.name.clone(),
                kind: DefKind::ExternFunc,
            };
            self.def_map_builder.record_def(def, func_decl.id);
            self.insert_symbol(
                &func_decl.sig.name,
                Symbol {
                    name: func_decl.sig.name.clone(),
                    kind: SymbolKind::Func {
                        overloads: vec![def_id],
                    },
                },
                func_decl.span,
            );
        }
    }

    fn populate_funcs(&mut self, funcs: &[&Function]) {
        for &func in funcs {
            // Check if the function name is already defined as a function decl
            if self.func_decl_names.contains(&func.sig.name) {
                self.errors.push(ResolveError::SymbolAlreadyDefined(
                    func.sig.name.clone(),
                    func.span,
                ));
                continue;
            }

            // Create and record the def
            let def_id = self.def_id_gen.new_id();
            let def = Def {
                id: def_id,
                name: func.sig.name.clone(),
                kind: DefKind::Func,
            };
            self.def_map_builder.record_def(def, func.id);
            self.insert_symbol(
                &func.sig.name,
                Symbol {
                    name: func.sig.name.clone(),
                    kind: SymbolKind::Func {
                        overloads: vec![def_id],
                    },
                },
                func.span,
            );
        }
    }

    pub fn resolve(&mut self, module: &Module) -> Result<DefMap, Vec<ResolveError>> {
        self.with_scope(|resolver| {
            // global scope

            // add built-in types
            for ty in BUILTIN_TYPES {
                let ty_name = ty.to_string();
                resolver.add_built_in_symbol(&ty_name, |def_id| SymbolKind::TypeAlias {
                    def_id,
                    ty_expr: TypeExpr {
                        id: NodeId(0),
                        kind: TypeExprKind::Named(ty_name.clone()),
                        span: Span::default(),
                    },
                });
            }

            resolver.populate_decls(module);

            for decl in &module.decls {
                match decl {
                    Decl::TypeDecl(type_decl) => resolver.check_type_decl(type_decl),
                    Decl::FunctionDecl(func_decl) => resolver.check_function_decl(func_decl),
                    Decl::Function(function) => resolver.check_function(function),
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
            TypeDeclKind::Enum { variants } => {
                // resolve each variant payload type expr
                for variant in variants {
                    for payload_ty in &variant.payload {
                        self.check_type_expr(payload_ty);
                    }
                }
            }
        }
    }

    fn check_function_decl(&mut self, func_decl: &ast::FunctionDecl) {
        // resolve return type
        self.check_type_expr(&func_decl.sig.return_type);

        // resolve param types
        for param in &func_decl.sig.params {
            self.check_type_expr(&param.typ);
        }
    }

    fn check_function(&mut self, func: &ast::Function) {
        // resolve return type
        self.check_type_expr(&func.sig.return_type);

        // Enter a new scope for the function body
        self.with_scope(|checker| {
            // add parameters to scope
            for (index, param) in func.sig.params.iter().enumerate() {
                // resolve param type
                checker.check_type_expr(&param.typ);

                // record the param def
                let def_id = checker.def_id_gen.new_id();
                let def = Def {
                    id: def_id,
                    name: param.name.clone(),
                    kind: DefKind::Param {
                        index: index as u32,
                    },
                };
                checker.def_map_builder.record_def(def, param.id);
                checker.insert_symbol(
                    &param.name,
                    Symbol {
                        name: param.name.clone(),
                        kind: SymbolKind::Var {
                            def_id,
                            is_mutable: false,
                        },
                    },
                    param.span,
                );
            }

            // check function body
            checker.check_expr(&func.body);
        });
    }

    fn check_lvalue_mutability(&mut self, expr: &ast::Expr) {
        match &expr.kind {
            ExprKind::Var(name) => {
                match self.lookup_symbol(name) {
                    Some(symbol) => match &symbol.kind {
                        SymbolKind::Var {
                            is_mutable: true, ..
                        } => {
                            // Mutable: ok
                            self.def_map_builder.record_use(expr.id, symbol.def_id());
                        }
                        SymbolKind::Var {
                            is_mutable: false, ..
                        } => {
                            // Immutable: error
                            self.def_map_builder.record_use(expr.id, symbol.def_id());
                            self.errors
                                .push(ResolveError::VarImmutable(name.clone(), expr.span));
                        }
                        _ => {
                            self.errors
                                .push(ResolveError::VarUndefined(name.clone(), expr.span));
                        }
                    },
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
                let def_id = self.def_id_gen.new_id();
                let def = Def {
                    id: def_id,
                    name: name.to_string(),
                    kind: DefKind::LocalVar {
                        nrvo_eligible: false,
                    },
                };
                self.def_map_builder.record_def(def, pattern.id);
                self.insert_symbol(
                    name,
                    Symbol {
                        name: name.to_string(),
                        kind: SymbolKind::Var { def_id, is_mutable },
                    },
                    pattern.span,
                );
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
            PatternKind::Struct { name, fields } => {
                // Resolve struct type name
                match self.lookup_symbol(name) {
                    Some(Symbol {
                        kind: SymbolKind::StructDef { def_id, .. },
                        ..
                    }) => {
                        self.def_map_builder.record_use(pattern.id, *def_id);
                    }
                    Some(symbol) => {
                        self.errors.push(ResolveError::ExpectedType(
                            name.clone(),
                            symbol.kind.clone(),
                            pattern.span,
                        ));
                    }
                    None => self
                        .errors
                        .push(ResolveError::StructUndefined(name.clone(), pattern.span)),
                }

                // Bind each field's sub-pattern
                for field in fields {
                    self.check_pattern(&field.pattern, is_mutable);
                }
            }
        }
    }

    fn check_match_pattern(&mut self, pattern: &MatchPattern, arm_id: NodeId) {
        match pattern {
            MatchPattern::Wildcard { .. } => {}
            MatchPattern::EnumVariant {
                enum_name,
                bindings,
                span,
                ..
            } => {
                // Resolve the enum name if present
                if let Some(enum_name) = enum_name {
                    let Some(Symbol {
                        kind: SymbolKind::EnumDef { def_id, .. },
                        ..
                    }) = self.lookup_symbol(enum_name)
                    else {
                        self.errors
                            .push(ResolveError::EnumUndefined(enum_name.clone(), *span));
                        return;
                    };
                    self.def_map_builder.record_use(arm_id, *def_id);
                }

                // Note: We delegate to the type checker to validate the variant.

                // Bind each binding's sub-pattern
                for binding in bindings {
                    // Create a new def
                    let def_id = self.def_id_gen.new_id();
                    let def = Def {
                        id: def_id,
                        name: binding.name.clone(),
                        kind: DefKind::LocalVar {
                            nrvo_eligible: false,
                        },
                    };
                    self.def_map_builder.record_def(def, binding.id);
                    self.insert_symbol(
                        &binding.name,
                        Symbol {
                            name: binding.name.clone(),
                            kind: SymbolKind::Var {
                                def_id,
                                is_mutable: false,
                            },
                        },
                        binding.span,
                    );
                }
            }
        }
    }

    fn check_type_expr(&mut self, type_expr: &TypeExpr) {
        match &type_expr.kind {
            TypeExprKind::Named(name) => match self.lookup_symbol(name) {
                Some(symbol) => match &symbol.kind {
                    SymbolKind::TypeAlias { .. }
                    | SymbolKind::StructDef { .. }
                    | SymbolKind::EnumDef { .. } => {
                        self.def_map_builder
                            .record_use(type_expr.id, symbol.def_id());
                    }
                    other => self.errors.push(ResolveError::ExpectedType(
                        name.clone(),
                        other.clone(),
                        type_expr.span,
                    )),
                },
                None => self
                    .errors
                    .push(ResolveError::TypeUndefined(name.clone(), type_expr.span)),
            },
            TypeExprKind::Array { elem_ty, .. } => {
                self.check_type_expr(elem_ty);
            }
            TypeExprKind::Tuple { fields } => {
                for field in fields {
                    self.check_type_expr(field);
                }
            }
            TypeExprKind::Range { .. } => { /* nothing to resolve */ }
            TypeExprKind::Slice { elem_ty } => {
                self.check_type_expr(elem_ty);
            }
        }
    }

    fn check_stmt_expr(&mut self, stmt: &ast::StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => {
                // Check the value first before introducing the lhs symbol(s) into the scope.
                self.check_expr(value);
                if let Some(decl_ty) = decl_ty {
                    self.check_type_expr(decl_ty);
                }
                self.check_pattern(pattern, false);
            }

            StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                // Check the value first before introducing the lhs symbol(s) into the scope.
                self.check_expr(value);
                if let Some(decl_ty) = decl_ty {
                    self.check_type_expr(decl_ty);
                }
                self.check_pattern(pattern, true);
            }

            StmtExprKind::Assign { assignee, value } => {
                self.check_lvalue_mutability(assignee);
                self.check_expr(value);
            }

            StmtExprKind::While { cond, body } => {
                self.check_expr(cond);
                self.check_expr(body);
            }

            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                // Resolve iter first (pattern not in scope for it)
                self.check_expr(iter);
                // Enter a new scope for the pattern + body
                self.with_scope(|checker| {
                    checker.check_pattern(pattern, false);
                    checker.check_expr(body);
                });
            }
        }
    }

    fn check_expr(&mut self, expr: &ast::Expr) {
        match &expr.kind {
            ExprKind::Block { items, tail } => {
                self.with_scope(|checker| {
                    for item in items {
                        match item {
                            BlockItem::Stmt(stmt) => checker.check_stmt_expr(stmt),
                            BlockItem::Expr(expr) => checker.check_expr(expr),
                        }
                    }
                    if let Some(tail) = tail {
                        checker.check_expr(tail);
                    }
                });
            }

            // Scalar literals
            ExprKind::IntLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit { .. }
            | ExprKind::UnitLit => {}

            // Compound literals
            ExprKind::ArrayLit { elem_ty, elems } => {
                if let Some(elem_ty) = elem_ty {
                    self.check_type_expr(elem_ty);
                }
                for elem in elems {
                    self.check_expr(elem);
                }
            }

            ExprKind::TupleLit(fields) => {
                for field in fields {
                    self.check_expr(field);
                }
            }

            ExprKind::StructLit { name, fields } => {
                // Resolve the struct name
                match self.lookup_symbol(name) {
                    Some(Symbol {
                        kind: SymbolKind::StructDef { def_id, .. },
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

            // Accessors
            ExprKind::Var(name) => match self.lookup_symbol(name) {
                Some(symbol) => self.def_map_builder.record_use(expr.id, symbol.def_id()),
                None => self
                    .errors
                    .push(ResolveError::VarUndefined(name.to_string(), expr.span)),
            },

            ExprKind::ArrayIndex { target, indices } => {
                self.check_expr(target);
                for index in indices {
                    self.check_expr(index);
                }
            }

            ExprKind::Slice { target, start, end } => {
                self.check_expr(target);
                if let Some(start) = start {
                    self.check_expr(start);
                }
                if let Some(end) = end {
                    self.check_expr(end);
                }
            }

            ExprKind::TupleField { target, .. } => {
                self.check_expr(target);
            }

            ExprKind::StructField { target, .. } => {
                self.check_expr(target);
            }

            ExprKind::StructUpdate { target, fields } => {
                // Resolve the target
                self.check_expr(target);
                // Resolve each field value
                for field in fields {
                    self.check_expr(&field.value);
                }
            }

            // Range
            ExprKind::Range { .. } => { /* nothing to resolve */ }

            // Enum variants
            ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
            } => {
                // Resolve the enum name
                let Some(Symbol {
                    kind: SymbolKind::EnumDef { def_id, variants },
                    ..
                }) = self.lookup_symbol(enum_name)
                else {
                    self.errors
                        .push(ResolveError::EnumUndefined(enum_name.clone(), expr.span));
                    return;
                };

                // Ensure the variant is valid
                if !variants.iter().any(|v| v.name == *variant) {
                    self.errors.push(ResolveError::EnumVariantUndefined(
                        enum_name.clone(),
                        variant.clone(),
                        expr.span,
                    ));
                    return;
                }

                // Record the use
                self.def_map_builder.record_use(expr.id, *def_id);

                // Resolve each payload expression
                for payload_expr in payload {
                    self.check_expr(payload_expr);
                }
            }

            // Operators
            ExprKind::BinOp { left, right, .. } => {
                self.check_expr(left);
                self.check_expr(right);
            }

            ExprKind::UnaryOp { expr, .. } => {
                self.check_expr(expr);
            }

            // Control flow (If)
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.check_expr(cond);
                self.check_expr(then_body);
                self.check_expr(else_body);
            }

            // Control flow (Match)
            ExprKind::Match { scrutinee, arms } => {
                self.check_expr(scrutinee);
                for arm in arms {
                    // enter a new scope
                    self.with_scope(|checker| {
                        checker.check_match_pattern(&arm.pattern, arm.id);
                        checker.check_expr(&arm.body);
                    });
                }
            }

            // Function calls
            ExprKind::Call { callee, args } => {
                // For now, callee must be a Var to a function.
                // In the future, this can be generalized.
                match &callee.kind {
                    ExprKind::Var(name) => match self.lookup_symbol(name) {
                        Some(symbol) if matches!(&symbol.kind, SymbolKind::Func { .. }) => {
                            self.def_map_builder.record_use(callee.id, symbol.def_id());
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
