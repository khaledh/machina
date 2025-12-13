use crate::analysis::{Def, DefKind, DefMap, DefMapBuilder};
use crate::ast;
use crate::ast::{ExprKind, Module, Pattern};
use crate::context::{AstContext, ResolvedContext};
use crate::diagnostics::Span;
use crate::ids::{DefId, DefIdGen};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq)]
enum SymbolKind {
    Var { is_mutable: bool },
    Func,
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

    fn populate_funcs(&mut self, funcs: &Vec<ast::Function>) {
        for func in funcs {
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
            checker.populate_funcs(&module.funcs);
            for function in &module.funcs {
                checker.check_function(function);
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

    fn check_function(&mut self, function: &ast::Function) {
        self.with_scope(|checker| {
            // add parameters to scope
            for (index, param) in function.params.iter().enumerate() {
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
            ExprKind::VarRef(name) => {
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
            ExprKind::Index { target, indices } => {
                // Recursively check the target. If target is mutable, then target[index] is mutable.
                self.check_lvalue_mutability(target);
                for index in indices {
                    self.check_expr(index);
                }
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
        match pattern {
            Pattern::Ident { id, name, span } => {
                if self.lookup_symbol_direct(name).is_some() {
                    self.errors
                        .push(ResolveError::VarAlreadyDefined(name.to_string(), *span));
                } else {
                    let def_id = self.def_id_gen.new_id();
                    let def = Def {
                        id: def_id,
                        name: name.to_string(),
                        kind: DefKind::LocalVar,
                        nrvo_eligible: false,
                    };
                    self.def_map_builder.record_def(def, *id);
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
            Pattern::Array { patterns, .. } => {
                // Recursively check each sub-pattern
                for pattern in patterns {
                    self.check_pattern(pattern, is_mutable);
                }
            }
            Pattern::Tuple { patterns, .. } => {
                // Recursively check each sub-pattern
                for pattern in patterns {
                    self.check_pattern(pattern, is_mutable);
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

            ExprKind::Index { target, indices } => {
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

            ExprKind::TupleFieldAccess { target, .. } => {
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

            ExprKind::Let { pattern, value } => {
                // Check the value first before introducing the lhs symbol(s) into the scope.
                self.check_expr(value);
                self.check_pattern(pattern, false);
            }

            ExprKind::Var { pattern, value } => {
                // Check the value first before introducing the lhs symbol(s) into the scope.
                self.check_expr(value);
                self.check_pattern(pattern, true);
            }

            ExprKind::VarRef(name) => match self.lookup_symbol(name) {
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
                // For now, callee must be a VarRef to a function.
                // In the future, this can be generalized.
                match &callee.kind {
                    ExprKind::VarRef(name) => match self.lookup_symbol(name) {
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
