use crate::analysis::{DefMap, DefMapBuilder};
use crate::ast;
use crate::ast::{ExprKind, Module};
use crate::context::{Context, ResolvedContext};
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
}

impl ResolveError {
    pub fn span(&self) -> Span {
        match self {
            ResolveError::VarAlreadyDefined(_, span) => *span,
            ResolveError::VarUndefined(_, span) => *span,
            ResolveError::VarImmutable(_, span) => *span,
            ResolveError::FuncUndefined(_, span) => *span,
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
            scopes: Vec::new(),
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

    fn insert_symbol(&mut self, name: &str, symbol: Symbol) {
        self.scopes
            .last_mut()
            .unwrap()
            .defs
            .insert(name.to_string(), symbol);
    }

    fn populate_funcs(&mut self, functions: &Vec<ast::Function>) {
        for function in functions {
            let def_id = self.def_id_gen.new_id();
            self.def_map_builder.record_def(def_id, function.id);
            self.insert_symbol(
                &function.name,
                Symbol {
                    def_id,
                    name: function.name.clone(),
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
                checker.check_function(&function);
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
            for param in &function.params {
                let def_id = checker.def_id_gen.new_id();
                checker.def_map_builder.record_def(def_id, param.id);
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

    fn check_expr(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr {
                kind: ExprKind::UInt32Lit(_),
                ..
            } => {}
            ast::Expr {
                kind: ExprKind::BoolLit(_),
                ..
            } => {}
            ast::Expr {
                kind: ExprKind::UnitLit,
                ..
            } => {}
            ast::Expr {
                kind: ExprKind::BinOp { left, right, .. },
                ..
            } => {
                self.check_expr(left);
                self.check_expr(right);
            }

            ast::Expr {
                kind: ExprKind::UnaryOp { expr, .. },
                ..
            } => {
                self.check_expr(expr);
            }

            ast::Expr {
                kind: ExprKind::Block(body),
                ..
            } => {
                self.with_scope(|checker| {
                    for expr in body {
                        checker.check_expr(expr);
                    }
                });
            }

            ast::Expr {
                kind: ExprKind::Let { name, value },
                ..
            } => {
                if self.lookup_symbol_direct(name).is_some() {
                    self.errors
                        .push(ResolveError::VarAlreadyDefined(name.to_string(), expr.span));
                } else {
                    self.check_expr(value);
                    let def_id = self.def_id_gen.new_id();
                    self.def_map_builder.record_def(def_id, expr.id);
                    self.insert_symbol(
                        name,
                        Symbol {
                            def_id,
                            name: name.to_string(),
                            kind: SymbolKind::Var { is_mutable: false },
                        },
                    );
                }
            }

            ast::Expr {
                kind: ExprKind::Var { name, value },
                ..
            } => {
                if self.lookup_symbol_direct(name).is_some() {
                    self.errors
                        .push(ResolveError::VarAlreadyDefined(name.to_string(), expr.span));
                } else {
                    self.check_expr(value);
                    let def_id = self.def_id_gen.new_id();
                    self.def_map_builder.record_def(def_id, expr.id);
                    self.insert_symbol(
                        name,
                        Symbol {
                            def_id,
                            name: name.to_string(),
                            kind: SymbolKind::Var { is_mutable: true },
                        },
                    );
                }
            }

            ast::Expr {
                kind: ExprKind::VarRef(name),
                ..
            } => match self.lookup_symbol(name) {
                Some(symbol) => self.def_map_builder.record_use(expr.id, symbol.def_id),
                None => self
                    .errors
                    .push(ResolveError::VarUndefined(name.to_string(), expr.span)),
            },

            ast::Expr {
                kind:
                    ExprKind::If {
                        cond,
                        then_body,
                        else_body,
                    },
                ..
            } => {
                self.check_expr(cond);
                self.check_expr(then_body);
                self.check_expr(else_body);
            }

            ast::Expr {
                kind: ExprKind::Assign { name, value },
                ..
            } => match self.lookup_symbol(name) {
                Some(symbol) => match symbol.kind {
                    SymbolKind::Var { is_mutable: true } => {
                        self.def_map_builder.record_use(expr.id, symbol.def_id);
                        self.check_expr(value)
                    }
                    SymbolKind::Var { is_mutable: false } => {
                        self.def_map_builder.record_use(expr.id, symbol.def_id);
                        self.errors
                            .push(ResolveError::VarImmutable(name.to_string(), expr.span));
                    }
                    _ => {
                        self.errors
                            .push(ResolveError::VarUndefined(name.to_string(), expr.span));
                    }
                },
                None => {
                    self.errors
                        .push(ResolveError::VarUndefined(name.to_string(), expr.span));
                }
            },

            ast::Expr {
                kind: ExprKind::While { cond, body },
                ..
            } => {
                self.check_expr(cond);
                self.check_expr(body);
            }

            ast::Expr {
                kind: ExprKind::Call { name, args },
                ..
            } => match self.lookup_symbol(name) {
                Some(symbol) if symbol.kind == SymbolKind::Func => {
                    self.def_map_builder.record_use(expr.id, symbol.def_id);
                    for arg in args {
                        self.check_expr(arg);
                    }
                }
                _ => self
                    .errors
                    .push(ResolveError::FuncUndefined(name.to_string(), expr.span)),
            },
        }
    }
}

pub fn resolve(context: Context) -> Result<ResolvedContext, Vec<ResolveError>> {
    let mut resolver = SymbolResolver::new();
    let def_map = resolver.resolve(&context.module)?;
    let resolved_context = context.with_def_map(def_map);
    Ok(resolved_context)
}
