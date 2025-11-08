use crate::ast;
use crate::ast::{ExprKind, Module};
use crate::diagnostics::Span;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq)]
enum SymbolKind {
    Var { is_mutable: bool },
    Func,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Symbol {
    name: String,
    kind: SymbolKind,
}

#[derive(Clone, Debug)]
pub struct Scope {
    symbols: HashMap<String, Symbol>,
}

#[derive(Clone, Debug, Error)]
pub enum SemCheckError {
    #[error("Variable already defined in current scope: {0}")]
    VarAlreadyDefined(String, Span),

    #[error("Undefined variable: {0}")]
    VarUndefined(String, Span),

    #[error("Cannot assign to immutable variable: {0}")]
    VarImmutable(String, Span),

    #[error("Undefined function: {0}")]
    FuncUndefined(String, Span),
}

impl SemCheckError {
    pub fn span(&self) -> Span {
        match self {
            SemCheckError::VarAlreadyDefined(_, span) => *span,
            SemCheckError::VarUndefined(_, span) => *span,
            SemCheckError::VarImmutable(_, span) => *span,
            SemCheckError::FuncUndefined(_, span) => *span,
        }
    }
}

pub struct SemanticChecker {
    scopes: Vec<Scope>,
    errors: Vec<SemCheckError>,
}

impl SemanticChecker {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scope {
            symbols: HashMap::new(),
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
            if let Some(symbol) = scope.symbols.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    fn lookup_symbol_direct(&self, name: &str) -> Option<&Symbol> {
        self.scopes.last().unwrap().symbols.get(name)
    }

    fn insert_symbol(&mut self, name: &str, symbol: Symbol) {
        self.scopes
            .last_mut()
            .unwrap()
            .symbols
            .insert(name.to_string(), symbol);
    }

    fn populate_funcs(&mut self, functions: &Vec<ast::Function>) {
        for function in functions {
            self.insert_symbol(
                &function.name,
                Symbol {
                    name: function.name.clone(),
                    kind: SymbolKind::Func,
                },
            );
        }
    }

    pub fn check(&mut self, module: &Module) -> Result<(), Vec<SemCheckError>> {
        self.with_scope(|checker| {
            // global scope
            checker.populate_funcs(&module.funcs);
            for function in &module.funcs {
                checker.check_function(&function);
            }
        });

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn check_function(&mut self, function: &ast::Function) {
        self.with_scope(|checker| {
            // add parameters to scope
            for param in &function.params {
                checker.insert_symbol(
                    &param.name,
                    Symbol {
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
                    self.errors.push(SemCheckError::VarAlreadyDefined(
                        name.to_string(),
                        expr.span,
                    ));
                } else {
                    self.check_expr(value);
                    self.insert_symbol(
                        name,
                        Symbol {
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
                    self.errors.push(SemCheckError::VarAlreadyDefined(
                        name.to_string(),
                        expr.span,
                    ));
                } else {
                    self.check_expr(value);
                    self.insert_symbol(
                        name,
                        Symbol {
                            name: name.to_string(),
                            kind: SymbolKind::Var { is_mutable: true },
                        },
                    );
                }
            }

            ast::Expr {
                kind: ExprKind::VarRef(name),
                ..
            } => {
                if self.lookup_symbol(name).is_none() {
                    self.errors
                        .push(SemCheckError::VarUndefined(name.to_string(), expr.span));
                }
            }

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
                Some(symbol) if symbol.kind == SymbolKind::Var { is_mutable: true } => {
                    self.check_expr(value)
                }
                _ => {
                    self.errors
                        .push(SemCheckError::VarImmutable(name.to_string(), expr.span));
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
            } => {
                if self
                    .lookup_symbol(name)
                    .map(|s| s.kind == SymbolKind::Func)
                    .is_none()
                {
                    self.errors
                        .push(SemCheckError::FuncUndefined(name.to_string(), expr.span));
                }
                for arg in args {
                    self.check_expr(arg);
                }
            }
        }
    }
}
