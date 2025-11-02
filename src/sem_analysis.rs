use crate::ast;
use crate::ast::Module;
use std::collections::HashMap;

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

pub struct SemanticAnalyzer {
    scopes: Vec<Scope>,
    errors: Vec<String>,
}

impl SemanticAnalyzer {
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

    pub fn analyze(&mut self, module: &Module) -> Result<(), Vec<String>> {
        self.with_scope(|analyzer| {
            // global scope
            analyzer.populate_funcs(&module.funcs);
            for function in &module.funcs {
                analyzer.analyze_function(&function);
            }
        });

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn analyze_function(&mut self, function: &ast::Function) {
        self.analyze_expr(&function.body);
        self.insert_symbol(
            &function.name,
            Symbol {
                name: function.name.clone(),
                kind: SymbolKind::Func,
            },
        );
    }

    fn analyze_expr(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::UInt32Lit(_) => {}
            ast::Expr::BoolLit(_) => {}
            ast::Expr::UnitLit => {}
            ast::Expr::BinOp { left, right, .. } => {
                self.analyze_expr(left);
                self.analyze_expr(right);
            }

            ast::Expr::UnaryOp { expr, .. } => {
                self.analyze_expr(expr);
            }

            ast::Expr::Block(body) => {
                self.with_scope(|analyzer| {
                    for expr in body {
                        analyzer.analyze_expr(expr);
                    }
                });
            }

            ast::Expr::Let { name, value } => {
                if self.lookup_symbol_direct(name).is_some() {
                    self.errors
                        .push(format!("Variable already defined in current scope: {name}"));
                } else {
                    self.analyze_expr(value);
                    self.insert_symbol(
                        name,
                        Symbol {
                            name: name.to_string(),
                            kind: SymbolKind::Var { is_mutable: false },
                        },
                    );
                }
            }

            ast::Expr::Var { name, value } => {
                if self.lookup_symbol_direct(name).is_some() {
                    self.errors
                        .push(format!("Variable already defined in current scope: {name}"));
                } else {
                    self.analyze_expr(value);
                    self.insert_symbol(
                        name,
                        Symbol {
                            name: name.to_string(),
                            kind: SymbolKind::Var { is_mutable: true },
                        },
                    );
                }
            }

            ast::Expr::VarRef(name) => {
                if self.lookup_symbol(name).is_none() {
                    self.errors.push(format!("Undefined variable: {name}"));
                }
            }

            ast::Expr::If {
                cond,
                then_body,
                else_body,
            } => {
                self.analyze_expr(cond);
                self.analyze_expr(then_body);
                self.analyze_expr(else_body);
            }

            ast::Expr::Assign { name, value } => match self.lookup_symbol(name) {
                Some(symbol) if symbol.kind == SymbolKind::Var { is_mutable: true } => {
                    self.analyze_expr(value)
                }
                _ => self
                    .errors
                    .push(format!("Cannot assign to immutable variable: {name}")),
            },

            ast::Expr::While { cond, body } => {
                self.analyze_expr(cond);
                self.analyze_expr(body);
            }

            ast::Expr::Call { name, args } => {
                if self
                    .lookup_symbol(name)
                    .map(|s| s.kind == SymbolKind::Func)
                    .is_none()
                {
                    self.errors.push(format!("Undefined function: {name}"));
                }
                for arg in args {
                    self.analyze_expr(arg);
                }
            }
        }
    }
}
