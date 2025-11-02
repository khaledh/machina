use crate::ast;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Symbol {
    Variable { name: String, is_mutable: bool },
}

#[derive(Clone, Debug)]
pub struct Scope {
    pub symbols: HashMap<String, Symbol>,
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

    pub fn analyze(&mut self, function: &ast::Function) -> Result<Vec<Scope>, Vec<String>> {
        self.analyze_expr(&function.body);

        if self.errors.is_empty() {
            Ok(self.scopes.clone())
        } else {
            Err(self.errors.clone())
        }
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
                    let symbol = Symbol::Variable {
                        name: name.to_string(),
                        is_mutable: false,
                    };
                    self.insert_symbol(name, symbol);
                }
            }

            ast::Expr::Var { name, value } => {
                if self.lookup_symbol_direct(name).is_some() {
                    self.errors
                        .push(format!("Variable already defined in current scope: {name}"));
                } else {
                    self.analyze_expr(value);
                    let symbol = Symbol::Variable {
                        name: name.to_string(),
                        is_mutable: true,
                    };
                    self.insert_symbol(name, symbol);
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
                Some(Symbol::Variable { is_mutable, .. }) if *is_mutable => {
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
        }
    }
}
