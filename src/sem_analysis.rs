use crate::ast;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Symbol {
    Variable {
        name: String,
        is_mutable: bool,
        stack_offset: u32,
    },
}

pub struct SemanticAnalyzer {
    symbols: HashMap<String, Symbol>,
    next_offset: u32,
    errors: Vec<String>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            next_offset: 0,
            errors: Vec::new(),
        }
    }

    pub fn analyze(
        &mut self,
        function: &ast::Function,
    ) -> Result<HashMap<String, Symbol>, Vec<String>> {
        self.analyze_expr(&function.body);

        if self.errors.is_empty() {
            Ok(self.symbols.clone())
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
                for expr in body {
                    self.analyze_expr(expr);
                }
            }

            ast::Expr::Let { name, value } => {
                if self.symbols.contains_key(name) {
                    self.errors
                        .push(format!("Variable already defined: {name}"));
                } else {
                    self.analyze_expr(value);
                    self.symbols.insert(
                        name.clone(),
                        Symbol::Variable {
                            name: name.clone(),
                            is_mutable: false,
                            stack_offset: self.next_offset,
                        },
                    );
                    self.next_offset += 8;
                }
            }

            ast::Expr::VarRef(name) => {
                if !self.symbols.contains_key(name) {
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

            ast::Expr::Var { name, value } => {
                self.analyze_expr(value);
                self.symbols.insert(
                    name.clone(),
                    Symbol::Variable {
                        name: name.clone(),
                        is_mutable: true,
                        stack_offset: self.next_offset,
                    },
                );
                self.next_offset += 8;
            }

            ast::Expr::Assign { name, value } => match self.symbols.get(name) {
                Some(Symbol::Variable { is_mutable, .. }) if !*is_mutable => {
                    self.errors
                        .push(format!("Cannot assign to immutable variable: {name}"));
                }
                Some(_) => self.analyze_expr(value),
                None => self.errors.push(format!("Undefined variable: {name}")),
            },
            ast::Expr::While { cond, body } => {
                self.analyze_expr(cond);
                self.analyze_expr(body);
            }
        }
    }
}
