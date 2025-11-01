use crate::ast::{BinOp, Expr, Function, Type};
use std::collections::HashMap;

pub struct TypeChecker {
    symbols: HashMap<String, Type>,
    errors: Vec<String>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn type_check(&mut self, function: &Function) -> Result<Type, Vec<String>> {
        let return_type = self.type_check_expr(&function.body).map_err(|e| vec![e])?;
        if return_type != function.return_type {
            self.errors.push(format!(
                "Return type mismatch: expected {:?}, found {:?}",
                function.return_type, return_type
            ));
        }

        if self.errors.is_empty() {
            Ok(return_type)
        } else {
            Err(self.errors.clone())
        }
    }

    fn type_check_expr(&mut self, expr: &Expr) -> Result<Type, String> {
        match expr {
            Expr::UInt32Lit(_) => Ok(Type::UInt32),
            Expr::BoolLit(_) => Ok(Type::Bool),
            Expr::BinOp { left, op, right } => {
                let left_type = self.type_check_expr(left)?;
                let right_type = self.type_check_expr(right)?;

                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        if left_type != Type::UInt32 || right_type != Type::UInt32 {
                            Err(format!(
                                "Invalid types for arithmetic operation: {:?} != {:?}",
                                left_type, right_type
                            ))
                        } else {
                            Ok(Type::UInt32)
                        }
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                        if left_type != right_type {
                            Err(format!(
                                "Invalid types for comparison operation: {:?} != {:?}",
                                left_type, right_type
                            ))
                        } else {
                            Ok(Type::Bool)
                        }
                    }
                }
            }
            Expr::UnaryOp { expr, .. } => {
                let expr_type = self.type_check_expr(expr)?;
                Ok(expr_type)
            }
            Expr::Block(body) => {
                let mut last_type = Type::Unit;
                for expr in body {
                    last_type = self.type_check_expr(expr)?;
                }
                Ok(last_type)
            }
            Expr::Let { name, value } => {
                let expr_type = self.type_check_expr(value)?;
                self.symbols.insert(name.clone(), expr_type);
                Ok(Type::Unit)
            }
            Expr::VarRef(name) => {
                if let Some(expr_type) = self.symbols.get(name) {
                    Ok(expr_type.clone())
                } else {
                    Err(format!("Undefined variable: {name}"))
                }
            }
            Expr::If {
                cond,
                then_body,
                else_body,
            } => {
                let cond_type = self.type_check_expr(cond)?;
                if cond_type != Type::Bool {
                    Err(format!(
                        "Condition must be a boolean, found {:?}",
                        cond_type
                    ))
                } else {
                    let then_type = self.type_check_expr(then_body)?;
                    let else_type = self.type_check_expr(else_body)?;
                    if then_type != else_type {
                        Err(format!(
                            "Then and else branches have different types: {:?} != {:?}",
                            then_type, else_type
                        ))
                    } else {
                        Ok(then_type)
                    }
                }
            }
        }
    }
}
