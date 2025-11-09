use crate::ast::{BinOp, Expr, ExprKind, Function, Module};
use crate::types::Type;
use std::collections::HashMap;
use thiserror::Error;

struct FuncSig {
    params: Vec<Type>,
    return_type: Type,
}

#[derive(Debug, Clone, Error)]
pub enum TypeCheckError {
    #[error("Undefined variable: {0}")]
    VarUndefined(String),

    #[error("Undefined function: {0}")]
    FuncUndefined(String),

    #[error("Type mismatch: expected {0:?}, found {1:?}")]
    FuncReturnTypeMismatch(Type, Type),

    #[error("Invalid types for arithmetic operation: {0:?} != {1:?}")]
    ArithTypeMismatch(Type, Type),

    #[error("Invalid types for comparison operation: {0:?} != {1:?}")]
    CmpTypeMismatch(Type, Type),

    #[error("Condition must be a boolean, found {0:?}")]
    CondNotBoolean(Type),

    #[error("Then and else branches have different types: {0:?} != {1:?}")]
    ThenElseTypeMismatch(Type, Type),

    #[error("Type mismatch in assignment: lhs type {0:?} != rhs type {1:?}")]
    AssignTypeMismatch(Type, Type),

    #[error("Invalid argument count for function {0}: expected {1}, found {2}")]
    ArgCountMismatch(String, usize, usize),

    #[error("Type mismatch in argument {0} for function {1}: expected {2:?}, found {3:?}")]
    ArgTypeMismatch(usize, String, Type, Type),
}

pub struct TypeChecker {
    vars: HashMap<String, Type>,
    funcs: HashMap<String, FuncSig>,
    errors: Vec<TypeCheckError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            errors: Vec::new(),
        }
    }

    fn populate_function_symbols(&mut self, functions: &Vec<Function>) {
        for function in functions {
            self.funcs.insert(
                function.name.clone(),
                FuncSig {
                    params: function.params.iter().map(|p| p.typ.clone()).collect(),
                    return_type: function.return_type.clone(),
                },
            );
        }
    }

    pub fn check(&mut self, module: &Module) -> Result<(), Vec<TypeCheckError>> {
        self.populate_function_symbols(&module.funcs);

        for function in &module.funcs {
            self.type_check_function(function)?;
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    pub fn type_check_function(
        &mut self,
        function: &Function,
    ) -> Result<Type, Vec<TypeCheckError>> {
        self.vars.clear();
        for param in &function.params {
            self.vars.insert(param.name.clone(), param.typ.clone());
        }

        let return_type = self.type_check_expr(&function.body).map_err(|e| vec![e])?;
        if return_type != function.return_type {
            self.errors.push(TypeCheckError::FuncReturnTypeMismatch(
                function.return_type.clone(),
                return_type.clone(),
            ));
        }

        if self.errors.is_empty() {
            Ok(return_type)
        } else {
            Err(self.errors.clone())
        }
    }

    fn type_check_call(&mut self, name: &str, args: &Vec<Expr>) -> Result<Type, TypeCheckError> {
        // Compute argument types first to avoid holding an immutable borrow of self.funcs
        let mut arg_types = Vec::new();
        for arg in args {
            let ty = self.type_check_expr(arg)?;
            arg_types.push(ty);
        }
        // Get function signature
        let Some(func_sig) = self.funcs.get(name) else {
            return Err(TypeCheckError::FuncUndefined(name.to_string()));
        };
        // Check number of arguments
        if arg_types.len() != func_sig.params.len() {
            return Err(TypeCheckError::ArgCountMismatch(
                name.to_string(),
                func_sig.params.len(),
                arg_types.len(),
            ));
        }
        // Check argument types
        for (i, arg_type) in arg_types.iter().enumerate() {
            if arg_type != &func_sig.params[i] {
                return Err(TypeCheckError::ArgTypeMismatch(
                    i,
                    name.to_string(),
                    func_sig.params[i].clone(),
                    arg_type.clone(),
                ));
            }
        }
        Ok(func_sig.return_type.clone())
    }

    fn type_check_expr(&mut self, expr: &Expr) -> Result<Type, TypeCheckError> {
        match expr {
            Expr {
                kind: ExprKind::UInt32Lit(_),
                ..
            } => Ok(Type::UInt32),
            Expr {
                kind: ExprKind::BoolLit(_),
                ..
            } => Ok(Type::Bool),
            Expr {
                kind: ExprKind::UnitLit,
                ..
            } => Ok(Type::Unit),
            Expr {
                kind: ExprKind::BinOp { left, op, right },
                ..
            } => {
                let left_type = self.type_check_expr(left)?;
                let right_type = self.type_check_expr(right)?;

                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        if left_type != Type::UInt32 || right_type != Type::UInt32 {
                            Err(TypeCheckError::ArithTypeMismatch(left_type, right_type))
                        } else {
                            Ok(Type::UInt32)
                        }
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                        if left_type != right_type {
                            Err(TypeCheckError::CmpTypeMismatch(left_type, right_type))
                        } else {
                            Ok(Type::Bool)
                        }
                    }
                }
            }
            Expr {
                kind: ExprKind::UnaryOp { expr, .. },
                ..
            } => {
                let expr_type = self.type_check_expr(expr)?;
                Ok(expr_type)
            }
            Expr {
                kind: ExprKind::Block(body),
                ..
            } => {
                let mut last_type = Type::Unit;
                for expr in body {
                    last_type = self.type_check_expr(expr)?;
                }
                Ok(last_type)
            }
            Expr {
                kind: ExprKind::Let { name, value },
                ..
            } => {
                let expr_type = self.type_check_expr(value)?;
                self.vars.insert(name.clone(), expr_type);
                Ok(Type::Unit)
            }
            Expr {
                kind: ExprKind::VarRef(name),
                ..
            } => {
                if let Some(expr_type) = self.vars.get(name) {
                    Ok(expr_type.clone())
                } else {
                    Err(TypeCheckError::VarUndefined(name.clone()))
                }
            }
            Expr {
                kind:
                    ExprKind::If {
                        cond,
                        then_body,
                        else_body,
                    },
                ..
            } => {
                let cond_type = self.type_check_expr(cond)?;
                if cond_type != Type::Bool {
                    Err(TypeCheckError::CondNotBoolean(cond_type))
                } else {
                    let then_type = self.type_check_expr(then_body)?;
                    let else_type = self.type_check_expr(else_body)?;
                    if then_type != else_type {
                        Err(TypeCheckError::ThenElseTypeMismatch(then_type, else_type))
                    } else {
                        Ok(then_type)
                    }
                }
            }
            Expr {
                kind: ExprKind::While { cond, body },
                ..
            } => {
                let cond_type = self.type_check_expr(cond)?;
                if cond_type != Type::Bool {
                    Err(TypeCheckError::CondNotBoolean(cond_type))
                } else {
                    let _ = self.type_check_expr(body)?;
                    Ok(Type::Unit)
                }
            }
            Expr {
                kind: ExprKind::Var { name, value },
                ..
            } => {
                let expr_type = self.type_check_expr(value)?;
                self.vars.insert(name.clone(), expr_type.clone());
                Ok(expr_type)
            }
            Expr {
                kind: ExprKind::Assign { name, value },
                ..
            } => match self.vars.get(name) {
                Some(lhs_type) => {
                    let lhs_type = lhs_type.clone();
                    let rhs_type = self.type_check_expr(value)?;
                    if lhs_type != rhs_type {
                        Err(TypeCheckError::AssignTypeMismatch(lhs_type, rhs_type))
                    } else {
                        Ok(Type::Unit)
                    }
                }
                None => Err(TypeCheckError::VarUndefined(name.clone())),
            },
            Expr {
                kind: ExprKind::Call { name, args },
                ..
            } => self.type_check_call(name, args),
        }
    }
}
