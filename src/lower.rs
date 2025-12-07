use std::collections::HashMap;
use thiserror::Error;

use crate::ast;
use crate::context::{LoweredContext, TypeCheckedContext};
use crate::ids::{DefId, NodeId};
use crate::ir::builder::IrFunctionBuilder;
use crate::ir::types::{IrFunction, IrOperand, IrTempId, IrTerminator, IrType};
use crate::types::Type;

#[derive(Debug, Error)]
pub enum LowerError {
    #[error("Parameter definition not found: Node {0}")]
    ParamDefNotFound(NodeId),

    #[error("Node type not found: Node {0}")]
    NodeTypeNotFound(NodeId),

    #[error("Block is empty: Node {0}")]
    BlockEmpty(NodeId),

    #[error("Variable definition not found: Node {0}")]
    VarDefNotFound(NodeId),

    #[error("Destination is not a temp: Node {0}, Operand {1:?}")]
    DestIsNotTemp(NodeId, IrOperand),

    #[error("Operand not found: Node {0}, Def {1}")]
    OperandNotFound(NodeId, DefId),

    #[error("Mismatched branch types: Node {0} type {1} != Node {2} type {3}")]
    MismatchedBranchTypes(NodeId, Type, NodeId, Type),

    #[error("Array is not a temp: Node {0}, Operand {1:?}")]
    ArrayIsNotTemp(NodeId, IrOperand),

    #[error("Unsupported assignee: Node {0}, Kind {1:?}")]
    UnsupportedAssignee(NodeId, ast::ExprKind),

    #[error("Array literal requires a destination temp: Node {0}")]
    ArrayLitRequiresDestTemp(NodeId),
}

pub struct Lowerer<'a> {
    ctx: &'a TypeCheckedContext,
    def_op: HashMap<DefId, IrOperand>,
}

impl<'a> Lowerer<'a> {
    pub fn new(ctx: &'a TypeCheckedContext) -> Self {
        Self {
            ctx,
            def_op: HashMap::new(),
        }
    }

    fn lower_type(&self, ty: &Type) -> IrType {
        match ty {
            Type::UInt64 => IrType::Int {
                bits: 64,
                signed: false,
            },
            Type::Bool => IrType::Bool,
            Type::Unit => IrType::Int {
                bits: 1,
                signed: false,
            },
            Type::Unknown => panic!("Unknown type"),
            Type::Array { elem_ty, len } => IrType::Array {
                elem_ty: Box::new(self.lower_type(elem_ty)),
                len: *len,
            },
        }
    }

    pub fn lower_func(&mut self, func: &ast::Function) -> Result<IrFunction, LowerError> {
        // clear the def_addr map for each function
        self.def_op.clear();

        let mut fb = IrFunctionBuilder::new(func.name.clone(), self.lower_type(&func.return_type));

        // lower params and store them in the def_temp map
        for (i, param) in func.params.iter().enumerate() {
            match self.ctx.def_map.lookup_def(param.id) {
                Some(def) => {
                    let param_temp =
                        fb.new_param(i as u32, param.name.clone(), self.lower_type(&param.typ));
                    self.def_op.insert(def.id, IrOperand::Temp(param_temp));
                }
                None => return Err(LowerError::ParamDefNotFound(param.id)),
            }
        }

        // lower the body
        let ret_temp = fb.ret_temp();
        let ret_op = self.lower_expr(&mut fb, &func.body, ret_temp)?;

        match ret_temp {
            // Indirect result (compound return type)
            Some(_) => fb.terminate(IrTerminator::Ret { value: None }),
            // Direct result
            None => fb.terminate(IrTerminator::Ret {
                value: Some(ret_op),
            }),
        }

        Ok(fb.finish())
    }

    fn lower_let(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        name: String,
        value: &ast::Expr,
    ) -> Result<IrOperand, LowerError> {
        match self.ctx.def_map.lookup_def(expr.id) {
            Some(def) => {
                let value_ty = self.lower_type(&self.get_node_type(value)?);
                let dest_temp = if value_ty.is_compound() {
                    Some(fb.new_temp(value_ty))
                } else {
                    None
                };
                let value_op = self.lower_expr(fb, value, dest_temp)?;
                // let bindings are immutable; they can hold any operand.
                if let IrOperand::Temp(temp) = value_op {
                    fb.make_local(temp, name);
                }
                self.def_op.insert(def.id, value_op);
            }
            None => return Err(LowerError::VarDefNotFound(expr.id)),
        }
        Ok(fb.new_const_unit())
    }

    fn lower_var(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        name: String,
        value: &ast::Expr,
    ) -> Result<IrOperand, LowerError> {
        match self.ctx.def_map.lookup_def(expr.id) {
            Some(def) => {
                let value_ty = self.lower_type(&self.get_node_type(value)?);
                let dest_temp = if value_ty.is_compound() {
                    Some(fb.new_temp(value_ty))
                } else {
                    None
                };
                let value_op = self.lower_expr(fb, value, dest_temp)?;
                // var bindings must always be temps so they can be reassigned.
                let temp = match value_op {
                    IrOperand::Temp(t) => t,
                    other => {
                        let ty = self.lower_type(&self.get_node_type(value)?);
                        let t = fb.new_temp(ty);
                        fb.move_to(t, other);
                        t
                    }
                };
                fb.make_local(temp, name);
                self.def_op.insert(def.id, IrOperand::Temp(temp));
            }
            None => return Err(LowerError::VarDefNotFound(expr.id)),
        }
        Ok(fb.new_const_unit())
    }

    fn lower_assign(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        assignee: &ast::Expr,
        value: &ast::Expr,
    ) -> Result<IrOperand, LowerError> {
        match &assignee.kind {
            ast::ExprKind::VarRef(_) => {
                // Variable assignment
                match self.ctx.def_map.lookup_def(assignee.id) {
                    Some(def) => {
                        let temp = match self.def_op.get(&def.id) {
                            Some(IrOperand::Temp(temp)) => *temp,
                            Some(op) => return Err(LowerError::DestIsNotTemp(expr.id, *op)),
                            None => return Err(LowerError::VarDefNotFound(expr.id)),
                        };
                        let value_ty = self.lower_type(&self.get_node_type(value)?);
                        let dest_temp = if value_ty.is_compound() {
                            Some(temp)
                        } else {
                            None
                        };
                        let value_op = self.lower_expr(fb, value, dest_temp)?;
                        // Only move if the value is scalar (compound already constructed in place)
                        if !value_ty.is_compound() {
                            fb.move_to(temp, value_op);
                        }
                    }
                    None => return Err(LowerError::VarDefNotFound(expr.id)),
                }
            }
            ast::ExprKind::Index { target, index } => {
                // Array element assignment
                let array_op = self.lower_expr(fb, target, None)?;
                let index_op = self.lower_expr(fb, index, None)?;
                // TODO: Handle compound value as array element
                let value_op = self.lower_expr(fb, value, None)?;

                let array_temp = match array_op {
                    IrOperand::Temp(temp) => temp,
                    _ => return Err(LowerError::ArrayIsNotTemp(expr.id, array_op)),
                };
                fb.store_element(array_temp, index_op, value_op);
            }
            _ => {
                return Err(LowerError::UnsupportedAssignee(
                    expr.id,
                    assignee.kind.clone(),
                ));
            }
        }
        Ok(fb.new_const_unit())
    }

    fn lower_var_ref(&mut self, expr: &ast::Expr) -> Result<IrOperand, LowerError> {
        match self.ctx.def_map.lookup_def(expr.id) {
            Some(def) => match self.def_op.get(&def.id) {
                Some(op) => Ok(*op),
                None => Err(LowerError::OperandNotFound(expr.id, def.id)),
            },
            None => Err(LowerError::VarDefNotFound(expr.id)),
        }
    }

    fn lower_array_lit(
        &mut self,
        fb: &mut IrFunctionBuilder,
        elems: &Vec<ast::Expr>,
        dest_temp: IrTempId,
    ) -> Result<IrOperand, LowerError> {
        // Store each element at its index
        for (i, elem) in elems.iter().enumerate() {
            let elem_op = self.lower_expr(fb, elem, None)?;
            let index_op = fb.new_const_int(i as i64, 64, false);
            fb.store_element(dest_temp, index_op, elem_op);
        }

        Ok(IrOperand::Temp(dest_temp))
    }

    fn lower_index(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        target: &ast::Expr,
        index: &ast::Expr,
    ) -> Result<IrOperand, LowerError> {
        let array_op = self.lower_expr(fb, target, None)?;
        let index_op = self.lower_expr(fb, index, None)?;

        // Extract the array temp from the operand
        let array_temp = match array_op {
            IrOperand::Temp(temp) => temp,
            _ => return Err(LowerError::ArrayIsNotTemp(expr.id, array_op)),
        };

        // Create a new temp for the result
        let result_ty = self.lower_type(&self.get_node_type(expr)?);
        let result = fb.new_temp(result_ty);

        fb.load_element(result, array_temp, index_op);

        Ok(IrOperand::Temp(result))
    }

    fn lower_expr(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        match &expr.kind {
            ast::ExprKind::UInt64Lit(value) => Ok(fb.new_const_int(*value as i64, 64, false)),
            ast::ExprKind::BoolLit(value) => Ok(fb.new_const_bool(*value)),
            ast::ExprKind::UnitLit => Ok(fb.new_const_unit()),
            ast::ExprKind::BinOp { left, op, right } => self.lower_binary_op(fb, op, left, right),
            ast::ExprKind::UnaryOp { op, expr } => self.lower_unary_op(fb, op, expr),
            ast::ExprKind::Block(body) => self.lower_block_into(fb, expr.id, body, dest_temp),
            ast::ExprKind::Let { name, value } => self.lower_let(fb, expr, name.clone(), value),
            ast::ExprKind::Var { name, value } => self.lower_var(fb, expr, name.clone(), value),
            ast::ExprKind::Assign {
                value, assignee, ..
            } => self.lower_assign(fb, expr, assignee, value),
            ast::ExprKind::VarRef(_) => self.lower_var_ref(expr),
            ast::ExprKind::If {
                cond,
                then_body,
                else_body,
            } => self.lower_if(fb, cond, then_body, else_body, dest_temp),
            ast::ExprKind::While { cond, body } => self.lower_while(fb, cond, body),
            ast::ExprKind::Call { callee, args } => match &callee.kind {
                ast::ExprKind::VarRef(name) => {
                    self.lower_call(fb, expr, name.clone(), args, dest_temp)
                }
                _ => panic!("Unsupported callee: {:?}", callee.kind),
            },
            ast::ExprKind::ArrayLit(elems) => {
                if let Some(dest_temp) = dest_temp {
                    self.lower_array_lit(fb, elems, dest_temp)
                } else {
                    return Err(LowerError::ArrayLitRequiresDestTemp(expr.id));
                }
            }
            ast::ExprKind::Index { target, index } => self.lower_index(fb, expr, target, index),
        }
    }

    fn lower_binary_op(
        &mut self,
        fb: &mut IrFunctionBuilder,
        op: &ast::BinaryOp,
        left: &ast::Expr,
        right: &ast::Expr,
    ) -> Result<IrOperand, LowerError> {
        let result = fb.new_temp(self.lower_type(&self.get_node_type(left)?));
        let left_op = self.lower_expr(fb, left, None)?;
        let right_op = self.lower_expr(fb, right, None)?;
        fb.binary_op(result, *op, left_op, right_op);
        Ok(IrOperand::Temp(result))
    }

    fn lower_unary_op(
        &mut self,
        fb: &mut IrFunctionBuilder,
        op: &ast::UnaryOp,
        expr: &ast::Expr,
    ) -> Result<IrOperand, LowerError> {
        let result = fb.new_temp(self.lower_type(&self.get_node_type(expr)?));
        let expr_op = self.lower_expr(fb, expr, None)?;
        fb.unary_op(result, *op, expr_op);
        Ok(IrOperand::Temp(result))
    }

    fn lower_if(
        &mut self,
        fb: &mut IrFunctionBuilder,
        cond: &ast::Expr,
        then_body: &ast::Expr,
        else_body: &ast::Expr,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        // Validate that the then and else bodies have the same type
        let then_type = self.get_node_type(then_body)?;
        let else_type = self.get_node_type(else_body)?;
        if then_type != else_type {
            return Err(LowerError::MismatchedBranchTypes(
                then_body.id,
                then_type,
                else_body.id,
                else_type,
            ));
        }

        // Create the required blocks
        let then_b = fb.new_block("then".to_string());
        let else_b = fb.new_block("else".to_string());
        let merge_b = fb.new_block("merge".to_string());

        // Build the conditional branch
        let cond = self.lower_expr(fb, cond, None)?;
        fb.terminate(IrTerminator::CondBr {
            cond,
            then_b,
            else_b,
        });

        // Then block
        fb.select_block(then_b);
        let then_op = match self.lower_expr(fb, then_body, dest_temp)? {
            IrOperand::Temp(temp) => temp,
            other => {
                // Materialize the constant to a temp (so that phi can use it)
                let temp = fb.new_temp(self.lower_type(&self.get_node_type(then_body)?));
                fb.move_to(temp, other);
                temp
            }
        };
        fb.terminate(IrTerminator::Br { target: merge_b });

        // Else block
        fb.select_block(else_b);
        let else_op = match self.lower_expr(fb, else_body, dest_temp)? {
            IrOperand::Temp(temp) => temp,
            other => {
                // Materialize the constant to a temp (so that phi can use it)
                let temp = fb.new_temp(self.lower_type(&self.get_node_type(else_body)?));
                fb.move_to(temp, other);
                temp
            }
        };
        fb.terminate(IrTerminator::Br { target: merge_b });

        // Merge block
        fb.select_block(merge_b);
        if let Some(dest_temp) = dest_temp {
            Ok(IrOperand::Temp(dest_temp))
        } else {
            let merge_type = self.get_node_type(then_body)?;
            let merge_op = fb.new_temp(self.lower_type(&merge_type));
            fb.phi(merge_op, vec![(then_b, then_op), (else_b, else_op)]);
            Ok(IrOperand::Temp(merge_op))
        }
    }

    fn lower_while(
        &mut self,
        fb: &mut IrFunctionBuilder,
        cond: &ast::Expr,
        body: &ast::Expr,
    ) -> Result<IrOperand, LowerError> {
        let header_b = fb.new_block("loop_header".to_string());
        let body_b = fb.new_block("loop_body".to_string());
        let after_b = fb.new_block("loop_after".to_string());

        // Terminate current block
        fb.terminate(IrTerminator::Br { target: header_b });

        // Build the header block
        fb.select_block(header_b);
        let cond = self.lower_expr(fb, cond, None)?;
        fb.terminate(IrTerminator::CondBr {
            cond,
            then_b: body_b,
            else_b: after_b,
        });

        // Body block
        fb.select_block(body_b);
        self.lower_expr(fb, body, None)?;
        fb.terminate(IrTerminator::Br { target: header_b });

        // After block
        fb.select_block(after_b);

        Ok(fb.new_const_unit())
    }

    fn lower_call(
        &mut self,
        fb: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        name: String,
        args: &Vec<ast::Expr>,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        let ret_ty = self.lower_type(&self.get_node_type(expr)?);

        // Lower the arguments
        let args = args
            .iter()
            .map(|arg| self.lower_expr(fb, arg, None))
            .collect::<Result<Vec<IrOperand>, LowerError>>()?;

        // Create the result temp
        let result = if let Some(dest_temp) = dest_temp {
            dest_temp
        } else {
            fb.new_temp(ret_ty.clone())
        };

        // Call the function
        fb.call(Some(result), name, args, ret_ty);

        Ok(IrOperand::Temp(result))
    }

    fn lower_block_into(
        &mut self,
        fb: &mut IrFunctionBuilder,
        id: NodeId,
        body: &Vec<ast::Expr>,
        dest_temp: Option<IrTempId>,
    ) -> Result<IrOperand, LowerError> {
        for expr in body.iter().take(body.len().saturating_sub(1)) {
            self.lower_expr(fb, expr, dest_temp)?;
        }
        match body.last() {
            Some(expr) => Ok(self.lower_expr(fb, expr, dest_temp)?),
            None => Err(LowerError::BlockEmpty(id)),
        }
    }

    fn get_node_type(&self, expr: &ast::Expr) -> Result<Type, LowerError> {
        self.ctx
            .type_map
            .lookup_node_type(expr.id)
            .ok_or(LowerError::NodeTypeNotFound(expr.id))
    }
}

pub fn lower(context: TypeCheckedContext) -> Result<LoweredContext, LowerError> {
    let mut lowerer = Lowerer::new(&context);
    let mut ir_funcs = Vec::new();
    for func in &context.module.funcs {
        let ir_func = lowerer.lower_func(&func)?;
        ir_funcs.push(ir_func);
    }
    Ok(context.with_ir_funcs(ir_funcs))
}

#[cfg(test)]
#[path = "tests/t_lower.rs"]
mod tests;
