use std::collections::HashMap;
use thiserror::Error;

use crate::ast;
use crate::context::{LoweredContext, TypeCheckedContext};
use crate::ids::{DefId, NodeId};
use crate::ir::builder::IrFunctionBuilder;
use crate::ir::{IrAddrId, IrFunction, IrParam, IrTempId, IrTerminator};
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

    #[error("Variable address not found: Node {0}, Def {1}")]
    VarAddrNotFound(NodeId, DefId),

    #[error("Mismatched branch types: Node {0} type {1} != Node {2} type {3}")]
    MismatchedBranchTypes(NodeId, Type, NodeId, Type),
}

pub struct Lowerer<'a> {
    ctx: &'a TypeCheckedContext,
    def_addr: HashMap<DefId, IrAddrId>,
}

impl<'a> Lowerer<'a> {
    pub fn new(ctx: &'a TypeCheckedContext) -> Self {
        Self {
            ctx,
            def_addr: HashMap::new(),
        }
    }

    pub fn lower_func(&mut self, func: &ast::Function) -> Result<IrFunction, LowerError> {
        // clear the def_addr map for each function
        self.def_addr.clear();

        let ir_params = func
            .params
            .iter()
            .map(|param| IrParam {
                name: param.name.clone(),
                typ: param.typ.clone(),
            })
            .collect::<Vec<IrParam>>();

        let mut fn_builder =
            IrFunctionBuilder::new(func.name.clone(), ir_params, func.return_type.clone());

        // allocate stack slots for params
        for (i, param) in func.params.iter().enumerate() {
            // get the def of the param
            match self.ctx.def_map.lookup_def(param.id) {
                Some(def) => {
                    let addr = fn_builder.alloc_var(param.typ, def.name.clone());
                    fn_builder.store_param(addr, i as u32, param.typ);
                    self.def_addr.insert(def.id, addr);
                }
                None => return Err(LowerError::ParamDefNotFound(param.id)),
            }
        }

        let ret_temp = if func.return_type != Type::Unit {
            let ret_temp = fn_builder.new_temp(func.return_type);
            self.lower_expr_into(&mut fn_builder, &func.body, ret_temp)?;
            Some(ret_temp)
        } else {
            self.lower_expr(&mut fn_builder, &func.body)?;
            None
        };

        fn_builder.terminate(IrTerminator::Ret { value: ret_temp });

        Ok(fn_builder.finish())
    }

    fn lower_expr(
        &mut self,
        fn_builder: &mut IrFunctionBuilder,
        expr: &ast::Expr,
    ) -> Result<IrTempId, LowerError> {
        // lookup the type of the expression
        let typ = self.get_node_type(expr)?;
        let target = fn_builder.new_temp(typ);
        self.lower_expr_into(fn_builder, expr, target)?;
        Ok(target)
    }

    fn lower_expr_into(
        &mut self,
        fn_builder: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        target: IrTempId,
    ) -> Result<(), LowerError> {
        match &expr.kind {
            ast::ExprKind::UInt32Lit(value) => {
                fn_builder.const_u32(target, *value);
            }
            ast::ExprKind::BoolLit(value) => {
                fn_builder.const_bool(target, *value);
            }
            ast::ExprKind::UnitLit => {
                fn_builder.const_unit(target);
            }
            ast::ExprKind::BinOp { left, op, right } => {
                self.lower_binary_op(fn_builder, op, left, right, target)?;
            }
            ast::ExprKind::Block(body) => {
                self.lower_block_into(fn_builder, expr.id, body, target)?;
            }
            ast::ExprKind::Let { name, value } | ast::ExprKind::Var { name, value } => {
                self.lower_let_or_var(fn_builder, expr, name.clone(), value)?;
            }
            ast::ExprKind::Assign { value, .. } => {
                self.lower_assign(fn_builder, expr, value)?;
            }
            ast::ExprKind::VarRef(_) => {
                self.lower_var_ref(fn_builder, expr, target)?;
            }
            ast::ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.lower_if(fn_builder, cond, then_body, else_body, target)?;
            }
            ast::ExprKind::While { cond, body } => {
                self.lower_while(fn_builder, cond, body)?;
            }
            _ => todo!("Implement other expression kinds"),
        }
        Ok(())
    }

    fn lower_binary_op(
        &mut self,
        fn_builder: &mut IrFunctionBuilder,
        op: &ast::BinaryOp,
        left: &ast::Expr,
        right: &ast::Expr,
        target: IrTempId,
    ) -> Result<(), LowerError> {
        let left = self.lower_expr(fn_builder, left)?;
        let right = self.lower_expr(fn_builder, right)?;
        fn_builder.binary_op(target, *op, left, right);
        Ok(())
    }

    fn lower_let_or_var(
        &mut self,
        fn_builder: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        name: String,
        value: &ast::Expr,
    ) -> Result<(), LowerError> {
        match self.ctx.def_map.lookup_def(expr.id) {
            Some(def) => {
                let addr = fn_builder.alloc_var(self.get_node_type(value)?, name.clone());
                let value = self.lower_expr(fn_builder, value)?;
                fn_builder.store_var(addr, value);
                self.def_addr.insert(def.id, addr);
            }
            None => return Err(LowerError::VarDefNotFound(expr.id)),
        }
        Ok(())
    }

    fn lower_assign(
        &mut self,
        fn_builder: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        value: &ast::Expr,
    ) -> Result<(), LowerError> {
        match self.ctx.def_map.lookup_def(expr.id) {
            Some(def) => {
                let addr = match self.def_addr.get(&def.id) {
                    Some(addr) => *addr,
                    None => return Err(LowerError::VarAddrNotFound(expr.id, def.id)),
                };
                let assigned_value = fn_builder.new_temp(self.get_node_type(value)?);
                self.lower_expr_into(fn_builder, value, assigned_value)?;
                fn_builder.store_var(addr, assigned_value);
            }
            None => return Err(LowerError::VarDefNotFound(expr.id)),
        }
        Ok(())
    }

    fn lower_var_ref(
        &mut self,
        fn_builder: &mut IrFunctionBuilder,
        expr: &ast::Expr,
        target: IrTempId,
    ) -> Result<(), LowerError> {
        match self.ctx.def_map.lookup_def(expr.id) {
            Some(def) => {
                let addr = match self.def_addr.get(&def.id) {
                    Some(addr) => *addr,
                    None => return Err(LowerError::VarAddrNotFound(expr.id, def.id)),
                };
                fn_builder.load_var_into(target, addr);
            }
            None => return Err(LowerError::VarDefNotFound(expr.id)),
        }
        Ok(())
    }

    fn lower_if(
        &mut self,
        fn_builder: &mut IrFunctionBuilder,
        cond: &ast::Expr,
        then_body: &ast::Expr,
        else_body: &ast::Expr,
        target: IrTempId,
    ) -> Result<(), LowerError> {
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
        let then_b = fn_builder.new_block("then".to_string());
        let else_b = fn_builder.new_block("else".to_string());
        let merge_b = fn_builder.new_block("merge".to_string());

        // Build the conditional branch
        let cond = self.lower_expr(fn_builder, cond)?;
        fn_builder.terminate(IrTerminator::CondBr {
            cond,
            then_b,
            else_b,
        });

        // Then block
        fn_builder.select_block(then_b);
        let then_target = fn_builder.new_temp(self.get_node_type(then_body)?);
        self.lower_expr_into(fn_builder, then_body, then_target)?;
        fn_builder.terminate(IrTerminator::Br { target: merge_b });

        // Else block
        fn_builder.select_block(else_b);
        let else_target = fn_builder.new_temp(self.get_node_type(else_body)?);
        self.lower_expr_into(fn_builder, else_body, else_target)?;
        fn_builder.terminate(IrTerminator::Br { target: merge_b });

        // Merge block
        fn_builder.select_block(merge_b);
        fn_builder.phi(target, vec![(then_b, then_target), (else_b, else_target)]);

        Ok(())
    }

    fn lower_while(
        &mut self,
        fn_builder: &mut IrFunctionBuilder,
        cond: &ast::Expr,
        body: &ast::Expr,
    ) -> Result<(), LowerError> {
        let header_b = fn_builder.new_block("loop_header".to_string());
        let body_b = fn_builder.new_block("loop_body".to_string());
        let after_b = fn_builder.new_block("loop_after".to_string());

        // Terminate current block
        fn_builder.terminate(IrTerminator::Br { target: header_b });

        // Build the header block
        fn_builder.select_block(header_b);
        let cond = self.lower_expr(fn_builder, cond)?;
        fn_builder.terminate(IrTerminator::CondBr {
            cond,
            then_b: body_b,
            else_b: after_b,
        });

        // Body block
        fn_builder.select_block(body_b);
        self.lower_expr(fn_builder, body)?;
        fn_builder.terminate(IrTerminator::Br { target: header_b });

        // After block
        fn_builder.select_block(after_b);

        Ok(())
    }

    fn lower_block_into(
        &mut self,
        fn_builder: &mut IrFunctionBuilder,
        id: NodeId,
        body: &Vec<ast::Expr>,
        target: IrTempId,
    ) -> Result<(), LowerError> {
        for expr in body.iter().take(body.len().saturating_sub(1)) {
            let node_type = self.get_node_type(expr)?;
            let temp = fn_builder.new_temp(node_type);
            self.lower_expr_into(fn_builder, expr, temp)?;
        }
        match body.last() {
            Some(expr) => self.lower_expr_into(fn_builder, expr, target),
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
