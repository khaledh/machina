use std::collections::HashMap;
use thiserror::Error;

use crate::ast;
use crate::context::TypeCheckedContext;
use crate::ids::{DefId, NodeId};
use crate::ir::{IrAddrId, IrFunction, IrFunctionBuilder, IrTempId, IrTerminator};
use crate::types::Type;

#[derive(Debug, Error)]
pub enum LowerError {
    #[error("Node type not found: Node {0}")]
    NodeTypeNotFound(NodeId),

    #[error("Block is empty: Node {0}")]
    BlockEmpty(NodeId),

    #[error("Variable definition not found: Node {0}")]
    VarDefNotFound(NodeId),

    #[error("Variable address not found: Node {0}, Def {1}")]
    VarAddrNotFound(NodeId, DefId),
}

pub struct Lowerer {
    ctx: TypeCheckedContext,
    def_addr: HashMap<DefId, IrAddrId>,
}

impl Lowerer {
    pub fn new(ctx: TypeCheckedContext) -> Self {
        Self {
            ctx,
            def_addr: HashMap::new(),
        }
    }

    pub fn lower_func(&mut self, func: &ast::Function) -> Result<IrFunction, LowerError> {
        // clear the def_addr map for each function
        self.def_addr.clear();

        let mut fn_builder =
            IrFunctionBuilder::new(func.name.clone(), vec![], func.return_type.clone());

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
            Some(def_id) => {
                let addr = fn_builder.alloc_var(self.get_node_type(value)?, name.clone());
                let value = self.lower_expr(fn_builder, value)?;
                fn_builder.store_var(addr, value);
                self.def_addr.insert(def_id, addr);
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
            Some(def_id) => {
                let addr = match self.def_addr.get(&def_id) {
                    Some(addr) => *addr,
                    None => return Err(LowerError::VarAddrNotFound(expr.id, def_id)),
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
            Some(def_id) => {
                let addr = match self.def_addr.get(&def_id) {
                    Some(addr) => *addr,
                    None => return Err(LowerError::VarAddrNotFound(expr.id, def_id)),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::{DefMapBuilder, TypeMapBuilder};
    use crate::ast::BinaryOp;
    use crate::context::Context;
    use crate::ir::{IrAddrId, IrBlockId, IrInst, IrTempId};
    use crate::lexer::{LexError, Lexer, Token};
    use crate::parser::Parser;
    use crate::resolver::resolve;
    use crate::type_check::type_check;

    fn compile_and_lower(source: &str) -> Result<IrFunction, LowerError> {
        let lexer = Lexer::new(source);
        let tokens = lexer
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .expect("Failed to tokenize");

        let mut parser = Parser::new(&tokens);
        let module = parser.parse().expect("Failed to parse");

        let context = Context::new(module);
        let resolved_context = resolve(context).expect("Failed to resolve");
        let type_checked_context = type_check(resolved_context).expect("Failed to type check");

        let mut lowerer = Lowerer::new(type_checked_context.clone());
        let ir_func = lowerer
            .lower_func(&type_checked_context.module.funcs[0])
            .expect("Failed to lower function");

        Ok(ir_func)
    }

    #[test]
    fn test_lower_func() {
        let source = r#"
            fn test() -> u32 {
                var x = 20;
                x = x * 2;
                let y = if true { 2 } else { 3 };
                x + y
            }
        "#;

        let ir_func = compile_and_lower(source).expect("Failed to compile and lower");

        // Assert
        // if result is Err, print the error
        println!("{}", ir_func);

        // Output:
        // fn test() -> u32 {
        // entry:
        //     &a0 = alloc u32 (size=4, align=4) name_hint=x
        //     %t2 = const.u32 20
        //     store %t2 -> &a0 : u32
        //     %t5 <- load &a0 : u32
        //     %t6 = const.u32 2
        //     %t4 = binop.mul %t5, %t6 : u32
        //     store %t4 -> &a0 : u32
        //     &a1 = alloc u32 (size=4, align=4) name_hint=y
        //     %t9 = const.bool true
        //     condbr %t9, then, else

        // then:
        //     %t10 = const.u32 2
        //     br merge

        // else:
        //     %t11 = const.u32 3
        //     br merge

        // merge:
        //     %t8 = phi [(then -> %t10), (else -> %t11)]
        //     store %t8 -> &a1 : u32
        //     %t12 <- load &a0 : u32
        //     %t13 <- load &a1 : u32
        //     %t0 = binop.add %t12, %t13 : u32
        //     ret %t0
        // }

        assert_eq!(ir_func.blocks.len(), 4);

        let entry_block = &ir_func.blocks[0];
        let entry_insts = entry_block.insts();
        assert_eq!(entry_insts.len(), 9);

        let then_block = &ir_func.blocks[1];
        let then_insts = then_block.insts();
        assert_eq!(then_insts.len(), 1);

        let else_block = &ir_func.blocks[2];
        let else_insts = else_block.insts();
        assert_eq!(else_insts.len(), 1);

        let merge_block = &ir_func.blocks[3];
        let merge_insts = merge_block.insts();
        assert_eq!(merge_insts.len(), 5);

        //
        // entry block instructions
        //

        // &a0 = alloc u32 (size=4, align=4) name_hint=x
        assert!(
            matches!(&entry_insts[0], IrInst::AllocVar { addr, typ, name_hint }
            if addr.id() == 0 && *typ == Type::UInt32 && name_hint == "x")
        );

        // %t2 = const.u32 20
        assert!(matches!(&entry_insts[1], IrInst::ConstU32 { result, value }
            if result.id() == 2 && *value == 20));

        // store %t2 -> &a0 : u32
        assert!(
            matches!(&entry_insts[2], IrInst::StoreVar { addr, value, typ }
            if addr.id() == 0 && value.id() == 2 && *typ == Type::UInt32)
        );

        // %t5 <- load &a0 : u32
        assert!(
            matches!(&entry_insts[3], IrInst::LoadVar { value, addr, typ }
            if value.id() == 5 && addr.id() == 0 && *typ == Type::UInt32)
        );

        // %t6 = const.u32 2
        assert!(matches!(&entry_insts[4], IrInst::ConstU32 { result, value }
            if result.id() == 6 && *value == 2));

        // %t4 = binop.mul %t5, %t6 : u32
        assert!(
            matches!(&entry_insts[5], IrInst::BinaryOp { result, op, lhs, rhs }
            if result.id() == 4 && *op == BinaryOp::Mul && lhs.id() == 5 && rhs.id() == 6)
        );

        // store %t4 -> &a0 : u32
        assert!(
            matches!(&entry_insts[6], IrInst::StoreVar { addr, value, typ }
            if addr.id() == 0 && value.id() == 4 && *typ == Type::UInt32)
        );

        // &a1 = alloc u32 (size=4, align=4) name_hint=y
        assert!(
            matches!(&entry_insts[7], IrInst::AllocVar { addr, typ, name_hint }
            if addr.id() == 1 && *typ == Type::UInt32 && name_hint == "y")
        );

        //
        // then block instructions
        //

        // %t10 = const.u32 2
        assert!(
            matches!(&then_block.insts()[0], IrInst::ConstU32 { result, value }
            if result.id() == 10 && *value == 2)
        );

        // br merge
        assert!(matches!(&then_block.term(), IrTerminator::Br { target }
                if *target == merge_block.id()));

        //
        // else block instructions
        //

        // %t11 = const.u32 3
        assert!(
            matches!(&else_block.insts()[0], IrInst::ConstU32 { result, value }
            if result.id() == 11 && *value == 3)
        );

        // br merge
        assert!(matches!(&else_block.term(), IrTerminator::Br { target }
            if *target == merge_block.id()));

        //
        // merge block instructions
        //

        // %t8 = phi [ (then -> %t10), (else -> %t11) ]
        assert!(
            matches!(&merge_block.insts()[0], IrInst::Phi { result, incoming }
            if result.id() == 8 && *incoming == vec![
                (then_block.id(), IrTempId(10)),
                (else_block.id(), IrTempId(11)),
            ])
        );

        // store %t8 -> &a1 : u32
        assert!(
            matches!(&merge_block.insts()[1], IrInst::StoreVar { addr, value, typ }
            if addr.id() == 1 && value.id() == 8 && *typ == Type::UInt32)
        );

        // %t12 <- load &a0 : u32
        assert!(
            matches!(&merge_block.insts()[2], IrInst::LoadVar { value, addr, typ }
            if value.id() == 12 && addr.id() == 0 && *typ == Type::UInt32)
        );

        // %t13 <- load &a1 : u32
        assert!(
            matches!(&merge_block.insts()[3], IrInst::LoadVar { value, addr, typ }
            if value.id() == 13 && addr.id() == 1 && *typ == Type::UInt32)
        );

        // %t0 = binop.add %t12, %t13 : u32
        assert!(
            matches!(&merge_block.insts()[4], IrInst::BinaryOp { result, op, lhs, rhs }
            if result.id() == 0 && *op == BinaryOp::Add && lhs.id() == 12 && rhs.id() == 13)
        );

        // ret %t0
        assert!(
            matches!(&merge_block.term(), IrTerminator::Ret { value: Some(result) }
            if result.id() == 0)
        );
    }
}
