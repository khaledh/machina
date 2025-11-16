use std::collections::HashMap;
use thiserror::Error;

use crate::ast;
use crate::context::TypeCheckedContext;
use crate::ids::{DefId, NodeId};
use crate::ir::{IrAddrId, IrBlockBuilder, IrFunction, IrFunctionBuilder, IrTempId, IrTerminator};
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
    def_addr: HashMap<DefId, IrAddrId>,
    ctx: TypeCheckedContext,
}

impl Lowerer {
    pub fn new(ctx: TypeCheckedContext) -> Self {
        Self {
            def_addr: HashMap::new(),
            ctx,
        }
    }

    pub fn lower_func(&mut self, func: &ast::Function) -> Result<IrFunction, LowerError> {
        // clear the def_addr map for each function
        self.def_addr.clear();

        let mut fn_builder =
            IrFunctionBuilder::new(func.name.clone(), vec![], func.return_type.clone());

        let ret_temp = if func.return_type != Type::Unit {
            Some(fn_builder.new_temp(func.return_type))
        } else {
            None
        };

        let entry = fn_builder.new_block("entry".to_string());
        fn_builder.build_block(entry, |mut bb| match ret_temp {
            Some(ret_temp) => {
                self.lower_expr_into(&mut bb, &func.body, ret_temp)?;
                Ok(bb.terminate(IrTerminator::Ret {
                    value: Some(ret_temp),
                }))
            }
            None => {
                self.lower_expr(&mut bb, &func.body)?;
                Ok(bb.terminate(IrTerminator::Ret { value: None }))
            }
        })?;

        Ok(fn_builder.finish())
    }

    fn lower_expr(
        &mut self,
        bb: &mut IrBlockBuilder,
        expr: &ast::Expr,
    ) -> Result<IrTempId, LowerError> {
        // lookup the type of the expression
        let typ = self.get_node_type(expr)?;
        let target = bb.new_temp(typ);
        self.lower_expr_into(bb, expr, target)?;
        Ok(target)
    }

    fn lower_expr_into(
        &mut self,
        bb: &mut IrBlockBuilder,
        expr: &ast::Expr,
        target: IrTempId,
    ) -> Result<(), LowerError> {
        match &expr.kind {
            ast::ExprKind::UInt32Lit(value) => {
                bb.const_u32(target, *value);
            }
            ast::ExprKind::BoolLit(value) => {
                bb.const_bool(target, *value);
            }
            ast::ExprKind::UnitLit => {
                bb.const_unit(target);
            }
            ast::ExprKind::BinOp { left, op, right } => {
                self.lower_binary_op(bb, op, left, right, target)?;
            }
            ast::ExprKind::Block(body) => {
                self.lower_block_into(bb, expr.id, body, target)?;
            }
            ast::ExprKind::Let { name, value } | ast::ExprKind::Var { name, value } => {
                self.lower_let_or_var(bb, expr, name.clone(), value)?;
            }
            ast::ExprKind::Assign { value, .. } => {
                self.lower_assign(bb, expr, value)?;
            }
            ast::ExprKind::VarRef(_) => {
                self.lower_var_ref(bb, expr, target)?;
            }
            _ => todo!("Implement other expression kinds"),
        }
        Ok(())
    }

    fn lower_binary_op(
        &mut self,
        bb: &mut IrBlockBuilder,
        op: &ast::BinaryOp,
        left: &ast::Expr,
        right: &ast::Expr,
        target: IrTempId,
    ) -> Result<(), LowerError> {
        let left = self.lower_expr(bb, left)?;
        let right = self.lower_expr(bb, right)?;
        bb.binary_op(target, *op, left, right);
        Ok(())
    }

    fn lower_let_or_var(
        &mut self,
        bb: &mut IrBlockBuilder,
        expr: &ast::Expr,
        name: String,
        value: &ast::Expr,
    ) -> Result<(), LowerError> {
        match self.ctx.def_map.lookup_def(expr.id) {
            Some(def_id) => {
                let addr = bb.alloc_var(self.get_node_type(value)?, name.clone());
                let value = self.lower_expr(bb, value)?;
                bb.store_var(addr, value);
                self.def_addr.insert(def_id, addr);
            }
            None => return Err(LowerError::VarDefNotFound(expr.id)),
        }
        Ok(())
    }

    fn lower_assign(
        &mut self,
        bb: &mut IrBlockBuilder,
        expr: &ast::Expr,
        value: &ast::Expr,
    ) -> Result<(), LowerError> {
        match self.ctx.def_map.lookup_def(expr.id) {
            Some(def_id) => {
                let addr = match self.def_addr.get(&def_id) {
                    Some(addr) => *addr,
                    None => return Err(LowerError::VarAddrNotFound(expr.id, def_id)),
                };
                let assigned_value = bb.new_temp(self.get_node_type(value)?);
                self.lower_expr_into(bb, value, assigned_value)?;
                bb.store_var(addr, assigned_value);
            }
            None => return Err(LowerError::VarDefNotFound(expr.id)),
        }
        Ok(())
    }

    fn lower_var_ref(
        &mut self,
        bb: &mut IrBlockBuilder,
        expr: &ast::Expr,
        target: IrTempId,
    ) -> Result<(), LowerError> {
        match self.ctx.def_map.lookup_def(expr.id) {
            Some(def_id) => {
                let addr = match self.def_addr.get(&def_id) {
                    Some(addr) => *addr,
                    None => return Err(LowerError::VarAddrNotFound(expr.id, def_id)),
                };
                bb.load_var_into(target, addr);
            }
            None => return Err(LowerError::VarDefNotFound(expr.id)),
        }
        Ok(())
    }

    fn lower_block_into(
        &mut self,
        bb: &mut IrBlockBuilder,
        id: NodeId,
        body: &Vec<ast::Expr>,
        target: IrTempId,
    ) -> Result<(), LowerError> {
        for i in 0..body.len() - 1 {
            let expr = &body[i];
            let node_type = self.get_node_type(expr)?;
            let temp = bb.new_temp(node_type);
            self.lower_expr_into(bb, expr, temp)?;
        }
        match body.last() {
            Some(expr) => self.lower_expr_into(bb, expr, target),
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
    use crate::diagnostics::Span;
    use crate::ids::NodeId;
    use crate::ir::{IrAddrId, IrInst, IrTempId};

    #[test]
    fn test_lower_func() {
        // fn test() -> u32 {
        //     var x = 20;
        //     x = x * 2;
        //     let y = 2;
        //     x + y
        // }
        let func = ast::Function {
            id: NodeId(0),
            name: "test".to_string(),
            return_type: Type::UInt32,
            params: vec![],
            body: ast::Expr {
                id: NodeId(1),
                kind: ast::ExprKind::Block(vec![
                    ast::Expr {
                        id: NodeId(2),
                        kind: ast::ExprKind::Var {
                            name: "x".to_string(),
                            value: Box::new(ast::Expr {
                                id: NodeId(3),
                                kind: ast::ExprKind::UInt32Lit(20),
                                span: Span::default(),
                            }),
                        },
                        span: Span::default(),
                    },
                    ast::Expr {
                        id: NodeId(4),
                        kind: ast::ExprKind::Assign {
                            name: "x".to_string(),
                            value: Box::new(ast::Expr {
                                id: NodeId(5),
                                kind: ast::ExprKind::BinOp {
                                    left: Box::new(ast::Expr {
                                        id: NodeId(6),
                                        kind: ast::ExprKind::VarRef("x".to_string()),
                                        span: Span::default(),
                                    }),
                                    op: ast::BinaryOp::Mul,
                                    right: Box::new(ast::Expr {
                                        id: NodeId(7),
                                        kind: ast::ExprKind::UInt32Lit(2),
                                        span: Span::default(),
                                    }),
                                },
                                span: Span::default(),
                            }),
                        },
                        span: Span::default(),
                    },
                    ast::Expr {
                        id: NodeId(8),
                        kind: ast::ExprKind::Let {
                            name: "y".to_string(),
                            value: Box::new(ast::Expr {
                                id: NodeId(9),
                                kind: ast::ExprKind::UInt32Lit(2),
                                span: Span::default(),
                            }),
                        },
                        span: Span::default(),
                    },
                    ast::Expr {
                        id: NodeId(10),
                        kind: ast::ExprKind::BinOp {
                            left: Box::new(ast::Expr {
                                id: NodeId(11),
                                kind: ast::ExprKind::VarRef("x".to_string()),
                                span: Span::default(),
                            }),
                            op: ast::BinaryOp::Add,
                            right: Box::new(ast::Expr {
                                id: NodeId(12),
                                kind: ast::ExprKind::VarRef("y".to_string()),
                                span: Span::default(),
                            }),
                        },
                        span: Span::default(),
                    },
                ]),
                span: Span::default(),
            },
        };

        // Module
        let module = ast::Module {
            funcs: vec![func.clone()],
        };

        // DefMap
        let mut def_map_builder = DefMapBuilder::new();
        // defs
        def_map_builder.record_def(DefId(1), NodeId(2)); // var _x_
        def_map_builder.record_def(DefId(2), NodeId(8)); // let _y_
        // uses
        def_map_builder.record_use(NodeId(4), DefId(1)); // _x_ = x * 2
        def_map_builder.record_use(NodeId(6), DefId(1)); // x = _x_ * 2
        def_map_builder.record_use(NodeId(11), DefId(1)); // _x_ + y
        def_map_builder.record_use(NodeId(12), DefId(2)); // x + _y_

        let def_map = def_map_builder.finish();

        // TypeMap
        let mut type_map_builder = TypeMapBuilder::new();
        type_map_builder.record_node_type(NodeId(1), Type::UInt32);
        type_map_builder.record_node_type(NodeId(2), Type::UInt32);
        type_map_builder.record_node_type(NodeId(3), Type::UInt32);
        type_map_builder.record_node_type(NodeId(4), Type::UInt32);
        type_map_builder.record_node_type(NodeId(5), Type::UInt32);
        type_map_builder.record_node_type(NodeId(6), Type::UInt32);
        type_map_builder.record_node_type(NodeId(7), Type::UInt32);
        type_map_builder.record_node_type(NodeId(8), Type::UInt32);
        type_map_builder.record_node_type(NodeId(9), Type::UInt32);
        type_map_builder.record_node_type(NodeId(11), Type::UInt32);
        type_map_builder.record_node_type(NodeId(12), Type::UInt32);

        let type_map = type_map_builder.finish();

        // Context
        let ctx = TypeCheckedContext {
            module,
            def_map,
            type_map,
        };

        // Lower
        let mut lowerer = Lowerer::new(ctx.clone());
        let result = lowerer.lower_func(&func);

        // Assert
        // if result is Err, print the error
        let ir_func = result.expect("Failed to lower function");
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
        //     %t8 = const.u32 2
        //     store %t8 -> &a1 : u32
        //     %t9 <- load &a0 : u32
        //     %t10 <- load &a1 : u32
        //     %t0 = binop.add %t9, %t10 : u32
        //     ret %t0
        // }
        assert_eq!(ir_func.blocks.len(), 1);
        let block = &ir_func.blocks[0];
        let insts = block.insts();
        assert_eq!(insts.len(), 13);

        // &a0 = alloc u32 (size=4, align=4) name_hint=x
        assert!(
            matches!(&insts[0], IrInst::AllocVar { addr, typ, name_hint }
            if addr.id() == 0 && *typ == Type::UInt32 && name_hint == "x")
        );

        // %t2 = const.u32 20
        assert!(matches!(&insts[1], IrInst::ConstU32 { result, value }
            if result.id() == 2 && *value == 20));

        // store %t2 -> &a0 : u32
        assert!(matches!(&insts[2], IrInst::StoreVar { addr, value, typ }
            if addr.id() == 0 && value.id() == 2 && *typ == Type::UInt32));

        // %t5 <- load &a0 : u32
        assert!(matches!(&insts[3], IrInst::LoadVar { value, addr, typ }
            if value.id() == 5 && addr.id() == 0 && *typ == Type::UInt32));

        // %t6 = const.u32 2
        assert!(matches!(&insts[4], IrInst::ConstU32 { result, value }
            if result.id() == 6 && *value == 2));

        // %t4 = binop.mul %t5, %t6 : u32
        assert!(
            matches!(&insts[5], IrInst::BinaryOp { result, op, lhs, rhs }
            if result.id() == 4 && *op == BinaryOp::Mul && lhs.id() == 5 && rhs.id() == 6)
        );

        // store %t4 -> &a0 : u32
        assert!(matches!(&insts[6], IrInst::StoreVar { addr, value, typ }
            if addr.id() == 0 && value.id() == 4 && *typ == Type::UInt32));

        // &a1 = alloc u32 (size=4, align=4) name_hint=y
        assert!(
            matches!(&insts[7], IrInst::AllocVar { addr, typ, name_hint }
            if addr.id() == 1 && *typ == Type::UInt32 && name_hint == "y")
        );

        // %t8 = const.u32 2
        assert!(matches!(&insts[8], IrInst::ConstU32 { result, value }
            if result.id() == 8 && *value == 2));

        // store %t8 -> &a1 : u32
        assert!(matches!(&insts[9], IrInst::StoreVar { addr, value, typ }
            if addr.id() == 1 && value.id() == 8 && *typ == Type::UInt32));

        // %t9 <- load &a0 : u32
        assert!(matches!(&insts[10], IrInst::LoadVar { value, addr, typ }
            if value.id() == 9 && addr.id() == 0 && *typ == Type::UInt32));

        // %t10 <- load &a1 : u32
        assert!(matches!(&insts[11], IrInst::LoadVar { value, addr, typ }
            if value.id() == 10 && addr.id() == 1 && *typ == Type::UInt32));

        // %t0 = binop.add %t9, %t10 : u32
        assert!(
            matches!(&insts[12], IrInst::BinaryOp { result, op, lhs, rhs }
            if result.id() == 0 && *op == BinaryOp::Add && lhs.id() == 9 && rhs.id() == 10)
        );

        // ret %t0
        assert!(matches!(block.term(), IrTerminator::Ret { value: Some(v) } if v.id() == 0));
    }
}
