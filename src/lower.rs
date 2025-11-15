use crate::ast;
use crate::context::TypeCheckedContext;
use crate::ids::NodeId;
use crate::ir::{IrBlockBuilder, IrFunction, IrFunctionBuilder, IrTempId, IrTerminator};
use crate::types::Type;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LowerError {
    #[error("Node type not found at node {0}")]
    NodeTypeNotFound(NodeId),

    #[error("Block is empty at node {0}")]
    BlockEmpty(NodeId),
}

pub fn lower_func(
    func: &ast::Function,
    ctx: &TypeCheckedContext,
) -> Result<IrFunction, LowerError> {
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
            lower_expr_into(&mut bb, &func.body, ret_temp, ctx)?;
            Ok(bb.terminate(IrTerminator::Ret {
                value: Some(ret_temp),
            }))
        }
        None => {
            lower_expr(&mut bb, &func.body, ctx)?;
            Ok(bb.terminate(IrTerminator::Ret { value: None }))
        }
    })?;

    Ok(fn_builder.finish())
}

fn lower_expr(
    bb: &mut IrBlockBuilder,
    expr: &ast::Expr,
    ctx: &TypeCheckedContext,
) -> Result<IrTempId, LowerError> {
    // lookup the type of the expression
    let typ = get_node_type(expr, ctx)?;
    let target = bb.new_temp(typ);
    lower_expr_into(bb, expr, target, ctx)?;
    Ok(target)
}

fn lower_expr_into(
    bb: &mut IrBlockBuilder,
    expr: &ast::Expr,
    target: IrTempId,
    ctx: &TypeCheckedContext,
) -> Result<(), LowerError> {
    // handle u32 literals for now
    match &expr.kind {
        ast::ExprKind::UInt32Lit(value) => bb.const_u32(target, *value),
        ast::ExprKind::BoolLit(value) => bb.const_bool(target, *value),
        ast::ExprKind::UnitLit => bb.const_unit(target),
        ast::ExprKind::BinOp { left, op, right } => {
            let left = lower_expr(bb, &left, ctx)?;
            let right = lower_expr(bb, &right, ctx)?;
            bb.binary_op(target, *op, left, right)
        }
        ast::ExprKind::Block(body) => lower_block_into(bb, expr.id, body, target, ctx)?,
        _ => todo!("Implement other expression kinds"),
    }
    Ok(())
}

fn lower_block_into(
    bb: &mut IrBlockBuilder,
    id: NodeId,
    body: &Vec<ast::Expr>,
    target: IrTempId,
    ctx: &TypeCheckedContext,
) -> Result<(), LowerError> {
    for i in 0..body.len() - 1 {
        let expr = &body[i];
        let node_type = get_node_type(expr, ctx)?;
        let temp = bb.new_temp(node_type);
        lower_expr_into(bb, expr, temp, ctx)?;
    }
    match body.last() {
        Some(expr) => lower_expr_into(bb, expr, target, ctx),
        None => Err(LowerError::BlockEmpty(id)),
    }
}

fn get_node_type(expr: &ast::Expr, ctx: &TypeCheckedContext) -> Result<Type, LowerError> {
    ctx.type_map
        .lookup_node_type(expr.id)
        .ok_or(LowerError::NodeTypeNotFound(expr.id))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::{DefMapBuilder, TypeMapBuilder};
    use crate::diagnostics::Span;
    use crate::ids::NodeId;

    #[test]
    fn test_lower_func() {
        // fn test() -> u32 {
        //     40 + 2
        // }
        let func = ast::Function {
            id: NodeId(0),
            name: "test".to_string(),
            return_type: Type::UInt32,
            params: vec![],
            body: ast::Expr {
                id: NodeId(1),
                kind: ast::ExprKind::BinOp {
                    left: Box::new(ast::Expr {
                        id: NodeId(2),
                        kind: ast::ExprKind::UInt32Lit(40),
                        span: Span::default(),
                    }),
                    op: ast::BinaryOp::Add,
                    right: Box::new(ast::Expr {
                        id: NodeId(3),
                        kind: ast::ExprKind::UInt32Lit(2),
                        span: Span::default(),
                    }),
                },
                span: Span::default(),
            },
        };

        // Module
        let module = ast::Module { funcs: vec![func] };

        // TypeMap
        let mut type_map_builder = TypeMapBuilder::new();
        type_map_builder.record_node_type(NodeId(1), Type::UInt32);
        type_map_builder.record_node_type(NodeId(2), Type::UInt32);
        type_map_builder.record_node_type(NodeId(3), Type::UInt32);

        // Context
        let ctx = TypeCheckedContext {
            module: module,
            def_map: DefMapBuilder::new().finish(),
            type_map: type_map_builder.finish(),
        };

        // Lower
        let result = lower_func(&ctx.module.funcs[0], &ctx);

        // Assert
        assert!(result.is_ok());
        let ir_func = result.expect("Failed to lower function");
        println!("{}", ir_func);
    }
}
