#[cfg(test)]
mod tests {
    use crate::ast::BinaryOp;
    use crate::ir::builder::IrFunctionBuilder;
    use crate::ir::types::*;
    use crate::types::Type;

    /// This is just a proof of concept to validate how the IR builder works.
    #[test]
    pub fn test_if_expr() {
        // fn foo() -> u32 {
        //     if 2 > 1 { 42 } else { 99 }
        // }
        let mut fn_builder = IrFunctionBuilder::new("foo".to_string(), vec![], Type::UInt32);

        // Create the required blocks
        let then_b = fn_builder.new_block("then".to_string());
        let else_b = fn_builder.new_block("else".to_string());
        let merge_b = fn_builder.new_block("merge".to_string());

        // Build the entry block
        let lhs = fn_builder.new_temp(Type::UInt32);
        let rhs = fn_builder.new_temp(Type::UInt32);
        let cond = fn_builder.new_temp(Type::Bool);
        fn_builder.const_u32(lhs, 2);
        fn_builder.const_u32(rhs, 1);
        fn_builder.binary_op(cond, BinaryOp::Gt, lhs, rhs);
        fn_builder.terminate(IrTerminator::CondBr {
            cond,
            then_b,
            else_b,
        });

        // Then block
        fn_builder.select_block(then_b);
        let then_result = fn_builder.new_temp(Type::UInt32);
        fn_builder.const_u32(then_result, 42);
        fn_builder.terminate(IrTerminator::Br { target: merge_b });

        // Else block
        fn_builder.select_block(else_b);
        let else_result = fn_builder.new_temp(Type::UInt32);
        fn_builder.const_u32(else_result, 99);
        fn_builder.terminate(IrTerminator::Br { target: merge_b });

        // Merge block
        fn_builder.select_block(merge_b);
        let merge_result = fn_builder.new_temp(Type::UInt32);
        fn_builder.phi(
            merge_result,
            vec![(then_b, then_result), (else_b, else_result)],
        );
        fn_builder.terminate(IrTerminator::Ret {
            value: Some(merge_result),
        });

        let function = fn_builder.finish();

        assert_eq!(function.blocks.len(), 4);
        assert_eq!(function.blocks[&IrBlockId(0)].insts.len(), 3);
        assert_eq!(function.blocks[&IrBlockId(1)].insts.len(), 1);
        assert_eq!(function.blocks[&IrBlockId(2)].insts.len(), 1);
        assert_eq!(function.blocks[&IrBlockId(3)].insts.len(), 1);

        println!("{function}");

        // Output:
        // fn foo() -> u32 {
        // entry:
        //     %t0 = const.u32 2
        //     %t1 = const.u32 1
        //     %t2 = binop.gt %t0, %t1 : bool
        //     condbr %t2, then, else
        //
        // then:
        //     %t3 = const.u32 42
        //     br merge
        //
        // else:
        //     %t4 = const.u32 99
        //     br merge
        //
        // merge:
        //     %t5 = phi [(then -> %t3), (else -> %t4)]
        //     ret %t5
        // }
    }
}
