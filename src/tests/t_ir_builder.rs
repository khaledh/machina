#[cfg(test)]
mod tests {
    use crate::ast::BinaryOp;
    use crate::ir::builder::IrFunctionBuilder;
    use crate::ir::types::*;

    /// This is just a proof of concept to validate how the IR builder works.
    #[test]
    pub fn test_if_expr() {
        // fn foo() -> u32 {
        //     if 2 > 1 { 42 } else { 99 }
        // }
        let mut fb = IrFunctionBuilder::new(
            "foo".to_string(),
            IrType::Int {
                bits: 32,
                signed: false,
            },
        );

        // Create the required blocks
        let then_b = fb.new_block("then".to_string());
        let else_b = fb.new_block("else".to_string());
        let merge_b = fb.new_block("merge".to_string());

        // Build the entry block
        let cond = fb.new_temp(IrType::Bool);
        let lhs = fb.new_const_int(2, 32, false);
        let rhs = fb.new_const_int(1, 32, false);
        fb.binary_op(cond, BinaryOp::Gt, lhs, rhs);
        fb.terminate(IrTerminator::CondBr {
            cond: IrOperand::Temp(cond),
            then_b,
            else_b,
        });

        // Then block
        fb.select_block(then_b);
        let then_result = fb.new_const_int(42, 32, false);
        fb.terminate(IrTerminator::Br { target: merge_b });

        // Else block
        fb.select_block(else_b);
        let else_result = fb.new_const_int(99, 32, false);
        fb.terminate(IrTerminator::Br { target: merge_b });

        // Merge block
        fb.select_block(merge_b);
        let merge_result = fb.new_temp(IrType::Int {
            bits: 32,
            signed: false,
        });
        fb.phi(
            merge_result,
            vec![(then_b, then_result), (else_b, else_result)],
        );
        fb.terminate(IrTerminator::Ret {
            value: Some(IrOperand::Temp(merge_result)),
        });

        let function = fb.finish();

        assert_eq!(function.blocks.len(), 4);
        assert_eq!(function.blocks[&IrBlockId(0)].insts.len(), 1);
        assert_eq!(function.blocks[&IrBlockId(1)].insts.len(), 0);
        assert_eq!(function.blocks[&IrBlockId(2)].insts.len(), 0);
        assert_eq!(function.blocks[&IrBlockId(3)].insts.len(), 1);

        println!("{function}");

        // Output:
        // fn foo() -> u32 {
        // entry:
        //     %t2 = binop.gt 2, 1 : bool
        //     condbr %t2, then, else
        //
        // then:
        //     // const.int 42
        //     br merge
        //
        // else:
        //     // const.int 99
        //     br merge
        //
        // merge:
        //     %t5 = phi [(then -> const.int 42), (else -> const.int 99)]
        //     ret %t5
        // }
    }
}
