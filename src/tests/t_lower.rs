use super::*;
use crate::ast::BinaryOp;
use crate::context::AstContext;
use crate::ir::types::{IrBlockId, IrConst, IrInst, IrOperand, IrTempId, IrTerminator};
use crate::lexer::{LexError, Lexer, Token};
use crate::parser::Parser;
use crate::resolver::resolve;
use crate::type_check::type_check;

use ir_assert::*;

fn compile_and_lower(source: &str) -> Result<IrFunction, LowerError> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");

    let ast_context = AstContext::new(module);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    let type_checked_context = type_check(resolved_context).expect("Failed to type check");

    let mut lowerer = Lowerer::new(&type_checked_context);
    let ir_func = lowerer
        .lower_func(&type_checked_context.module.funcs[0])
        .expect("Failed to lower function");

    Ok(ir_func)
}

#[test]
fn test_lower_func() {
    let source = r#"
        fn test() -> u64 {
            var x = 20;
            x = x * 2;
            let y = if true { 2 } else { 3 };
            x + y
        }
    "#;

    let ir_func = compile_and_lower(source).expect("Failed to compile and lower");

    // Output:
    // fn test() -> u64 {
    // entry:
    //   %t0 = move const.20
    //   %t1 = binop.mul %t0, const.2 : u32
    //   %t0 = move %t1
    //   condbr const.bool true, then, else

    // then:
    //   %t2 = move const.2
    //   br merge

    // else:
    //   %t3 = move const.3
    //   br merge

    // merge:
    //   %t4 = phi [(then -> t2), (else -> t3)]
    //   %t5 = binop.add %t0, %t4 : u32
    //   ret %t5
    // }

    assert_eq!(ir_func.blocks.len(), 4);

    let entry_block = &ir_func.blocks[&IrBlockId(0)];
    let entry_insts = entry_block.insts();
    assert_eq!(entry_insts.len(), 3);

    let then_block = &ir_func.blocks[&IrBlockId(1)];
    let then_insts = then_block.insts();
    assert_eq!(then_insts.len(), 1);

    let else_block = &ir_func.blocks[&IrBlockId(2)];
    let else_insts = else_block.insts();
    assert_eq!(else_insts.len(), 1);

    let merge_block = &ir_func.blocks[&IrBlockId(3)];
    let merge_insts = merge_block.insts();
    assert_eq!(merge_insts.len(), 2);

    // entry block
    assert_move(&entry_insts[0], 0, const_u64(20));
    assert_binary_op(&entry_insts[1], 1, BinaryOp::Mul, temp(0), const_u64(2));
    assert_move(&entry_insts[2], 0, IrOperand::Temp(IrTempId(1)));

    // condbr in entry: constant condition, so just assert the targets
    match entry_block.term() {
        IrTerminator::CondBr { then_b, else_b, .. } => {
            assert_eq!(*then_b, then_block.id());
            assert_eq!(*else_b, else_block.id());
        }
        other => panic!("expected CondBr in entry, found {:?}", other),
    }

    // then block
    assert_move(&then_block.insts()[0], 2, const_u64(2));
    assert_br_to(&then_block.term(), merge_block.id());

    // else block
    assert_move(&else_block.insts()[0], 3, const_u64(3));
    assert_br_to(&else_block.term(), merge_block.id());

    // merge block
    assert_phi(
        &merge_block.insts()[0],
        4,
        vec![
            (then_block.id(), IrTempId(2)),
            (else_block.id(), IrTempId(3)),
        ],
    );
    assert_binary_op(&merge_block.insts()[1], 5, BinaryOp::Add, temp(0), temp(4));
    assert_ret_with(&merge_block.term(), 5);
}

#[test]
fn test_lower_while() {
    let source = r#"
        fn test() -> u64 {
            var x = 0;
            while x < 10 {
                x = x + 1;
            }
            x
        }
    "#;

    let ir_func = compile_and_lower(source).expect("Failed to compile and lower");

    // Output:
    // fn test() -> u64 {
    // entry:
    //   %t0 = move const.0
    //   br loop_header
    //
    // loop_header:
    //   %t1 = binop.lt %t0, const.10 : u32
    //   condbr %t1, loop_body, loop_after
    //
    // loop_body:
    //   %t2 = binop.add %t0, const.1 : u32
    //   %t0 = move %t2
    //   br loop_header
    //
    // loop_after:
    //   ret %t0
    // }

    assert_eq!(ir_func.blocks.len(), 4);

    let entry_block = &ir_func.blocks[&IrBlockId(0)];
    let entry_insts = entry_block.insts();
    assert_eq!(entry_insts.len(), 1);

    let loop_header_block = &ir_func.blocks[&IrBlockId(1)];
    let loop_header_insts = loop_header_block.insts();
    assert_eq!(loop_header_insts.len(), 1);

    let loop_body_block = &ir_func.blocks[&IrBlockId(2)];
    let loop_body_insts = loop_body_block.insts();
    assert_eq!(loop_body_insts.len(), 2);

    let loop_after_block = &ir_func.blocks[&IrBlockId(3)];
    let loop_after_insts = loop_after_block.insts();
    assert_eq!(loop_after_insts.len(), 0);

    // entry block
    assert_move(&entry_insts[0], 0, const_u64(0));
    assert_br_to(&entry_block.term(), loop_header_block.id());

    // loop header block
    assert_binary_op(
        &loop_header_insts[0],
        1,
        BinaryOp::Lt,
        temp(0),
        const_u64(10),
    );
    assert_cond_br(
        &loop_header_block.term(),
        1,
        loop_body_block.id(),
        loop_after_block.id(),
    );

    // loop body block
    assert_binary_op(&loop_body_insts[0], 2, BinaryOp::Add, temp(0), const_u64(1));
    assert_move(&loop_body_insts[1], 0, IrOperand::Temp(IrTempId(2)));
    assert_br_to(&loop_body_block.term(), loop_header_block.id());

    // loop after block
    assert_ret_with(&loop_after_block.term(), 0);
}

#[test]
fn test_lower_func_with_params() {
    let source = r#"
        fn test(a: u64, b: u64) -> u64 {
            a + b
        }
    "#;

    let ir_func = compile_and_lower(source).expect("Failed to compile and lower");

    // Output:
    // fn test(a: u64, b: u64) -> u64 {
    // entry:
    //   %t2 = binop.add %t0, %t1 : u32
    //   ret %t2
    // }

    assert_eq!(ir_func.blocks.len(), 1);

    let entry_block = &ir_func.blocks[&IrBlockId(0)];
    let entry_insts = entry_block.insts();
    assert_eq!(entry_insts.len(), 1);

    // entry block
    assert_binary_op(&entry_insts[0], 2, BinaryOp::Add, temp(0), temp(1));
    assert_ret_with(&entry_block.term(), 2);
}

mod ir_assert {
    use super::*;

    /// Assert a BinaryOp with a temp result and temp lhs. The rhs may be a
    /// temp or a const; when `rhs_temp_id` is `Some`, we require it to be a
    /// temp with that id, otherwise we only assert on result and lhs.
    pub fn assert_binary_op(
        inst: &IrInst,
        result_id: u32,
        op_expected: BinaryOp,
        lhs_operand: IrOperand,
        rhs_operand: IrOperand,
    ) {
        match inst {
            IrInst::BinaryOp {
                result,
                op,
                lhs,
                rhs,
            } => {
                assert_eq!(result.id(), result_id, "binary op result id mismatch");
                assert_eq!(*op, op_expected, "binary op kind mismatch");

                assert_operands_equal(lhs, &lhs_operand);
                assert_operands_equal(rhs, &rhs_operand);
            }
            other => panic!("Expected BinaryOp, found {:?}", other),
        }
    }

    pub fn assert_br_to(term: &IrTerminator, target_block: IrBlockId) {
        assert!(matches!(term, IrTerminator::Br { target }
            if *target == target_block));
    }

    pub fn assert_cond_br(term: &IrTerminator, cond_id: u32, then_b: IrBlockId, else_b: IrBlockId) {
        assert!(
            matches!(term, IrTerminator::CondBr { cond, then_b: t, else_b: e }
                if operand_temp_id(cond) == Some(cond_id) && *t == then_b && *e == else_b)
        );
    }

    pub fn assert_phi(
        inst: &IrInst,
        result_id: u32,
        incoming_expected: Vec<(IrBlockId, IrTempId)>,
    ) {
        match inst {
            IrInst::Phi { result, incoming } => {
                assert_eq!(result.id(), result_id, "phi result id mismatch");

                // Assert that the incoming blocks match
                let incoming_blocks: Vec<IrBlockId> = incoming.iter().map(|(b, _)| *b).collect();
                let expected_blocks: Vec<IrBlockId> =
                    incoming_expected.iter().map(|(b, _)| *b).collect();
                assert_eq!(
                    incoming_blocks, expected_blocks,
                    "phi incoming blocks mismatch"
                );

                // Assert that the incoming operands match
                let incoming_temps: Vec<IrTempId> = incoming.iter().map(|(_, t)| *t).collect();
                let expected_temps: Vec<IrTempId> =
                    incoming_expected.iter().map(|(_, t)| *t).collect();
                assert_eq!(
                    incoming_temps, expected_temps,
                    "phi incoming temps mismatch"
                );
            }
            other => panic!("Expected Phi, found {:?}", other),
        }
    }

    pub fn assert_ret_with(term: &IrTerminator, result_id: u32) {
        assert!(matches!(term, IrTerminator::Ret { value: Some(result) }
                if operand_temp_id(result) == Some(result_id)));
    }

    pub fn assert_move(inst: &IrInst, dest_id: u32, src_op: IrOperand) {
        match inst {
            IrInst::Move { dest, src } => {
                assert_eq!(dest.id(), dest_id, "move dest id mismatch");
                assert_operands_equal(src, &src_op);
            }
            other => panic!("Expected Move, found {:?}", other),
        }
    }

    fn operand_temp_id(op: &IrOperand) -> Option<u32> {
        match op {
            IrOperand::Temp(t) => Some(t.id()),
            _ => None,
        }
    }

    fn assert_operands_equal(a: &IrOperand, b: &IrOperand) {
        match (a, b) {
            (IrOperand::Temp(t1), IrOperand::Temp(t2)) => {
                assert_eq!(t1.id(), t2.id(), "operands are not equal");
            }
            (IrOperand::Const(c1), IrOperand::Const(c2)) => match (c1, c2) {
                (
                    IrConst::Int {
                        value: v1,
                        bits: b1,
                        signed: s1,
                    },
                    IrConst::Int {
                        value: v2,
                        bits: b2,
                        signed: s2,
                    },
                ) => {
                    assert_eq!(v1, v2, "const values are not equal");
                    assert_eq!(b1, b2, "const bits are not equal");
                    assert_eq!(s1, s2, "const signed are not equal");
                }
                (IrConst::Bool(b1), IrConst::Bool(b2)) => {
                    assert_eq!(b1, b2, "const bool are not equal");
                }
                (IrConst::Unit, IrConst::Unit) => {}
                _ => panic!("const types are not equal: {:?} != {:?}", c1, c2),
            },
            _ => panic!("operands are not equal: {:?} != {:?}", a, b),
        }
    }

    pub fn temp(id: u32) -> IrOperand {
        IrOperand::Temp(IrTempId(id))
    }

    pub fn const_u64(value: u32) -> IrOperand {
        IrOperand::Const(IrConst::Int {
            value: value as i64,
            bits: 64,
            signed: false,
        })
    }
}
