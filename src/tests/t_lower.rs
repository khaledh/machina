use super::*;
use crate::ast::BinaryOp;
use crate::context::AstContext;
use crate::ir::types::{IrBlockId, IrInst, IrTempId, IrTerminator};
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

    let entry_block = &ir_func.blocks[&IrBlockId(0)];
    let entry_insts = entry_block.insts();
    assert_eq!(entry_insts.len(), 9);

    let then_block = &ir_func.blocks[&IrBlockId(1)];
    let then_insts = then_block.insts();
    assert_eq!(then_insts.len(), 1);

    let else_block = &ir_func.blocks[&IrBlockId(2)];
    let else_insts = else_block.insts();
    assert_eq!(else_insts.len(), 1);

    let merge_block = &ir_func.blocks[&IrBlockId(3)];
    let merge_insts = merge_block.insts();
    assert_eq!(merge_insts.len(), 5);

    // entry block
    assert_alloc_var(&entry_insts[0], 0, "x", Type::UInt32);
    assert_const_u32(&entry_insts[1], 2, 20);
    assert_store_var(&entry_insts[2], 0, 2, Type::UInt32);
    assert_load_var(&entry_insts[3], 5, 0, Type::UInt32);
    assert_const_u32(&entry_insts[4], 6, 2);
    assert_binary_op(&entry_insts[5], 4, BinaryOp::Mul, 5, 6);
    assert_store_var(&entry_insts[6], 0, 4, Type::UInt32);
    assert_alloc_var(&entry_insts[7], 1, "y", Type::UInt32);

    // then block
    assert_const_u32(&then_block.insts()[0], 10, 2);
    assert_br_to(&then_block.term(), merge_block.id());

    // else block
    assert_const_u32(&else_block.insts()[0], 11, 3);
    assert_br_to(&else_block.term(), merge_block.id());

    // merge block
    assert_phi(
        &merge_block.insts()[0],
        8,
        vec![
            (then_block.id(), IrTempId(10)),
            (else_block.id(), IrTempId(11)),
        ],
    );
    assert_store_var(&merge_block.insts()[1], 1, 8, Type::UInt32);
    assert_load_var(&merge_block.insts()[2], 12, 0, Type::UInt32);
    assert_load_var(&merge_block.insts()[3], 13, 1, Type::UInt32);
    assert_binary_op(&merge_block.insts()[4], 0, BinaryOp::Add, 12, 13);
    assert_ret_with(&merge_block.term(), 0);
}

#[test]
fn test_lower_while() {
    let source = r#"
        fn test() -> u32 {
            var x = 0;
            while x < 10 {
                x = x + 1;
            }
            x
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
    //     %t2 = const.u32 0
    //     store %t2 -> &a0 : u32
    //     br loop_header
    //
    // loop_header:
    //     %t5 <- load &a0 : u32
    //     %t6 = const.u32 10
    //     %t4 = binop.lt %t5, %t6 : bool
    //     condbr %t4, loop_body, loop_after
    //
    // loop_body:
    //     %t9 <- load &a0 : u32
    //     %t10 = const.u32 1
    //     %t8 = binop.add %t9, %t10 : u32
    //     store %t8 -> &a0 : u32
    //     br loop_header
    //
    // loop_after:
    //     %t0 <- load &a0 : u32
    //     ret %t0
    // }

    assert_eq!(ir_func.blocks.len(), 4);

    let entry_block = &ir_func.blocks[&IrBlockId(0)];
    let entry_insts = entry_block.insts();
    assert_eq!(entry_insts.len(), 3);

    let loop_header_block = &ir_func.blocks[&IrBlockId(1)];
    let loop_header_insts = loop_header_block.insts();
    assert_eq!(loop_header_insts.len(), 3);

    let loop_body_block = &ir_func.blocks[&IrBlockId(2)];
    let loop_body_insts = loop_body_block.insts();
    assert_eq!(loop_body_insts.len(), 4);

    let loop_after_block = &ir_func.blocks[&IrBlockId(3)];
    let loop_after_insts = loop_after_block.insts();
    assert_eq!(loop_after_insts.len(), 1);

    // entry block
    assert_alloc_var(&entry_insts[0], 0, "x", Type::UInt32);
    assert_const_u32(&entry_insts[1], 2, 0);
    assert_store_var(&entry_insts[2], 0, 2, Type::UInt32);
    assert_br_to(&entry_block.term(), loop_header_block.id());

    // loop header block
    assert_load_var(&loop_header_insts[0], 5, 0, Type::UInt32);
    assert_const_u32(&loop_header_insts[1], 6, 10);
    assert_binary_op(&loop_header_insts[2], 4, BinaryOp::Lt, 5, 6);
    assert_cond_br(
        &loop_header_block.term(),
        4,
        loop_body_block.id(),
        loop_after_block.id(),
    );

    // loop body block
    assert_load_var(&loop_body_insts[0], 9, 0, Type::UInt32);
    assert_const_u32(&loop_body_insts[1], 10, 1);
    assert_binary_op(&loop_body_insts[2], 8, BinaryOp::Add, 9, 10);
    assert_store_var(&loop_body_insts[3], 0, 8, Type::UInt32);
    assert_br_to(&loop_body_block.term(), loop_header_block.id());

    // loop after block
    assert_load_var(&loop_after_block.insts()[0], 0, 0, Type::UInt32);
    assert_ret_with(&loop_after_block.term(), 0);
}

#[test]
fn test_lower_func_with_params() {
    let source = r#"
        fn test(a: u32, b: u32) -> u32 {
            a + b
        }
    "#;

    let ir_func = compile_and_lower(source).expect("Failed to compile and lower");

    // Assert
    // if result is Err, print the error
    println!("{}", ir_func);

    // Output:
    // fn test(a: u32, b: u32) -> u32 {
    // entry:
    //     &a0 = alloc u32 (size=4, align=4) name_hint=a
    //     store param.0 -> &a0 : u32
    //     &a1 = alloc u32 (size=4, align=4) name_hint=b
    //     store param.1 -> &a1 : u32
    //     %t1 <- load &a0 : u32
    //     %t2 <- load &a1 : u32
    //     %t0 = binop.add %t1, %t2 : u32
    //     ret %t0
    // }

    assert_eq!(ir_func.blocks.len(), 1);

    let entry_block = &ir_func.blocks[&IrBlockId(0)];
    let entry_insts = entry_block.insts();
    assert_eq!(entry_insts.len(), 7);

    // entry block
    assert_alloc_var(&entry_insts[0], 0, "a", Type::UInt32);
    assert_store_param(&entry_insts[1], 0, 0, Type::UInt32);
    assert_alloc_var(&entry_insts[2], 1, "b", Type::UInt32);
    assert_store_param(&entry_insts[3], 1, 1, Type::UInt32);
    assert_load_var(&entry_insts[4], 1, 0, Type::UInt32);
    assert_load_var(&entry_insts[5], 2, 1, Type::UInt32);
    assert_binary_op(&entry_insts[6], 0, BinaryOp::Add, 1, 2);
    assert_ret_with(&entry_block.term(), 0);
}

mod ir_assert {
    use super::*;
    use crate::types::Type;

    pub fn assert_alloc_var(inst: &IrInst, addr_id: u32, name: &str, var_type: Type) {
        assert!(matches!(inst, IrInst::AllocVar { addr, typ, name_hint }
                if addr.id() == addr_id && *typ == var_type && name_hint == name));
    }

    pub fn assert_store_param(inst: &IrInst, addr_id: u32, index_expected: u32, var_type: Type) {
        assert!(matches!(inst, IrInst::StoreParam { addr, index, typ }
                if addr.id() == addr_id && *index == index_expected && *typ == var_type));
    }

    pub fn assert_store_var(inst: &IrInst, addr_id: u32, value_id: u32, var_type: Type) {
        assert!(matches!(inst, IrInst::StoreVar { addr, value, typ }
                if addr.id() == addr_id && value.id() == value_id && *typ == var_type));
    }

    pub fn assert_load_var(inst: &IrInst, value_id: u32, addr_id: u32, var_type: Type) {
        assert!(matches!(inst, IrInst::LoadVar { value, addr, typ }
                if value.id() == value_id && addr.id() == addr_id && *typ == var_type));
    }

    pub fn assert_const_u32(inst: &IrInst, result_id: u32, value: u32) {
        assert!(matches!(inst, IrInst::ConstU32 { result, value: v }
                if result.id() == result_id && *v == value));
    }

    pub fn assert_binary_op(
        inst: &IrInst,
        result_id: u32,
        op_expected: BinaryOp,
        lhs_id: u32,
        rhs_id: u32,
    ) {
        assert!(matches!(inst, IrInst::BinaryOp { result, op, lhs, rhs }
                if result.id() == result_id && *op == op_expected && lhs.id() == lhs_id && rhs.id() == rhs_id));
    }

    pub fn assert_br_to(term: &IrTerminator, target_block: IrBlockId) {
        assert!(matches!(term, IrTerminator::Br { target }
            if *target == target_block));
    }

    pub fn assert_cond_br(term: &IrTerminator, cond_id: u32, then_b: IrBlockId, else_b: IrBlockId) {
        assert!(
            matches!(term, IrTerminator::CondBr { cond, then_b: t, else_b: e }
                if cond.id() == cond_id && *t == then_b && *e == else_b)
        );
    }

    pub fn assert_phi(
        inst: &IrInst,
        result_id: u32,
        incoming_expected: Vec<(IrBlockId, IrTempId)>,
    ) {
        assert!(matches!(inst, IrInst::Phi { result, incoming }
                if result.id() == result_id && *incoming == incoming_expected));
    }

    pub fn assert_ret_with(term: &IrTerminator, result_id: u32) {
        assert!(matches!(term, IrTerminator::Ret { value: Some(result) }
                if result.id() == result_id));
    }
}
