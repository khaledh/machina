use indoc::indoc;
use textwrap::indent;

use crate::ast::BinaryOp;
use crate::codegen::arm64::FuncCodegen;
use crate::ir::types::IrFunction;
use crate::regalloc::alloc::{AllocationResult, RegAlloc};
use crate::regalloc::constraints::analyze_constraints;
use crate::regalloc::moves::FnMoveList;
use crate::regalloc::regs::Arm64Reg as R;
use crate::regalloc::stack::StackSlotId;
use std::collections::HashMap;

include!("ir_test_utils.rs");

// Helper to create a minimal IrFunction for testing
fn create_test_function(name: &str) -> IrFunction {
    let mut b = IrFunctionBuilder::new(name.to_string(), IrType::Unit);
    b.terminate(IrTerminator::Ret { value: None });
    b.finish()
}

// Helper to create AllocationResult with specific config
fn create_alloc_result(
    frame_size: u32,
    used_callee_saved: Vec<R>,
    stack_slot_count: u32,
) -> AllocationResult {
    AllocationResult {
        alloc_map: HashMap::new(),
        moves: FnMoveList::new(),
        frame_size,
        used_callee_saved,
        stack_slot_count,
    }
}

#[test]
fn test_prologue_no_frame() {
    let func = create_test_function("test");
    let alloc_result = create_alloc_result(0, vec![], 0);
    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);

    let prologue = codegen.emit_prologue().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
    "};
    assert_eq!(prologue, expected);
}

#[test]
fn test_epilogue_no_frame() {
    let func = create_test_function("test");
    let alloc_result = create_alloc_result(0, vec![], 0);
    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);

    let epilogue = codegen.emit_epilogue().unwrap();

    let expected = indoc! {"
      ldp x29, x30, [sp], #16
      ret
    "};
    let expected = indent(expected, "  ");
    assert_eq!(epilogue, expected);
}

#[test]
fn test_prologue_two_callee_saved() {
    let func = create_test_function("test");
    let alloc_result = create_alloc_result(16, vec![R::X19, R::X20], 0);
    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);

    let prologue = codegen.emit_prologue().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        sub sp, sp, #16
        stp x19, x20, [sp, #0]
    "};
    assert_eq!(prologue, expected);
}

#[test]
fn test_epilogue_two_callee_saved() {
    let func = create_test_function("test");
    let alloc_result = create_alloc_result(16, vec![R::X19, R::X20], 0);
    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);

    let epilogue = codegen.emit_epilogue().unwrap();

    let expected = indoc! {"
      ldp x19, x20, [sp, #0]
      add sp, sp, #16
      ldp x29, x30, [sp], #16
      ret
    "};
    let expected = indent(expected, "  ");
    assert_eq!(epilogue, expected);
}

#[test]
fn test_prologue_three_callee_saved() {
    let func = create_test_function("test");
    let alloc_result = create_alloc_result(24, vec![R::X19, R::X20, R::X21], 0);
    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);

    let prologue = codegen.emit_prologue().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        sub sp, sp, #24
        stp x19, x20, [sp, #8]
        str x21, [sp, #0]
    "};
    assert_eq!(prologue, expected);
}

#[test]
fn test_get_stack_offset() {
    let func = create_test_function("test");
    // frame_size = 32, callee_saved = 16 bytes (2 regs), spilled = 16 bytes (2 slots)
    let alloc_result = create_alloc_result(32, vec![R::X19, R::X20], 2);
    let codegen = FuncCodegen::new(&func, &alloc_result, 0);

    // spilled_size = 32 - 16 = 16
    // Slot 0: offset = 16 - 0 - 8 = 8
    let offset0 = codegen.get_stack_offset(&StackSlotId(0)).unwrap();
    assert_eq!(offset0, 8);

    // Slot 1: offset = 16 - 8 - 8 = 0
    let offset1 = codegen.get_stack_offset(&StackSlotId(1)).unwrap();
    assert_eq!(offset1, 0);
}

#[test]
fn test_mixed_callee_saved_and_spills() {
    let func = create_test_function("test");
    // frame_size = 32, callee_saved = 16 bytes, spills = 16 bytes
    let alloc_result = create_alloc_result(32, vec![R::X19, R::X20], 2);
    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);

    let prologue = codegen.emit_prologue().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        sub sp, sp, #32
        stp x19, x20, [sp, #16]
    "};
    assert_eq!(prologue, expected);
}

#[test]
fn test_simple_addition() {
    // fn add_five() -> u32 {
    //   5 + 10
    // }
    let mut builder = mk_builder();

    let five = builder.new_temp(u64_ty());
    let ten = builder.new_temp(u64_ty());
    let result = builder.new_temp(u64_ty());

    builder.move_to(five, const_u64(5));
    builder.move_to(ten, const_u64(10));
    builder.binary_op(result, BinaryOp::Add, temp_operand(five), temp_operand(ten));
    builder.terminate(IrTerminator::Ret {
        value: Some(temp_operand(result)),
    });

    let func = builder.finish();

    // Run register allocation
    let constraints = analyze_constraints(&func);
    let allocator = RegAlloc::new(&func, &constraints);
    let alloc_result = allocator.alloc();

    // Generate code
    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);
    let asm = codegen.generate().unwrap();

    // Basic sanity checks
    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        mov x0, #5
        mov x1, #10
        add x2, x0, x1
        mov x0, x2
        ldp x29, x30, [sp], #16
        ret
    "};
    assert_eq!(asm, expected);
}

#[test]
fn test_function_call() {
    // fn caller() -> u32 {
    //   callee(5, 10)
    // }
    let mut builder = mk_builder();

    let arg1 = builder.new_temp(u64_ty());
    let arg2 = builder.new_temp(u64_ty());
    let result = builder.new_temp(u64_ty());

    builder.move_to(arg1, const_u64(5));
    builder.move_to(arg2, const_u64(10));
    builder.call(
        Some(result),
        "callee".to_string(),
        vec![temp_operand(arg1), temp_operand(arg2)],
        u64_ty(),
    );
    builder.terminate(IrTerminator::Ret {
        value: Some(temp_operand(result)),
    });

    let func = builder.finish();

    // Run register allocation
    let constraints = analyze_constraints(&func);
    let allocator = RegAlloc::new(&func, &constraints);
    let alloc_result = allocator.alloc();

    // Generate code
    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);
    let asm = codegen.generate().unwrap();

    // Expected: args moved to x0/x1, call, result already in x0
    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        mov x0, #5
        mov x1, #10
        bl _callee
        mov x2, x0
        mov x0, x2
        ldp x29, x30, [sp], #16
        ret
    "};
    assert_eq!(asm, expected);
}

// Array element access tests

fn array_u64_ty(len: usize) -> IrType {
    IrType::Array {
        elem_ty: Box::new(u64_ty()),
        len,
    }
}

fn array_u32_ty(len: usize) -> IrType {
    IrType::Array {
        elem_ty: Box::new(IrType::Int {
            bits: 32,
            signed: false,
        }),
        len,
    }
}

fn array_u16_ty(len: usize) -> IrType {
    IrType::Array {
        elem_ty: Box::new(IrType::Int {
            bits: 16,
            signed: false,
        }),
        len,
    }
}

fn array_u8_ty(len: usize) -> IrType {
    IrType::Array {
        elem_ty: Box::new(IrType::Int {
            bits: 8,
            signed: false,
        }),
        len,
    }
}

#[test]
fn test_store_element_const_index() {
    // fn test(arr: u64[10]) {
    //   arr[2] = 42
    // }
    let mut builder = IrFunctionBuilder::new("test".to_string(), unit_ty());
    let array_param = builder.new_param(0, "arr".to_string(), array_u64_ty(10));
    let value = const_u64(42);

    builder.store_element(array_param, const_u64(2), value);
    builder.terminate(IrTerminator::Ret { value: None });

    let func = builder.finish();
    let constraints = analyze_constraints(&func);
    let allocator = RegAlloc::new(&func, &constraints);
    let alloc_result = allocator.alloc();

    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);
    let asm = codegen.generate().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        add x17, x0, #16
        mov x16, #42
        str x16, [x17]
        ldp x29, x30, [sp], #16
        ret
    "};
    assert_eq!(asm, expected);
}

#[test]
fn test_load_element_const_index() {
    // fn test(arr: u64[10]) -> u64 {
    //   arr[3]
    // }
    let mut builder = IrFunctionBuilder::new("test".to_string(), u64_ty());
    let array_param = builder.new_param(0, "arr".to_string(), array_u64_ty(10));
    let result = builder.new_temp(u64_ty());

    builder.load_element(result, array_param, const_u64(3));
    builder.terminate(IrTerminator::Ret {
        value: Some(temp_operand(result)),
    });

    let func = builder.finish();
    let constraints = analyze_constraints(&func);
    let allocator = RegAlloc::new(&func, &constraints);
    let alloc_result = allocator.alloc();

    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);
    let asm = codegen.generate().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        add x17, x0, #24
        ldr x1, [x17]
        mov x0, x1
        ldp x29, x30, [sp], #16
        ret
    "};
    assert_eq!(asm, expected);
}

#[test]
fn test_store_element_variable_index() {
    // fn test(arr: u64[10], idx: u64) {
    //   arr[idx] = 99
    // }
    let mut builder = IrFunctionBuilder::new("test".to_string(), unit_ty());
    let array_param = builder.new_param(0, "arr".to_string(), array_u64_ty(10));
    let index_param = builder.new_param(1, "idx".to_string(), u64_ty());
    let value = const_u64(99);

    builder.store_element(array_param, temp_operand(index_param), value);
    builder.terminate(IrTerminator::Ret { value: None });

    let func = builder.finish();
    let constraints = analyze_constraints(&func);
    let allocator = RegAlloc::new(&func, &constraints);
    let alloc_result = allocator.alloc();

    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);
    let asm = codegen.generate().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        mov x16, #8
        mul x17, x1, x16
        add x17, x0, x17
        mov x16, #99
        str x16, [x17]
        ldp x29, x30, [sp], #16
        ret
    "};
    assert_eq!(asm, expected);
}

#[test]
fn test_load_element_variable_index() {
    // fn test(arr: u64[10], idx: u64) -> u64 {
    //   arr[idx]
    // }
    let mut builder = IrFunctionBuilder::new("test".to_string(), u64_ty());
    let array_param = builder.new_param(0, "arr".to_string(), array_u64_ty(10));
    let index_param = builder.new_param(1, "idx".to_string(), u64_ty());
    let result = builder.new_temp(u64_ty());

    builder.load_element(result, array_param, temp_operand(index_param));
    builder.terminate(IrTerminator::Ret {
        value: Some(temp_operand(result)),
    });

    let func = builder.finish();
    let constraints = analyze_constraints(&func);
    let allocator = RegAlloc::new(&func, &constraints);
    let alloc_result = allocator.alloc();

    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);
    let asm = codegen.generate().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        mov x16, #8
        mul x17, x1, x16
        add x17, x0, x17
        ldr x2, [x17]
        mov x0, x2
        ldp x29, x30, [sp], #16
        ret
    "};
    assert_eq!(asm, expected);
}

#[test]
fn test_store_element_different_sizes() {
    // Test u32 array (4-byte elements)
    let mut builder = IrFunctionBuilder::new("test".to_string(), unit_ty());
    let array_param = builder.new_param(0, "arr".to_string(), array_u32_ty(10));
    builder.store_element(
        array_param,
        const_u64(1),
        IrOperand::Const(IrConst::Int {
            value: 123,
            bits: 32,
            signed: false,
        }),
    );
    builder.terminate(IrTerminator::Ret { value: None });

    let func = builder.finish();
    let constraints = analyze_constraints(&func);
    let allocator = RegAlloc::new(&func, &constraints);
    let alloc_result = allocator.alloc();

    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);
    let asm = codegen.generate().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        add x17, x0, #4
        mov x16, #123
        str w16, [x17]
        ldp x29, x30, [sp], #16
        ret
    "};
    assert_eq!(asm, expected);
}

#[test]
fn test_load_element_halfword_array() {
    // Test u16 array (2-byte elements)
    let mut builder = IrFunctionBuilder::new(
        "test".to_string(),
        IrType::Int {
            bits: 16,
            signed: false,
        },
    );
    let array_param = builder.new_param(0, "arr".to_string(), array_u16_ty(10));
    let result = builder.new_temp(IrType::Int {
        bits: 16,
        signed: false,
    });

    builder.load_element(result, array_param, const_u64(3));
    builder.terminate(IrTerminator::Ret {
        value: Some(temp_operand(result)),
    });

    let func = builder.finish();
    let constraints = analyze_constraints(&func);
    let allocator = RegAlloc::new(&func, &constraints);
    let alloc_result = allocator.alloc();

    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);
    let asm = codegen.generate().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        add x17, x0, #6
        ldrh w1, [x17]
        mov x0, x1
        ldp x29, x30, [sp], #16
        ret
    "};
    assert_eq!(asm, expected);
}

#[test]
fn test_load_element_byte_array() {
    // Test u8 array (1-byte elements)
    let mut builder = IrFunctionBuilder::new(
        "test".to_string(),
        IrType::Int {
            bits: 8,
            signed: false,
        },
    );
    let array_param = builder.new_param(0, "arr".to_string(), array_u8_ty(10));
    let result = builder.new_temp(IrType::Int {
        bits: 8,
        signed: false,
    });

    builder.load_element(result, array_param, const_u64(5));
    builder.terminate(IrTerminator::Ret {
        value: Some(temp_operand(result)),
    });

    let func = builder.finish();
    let constraints = analyze_constraints(&func);
    let allocator = RegAlloc::new(&func, &constraints);
    let alloc_result = allocator.alloc();

    let mut codegen = FuncCodegen::new(&func, &alloc_result, 0);
    let asm = codegen.generate().unwrap();

    let expected = indoc! {"
      .global _test
      _test:
        stp x29, x30, [sp, #-16]!
        mov x29, sp
        add x17, x0, #5
        ldrb w1, [x17]
        mov x0, x1
        ldp x29, x30, [sp], #16
        ret
    "};
    assert_eq!(asm, expected);
}
