use indoc::indoc;
use textwrap::indent;

use crate::codegen::arm64::FuncCodegen;
use crate::ir::builder::IrFunctionBuilder;
use crate::ir::types::{IrFunction, IrTerminator, IrType};
use crate::regalloc::Arm64Reg as R;
use crate::regalloc::alloc::AllocationResult;
use crate::regalloc::moves::FnMoveList;
use crate::regalloc::spill::StackSlotId;
use std::collections::HashMap;

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
    spill_slot_count: u32,
) -> AllocationResult {
    AllocationResult {
        alloc_map: HashMap::new(),
        moves: FnMoveList::new(),
        frame_size,
        used_callee_saved,
        spill_slot_count,
    }
}

#[test]
fn test_prologue_no_frame() {
    let func = create_test_function("test");
    let alloc_result = create_alloc_result(0, vec![], 0);
    let mut codegen = FuncCodegen::new(&func, &alloc_result);

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
    let mut codegen = FuncCodegen::new(&func, &alloc_result);

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
    let mut codegen = FuncCodegen::new(&func, &alloc_result);

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
    let mut codegen = FuncCodegen::new(&func, &alloc_result);

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
    let mut codegen = FuncCodegen::new(&func, &alloc_result);

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
    let codegen = FuncCodegen::new(&func, &alloc_result);

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
    let mut codegen = FuncCodegen::new(&func, &alloc_result);

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
