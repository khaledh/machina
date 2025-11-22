use crate::ast::BinaryOp;
use crate::ir::builder::IrFunctionBuilder;
use crate::ir::types::{IrBlockId, IrTerminator};
use crate::regalloc::{Arm64Reg as R, MappedTemp, RegAlloc};
use crate::types::Type;

#[test]
fn test_regalloc_empty_function() {
    let mut b = IrFunctionBuilder::new("test".to_string(), vec![], Type::Unit);
    b.terminate(IrTerminator::Ret { value: None });
    let func = b.finish();

    let alloc_map = RegAlloc::new().alloc(&func);

    assert_eq!(alloc_map.len(), 0);
}

#[test]
fn test_regalloc_simple_function() {
    let mut b = IrFunctionBuilder::new("test".to_string(), vec![], Type::Unit);

    let t0 = b.new_temp(Type::UInt32);
    b.const_u32(t0, 1);

    b.terminate(IrTerminator::Ret { value: Some(t0) });
    let func = b.finish();

    let alloc_map = RegAlloc::new().alloc(&func);

    assert_eq!(alloc_map.len(), 1);
    assert!(matches!(alloc_map[&t0], MappedTemp::Reg(..)));
}

#[test]
fn test_regalloc_simple_function_with_two_temps() {
    let mut b = IrFunctionBuilder::new("test".to_string(), vec![], Type::Unit);

    let t0 = b.new_temp(Type::UInt32);
    b.const_u32(t0, 1);

    let t1 = b.new_temp(Type::UInt32);
    b.const_u32(t1, 2);

    b.terminate(IrTerminator::Ret { value: Some(t0) });
    let func = b.finish();

    let alloc_map = RegAlloc::new().alloc_into(&func, vec![R::X0, R::X1]);

    assert_eq!(alloc_map.len(), 2);
    assert!(matches!(alloc_map[&t0], MappedTemp::Reg(R::X0)));
    assert!(matches!(alloc_map[&t1], MappedTemp::Reg(R::X0)));
}

#[test]
fn test_regalloc_overlapping_temps_use_different_regs() {
    // Build:
    //
    // entry:
    //   %t0 = const.u32 1       // live at position 0
    //   %t1 = binop.add t0, t0  // uses t0; t0 and t1 overlap
    //   ret %t1
    let mut b = IrFunctionBuilder::new("test".to_string(), vec![], Type::Unit);

    let t0 = b.new_temp(Type::UInt32);
    b.const_u32(t0, 1);

    let t1 = b.new_temp(Type::UInt32);
    // This ensures t0 is still live when t1 is defined.
    b.binary_op(t1, crate::ast::BinaryOp::Add, t0, t0);

    b.terminate(IrTerminator::Ret { value: Some(t1) });
    let func = b.finish();

    let alloc_map = RegAlloc::new().alloc_into(&func, vec![R::X0, R::X1]);

    assert_eq!(alloc_map.len(), 2);
    // Overlapping temps must not share a register.
    let r0 = &alloc_map[&t0];
    let r1 = &alloc_map[&t1];
    assert!(matches!(r0, MappedTemp::Reg(_)));
    assert!(matches!(r1, MappedTemp::Reg(_)));
    if let (MappedTemp::Reg(r0), MappedTemp::Reg(r1)) = (r0, r1) {
        assert_ne!(r0, r1);
    }
}

#[test]
fn test_regalloc_reuses_single_register_without_spilling() {
    // Build a function with three independent temps but only one allocatable
    // register; since their lifetimes do not overlap, all of them should fit
    // in that single register without any spills.
    let mut b = IrFunctionBuilder::new("test".to_string(), vec![], Type::Unit);

    let t0 = b.new_temp(Type::UInt32);
    b.const_u32(t0, 1);

    let t1 = b.new_temp(Type::UInt32);
    b.const_u32(t1, 2);

    let t2 = b.new_temp(Type::UInt32);
    b.const_u32(t2, 3);

    b.terminate(IrTerminator::Ret { value: Some(t2) });
    let func = b.finish();

    // Only X0 is available; the allocator should just keep reusing it.
    let alloc_map = RegAlloc::new().alloc_into(&func, vec![R::X0]);

    assert_eq!(alloc_map.len(), 3);
    // No temp should be in a stack slot.
    let stack_count = alloc_map
        .values()
        .filter(|m| matches!(m, MappedTemp::Stack(..)))
        .count();
    assert_eq!(stack_count, 0);
}

#[test]
fn test_regalloc_default_reg_set_is_used() {
    // Smoke test for RegAlloc::alloc using its built-in register set.
    let mut b = IrFunctionBuilder::new("test".to_string(), vec![], Type::Unit);

    let t0 = b.new_temp(Type::UInt32);
    b.const_u32(t0, 1);

    let t1 = b.new_temp(Type::UInt32);
    b.const_u32(t1, 2);

    b.terminate(IrTerminator::Ret { value: Some(t1) });
    let func = b.finish();

    let alloc_map = RegAlloc::new().alloc(&func);

    // Both temps should reside in registers drawn from the default pool.
    assert_eq!(alloc_map.len(), 2);
    assert!(matches!(alloc_map[&t0], MappedTemp::Reg(..)));
    assert!(matches!(alloc_map[&t1], MappedTemp::Reg(..)));
}

#[test]
fn test_regalloc_spills_multiple_temps_with_one_reg() {
    // Build a function where more temps are simultaneously live than the
    // number of available registers (one), forcing some temps to spill.
    //
    // entry:
    //   %t0 = const.u32 1
    //   %t1 = const.u32 2
    //   %t2 = const.u32 3
    //   %t3 = binop.add t0, t1
    //   %t4 = binop.add t3, t2
    //   ret %t4
    let mut b = IrFunctionBuilder::new("spill_many".to_string(), vec![], Type::Unit);

    let t0 = b.new_temp(Type::UInt32);
    b.const_u32(t0, 1);

    let t1 = b.new_temp(Type::UInt32);
    b.const_u32(t1, 2);

    let t2 = b.new_temp(Type::UInt32);
    b.const_u32(t2, 3);

    let t3 = b.new_temp(Type::UInt32);
    b.binary_op(t3, crate::ast::BinaryOp::Add, t0, t1);

    let t4 = b.new_temp(Type::UInt32);
    b.binary_op(t4, crate::ast::BinaryOp::Add, t3, t2);

    b.terminate(IrTerminator::Ret { value: Some(t4) });
    let func = b.finish();

    // Only a single allocatable register, so some of the overlapping temps
    // must be mapped to stack slots.
    let alloc_map = RegAlloc::new().alloc_into(&func, vec![R::X0]);

    assert_eq!(alloc_map.len(), 5);

    // With only one register and several overlapping live ranges we must see
    // at least one spill to the stack.
    let stack_count = alloc_map
        .values()
        .filter(|m| matches!(m, MappedTemp::Stack(..)))
        .count();
    let reg_count = alloc_map
        .values()
        .filter(|m| matches!(m, MappedTemp::Reg(..)))
        .count();

    assert!(stack_count >= 1);
    assert!(reg_count >= 1);
}

#[test]
fn test_regalloc_spills_victim_when_new_interval_ends_earlier() {
    // Build a shape that exercises the victim-spill branch of the allocator:
    //
    // entry:
    //   %t0 = const.u32 1          // long-lived
    //   %t1 = const.u32 2          // shorter-lived
    //   %t2 = binop.add t1, t1     // keeps t1 live briefly
    //   %t3 = binop.add t0, t2     // uses t0 later, extending its lifetime
    //   ret %t3
    //
    // With only one register:
    // - t0 is allocated first, then spilled when t1 (shorter interval) arrives
    //   (exercising the "spill victim, keep current in reg" path).
    // - Later temps force additional spills, but t1 and t3 end up in a reg.
    let mut b = IrFunctionBuilder::new("spill_victim".to_string(), vec![], Type::Unit);

    let t0 = b.new_temp(Type::UInt32);
    b.const_u32(t0, 1);

    let t1 = b.new_temp(Type::UInt32);
    b.const_u32(t1, 2);

    let t2 = b.new_temp(Type::UInt32);
    b.binary_op(t2, crate::ast::BinaryOp::Add, t1, t1);

    let t3 = b.new_temp(Type::UInt32);
    b.binary_op(t3, crate::ast::BinaryOp::Add, t0, t2);

    b.terminate(IrTerminator::Ret { value: Some(t3) });
    let func = b.finish();

    let alloc_map = RegAlloc::new().alloc_into(&func, vec![R::X0]);

    assert_eq!(alloc_map.len(), 4);

    // This pattern should also require at least one spill when only a single
    // register is available.
    let stack_count = alloc_map
        .values()
        .filter(|m| matches!(m, MappedTemp::Stack(..)))
        .count();
    let reg_count = alloc_map
        .values()
        .filter(|m| matches!(m, MappedTemp::Reg(..)))
        .count();

    assert!(stack_count >= 1);
    assert!(reg_count >= 1);
}

#[test]
fn test_regalloc_if_expression_shape() {
    // Model something like:
    //
    //   if cond { then_val } else { else_val }
    //
    // as:
    //
    // entry:
    //   %t0 = const.bool true
    //   condbr %t0, then, else
    //
    // then:
    //   %t1 = const.u32 1
    //   br join
    //
    // else:
    //   %t2 = const.u32 2
    //   br join
    //
    // join:
    //   %t3 = phi [(entry, t1), (else, t2)]
    //   ret %t3
    let mut b = IrFunctionBuilder::new("if_expr".to_string(), vec![], Type::Unit);

    // entry
    let cond = b.new_temp(Type::Bool);
    b.const_bool(cond, true);

    let then_id = b.new_block("then".to_string());
    let else_id = b.new_block("else".to_string());
    let join_id = b.new_block("join".to_string());

    b.terminate(IrTerminator::CondBr {
        cond,
        then_b: then_id,
        else_b: else_id,
    });

    // then block
    b.select_block(then_id);
    let t_then = b.new_temp(Type::UInt32);
    b.const_u32(t_then, 1);
    b.terminate(IrTerminator::Br { target: join_id });

    // else block
    b.select_block(else_id);
    let t_else = b.new_temp(Type::UInt32);
    b.const_u32(t_else, 2);
    b.terminate(IrTerminator::Br { target: join_id });

    // join block
    b.select_block(join_id);
    let t_phi = b.new_temp(Type::UInt32);
    b.phi(t_phi, vec![(then_id, t_then), (else_id, t_else)]);
    b.terminate(IrTerminator::Ret { value: Some(t_phi) });

    let func = b.finish();

    let alloc_map = RegAlloc::new().alloc_into(&func, vec![R::X0, R::X1]);

    // With two registers, current behaviour is:
    // - `cond` and the then-branch value `t_then` share X0 (their lifetimes don't overlap),
    // - the else-branch value `t_else` uses X1,
    // - the merged phi result `t_phi` is spilled to the stack.
    assert!(matches!(alloc_map[&cond], MappedTemp::Reg(R::X0)));
    assert!(matches!(alloc_map[&t_then], MappedTemp::Reg(R::X0)));
    assert!(matches!(alloc_map[&t_else], MappedTemp::Reg(R::X1)));
    assert!(matches!(alloc_map[&t_phi], MappedTemp::Stack(..)));
}

#[test]
fn test_regalloc_while_expression_shape() {
    // Model something like:
    //
    //   var i = 0;
    //   while i < 3 {
    //       i = i + 1;
    //   }
    //   i
    //
    // as:
    //
    // entry:
    //   %i0 = const.u32 0
    //   br loop_header
    //
    // loop_header:
    //   %i = phi [(entry, i0), (loop_body, i_next)]
    //   %c = binop.lt i, 3
    //   condbr %c, loop_body, exit
    //
    // loop_body:
    //   %one = const.u32 1
    //   %i_next = binop.add i, one
    //   br loop_header
    //
    // exit:
    //   ret %i
    let mut b = IrFunctionBuilder::new("while_expr".to_string(), vec![], Type::Unit);

    // entry
    let i0 = b.new_temp(Type::UInt32);
    b.const_u32(i0, 0);
    let loop_header = b.new_block("loop_header".to_string());
    b.terminate(IrTerminator::Br {
        target: loop_header,
    });

    // loop_header
    b.select_block(loop_header);
    let loop_body = b.new_block("loop_body".to_string());
    let exit = b.new_block("exit".to_string());

    // Temps representing the loop-carried value on entry/after body.
    let i = b.new_temp(Type::UInt32); // value visible in the header / exit
    let i_next = b.new_temp(Type::UInt32); // value produced by the body

    // Note: predecessor blocks are (entry, loop_body)
    b.phi(i, vec![(IrBlockId(0), i0), (loop_body, i_next)]);

    let three = b.new_temp(Type::UInt32);
    b.const_u32(three, 3);

    let cond = b.new_temp(Type::Bool);
    b.binary_op(cond, BinaryOp::Lt, i, three);

    b.terminate(IrTerminator::CondBr {
        cond,
        then_b: loop_body,
        else_b: exit,
    });

    // loop_body
    b.select_block(loop_body);
    let one = b.new_temp(Type::UInt32);
    b.const_u32(one, 1);

    b.binary_op(i_next, BinaryOp::Add, i, one);

    b.terminate(IrTerminator::Br {
        target: loop_header,
    });

    // exit
    b.select_block(exit);
    b.terminate(IrTerminator::Ret { value: Some(i) });

    let func = b.finish();

    let alloc_map = RegAlloc::new().alloc_into(&func, vec![R::X0, R::X1]);

    // With two registers, current behaviour is:
    // - `i0` and `three` reside in X0,
    // - `cond` and `one` reside in X1,
    // - the loop-carried header value `i` and body result `i_next` are spilled.
    assert!(matches!(alloc_map[&i0], MappedTemp::Reg(R::X0)));
    assert!(matches!(alloc_map[&three], MappedTemp::Reg(R::X0)));
    assert!(matches!(alloc_map[&cond], MappedTemp::Reg(R::X1)));
    assert!(matches!(alloc_map[&one], MappedTemp::Reg(R::X1)));
    assert!(matches!(alloc_map[&i], MappedTemp::Stack(..)));
    assert!(matches!(alloc_map[&i_next], MappedTemp::Stack(..)));
}
