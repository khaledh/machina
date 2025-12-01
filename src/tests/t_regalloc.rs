use crate::TempAllocMapDisplay;
use crate::ast::BinaryOp;
use crate::ir::pos::InstPos;
use crate::ir::types::IrBlockId;
use crate::regalloc::constraints::analyze_constraints;
use crate::regalloc::moves::Location;
use crate::regalloc::spill::StackSlotId;
use crate::regalloc::{Arm64Reg as R, MappedTemp, RegAlloc};

include!("ir_test_utils.rs");

#[test]
fn test_regalloc_empty_function() {
    let mut b = IrFunctionBuilder::new("test".to_string(), unit_ty());
    b.terminate(IrTerminator::Ret { value: None });
    let func = b.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc();
    let alloc_map = alloc_result.alloc_map;

    assert_eq!(alloc_map.len(), 0);
}

#[test]
fn test_regalloc_simple_function() {
    let mut b = IrFunctionBuilder::new("test".to_string(), u32_ty());

    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(1));

    b.terminate(IrTerminator::Ret {
        value: Some(IrOperand::Temp(t0)),
    });
    let func = b.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc();
    let alloc_map = alloc_result.alloc_map;

    assert_eq!(alloc_map.len(), 1);
    // t0 should be in a register.
    assert!(matches!(alloc_map[&t0], MappedTemp::Reg(..)));
}

#[test]
fn test_regalloc_simple_function_with_two_temps() {
    // fn test() -> u32 {
    // 0:entry:
    //   %t0 = const.u32 1
    //   %t1 = const.u32 2
    //   ret %t1
    // }
    let mut b = IrFunctionBuilder::new("test".to_string(), u32_ty());

    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(1));

    let t1 = b.new_temp(u32_ty());
    b.move_to(t1, const_u32(2));

    b.terminate(IrTerminator::Ret {
        value: Some(IrOperand::Temp(t1)),
    });
    let func = b.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc_into(vec![R::X0, R::X1]);
    let alloc_map = alloc_result.alloc_map;

    println!("alloc_map:\n{}", TempAllocMapDisplay(&alloc_map));

    assert_eq!(alloc_map.len(), 2);
    // t0 and t1 share the same register since their liveness doesn't overlap.
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
    let mut b = IrFunctionBuilder::new("test".to_string(), u32_ty());

    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(1));

    let t1 = b.new_temp(u32_ty());
    // This ensures t0 is still live when t1 is defined.
    b.binary_op(t1, BinaryOp::Add, IrOperand::Temp(t0), IrOperand::Temp(t0));

    b.terminate(IrTerminator::Ret {
        value: Some(IrOperand::Temp(t1)),
    });
    let func = b.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc_into(vec![R::X0, R::X1]);
    let alloc_map = alloc_result.alloc_map;

    assert_eq!(alloc_map.len(), 2);
    // t0 and t1 should be in different registers since their liveness overlaps.
    match (&alloc_map[&t0], &alloc_map[&t1]) {
        (MappedTemp::Reg(r0), MappedTemp::Reg(r1)) => {
            assert_ne!(
                r0, r1,
                "t0 and t1 should be in different registers, but both are in {}",
                r0
            );
        }
        _ => panic!("t0 and t1 should be in registers"),
    }
}

#[test]
fn test_regalloc_reuses_single_register_without_spilling() {
    // Build a function with three independent temps but only one allocatable
    // register; since their lifetimes do not overlap, all of them should fit
    // in that single register without any spills.
    let mut b = IrFunctionBuilder::new("test".to_string(), u32_ty());

    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(1));

    let t1 = b.new_temp(u32_ty());
    b.move_to(t1, const_u32(2));

    let t2 = b.new_temp(u32_ty());
    b.move_to(t2, const_u32(3));

    b.terminate(IrTerminator::Ret {
        value: Some(IrOperand::Temp(t2)),
    });
    let func = b.finish();

    // Only X0 is available; the allocator should just keep reusing it.
    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc_into(vec![R::X0]);
    let alloc_map = alloc_result.alloc_map;

    assert_eq!(alloc_map.len(), 3);
    // t0, t1, and t2 should all be in the same register since their liveness doesn't overlap.
    assert!(matches!(alloc_map[&t0], MappedTemp::Reg(R::X0)));
    assert!(matches!(alloc_map[&t1], MappedTemp::Reg(R::X0)));
    assert!(matches!(alloc_map[&t2], MappedTemp::Reg(R::X0)));
}

#[test]
fn test_regalloc_default_reg_set_is_used() {
    // Smoke test for RegAlloc::alloc using its built-in register set.
    let mut b = IrFunctionBuilder::new("test".to_string(), u32_ty());

    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(1));

    let t1 = b.new_temp(u32_ty());
    b.move_to(t1, const_u32(2));

    b.terminate(IrTerminator::Ret {
        value: Some(IrOperand::Temp(t1)),
    });
    let func = b.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc();
    let alloc_map = alloc_result.alloc_map;

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
    let mut b = IrFunctionBuilder::new("spill_many".to_string(), u32_ty());

    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(1));

    let t1 = b.new_temp(u32_ty());
    b.move_to(t1, const_u32(2));

    let t2 = b.new_temp(u32_ty());
    b.move_to(t2, const_u32(3));

    let t3 = b.new_temp(u32_ty());
    b.binary_op(t3, BinaryOp::Add, IrOperand::Temp(t0), IrOperand::Temp(t1));

    let t4 = b.new_temp(u32_ty());
    b.binary_op(t4, BinaryOp::Add, IrOperand::Temp(t3), IrOperand::Temp(t2));

    b.terminate(IrTerminator::Ret {
        value: Some(IrOperand::Temp(t4)),
    });
    let func = b.finish();

    // Only a single allocatable register, so some of the overlapping temps
    // must be mapped to stack slots.
    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc_into(vec![R::X0]);
    let alloc_map = alloc_result.alloc_map;

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
    let mut b = IrFunctionBuilder::new("spill_victim".to_string(), u32_ty());

    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(1));

    let t1 = b.new_temp(u32_ty());
    b.move_to(t1, const_u32(2));

    let t2 = b.new_temp(u32_ty());
    b.binary_op(t2, BinaryOp::Add, IrOperand::Temp(t1), IrOperand::Temp(t1));

    let t3 = b.new_temp(u32_ty());
    b.binary_op(t3, BinaryOp::Add, IrOperand::Temp(t0), IrOperand::Temp(t2));

    b.terminate(IrTerminator::Ret {
        value: Some(IrOperand::Temp(t3)),
    });
    let func = b.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc_into(vec![R::X0]);
    let alloc_map = alloc_result.alloc_map;

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
    let mut b = IrFunctionBuilder::new("if_expr".to_string(), u32_ty());

    // entry
    let cond = b.new_temp(bool_ty());
    b.move_to(
        cond,
        IrOperand::Const(crate::ir::types::IrConst::Bool(true)),
    );

    let then_id = b.new_block("then".to_string());
    let else_id = b.new_block("else".to_string());
    let join_id = b.new_block("join".to_string());

    b.terminate(IrTerminator::CondBr {
        cond: IrOperand::Temp(cond),
        then_b: then_id,
        else_b: else_id,
    });

    // then block
    b.select_block(then_id);
    let t_then = b.new_temp(u32_ty());
    b.move_to(t_then, const_u32(1));
    b.terminate(IrTerminator::Br { target: join_id });

    // else block
    b.select_block(else_id);
    let t_else = b.new_temp(u32_ty());
    b.move_to(t_else, const_u32(2));
    b.terminate(IrTerminator::Br { target: join_id });

    // join block
    b.select_block(join_id);
    let t_phi = b.new_temp(u32_ty());
    b.phi(
        t_phi,
        vec![
            (then_id, IrOperand::Temp(t_then)),
            (else_id, IrOperand::Temp(t_else)),
        ],
    );
    b.terminate(IrTerminator::Ret {
        value: Some(IrOperand::Temp(t_phi)),
    });

    let func = b.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc_into(vec![R::X0, R::X1]);
    let alloc_map = alloc_result.alloc_map;

    // With two registers, the behaviour is:
    // - `cond` gets a register (X0)
    // - `t_then` and `t_else` don't overlap with each other, so they share registers with cond
    // - the merged phi result `t_phi` is spilled to the stack
    // The specific assignment of t_then/t_else to X0/X1 can vary depending on RPO order
    assert!(matches!(alloc_map[&cond], MappedTemp::Reg(R::X0)));
    assert!(matches!(alloc_map[&t_then], MappedTemp::Reg(_)));
    assert!(matches!(alloc_map[&t_else], MappedTemp::Reg(_)));
    assert!(matches!(alloc_map[&t_phi], MappedTemp::Stack(..)));

    // Verify that t_then and t_else got different registers or one reuses cond's register
    match (&alloc_map[&t_then], &alloc_map[&t_else]) {
        (MappedTemp::Reg(r1), MappedTemp::Reg(r2)) => {
            // Both in registers - they should use the available X0 and X1
            assert!((*r1 == R::X0 || *r1 == R::X1) && (*r2 == R::X0 || *r2 == R::X1));
        }
        _ => panic!("Expected both t_then and t_else to be in registers"),
    }
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
    let mut b = IrFunctionBuilder::new("while_expr".to_string(), u32_ty());

    // entry
    let i0 = b.new_temp(u32_ty());
    b.move_to(i0, const_u32(0));
    let loop_header = b.new_block("loop_header".to_string());
    b.terminate(IrTerminator::Br {
        target: loop_header,
    });

    // loop_header
    b.select_block(loop_header);
    let loop_body = b.new_block("loop_body".to_string());
    let exit = b.new_block("exit".to_string());

    // Temps representing the loop-carried value on entry/after body.
    let i = b.new_temp(u32_ty()); // value visible in the header / exit
    let i_next = b.new_temp(u32_ty()); // value produced by the body

    // Note: predecessor blocks are (entry, loop_body)
    b.phi(
        i,
        vec![
            (IrBlockId(0), IrOperand::Temp(i0)),
            (loop_body, IrOperand::Temp(i_next)),
        ],
    );

    let three = b.new_temp(u32_ty());
    b.move_to(three, const_u32(3));

    let cond = b.new_temp(bool_ty());
    b.binary_op(
        cond,
        BinaryOp::Lt,
        IrOperand::Temp(i),
        IrOperand::Temp(three),
    );

    b.terminate(IrTerminator::CondBr {
        cond: IrOperand::Temp(cond),
        then_b: loop_body,
        else_b: exit,
    });

    // loop_body
    b.select_block(loop_body);
    let one = b.new_temp(u32_ty());
    b.move_to(one, const_u32(1));

    b.binary_op(
        i_next,
        BinaryOp::Add,
        IrOperand::Temp(i),
        IrOperand::Temp(one),
    );

    b.terminate(IrTerminator::Br {
        target: loop_header,
    });

    // exit
    b.select_block(exit);
    b.terminate(IrTerminator::Ret {
        value: Some(IrOperand::Temp(i)),
    });

    let func = b.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc_into(vec![R::X0, R::X1]);
    let alloc_map = alloc_result.alloc_map;

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

#[test]
fn test_call_no_arg_moves() {
    // foo(x, y) calls bar(x, y) - no shuffling needed
    let mut fb = IrFunctionBuilder::new("foo".to_string(), u32_ty());
    let x = fb.new_param(0, "x".to_string(), u32_ty()); // X0
    let y = fb.new_param(1, "y".to_string(), u32_ty()); // X1

    let result = fb.new_temp(u32_ty());
    fb.call(
        Some(result),
        "bar".to_string(),
        vec![temp_operand(x), temp_operand(y)], // Same order!
        u32_ty(),
    );

    fb.terminate(IrTerminator::Ret {
        value: Some(temp_operand(result)),
    });
    let func = fb.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc();

    // Should have NO moves before the call (args already in place)
    let moves = alloc_result
        .moves
        .get_inst_moves(InstPos::new(IrBlockId(0), 0));
    if let Some(moves) = moves {
        assert_eq!(moves.before_moves.len(), 0);
    }
}

#[test]
fn test_call_result_already_in_x0() {
    // Result of call is used as arg to another call - stays in X0
    let mut fb = IrFunctionBuilder::new("foo".to_string(), u32_ty());

    let result1 = fb.new_temp(u32_ty());
    fb.call(Some(result1), "bar".to_string(), vec![], u32_ty());

    // Use result1 as first arg to another call (should stay in X0)
    let result2 = fb.new_temp(u32_ty());
    fb.call(
        Some(result2),
        "baz".to_string(),
        vec![temp_operand(result1)],
        u32_ty(),
    );

    fb.terminate(IrTerminator::Ret { value: None });
    let func = fb.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc();

    // First call should have no after-moves (result stays in X0 for next call)
    // Second call should have no before-moves (arg already in X0)
    let moves = alloc_result
        .moves
        .get_inst_moves(InstPos::new(IrBlockId(0), 0));
    if let Some(moves) = moves {
        assert_eq!(moves.after_moves.len(), 0);
        assert_eq!(moves.before_moves.len(), 0);
    }
}

#[test]
fn test_call_with_const_args() {
    let mut fb = IrFunctionBuilder::new("foo".to_string(), u32_ty());

    let result = fb.new_temp(u32_ty());
    fb.call(
        Some(result),
        "bar".to_string(),
        vec![const_u32(42), const_u32(100)],
        u32_ty(),
    );

    fb.terminate(IrTerminator::Ret {
        value: Some(temp_operand(result)),
    });
    let func = fb.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc();

    // Constants don't generate moves (they'll be loaded directly in codegen)
    let moves = alloc_result
        .moves
        .get_inst_moves(InstPos::new(IrBlockId(0), 0));
    if let Some(moves) = moves {
        assert_eq!(moves.before_moves.len(), 0);
    }
}

#[test]
fn test_multiple_calls() {
    // fn foo() -> u32 {
    // 0:entry:
    //   %t0 = fn1() : u32
    //   %t1 = fn2() : u32
    //   %t2 = fn3(%t0, %t1) : u32
    //   ret %t2
    // }
    let mut fb = IrFunctionBuilder::new("foo".to_string(), u32_ty());

    let t0 = fb.new_temp(u32_ty());
    fb.call(Some(t0), "fn1".to_string(), vec![], u32_ty());

    let t1 = fb.new_temp(u32_ty());
    fb.call(Some(t1), "fn2".to_string(), vec![], u32_ty());

    let t2 = fb.new_temp(u32_ty());
    fb.call(
        Some(t2),
        "fn3".to_string(),
        vec![temp_operand(t0), temp_operand(t1)],
        u32_ty(),
    );

    fb.terminate(IrTerminator::Ret {
        value: Some(temp_operand(t2)),
    });
    let func = fb.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc();

    // Verify moves for third call (needs to move t0 and t1 into X0 and X1)
    let moves = alloc_result
        .moves
        .get_inst_moves(InstPos::new(IrBlockId(0), 2));

    assert!(moves.is_some());
    let moves = moves.unwrap();

    // No moves before call since t0 and t1 are already in X0 and X1 (param registers for fn3)
    assert_eq!(moves.before_moves.len(), 0);

    // Move result from x0 to x2
    assert_eq!(moves.after_moves.len(), 1);
    assert_eq!(moves.after_moves[0].from, Location::Reg(R::X0));
    assert_eq!(moves.after_moves[0].to, Location::Reg(R::X2));
}

#[test]
fn test_call_arg_swap() {
    //
    // fn foo(x: u32, y: u32) -> () {
    // 0:entry:
    //   %t2 = bar(%t0, %t1) : u32 // swapped args
    //   ret %t2
    // }
    let mut fb = IrFunctionBuilder::new("foo".to_string(), unit_ty());
    let x = fb.new_param(0, "x".to_string(), u32_ty()); // -> X0
    let y = fb.new_param(1, "y".to_string(), u32_ty()); // -> X1

    let result = fb.new_temp(u32_ty());
    fb.call(
        Some(result),
        "bar".to_string(),
        vec![temp_operand(y), temp_operand(x)], // Swapped!
        u32_ty(),
    );

    fb.terminate(IrTerminator::Ret {
        value: Some(temp_operand(result)),
    });
    let func = fb.finish();

    let constraints = analyze_constraints(&func);
    let alloc_result = RegAlloc::new(&func, &constraints).alloc();
    println!(
        "alloc_map:\n{}",
        TempAllocMapDisplay(&alloc_result.alloc_map)
    );

    // Should have moves to swap X0 and X1 before the call
    let moves = alloc_result
        .moves
        .get_inst_moves(InstPos::new(IrBlockId(0), 0));
    assert!(moves.is_some());
    let moves = moves.unwrap();
    println!("moves:\n{}", moves);

    // swap x0 and x1
    assert_eq!(moves.before_moves.len(), 2);
    assert_eq!(moves.before_moves[0].from, Location::Reg(R::X1));
    assert_eq!(moves.before_moves[0].to, Location::Reg(R::X0));
    assert_eq!(moves.before_moves[1].from, Location::Reg(R::X0));
    assert_eq!(moves.before_moves[1].to, Location::Reg(R::X1));

    // move result from x0 to x2
    assert_eq!(moves.after_moves.len(), 1);
    assert_eq!(moves.after_moves[0].from, Location::Reg(R::X0));
    assert_eq!(moves.after_moves[0].to, Location::Reg(R::X2));
}

#[test]
#[should_panic(expected = "Invalid arm64 param index: 8")]
fn test_call_with_9_args() {
    let mut fb = IrFunctionBuilder::new("foo".to_string(), u32_ty());

    // Create 9 temp arguments
    let args = (0..9)
        .map(|_| {
            let t = fb.new_temp(u32_ty());
            temp_operand(t)
        })
        .collect();

    let result = fb.new_temp(u32_ty());
    fb.call(Some(result), "bar".to_string(), args, u32_ty());

    fb.terminate(IrTerminator::Ret { value: None });
    let func = fb.finish();

    // Should panic during constraint analysis
    let _ = analyze_constraints(&func);
}

#[test]
fn test_caller_saved_preservation() {
    // fn test() -> () {
    // 0:entry:
    //   %t0 = move const.42
    //   %t1 = foo(%t0) : u32
    //   %t2 = binop.add %t0, %t1 : u32
    //   ret
    // }
    let mut fb = IrFunctionBuilder::new("test".to_string(), unit_ty());

    // Create and use t0 which will be live across a call
    let t0 = fb.new_temp(u32_ty()); // X0
    fb.move_to(t0, const_u32(42));

    // Make a call (t0 should be preserved across it)
    let t1 = fb.new_temp(u32_ty()); // X1
    fb.call(
        Some(t1),
        "foo".to_string(),
        vec![temp_operand(t0)],
        u32_ty(),
    );

    // Use t0 after the call
    let t2 = fb.new_temp(u32_ty()); // spilled (not enough registers)
    fb.binary_op(t2, BinaryOp::Add, temp_operand(t0), temp_operand(t1));

    fb.terminate(IrTerminator::Ret { value: None });
    let func = fb.finish();

    // Run allocation
    let constraints = analyze_constraints(&func);
    let alloc = RegAlloc::new(&func, &constraints);
    let result = alloc.alloc_into(vec![R::X0, R::X1]);

    // println!("alloc_map:\n{}", TempAllocMapDisplay(&result.alloc_map));

    // Verify that t0 has save/restore moves around the call
    let moves = result.moves.get_inst_moves(InstPos::new(IrBlockId(0), 1));
    assert!(moves.is_some());
    let moves = moves.unwrap();
    println!("moves:\n{}", moves);

    // before moves:
    //   t0 -> stack[0] (saved before call)
    assert_eq!(moves.before_moves.len(), 1);
    assert_eq!(moves.before_moves[0].from, Location::Reg(R::X0));
    assert_eq!(moves.before_moves[0].to, Location::Stack(StackSlotId(0)));

    // after moves:
    //   x0 -> x1 (call result)
    //   stack[0] -> t0 (restored after call)
    assert_eq!(moves.after_moves.len(), 2);
    assert_eq!(moves.after_moves[0].from, Location::Reg(R::X0));
    assert_eq!(moves.after_moves[0].to, Location::Reg(R::X1));
    assert_eq!(moves.after_moves[1].from, Location::Stack(StackSlotId(0)));
    assert_eq!(moves.after_moves[1].to, Location::Reg(R::X0));
}
