use crate::regalloc::target::PhysReg;
use crate::resolve::DefId;
use crate::ssa::analysis::liveness;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{BlockId, Callee, FunctionSig, Terminator};
use crate::ssa::regalloc::moves::{EdgeMove, MoveOp, MovePlan};
use crate::ssa::regalloc::{Location, StackSlotId, TargetSpec, regalloc};
use crate::ssa::{IrStructField, IrTypeCache, IrTypeKind};

// ============================================================================
// Test Target Specs
// ============================================================================

/// Test target with configurable param/result registers for call ABI tests.
struct CallTarget {
    allocatable: Vec<PhysReg>,
    param_regs: Vec<PhysReg>,
    result: PhysReg,
    indirect_result: Option<PhysReg>,
}

impl CallTarget {
    fn new(
        allocatable: Vec<PhysReg>,
        param_regs: Vec<PhysReg>,
        result: PhysReg,
        indirect_result: Option<PhysReg>,
    ) -> Self {
        Self {
            allocatable,
            param_regs,
            result,
            indirect_result,
        }
    }
}

impl TargetSpec for CallTarget {
    fn allocatable_regs(&self) -> &[PhysReg] {
        &self.allocatable
    }

    fn caller_saved(&self) -> &[PhysReg] {
        &self.allocatable
    }

    fn callee_saved(&self) -> &[PhysReg] {
        &[]
    }

    fn param_reg(&self, index: u32) -> Option<PhysReg> {
        self.param_regs.get(index as usize).copied()
    }

    fn result_reg(&self) -> PhysReg {
        self.result
    }

    fn indirect_result_reg(&self) -> Option<PhysReg> {
        self.indirect_result
    }

    fn indirect_call_reg(&self) -> PhysReg {
        self.result
    }

    fn scratch_regs(&self) -> &[PhysReg] {
        &[]
    }

    fn reg_name(&self, reg: PhysReg) -> &'static str {
        match reg.0 {
            0 => "r0",
            1 => "r1",
            2 => "r2",
            3 => "r3",
            _ => "rx",
        }
    }
}

/// Test target for indirect calls with a dedicated indirect call register.
struct IndirectCallTarget {
    allocatable: Vec<PhysReg>,
    indirect_call: PhysReg,
}

impl IndirectCallTarget {
    fn new(allocatable: Vec<PhysReg>, indirect_call: PhysReg) -> Self {
        Self {
            allocatable,
            indirect_call,
        }
    }
}

impl TargetSpec for IndirectCallTarget {
    fn allocatable_regs(&self) -> &[PhysReg] {
        &self.allocatable
    }

    fn caller_saved(&self) -> &[PhysReg] {
        &self.allocatable
    }

    fn callee_saved(&self) -> &[PhysReg] {
        &[]
    }

    fn param_reg(&self, _index: u32) -> Option<PhysReg> {
        None
    }

    fn result_reg(&self) -> PhysReg {
        PhysReg(99)
    }

    fn indirect_result_reg(&self) -> Option<PhysReg> {
        None
    }

    fn indirect_call_reg(&self) -> PhysReg {
        self.indirect_call
    }

    fn scratch_regs(&self) -> &[PhysReg] {
        &[]
    }

    fn reg_name(&self, _reg: PhysReg) -> &'static str {
        "rx"
    }
}

// ============================================================================
// Edge Move Tests
// ============================================================================

/// Basic edge move: branch argument flows to block parameter via stack move.
#[test]
fn test_regalloc_edge_moves_for_block_args() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "edge_moves",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let value = builder.const_int(7, false, 64, u64_ty);

    let target_block = builder.add_block();
    let param = builder.add_block_param(target_block, u64_ty);
    builder.set_terminator(
        entry,
        Terminator::Br {
            target: target_block,
            args: vec![value],
        },
    );

    builder.select_block(target_block);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = CallTarget::new(vec![], vec![], PhysReg(0), None);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let edge = alloc
        .edge_moves
        .iter()
        .find(|edge| edge.from == entry && edge.to == target_block)
        .expect("missing edge move");
    assert_eq!(edge.moves.len(), 1);

    let move_op = edge.moves[0];
    assert!(matches!(move_op.src, Location::Stack(_)));
    assert!(matches!(move_op.dst, Location::Stack(_)));

    if let (Location::Stack(src), Location::Stack(dst)) = (move_op.src, move_op.dst) {
        assert_ne!(src, dst);
    }

    // Ensure the block parameter got a location for the move destination.
    let param_loc = alloc.alloc_map.get(&param).expect("missing param alloc");
    assert!(matches!(param_loc, Location::Stack(_)));
}

// ============================================================================
// Call Move Tests
// ============================================================================

/// Basic call: arg moves to param register, result moves from return register.
#[test]
fn test_regalloc_call_moves() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "call_moves",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let arg = builder.const_int(5, false, 64, u64_ty);
    let _call = builder.call(Callee::Direct(DefId(1)), vec![arg], u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = CallTarget::new(vec![], vec![PhysReg(1)], PhysReg(0), None);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let call_move = alloc.call_moves.first().expect("missing call moves");
    assert_eq!(call_move.pre_moves.len(), 1);
    assert_eq!(call_move.post_moves.len(), 1);

    let pre = call_move.pre_moves[0];
    assert!(matches!(pre.dst, Location::Reg(reg) if reg == PhysReg(1)));
    assert!(matches!(pre.src, Location::Stack(_)));

    let post = call_move.post_moves[0];
    assert!(matches!(post.src, Location::Reg(reg) if reg == PhysReg(0)));
    assert!(matches!(post.dst, Location::Stack(_)));
}

/// Overflow args: args beyond param_reg_count go to OutgoingArg stack slots.
#[test]
fn test_regalloc_call_moves_stack_args() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "call_stack_args",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let args = vec![
        builder.const_int(1, false, 64, u64_ty),
        builder.const_int(2, false, 64, u64_ty),
        builder.const_int(3, false, 64, u64_ty),
        builder.const_int(4, false, 64, u64_ty),
    ];
    let _call = builder.call(Callee::Direct(DefId(1)), args, u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = CallTarget::new(vec![], vec![PhysReg(1), PhysReg(2)], PhysReg(0), None);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let call_move = alloc.call_moves.first().expect("missing call moves");
    let stack_moves: Vec<_> = call_move
        .pre_moves
        .iter()
        .filter(|mov| matches!(mov.dst, Location::OutgoingArg(_)))
        .collect();
    assert_eq!(stack_moves.len(), 2);
    assert!(
        stack_moves
            .iter()
            .any(|mov| matches!(mov.dst, Location::OutgoingArg(0)))
    );
    assert!(
        stack_moves
            .iter()
            .any(|mov| matches!(mov.dst, Location::OutgoingArg(8)))
    );
}

/// Struct return (sret): aggregate result address passed via indirect_result_reg.
#[test]
fn test_regalloc_call_moves_sret() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let pair_ty = types.add(IrTypeKind::Struct {
        fields: vec![
            IrStructField {
                name: "a".to_string(),
                ty: u64_ty,
            },
            IrStructField {
                name: "b".to_string(),
                ty: u64_ty,
            },
        ],
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "call_sret",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let call = builder.call(Callee::Direct(DefId(1)), vec![], pair_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = CallTarget::new(vec![], vec![PhysReg(0)], PhysReg(0), Some(PhysReg(8)));
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let result_loc = alloc.alloc_map.get(&call).expect("missing call alloc");
    assert!(matches!(result_loc, Location::Stack(_)));

    let call_move = alloc.call_moves.first().expect("missing call moves");
    assert!(call_move.post_moves.is_empty());
    assert!(call_move.pre_moves.iter().any(|mov| matches!(
        (mov.src, mov.dst),
        (Location::StackAddr(_), Location::Reg(reg)) if reg == PhysReg(8)
    )));
}

/// Indirect call: function pointer moves to indirect_call_reg before the call.
#[test]
fn test_regalloc_call_moves_indirect_callee() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let fn_ty = types.add(IrTypeKind::Fn {
        params: vec![],
        ret: unit_ty,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "call_indirect",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let func_ptr = builder.const_func_addr(DefId(1), fn_ty);
    builder.call(Callee::Value(func_ptr), vec![], unit_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = IndirectCallTarget::new(vec![PhysReg(1), PhysReg(2)], PhysReg(0));
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let call_move = alloc.call_moves.first().expect("missing call moves");
    assert_eq!(call_move.pre_moves.len(), 1);
    assert!(call_move.post_moves.iter().all(|mov| mov.size == 0));
    assert_eq!(
        call_move.pre_moves[0].dst,
        Location::Reg(target.indirect_call_reg())
    );
}

// ============================================================================
// Parallel Move Resolution Tests
// ============================================================================

/// Two-way cycle (r0 <-> r1): resolved via scratch to r0->scratch, r1->r0, scratch->r1.
#[test]
fn test_move_plan_resolves_cycle_with_scratch() {
    let mut plan = MovePlan {
        entry_moves: Vec::new(),
        param_copies: Vec::new(),
        edge_moves: vec![EdgeMove {
            from: BlockId(0),
            to: BlockId(1),
            moves: vec![
                MoveOp {
                    src: Location::Reg(PhysReg(0)),
                    dst: Location::Reg(PhysReg(1)),
                    size: 8,
                },
                MoveOp {
                    src: Location::Reg(PhysReg(1)),
                    dst: Location::Reg(PhysReg(0)),
                    size: 8,
                },
            ],
        }],
        call_moves: Vec::new(),
    };

    plan.resolve_parallel_moves(&[PhysReg(3)]);
    let moves = &plan.edge_moves[0].moves;
    assert_eq!(moves.len(), 3);
    assert!(matches!(
        moves[0],
        MoveOp {
            src: Location::Reg(reg),
            dst: Location::Reg(dst),
            ..
        } if reg == PhysReg(0) && dst == PhysReg(3)
    ));
    assert!(matches!(
        moves[1],
        MoveOp {
            src: Location::Reg(reg),
            dst: Location::Reg(dst),
            ..
        } if reg == PhysReg(1) && dst == PhysReg(0)
    ));
    assert!(matches!(
        moves[2],
        MoveOp {
            src: Location::Reg(reg),
            dst: Location::Reg(dst),
            ..
        } if reg == PhysReg(3) && dst == PhysReg(1)
    ));
}

/// Scratch clobber: stack-to-stack uses scratch, so r0->r1 must come first.
#[test]
fn test_move_plan_respects_scratch_clobber() {
    let scratch = PhysReg(0);
    let mut plan = MovePlan {
        entry_moves: Vec::new(),
        param_copies: Vec::new(),
        edge_moves: vec![EdgeMove {
            from: BlockId(0),
            to: BlockId(1),
            moves: vec![
                MoveOp {
                    src: Location::Stack(StackSlotId(0)),
                    dst: Location::Stack(StackSlotId(1)),
                    size: 8,
                },
                MoveOp {
                    src: Location::Reg(scratch),
                    dst: Location::Reg(PhysReg(1)),
                    size: 8,
                },
            ],
        }],
        call_moves: vec![],
    };

    plan.resolve_parallel_moves(&[scratch]);

    let moves = &plan.edge_moves[0].moves;
    assert_eq!(moves[0].src, Location::Reg(scratch));
    assert!(matches!(moves[1].src, Location::Stack(_)));
}

/// Conditional branch: generates separate edge moves for then and else targets.
#[test]
fn test_regalloc_condbr_edge_moves() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let bool_ty = types.add(IrTypeKind::Bool);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "condbr_moves",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let cond = builder.const_bool(true, bool_ty);
    let val_then = builder.const_int(1, false, 64, u64_ty);
    let val_else = builder.const_int(2, false, 64, u64_ty);

    let then_block = builder.add_block();
    let then_param = builder.add_block_param(then_block, u64_ty);
    let else_block = builder.add_block();
    let else_param = builder.add_block_param(else_block, u64_ty);

    builder.set_terminator(
        entry,
        Terminator::CondBr {
            cond,
            then_bb: then_block,
            then_args: vec![val_then],
            else_bb: else_block,
            else_args: vec![val_else],
        },
    );

    builder.select_block(then_block);
    builder.terminate(Terminator::Return { value: None });
    builder.select_block(else_block);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = CallTarget::new(vec![], vec![], PhysReg(0), None);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    // Should have edge moves for both branches
    let then_edge = alloc
        .edge_moves
        .iter()
        .find(|e| e.from == entry && e.to == then_block);
    let else_edge = alloc
        .edge_moves
        .iter()
        .find(|e| e.from == entry && e.to == else_block);

    assert!(then_edge.is_some(), "missing then edge move");
    assert!(else_edge.is_some(), "missing else edge move");

    // Verify destinations match the block parameters
    let then_param_loc = alloc.alloc_map.get(&then_param).unwrap();
    let else_param_loc = alloc.alloc_map.get(&else_param).unwrap();
    assert_eq!(then_edge.unwrap().moves[0].dst, *then_param_loc);
    assert_eq!(else_edge.unwrap().moves[0].dst, *else_param_loc);
}

/// Multiple params: edge with 3 arguments generates 3 moves to param locations.
#[test]
fn test_regalloc_multiple_block_params() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "multi_params",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let v1 = builder.const_int(1, false, 64, u64_ty);
    let v2 = builder.const_int(2, false, 64, u64_ty);
    let v3 = builder.const_int(3, false, 64, u64_ty);

    let target_block = builder.add_block();
    let p1 = builder.add_block_param(target_block, u64_ty);
    let p2 = builder.add_block_param(target_block, u64_ty);
    let p3 = builder.add_block_param(target_block, u64_ty);

    builder.set_terminator(
        entry,
        Terminator::Br {
            target: target_block,
            args: vec![v1, v2, v3],
        },
    );

    builder.select_block(target_block);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = CallTarget::new(vec![], vec![], PhysReg(0), None);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let edge = alloc
        .edge_moves
        .iter()
        .find(|e| e.from == entry && e.to == target_block)
        .expect("missing edge move");

    // Should have 3 moves for the 3 parameters
    assert_eq!(edge.moves.len(), 3);

    // Verify each move destination matches its corresponding parameter
    let p1_loc = alloc.alloc_map.get(&p1).unwrap();
    let p2_loc = alloc.alloc_map.get(&p2).unwrap();
    let p3_loc = alloc.alloc_map.get(&p3).unwrap();

    assert!(edge.moves.iter().any(|m| m.dst == *p1_loc));
    assert!(edge.moves.iter().any(|m| m.dst == *p2_loc));
    assert!(edge.moves.iter().any(|m| m.dst == *p3_loc));
}

/// Three-way cycle (r0->r1->r2->r0): breaks one edge, producing 4 total moves.
#[test]
fn test_move_plan_three_way_cycle() {
    // Test cycle: r0 -> r1 -> r2 -> r0
    let mut plan = MovePlan {
        entry_moves: Vec::new(),
        param_copies: Vec::new(),
        edge_moves: vec![EdgeMove {
            from: BlockId(0),
            to: BlockId(1),
            moves: vec![
                MoveOp {
                    src: Location::Reg(PhysReg(0)),
                    dst: Location::Reg(PhysReg(1)),
                    size: 8,
                },
                MoveOp {
                    src: Location::Reg(PhysReg(1)),
                    dst: Location::Reg(PhysReg(2)),
                    size: 8,
                },
                MoveOp {
                    src: Location::Reg(PhysReg(2)),
                    dst: Location::Reg(PhysReg(0)),
                    size: 8,
                },
            ],
        }],
        call_moves: Vec::new(),
    };

    let scratch = PhysReg(3);
    plan.resolve_parallel_moves(&[scratch]);

    let moves = &plan.edge_moves[0].moves;
    // Should have 4 moves: save one to scratch, then 3 regular moves
    assert_eq!(moves.len(), 4);

    // First move should save to scratch
    assert_eq!(moves[0].dst, Location::Reg(scratch));

    // Last move should read from scratch
    assert!(moves.iter().any(|m| m.src == Location::Reg(scratch)));
}

/// No cycle: independent moves (r0->r1, r2->r3) need no scratch, stay at 2 moves.
#[test]
fn test_move_plan_no_cycle() {
    // Non-cyclic moves: r0 -> r1, r2 -> r3 (independent, no ordering needed)
    let mut plan = MovePlan {
        entry_moves: Vec::new(),
        param_copies: Vec::new(),
        edge_moves: vec![EdgeMove {
            from: BlockId(0),
            to: BlockId(1),
            moves: vec![
                MoveOp {
                    src: Location::Reg(PhysReg(0)),
                    dst: Location::Reg(PhysReg(1)),
                    size: 8,
                },
                MoveOp {
                    src: Location::Reg(PhysReg(2)),
                    dst: Location::Reg(PhysReg(3)),
                    size: 8,
                },
            ],
        }],
        call_moves: Vec::new(),
    };

    plan.resolve_parallel_moves(&[PhysReg(4)]);

    let moves = &plan.edge_moves[0].moves;
    // Should still have exactly 2 moves (no scratch needed)
    assert_eq!(moves.len(), 2);
}

/// Chain ordering: r0->r1, r1->r2 requires r1->r2 first to avoid clobbering r1.
#[test]
fn test_move_plan_chain_ordering() {
    // Chain: r0 -> r1, r1 -> r2 (must emit r1->r2 before r0->r1)
    let mut plan = MovePlan {
        entry_moves: Vec::new(),
        param_copies: Vec::new(),
        edge_moves: vec![EdgeMove {
            from: BlockId(0),
            to: BlockId(1),
            moves: vec![
                MoveOp {
                    src: Location::Reg(PhysReg(0)),
                    dst: Location::Reg(PhysReg(1)),
                    size: 8,
                },
                MoveOp {
                    src: Location::Reg(PhysReg(1)),
                    dst: Location::Reg(PhysReg(2)),
                    size: 8,
                },
            ],
        }],
        call_moves: Vec::new(),
    };

    plan.resolve_parallel_moves(&[PhysReg(3)]);

    let moves = &plan.edge_moves[0].moves;
    // Should have exactly 2 moves (no cycle, no scratch needed)
    assert_eq!(moves.len(), 2);

    // r1 -> r2 must come before r0 -> r1
    let idx_r1_to_r2 = moves
        .iter()
        .position(|m| m.src == Location::Reg(PhysReg(1)) && m.dst == Location::Reg(PhysReg(2)))
        .unwrap();
    let idx_r0_to_r1 = moves
        .iter()
        .position(|m| m.src == Location::Reg(PhysReg(0)) && m.dst == Location::Reg(PhysReg(1)))
        .unwrap();
    assert!(
        idx_r1_to_r2 < idx_r0_to_r1,
        "r1->r2 must be emitted before r0->r1"
    );
}

/// Single move: no reordering needed, passes through unchanged.
#[test]
fn test_move_plan_single_move_unchanged() {
    let mut plan = MovePlan {
        entry_moves: Vec::new(),
        param_copies: Vec::new(),
        edge_moves: vec![EdgeMove {
            from: BlockId(0),
            to: BlockId(1),
            moves: vec![MoveOp {
                src: Location::Reg(PhysReg(0)),
                dst: Location::Reg(PhysReg(1)),
                size: 8,
            }],
        }],
        call_moves: Vec::new(),
    };

    plan.resolve_parallel_moves(&[PhysReg(2)]);

    let moves = &plan.edge_moves[0].moves;
    assert_eq!(moves.len(), 1);
    assert_eq!(moves[0].src, Location::Reg(PhysReg(0)));
    assert_eq!(moves[0].dst, Location::Reg(PhysReg(1)));
}

/// Empty scratch: with no scratch registers, resolve_parallel_moves is a no-op.
#[test]
fn test_move_plan_empty_scratch_noop() {
    let mut plan = MovePlan {
        entry_moves: Vec::new(),
        param_copies: Vec::new(),
        edge_moves: vec![EdgeMove {
            from: BlockId(0),
            to: BlockId(1),
            moves: vec![
                MoveOp {
                    src: Location::Reg(PhysReg(0)),
                    dst: Location::Reg(PhysReg(1)),
                    size: 8,
                },
                MoveOp {
                    src: Location::Reg(PhysReg(1)),
                    dst: Location::Reg(PhysReg(0)),
                    size: 8,
                },
            ],
        }],
        call_moves: Vec::new(),
    };

    let original_moves = plan.edge_moves[0].moves.clone();

    // With empty scratch, resolve should be a no-op
    plan.resolve_parallel_moves(&[]);

    assert_eq!(plan.edge_moves[0].moves, original_moves);
}
