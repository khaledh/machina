use crate::regalloc::target::PhysReg;
use crate::resolve::DefId;
use crate::ssa::analysis::liveness;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{BlockId, Callee, FunctionSig, Terminator};
use crate::ssa::regalloc::moves::{EdgeMove, MoveOp, MovePlan};
use crate::ssa::regalloc::{Location, TargetSpec, regalloc};
use crate::ssa::{IrStructField, IrTypeCache, IrTypeKind};

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

#[test]
fn test_move_plan_resolves_cycle_with_scratch() {
    let mut plan = MovePlan {
        edge_moves: vec![EdgeMove {
            from: BlockId(0),
            to: BlockId(1),
            moves: vec![
                MoveOp {
                    src: Location::Reg(PhysReg(0)),
                    dst: Location::Reg(PhysReg(1)),
                },
                MoveOp {
                    src: Location::Reg(PhysReg(1)),
                    dst: Location::Reg(PhysReg(0)),
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
            dst: Location::Reg(dst)
        } if reg == PhysReg(0) && dst == PhysReg(3)
    ));
    assert!(matches!(
        moves[1],
        MoveOp {
            src: Location::Reg(reg),
            dst: Location::Reg(dst)
        } if reg == PhysReg(1) && dst == PhysReg(0)
    ));
    assert!(matches!(
        moves[2],
        MoveOp {
            src: Location::Reg(reg),
            dst: Location::Reg(dst)
        } if reg == PhysReg(3) && dst == PhysReg(1)
    ));
}
