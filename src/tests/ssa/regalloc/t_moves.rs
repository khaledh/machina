use crate::regalloc::target::PhysReg;
use crate::resolve::DefId;
use crate::ssa::analysis::liveness;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{Callee, FunctionSig, Terminator};
use crate::ssa::regalloc::{Location, TargetSpec, regalloc};
use crate::ssa::{IrTypeCache, IrTypeKind};

struct CallTarget {
    allocatable: Vec<PhysReg>,
    param_regs: Vec<PhysReg>,
    result: PhysReg,
}

impl CallTarget {
    fn new(allocatable: Vec<PhysReg>, param_regs: Vec<PhysReg>, result: PhysReg) -> Self {
        Self {
            allocatable,
            param_regs,
            result,
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
        None
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
    let target = CallTarget::new(vec![], vec![], PhysReg(0));
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
    let target = CallTarget::new(vec![], vec![PhysReg(1)], PhysReg(0));
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
