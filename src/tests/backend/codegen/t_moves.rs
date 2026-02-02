use crate::backend::analysis::liveness;
use crate::backend::codegen::moves::{EdgeMovePlacement, EdgeMovePlan, EdgeTarget, MoveSchedule};
use crate::backend::regalloc::moves::{EdgeMove, MoveOp};
use crate::backend::regalloc::stack::StackSlotId;
use crate::backend::regalloc::target::PhysReg;
use crate::backend::regalloc::{Location, TargetSpec, regalloc};
use crate::ir::builder::FunctionBuilder;
use crate::ir::{Callee, FunctionSig, InstKind, Terminator};
use crate::ir::{IrTypeCache, IrTypeKind};
use crate::resolve::DefId;

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
        &[PhysReg(3)]
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
fn test_codegen_move_schedule_for_call() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "call_schedule",
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
    let schedule = MoveSchedule::from_moves(
        &alloc.edge_moves,
        &alloc.call_moves,
        &alloc.entry_moves,
        &alloc.param_copies,
    );

    let call_index = func.blocks[0]
        .insts
        .iter()
        .position(|inst| matches!(inst.kind, InstKind::Call { .. }))
        .expect("missing call inst");
    let (pre, post) = schedule
        .call_moves(func.blocks[0].id, call_index)
        .expect("missing call schedule");

    assert_eq!(pre.len(), 1);
    assert_eq!(post.len(), 1);

    assert!(matches!(pre[0].dst, Location::Reg(reg) if reg == PhysReg(1)));
    assert!(matches!(post[0].src, Location::Reg(reg) if reg == PhysReg(0)));
}

#[test]
fn test_codegen_edge_move_plan_splits_on_multi_succ() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let bool_ty = types.add(IrTypeKind::Bool);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "edge_split",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let then_bb = builder.add_block();
    let else_bb = builder.add_block();
    let cond = builder.const_bool(true, bool_ty);

    builder.set_terminator(
        entry,
        Terminator::CondBr {
            cond,
            then_bb,
            then_args: vec![],
            else_bb,
            else_args: vec![],
        },
    );

    builder.select_block(then_bb);
    builder.terminate(Terminator::Return { value: None });
    builder.select_block(else_bb);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let edge_moves = vec![EdgeMove {
        from: entry,
        to: then_bb,
        moves: vec![MoveOp {
            src: Location::Stack(StackSlotId(0)),
            dst: Location::Stack(StackSlotId(1)),
            size: 8,
        }],
    }];
    let schedule = MoveSchedule::from_moves(&edge_moves, &[], &[], &[]);
    let plan = EdgeMovePlan::new(&func, schedule);

    let placement = plan
        .edge_placement(entry, then_bb)
        .expect("missing placement");
    assert!(matches!(placement, EdgeMovePlacement::Split { .. }));

    assert!(matches!(
        plan.edge_target(entry, then_bb),
        EdgeTarget::Via(_)
    ));

    let blocks = plan.move_blocks();
    assert_eq!(blocks.len(), 1);
    assert_eq!(blocks[0].from, entry);
    assert_eq!(blocks[0].to, then_bb);
    assert_eq!(blocks[0].moves.len(), 1);

    let target = plan
        .move_block_target(blocks[0].id)
        .expect("missing move block target");
    assert_eq!(target, then_bb);
}

#[test]
fn test_codegen_edge_move_plan_inserts_in_pred() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "edge_pred",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let target = builder.add_block();
    builder.set_terminator(
        entry,
        Terminator::Br {
            target,
            args: vec![],
        },
    );

    builder.select_block(target);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let edge_moves = vec![EdgeMove {
        from: entry,
        to: target,
        moves: vec![MoveOp {
            src: Location::Stack(StackSlotId(0)),
            dst: Location::Stack(StackSlotId(1)),
            size: 8,
        }],
    }];
    let schedule = MoveSchedule::from_moves(&edge_moves, &[], &[], &[]);
    let plan = EdgeMovePlan::new(&func, schedule);

    let placement = plan
        .edge_placement(entry, target)
        .expect("missing placement");
    assert!(matches!(placement, EdgeMovePlacement::InPredecessor));

    assert!(matches!(
        plan.edge_target(entry, target),
        EdgeTarget::Direct(block) if block == target
    ));
}
