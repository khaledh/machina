use crate::regalloc::target::PhysReg;
use crate::resolve::DefId;
use crate::ssa::analysis::liveness;
use crate::ssa::codegen::moves::MoveSchedule;
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
    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);

    let call_index = func.blocks[0]
        .insts
        .iter()
        .position(|inst| matches!(inst.kind, crate::ssa::model::ir::InstKind::Call { .. }))
        .expect("missing call inst");
    let (pre, post) = schedule
        .call_moves(func.blocks[0].id, call_index)
        .expect("missing call schedule");

    assert_eq!(pre.len(), 1);
    assert_eq!(post.len(), 1);

    assert!(matches!(pre[0].dst, Location::Reg(reg) if reg == PhysReg(1)));
    assert!(matches!(post[0].src, Location::Reg(reg) if reg == PhysReg(0)));
}
