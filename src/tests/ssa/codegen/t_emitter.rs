use crate::regalloc::target::PhysReg;
use crate::resolve::DefId;
use crate::ssa::analysis::liveness;
use crate::ssa::codegen::arm64::Arm64Emitter;
use crate::ssa::codegen::graph::CodegenGraph;
use crate::ssa::codegen::moves::{EdgeMovePlan, MoveSchedule};
use crate::ssa::codegen::traverse::emit_graph_with_emitter;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{BinOp, FunctionSig, Terminator};
use crate::ssa::regalloc::{Location, TargetSpec, regalloc};
use crate::ssa::{IrTypeCache, IrTypeKind};

struct TinyTarget {
    regs: Vec<PhysReg>,
}

impl TinyTarget {
    fn new(count: u8) -> Self {
        Self {
            regs: (0..count).map(PhysReg).collect(),
        }
    }
}

impl TargetSpec for TinyTarget {
    fn allocatable_regs(&self) -> &[PhysReg] {
        &self.regs
    }

    fn caller_saved(&self) -> &[PhysReg] {
        &self.regs
    }

    fn callee_saved(&self) -> &[PhysReg] {
        &[]
    }

    fn param_reg(&self, _index: u32) -> Option<PhysReg> {
        None
    }

    fn result_reg(&self) -> PhysReg {
        PhysReg(0)
    }

    fn indirect_result_reg(&self) -> Option<PhysReg> {
        None
    }

    fn indirect_call_reg(&self) -> PhysReg {
        PhysReg(0)
    }

    fn scratch_regs(&self) -> &[PhysReg] {
        &[]
    }

    fn reg_name(&self, reg: PhysReg) -> &'static str {
        match reg.0 {
            0 => "r0",
            1 => "r1",
            _ => "rx",
        }
    }
}

#[test]
fn test_arm64_emitter_basic() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_basic",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let a = builder.const_int(1, false, 64, u64_ty);
    let b = builder.const_int(2, false, 64, u64_ty);
    let _sum = builder.binop(BinOp::Add, a, b, u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TinyTarget::new(2);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(&graph, &func, &alloc.alloc_map, &mut emitter);

    let asm = emitter.finish();
    assert!(asm.contains("bb0:"));
    assert!(asm.contains("mov"));
    assert!(asm.contains("ret"));
}
