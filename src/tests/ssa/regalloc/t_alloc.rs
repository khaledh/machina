use crate::regalloc::target::PhysReg;
use crate::resolve::DefId;
use crate::ssa::analysis::liveness;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{FunctionSig, Terminator};
use crate::ssa::regalloc::{Location, TargetSpec, regalloc};
use crate::ssa::{IrTypeCache, IrTypeKind};

struct TestTarget {
    regs: Vec<PhysReg>,
}

impl TestTarget {
    fn new(count: u8) -> Self {
        Self {
            regs: (0..count).map(PhysReg).collect(),
        }
    }
}

impl TargetSpec for TestTarget {
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
            2 => "r2",
            3 => "r3",
            _ => "rx",
        }
    }
}

#[test]
fn test_regalloc_uses_registers() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "const",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    let value = builder.const_int(1, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: Some(value) });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TestTarget::new(2);
    let alloc = regalloc(&func, &live_map, &target);

    let location = alloc.alloc_map.get(&value).expect("missing alloc");
    assert!(matches!(location, Location::Reg(_)));
}

#[test]
fn test_regalloc_spills_when_no_regs() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "const",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    let value = builder.const_int(1, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: Some(value) });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TestTarget::new(0);
    let alloc = regalloc(&func, &live_map, &target);

    let location = alloc.alloc_map.get(&value).expect("missing alloc");
    assert!(matches!(location, Location::Stack(_)));
}
