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

struct SplitTarget {
    allocatable: Vec<PhysReg>,
    caller_saved: Vec<PhysReg>,
    callee_saved: Vec<PhysReg>,
    param_regs: Vec<PhysReg>,
    result: PhysReg,
}

impl SplitTarget {
    fn new(
        allocatable: Vec<PhysReg>,
        caller_saved: Vec<PhysReg>,
        callee_saved: Vec<PhysReg>,
        param_regs: Vec<PhysReg>,
        result: PhysReg,
    ) -> Self {
        Self {
            allocatable,
            caller_saved,
            callee_saved,
            param_regs,
            result,
        }
    }
}

impl TargetSpec for SplitTarget {
    fn allocatable_regs(&self) -> &[PhysReg] {
        &self.allocatable
    }

    fn caller_saved(&self) -> &[PhysReg] {
        &self.caller_saved
    }

    fn callee_saved(&self) -> &[PhysReg] {
        &self.callee_saved
    }

    fn param_reg(&self, _index: u32) -> Option<PhysReg> {
        self.param_regs.get(_index as usize).copied()
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
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let location = alloc.alloc_map.get(&value).expect("missing alloc");
    assert!(matches!(location, Location::Reg(_)));
}

#[test]
fn test_regalloc_spills_when_no_regs() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "const",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let value = builder.const_int(1, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TestTarget::new(0);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let location = alloc.alloc_map.get(&value).expect("missing alloc");
    assert!(matches!(location, Location::Stack(_)));
}

#[test]
fn test_regalloc_prefers_call_safe_regs() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "call_safe",
        FunctionSig {
            params: vec![u64_ty],
            ret: u64_ty,
        },
    );

    let entry = builder.current_block();
    let param = builder.add_block_param(entry, u64_ty);
    let callee = DefId(1);
    let call = builder.call(
        crate::ssa::model::ir::Callee::Direct(callee),
        vec![param],
        u64_ty,
    );
    let sum = builder.binop(crate::ssa::model::ir::BinOp::Add, param, call, u64_ty);
    builder.terminate(Terminator::Return { value: Some(sum) });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = SplitTarget::new(
        vec![PhysReg(0), PhysReg(1), PhysReg(2)],
        vec![PhysReg(0)],
        vec![PhysReg(1), PhysReg(2)],
        vec![PhysReg(1)],
        PhysReg(0),
    );
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let location = alloc.alloc_map.get(&param).expect("missing alloc");
    assert!(matches!(location, Location::Reg(reg) if reg.0 != 0));
}

#[test]
fn test_regalloc_precolors_return_value() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "ret_reg",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    let value = builder.const_int(42, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: Some(value) });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = SplitTarget::new(
        vec![PhysReg(0), PhysReg(1)],
        vec![],
        vec![PhysReg(1)],
        vec![],
        PhysReg(0),
    );
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let location = alloc.alloc_map.get(&value).expect("missing alloc");
    assert!(matches!(location, Location::Reg(reg) if *reg == PhysReg(0)));
}

#[test]
fn test_regalloc_sizes_spill_slots() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let array_ty = types.add(IrTypeKind::Array {
        elem: u64_ty,
        dims: vec![2],
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "spill_size",
        FunctionSig {
            params: vec![array_ty],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let _param = builder.add_block_param(entry, array_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TestTarget::new(0);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    assert_eq!(alloc.stack_slot_count, 2);
    assert_eq!(alloc.frame_size, 16);
}

#[test]
fn test_regalloc_precolors_param_reg() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "param_reg",
        FunctionSig {
            params: vec![u64_ty],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let param = builder.add_block_param(entry, u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = SplitTarget::new(
        vec![PhysReg(0), PhysReg(1)],
        vec![],
        vec![PhysReg(1)],
        vec![PhysReg(1)],
        PhysReg(0),
    );
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let location = alloc.alloc_map.get(&param).expect("missing alloc");
    assert!(matches!(location, Location::Reg(reg) if *reg == PhysReg(1)));
}
