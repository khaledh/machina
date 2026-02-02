use crate::backend::analysis::liveness;
use crate::backend::regalloc::TargetSpec;
use crate::backend::regalloc::constraints;
use crate::backend::regalloc::intervals;
use crate::backend::regalloc::target::PhysReg;
use crate::backend::{IrTypeCache, IrTypeKind};
use crate::ir::builder::FunctionBuilder;
use crate::ir::ir::{BinOp, Callee, FunctionSig, Terminator};
use crate::resolve::DefId;

// ============================================================================
// Test Target Specs
// ============================================================================

/// Test target with configurable registers for constraint tests.
struct TestTarget {
    allocatable: Vec<PhysReg>,
    param_regs: Vec<PhysReg>,
    caller_saved: Vec<PhysReg>,
    callee_saved: Vec<PhysReg>,
    result: PhysReg,
}

impl TestTarget {
    fn new(
        allocatable: Vec<PhysReg>,
        param_regs: Vec<PhysReg>,
        caller_saved: Vec<PhysReg>,
        callee_saved: Vec<PhysReg>,
        result: PhysReg,
    ) -> Self {
        Self {
            allocatable,
            param_regs,
            caller_saved,
            callee_saved,
            result,
        }
    }

    /// Creates a target with 2 param regs (r1, r2), result in r0, all caller-saved.
    fn default_caller_saved() -> Self {
        Self::new(
            vec![PhysReg(0), PhysReg(1), PhysReg(2)],
            vec![PhysReg(1), PhysReg(2)],
            vec![PhysReg(0), PhysReg(1), PhysReg(2)],
            vec![],
            PhysReg(0),
        )
    }

    /// Creates a target where param regs are callee-saved.
    fn with_callee_saved_params() -> Self {
        Self::new(
            vec![PhysReg(0), PhysReg(1), PhysReg(2)],
            vec![PhysReg(1), PhysReg(2)],
            vec![PhysReg(0)], // Only result reg is caller-saved
            vec![PhysReg(1), PhysReg(2)],
            PhysReg(0),
        )
    }

    /// Creates a target with caller-saved + callee-saved regs for call-crossing tests.
    fn with_callee_saved_pool() -> Self {
        Self::new(
            vec![PhysReg(0), PhysReg(1), PhysReg(2)],
            vec![PhysReg(0), PhysReg(1)],
            vec![PhysReg(0), PhysReg(1)], // r0/r1 are caller-saved
            vec![PhysReg(2)],             // r2 is callee-saved
            PhysReg(0),
        )
    }
}

impl TargetSpec for TestTarget {
    fn allocatable_regs(&self) -> &[PhysReg] {
        &self.allocatable
    }

    fn caller_saved(&self) -> &[PhysReg] {
        &self.caller_saved
    }

    fn callee_saved(&self) -> &[PhysReg] {
        &self.callee_saved
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
        PhysReg(99)
    }

    fn scratch_regs(&self) -> &[PhysReg] {
        &[]
    }

    fn reg_name(&self, reg: PhysReg) -> &'static str {
        match reg.0 {
            0 => "r0",
            1 => "r1",
            2 => "r2",
            _ => "rx",
        }
    }
}

// ============================================================================
// Precoloring Tests
// ============================================================================

/// Return value precolored to result register when not crossing a call.
#[test]
fn test_return_value_precolored_no_call() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "return_precolor",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    let value = builder.const_int(42, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: Some(value) });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let analysis = intervals::analyze(&func, &live_map);
    let target = TestTarget::default_caller_saved();
    let constraints = constraints::build(&analysis, &mut types, &target);

    // Return value should be precolored to result register
    assert!(
        constraints.fixed_regs.contains_key(&value),
        "return value should be precolored"
    );
    assert_eq!(
        constraints.fixed_regs.get(&value),
        Some(&target.result_reg())
    );
}

/// Return value NOT precolored when it crosses a call site.
#[test]
fn test_return_value_not_precolored_crosses_call() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "return_crosses_call",
        FunctionSig {
            params: vec![],
            ret: u64_ty,
        },
    );

    // Create value before call
    let value = builder.const_int(42, false, 64, u64_ty);
    // Call clobbers caller-saved registers
    builder.call(Callee::Direct(DefId(1)), vec![], u64_ty);
    // Return the value that was live across the call
    builder.terminate(Terminator::Return { value: Some(value) });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let analysis = intervals::analyze(&func, &live_map);
    let target = TestTarget::default_caller_saved();
    let constraints = constraints::build(&analysis, &mut types, &target);

    // Return value should NOT be precolored (crosses call, would be clobbered)
    assert!(
        !constraints.fixed_regs.contains_key(&value),
        "return value crossing call should not be precolored"
    );
}

/// Entry parameter precolored to its ABI register when not crossing a call.
#[test]
fn test_param_precolored_no_call() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "param_precolor",
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
    let analysis = intervals::analyze(&func, &live_map);
    let target = TestTarget::default_caller_saved();
    let constraints = constraints::build(&analysis, &mut types, &target);

    // Parameter should be precolored to its ABI register
    assert!(
        constraints.fixed_regs.contains_key(&param),
        "param should be precolored"
    );
    assert_eq!(
        constraints.fixed_regs.get(&param),
        Some(&PhysReg(1)) // First param reg
    );
}

/// Entry parameter NOT precolored when it crosses a call (caller-saved reg).
#[test]
fn test_param_not_precolored_crosses_call_caller_saved() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "param_crosses_call",
        FunctionSig {
            params: vec![u64_ty],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let param = builder.add_block_param(entry, u64_ty);
    // Call clobbers caller-saved registers
    builder.call(Callee::Direct(DefId(1)), vec![], u64_ty);
    // Use param after call (forces it to be live across call)
    let _use_param = builder.binop(BinOp::Add, param, param, u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let analysis = intervals::analyze(&func, &live_map);
    let target = TestTarget::default_caller_saved();
    let constraints = constraints::build(&analysis, &mut types, &target);

    // Parameter should NOT be precolored (crosses call, caller-saved would clobber)
    assert!(
        !constraints.fixed_regs.contains_key(&param),
        "param crossing call in caller-saved reg should not be precolored"
    );
}

/// Live value crossing a call should avoid caller-saved regs if a callee-saved
/// register is available.
#[test]
fn test_cross_call_avoids_caller_saved() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "cross_call_alloc",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let val = builder.const_int(42, false, 64, u64_ty);
    builder.call(Callee::Direct(DefId(1)), vec![], unit_ty);
    let use_after = builder.binop(BinOp::Add, val, val, u64_ty);
    builder.terminate(Terminator::Return {
        value: Some(use_after),
    });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TestTarget::with_callee_saved_pool();
    let alloc = crate::backend::regalloc::regalloc(&func, &mut types, &live_map, &target);

    let loc = alloc.alloc_map.get(&val).expect("missing alloc for value");
    assert!(
        matches!(
            loc,
            crate::backend::regalloc::Location::Reg(PhysReg(2))
                | crate::backend::regalloc::Location::Stack(_)
        ),
        "expected call-crossing value to avoid caller-saved regs, got {loc:?}"
    );
}

/// Entry parameter IS precolored when it crosses a call but reg is callee-saved.
#[test]
fn test_param_precolored_crosses_call_callee_saved() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "param_callee_saved",
        FunctionSig {
            params: vec![u64_ty],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let param = builder.add_block_param(entry, u64_ty);
    // Call would clobber caller-saved, but param reg is callee-saved
    builder.call(Callee::Direct(DefId(1)), vec![], u64_ty);
    // Use param after call
    let _use_param = builder.binop(BinOp::Add, param, param, u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let analysis = intervals::analyze(&func, &live_map);
    let target = TestTarget::with_callee_saved_params();
    let constraints = constraints::build(&analysis, &mut types, &target);

    // Parameter SHOULD be precolored (callee-saved survives across calls)
    assert!(
        constraints.fixed_regs.contains_key(&param),
        "param in callee-saved reg should be precolored even across calls"
    );
    assert_eq!(constraints.fixed_regs.get(&param), Some(&PhysReg(1)));
}

/// Multiple parameters get their respective ABI registers.
#[test]
fn test_multiple_params_precolored() {
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
            params: vec![u64_ty, u64_ty],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let param1 = builder.add_block_param(entry, u64_ty);
    let param2 = builder.add_block_param(entry, u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let analysis = intervals::analyze(&func, &live_map);
    let target = TestTarget::default_caller_saved();
    let constraints = constraints::build(&analysis, &mut types, &target);

    // Both parameters should be precolored to their respective registers
    assert_eq!(constraints.fixed_regs.get(&param1), Some(&PhysReg(1)));
    assert_eq!(constraints.fixed_regs.get(&param2), Some(&PhysReg(2)));
}

// ============================================================================
// Incoming Stack Argument Tests
// ============================================================================

/// Stack-passed parameters (beyond reg count) mapped to incoming arg offsets.
#[test]
fn test_incoming_stack_args() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "stack_args",
        FunctionSig {
            params: vec![u64_ty, u64_ty, u64_ty, u64_ty], // 4 params, only 2 regs
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let _p1 = builder.add_block_param(entry, u64_ty);
    let _p2 = builder.add_block_param(entry, u64_ty);
    let p3 = builder.add_block_param(entry, u64_ty); // Stack arg 0
    let p4 = builder.add_block_param(entry, u64_ty); // Stack arg 1
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let analysis = intervals::analyze(&func, &live_map);
    let target = TestTarget::default_caller_saved(); // 2 param regs
    let constraints = constraints::build(&analysis, &mut types, &target);

    // Third and fourth params should be incoming stack args
    assert_eq!(constraints.incoming_args.get(&p3), Some(&0)); // Offset 0
    assert_eq!(constraints.incoming_args.get(&p4), Some(&8)); // Offset 8
    assert_eq!(constraints.param_reg_count, 2);
}

/// Register-passed parameters should NOT be in incoming_args.
#[test]
fn test_reg_params_not_in_incoming_args() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "reg_params",
        FunctionSig {
            params: vec![u64_ty, u64_ty],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let p1 = builder.add_block_param(entry, u64_ty);
    let p2 = builder.add_block_param(entry, u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let analysis = intervals::analyze(&func, &live_map);
    let target = TestTarget::default_caller_saved();
    let constraints = constraints::build(&analysis, &mut types, &target);

    // Register-passed params should not be in incoming_args
    assert!(!constraints.incoming_args.contains_key(&p1));
    assert!(!constraints.incoming_args.contains_key(&p2));
    assert!(constraints.incoming_args.is_empty());
}

// ============================================================================
// Edge Case Tests
// ============================================================================

/// Function with no parameters or return value.
#[test]
fn test_no_params_no_return() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "void_fn",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let analysis = intervals::analyze(&func, &live_map);
    let target = TestTarget::default_caller_saved();
    let constraints = constraints::build(&analysis, &mut types, &target);

    assert!(constraints.fixed_regs.is_empty());
    assert!(constraints.incoming_args.is_empty());
}

/// Parameter that is also returned should not conflict.
#[test]
fn test_param_returned_directly() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "identity",
        FunctionSig {
            params: vec![u64_ty],
            ret: u64_ty,
        },
    );

    let entry = builder.current_block();
    let param = builder.add_block_param(entry, u64_ty);
    builder.terminate(Terminator::Return { value: Some(param) });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let analysis = intervals::analyze(&func, &live_map);
    let target = TestTarget::default_caller_saved();
    let constraints = constraints::build(&analysis, &mut types, &target);

    // Param is returned, but param_set check prevents return-value precoloring
    // Should be precolored as a parameter instead
    assert!(constraints.fixed_regs.contains_key(&param));
    // Should be in param reg, not result reg (param takes precedence)
    assert_eq!(constraints.fixed_regs.get(&param), Some(&PhysReg(1)));
}

/// Param reg count correctly computed from target.
#[test]
fn test_param_reg_count() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "count_test",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let analysis = intervals::analyze(&func, &live_map);
    let target = TestTarget::default_caller_saved();
    let constraints = constraints::build(&analysis, &mut types, &target);

    assert_eq!(constraints.param_reg_count, 2);
}
