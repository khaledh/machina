use crate::regalloc::target::PhysReg;
use crate::resolve::DefId;
use crate::ssa::analysis::liveness;
use crate::ssa::codegen::arm64::Arm64Emitter;
use crate::ssa::codegen::graph::CodegenGraph;
use crate::ssa::codegen::moves::{EdgeMovePlan, MoveSchedule};
use crate::ssa::codegen::traverse::emit_graph_with_emitter;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{
    BinOp, Callee, CmpOp, ConstValue, FunctionSig, RuntimeFn, SwitchCase, Terminator,
};
use crate::ssa::regalloc::{TargetSpec, regalloc};
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

struct NoRegTarget;

impl TargetSpec for NoRegTarget {
    fn allocatable_regs(&self) -> &[PhysReg] {
        &[]
    }

    fn caller_saved(&self) -> &[PhysReg] {
        &[]
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

    fn reg_name(&self, _reg: PhysReg) -> &'static str {
        "r0"
    }
}

struct CalleeSavedTarget {
    reg: PhysReg,
}

impl CalleeSavedTarget {
    fn new(reg: PhysReg) -> Self {
        Self { reg }
    }
}

impl TargetSpec for CalleeSavedTarget {
    fn allocatable_regs(&self) -> &[PhysReg] {
        std::slice::from_ref(&self.reg)
    }

    fn caller_saved(&self) -> &[PhysReg] {
        &[]
    }

    fn callee_saved(&self) -> &[PhysReg] {
        std::slice::from_ref(&self.reg)
    }

    fn param_reg(&self, _index: u32) -> Option<PhysReg> {
        Some(self.reg)
    }

    fn result_reg(&self) -> PhysReg {
        self.reg
    }

    fn indirect_result_reg(&self) -> Option<PhysReg> {
        None
    }

    fn indirect_call_reg(&self) -> PhysReg {
        self.reg
    }

    fn scratch_regs(&self) -> &[PhysReg] {
        &[]
    }

    fn reg_name(&self, _reg: PhysReg) -> &'static str {
        "r0"
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
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("bb0:"));
    assert!(asm.contains("mov"));
    assert!(asm.contains("ret"));
}

#[test]
fn test_arm64_emitter_cmp_cset() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let bool_ty = types.add(IrTypeKind::Bool);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_cmp",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let lhs = builder.const_int(1, false, 64, u64_ty);
    let rhs = builder.const_int(2, false, 64, u64_ty);
    let _cmp = builder.cmp(CmpOp::Lt, lhs, rhs, bool_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TinyTarget::new(2);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("cmp"));
    assert!(asm.contains("cset"));
}

#[test]
fn test_arm64_emitter_switch() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_switch",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let case0 = builder.add_block();
    let case1 = builder.add_block();
    let default = builder.add_block();
    let value = builder.const_int(1, false, 64, u64_ty);

    builder.set_terminator(
        entry,
        Terminator::Switch {
            value,
            cases: vec![
                SwitchCase {
                    value: ConstValue::Int {
                        value: 0,
                        signed: false,
                        bits: 64,
                    },
                    target: case0,
                    args: vec![],
                },
                SwitchCase {
                    value: ConstValue::Int {
                        value: 1,
                        signed: false,
                        bits: 64,
                    },
                    target: case1,
                    args: vec![],
                },
            ],
            default,
            default_args: vec![],
        },
    );

    builder.select_block(case0);
    builder.terminate(Terminator::Return { value: None });
    builder.select_block(case1);
    builder.terminate(Terminator::Return { value: None });
    builder.select_block(default);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TinyTarget::new(1);
    let alloc = regalloc(&func, &mut types, &live_map, &target);
    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("cmp"));
    assert!(asm.contains(&format!("b.eq bb{}", case0.0)));
    assert!(asm.contains(&format!("b.eq bb{}", case1.0)));
    assert!(asm.contains(&format!("b bb{}", default.0)));
}

#[test]
fn test_arm64_emitter_condbr_stack_cond() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let bool_ty = types.add(IrTypeKind::Bool);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_condbr_stack",
        FunctionSig {
            params: vec![bool_ty],
            ret: unit_ty,
        },
    );

    let entry = builder.current_block();
    let then_bb = builder.add_block();
    let else_bb = builder.add_block();
    let cond = builder.add_block_param(entry, bool_ty);

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
    let live_map = liveness::analyze(&func);
    let target = NoRegTarget;
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("ldr x9"));
    assert!(asm.contains("cbnz x9"));
}

#[test]
fn test_arm64_emitter_runtime_call() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_runtime_call",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let _call = builder.call(Callee::Runtime(RuntimeFn::Trap), vec![], unit_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TinyTarget::new(1);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("__rt_trap"));
}

#[test]
fn test_arm64_emitter_indirect_call() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let fn_ty = types.add(IrTypeKind::Fn {
        params: vec![],
        ret: unit_ty,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_indirect_call",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let func_ptr = builder.const_func_addr(DefId(1), fn_ty);
    let _call = builder.call(Callee::Value(func_ptr), vec![], unit_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TinyTarget::new(1);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("blr"));
}

#[test]
fn test_arm64_emitter_stack_frame_prologue() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_stack_frame",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let _value = builder.const_int(123, false, 64, u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = NoRegTarget;
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("sub sp, sp"));
    assert!(asm.contains("add sp, sp"));
}

#[test]
fn test_arm64_emitter_saves_callee_saved() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_callee_saved",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let value = builder.const_int(7, false, 64, u64_ty);
    let _call = builder.call(Callee::Runtime(RuntimeFn::Print), vec![value], unit_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = CalleeSavedTarget::new(PhysReg(4));
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("str x4"));
    assert!(asm.contains("ldr x4"));
}

#[test]
fn test_arm64_emitter_saves_multiple_callee_saved() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_multi_callee_saved",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let value = builder.const_int(11, false, 64, u64_ty);
    let _call = builder.call(Callee::Runtime(RuntimeFn::Print), vec![value], unit_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);

    struct MultiCalleeSavedTarget {
        regs: Vec<PhysReg>,
    }

    impl TargetSpec for MultiCalleeSavedTarget {
        fn allocatable_regs(&self) -> &[PhysReg] {
            &self.regs
        }

        fn caller_saved(&self) -> &[PhysReg] {
            &[]
        }

        fn callee_saved(&self) -> &[PhysReg] {
            &self.regs
        }

        fn param_reg(&self, index: u32) -> Option<PhysReg> {
            self.regs.get(index as usize).copied()
        }

        fn result_reg(&self) -> PhysReg {
            self.regs[0]
        }

        fn indirect_result_reg(&self) -> Option<PhysReg> {
            None
        }

        fn indirect_call_reg(&self) -> PhysReg {
            self.regs[0]
        }

        fn scratch_regs(&self) -> &[PhysReg] {
            &[]
        }

        fn reg_name(&self, _reg: PhysReg) -> &'static str {
            "r0"
        }
    }

    let target = MultiCalleeSavedTarget {
        regs: vec![PhysReg(4), PhysReg(5)],
    };
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("stp x4, x5"));
    assert!(asm.contains("ldp x4, x5"));
}

#[test]
fn test_arm64_emitter_addr_of_local() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let u64_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_addr_of_local",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let local = builder.add_local(u64_ty, None);
    let _addr = builder.addr_of_local(local, u64_ptr);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TinyTarget::new(2);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("add"));
    assert!(asm.contains("sp"));
}

#[test]
fn test_arm64_emitter_field_addr() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let struct_ty = types.add(IrTypeKind::Struct {
        fields: vec![
            crate::ssa::IrStructField {
                name: "a".to_string(),
                ty: u64_ty,
            },
            crate::ssa::IrStructField {
                name: "b".to_string(),
                ty: u64_ty,
            },
        ],
    });
    let struct_ptr = types.add(IrTypeKind::Ptr { elem: struct_ty });
    let field_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_field_addr",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let local = builder.add_local(struct_ty, None);
    let base = builder.addr_of_local(local, struct_ptr);
    let _field = builder.field_addr(base, 1, field_ptr);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TinyTarget::new(2);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("#8"));
}

#[test]
fn test_arm64_emitter_index_addr() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let u64_ptr = types.add(IrTypeKind::Ptr { elem: u64_ty });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_index_addr",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let local = builder.add_local(u64_ty, None);
    let base = builder.addr_of_local(local, u64_ptr);
    let index = builder.const_int(2, false, 64, u64_ty);
    let _elem = builder.index_addr(base, index, u64_ptr);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let live_map = liveness::analyze(&func);
    let target = TinyTarget::new(2);
    let alloc = regalloc(&func, &mut types, &live_map, &target);

    let schedule = MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        &func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        &mut emitter,
    );

    let asm = emitter.finish();
    assert!(asm.contains("lsl #3"));
}
