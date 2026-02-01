use crate::regalloc::target::PhysReg;
use crate::resolve::DefId;
use crate::ssa::analysis::liveness;
use crate::ssa::codegen::arm64::Arm64Emitter;
use crate::ssa::codegen::emit_module_arm64;
use crate::ssa::codegen::emitter::CodegenEmitter;
use crate::ssa::codegen::graph::CodegenGraph;
use crate::ssa::codegen::moves::{EdgeMovePlan, MoveSchedule};
use crate::ssa::codegen::traverse::emit_graph_with_emitter as emit_graph_with_emitter_impl;
use crate::ssa::lower::{LoweredFunction, LoweredModule};
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{
    BinOp, Callee, CmpOp, ConstValue, FunctionSig, GlobalData, GlobalId, RuntimeFn, SwitchCase,
    Terminator,
};
use crate::ssa::regalloc::{TargetSpec, ValueAllocMap, regalloc};
use crate::ssa::{IrStructField, IrTypeCache, IrTypeId, IrTypeKind};

// ============================================================================
// Test Helpers
// ============================================================================

/// Common IR types used across most tests.
struct TestTypes {
    types: IrTypeCache,
    unit_ty: IrTypeId,
    u8_ty: IrTypeId,
    u32_ty: IrTypeId,
    u64_ty: IrTypeId,
    bool_ty: IrTypeId,
}

impl TestTypes {
    fn new() -> Self {
        let mut types = IrTypeCache::new();
        let unit_ty = types.add(IrTypeKind::Unit);
        let u8_ty = types.add(IrTypeKind::Int {
            signed: false,
            bits: 8,
        });
        let u32_ty = types.add(IrTypeKind::Int {
            signed: false,
            bits: 32,
        });
        let u64_ty = types.add(IrTypeKind::Int {
            signed: false,
            bits: 64,
        });
        let bool_ty = types.add(IrTypeKind::Bool);
        Self {
            types,
            unit_ty,
            u8_ty,
            u32_ty,
            u64_ty,
            bool_ty,
        }
    }

    fn ptr_to(&mut self, elem: IrTypeId) -> IrTypeId {
        self.types.add(IrTypeKind::Ptr { elem })
    }

    fn fn_ty(&mut self, params: Vec<IrTypeId>, ret: IrTypeId) -> IrTypeId {
        self.types.add(IrTypeKind::Fn { params, ret })
    }
}

/// Runs the full codegen pipeline on a function and returns the assembly output.
fn emit_function(
    func: &crate::ssa::model::ir::Function,
    types: &mut IrTypeCache,
    target: &dyn TargetSpec,
) -> String {
    let live_map = liveness::analyze(func);
    let alloc = regalloc(func, types, &live_map, target);
    let schedule = MoveSchedule::from_moves(
        &alloc.edge_moves,
        &alloc.call_moves,
        &alloc.entry_moves,
        &alloc.param_copies,
    );
    let plan = EdgeMovePlan::new(func, schedule);
    let graph = CodegenGraph::new(func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        types,
        &mut emitter,
    );
    emitter.finish()
}

/// Variant that returns both the asm and the allocation result for tests that need frame info.
fn emit_function_with_alloc(
    func: &crate::ssa::model::ir::Function,
    types: &mut IrTypeCache,
    target: &dyn TargetSpec,
) -> (String, crate::ssa::regalloc::AllocationResult) {
    let live_map = liveness::analyze(func);
    let alloc = regalloc(func, types, &live_map, target);
    let schedule = MoveSchedule::from_moves(
        &alloc.edge_moves,
        &alloc.call_moves,
        &alloc.entry_moves,
        &alloc.param_copies,
    );
    let plan = EdgeMovePlan::new(func, schedule);
    let graph = CodegenGraph::new(func, &plan);
    let mut emitter = Arm64Emitter::new();
    emit_graph_with_emitter(
        &graph,
        func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        types,
        &mut emitter,
    );
    (emitter.finish(), alloc)
}

// ============================================================================
// Test Target Specs
// ============================================================================

struct TinyTarget {
    regs: Vec<PhysReg>,
}

fn emit_graph_with_emitter(
    graph: &CodegenGraph,
    func: &crate::ssa::model::ir::Function,
    alloc_map: &ValueAllocMap,
    frame_size: u32,
    callee_saved: &[PhysReg],
    types: &mut IrTypeCache,
    emitter: &mut Arm64Emitter,
) {
    let def_names = std::collections::HashMap::new();
    let func_label = format!("_fn{}", func.def_id.0);
    emit_graph_with_emitter_impl(
        graph,
        func,
        alloc_map,
        frame_size,
        callee_saved,
        types,
        &def_names,
        &func_label,
        emitter,
    );
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
        PhysReg(16)
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

struct AapcsTarget {
    regs: Vec<PhysReg>,
}

impl AapcsTarget {
    fn new() -> Self {
        Self {
            regs: (0..=10).filter(|reg| *reg != 8).map(PhysReg).collect(),
        }
    }
}

impl TargetSpec for AapcsTarget {
    fn allocatable_regs(&self) -> &[PhysReg] {
        &self.regs
    }

    fn caller_saved(&self) -> &[PhysReg] {
        &self.regs
    }

    fn callee_saved(&self) -> &[PhysReg] {
        &[]
    }

    fn param_reg(&self, index: u32) -> Option<PhysReg> {
        match index {
            0 => Some(PhysReg(0)),
            1 => Some(PhysReg(1)),
            2 => Some(PhysReg(2)),
            3 => Some(PhysReg(3)),
            4 => Some(PhysReg(4)),
            5 => Some(PhysReg(5)),
            6 => Some(PhysReg(6)),
            7 => Some(PhysReg(7)),
            _ => None,
        }
    }

    fn result_reg(&self) -> PhysReg {
        PhysReg(0)
    }

    fn indirect_result_reg(&self) -> Option<PhysReg> {
        Some(PhysReg(8))
    }

    fn indirect_call_reg(&self) -> PhysReg {
        PhysReg(16)
    }

    fn scratch_regs(&self) -> &[PhysReg] {
        &[]
    }

    fn reg_name(&self, reg: PhysReg) -> &'static str {
        match reg.0 {
            0 => "x0",
            1 => "x1",
            2 => "x2",
            3 => "x3",
            4 => "x4",
            5 => "x5",
            6 => "x6",
            7 => "x7",
            8 => "x8",
            9 => "x9",
            10 => "x10",
            _ => "x?",
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

#[test]
fn test_arm64_emitter_basic() {
    let mut tt = TestTypes::new();
    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_basic",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let a = builder.const_int(1, false, 64, tt.u64_ty);
    let b = builder.const_int(2, false, 64, tt.u64_ty);
    let _sum = builder.binop(BinOp::Add, a, b, tt.u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = TinyTarget::new(2);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains(".L_fn0_bb0:"));
    assert!(asm.contains("mov"));
    assert!(asm.contains("ret"));
}

#[test]
fn test_arm64_emitter_global_bytes() {
    let mut emitter = Arm64Emitter::new();
    let data = GlobalData {
        id: GlobalId(0),
        bytes: vec![1, 2, 3],
        align: 4,
    };
    emitter.emit_global(&data);
    let asm = emitter.finish();
    assert!(asm.contains(".data"));
    assert!(asm.contains("_g0:"));
    assert!(asm.contains(".byte 1, 2, 3"));
}

#[test]
fn test_arm64_emit_module() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);

    let mut builder0 = FunctionBuilder::new(
        DefId(0),
        "module_fn0",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );
    builder0.terminate(Terminator::Return { value: None });
    let func0 = builder0.finish();

    let mut builder1 = FunctionBuilder::new(
        DefId(1),
        "module_fn1",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );
    builder1.terminate(Terminator::Return { value: None });
    let func1 = builder1.finish();

    let module = LoweredModule {
        funcs: vec![
            LoweredFunction {
                func: func0,
                types: types.clone(),
                globals: Vec::new(),
            },
            LoweredFunction {
                func: func1,
                types: types.clone(),
                globals: Vec::new(),
            },
        ],
        globals: vec![GlobalData {
            id: GlobalId(0),
            bytes: vec![9],
            align: 1,
        }],
    };

    let target = TinyTarget::new(2);
    let def_names = std::collections::HashMap::new();
    let asm = emit_module_arm64(&module, &def_names, &target);
    assert!(asm.contains("g0:"));
    assert!(asm.contains("fn0:"));
    assert!(asm.contains("fn1:"));
}

#[test]
fn test_arm64_emitter_cmp_cset() {
    let mut tt = TestTypes::new();
    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_cmp",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let lhs = builder.const_int(1, false, 64, tt.u64_ty);
    let rhs = builder.const_int(2, false, 64, tt.u64_ty);
    let _cmp = builder.cmp(CmpOp::Lt, lhs, rhs, tt.bool_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = TinyTarget::new(2);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("cmp"));
    assert!(asm.contains("cset"));
}

#[test]
fn test_arm64_emitter_switch() {
    let mut tt = TestTypes::new();
    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_switch",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let entry = builder.current_block();
    let case0 = builder.add_block();
    let case1 = builder.add_block();
    let default = builder.add_block();
    let value = builder.const_int(1, false, 64, tt.u64_ty);

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
    let target = TinyTarget::new(1);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("cmp"));
    let label_prefix = format!(".L_fn{}_bb", func.def_id.0);
    let beq_lines: Vec<_> = asm
        .lines()
        .filter(|line| line.trim_start().starts_with("b.eq "))
        .collect();
    assert_eq!(beq_lines.len(), 2);
    assert!(beq_lines.iter().all(|line| line.contains(&label_prefix)));

    let has_default = asm.lines().any(|line| {
        let line = line.trim_start();
        line.starts_with("b ") && line.contains(&label_prefix)
    });
    assert!(has_default);
}

#[test]
fn test_arm64_emitter_memcopy() {
    let mut tt = TestTypes::new();
    let u8_ptr = tt.ptr_to(tt.u8_ty);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_memcopy",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let dst = builder.add_local(tt.u8_ty, None);
    let src = builder.add_local(tt.u8_ty, None);
    let dst_addr = builder.addr_of_local(dst, u8_ptr);
    let src_addr = builder.addr_of_local(src, u8_ptr);
    let len = builder.const_int(16, false, 64, tt.u64_ty);
    builder.memcopy(dst_addr, src_addr, len);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = TinyTarget::new(2);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("bl ___rt_memcpy"));
}

#[test]
fn test_arm64_emitter_memset() {
    let mut tt = TestTypes::new();
    let u8_ptr = tt.ptr_to(tt.u8_ty);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_memset",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let dst = builder.add_local(tt.u8_ty, None);
    let dst_addr = builder.addr_of_local(dst, u8_ptr);
    let byte = builder.const_int(0x7f, false, 8, tt.u8_ty);
    let len = builder.const_int(8, false, 64, tt.u64_ty);
    builder.memset(dst_addr, byte, len);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = TinyTarget::new(2);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("bl ___rt_memset"));
}

#[test]
fn test_arm64_emitter_drop_string() {
    let mut tt = TestTypes::new();
    let u8_ptr = tt.ptr_to(tt.u8_ty);
    let string_ty = tt.types.add_named(
        IrTypeKind::Struct {
            fields: vec![
                IrStructField {
                    name: "ptr".to_string(),
                    ty: u8_ptr,
                },
                IrStructField {
                    name: "len".to_string(),
                    ty: tt.u32_ty,
                },
                IrStructField {
                    name: "cap".to_string(),
                    ty: tt.u32_ty,
                },
            ],
        },
        "string".to_string(),
    );
    let string_ptr = tt.ptr_to(string_ty);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_drop_string",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let local = builder.add_local(string_ty, None);
    let addr = builder.addr_of_local(local, string_ptr);
    builder.drop_ptr(addr);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = TinyTarget::new(2);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("bl ___rt_string_drop"));
}

#[test]
fn test_arm64_emitter_const_func_addr() {
    let mut tt = TestTypes::new();
    let fn_ty = tt.fn_ty(vec![], tt.unit_ty);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_const_func_addr",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let _addr = builder.const_func_addr(DefId(2), fn_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = TinyTarget::new(1);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("adrp"));
    assert!(asm.contains("fn2"));
}

#[test]
fn test_arm64_emitter_condbr_stack_cond() {
    let mut tt = TestTypes::new();
    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_condbr_stack",
        FunctionSig {
            params: vec![tt.bool_ty],
            ret: tt.unit_ty,
        },
    );

    let entry = builder.current_block();
    let then_bb = builder.add_block();
    let else_bb = builder.add_block();
    let cond = builder.add_block_param(entry, tt.bool_ty);

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
    let target = NoRegTarget;
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("ldrb w9"));
    assert!(asm.contains("cbnz w9"));
}

#[test]
fn test_arm64_emitter_runtime_call() {
    let mut tt = TestTypes::new();
    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_runtime_call",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let _call = builder.call(Callee::Runtime(RuntimeFn::Trap), vec![], tt.unit_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = TinyTarget::new(1);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("__rt_trap"));
}

#[test]
fn test_arm64_emitter_stack_args() {
    let mut tt = TestTypes::new();
    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_stack_args",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let mut args = Vec::new();
    for i in 0..10 {
        args.push(builder.const_int(i, false, 64, tt.u64_ty));
    }
    let _call = builder.call(Callee::Direct(DefId(1)), args, tt.u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = AapcsTarget::new();
    let (asm, alloc) = emit_function_with_alloc(&func, &mut tt.types, &target);

    let aligned = (alloc.frame_size + 15) & !15;
    let outgoing_base = aligned.saturating_sub(alloc.frame_size);
    let first = outgoing_base;
    let second = outgoing_base.saturating_add(8);
    assert!(asm.contains(&format!("[sp, #{}]", first)));
    assert!(asm.contains(&format!("[sp, #{}]", second)));
}

#[test]
fn test_arm64_emitter_sret_call_setup() {
    let mut tt = TestTypes::new();
    let pair_ty = tt.types.add(IrTypeKind::Struct {
        fields: vec![
            IrStructField {
                name: "a".to_string(),
                ty: tt.u64_ty,
            },
            IrStructField {
                name: "b".to_string(),
                ty: tt.u64_ty,
            },
        ],
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_sret_call",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let _call = builder.call(Callee::Direct(DefId(1)), vec![], pair_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = AapcsTarget::new();
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("add x8, sp"));
}

#[test]
fn test_arm64_emitter_sret_return() {
    let mut tt = TestTypes::new();
    let pair_ty = tt.types.add(IrTypeKind::Struct {
        fields: vec![
            IrStructField {
                name: "a".to_string(),
                ty: tt.u64_ty,
            },
            IrStructField {
                name: "b".to_string(),
                ty: tt.u64_ty,
            },
        ],
    });
    let pair_ptr = tt.ptr_to(pair_ty);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_sret_return",
        FunctionSig {
            params: vec![],
            ret: pair_ty,
        },
    );

    let local = builder.add_local(pair_ty, None);
    let addr = builder.addr_of_local(local, pair_ptr);
    let value = builder.load(addr, pair_ty);
    builder.terminate(Terminator::Return { value: Some(value) });

    let func = builder.finish();
    let target = AapcsTarget::new();
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("mov x0, x8"));
    assert!(asm.contains("bl ___rt_memcpy"));
}

#[test]
fn test_arm64_emitter_indirect_call() {
    let mut tt = TestTypes::new();
    let fn_ty = tt.fn_ty(vec![], tt.unit_ty);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_indirect_call",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let func_ptr = builder.const_func_addr(DefId(1), fn_ty);
    let _call = builder.call(Callee::Value(func_ptr), vec![], tt.unit_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = TinyTarget::new(1);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("blr"));
}

#[test]
fn test_arm64_emitter_stack_frame_prologue() {
    let mut tt = TestTypes::new();
    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_stack_frame",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let _value = builder.const_int(123, false, 64, tt.u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = NoRegTarget;
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("sub sp, sp"));
    assert!(asm.contains("add sp, sp"));
}

#[test]
fn test_arm64_emitter_saves_callee_saved() {
    let mut tt = TestTypes::new();
    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_callee_saved",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let value = builder.const_int(7, false, 64, tt.u64_ty);
    let _call = builder.call(Callee::Runtime(RuntimeFn::Print), vec![value], tt.unit_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = CalleeSavedTarget::new(PhysReg(4));
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("stp x4"));
    assert!(asm.contains("ldp x4"));
}

#[test]
fn test_arm64_emitter_saves_multiple_callee_saved() {
    let mut tt = TestTypes::new();
    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_multi_callee_saved",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let value = builder.const_int(11, false, 64, tt.u64_ty);
    let _call = builder.call(Callee::Runtime(RuntimeFn::Print), vec![value], tt.unit_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = MultiCalleeSavedTarget {
        regs: vec![PhysReg(4), PhysReg(5)],
    };
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("stp x4, x5"));
    assert!(asm.contains("ldp x4, x5"));
}

#[test]
fn test_arm64_emitter_addr_of_local() {
    let mut tt = TestTypes::new();
    let u64_ptr = tt.ptr_to(tt.u64_ty);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_addr_of_local",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let local = builder.add_local(tt.u64_ty, None);
    let _addr = builder.addr_of_local(local, u64_ptr);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = TinyTarget::new(2);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("add"));
    assert!(asm.contains("sp"));
}

#[test]
fn test_arm64_emitter_field_addr() {
    let mut tt = TestTypes::new();
    let struct_ty = tt.types.add(IrTypeKind::Struct {
        fields: vec![
            IrStructField {
                name: "a".to_string(),
                ty: tt.u64_ty,
            },
            IrStructField {
                name: "b".to_string(),
                ty: tt.u64_ty,
            },
        ],
    });
    let struct_ptr = tt.ptr_to(struct_ty);
    let field_ptr = tt.ptr_to(tt.u64_ty);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_field_addr",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let local = builder.add_local(struct_ty, None);
    let base = builder.addr_of_local(local, struct_ptr);
    let _field = builder.field_addr(base, 1, field_ptr);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = TinyTarget::new(2);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("#8"));
}

#[test]
fn test_arm64_emitter_index_addr() {
    let mut tt = TestTypes::new();
    let u64_ptr = tt.ptr_to(tt.u64_ty);

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "emit_index_addr",
        FunctionSig {
            params: vec![],
            ret: tt.unit_ty,
        },
    );

    let local = builder.add_local(tt.u64_ty, None);
    let base = builder.addr_of_local(local, u64_ptr);
    let index = builder.const_int(2, false, 64, tt.u64_ty);
    let _elem = builder.index_addr(base, index, u64_ptr);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();
    let target = TinyTarget::new(2);
    let asm = emit_function(&func, &mut tt.types, &target);

    assert!(asm.contains("lsl #3"));
}
