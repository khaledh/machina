use crate::core::backend::codegen::graph::CodegenBlockId;
use crate::core::backend::codegen::graph::CodegenGraph;
use crate::core::backend::codegen::moves::{EdgeMovePlan, MoveSchedule};
use crate::core::backend::codegen::traverse::{CodegenSink, emit_graph};
use crate::core::backend::regalloc::Location;
use crate::core::backend::regalloc::moves::{CallMove, MoveOp};
use crate::core::backend::regalloc::stack::StackSlotId;
use crate::core::ir::builder::FunctionBuilder;
use crate::core::ir::{Callee, ConstValue, FunctionSig, Instruction, Terminator, ValueId};
use crate::core::ir::{IrTypeCache, IrTypeKind};
use crate::core::resolve::DefId;

struct DummySink {
    emitted_moves: usize,
    emitted_insts: usize,
    emitted_terms: usize,
}

impl DummySink {
    fn new() -> Self {
        Self {
            emitted_moves: 0,
            emitted_insts: 0,
            emitted_terms: 0,
        }
    }
}

impl CodegenSink for DummySink {
    fn enter_block(&mut self, _block: CodegenBlockId) {}

    fn emit_moves(&mut self, moves: &[MoveOp]) {
        self.emitted_moves += moves.len();
    }

    fn emit_inst(&mut self, _inst: &Instruction) {
        self.emitted_insts += 1;
    }

    fn emit_terminator(&mut self, _term: &Terminator) {
        self.emitted_terms += 1;
    }

    fn emit_branch(&mut self, _target: CodegenBlockId) {
        self.emitted_terms += 1;
    }

    fn emit_cond_branch(
        &mut self,
        _cond: ValueId,
        _then_target: CodegenBlockId,
        _else_target: CodegenBlockId,
    ) {
        self.emitted_terms += 1;
    }

    fn emit_switch(
        &mut self,
        _value: ValueId,
        _cases: &[(ConstValue, CodegenBlockId)],
        _default_target: CodegenBlockId,
    ) {
        self.emitted_terms += 1;
    }
}

#[test]
fn test_codegen_traverse_emits_moves_and_insts() {
    let mut types = IrTypeCache::new();
    let unit_ty = types.add(IrTypeKind::Unit);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });

    let mut builder = FunctionBuilder::new(
        DefId(0),
        "traverse",
        FunctionSig {
            params: vec![],
            ret: unit_ty,
        },
    );

    let arg = builder.const_int(1, false, 64, u64_ty);
    let _call = builder.call(Callee::Direct(DefId(1)), vec![arg], u64_ty);
    builder.terminate(Terminator::Return { value: None });

    let func = builder.finish();

    let call_moves = vec![CallMove {
        block: func.blocks[0].id,
        inst_index: 1,
        pre_moves: vec![MoveOp {
            src: Location::Stack(StackSlotId(0)),
            dst: Location::Stack(StackSlotId(1)),
            size: 8,
        }],
        post_moves: vec![MoveOp {
            src: Location::Stack(StackSlotId(1)),
            dst: Location::Stack(StackSlotId(2)),
            size: 8,
        }],
    }];
    let schedule = MoveSchedule::from_moves(&[], &call_moves, &[], &[]);
    let plan = EdgeMovePlan::new(&func, schedule);
    let graph = CodegenGraph::new(&func, &plan);

    let mut sink = DummySink::new();
    emit_graph(&graph, &func, &mut sink);

    assert!(sink.emitted_insts >= 1);
    assert!(sink.emitted_moves >= 2);
    assert!(sink.emitted_terms >= 1);
}
