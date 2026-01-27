//! Codegen traversal utilities for SSA functions.

use std::collections::{HashSet, VecDeque};

use crate::ssa::model::ir::Function;
use crate::ssa::regalloc::ValueAllocMap;
use crate::ssa::regalloc::moves::MoveOp;

use super::emitter::{CodegenEmitter, LocationResolver};
use super::graph::{CodegenBlockId, CodegenEmit, CodegenGraph};

/// Callback interface used by the traversal to emit codegen steps.
pub trait CodegenSink {
    fn enter_block(&mut self, block: CodegenBlockId);
    fn emit_moves(&mut self, moves: &[MoveOp]);
    fn emit_inst(&mut self, inst: &crate::ssa::model::ir::Instruction);
    fn emit_terminator(&mut self, term: &crate::ssa::model::ir::Terminator);
}

/// Emits code using a target emitter and allocation map.
pub fn emit_graph_with_emitter(
    graph: &CodegenGraph,
    func: &Function,
    alloc_map: &ValueAllocMap,
    frame_size: u32,
    callee_saved: &[crate::regalloc::target::PhysReg],
    emitter: &mut dyn CodegenEmitter,
) {
    struct EmitSink<'a> {
        emitter: &'a mut dyn CodegenEmitter,
        locs: LocationResolver<'a>,
    }

    impl<'a> CodegenSink for EmitSink<'a> {
        fn enter_block(&mut self, block: CodegenBlockId) {
            let label = match block {
                CodegenBlockId::Ssa(id) => format!("bb{}", id.0),
                CodegenBlockId::Move(id) => format!("mb{}", id.0),
            };
            self.emitter.begin_block(&label);
        }

        fn emit_moves(&mut self, moves: &[MoveOp]) {
            self.emitter.emit_moves(moves);
        }

        fn emit_inst(&mut self, inst: &crate::ssa::model::ir::Instruction) {
            self.emitter.emit_inst(inst, &self.locs);
        }

        fn emit_terminator(&mut self, term: &crate::ssa::model::ir::Terminator) {
            self.emitter.emit_terminator(term, &self.locs);
        }
    }

    let mut sink = EmitSink {
        emitter,
        locs: LocationResolver { map: alloc_map },
    };
    sink.emitter
        .begin_function(&format!("fn{}", func.def_id.0), frame_size, callee_saved);
    emit_graph(graph, func, &mut sink);
    sink.emitter.end_function();
}

/// Walks the codegen graph in RPO order and emits instructions.
pub fn emit_graph(graph: &CodegenGraph, func: &Function, sink: &mut dyn CodegenSink) {
    let order = rpo(graph);
    let mut visited = HashSet::new();

    for block_id in order {
        if !visited.insert(block_id) {
            continue;
        }

        sink.enter_block(block_id);

        match block_id {
            CodegenBlockId::Ssa(id) => emit_ssa_block(graph, func, id, sink),
            CodegenBlockId::Move(id) => emit_move_block(graph, id, sink),
        }
    }
}

fn emit_ssa_block(
    graph: &CodegenGraph,
    func: &Function,
    block: crate::ssa::model::ir::BlockId,
    sink: &mut dyn CodegenSink,
) {
    let mut stream = graph.block_stream(func, block);
    while let Some(item) = stream.next() {
        match item {
            CodegenEmit::PreMoves(moves) | CodegenEmit::PostMoves(moves) => {
                sink.emit_moves(moves);
            }
            CodegenEmit::Inst(inst) => sink.emit_inst(inst),
        }
    }

    let term = func
        .blocks
        .iter()
        .find(|b| b.id == block)
        .unwrap_or_else(|| panic!("ssa codegen: missing block {:?}", block))
        .term
        .clone();
    sink.emit_terminator(&term);
}

fn emit_move_block(
    graph: &CodegenGraph,
    block: super::moves::MoveBlockId,
    sink: &mut dyn CodegenSink,
) {
    let moves = graph
        .blocks()
        .iter()
        .find(|b| b.id == CodegenBlockId::Move(block))
        .map(|b| b.moves.as_slice())
        .unwrap_or_else(|| panic!("ssa codegen: missing move block {:?}", block));
    sink.emit_moves(moves);

    // Move blocks are single-successor by construction.
    let succs = graph.succs(CodegenBlockId::Move(block));
    let target = succs
        .first()
        .copied()
        .unwrap_or_else(|| panic!("ssa codegen: move block {:?} has no successor", block));
    let term = match target {
        CodegenBlockId::Ssa(block) => crate::ssa::model::ir::Terminator::Br {
            target: block,
            args: Vec::new(),
        },
        CodegenBlockId::Move(_) => {
            panic!("ssa codegen: move block successor must be SSA block")
        }
    };
    sink.emit_terminator(&term);
}

fn rpo(graph: &CodegenGraph) -> Vec<CodegenBlockId> {
    let mut order = Vec::new();
    let mut visited = HashSet::new();
    let mut stack = VecDeque::new();
    stack.push_back(graph.entry());

    while let Some(block) = stack.pop_back() {
        if !visited.insert(block) {
            continue;
        }
        order.push(block);
        for succ in graph.succs(block) {
            stack.push_back(*succ);
        }
    }

    order
}
