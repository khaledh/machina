//! Codegen graph that overlays move blocks on top of SSA blocks.

use std::collections::HashMap;

use crate::ssa::model::ir::{BlockId, Function, Terminator};

use super::moves::{EdgeMovePlan, EdgeTarget, MoveBlockId, MoveSchedule};
use crate::ssa::regalloc::moves::MoveOp;

/// Identifier for a codegen block (SSA block or synthetic move block).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CodegenBlockId {
    Ssa(BlockId),
    Move(MoveBlockId),
}

/// Codegen block carrying optional move ops and successor list.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodegenBlock {
    pub id: CodegenBlockId,
    pub moves: Vec<MoveOp>,
    pub succs: Vec<CodegenBlockId>,
}

/// Graph used by codegen after applying edge move planning.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodegenGraph {
    entry: CodegenBlockId,
    blocks: Vec<CodegenBlock>,
    index: HashMap<CodegenBlockId, usize>,
    schedule: MoveSchedule,
}

impl CodegenGraph {
    /// Builds a codegen graph from an SSA function and edge move plan.
    pub fn new(func: &Function, plan: &EdgeMovePlan) -> Self {
        let mut blocks = Vec::with_capacity(func.blocks.len());
        let mut index = HashMap::new();

        for block in &func.blocks {
            let id = CodegenBlockId::Ssa(block.id);
            index.insert(id, blocks.len());
            blocks.push(CodegenBlock {
                id,
                moves: Vec::new(),
                succs: Vec::new(),
            });
        }

        for move_block in plan.move_blocks() {
            let id = CodegenBlockId::Move(move_block.id);
            index.insert(id, blocks.len());
            blocks.push(CodegenBlock {
                id,
                moves: move_block.moves.clone(),
                succs: Vec::new(),
            });
        }

        let mut graph = CodegenGraph {
            entry: CodegenBlockId::Ssa(func.blocks.first().map(|b| b.id).unwrap_or(BlockId(0))),
            blocks,
            index,
            schedule: plan.schedule().clone(),
        };

        graph.populate_succs(func, plan);
        graph
    }

    /// Returns the entry block for codegen.
    pub fn entry(&self) -> CodegenBlockId {
        self.entry
    }

    /// Returns all codegen blocks.
    pub fn blocks(&self) -> &[CodegenBlock] {
        &self.blocks
    }

    /// Returns successors for a codegen block.
    pub fn succs(&self, id: CodegenBlockId) -> &[CodegenBlockId] {
        let idx = self.index[&id];
        &self.blocks[idx].succs
    }

    /// Returns edge moves scheduled for an SSA edge, if any.
    pub fn edge_moves(&self, from: BlockId, to: BlockId) -> Option<&[MoveOp]> {
        self.schedule.edge_moves(from, to)
    }

    /// Returns the codegen edge target (SSA or move block) for a given SSA edge.
    pub fn edge_target(&self, from: BlockId, to: BlockId) -> CodegenBlockId {
        let from_id = CodegenBlockId::Ssa(from);
        let succs = self.succs(from_id);
        for succ in succs {
            match *succ {
                CodegenBlockId::Ssa(id) if id == to => return *succ,
                CodegenBlockId::Move(_) => {
                    let target = self.succs(*succ).first().copied();
                    if target == Some(CodegenBlockId::Ssa(to)) {
                        return *succ;
                    }
                }
                _ => {}
            }
        }
        panic!("ssa codegen: missing edge target {:?} -> {:?}", from, to);
    }

    /// Returns the pre-/post-moves for a call site.
    pub fn call_moves(&self, block: BlockId, inst_index: usize) -> Option<(&[MoveOp], &[MoveOp])> {
        self.schedule.call_moves(block, inst_index)
    }

    /// Produces a linearized emission stream for a block.
    pub fn block_stream<'a>(
        &'a self,
        func: &'a Function,
        block: BlockId,
    ) -> CodegenBlockStream<'a> {
        let block = func
            .blocks
            .iter()
            .find(|b| b.id == block)
            .unwrap_or_else(|| panic!("ssa codegen: missing block {:?}", block));
        CodegenBlockStream::new(block, &self.schedule)
    }

    fn populate_succs(&mut self, func: &Function, plan: &EdgeMovePlan) {
        // First wire successors for SSA blocks, accounting for any split edge moves.
        for block in &func.blocks {
            let from = block.id;
            let id = CodegenBlockId::Ssa(from);
            let idx = self.index[&id];

            let mut succs = Vec::new();
            match &block.term {
                Terminator::Br { target, .. } => {
                    succs.push(edge_target(plan, from, *target));
                }
                Terminator::CondBr {
                    then_bb, else_bb, ..
                } => {
                    succs.push(edge_target(plan, from, *then_bb));
                    succs.push(edge_target(plan, from, *else_bb));
                }
                Terminator::Switch { cases, default, .. } => {
                    for case in cases {
                        succs.push(edge_target(plan, from, case.target));
                    }
                    succs.push(edge_target(plan, from, *default));
                }
                Terminator::Return { .. } | Terminator::Unreachable => {}
            }

            self.blocks[idx].succs = succs;
        }

        // Then wire successors for synthetic move blocks to their final SSA targets.
        for move_block in plan.move_blocks() {
            let id = CodegenBlockId::Move(move_block.id);
            let idx = self.index[&id];
            let target = plan
                .move_block_target(move_block.id)
                .unwrap_or(move_block.to);
            self.blocks[idx].succs = vec![CodegenBlockId::Ssa(target)];
        }
    }
}

fn edge_target(plan: &EdgeMovePlan, from: BlockId, to: BlockId) -> CodegenBlockId {
    match plan.edge_target(from, to) {
        EdgeTarget::Direct(block) => CodegenBlockId::Ssa(block),
        EdgeTarget::Via(block) => CodegenBlockId::Move(block),
    }
}

/// Emission item for a codegen block.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CodegenEmit<'a> {
    PreMoves(&'a [MoveOp]),
    Inst(&'a crate::ssa::model::ir::Instruction),
    PostMoves(&'a [MoveOp]),
}

/// Iterator-like helper that yields instructions with pre/post call moves.
#[derive(Debug)]
pub struct CodegenBlockStream<'a> {
    block: &'a crate::ssa::model::ir::Block,
    schedule: &'a MoveSchedule,
    cursor: usize,
    stage: StreamStage,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StreamStage {
    Pre,
    Inst,
    Post,
}

impl<'a> CodegenBlockStream<'a> {
    fn new(block: &'a crate::ssa::model::ir::Block, schedule: &'a MoveSchedule) -> Self {
        Self {
            block,
            schedule,
            cursor: 0,
            stage: StreamStage::Pre,
        }
    }

    pub fn next(&mut self) -> Option<CodegenEmit<'a>> {
        while self.cursor < self.block.insts.len() {
            let inst_index = self.cursor;
            let has_call = matches!(
                self.block.insts[inst_index].kind,
                crate::ssa::model::ir::InstKind::Call { .. }
            );

            if has_call {
                match self.stage {
                    StreamStage::Pre => {
                        self.stage = StreamStage::Inst;
                        if let Some((pre, _)) = self.schedule.call_moves(self.block.id, inst_index)
                        {
                            if !pre.is_empty() {
                                return Some(CodegenEmit::PreMoves(pre));
                            }
                        }
                    }
                    StreamStage::Inst => {
                        self.stage = StreamStage::Post;
                        return Some(CodegenEmit::Inst(&self.block.insts[inst_index]));
                    }
                    StreamStage::Post => {
                        self.stage = StreamStage::Pre;
                        self.cursor += 1;
                        if let Some((_, post)) = self.schedule.call_moves(self.block.id, inst_index)
                        {
                            if !post.is_empty() {
                                return Some(CodegenEmit::PostMoves(post));
                            }
                        }
                    }
                }
            } else {
                self.cursor += 1;
                return Some(CodegenEmit::Inst(&self.block.insts[inst_index]));
            }
        }

        None
    }
}
