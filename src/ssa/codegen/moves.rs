//! Move scheduling for SSA codegen.

use std::collections::HashMap;

use crate::ssa::analysis::cfg::Cfg;
use crate::ssa::model::ir::{BlockId, Function};
use crate::ssa::regalloc::moves::{CallMove, EdgeMove, MoveOp};

/// Move schedule keyed by edges and call sites.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MoveSchedule {
    edge_moves: HashMap<(BlockId, BlockId), Vec<MoveOp>>,
    call_moves: HashMap<(BlockId, usize), (Vec<MoveOp>, Vec<MoveOp>)>,
}

impl MoveSchedule {
    /// Build a schedule from regalloc edge/call move lists.
    pub fn from_moves(edge_moves: &[EdgeMove], call_moves: &[CallMove]) -> Self {
        let mut schedule = MoveSchedule::default();

        for edge in edge_moves {
            schedule
                .edge_moves
                .insert((edge.from, edge.to), edge.moves.clone());
        }

        for call in call_moves {
            schedule.call_moves.insert(
                (call.block, call.inst_index),
                (call.pre_moves.clone(), call.post_moves.clone()),
            );
        }

        schedule
    }

    /// Returns the edge moves for a specific control-flow edge.
    pub fn edge_moves(&self, from: BlockId, to: BlockId) -> Option<&[MoveOp]> {
        self.edge_moves
            .get(&(from, to))
            .map(|moves: &Vec<MoveOp>| moves.as_slice())
    }

    /// Returns the pre-/post-moves for a call site.
    pub fn call_moves(&self, block: BlockId, inst_index: usize) -> Option<(&[MoveOp], &[MoveOp])> {
        self.call_moves
            .get(&(block, inst_index))
            .map(|(pre, post): &(Vec<MoveOp>, Vec<MoveOp>)| (pre.as_slice(), post.as_slice()))
    }
}

/// Identifier for a synthesized move block used during codegen.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MoveBlockId(pub u32);

/// Placement decision for edge moves.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeMovePlacement {
    /// Emit moves directly in the predecessor block before the branch.
    InPredecessor,
    /// Split the edge and emit moves in a dedicated block.
    Split { block: MoveBlockId },
}

/// Resulting edge target after accounting for split move blocks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeTarget {
    Direct(BlockId),
    Via(MoveBlockId),
}

/// Plan for emitting edge moves during codegen without mutating SSA IR.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EdgeMovePlan {
    schedule: MoveSchedule,
    placements: HashMap<(BlockId, BlockId), EdgeMovePlacement>,
    move_blocks: HashMap<MoveBlockId, Vec<MoveOp>>,
}

impl EdgeMovePlan {
    /// Build an edge-move plan based on CFG successor counts.
    pub fn new(func: &Function, schedule: MoveSchedule) -> Self {
        let cfg = Cfg::new(func);
        let mut plan = EdgeMovePlan {
            schedule,
            placements: HashMap::new(),
            move_blocks: HashMap::new(),
        };

        let mut next_id = 0u32;
        for ((from, to), moves) in plan.schedule.edge_moves.iter() {
            let placement = if cfg.succs(*from).len() > 1 {
                let id = MoveBlockId(next_id);
                next_id += 1;
                plan.move_blocks.insert(id, moves.clone());
                EdgeMovePlacement::Split { block: id }
            } else {
                EdgeMovePlacement::InPredecessor
            };

            plan.placements.insert((*from, *to), placement);
        }

        plan
    }

    /// Returns the placement decision for a specific edge.
    pub fn edge_placement(&self, from: BlockId, to: BlockId) -> Option<EdgeMovePlacement> {
        self.placements.get(&(from, to)).copied()
    }

    /// Returns the edge target to branch to during codegen.
    pub fn edge_target(&self, from: BlockId, to: BlockId) -> EdgeTarget {
        match self
            .placements
            .get(&(from, to))
            .copied()
            .unwrap_or(EdgeMovePlacement::InPredecessor)
        {
            EdgeMovePlacement::InPredecessor => EdgeTarget::Direct(to),
            EdgeMovePlacement::Split { block } => EdgeTarget::Via(block),
        }
    }

    /// Returns the final target block for a synthesized move block.
    pub fn move_block_target(&self, block: MoveBlockId) -> Option<BlockId> {
        self.placements
            .iter()
            .find_map(|((_, to), placement)| match placement {
                EdgeMovePlacement::Split { block: id } if *id == block => Some(*to),
                _ => None,
            })
    }

    /// Returns the move list for a synthesized move block.
    pub fn move_block(&self, block: MoveBlockId) -> Option<&[MoveOp]> {
        self.move_blocks.get(&block).map(|moves| moves.as_slice())
    }

    /// Returns the synthesized move blocks with their original edge metadata.
    pub fn move_blocks(&self) -> Vec<MoveBlock> {
        let mut blocks = Vec::new();
        for ((from, to), placement) in &self.placements {
            if let EdgeMovePlacement::Split { block } = *placement {
                if let Some(moves) = self.move_blocks.get(&block) {
                    blocks.push(MoveBlock {
                        id: block,
                        from: *from,
                        to: *to,
                        moves: moves.clone(),
                    });
                }
            }
        }
        blocks
    }

    /// Returns the underlying move schedule for edge/call queries.
    pub fn schedule(&self) -> &MoveSchedule {
        &self.schedule
    }
}

/// Describes a synthetic block used to emit edge moves.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MoveBlock {
    pub id: MoveBlockId,
    pub from: BlockId,
    pub to: BlockId,
    pub moves: Vec<MoveOp>,
}
