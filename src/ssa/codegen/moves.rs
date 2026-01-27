//! Move scheduling for SSA codegen.

use std::collections::HashMap;

use crate::ssa::model::ir::BlockId;
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
