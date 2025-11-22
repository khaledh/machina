use indexmap::IndexMap;

use crate::ir::types::{IrBlock, IrBlockId, IrTerminator};
use std::collections::HashMap;

/// A Control Flow Graph (CFG)
/// - block_ids: ordered list of block ids in the function
/// - succ: map of block id to its successor block ids
/// - pred: map of block id to its predecessor block ids
#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub block_ids: Vec<IrBlockId>,
    pub succ: HashMap<IrBlockId, Vec<IrBlockId>>,
    pub pred: HashMap<IrBlockId, Vec<IrBlockId>>,
}

impl ControlFlowGraph {
    pub fn from(blocks: &IndexMap<IrBlockId, IrBlock>) -> Self {
        let mut cfg = ControlFlowGraph {
            block_ids: Vec::new(),
            succ: HashMap::new(),
            pred: HashMap::new(),
        };

        // Collect the block ids up front for easier lookup
        for id in blocks.keys() {
            cfg.block_ids.push(*id);
            // Pre-insert empty vectors for every block so they always exist
            cfg.succ.insert(*id, Vec::new());
            cfg.pred.insert(*id, Vec::new());
        }

        for (id, block) in blocks {
            match block.term() {
                IrTerminator::Br { target } => {
                    cfg.succ.get_mut(id).unwrap().push(*target);
                    cfg.pred.get_mut(target).unwrap().push(*id);
                }
                IrTerminator::CondBr { then_b, else_b, .. } => {
                    cfg.succ.get_mut(id).unwrap().push(*then_b);
                    cfg.succ.get_mut(id).unwrap().push(*else_b);
                    cfg.pred.get_mut(then_b).unwrap().push(*id);
                    cfg.pred.get_mut(else_b).unwrap().push(*id);
                }
                IrTerminator::Ret { .. } => {
                    // no outgoing edge, nothing to do
                }
                IrTerminator::_Unterminated => {
                    panic!("Block is not terminated");
                }
            }
        }

        cfg
    }
}
