use crate::analysis::dataflow::DataflowGraph;
use crate::mcir::types::{BasicBlock, BlockId, FuncBody, Terminator};

pub struct McirCfg {
    preds: Vec<Vec<BlockId>>,
    succs: Vec<Vec<BlockId>>,
}

impl McirCfg {
    pub fn new(body: &FuncBody) -> Self {
        let mut succs = vec![vec![]; body.blocks.len()];
        for (idx, block) in body.blocks.iter().enumerate() {
            succs[idx] = succs_for(block);
        }

        let mut preds = vec![vec![]; body.blocks.len()];
        for (idx, outs) in succs.iter().enumerate() {
            let src = BlockId(idx as u32);
            for &dst in outs {
                preds[dst.index()].push(src);
            }
        }

        Self { preds, succs }
    }
}

fn succs_for(block: &BasicBlock) -> Vec<BlockId> {
    match &block.terminator {
        Terminator::Goto(target) => vec![*target],
        Terminator::If {
            then_bb, else_bb, ..
        } => vec![*then_bb, *else_bb],
        Terminator::Switch { cases, default, .. } => {
            let mut out = Vec::with_capacity(cases.len() + 1);
            for case in cases {
                out.push(case.target);
            }
            out.push(*default);
            out
        }
        Terminator::Return | Terminator::Unreachable | Terminator::Unterminated => vec![],
    }
}

impl DataflowGraph for McirCfg {
    type Node = BlockId;

    fn num_nodes(&self) -> usize {
        self.preds.len()
    }

    fn index(&self, node: Self::Node) -> usize {
        node.index()
    }

    fn node_at(&self, idx: usize) -> Self::Node {
        BlockId(idx as u32)
    }

    fn preds(&self, node: Self::Node) -> &[Self::Node] {
        &self.preds[node.index()]
    }

    fn succs(&self, node: Self::Node) -> &[Self::Node] {
        &self.succs[node.index()]
    }
}
