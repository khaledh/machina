//! SSA control-flow graph utilities.

use std::collections::HashMap;

use crate::analysis::dataflow::DataflowGraph;
use crate::ssa::model::ir::{BlockId, Function, Terminator};

/// Control-flow graph for a single SSA function.
pub struct Cfg {
    entry: BlockId,
    blocks: Vec<BlockId>,
    preds: Vec<Vec<BlockId>>,
    succs: Vec<Vec<BlockId>>,
    index_map: HashMap<BlockId, usize>,
}

impl Cfg {
    pub fn new(func: &Function) -> Self {
        let mut blocks = Vec::with_capacity(func.blocks.len());
        let mut index_map = HashMap::with_capacity(func.blocks.len());
        for (idx, block) in func.blocks.iter().enumerate() {
            blocks.push(block.id);
            index_map.insert(block.id, idx);
        }

        let entry = blocks.first().copied().unwrap_or(BlockId(0));
        let mut preds = vec![Vec::new(); blocks.len()];
        let mut succs = vec![Vec::new(); blocks.len()];

        for block in &func.blocks {
            let idx = index_map[&block.id];
            let mut block_succs = Vec::new();
            match &block.term {
                Terminator::Br { target, .. } => {
                    push_unique(&mut block_succs, *target);
                }
                Terminator::CondBr {
                    then_bb, else_bb, ..
                } => {
                    push_unique(&mut block_succs, *then_bb);
                    push_unique(&mut block_succs, *else_bb);
                }
                Terminator::Switch { cases, default, .. } => {
                    for case in cases {
                        push_unique(&mut block_succs, case.target);
                    }
                    push_unique(&mut block_succs, *default);
                }
                Terminator::Return { .. } | Terminator::Unreachable => {}
            }

            for succ in &block_succs {
                let succ_idx = index_map[succ];
                push_unique(&mut preds[succ_idx], block.id);
            }

            succs[idx] = block_succs;
        }

        Self {
            entry,
            blocks,
            preds,
            succs,
            index_map,
        }
    }

    pub fn entry(&self) -> BlockId {
        self.entry
    }

    pub fn blocks(&self) -> &[BlockId] {
        &self.blocks
    }

    pub fn preds(&self, block: BlockId) -> &[BlockId] {
        let idx = self.index(block);
        &self.preds[idx]
    }

    pub fn succs(&self, block: BlockId) -> &[BlockId] {
        let idx = self.index(block);
        &self.succs[idx]
    }

    /// Returns reverse postorder for the reachable portion of the CFG.
    pub fn rpo(&self) -> Vec<BlockId> {
        let mut order = self.postorder();
        order.reverse();
        order
    }

    /// Returns postorder for the reachable portion of the CFG.
    pub fn postorder(&self) -> Vec<BlockId> {
        let mut visited = vec![false; self.blocks.len()];
        let mut order = Vec::new();
        self.dfs(self.entry, &mut visited, &mut order);
        order
    }

    fn dfs(&self, block: BlockId, visited: &mut [bool], order: &mut Vec<BlockId>) {
        let idx = self.index(block);
        if visited[idx] {
            return;
        }
        visited[idx] = true;

        for succ in self.succs(block) {
            self.dfs(*succ, visited, order);
        }

        order.push(block);
    }
}

impl DataflowGraph for Cfg {
    type Node = BlockId;

    fn num_nodes(&self) -> usize {
        self.blocks.len()
    }

    fn index(&self, node: Self::Node) -> usize {
        *self
            .index_map
            .get(&node)
            .unwrap_or_else(|| panic!("ssa cfg missing block {:?}", node))
    }

    fn node_at(&self, idx: usize) -> Self::Node {
        self.blocks[idx]
    }

    fn preds(&self, node: Self::Node) -> &[Self::Node] {
        self.preds(node)
    }

    fn succs(&self, node: Self::Node) -> &[Self::Node] {
        self.succs(node)
    }
}

fn push_unique(list: &mut Vec<BlockId>, block: BlockId) {
    if !list.contains(&block) {
        list.push(block);
    }
}
