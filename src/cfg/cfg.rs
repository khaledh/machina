use std::collections::HashSet;
use std::fmt;

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
    pub names: HashMap<IrBlockId, String>,
}

impl ControlFlowGraph {
    pub fn from(blocks: &Vec<IrBlock>) -> Self {
        let mut cfg = ControlFlowGraph {
            block_ids: Vec::new(),
            succ: HashMap::new(),
            pred: HashMap::new(),
            names: HashMap::new(),
        };

        // Collect the block ids up front for easier lookup
        for block in blocks {
            cfg.block_ids.push(block.id);
            // Pre-insert empty vectors for every block so they always exist
            cfg.succ.insert(block.id, Vec::new());
            cfg.pred.insert(block.id, Vec::new());
            cfg.names.insert(block.id, block.name.clone());
        }

        for block in blocks {
            match block.term {
                IrTerminator::Br { target } => {
                    cfg.succ.get_mut(&block.id).unwrap().push(target);
                    cfg.pred.get_mut(&target).unwrap().push(block.id);
                }
                IrTerminator::CondBr { then_b, else_b, .. } => {
                    cfg.succ.get_mut(&block.id).unwrap().push(then_b);
                    cfg.succ.get_mut(&block.id).unwrap().push(else_b);
                    cfg.pred.get_mut(&then_b).unwrap().push(block.id);
                    cfg.pred.get_mut(&else_b).unwrap().push(block.id);
                }
                IrTerminator::Ret { .. } => {
                    // no outgoing edge, nothing to do
                }
                IrTerminator::_Unterminated => {
                    panic!("Block is not terminated");
                }
            }
        }

        // Compute the reverse post-order
        cfg.block_ids = cfg.rpo();

        cfg
    }

    /// Return the block ids in reverse post-order (RPO).
    ///
    /// This implementation visits the CFG using a depth-first search starting from the entry
    /// block (first block). Each block id is pushed to the output vector after recursing on all
    /// its successors, which ensures post-order. The result is then reversed to get the strict
    /// reverse post-order.
    fn rpo(&self) -> Vec<IrBlockId> {
        let mut rpo = Vec::new();
        let mut visited = HashSet::new();

        // Start DFS from the entry block (first block in original order)
        if let Some(&entry_id) = self.block_ids.first() {
            self.rpo_visit(entry_id, &mut visited, &mut rpo);
        }

        // Handle any unreachable blocks (shouldn't happen in well-formed IR)
        for block_id in &self.block_ids {
            if !visited.contains(block_id) {
                self.rpo_visit(*block_id, &mut visited, &mut rpo);
            }
        }

        rpo.reverse();
        rpo
    }

    fn rpo_visit(
        &self,
        block_id: IrBlockId,
        visited: &mut HashSet<IrBlockId>,
        rpo: &mut Vec<IrBlockId>,
    ) {
        if !visited.insert(block_id) {
            // Already visited
            return;
        }
        if let Some(successors) = self.succ.get(&block_id) {
            // Visit successors in reverse order so that the first successor (e.g., then branch,
            // loop body) appears before the second successor (e.g., else branch, loop exit) in RPO.
            // This produces more intuitive/readable block ordering.
            for succ_id in successors.iter().rev() {
                if !visited.contains(succ_id) {
                    self.rpo_visit(*succ_id, visited, rpo);
                }
            }
        }
        rpo.push(block_id);
    }
}

impl fmt::Display for ControlFlowGraph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "block_ids: [")?;
        for (i, id) in self.block_ids.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            if let Some(name) = self.names.get(id) {
                write!(f, "{}:{}", id.id(), name)?;
            } else {
                write!(f, "{}", id.id())?;
            }
        }
        writeln!(f, "]")?;

        writeln!(f, "succ: [")?;
        for id in &self.block_ids {
            let succ = &self.succ[id];
            if let Some(name) = self.names.get(id) {
                write!(f, "  {}:{} -> [", id.id(), name)?;
            } else {
                write!(f, "  {} -> [", id.id())?;
            }
            for (i, succ_id) in succ.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                if let Some(name) = self.names.get(succ_id) {
                    write!(f, "{}:{}", succ_id.id(), name)?;
                } else {
                    write!(f, "{}", succ_id.id())?;
                }
            }
            writeln!(f, "]")?;
        }
        writeln!(f, "]")?;

        writeln!(f, "pred: [")?;
        for id in &self.block_ids {
            let pred = &self.pred[id];
            if let Some(name) = self.names.get(id) {
                write!(f, "  {}:{} -> [", id.id(), name)?;
            } else {
                write!(f, "  {} -> [", id.id())?;
            }
            for (i, pred_id) in pred.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                if let Some(name) = self.names.get(pred_id) {
                    write!(f, "{}:{}", pred_id.id(), name)?;
                } else {
                    write!(f, "{}", pred_id.id())?;
                }
            }
            writeln!(f, "]")?;
        }
        writeln!(f, "]")?;
        Ok(())
    }
}

#[cfg(test)]
#[path = "../tests/t_cfg.rs"]
mod tests;
