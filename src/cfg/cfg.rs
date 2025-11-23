use indexmap::IndexMap;
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
    pub fn from(blocks: &IndexMap<IrBlockId, IrBlock>) -> Self {
        let mut cfg = ControlFlowGraph {
            block_ids: Vec::new(),
            succ: HashMap::new(),
            pred: HashMap::new(),
            names: HashMap::new(),
        };

        // Collect the block ids up front for easier lookup
        for (id, block) in blocks {
            cfg.block_ids.push(*id);
            // Pre-insert empty vectors for every block so they always exist
            cfg.succ.insert(*id, Vec::new());
            cfg.pred.insert(*id, Vec::new());
            cfg.names.insert(*id, block.name.clone());
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
