use crate::ir::types::{IrBlock, IrBlockId, IrFunction, IrInst, IrOperand, IrTempId, IrTerminator};
use std::collections::{HashMap, HashSet};
use std::fmt;

/// A block's GenKillSet
///
/// gen[B]
/// - The set of variables that are used in B before being defined in B.
/// - These are variables whose liveness is _generated_ by the block itself, regardless of what was
///   live on entry.
///
/// kill[B]
/// - The set of variables that are assigned a value (i.e., defined) anywhere in B.
/// - These definitions _kill_ any previous value of those variables coming into the block.
struct GenKillSet {
    gen_set: HashSet<IrTempId>,
    kill_set: HashSet<IrTempId>,
}

impl GenKillSet {
    pub fn new() -> Self {
        Self {
            gen_set: HashSet::new(),
            kill_set: HashSet::new(),
        }
    }
}

type GenKillMap = HashMap<IrBlockId, GenKillSet>;

impl GenKillSet {
    pub fn from(block: &IrBlock) -> Self {
        let mut gen_kill_set = GenKillSet::new();

        for inst in block.insts() {
            // Phi sources are treated as used at the end of each predecessor block, not as uses
            // in the current block. The Phi destination is defined at the start of the block,
            // so it belongs in Kill but not Gen.
            if let IrInst::Phi { .. } = inst {
                if let Some(dest) = inst.get_dest() {
                    gen_kill_set.kill_set.insert(dest);
                }
                continue;
            }

            for source in inst.get_sources() {
                if let IrOperand::Temp(temp_id) = source {
                    if !gen_kill_set.kill_set.contains(&temp_id) {
                        gen_kill_set.gen_set.insert(temp_id);
                    }
                }
            }
            if let Some(dest) = inst.get_dest() {
                gen_kill_set.kill_set.insert(dest);
            }
        }

        match block.term() {
            IrTerminator::CondBr { cond, .. } => {
                if let IrOperand::Temp(temp_id) = cond {
                    if !gen_kill_set.kill_set.contains(temp_id) {
                        gen_kill_set.gen_set.insert(*temp_id);
                    }
                }
            }
            IrTerminator::Ret { value } => {
                if let Some(IrOperand::Temp(temp_id)) = value {
                    if !gen_kill_set.kill_set.contains(temp_id) {
                        gen_kill_set.gen_set.insert(*temp_id);
                    }
                }
            }
            IrTerminator::Br { .. } | IrTerminator::_Unterminated => {}
        }

        gen_kill_set
    }
}

/// A Live Set for a block
/// - live_in: set of variables live at block entry
/// - live_out: set of variables live at block exit
pub struct LiveSet {
    live_in: HashSet<IrTempId>,
    live_out: HashSet<IrTempId>,
}

impl LiveSet {
    pub fn new() -> Self {
        Self {
            live_in: HashSet::new(),
            live_out: HashSet::new(),
        }
    }
}

pub type LiveMap = HashMap<IrBlockId, LiveSet>;

/// Helper wrapper to pretty-print a `LiveMap` in block order.
pub struct LiveMapDisplay<'a> {
    pub func: &'a IrFunction,
    pub live_map: &'a LiveMap,
}

impl<'a> fmt::Display for LiveMapDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "LiveMap:")?;
        for block_id in &self.func.cfg.block_ids {
            let block = &self.func.blocks[block_id];
            let live = &self.live_map[block_id];
            write!(f, "{}:{}\n", block_id.id(), block.name)?;

            write!(f, "  live_in: [")?;
            let mut temps: Vec<_> = live.live_in.iter().cloned().collect();
            temps.sort_by_key(|t| t.id());
            for (i, t) in temps.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "%t{}", t.id())?;
            }
            writeln!(f, "]")?;

            write!(f, "  live_out: [")?;
            let mut temps: Vec<_> = live.live_out.iter().cloned().collect();
            temps.sort_by_key(|t| t.id());
            for (i, t) in temps.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "%t{}", t.id())?;
            }
            writeln!(f, "]")?;
        }
        Ok(())
    }
}

pub struct LivenessAnalysis {
    func: IrFunction,
}

impl LivenessAnalysis {
    pub fn new(func: IrFunction) -> Self {
        Self { func }
    }

    // Input: A Control Flow Graph (CFG) where each node is a Basic Block (B)
    // Output: A Live Map where each block has a LiveSet (live_in and live_out)
    //
    // Data Structures:
    //
    // global LiveIn[B] = set of variables live at block entry
    // global LiveOut[B] = set of variables live at block exit
    // local Gen[B] = set of variables used before definition in B
    // local Kill[B] = set of variables defined in B
    pub fn analyze(&self) -> LiveMap {
        // Compute the local liveness for each block
        let mut gen_kill_map = GenKillMap::new();
        for block_id in &self.func.cfg.block_ids {
            let block = &self.func.blocks[block_id];
            gen_kill_map.insert(*block_id, GenKillSet::from(block));
        }

        // Initialize the live map with empty live sets for each block
        let mut live_map = LiveMap::new();
        for block_id in self.func.cfg.block_ids.iter() {
            live_map.insert(*block_id, LiveSet::new());
        }

        // Iterate to a fixed point (backward dataflow analysis)
        let mut changed = true;
        while changed {
            changed = false;
            for block_id in self.func.cfg.block_ids.iter() {
                // 1. Calculate new LiveOut based on successors' LiveIn
                let mut new_live_out = HashSet::new();
                for succ_id in self.func.cfg.succ[block_id].iter() {
                    let succ_live_in = &live_map[succ_id].live_in;
                    new_live_out.extend(succ_live_in);
                }

                // For each phi in a successor block, add its incoming operand form this block to
                // the new live-out set.
                for succ_id in self.func.cfg.succ[block_id].iter() {
                    let succ_block = &self.func.blocks[succ_id];
                    for inst in succ_block.insts().iter() {
                        if let IrInst::Phi { incoming, .. } = inst {
                            for (pred_id, source) in incoming.iter() {
                                if *pred_id == *block_id {
                                    if let IrOperand::Temp(temp_id) = source {
                                        new_live_out.insert(*temp_id);
                                    }
                                }
                            }
                        }
                    }
                }

                // Check if LiveOut changed
                if live_map[block_id].live_out != new_live_out {
                    live_map.get_mut(block_id).unwrap().live_out = new_live_out;
                    changed = true;
                }

                // 2. Calculate new LiveIn based on local sets and new LiveOut
                //    LiveIn(B) = Gen(B) U (LiveOut(B) - Kill(B))
                // The (LiveOut[B] - Kill[B]) ensures variables defined in B don't "look through" the definition
                let gen_set = &gen_kill_map[block_id].gen_set;
                let kill_set = &gen_kill_map[block_id].kill_set;
                let live_out = &live_map[block_id].live_out;
                let diff = live_out.difference(kill_set).cloned().collect();
                let new_live_in = gen_set.union(&diff).cloned().collect();

                // Check if LiveIn changed
                if live_map[block_id].live_in != new_live_in {
                    live_map.get_mut(block_id).unwrap().live_in = new_live_in;
                    changed = true;
                }
            }
        }
        live_map
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LiveInterval {
    pub start: u32,
    pub end: u32,
}

pub type LiveIntervalMap = HashMap<IrTempId, LiveInterval>;

pub fn build_live_intervals(func: &IrFunction, live_map: &LiveMap) -> LiveIntervalMap {
    let mut map = LiveIntervalMap::new();

    let mut block_last_inst_idx = HashMap::new();

    // 1. For each instruction, assign a start and end position
    let mut inst_idx = 0;
    for block in func.blocks.values() {
        for inst in block.insts().iter() {
            for operand in inst.get_sources() {
                if let IrOperand::Temp(temp_id) = operand {
                    map.entry(temp_id)
                        .and_modify(|interval| interval.end = inst_idx + 1)
                        .or_insert(LiveInterval {
                            start: inst_idx,
                            end: inst_idx + 1,
                        });
                }
            }
            if let Some(temp_id) = inst.get_dest() {
                map.entry(temp_id)
                    .and_modify(|interval| interval.end = inst_idx + 1)
                    .or_insert(LiveInterval {
                        start: inst_idx,
                        end: inst_idx + 1,
                    });
            }
            inst_idx += 1;
        }
        block_last_inst_idx.insert(block.id(), inst_idx);
    }

    // 2. Extend intervals across block boundaries using live_out
    for block_id in func.blocks.keys() {
        let live_out = &live_map[block_id].live_out;
        for temp_id in live_out.iter() {
            map.entry(*temp_id)
                .and_modify(|interval| {
                    interval.end = std::cmp::max(interval.end, block_last_inst_idx[block_id] + 1)
                })
                .or_insert(LiveInterval {
                    start: block_last_inst_idx[block_id],
                    end: block_last_inst_idx[block_id] + 1,
                });
        }
    }

    map
}

#[cfg(test)]
#[path = "../tests/t_ir_liveness.rs"]
mod tests;
