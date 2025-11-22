use crate::ir::types::{IrBlock, IrBlockId, IrFunction, IrInst, IrTempId, IrTerminator};
use std::collections::{HashMap, HashSet};

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
            // Skip Phi sources:
            //   Phi operands are treated as used at the end of each predecessor block, not as uses
            //   in the current block.
            if !matches!(inst, IrInst::Phi { .. }) {
                for source in inst.get_sources() {
                    if !gen_kill_set.kill_set.contains(&source) {
                        gen_kill_set.gen_set.insert(source);
                    }
                }
            }
            if let Some(dest) = inst.get_dest() {
                gen_kill_set.kill_set.insert(dest);
            }
        }

        match block.term() {
            IrTerminator::CondBr { cond, .. } => {
                if !gen_kill_set.kill_set.contains(cond) {
                    gen_kill_set.gen_set.insert(*cond);
                }
            }
            IrTerminator::Ret { value } => {
                if let Some(value) = value {
                    if !gen_kill_set.kill_set.contains(value) {
                        gen_kill_set.gen_set.insert(*value);
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
                // the new live out set.
                for succ_id in self.func.cfg.succ[block_id].iter() {
                    let succ_block = &self.func.blocks[succ_id];
                    for inst in succ_block.insts().iter() {
                        if let IrInst::Phi { incoming, .. } = inst {
                            for (pred_id, source) in incoming.iter() {
                                if *pred_id == *block_id {
                                    new_live_out.insert(*source);
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

    let mut block_last_pos = HashMap::new();

    // 1. For each instruction, assign a start and end position
    let mut pos = 0;
    for block in func.blocks.values() {
        for inst in block.insts().iter() {
            for temp_id in inst.get_sources() {
                map.entry(temp_id)
                    .and_modify(|interval| interval.end = pos)
                    .or_insert(LiveInterval {
                        start: pos,
                        end: pos,
                    });
            }
            if let Some(temp_id) = inst.get_dest() {
                map.entry(temp_id)
                    .and_modify(|interval| interval.end = pos)
                    .or_insert(LiveInterval {
                        start: pos,
                        end: pos,
                    });
            }
            pos += 1;
        }
        block_last_pos.insert(block.id(), pos);
    }

    // 2. Extend intervals across block boundaries using live_out
    for block_id in func.blocks.keys() {
        let live_out = &live_map[block_id].live_out;
        for temp_id in live_out.iter() {
            map.entry(*temp_id)
                .and_modify(|interval| {
                    interval.end = std::cmp::max(interval.end, block_last_pos[block_id])
                })
                .or_insert(LiveInterval {
                    start: block_last_pos[block_id],
                    end: block_last_pos[block_id],
                });
        }
    }

    map
}

#[cfg(test)]
#[path = "../tests/t_ir_liveness.rs"]
mod tests;
