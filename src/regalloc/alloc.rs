use std::collections::{HashMap, VecDeque};
use std::fmt;

use crate::dataflow::liveness::{
    LiveInterval, LiveIntervalMap, LivenessAnalysis, build_live_intervals,
};
use crate::ir::pos::{InstPos, RelInstPos};
use crate::ir::types::{IrFunction, IrOperand, IrTempId};
use crate::regalloc::constraints::{CallConstraint, ConstraintMap, FnParamConstraint};
use crate::regalloc::moves::{FnMoveList, Location};
use crate::regalloc::regs::{Arm64Reg, CALLER_SAVED_REGS};
use crate::regalloc::spill::{SpillAllocator, StackSlotId};

#[derive(Debug, Clone)]
pub enum MappedTemp {
    Reg(Arm64Reg),
    Stack(StackSlotId),
}

pub type TempAllocMap = HashMap<IrTempId, MappedTemp>;

/// Helper wrapper to pretty-print a TempAllocMap in a stable order.
pub struct TempAllocMapDisplay<'a>(pub &'a TempAllocMap);

impl<'a> fmt::Display for TempAllocMapDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut entries: Vec<_> = self.0.iter().collect();
        entries.sort_by_key(|(temp, _)| temp.id());

        for (i, (temp, mapped)) in entries.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            match mapped {
                MappedTemp::Reg(reg) => write!(f, "%t{} -> {}", temp.id(), reg)?,
                MappedTemp::Stack(slot) => write!(f, "%t{} -> stack[{}]", temp.id(), slot.0)?,
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
struct ActiveTemp {
    temp_id: IrTempId,
    interval: LiveInterval,
    reg: Arm64Reg,
}

type ActiveSet = HashMap<IrTempId, ActiveTemp>;

#[derive(Debug)]
struct PosIndexMap {
    pos_to_idx: HashMap<InstPos, usize>,
    idx_to_pos: Vec<InstPos>,
}

#[derive(Debug, PartialEq, Eq)]
enum PosEventKind {
    IntervalStart {
        temp_id: IrTempId,
        interval: LiveInterval,
        is_param: bool,
    },
    IntervalEnd {
        temp_id: IrTempId,
        interval: LiveInterval,
    },
    Call {
        constr_idx: usize,
    },
}

impl fmt::Display for PosEventKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PosEventKind::IntervalStart { temp_id, is_param, .. } => {
                write!(f, "IntervalStart(%t{})", temp_id.id())?;
                if *is_param {
                    write!(f, " (param)")?;
                }
                Ok(())
            }
            PosEventKind::IntervalEnd { temp_id, .. } => {
                write!(f, "IntervalEnd(%t{})", temp_id.id())
            }
            PosEventKind::Call { constr_idx } => write!(f, "Call[{}]", constr_idx),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct PosEvent {
    inst_idx: usize,
    kind: PosEventKind,
}

impl fmt::Display for PosEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.inst_idx, self.kind)?;
        Ok(())
    }
}

impl PartialOrd for PosEvent {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PosEvent {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // sort by:
        // 1. inst_idx
        // 2. kind: IntervalEnd < Call < IntervalStart
        // 3. temp_id (for deterministic ordering when inst_idx and priority are equal)
        self.inst_idx
            .cmp(&other.inst_idx)
            .then_with(|| self.kind.priority().cmp(&other.kind.priority()))
            .then_with(|| self.kind.temp_id().cmp(&other.kind.temp_id()))
    }
}

impl PosEventKind {
    fn priority(&self) -> u8 {
        if let PosEventKind::IntervalStart { is_param, .. } = self {
            if *is_param {
                return 0;
            }
        }
        match self {
            PosEventKind::IntervalEnd { .. } => 0,
            PosEventKind::Call { .. } => 1,
            PosEventKind::IntervalStart { .. } => 2,
        }
    }

    fn temp_id(&self) -> u32 {
        match self {
            PosEventKind::IntervalStart { temp_id, .. } => temp_id.id(),
            PosEventKind::IntervalEnd { temp_id, .. } => temp_id.id(),
            PosEventKind::Call { constr_idx } => {
                panic!("Call event has no temp_id: {}", constr_idx)
            }
        }
    }
}

pub struct AllocationResult {
    pub alloc_map: TempAllocMap,
    pub moves: FnMoveList,
    pub frame_size: u32,
}

pub struct RegAlloc<'a> {
    func: &'a IrFunction,
    constraints: &'a ConstraintMap,
    pos_map: PosIndexMap,
    active_set: ActiveSet,
    alloc_map: TempAllocMap,
    spill_alloc: SpillAllocator,
    moves: FnMoveList,
}

impl<'a> RegAlloc<'a> {
    pub fn new(func: &'a IrFunction, constraints: &'a ConstraintMap) -> Self {
        Self {
            func,
            constraints,
            pos_map: Self::build_pos_map(func),
            active_set: ActiveSet::new(),
            alloc_map: TempAllocMap::new(),
            spill_alloc: SpillAllocator::new(),
            moves: FnMoveList::new(),
        }
    }

    fn build_pos_map(func: &IrFunction) -> PosIndexMap {
        let mut pos_to_idx = HashMap::new();
        let mut idx_to_pos = Vec::new();
        let mut global_idx = 0;
        for (block_id, block) in func.blocks.iter() {
            for (inst_idx, _) in block.insts.iter().enumerate() {
                let pos = InstPos::new(*block_id, inst_idx);
                pos_to_idx.insert(pos, global_idx);
                idx_to_pos.push(pos);
                global_idx += 1;
            }
        }
        PosIndexMap {
            pos_to_idx,
            idx_to_pos,
        }
    }

    fn initial_free_regs(&self) -> Vec<Arm64Reg> {
        CALLER_SAVED_REGS.to_vec()
    }

    fn handle_interval_end(&mut self, free_regs: &mut VecDeque<Arm64Reg>, temp_id: IrTempId) {
        // If the temp was spilled, it's no longer in the active set, so we can skip it.
        if let Some(expired) = self.active_set.remove(&temp_id) {
            // Keep this register hot for future intervals.
            free_regs.push_front(expired.reg);
        }
    }

    fn insert_into_active_set(&mut self, active_temp: ActiveTemp) {
        self.active_set.insert(active_temp.temp_id, active_temp);
    }

    fn spill(&mut self, active_temp: ActiveTemp) {
        let stack_slot = self.spill_alloc.alloc_slot();
        self.alloc_map
            .insert(active_temp.temp_id, MappedTemp::Stack(stack_slot));
        self.active_set.remove(&active_temp.temp_id);
    }

    fn assign_reg(&mut self, temp_id: IrTempId, interval: LiveInterval, reg: Arm64Reg) {
        self.alloc_map.insert(temp_id, MappedTemp::Reg(reg));
        self.insert_into_active_set(ActiveTemp {
            temp_id,
            interval,
            reg,
        });
    }

    fn alloc_param_regs(
        &mut self,
        free_regs: &mut VecDeque<Arm64Reg>,
        intervals: &LiveIntervalMap,
    ) {
        for param_constraint in self.constraints.fn_param_constraints.iter() {
            let temp_id = param_constraint.temp;
            let reg = param_constraint.reg;
            let interval = intervals.get(&temp_id).expect(&format!(
                "Param temp {} not found in intervals",
                temp_id.id()
            ));
            self.assign_reg(temp_id, *interval, reg);
            free_regs.retain(|r| *r != reg);
        }
    }

    fn build_pos_events(&self, intervals: &LiveIntervalMap, param_constraints: &[FnParamConstraint]) -> Vec<PosEvent> {
        let mut events = Vec::new();

        // Add interval start/end events
        for (temp_id, interval) in intervals.iter() {
            let is_param = param_constraints.iter().any(|c| c.temp == *temp_id);
            events.push(PosEvent {
                inst_idx: interval.start as usize,
                kind: PosEventKind::IntervalStart {
                    temp_id: *temp_id,
                    interval: *interval,
                    is_param,
                },
            });
            events.push(PosEvent {
                inst_idx: interval.end as usize,
                kind: PosEventKind::IntervalEnd {
                    temp_id: *temp_id,
                    interval: *interval,
                },
            });
        }

        // Add call events
        for (constr_idx, call_constraint) in self.constraints.call_constraints.iter().enumerate() {
            events.push(PosEvent {
                inst_idx: self.pos_map.pos_to_idx[&call_constraint.pos],
                kind: PosEventKind::Call { constr_idx },
            });
        }

        events.sort();
        events
    }

    fn handle_interval_start(
        &mut self,
        free_regs: &mut VecDeque<Arm64Reg>,
        temp_id: IrTempId,
        interval: LiveInterval,
    ) {
        // Skip if already allocated (e.g., function params)
        if self.alloc_map.contains_key(&temp_id) {
            return;
        }

        match free_regs.pop_front() {
            Some(reg) => {
                // Allocate register to interval
                self.assign_reg(temp_id, interval, reg);
            }
            None => {
                // Choose a victim from the active set (the one with the highest end)
                let victim = *self
                    .active_set
                    .values()
                    .max_by_key(|a| a.interval.end)
                    .expect("active set should not be empty when spilling");

                if victim.interval.end <= interval.end {
                    // Spill current since it lives longer (it doesn't enter the active set)
                    let stack_slot = self.spill_alloc.alloc_slot();
                    self.alloc_map
                        .insert(temp_id, MappedTemp::Stack(stack_slot));
                } else {
                    // Spill victim, give its reg to current
                    self.assign_reg(temp_id, interval, victim.reg);
                    self.spill(victim);
                }
            }
        }
    }

    fn handle_call(&mut self, constr: &CallConstraint) {
        // 1. Move args into their respective registers before call
        for arg_constr in &constr.args {
            if let IrOperand::Temp(temp_id) = arg_constr.operand {
                let current_loc = match self.alloc_map.get(&temp_id) {
                    Some(MappedTemp::Reg(reg)) => Location::Reg(*reg),
                    Some(MappedTemp::Stack(slot)) => Location::Stack(*slot),
                    None => panic!("Temp {} not found in alloc map", temp_id.id()),
                };
                let target_loc = Location::Reg(arg_constr.reg);

                // If not already in the target location, add a move
                if current_loc != target_loc {
                    self.moves.add_inst_move(
                        RelInstPos::Before(constr.pos),
                        current_loc,
                        target_loc,
                    );
                }
            }
        }

        // 2. Move result into the result register after call
        if let Some(result_constr) = &constr.result {
            let result_temp = result_constr.temp;
            if let Some(allocated_temp) = self.alloc_map.get(&result_temp) {
                let source_loc = Location::Reg(result_constr.reg);
                let target_loc = match allocated_temp {
                    MappedTemp::Reg(reg) => Location::Reg(*reg),
                    MappedTemp::Stack(slot) => Location::Stack(*slot),
                };
                if source_loc != target_loc {
                    self.moves
                        .add_inst_move(RelInstPos::After(constr.pos), source_loc, target_loc);
                }
            }
        }

        // 3. Handle caller-saved register preservation
        // TODO: implement this
    }

    // Note: this consumes self, rendering it unusable after calling this method.
    pub fn alloc(self) -> AllocationResult {
        let free_regs = self.initial_free_regs();
        self.alloc_into(free_regs)
    }

    // Note: this consumes self, rendering it unusable after calling this method.
    pub fn alloc_into(mut self, free_regs: Vec<Arm64Reg>) -> AllocationResult {
        // Assumes there is at least one allocatable register.
        assert!(
            !free_regs.is_empty(),
            "RegAlloc::alloc_into called with an empty register list"
        );

        // 1. Build the live map and intervals
        let live_map = LivenessAnalysis::new(self.func.clone()).analyze();
        let intervals = build_live_intervals(self.func, &live_map);

        // 2. Initialize free registers
        let mut free_regs = VecDeque::from(free_regs);

        // 3. Pre-allocate parameters to ABI registers
        self.alloc_param_regs(&mut free_regs, &intervals);

        // 4. Build instruction position events
        let pos_events = self.build_pos_events(&intervals, &self.constraints.fn_param_constraints);

        // 5. Process events
        for event in pos_events {
            match event.kind {
                PosEventKind::IntervalEnd { temp_id, .. } => {
                    self.handle_interval_end(&mut free_regs, temp_id);
                }
                PosEventKind::Call { constr_idx } => {
                    let constr = &self.constraints.call_constraints[constr_idx];
                    self.handle_call(constr);
                }
                PosEventKind::IntervalStart { temp_id, interval, .. } => {
                    self.handle_interval_start(&mut free_regs, temp_id, interval);
                }
            }
        }

        AllocationResult {
            alloc_map: self.alloc_map.clone(),
            moves: self.moves,
            frame_size: self.spill_alloc.frame_size_bytes(),
        }
    }
}

#[cfg(test)]
#[path = "../tests/t_regalloc.rs"]
mod tests;
