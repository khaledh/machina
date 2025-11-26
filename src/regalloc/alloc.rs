use std::collections::{HashMap, VecDeque};
use std::fmt;

use crate::dataflow::liveness::{
    LiveInterval, LiveIntervalMap, LivenessAnalysis, build_live_intervals,
};
use crate::ir::types::{IrFunction, IrTempId, IrTempRole};
use crate::regalloc::regs::{self, Arm64Reg, CALLER_SAVED_REGS};

#[derive(Debug, Clone, Copy)]
pub struct StackSlotId(u32);

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

type ActiveSet = VecDeque<ActiveTemp>;

pub struct RegAlloc {
    active_set: ActiveSet,
    alloc_map: TempAllocMap,
    next_stack_slot: u32,
}

impl RegAlloc {
    pub fn new() -> Self {
        Self {
            active_set: ActiveSet::new(),
            alloc_map: TempAllocMap::new(),
            next_stack_slot: 0,
        }
    }

    fn new_stack_slot(&mut self) -> StackSlotId {
        let slot = StackSlotId(self.next_stack_slot);
        self.next_stack_slot += 1;
        slot
    }

    fn initial_free_regs(&self) -> Vec<Arm64Reg> {
        CALLER_SAVED_REGS.to_vec()
    }

    fn sort_intervals(intervals: &LiveIntervalMap) -> Vec<(IrTempId, LiveInterval)> {
        let mut intervals: Vec<_> = intervals
            .iter()
            .map(|(temp_id, interval)| (*temp_id, *interval))
            .collect();
        intervals.sort_by_key(|&(_, interval)| interval.start);
        intervals
    }

    fn expire_old_intervals(&mut self, free_regs: &mut VecDeque<Arm64Reg>, current_start: u32) {
        // Expire intervals in order of increasing end position. Because
        // `active_set` is kept sorted by `end`, we can stop at the first
        // interval that has not yet expired.
        while let Some(a) = self.active_set.front() {
            if a.interval.end >= current_start {
                // All remaining intervals are still active.
                break;
            }
            // front interval has ended, remove it and return its register to the
            // free list.
            let expired = self
                .active_set
                .pop_front()
                .expect("front element must exist after peek");
            // Keep this register hot for future intervals.
            free_regs.push_front(expired.reg);
        }
    }

    fn insert_into_active_set(&mut self, active_temp: ActiveTemp) {
        let index = self
            .active_set
            .partition_point(|a| a.interval.end <= active_temp.interval.end);
        self.active_set.insert(index, active_temp);
    }

    fn spill(&mut self, active_temp: ActiveTemp) {
        let stack_slot = self.new_stack_slot();
        self.alloc_map
            .insert(active_temp.temp_id, MappedTemp::Stack(stack_slot));
        self.active_set.retain(|a| {
            !(a.temp_id == active_temp.temp_id && a.interval.end == active_temp.interval.end)
        });
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
        func: &IrFunction,
        free_regs: &mut VecDeque<Arm64Reg>,
        intervals: &LiveIntervalMap,
    ) {
        for (i, temp) in func.temps.iter().enumerate() {
            let temp_id = IrTempId(i as u32);
            if let IrTempRole::Param { index } = temp.role {
                if index > 8 {
                    panic!(
                        "Only 8 parameters are supported. Stack parameters are not supported yet."
                    );
                }
                let reg = regs::get_param_reg(index);
                let interval = intervals.get(&temp_id).expect(&format!(
                    "Param temp {} not found in intervals",
                    temp_id.id()
                ));
                self.assign_reg(temp_id, *interval, reg);
                // remove the param from the free list
                free_regs.retain(|r| *r != reg);
            }
        }
    }

    pub fn alloc(&mut self, func: &IrFunction) -> TempAllocMap {
        self.alloc_into(func, self.initial_free_regs())
    }

    pub fn alloc_into(&mut self, func: &IrFunction, regs: Vec<Arm64Reg>) -> TempAllocMap {
        // Assumes there is at least one allocatable register.
        assert!(
            !regs.is_empty(),
            "RegAlloc::alloc_into called with an empty register list"
        );
        // 1. Build the live map and intervals
        let live_map = LivenessAnalysis::new(func.clone()).analyze();
        let intervals = build_live_intervals(func, &live_map);

        // 2. Sort intervals by start pos
        let mut sorted_intervals = Self::sort_intervals(&intervals);

        // 3. Initialize free registers
        let mut free_regs = VecDeque::from(regs);

        // 4. Pre-allocate parameters to ABI registers
        self.alloc_param_regs(func, &mut free_regs, &intervals);

        // 5. Scan intervals from earliest to latest
        for (temp_id, interval) in sorted_intervals.drain(..) {
            // 5.1. Expire old intervals
            self.expire_old_intervals(&mut free_regs, interval.start);

            match free_regs.pop_front() {
                Some(reg) => {
                    // 5.2. Allocate register to interval
                    self.assign_reg(temp_id, interval, reg);
                }
                None => {
                    // 5.3. No free registers, we need to spill

                    // Choose a victim from the active set (the one with the highest end)
                    let victim = *self
                        .active_set
                        .back()
                        .expect("active set should not be empty when spilling");

                    if victim.interval.end <= interval.end {
                        // Spill current since it lives longer (it doesn't enter the active set)
                        let stack_slot = self.new_stack_slot();
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

        self.alloc_map.clone()
    }
}

#[cfg(test)]
#[path = "../tests/t_regalloc.rs"]
mod tests;
