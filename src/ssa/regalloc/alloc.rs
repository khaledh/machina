//! Minimal linear-scan register allocator for SSA values.

use std::cmp::Ordering;

use crate::regalloc::stack::StackAllocator;
use crate::regalloc::target::{PhysReg, TargetSpec};
use crate::ssa::model::ir::ValueId;

use super::intervals::{LiveInterval, LiveIntervalMap};
use super::{AllocationResult, Location, ValueAllocMap};

pub struct LinearScan<'a> {
    intervals: &'a LiveIntervalMap,
    allocatable: Vec<PhysReg>,
}

impl<'a> LinearScan<'a> {
    pub fn new(intervals: &'a LiveIntervalMap, target: &dyn TargetSpec) -> Self {
        Self {
            intervals,
            allocatable: target.allocatable_regs().to_vec(),
        }
    }

    pub fn alloc(mut self) -> AllocationResult {
        let mut allocator = StackAllocator::new();
        let mut alloc_map: ValueAllocMap = ValueAllocMap::new();

        let mut ordered: Vec<(ValueId, LiveInterval)> =
            self.intervals.iter().map(|(id, iv)| (*id, *iv)).collect();
        ordered.sort_by(|a, b| match a.1.start.cmp(&b.1.start) {
            Ordering::Equal => a.1.end.cmp(&b.1.end),
            other => other,
        });

        let mut active: Vec<Active> = Vec::new();

        for (value, interval) in ordered {
            expire_old(&mut active, interval.start, &mut self.allocatable);

            if let Some(reg) = self.allocatable.pop() {
                active.push(Active {
                    value,
                    interval,
                    location: Location::Reg(reg),
                });
                alloc_map.insert(value, Location::Reg(reg));
            } else {
                spill_at_interval(value, interval, &mut active, &mut alloc_map, &mut allocator);
            }

            active.sort_by_key(|entry| entry.interval.end);
        }

        AllocationResult {
            alloc_map,
            frame_size: allocator.frame_size_bytes(),
            stack_slot_count: allocator.total_slots(),
        }
    }
}

#[derive(Clone)]
struct Active {
    value: ValueId,
    interval: LiveInterval,
    location: Location,
}

fn expire_old(active: &mut Vec<Active>, start: u32, free_regs: &mut Vec<PhysReg>) {
    let mut idx = 0;
    while idx < active.len() {
        if active[idx].interval.end > start {
            idx += 1;
            continue;
        }

        if let Location::Reg(reg) = active[idx].location {
            free_regs.push(reg);
        }
        active.remove(idx);
    }
}

fn spill_at_interval(
    value: ValueId,
    interval: LiveInterval,
    active: &mut Vec<Active>,
    alloc_map: &mut ValueAllocMap,
    allocator: &mut StackAllocator,
) {
    if let Some((spill_idx, spill)) = active
        .iter()
        .enumerate()
        .max_by_key(|(_, entry)| entry.interval.end)
    {
        if spill.interval.end > interval.end {
            let spill_slot = allocator.alloc_slot();
            alloc_map.insert(spill.value, Location::Stack(spill_slot));

            let reg = match spill.location {
                Location::Reg(reg) => reg,
                _ => return,
            };

            active.remove(spill_idx);
            active.push(Active {
                value,
                interval,
                location: Location::Reg(reg),
            });
            alloc_map.insert(value, Location::Reg(reg));
        } else {
            let slot = allocator.alloc_slot();
            alloc_map.insert(value, Location::Stack(slot));
        }
    } else {
        let slot = allocator.alloc_slot();
        alloc_map.insert(value, Location::Stack(slot));
    }
}
