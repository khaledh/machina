//! Minimal linear-scan register allocator for SSA values.

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

use crate::regalloc::stack::StackAllocator;
use crate::regalloc::target::{PhysReg, TargetSpec};
use crate::ssa::IrTypeCache;
use crate::ssa::model::ir::ValueId;

use super::constraints::AbiConstraints;
use super::intervals::{IntervalAnalysis, LiveInterval};
use super::{AllocationResult, Location, ValueAllocMap};

pub struct LinearScan<'a> {
    analysis: &'a IntervalAnalysis,
    allocatable: Vec<PhysReg>,
    call_safe: HashSet<PhysReg>,
    slot_sizes: HashMap<ValueId, u32>,
    fixed_regs: HashMap<ValueId, PhysReg>,
    needs_stack: HashSet<ValueId>,
}

impl<'a> LinearScan<'a> {
    /// Build a linear-scan allocator with interval analysis and ABI constraints.
    pub fn new(
        analysis: &'a IntervalAnalysis,
        types: &'a mut IrTypeCache,
        target: &dyn TargetSpec,
        constraints: &AbiConstraints,
    ) -> Self {
        let caller_saved: HashSet<PhysReg> = target.caller_saved().iter().copied().collect();
        let mut caller_saved_regs = Vec::new();
        let mut callee_saved_regs = Vec::new();
        for reg in target.allocatable_regs() {
            if caller_saved.contains(reg) {
                caller_saved_regs.push(*reg);
            } else {
                callee_saved_regs.push(*reg);
            }
        }
        // Prefer caller-saved regs for short-lived values by making them pop first.
        let mut allocatable = callee_saved_regs;
        allocatable.extend(caller_saved_regs);
        // Call-safe regs exclude caller-saved regs when an interval crosses a call.
        let call_safe = allocatable
            .iter()
            .copied()
            .filter(|reg| !caller_saved.contains(reg))
            .collect();

        // Precompute spill slot sizes per SSA value (in 8-byte slots).
        let mut slot_sizes = HashMap::new();
        let mut needs_stack = HashSet::new();
        for (value, ty) in &analysis.value_types {
            let slots = stack_slots_for(types, *ty);
            slot_sizes.insert(*value, slots);
            if !types.is_reg_type(*ty) {
                needs_stack.insert(*value);
            }
        }

        Self {
            analysis,
            allocatable,
            call_safe,
            slot_sizes,
            fixed_regs: constraints.fixed_regs.clone(),
            needs_stack,
        }
    }

    /// Allocate registers or spill slots for all SSA values in the function.
    pub fn alloc(mut self) -> AllocationResult {
        let mut allocator = StackAllocator::new();
        let mut alloc_map: ValueAllocMap = ValueAllocMap::new();

        // Sort intervals by start time.
        let mut ordered: Vec<(ValueId, LiveInterval)> = self
            .analysis
            .intervals
            .iter()
            .map(|(id, iv)| (*id, *iv))
            .collect();
        ordered.sort_by(|a, b| match a.1.start.cmp(&b.1.start) {
            Ordering::Equal => a.1.end.cmp(&b.1.end),
            other => other,
        });

        // Track active intervals.
        let mut active: Vec<Active> = Vec::new();

        for (value, interval) in ordered {
            expire_old(&mut active, interval.start, &mut self.allocatable);

            if self.needs_stack.contains(&value) {
                let slot = alloc_stack_for(value, &self.slot_sizes, &mut allocator);
                alloc_map.insert(value, Location::Stack(slot));
                continue;
            }

            // Fixed registers must be assigned even if they displace other intervals.
            if let Some(reg) = self.fixed_regs.get(&value).copied() {
                force_assign_fixed(
                    value,
                    interval,
                    reg,
                    &mut active,
                    &mut alloc_map,
                    &mut self.allocatable,
                    &mut allocator,
                    &self.slot_sizes,
                );
                continue;
            }

            // Intervals crossing a call must avoid caller-saved registers.
            let needs_call_safe = interval_crosses_call(interval, &self.analysis.call_positions);
            let reg = if needs_call_safe {
                take_free_reg_matching(&mut self.allocatable, &self.call_safe)
            } else {
                take_free_reg(&mut self.allocatable)
            };

            if let Some(reg) = reg {
                active.push(Active {
                    value,
                    interval,
                    location: Location::Reg(reg),
                });
                alloc_map.insert(value, Location::Reg(reg));
            } else {
                // Spill either the current interval or the furthest-ending active one.
                spill_at_interval(
                    value,
                    interval,
                    &mut active,
                    &mut alloc_map,
                    &mut allocator,
                    &self.slot_sizes,
                    needs_call_safe.then_some(&self.call_safe),
                );
            }

            active.sort_by_key(|entry| entry.interval.end);
        }

        AllocationResult {
            alloc_map,
            frame_size: allocator.frame_size_bytes(),
            stack_slot_count: allocator.total_slots(),
            used_callee_saved: Vec::new(),
            edge_moves: Vec::new(),
            call_moves: Vec::new(),
        }
    }
}

#[derive(Clone)]
struct Active {
    value: ValueId,
    interval: LiveInterval,
    location: Location,
}

/// Expire any active intervals that end before the next interval starts,
/// returning their registers to the free list.
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

/// Assign a register or spill slot for an incoming interval by possibly
/// spilling an active interval.
fn spill_at_interval(
    value: ValueId,
    interval: LiveInterval,
    active: &mut Vec<Active>,
    alloc_map: &mut ValueAllocMap,
    allocator: &mut StackAllocator,
    slot_sizes: &HashMap<ValueId, u32>,
    allowed_regs: Option<&HashSet<PhysReg>>,
) {
    // Choose a spill candidate among active intervals, preferring the one with
    // the latest end that is also in an allowed register set (if provided).
    let mut spill_idx = None;
    let mut spill_entry = None;

    for (idx, entry) in active.iter().enumerate() {
        if let Location::Reg(reg) = entry.location {
            if allowed_regs.map_or(true, |set| set.contains(&reg)) {
                if spill_entry
                    .map(|spill: &Active| entry.interval.end > spill.interval.end)
                    .unwrap_or(true)
                {
                    spill_idx = Some(idx);
                    spill_entry = Some(entry);
                }
            }
        }
    }

    // If the chosen spill candidate lives longer than the incoming interval,
    // spill it and reuse its register for the incoming value.
    if let (Some(spill_idx), Some(spill)) = (spill_idx, spill_entry) {
        if spill.interval.end > interval.end {
            let spill_slot = alloc_stack_for(spill.value, slot_sizes, allocator);
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
            return;
        }
    }

    // Otherwise spill the incoming value to its own stack slot.
    let slot = alloc_stack_for(value, slot_sizes, allocator);
    alloc_map.insert(value, Location::Stack(slot));
}

/// Force a fixed register assignment, spilling any active interval that
/// currently occupies that register.
fn force_assign_fixed(
    value: ValueId,
    interval: LiveInterval,
    reg: PhysReg,
    active: &mut Vec<Active>,
    alloc_map: &mut ValueAllocMap,
    free_regs: &mut Vec<PhysReg>,
    allocator: &mut StackAllocator,
    slot_sizes: &HashMap<ValueId, u32>,
) {
    // Evict any active interval currently occupying the fixed register.
    if let Some(idx) = active
        .iter()
        .position(|entry| entry.location == Location::Reg(reg))
    {
        spill_active_at(idx, active, alloc_map, allocator, slot_sizes);
    }

    // Remove the fixed reg from the free list if present.
    if let Some(pos) = free_regs.iter().position(|r| *r == reg) {
        free_regs.swap_remove(pos);
    }

    active.push(Active {
        value,
        interval,
        location: Location::Reg(reg),
    });
    alloc_map.insert(value, Location::Reg(reg));
    active.sort_by_key(|entry| entry.interval.end);
}

/// Spill a specific active interval to a stack slot.
fn spill_active_at(
    idx: usize,
    active: &mut Vec<Active>,
    alloc_map: &mut ValueAllocMap,
    allocator: &mut StackAllocator,
    slot_sizes: &HashMap<ValueId, u32>,
) {
    let entry = active.remove(idx);
    let slot = alloc_stack_for(entry.value, slot_sizes, allocator);
    alloc_map.insert(entry.value, Location::Stack(slot));
}

/// Returns true if the interval covers any call instruction position.
fn interval_crosses_call(interval: LiveInterval, call_positions: &[u32]) -> bool {
    call_positions
        .iter()
        .any(|pos| *pos >= interval.start && *pos < interval.end)
}

/// Pop any free register from the list.
fn take_free_reg(free_regs: &mut Vec<PhysReg>) -> Option<PhysReg> {
    free_regs.pop()
}

/// Pop a free register that is contained in the allowed set.
fn take_free_reg_matching(
    free_regs: &mut Vec<PhysReg>,
    allowed: &HashSet<PhysReg>,
) -> Option<PhysReg> {
    for idx in 0..free_regs.len() {
        if allowed.contains(&free_regs[idx]) {
            return Some(free_regs.swap_remove(idx));
        }
    }
    None
}

/// Compute how many 8-byte stack slots a value of the given type needs.
fn stack_slots_for(types: &mut IrTypeCache, ty: crate::ssa::IrTypeId) -> u32 {
    let size = types.layout(ty).size();
    let slots = (size + 7) / 8;
    slots.max(1) as u32
}

/// Allocate a stack slot range for a value, based on its precomputed size.
fn alloc_stack_for(
    value: ValueId,
    slot_sizes: &HashMap<ValueId, u32>,
    allocator: &mut StackAllocator,
) -> crate::regalloc::stack::StackSlotId {
    let slots = slot_sizes.get(&value).copied().unwrap_or(1);
    allocator.alloc_slots(slots)
}
