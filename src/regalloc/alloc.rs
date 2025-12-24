use std::collections::{HashMap, HashSet, VecDeque};

use crate::mcir::types::{
    BlockId, FuncBody, LocalId, LocalKind, PlaceAny, Rvalue, Statement, TyId, TyKind, TyTable,
};
use crate::regalloc::constraints::{CallConstraint, ConstraintMap, FnParamConstraint};
use crate::regalloc::liveness::{
    LiveInterval, LiveIntervalMap, LivenessAnalysis, build_live_intervals,
};
use crate::regalloc::moves::{FnMoveList, Location};
use crate::regalloc::pos::{InstPos, RelInstPos};
use crate::regalloc::stack::{StackAllocator, StackSlotId};
use crate::regalloc::target::{PhysReg, TargetSpec};
use crate::regalloc::{AllocationResult, LocalAllocMap, LocalClass, MappedLocal};

// --- Local classification ---

fn record_addr_of(place: &PlaceAny, out: &mut HashSet<LocalId>) {
    let base = match place {
        PlaceAny::Scalar(p) => p.base(),
        PlaceAny::Aggregate(p) => p.base(),
    };
    out.insert(base);
}

fn scan_address_taken(body: &FuncBody) -> HashSet<LocalId> {
    let mut out = HashSet::new();
    for block in &body.blocks {
        for stmt in &block.stmts {
            if let Statement::CopyScalar {
                src: Rvalue::AddrOf(place),
                ..
            } = stmt
            {
                record_addr_of(place, &mut out);
            }
        }
    }
    out
}

pub(crate) fn classify_locals(body: &FuncBody) -> Vec<LocalClass> {
    let addr_taken = scan_address_taken(body);
    body.locals
        .iter()
        .enumerate()
        .map(|(i, local)| {
            let id = LocalId(i as u32);
            let ty_info = body.types.get(local.ty);
            if (matches!(local.kind, LocalKind::Param { .. })
                || matches!(local.kind, LocalKind::Return))
                && ty_info.is_aggregate()
            {
                LocalClass::Reg
            } else if ty_info.is_aggregate() || addr_taken.contains(&id) {
                LocalClass::StackAddr
            } else {
                LocalClass::Reg
            }
        })
        .collect()
}

// --- Type sizing helpers ---

fn size_of_ty(types: &TyTable, ty: TyId) -> usize {
    match types.kind(ty) {
        TyKind::Unit => 0,
        TyKind::Bool => 1,
        TyKind::Char => 1,
        TyKind::Int { bits, .. } => (*bits as usize).div_ceil(8),
        TyKind::Array { elem_ty, dims } => {
            let elems: usize = dims.iter().product();
            elems * size_of_ty(types, *elem_ty)
        }
        TyKind::Tuple { field_tys } => field_tys.iter().map(|ty| size_of_ty(types, *ty)).sum(),
        TyKind::Struct { fields } => fields.iter().map(|field| size_of_ty(types, field.ty)).sum(),
    }
}

fn slots_for_ty(types: &TyTable, ty: TyId) -> u32 {
    let size = size_of_ty(types, ty);
    let slots = size.div_ceil(8);
    (slots.max(1)) as u32
}

// --- Linear scan allocator ---

#[derive(Debug, Clone, Copy)]
struct ActiveLocal {
    local: LocalId,
    interval: LiveInterval,
    reg: PhysReg,
}

type ActiveSet = HashMap<LocalId, ActiveLocal>;

#[derive(Debug)]
struct PosIndexMap {
    pos_to_idx: HashMap<InstPos, usize>,
}

#[derive(Debug, PartialEq, Eq)]
enum PosEventKind {
    IntervalStart {
        local: LocalId,
        interval: LiveInterval,
        is_param: bool,
    },
    IntervalEnd {
        local: LocalId,
        interval: LiveInterval,
    },
    Call {
        constr_idx: usize,
    },
}

impl PosEventKind {
    fn priority(&self) -> u8 {
        if let PosEventKind::IntervalStart { is_param: true, .. } = self {
            0
        } else {
            match self {
                PosEventKind::IntervalEnd { .. } => 0,
                PosEventKind::IntervalStart { .. } => 1,
                PosEventKind::Call { .. } => 2,
            }
        }
    }

    fn local_id(&self) -> u32 {
        match self {
            PosEventKind::IntervalStart { local, .. } => local.0,
            PosEventKind::IntervalEnd { local, .. } => local.0,
            PosEventKind::Call { .. } => u32::MAX,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct PosEvent {
    inst_idx: usize,
    kind: PosEventKind,
}

impl Ord for PosEvent {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.inst_idx
            .cmp(&other.inst_idx)
            .then_with(|| self.kind.priority().cmp(&other.kind.priority()))
            .then_with(|| self.kind.local_id().cmp(&other.kind.local_id()))
    }
}

impl PartialOrd for PosEvent {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub struct RegAlloc<'a> {
    body: &'a FuncBody,
    constraints: &'a ConstraintMap,
    target: &'a dyn TargetSpec,
    local_classes: Vec<LocalClass>,
    pos_map: PosIndexMap,
    active_set: ActiveSet,
    alloc_map: LocalAllocMap,
    stack_alloc: StackAllocator,
    moves: FnMoveList,
    used_callee_saved: HashSet<PhysReg>,
    ret_interval_start: Option<u32>,
}

impl<'a> RegAlloc<'a> {
    pub fn new(
        body: &'a FuncBody,
        constraints: &'a ConstraintMap,
        target: &'a dyn TargetSpec,
    ) -> Self {
        Self {
            body,
            constraints,
            target,
            local_classes: classify_locals(body),
            pos_map: Self::build_pos_map(body),
            active_set: ActiveSet::new(),
            alloc_map: LocalAllocMap::new(),
            stack_alloc: StackAllocator::new(),
            moves: FnMoveList::new(),
            used_callee_saved: HashSet::new(),
            ret_interval_start: None,
        }
    }

    fn build_pos_map(body: &FuncBody) -> PosIndexMap {
        let mut pos_to_idx = HashMap::new();
        let mut global_idx = 0;
        for (block_idx, block) in body.blocks.iter().enumerate() {
            let block_id = BlockId(block_idx as u32);
            for (inst_idx, _) in block.stmts.iter().enumerate() {
                let pos = InstPos::new(block_id, inst_idx);
                pos_to_idx.insert(pos, global_idx);
                global_idx += 1;
            }
            global_idx += 1; // terminator slot
        }
        PosIndexMap { pos_to_idx }
    }

    fn initial_free_regs(&self) -> Vec<PhysReg> {
        let mut regs = self.target.allocatable_regs().to_vec();
        let scratch = self.target.scratch_regs();
        regs.retain(|r| !scratch.contains(r));
        regs
    }

    fn local_class(&self, local: LocalId) -> LocalClass {
        self.local_classes[local.index()]
    }

    fn assign_reg(&mut self, local: LocalId, interval: LiveInterval, reg: PhysReg) {
        self.alloc_map.insert(local, MappedLocal::Reg(reg));
        self.active_set.insert(
            local,
            ActiveLocal {
                local,
                interval,
                reg,
            },
        );

        if self.target.callee_saved().contains(&reg) {
            self.used_callee_saved.insert(reg);
        }
    }

    fn alloc_stack(&mut self, local: LocalId) {
        let slot = self.stack_alloc.alloc_slot();
        self.alloc_map.insert(local, MappedLocal::Stack(slot));
    }

    fn alloc_stack_addr(&mut self, local: LocalId) {
        let ty = self.body.locals[local.index()].ty;
        let slot_count = slots_for_ty(&self.body.types, ty);
        let start_slot = self.stack_alloc.alloc_slots(slot_count);
        self.alloc_map
            .insert(local, MappedLocal::StackAddr(start_slot));
    }

    fn alloc_param_regs(
        &mut self,
        free_regs: &mut VecDeque<PhysReg>,
        intervals: &LiveIntervalMap,
        param_constraints: &[FnParamConstraint],
    ) {
        for param in param_constraints.iter() {
            let local = param.local;
            if self.local_class(local) == LocalClass::StackAddr {
                if !self.alloc_map.contains_key(&local) {
                    self.alloc_stack_addr(local);
                }
                continue;
            }

            if let Some(interval) = intervals.get(&local) {
                self.assign_reg(local, *interval, param.reg);
                free_regs.retain(|r| *r != param.reg);
            }
        }
    }

    fn build_pos_events(
        &self,
        intervals: &LiveIntervalMap,
        param_constraints: &[FnParamConstraint],
    ) -> Vec<PosEvent> {
        let mut events = Vec::new();
        for (local, interval) in intervals.iter() {
            let is_param = param_constraints.iter().any(|c| c.local == *local);
            events.push(PosEvent {
                inst_idx: interval.start as usize,
                kind: PosEventKind::IntervalStart {
                    local: *local,
                    interval: *interval,
                    is_param,
                },
            });
            events.push(PosEvent {
                inst_idx: interval.end as usize,
                kind: PosEventKind::IntervalEnd {
                    local: *local,
                    interval: *interval,
                },
            });
        }

        for (constr_idx, call) in self.constraints.call_constraints.iter().enumerate() {
            events.push(PosEvent {
                inst_idx: self.pos_map.pos_to_idx[&call.pos],
                kind: PosEventKind::Call { constr_idx },
            });
        }

        events.sort();
        events
    }

    fn handle_interval_end(&mut self, free_regs: &mut VecDeque<PhysReg>, local: LocalId) {
        if let Some(expired) = self.active_set.remove(&local) {
            free_regs.push_front(expired.reg);
        }
    }

    fn loc_for_value(&self, local: LocalId) -> Location {
        match self.alloc_map.get(&local) {
            Some(MappedLocal::Reg(reg)) => Location::Reg(*reg),
            Some(MappedLocal::Stack(slot)) => Location::Stack(*slot),
            Some(MappedLocal::StackAddr(slot)) => Location::Stack(*slot),
            None => panic!("Local {} not found in alloc map", local.0),
        }
    }

    fn loc_for_addr(&self, local: LocalId) -> Location {
        match self.alloc_map.get(&local) {
            Some(MappedLocal::Reg(reg)) => Location::Reg(*reg),
            Some(MappedLocal::Stack(slot)) => {
                if self.local_class(local) == LocalClass::StackAddr {
                    Location::StackAddr(*slot)
                } else {
                    Location::Stack(*slot)
                }
            }
            Some(MappedLocal::StackAddr(slot)) => Location::StackAddr(*slot),
            None => panic!("Local {} not found in alloc map", local.0),
        }
    }

    fn handle_call(&mut self, constr: &CallConstraint) {
        let call_inst_idx = self.pos_map.pos_to_idx[&constr.pos];
        let mut caller_saved_preserves: Vec<(StackSlotId, PhysReg)> = Vec::new();
        let ret_local = self.body.ret_local;
        let ret_ty = self.body.locals[ret_local.index()].ty;
        let ret_is_agg = self.body.types.get(ret_ty).is_aggregate();

        // 1. Save caller-saved registers that are live across the call
        for active in self.active_set.values() {
            let kind = self.body.locals[active.local.index()].kind;
            let is_param_like = matches!(kind, LocalKind::Param { .. })
                || (ret_is_agg && active.local == ret_local);
            let live_before_call = active.interval.start < call_inst_idx as u32
                || (is_param_like && active.interval.start == call_inst_idx as u32);
            if self.target.caller_saved().contains(&active.reg)
                && live_before_call
                && (active.interval.end - 1) > call_inst_idx as u32
            {
                let stack_slot = self.stack_alloc.alloc_slot();
                self.moves.add_inst_move(
                    RelInstPos::Before(constr.pos),
                    Location::Reg(active.reg),
                    Location::Stack(stack_slot),
                );
                caller_saved_preserves.push((stack_slot, active.reg));
            }
        }

        // 2. Move arguments to their target locations
        for arg_constr in &constr.args {
            let current_loc = match arg_constr.kind {
                crate::regalloc::constraints::CallArgKind::Value => match &arg_constr.place {
                    PlaceAny::Scalar(place) => {
                        if place.projections().is_empty() {
                            let loc = self.loc_for_value(place.base());
                            match loc {
                                Location::StackAddr(_) => Location::PlaceValue(place.clone()),
                                _ => loc,
                            }
                        } else {
                            Location::PlaceValue(place.clone())
                        }
                    }
                    PlaceAny::Aggregate(place) => self.loc_for_value(place.base()),
                },
                crate::regalloc::constraints::CallArgKind::Addr => match &arg_constr.place {
                    PlaceAny::Aggregate(place) => {
                        if place.projections().is_empty() {
                            self.loc_for_addr(place.base())
                        } else {
                            Location::PlaceAddr(PlaceAny::Aggregate(place.clone()))
                        }
                    }
                    PlaceAny::Scalar(place) => Location::PlaceAddr(PlaceAny::Scalar(place.clone())),
                },
            };
            let target_loc = Location::Reg(arg_constr.reg);
            if current_loc != target_loc {
                self.moves
                    .add_inst_move(RelInstPos::Before(constr.pos), current_loc, target_loc);
            }
        }

        // 3. Move result from result register to its target location
        if let Some(result_constr) = &constr.result {
            let source_loc = Location::Reg(result_constr.reg);
            let target_loc = self.loc_for_value(result_constr.local);
            if source_loc != target_loc {
                self.moves
                    .add_inst_move(RelInstPos::After(constr.pos), source_loc, target_loc);
            }
        }

        // 4. Restore caller-saved registers
        for (stack_slot, reg) in caller_saved_preserves {
            self.moves.add_inst_move(
                RelInstPos::After(constr.pos),
                Location::Stack(stack_slot),
                Location::Reg(reg),
            );
        }
    }

    fn handle_interval_start(
        &mut self,
        free_regs: &mut VecDeque<PhysReg>,
        local: LocalId,
        interval: LiveInterval,
    ) {
        if self.alloc_map.contains_key(&local) {
            return;
        }

        // Prefer the return register for the return local when possible.
        if local == self.body.ret_local && self.local_class(local) == LocalClass::Reg {
            let ret_reg = self.target.result_reg();
            if free_regs.contains(&ret_reg) {
                free_regs.retain(|r| *r != ret_reg);
                self.assign_reg(local, interval, ret_reg);
                return;
            }
        }

        if self.local_class(local) == LocalClass::StackAddr {
            self.alloc_stack_addr(local);
        } else {
            self.alloc_reg(free_regs, local, interval);
        }
    }

    fn alloc_reg(
        &mut self,
        free_regs: &mut VecDeque<PhysReg>,
        local: LocalId,
        interval: LiveInterval,
    ) {
        // Optimization: If possible, prevent non-return locals from using the
        // return register if they overlap with the return value interval.
        if local != self.body.ret_local {
            if let Some(ret_start) = self.ret_interval_start {
                // We only care if this local interval overlaps with the ret value interval.
                if interval.end > ret_start {
                    let ret_reg = self.target.result_reg();
                    if free_regs.contains(&ret_reg) {
                        // Prefer non‑return regs if possible.
                        if let Some((idx, reg)) = free_regs
                            .iter()
                            .enumerate()
                            .find(|(_, r)| **r != ret_reg)
                            .map(|(idx, r)| (idx, *r))
                        {
                            free_regs.remove(idx);
                            self.assign_reg(local, interval, reg);
                            return;
                        }
                    }
                }
            }
        }

        match free_regs.pop_front() {
            Some(reg) => {
                self.assign_reg(local, interval, reg);
            }
            None => {
                let victim = *self
                    .active_set
                    .values()
                    .max_by_key(|a| a.interval.end)
                    .expect("active set should not be empty when spilling");

                if victim.interval.end <= interval.end {
                    self.alloc_stack(local);
                } else {
                    self.assign_reg(local, interval, victim.reg);
                    self.spill_local(victim);
                }
            }
        }
    }

    fn spill_local(&mut self, active: ActiveLocal) {
        self.alloc_stack(active.local);
        self.active_set.remove(&active.local);
    }

    fn process_pos_events(&mut self, pos_events: &[PosEvent], free_regs: &mut VecDeque<PhysReg>) {
        for event in pos_events {
            match event.kind {
                PosEventKind::IntervalEnd { local, .. } => {
                    self.handle_interval_end(free_regs, local);
                }
                PosEventKind::Call { constr_idx } => {
                    let constr = &self.constraints.call_constraints[constr_idx];
                    self.handle_call(constr);
                }
                PosEventKind::IntervalStart {
                    local, interval, ..
                } => {
                    self.handle_interval_start(free_regs, local, interval);
                }
            }
        }
    }

    fn add_return_moves(&mut self) {
        for (block_id, ret_constr) in self.constraints.fn_return_constraints.iter() {
            let operand_loc = self.loc_for_value(ret_constr.local);
            let target_loc = Location::Reg(ret_constr.reg);
            if operand_loc != target_loc {
                self.moves
                    .add_return_move(*block_id, operand_loc, target_loc);
            }
        }
    }

    pub fn alloc(self) -> AllocationResult {
        let free_regs = self.initial_free_regs();
        self.alloc_into(free_regs)
    }

    pub fn alloc_into(mut self, free_regs: Vec<PhysReg>) -> AllocationResult {
        assert!(
            !free_regs.is_empty(),
            "RegAlloc::alloc_into called with an empty register list"
        );

        let live_map = LivenessAnalysis::new(self.body).analyze();
        let intervals = build_live_intervals(self.body, &live_map);
        let ret_local = self.body.ret_local;
        let ret_ty = self.body.locals[ret_local.index()].ty;
        if self.body.types.get(ret_ty).is_scalar() {
            // Track when the return value becomes live to optimize for assigning it
            // the return register.
            self.ret_interval_start = intervals.get(&ret_local).map(|iv| iv.start);
        }

        let mut free_regs = VecDeque::from(free_regs);

        self.alloc_param_regs(
            &mut free_regs,
            &intervals,
            &self.constraints.fn_param_constraints,
        );

        let pos_events = self.build_pos_events(&intervals, &self.constraints.fn_param_constraints);
        self.process_pos_events(&pos_events, &mut free_regs);

        self.add_return_moves();

        let mut used_callee_saved: Vec<PhysReg> = self.used_callee_saved.into_iter().collect();
        used_callee_saved.sort_by_key(|r| r.0);

        let callee_saved_size = used_callee_saved.len() * 8;
        let stack_alloc_size = self.stack_alloc.frame_size_bytes();
        let frame_size = callee_saved_size as u32 + stack_alloc_size;

        AllocationResult {
            alloc_map: self.alloc_map.clone(),
            moves: self.moves,
            frame_size,
            used_callee_saved,
            stack_slot_count: self.stack_alloc.total_slots(),
        }
    }
}
