use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::mcir::types::{BlockId, Place, PlaceAny, Scalar};
use crate::regalloc::pos::{InstPos, RelInstPos};
use crate::regalloc::stack::StackSlotId;
use crate::regalloc::target::PhysReg;
use crate::resolve::DefId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Location {
    #[allow(dead_code)]
    Imm(i64),
    FuncAddr(DefId),
    Reg(PhysReg),
    Stack(StackSlotId),
    StackAddr(StackSlotId),
    PlaceAddr(PlaceAny),
    PlaceValue(Place<Scalar>),
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Location::Imm(value) => write!(f, "Imm({})", value),
            Location::FuncAddr(def) => write!(f, "FuncAddr({})", def),
            Location::Reg(reg) => write!(f, "Reg(r{})", reg.0),
            Location::Stack(slot) => write!(f, "Stack({})", slot.0),
            Location::StackAddr(slot) => write!(f, "StackAddr({})", slot.0),
            Location::PlaceAddr(place) => write!(f, "PlaceAddr({})", place),
            Location::PlaceValue(place) => write!(f, "PlaceValue({})", place),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Move {
    pub from: Location,
    pub to: Location,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.from, self.to)
    }
}

#[derive(Debug)]
pub struct InstMoveList {
    pub before_moves: Vec<Move>,
    pub after_moves: Vec<Move>,
}

#[derive(Debug)]
pub struct FnMoveList {
    inst_moves: HashMap<InstPos, InstMoveList>,
    return_moves: HashMap<BlockId, Move>,
}

impl Default for FnMoveList {
    fn default() -> Self {
        Self::new()
    }
}

impl FnMoveList {
    pub fn new() -> Self {
        Self {
            inst_moves: HashMap::new(),
            return_moves: HashMap::new(),
        }
    }

    pub fn add_inst_move(&mut self, rel_pos: RelInstPos, from: Location, to: Location) {
        let inst_pos = match rel_pos {
            RelInstPos::Before(pos) | RelInstPos::After(pos) => pos,
        };

        let move_list = self.inst_moves.entry(inst_pos).or_insert(InstMoveList {
            before_moves: vec![],
            after_moves: vec![],
        });

        match rel_pos {
            RelInstPos::Before(_) => move_list.before_moves.push(Move { from, to }),
            RelInstPos::After(_) => move_list.after_moves.push(Move { from, to }),
        }
    }

    pub fn add_return_move(&mut self, block_id: BlockId, from: Location, to: Location) {
        self.return_moves.insert(block_id, Move { from, to });
    }

    pub fn get_inst_moves(&self, inst_pos: InstPos) -> Option<&InstMoveList> {
        self.inst_moves.get(&inst_pos)
    }

    pub fn get_return_move(&self, block_id: BlockId) -> Option<&Move> {
        self.return_moves.get(&block_id)
    }

    pub fn resolve_parallel_moves<F>(&mut self, scratch_regs: &[PhysReg], mut extra_src_regs: F)
    where
        F: FnMut(&Move) -> Vec<PhysReg>,
    {
        if scratch_regs.is_empty() {
            return;
        }
        let scratch = scratch_regs[0];
        for inst_moves in self.inst_moves.values_mut() {
            resolve_move_list(&mut inst_moves.before_moves, scratch, &mut extra_src_regs);
            resolve_move_list(&mut inst_moves.after_moves, scratch, &mut extra_src_regs);
        }
    }
}

fn resolve_move_list<F>(moves: &mut Vec<Move>, scratch: PhysReg, extra_src_regs: &mut F)
where
    F: FnMut(&Move) -> Vec<PhysReg>,
{
    if moves.len() <= 1 {
        return;
    }

    // Move resolution is a topological sort with cycle breaking:
    // emit any move whose destination is not a source of any *other* pending move,
    // otherwise break a reg->reg cycle using a scratch register.
    let mut pending = std::mem::take(moves);
    let mut pending_srcs: Vec<HashSet<PhysReg>> = pending
        .iter()
        .map(|mov| {
            let mut regs = HashSet::new();
            if let Location::Reg(reg) = mov.from {
                regs.insert(reg);
            }
            for reg in extra_src_regs(mov) {
                regs.insert(reg);
            }
            regs
        })
        .collect();
    let mut src_counts: HashMap<PhysReg, usize> = HashMap::new();
    for regs in &pending_srcs {
        for reg in regs {
            *src_counts.entry(*reg).or_insert(0) += 1;
        }
    }
    let mut ordered = Vec::with_capacity(pending.len());

    while !pending.is_empty() {
        // Find a move whose destination doesn't clobber any source reg used by
        // other pending moves. A move is allowed to use its own destination as
        // a source (e.g., `PlaceAddr` with base in the same register).
        let mut ready_idx = None;
        for (idx, mov) in pending.iter().enumerate() {
            match &mov.to {
                Location::Reg(reg) => {
                    let total = src_counts.get(reg).copied().unwrap_or(0);
                    let self_uses = if pending_srcs[idx].contains(reg) {
                        1
                    } else {
                        0
                    };
                    if total <= self_uses {
                        ready_idx = Some(idx);
                        break;
                    }
                }
                _ => {
                    ready_idx = Some(idx);
                    break;
                }
            }
        }

        if let Some(idx) = ready_idx {
            let removed = pending.remove(idx);
            let removed_srcs = pending_srcs.remove(idx);
            for reg in removed_srcs {
                if let Some(count) = src_counts.get_mut(&reg) {
                    *count -= 1;
                    if *count == 0 {
                        src_counts.remove(&reg);
                    }
                }
            }
            ordered.push(removed);
            continue;
        }

        // Otherwise we have a cycle; break it with a scratch reg.
        let cycle_idx = pending
            .iter()
            .position(|mov| matches!((&mov.from, &mov.to), (Location::Reg(_), Location::Reg(_))))
            .expect("cycle detection requires a reg-to-reg move");
        let mut mov = pending.remove(cycle_idx);
        let removed_srcs = pending_srcs.remove(cycle_idx);
        for reg in removed_srcs {
            if let Some(count) = src_counts.get_mut(&reg) {
                *count -= 1;
                if *count == 0 {
                    src_counts.remove(&reg);
                }
            }
        }
        let Location::Reg(from_reg) = mov.from else {
            unreachable!("cycle candidate must be reg -> reg");
        };

        // Save the source reg to scratch, then retry the original move.
        ordered.push(Move {
            from: Location::Reg(from_reg),
            to: Location::Reg(scratch),
        });

        mov.from = Location::Reg(scratch);

        // Recompute source regs for the requeued move so pending_srcs stays in sync,
        // and update src_counts to reflect the newly added sources.
        let mut regs = HashSet::new();
        if let Location::Reg(reg) = mov.from {
            regs.insert(reg);
        }
        for reg in extra_src_regs(&mov) {
            regs.insert(reg);
        }
        for reg in &regs {
            *src_counts.entry(*reg).or_insert(0) += 1;
        }
        pending_srcs.push(regs);

        pending.push(mov);
    }

    *moves = ordered;
}
