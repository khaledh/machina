use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::mcir::types::{BlockId, Place, PlaceAny, Scalar};
use crate::regalloc::pos::{InstPos, RelInstPos};
use crate::regalloc::stack::StackSlotId;
use crate::regalloc::target::PhysReg;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Location {
    #[allow(dead_code)]
    Imm(i64),
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

    pub fn resolve_parallel_moves(&mut self, scratch_regs: &[PhysReg]) {
        if scratch_regs.is_empty() {
            return;
        }
        let scratch = scratch_regs[0];
        for inst_moves in self.inst_moves.values_mut() {
            resolve_move_list(&mut inst_moves.before_moves, scratch);
            resolve_move_list(&mut inst_moves.after_moves, scratch);
        }
    }
}

fn resolve_move_list(moves: &mut Vec<Move>, scratch: PhysReg) {
    if moves.len() <= 1 {
        return;
    }

    let mut pending = std::mem::take(moves);
    let mut ordered = Vec::with_capacity(pending.len());

    while !pending.is_empty() {
        let mut src_regs = HashSet::new();
        for mov in &pending {
            if let Location::Reg(reg) = mov.from {
                src_regs.insert(reg);
            }
        }

        let mut ready_idx = None;
        for (idx, mov) in pending.iter().enumerate() {
            match &mov.to {
                Location::Reg(reg) => {
                    if !src_regs.contains(reg) {
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
            ordered.push(pending.remove(idx));
            continue;
        }

        let cycle_idx = pending
            .iter()
            .position(|mov| matches!((&mov.from, &mov.to), (Location::Reg(_), Location::Reg(_))))
            .expect("cycle detection requires a reg-to-reg move");
        let mut mov = pending.remove(cycle_idx);
        let Location::Reg(from_reg) = mov.from else {
            unreachable!("cycle candidate must be reg -> reg");
        };
        ordered.push(Move {
            from: Location::Reg(from_reg),
            to: Location::Reg(scratch),
        });
        mov.from = Location::Reg(scratch);
        pending.push(mov);
    }

    *moves = ordered;
}
