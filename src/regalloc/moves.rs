use std::collections::HashMap;
use std::fmt;

use crate::mcir::types::{BlockId, Place, PlaceAny, Scalar};
use crate::regalloc::pos::{InstPos, RelInstPos};
use crate::regalloc::regs::Arm64Reg;
use crate::regalloc::stack::StackSlotId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Location {
    #[allow(dead_code)]
    Imm(i64),
    Reg(Arm64Reg),
    Stack(StackSlotId),
    StackAddr(StackSlotId),
    PlaceAddr(PlaceAny),
    PlaceValue(Place<Scalar>),
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Location::Imm(value) => write!(f, "Imm({})", value),
            Location::Reg(reg) => write!(f, "Reg({})", reg),
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
}
