use std::collections::HashMap;

use crate::ir::pos::{InstPos, RelInstPos};
use crate::regalloc::regs::Arm64Reg;
use crate::regalloc::spill::StackSlotId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Reg(Arm64Reg),
    Stack(StackSlotId),
}

#[derive(Debug, Clone)]
pub struct Move {
    pub from: Location,
    pub to: Location,
}

#[derive(Debug)]
pub struct InstMoveList {
    pub pos: InstPos,
    pub before_moves: Vec<Move>,
    pub after_moves: Vec<Move>,
}

#[derive(Debug)]
pub struct FnMoveList {
    inst_moves: HashMap<InstPos, InstMoveList>,
}

impl FnMoveList {
    pub fn new() -> Self {
        Self {
            inst_moves: HashMap::new(),
        }
    }

    pub fn add_inst_move(&mut self, rel_pos: RelInstPos, from: Location, to: Location) {
        let inst_pos = match rel_pos {
            RelInstPos::Before(pos) | RelInstPos::After(pos) => pos,
        };

        // Create the instruction move list if it doesn't exist
        let move_list = self.inst_moves.entry(inst_pos).or_insert(InstMoveList {
            pos: inst_pos,
            before_moves: vec![],
            after_moves: vec![],
        });

        // Add the move (before or after)
        match rel_pos {
            RelInstPos::Before(_) => move_list.before_moves.push(Move { from, to }),
            RelInstPos::After(_) => move_list.after_moves.push(Move { from, to }),
        }
    }

    pub fn get_inst_moves(&self, inst_pos: InstPos) -> Option<&InstMoveList> {
        self.inst_moves.get(&inst_pos)
    }
}
