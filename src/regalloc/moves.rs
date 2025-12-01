use std::collections::HashMap;
use std::fmt;

use crate::ir::pos::{InstPos, RelInstPos};
use crate::ir::types::IrBlockId;
use crate::regalloc::regs::Arm64Reg;
use crate::regalloc::spill::StackSlotId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    Reg(Arm64Reg),
    Stack(StackSlotId),
    Imm(i64),
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Location::Reg(reg) => write!(f, "Reg({})", reg)?,
            Location::Stack(slot) => write!(f, "Stack({})", slot.0)?,
            Location::Imm(value) => write!(f, "Imm({})", value)?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Move {
    pub from: Location,
    pub to: Location,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.from, self.to)?;
        Ok(())
    }
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
    edge_moves: HashMap<IrBlockId, Vec<Move>>,
    return_moves: HashMap<IrBlockId, Move>,
}

impl FnMoveList {
    pub fn new() -> Self {
        Self {
            inst_moves: HashMap::new(),
            edge_moves: HashMap::new(),
            return_moves: HashMap::new(),
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

    pub fn add_edge_move(&mut self, from_block_id: IrBlockId, from: Location, to: Location) {
        self.edge_moves
            .entry(from_block_id)
            .or_insert(vec![])
            .push(Move { from, to });
    }

    pub fn add_return_move(&mut self, block_id: IrBlockId, from: Location, to: Location) {
        self.return_moves.insert(block_id, Move { from, to });
    }

    pub fn get_inst_moves(&self, inst_pos: InstPos) -> Option<&InstMoveList> {
        self.inst_moves.get(&inst_pos)
    }

    pub fn get_edge_moves(&self, from_block_id: IrBlockId) -> Option<&Vec<Move>> {
        self.edge_moves.get(&from_block_id)
    }

    pub fn get_return_move(&self, block_id: IrBlockId) -> Option<&Move> {
        self.return_moves.get(&block_id)
    }
}

impl fmt::Display for InstMoveList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "  before:")?;
        for m in &self.before_moves {
            writeln!(f, "    {} -> {}", m.from, m.to)?;
        }
        writeln!(f, "  after:")?;
        for m in &self.after_moves {
            writeln!(f, "    {} -> {}", m.from, m.to)?;
        }
        Ok(())
    }
}

impl fmt::Display for FnMoveList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (pos, moves) in &self.inst_moves {
            writeln!(f, "InstMoveList:")?;
            writeln!(f, "  pos: {}", pos)?;
            writeln!(f, "  moves: {}", moves)?;
        }
        writeln!(f, "EdgeMoves:")?;
        for (block_id, moves) in &self.edge_moves {
            writeln!(f, "  block_id: {}", block_id.id())?;
            for mov in moves {
                writeln!(f, "    {} -> {}", mov.from, mov.to)?;
            }
        }
        writeln!(f, "ReturnMoves:")?;
        for (block_id, mov) in &self.return_moves {
            writeln!(f, "  block_id: {}, {}", block_id.id(), mov)?;
        }
        Ok(())
    }
}
