use crate::ir::types::IrBlockId;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstPos {
    pub block: IrBlockId,
    pub index: usize,
}

impl InstPos {
    pub fn new(block: IrBlockId, index: usize) -> Self {
        Self { block, index }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelInstPos {
    Before(InstPos),
    After(InstPos),
}

impl fmt::Display for InstPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "block.{}:{}", self.block.id(), self.index)?;
        Ok(())
    }
}