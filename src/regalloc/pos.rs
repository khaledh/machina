use crate::mcir::types::BlockId;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstPos {
    pub block: BlockId,
    pub index: usize,
}

impl InstPos {
    pub fn new(block: BlockId, index: usize) -> Self {
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
        write!(f, "block.{}:{}", self.block.0, self.index)
    }
}
