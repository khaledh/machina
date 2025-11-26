use crate::ir::types::IrBlockId;

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
