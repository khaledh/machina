//! Minimal SSA function builder.
//!
//! Provides a small API for creating SSA functions for tests and early
//! lowering without exposing the raw ID allocation details.

use super::ir::*;
use crate::resolve::DefId;

/// Constructs SSA functions while managing ID allocation.
pub struct FunctionBuilder {
    func: Function,
    next_value: u32,
    next_block: u32,
    next_local: u32,
}

impl FunctionBuilder {
    /// Creates a new SSA builder for a single function.
    pub fn new(def_id: DefId, name: impl Into<String>, sig: FunctionSig) -> Self {
        Self {
            func: Function {
                def_id,
                name: name.into(),
                sig,
                locals: Vec::new(),
                blocks: Vec::new(),
            },
            next_value: 0,
            next_block: 0,
            next_local: 0,
        }
    }

    /// Appends a new basic block and returns its ID.
    pub fn add_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        self.func.blocks.push(Block {
            id,
            params: Vec::new(),
            insts: Vec::new(),
            term: Terminator::Unreachable,
        });
        id
    }

    /// Adds a local slot to the function locals list.
    pub fn add_local(&mut self, ty: TypeId, name: Option<String>) -> LocalId {
        let id = LocalId(self.next_local);
        self.next_local += 1;
        self.func.locals.push(Local { id, ty, name });
        id
    }

    /// Adds a parameter to the given block and returns the SSA value ID.
    pub fn add_block_param(&mut self, block: BlockId, ty: TypeId) -> ValueId {
        let value = self.alloc_value(ty);
        let block = self.block_mut(block);
        block.params.push(BlockParam {
            value: ValueDef { id: value, ty },
        });
        value
    }

    /// Emits an integer constant instruction in the given block.
    pub fn const_int(
        &mut self,
        block: BlockId,
        value: i128,
        signed: bool,
        bits: u8,
        ty: TypeId,
    ) -> ValueId {
        let result = self.alloc_value(ty);
        let block = self.block_mut(block);
        block.insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::Const {
                value: ConstValue::Int {
                    value,
                    signed,
                    bits,
                },
            },
        });
        result
    }

    /// Emits a boolean constant instruction in the given block.
    pub fn const_bool(&mut self, block: BlockId, value: bool, ty: TypeId) -> ValueId {
        let result = self.alloc_value(ty);
        let block = self.block_mut(block);
        block.insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::Const {
                value: ConstValue::Bool(value),
            },
        });
        result
    }

    /// Emits a binary operation instruction in the given block.
    pub fn binop(
        &mut self,
        block: BlockId,
        op: BinOp,
        lhs: ValueId,
        rhs: ValueId,
        ty: TypeId,
    ) -> ValueId {
        let result = self.alloc_value(ty);
        let block = self.block_mut(block);
        block.insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::BinOp { op, lhs, rhs },
        });
        result
    }

    /// Emits a comparison instruction in the given block.
    pub fn cmp(
        &mut self,
        block: BlockId,
        op: CmpOp,
        lhs: ValueId,
        rhs: ValueId,
        ty: TypeId,
    ) -> ValueId {
        let result = self.alloc_value(ty);
        let block = self.block_mut(block);
        block.insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::Cmp { op, lhs, rhs },
        });
        result
    }

    /// Emits a unary operation instruction in the given block.
    pub fn unop(&mut self, block: BlockId, op: UnOp, value: ValueId, ty: TypeId) -> ValueId {
        let result = self.alloc_value(ty);
        let block = self.block_mut(block);
        block.insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::UnOp { op, value },
        });
        result
    }

    /// Sets the terminator for a block.
    pub fn set_terminator(&mut self, block: BlockId, term: Terminator) {
        let block = self.block_mut(block);
        block.term = term;
    }

    /// Finalizes the builder and returns the constructed function.
    pub fn finish(self) -> Function {
        self.func
    }

    /// Allocates a fresh SSA value ID.
    fn alloc_value(&mut self, _ty: TypeId) -> ValueId {
        let id = ValueId(self.next_value);
        self.next_value += 1;
        id
    }

    /// Returns a mutable reference to a block by ID.
    fn block_mut(&mut self, block: BlockId) -> &mut Block {
        let index = block.index();
        self.func
            .blocks
            .get_mut(index)
            .unwrap_or_else(|| panic!("invalid block id {:?}", block))
    }
}
