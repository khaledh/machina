//! Minimal SSA function builder.
//!
//! Provides a small API for creating SSA functions for tests and early
//! lowering without exposing the raw ID allocation details.

use super::ir::*;
use crate::resolve::DefId;
use crate::ssa::IrTypeId;

/// Constructs SSA functions while managing ID allocation.
///
/// Maintains a "current block" cursor for instruction emission. Use `select_block`
/// to change the cursor, then emit instructions which append to that block.
/// The cursor starts at the entry block (block 0).
pub struct FunctionBuilder {
    func: Function,
    next_value: u32,
    next_block: u32,
    next_local: u32,
    cursor: BlockId,
}

impl FunctionBuilder {
    /// Creates a new SSA builder for a single function.
    ///
    /// Automatically creates the entry block (block 0) and sets the cursor to it.
    pub fn new(def_id: DefId, name: impl Into<String>, sig: FunctionSig) -> Self {
        let entry_block = Block {
            id: BlockId(0),
            params: Vec::new(),
            insts: Vec::new(),
            term: Terminator::Unreachable,
        };
        Self {
            func: Function {
                def_id,
                name: name.into(),
                sig,
                locals: Vec::new(),
                blocks: vec![entry_block],
            },
            next_value: 0,
            next_block: 1, // Entry block is 0
            next_local: 0,
            cursor: BlockId(0),
        }
    }

    pub fn current_block(&self) -> BlockId {
        self.cursor
    }

    /// Returns a mutable reference to the current block.
    fn current_block_mut(&mut self) -> &mut Block {
        self.block_mut(self.cursor)
    }

    /// Returns a mutable reference to a block by ID.
    fn block_mut(&mut self, block: BlockId) -> &mut Block {
        let index = block.index();
        self.func
            .blocks
            .get_mut(index)
            .unwrap_or_else(|| panic!("invalid block id {:?}", block))
    }

    /// Sets the current block cursor for instruction emission.
    pub fn select_block(&mut self, block: BlockId) {
        self.cursor = block;
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
    pub fn add_local(&mut self, ty: IrTypeId, name: Option<String>) -> LocalId {
        let id = LocalId(self.next_local);
        self.next_local += 1;
        self.func.locals.push(Local { id, ty, name });
        id
    }

    /// Adds a parameter to the given block and returns the SSA value ID.
    pub fn add_block_param(&mut self, block: BlockId, ty: IrTypeId) -> ValueId {
        let value = self.alloc_value(ty);
        let block = self.block_mut(block);
        block.params.push(BlockParam {
            value: ValueDef { id: value, ty },
        });
        value
    }

    /// Emits an integer constant instruction at the cursor.
    pub fn const_int(&mut self, value: i128, signed: bool, bits: u8, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
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

    /// Emits a unit constant instruction at the cursor.
    pub fn const_unit(&mut self, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::Const {
                value: ConstValue::Unit,
            },
        });
        result
    }

    /// Emits a boolean constant instruction at the cursor.
    pub fn const_bool(&mut self, value: bool, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::Const {
                value: ConstValue::Bool(value),
            },
        });
        result
    }

    /// Emits a global address constant instruction at the cursor.
    pub fn const_global_addr(&mut self, id: GlobalId, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::Const {
                value: ConstValue::GlobalAddr { id },
            },
        });
        result
    }

    /// Emits a function address constant instruction at the cursor.
    pub fn const_func_addr(&mut self, def: DefId, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::Const {
                value: ConstValue::FuncAddr { def },
            },
        });
        result
    }

    /// Emits a binary operation instruction at the cursor.
    pub fn binop(&mut self, op: BinOp, lhs: ValueId, rhs: ValueId, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::BinOp { op, lhs, rhs },
        });
        result
    }

    /// Emits a comparison instruction at the cursor.
    pub fn cmp(&mut self, op: CmpOp, lhs: ValueId, rhs: ValueId, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::Cmp { op, lhs, rhs },
        });
        result
    }

    /// Emits an integer truncation instruction at the cursor.
    pub fn int_trunc(&mut self, value: ValueId, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::IntTrunc { value, ty },
        });
        result
    }

    /// Emits an integer extension instruction at the cursor.
    pub fn int_extend(&mut self, value: ValueId, ty: IrTypeId, signed: bool) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::IntExtend { value, ty, signed },
        });
        result
    }

    /// Emits a pointer/integer cast instruction at the cursor.
    pub fn cast(&mut self, kind: CastKind, value: ValueId, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::Cast { kind, value, ty },
        });
        result
    }

    /// Emits a unary operation instruction at the cursor.
    pub fn unop(&mut self, op: UnOp, value: ValueId, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::UnOp { op, value },
        });
        result
    }

    /// Emits a call instruction at the cursor.
    pub fn call(&mut self, callee: Callee, args: Vec<ValueId>, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::Call { callee, args },
        });
        result
    }

    /// Emits an addr-of-local instruction at the cursor.
    pub fn addr_of_local(&mut self, local: LocalId, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::AddrOfLocal { local },
        });
        result
    }

    /// Emits a field-address (GEP) instruction at the cursor.
    pub fn field_addr(&mut self, base: ValueId, index: usize, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::FieldAddr { base, index },
        });
        result
    }

    /// Emits an index-address (GEP) instruction at the cursor.
    pub fn index_addr(&mut self, base: ValueId, index: ValueId, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::IndexAddr { base, index },
        });
        result
    }

    /// Emits a load instruction at the cursor.
    pub fn load(&mut self, ptr: ValueId, ty: IrTypeId) -> ValueId {
        let result = self.alloc_value(ty);
        self.current_block_mut().insts.push(Instruction {
            result: Some(ValueDef { id: result, ty }),
            kind: InstKind::Load { ptr },
        });
        result
    }

    /// Emits a store instruction at the cursor.
    pub fn store(&mut self, ptr: ValueId, value: ValueId) {
        self.current_block_mut().insts.push(Instruction {
            result: None,
            kind: InstKind::Store { ptr, value },
        });
    }

    /// Emits a memory copy instruction at the cursor.
    pub fn memcopy(&mut self, dst: ValueId, src: ValueId, len: ValueId) {
        self.current_block_mut().insts.push(Instruction {
            result: None,
            kind: InstKind::MemCopy { dst, src, len },
        });
    }

    /// Sets the terminator for the current block.
    pub fn terminate(&mut self, term: Terminator) {
        self.current_block_mut().term = term;
    }

    /// Sets the terminator for an arbitrary block (for non-current block updates).
    pub fn set_terminator(&mut self, block: BlockId, term: Terminator) {
        self.block_mut(block).term = term;
    }

    /// Finalizes the builder and returns the constructed function.
    pub fn finish(self) -> Function {
        self.func
    }

    /// Allocates a fresh SSA value ID.
    fn alloc_value(&mut self, _ty: IrTypeId) -> ValueId {
        let id = ValueId(self.next_value);
        self.next_value += 1;
        id
    }
}
