use std::collections::HashMap;
use thiserror::Error;

use crate::ast::{BinaryOp as BOp, UnaryOp as UOp};
use crate::context::LoweredRegAllocContext;
use crate::ir::pos::InstPos;
use crate::ir::types::{
    IrBlock, IrBlockId, IrConst, IrFunction, IrInst, IrOperand, IrTempId, IrTerminator,
};
use crate::regalloc::moves::Move;
use crate::regalloc::regs::Arm64Reg;
use crate::regalloc::{AllocationResult, MappedTemp, StackSlotId};

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Temp {0} is spilled to stack (no register assigned)")]
    TempIsSpilled(u32),

    #[error("Temp {0} not found in allocation map")]
    TempNotFound(u32),

    #[error("Stack-to-stack move should not happen: {0}")]
    StackToStackMove(Move),

    #[error("Stack slot is out of bounds: offset {0} > {1}")]
    StackSlotOutOfBounds(u32, u32),

    #[error("Constant left operand not supported yet: {0}")]
    ConstLeftOperand(IrConst),

    #[error("Constant condition value must be boolean, found {0}")]
    CondTypeNotBoolean(IrConst),

    #[error("Unterminated block: {0}")]
    UnterminatedBlock(String),
}

pub struct Arm64Codegen {
    context: LoweredRegAllocContext,
    label_map: HashMap<IrBlockId, String>,
}

impl Arm64Codegen {
    pub fn new(context: LoweredRegAllocContext) -> Self {
        Self {
            context,
            label_map: HashMap::new(),
        }
    }

    pub fn generate(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        for (ir_func, alloc_result) in self
            .context
            .ir_funcs
            .iter()
            .zip(self.context.alloc_results.iter())
        {
            let mut func_codegen = FuncCodegen::new(ir_func, alloc_result);
            let func_asm = func_codegen.generate()?;
            asm.push_str(&func_asm);
            asm.push_str("\n");
        }
        Ok(asm)
    }
}

struct FuncCodegen<'a> {
    func: &'a IrFunction,
    alloc_result: &'a AllocationResult,
    spilled_size: u32,
    label_counter: u32,
    block_labels: HashMap<IrBlockId, String>,
}

impl<'a> FuncCodegen<'a> {
    pub fn new(func: &'a IrFunction, alloc_result: &'a AllocationResult) -> Self {
        let spilled_size =
            alloc_result.frame_size - alloc_result.used_callee_saved.len() as u32 * 8;
        Self {
            func,
            alloc_result,
            spilled_size,
            label_counter: 0,
            block_labels: HashMap::new(),
        }
    }

    pub fn generate(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Prologue
        asm.push_str(&self.emit_prologue()?);

        // Pre-generate block labels
        for block in self.func.blocks.values() {
            if block.id() != IrBlockId(0) {
                let label = self.next_label();
                let block_label = format!("{}_{}", label, block.name());
                self.block_labels.insert(block.id(), block_label);
            }
        }

        // Function body
        for block in self.func.blocks.values() {
            asm.push_str(&self.emit_block(block)?);
        }

        // Note: Epilogue will be emitted at each Ret terminator
        Ok(asm)
    }

    // Epilogue / Prologue

    pub fn emit_prologue(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Function label
        asm.push_str(&format!(".global _{}\n", self.func.name));
        asm.push_str(&format!("_{}:\n", self.func.name));

        // AAPCS 64-bit ABI:
        // 6.4.6 The Frame Pointer
        //     Conforming code shall construct a linked list of stack-frames. Each frame shall link
        //     to the frame of its caller by means of a frame record of two 64-bit values on the
        //     stack (independent of the data model). The frame record for the innermost frame
        //     (belonging to the most recent routine invocation) shall be pointed to by the frame
        //     pointer register (FP). The lowest addressed double-word shall point to the previous
        //     frame record and the highest addressed double-word shall contain the value passed in
        //     LR on entry to the current function. [...] The end of the frame record chain is
        //     indicated by the address zero in the address for the previous frame. The location of
        //     the frame record within a stack frame is not specified.

        // Higher addresses
        // │
        // │   ┌─────────────────────────────────┐
        // │   │  Caller's stack frame           │
        // │   │  (aligned to 16 bytes)          │
        // │   └─────────────────────────────────┘
        // │         ▲
        // │         │ old SP (before call)
        // │         │
        // │   << CALL instruction >>
        // │         │
        // │         ▼
        // │   ┌─────────────────────────────────┐
        // │   │  [LR = X30]                     │
        // │   ├─────────────────────────────────┤
        // │   │  [FP = X29]                     │ ← SP after "stp x29, x30, [sp,#-16]!"
        // │   └─────────────────────────────────┘    (SP was decremented by 16)
        // │         ▲
        // │         │ FP points here after "mov x29, sp"
        // │         ▼
        // │   ┌─────────────────────────────────┐
        // │   │  [callee-saved X19]             │ ← SP + frame_size - 8
        // │   ├─────────────────────────────────┤
        // │   │  [callee-saved X20]             │ ← SP + frame_size - 16
        // │   ├─────────────────────────────────┤
        // │   │  [callee-saved X21]             │ ← SP + frame_size - 24
        // │   ├─────────────────────────────────┤
        // │   │         ...                     │
        // │   ├─────────────────────────────────┤
        // │   │  [callee-saved XN]              │ ← SP + frame_size - (callee_saved_size - 8)
        // │   ├─────────────────────────────────┤ ─┐
        // │   │  [spilled temp slot 0]          │  │ ← SP + (spilled size - 8)
        // │   ├─────────────────────────────────┤  │
        // │   │  [spilled temp slot 1]          │  │ ← SP + (spilled size - 16)
        // │   ├─────────────────────────────────┤  │
        // │   │         ...                     │  │
        // │   ├─────────────────────────────────┤  │
        // │   │  [spilled temp slot N]          │  │ ← SP + 0
        // ▼   └─────────────────────────────────┘ ─┘
        //          ▲
        //          │ new SP (current stack pointer during function execution)
        //          │
        // Lower addresses

        // Create a new stack frame (FP + LR). This instruction decrements SP by 16 bytes.
        asm.push_str("  stp x29, x30, [sp, #-16]!\n");
        asm.push_str("  mov x29, sp\n"); // Update frame pointer to the new frame

        // Allocate stack space (includes callee-saved regs and spilled temps)
        let frame_size = self.alloc_result.frame_size;
        if frame_size > 0 {
            asm.push_str(&format!("  sub sp, sp, #{frame_size}\n"));
        }

        // Save callee-saved registers (in pairs using stp) at the start of the stack frame
        // (spilled temps come after callee-saved registers from sp -> sp + spilled size)
        let used_callee = &self.alloc_result.used_callee_saved;
        let mut offset = frame_size as i32;
        if !used_callee.is_empty() {
            for pair in used_callee.chunks(2) {
                if pair.len() == 2 {
                    offset -= 16;
                    asm.push_str(&format!(
                        "  stp {}, {}, [sp, #{}]\n",
                        pair[0], pair[1], offset
                    ));
                } else {
                    // Last register is unpaired
                    offset -= 8;
                    asm.push_str(&format!("  str {}, [sp, #{}]\n", pair[0], offset));
                }
            }
        }

        Ok(asm)
    }

    pub fn emit_epilogue(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        let frame_size = self.alloc_result.frame_size;

        // Restore callee-saved registers
        let used_callee = &self.alloc_result.used_callee_saved;
        let mut offset = frame_size as i32;
        if !used_callee.is_empty() {
            for pair in used_callee.chunks(2) {
                if pair.len() == 2 {
                    offset -= 16;
                    asm.push_str(&format!(
                        "  ldp {}, {}, [sp, #{}]\n",
                        pair[0], pair[1], offset
                    ));
                } else {
                    // Last register is unpaired
                    offset -= 8;
                    asm.push_str(&format!("  ldr {}, [sp, #{}]\n", pair[0], offset));
                }
            }
        }

        // Deallocate stack space
        if frame_size > 0 {
            asm.push_str(&format!("  add sp, sp, #{frame_size}\n"));
        }

        // Pop frame pointer and return address
        asm.push_str("  ldp x29, x30, [sp], #16\n");
        asm.push_str("  ret\n");

        Ok(asm)
    }

    // Blocks, instructions, terminators

    pub fn emit_block(&mut self, block: &IrBlock) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Block label (skip entry block, it uses function label from prologue)
        if block.id() != IrBlockId(0) {
            asm.push_str(&format!("{}:\n", self.block_labels[&block.id()]));
        }

        // Block instructions
        for (idx, inst) in block.insts().iter().enumerate() {
            let pos = InstPos::new(block.id(), idx);

            // Emit before moves
            if let Some(moves) = self.alloc_result.moves.get_inst_moves(pos) {
                asm.push_str(&self.emit_moves(&moves.before_moves)?);
            }

            // Emit instruction
            asm.push_str(&self.emit_inst(inst)?);

            // Emit after moves
            if let Some(moves) = self.alloc_result.moves.get_inst_moves(pos) {
                asm.push_str(&self.emit_moves(&moves.after_moves)?);
            }
        }

        // Emit return moves
        if let Some(ret_move) = self.alloc_result.moves.get_return_move(block.id()) {
            asm.push_str(&self.emit_move(ret_move)?);
        }

        // Block terminator
        asm.push_str(&self.emit_terminator(block.term(), block.id())?);

        Ok(asm)
    }

    pub fn emit_inst(&mut self, inst: &IrInst) -> Result<String, CodegenError> {
        let mut asm = String::new();
        match inst {
            IrInst::Move { dest, src } => {
                let dest_reg = self.get_reg(dest)?;
                match src {
                    IrOperand::Temp(temp) => {
                        let src_reg = self.get_reg(temp)?;
                        asm.push_str(&format!("  mov {}, {}\n", dest_reg, src_reg));
                    }
                    IrOperand::Const(c) => {
                        asm.push_str(&format!("  mov {}, #{}\n", dest_reg, c));
                    }
                }
            }
            IrInst::BinaryOp {
                result,
                op,
                lhs,
                rhs,
            } => {
                let result_reg = self.get_reg(result)?;
                let lhs_operand = match lhs {
                    IrOperand::Temp(temp) => format!("{}", self.get_reg(temp)?),
                    IrOperand::Const(c) => {
                        return Err(CodegenError::ConstLeftOperand(*c));
                    }
                };
                let rhs_operand = match rhs {
                    IrOperand::Temp(temp) => format!("{}", self.get_reg(temp)?),
                    IrOperand::Const(IrConst::Int { value, .. }) => {
                        if *value > 4096 {
                            // Use a scratch register
                            asm.push_str(&format!("  mov x16, ={}\n", value));
                            "x16".to_string()
                        } else {
                            format!("#{}", value)
                        }
                    }
                    _ => todo!("handle other constant operands"),
                };
                match op {
                    // Arithmetic operators
                    BOp::Add | BOp::Sub | BOp::Mul | BOp::Div => {
                        let op_str = self.get_op_str(op);
                        asm.push_str(&format!(
                            "  {} {}, {}, {}\n",
                            op_str, result_reg, lhs_operand, rhs_operand
                        ));
                    }
                    // Comparison operators
                    BOp::Eq | BOp::Ne | BOp::Lt | BOp::Gt | BOp::LtEq | BOp::GtEq => {
                        let op_str = self.get_op_str(op);
                        asm.push_str(&format!("  cmp {}, {}\n", lhs_operand, rhs_operand));
                        asm.push_str(&format!("  cset {}, {}\n", result_reg, op_str));
                    }
                }
            }
            IrInst::UnaryOp {
                result,
                op,
                operand,
            } => {
                let result_reg = self.get_reg(result)?;
                let operand_str = match operand {
                    IrOperand::Temp(temp) => format!("{}", self.get_reg(temp)?),
                    IrOperand::Const(c) => format!("#{}", c),
                };
                match op {
                    UOp::Neg => {
                        asm.push_str(&format!("  neg {}, {}\n", result_reg, operand_str));
                    }
                }
            }
            IrInst::Call { name, .. } => {
                asm.push_str(&format!("  bl _{}\n", name));
            }
            IrInst::Phi { .. } => {
                // Phi nodes are handled by register allocator moves
                // No code needs to be emitted here
            }
        }
        Ok(asm)
    }

    pub fn emit_terminator(
        &mut self,
        terminator: &IrTerminator,
        block_id: IrBlockId,
    ) -> Result<String, CodegenError> {
        let mut asm = String::new();
        match terminator {
            IrTerminator::Ret { value } => {
                // Handle constant return value
                if let Some(IrOperand::Const(c)) = value {
                    asm.push_str(&format!("  mov x0, #{}\n", c));
                }
                asm.push_str(&self.emit_epilogue()?);
            }
            IrTerminator::Br { target } => {
                // Emit edge moves if any
                if let Some(edge_moves) = self.alloc_result.moves.get_edge_moves(block_id) {
                    asm.push_str(&self.emit_moves(&edge_moves)?);
                }
                asm.push_str(&format!("  b {}\n", self.block_labels[target]));
            }
            IrTerminator::CondBr {
                cond,
                then_b,
                else_b,
            } => {
                // Emit edge moves if any
                if let Some(edge_moves) = self.alloc_result.moves.get_edge_moves(block_id) {
                    asm.push_str(&self.emit_moves(&edge_moves)?);
                }
                match cond {
                    IrOperand::Temp(temp) => {
                        // cond expr already evaluated (cmp, cset)
                        let cond_reg = self.get_reg(temp)?;
                        // branch to then_b if cond is true, else_b if false
                        asm.push_str(&format!(
                            "  cbnz {}, {}\n",
                            cond_reg, self.block_labels[then_b]
                        ));
                        asm.push_str(&format!("  b {}\n", self.block_labels[else_b]));
                    }
                    IrOperand::Const(IrConst::Bool(value)) => {
                        // cond is a constant, emit either then or else block (but not both)
                        let target_block = if *value { then_b } else { else_b };
                        asm.push_str(&format!("  b {}\n", self.block_labels[target_block]));
                    }
                    IrOperand::Const(c) => {
                        return Err(CodegenError::CondTypeNotBoolean(*c));
                    }
                }
            }
            IrTerminator::_Unterminated => {
                let block_label = self.block_labels[&block_id].clone();
                return Err(CodegenError::UnterminatedBlock(block_label));
            }
        }
        Ok(asm)
    }

    // Moves

    pub fn emit_moves(&mut self, moves: &[Move]) -> Result<String, CodegenError> {
        moves.iter().map(|mov| self.emit_move(mov)).collect()
    }

    pub fn emit_move(&mut self, mov: &Move) -> Result<String, CodegenError> {
        use crate::regalloc::moves::Location;

        let mut asm = String::new();
        match (&mov.from, &mov.to) {
            (Location::Reg(from_reg), Location::Reg(to_reg)) => {
                asm.push_str(&format!("  mov {}, {}\n", to_reg, from_reg));
            }
            (Location::Reg(from_reg), Location::Stack(to_slot)) => {
                let offset = self.get_stack_offset(to_slot)?;
                asm.push_str(&format!("  str {}, [sp, #{}]\n", from_reg, offset));
            }
            (Location::Stack(from_slot), Location::Reg(to_reg)) => {
                let offset = self.get_stack_offset(from_slot)?;
                asm.push_str(&format!("  ldr {}, [sp, #{}]\n", to_reg, offset));
            }
            (Location::Stack(_), Location::Stack(_)) => {
                return Err(CodegenError::StackToStackMove(mov.clone()));
            }
        }
        Ok(asm)
    }

    // Helpers

    fn get_reg(&self, temp: &IrTempId) -> Result<Arm64Reg, CodegenError> {
        match self.alloc_result.alloc_map.get(temp) {
            Some(MappedTemp::Reg(reg)) => Ok(*reg),
            Some(MappedTemp::Stack(_)) => Err(CodegenError::TempIsSpilled(temp.id())),
            None => Err(CodegenError::TempNotFound(temp.id())),
        }
    }

    // Get stack offset for a stack slot
    fn get_stack_offset(&self, slot: &StackSlotId) -> Result<u32, CodegenError> {
        if slot.0 >= self.alloc_result.spill_slot_count {
            return Err(CodegenError::StackSlotOutOfBounds(
                slot.0,
                self.alloc_result.spill_slot_count,
            ));
        }
        // first spilled slot is at offset (spilled size - 8)
        let offset = self.spilled_size - slot.offset_bytes() - 8;
        Ok(offset)
    }

    fn next_label(&mut self) -> String {
        let label = format!(".L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    fn get_op_str(&self, op: &BOp) -> &str {
        match op {
            BOp::Add => "add",
            BOp::Sub => "sub",
            BOp::Mul => "mul",
            BOp::Div => "udiv",
            BOp::Eq => "eq",
            BOp::Ne => "ne",
            BOp::Lt => "lt",
            BOp::Gt => "gt",
            BOp::LtEq => "le",
            BOp::GtEq => "ge",
        }
    }
}

#[cfg(test)]
#[path = "../tests/t_codegen_arm64.rs"]
mod tests;
