use std::collections::HashMap;
use thiserror::Error;

use crate::context::LoweredRegAllocContext;
use crate::ir::types::{IrBlock, IrBlockId, IrFunction, IrInst, IrTempId, IrTerminator};
use crate::regalloc::moves::Move;
use crate::regalloc::regs::Arm64Reg;
use crate::regalloc::{AllocationResult, MappedTemp, StackSlotId};

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Placeholder error: {0}")]
    PlaceholderError(String),

    #[error("Temp {0} is spilled to stack (no register assigned)")]
    TempIsSpilled(u32),

    #[error("Temp {0} not found in allocation map")]
    TempNotFound(u32),

    #[error("Stack-to-stack move should not happen: {0}")]
    StackToStackMove(Move),

    #[error("Stack slot is out of bounds: offset {0} > {1}")]
    StackSlotOutOfBounds(u32, u32),
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
}

impl<'a> FuncCodegen<'a> {
    pub fn new(func: &'a IrFunction, alloc_result: &'a AllocationResult) -> Self {
        let spilled_size =
            alloc_result.frame_size - alloc_result.used_callee_saved.len() as u32 * 8;
        Self {
            func,
            alloc_result,
            spilled_size,
        }
    }

    pub fn generate(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Prologue
        asm.push_str(&self.emit_prologue()?);

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
        Ok(String::new())
    }

    pub fn emit_inst(&mut self, inst: &IrInst) -> Result<String, CodegenError> {
        Ok(String::new())
    }

    pub fn emit_terminator(&mut self, terminator: &IrTerminator) -> Result<String, CodegenError> {
        Ok(String::new())
    }

    // Moves

    pub fn emit_moves(&mut self, moves: &Vec<Move>) -> Result<String, CodegenError> {
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
}

#[cfg(test)]
#[path = "../tests/t_codegen_arm64.rs"]
mod tests;
