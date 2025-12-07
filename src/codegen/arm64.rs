use std::collections::HashMap;
use thiserror::Error;

use crate::ast::{BinaryOp as BOp, UnaryOp as UOp};
use crate::context::LoweredRegAllocContext;
use crate::ir::pos::InstPos;
use crate::ir::types::{
    IrBlock, IrBlockId, IrConst, IrFunction, IrInst, IrOperand, IrTempId, IrTerminator, IrType,
};
use crate::regalloc::moves::{Location, Move};
use crate::regalloc::regs::Arm64Reg;
use crate::regalloc::{AllocationResult, MappedTemp, StackSlotId};

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Temp {0} should be in a register, not stack or stack address")]
    TempIsNotInRegister(u32),

    #[error("Temp {0} not found in allocation map")]
    TempNotFound(u32),

    #[error("Stack slot is out of bounds: offset {0} > {1}")]
    StackSlotOutOfBounds(u32, u32),

    #[error("Constant condition value must be boolean, found {0}")]
    CondTypeNotBoolean(IrConst),

    #[error("Unterminated block: {0}")]
    UnterminatedBlock(String),

    #[error("Unsupported move: {0} -> {1}")]
    UnsupportedMove(Location, Location),

    #[error("Expected temp {0} to be an array, found {1}")]
    TempIsNotArray(IrTempId, IrType),
}

// Controls how integer constants are encoded for a given instruction.
// Some instructions (e.g. add/sub/cmp) support limited immediates, others
// (e.g. mul, neg, udiv) require register operands.
enum ImmPolicy {
    /// Use add/sub style immediates when they fit; otherwise materialize into a register.
    AddSubImm,
    /// Always materialize the value into a register (no immediate encoding allowed).
    RegOnly,
}

pub struct Arm64Codegen {
    context: LoweredRegAllocContext,
}

impl Arm64Codegen {
    pub fn new(context: LoweredRegAllocContext) -> Self {
        Self { context }
    }

    pub fn generate(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        let mut next_label_start: u32 = 0;

        for (ir_func, alloc_result) in self
            .context
            .ir_funcs
            .iter()
            .zip(self.context.alloc_results.iter())
        {
            let mut func_codegen = FuncCodegen::new(ir_func, alloc_result, next_label_start);
            let func_asm = func_codegen.generate()?;
            asm.push_str(&func_asm);
            asm.push_str("\n");

            // Advance the global label counter by the number of non-entry blocks in this function.
            let non_entry_blocks = ir_func
                .blocks
                .keys()
                .filter(|bid| **bid != IrBlockId(0))
                .count() as u32;
            next_label_start += non_entry_blocks;
        }
        Ok(asm)
    }
}

struct FuncCodegen<'a> {
    func: &'a IrFunction,
    alloc_result: &'a AllocationResult,
    stack_alloc_size: u32,
    stack_padding: u32,
    label_counter: u32,
    block_labels: HashMap<IrBlockId, String>,
}

impl<'a> FuncCodegen<'a> {
    pub fn new(func: &'a IrFunction, alloc_result: &'a AllocationResult, label_start: u32) -> Self {
        let stack_alloc_size =
            alloc_result.frame_size - alloc_result.used_callee_saved.len() as u32 * 8;
        let mut stack_padding = 0;
        if stack_alloc_size % 16 != 0 {
            stack_padding = 16 - stack_alloc_size % 16;
        }
        Self {
            func,
            alloc_result,
            stack_alloc_size,
            stack_padding,
            label_counter: label_start,
            block_labels: HashMap::new(),
        }
    }

    pub fn generate(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Pre-generate block labels
        for block in self.func.blocks.values() {
            if block.id() != IrBlockId(0) {
                let label = self.next_label();
                let block_label = format!("{}_{}", label, block.name());
                self.block_labels.insert(block.id(), block_label);
            }
        }

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
            let padded_frame_size = frame_size + self.stack_padding;
            asm.push_str(&format!("  sub sp, sp, #{padded_frame_size}\n"));
        }

        // Save callee-saved registers (in pairs using stp) at the start of the stack frame
        // (spilled/alloc'ed temps come after callee-saved registers from sp -> sp + spilled size)
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
            let padded_frame_size = frame_size + self.stack_padding;
            asm.push_str(&format!("  add sp, sp, #{padded_frame_size}\n"));
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
                match op {
                    // Division needs both operands in registers: UDIV Xd, Xn, Xm
                    BOp::Div => {
                        let scratch = "x16";
                        match (lhs, rhs) {
                            // temp / temp
                            (IrOperand::Temp(lhs_temp), IrOperand::Temp(rhs_temp)) => {
                                let lhs_reg = self.get_reg(lhs_temp)?;
                                let rhs_reg = self.get_reg(rhs_temp)?;
                                asm.push_str(&format!(
                                    "  udiv {}, {}, {}\n",
                                    result_reg, lhs_reg, rhs_reg
                                ));
                            }
                            // temp / const
                            (
                                IrOperand::Temp(lhs_temp),
                                IrOperand::Const(IrConst::Int { value: rhs_val, .. }),
                            ) => {
                                let lhs_reg = self.get_reg(lhs_temp)?;
                                asm.push_str(&format!("  mov {}, #{}\n", scratch, rhs_val));
                                asm.push_str(&format!(
                                    "  udiv {}, {}, {}\n",
                                    result_reg, lhs_reg, scratch
                                ));
                            }
                            // const / temp  — reuse result_reg for the lhs constant
                            (
                                IrOperand::Const(IrConst::Int { value: lhs_val, .. }),
                                IrOperand::Temp(rhs_temp),
                            ) => {
                                let rhs_reg = self.get_reg(rhs_temp)?;
                                asm.push_str(&format!("  mov {}, #{}\n", result_reg, lhs_val));
                                asm.push_str(&format!(
                                    "  udiv {}, {}, {}\n",
                                    result_reg, result_reg, rhs_reg
                                ));
                            }
                            // const / const — lhs in result_reg, rhs in scratch
                            (
                                IrOperand::Const(IrConst::Int { value: lhs_val, .. }),
                                IrOperand::Const(IrConst::Int { value: rhs_val, .. }),
                            ) => {
                                asm.push_str(&format!("  mov {}, #{}\n", result_reg, lhs_val));
                                asm.push_str(&format!("  mov {}, #{}\n", scratch, rhs_val));
                                asm.push_str(&format!(
                                    "  udiv {}, {}, {}\n",
                                    result_reg, result_reg, scratch
                                ));
                            }
                            // Any other constant kinds are not yet supported
                            _ => todo!("handle other constant operands for division"),
                        }
                    }
                    // Other arithmetic operators
                    BOp::Add | BOp::Sub => {
                        let lhs_operand = match lhs {
                            IrOperand::Temp(temp) => format!("{}", self.get_reg(temp)?),
                            IrOperand::Const(IrConst::Int { value, .. }) => self
                                .operand_for_int_with_policy(
                                    *value,
                                    ImmPolicy::AddSubImm,
                                    &mut asm,
                                    0,
                                ),
                            _ => todo!("handle other constant operands"),
                        };
                        let rhs_operand = match rhs {
                            IrOperand::Temp(temp) => format!("{}", self.get_reg(temp)?),
                            IrOperand::Const(IrConst::Int { value, .. }) => self
                                .operand_for_int_with_policy(
                                    *value,
                                    ImmPolicy::AddSubImm,
                                    &mut asm,
                                    1,
                                ),
                            _ => todo!("handle other constant operands"),
                        };
                        let op_str = self.get_op_str(op);
                        asm.push_str(&format!(
                            "  {} {}, {}, {}\n",
                            op_str, result_reg, lhs_operand, rhs_operand
                        ));
                    }
                    BOp::Mul => {
                        let lhs_operand = match lhs {
                            IrOperand::Temp(temp) => format!("{}", self.get_reg(temp)?),
                            IrOperand::Const(IrConst::Int { value, .. }) => self
                                .operand_for_int_with_policy(
                                    *value,
                                    ImmPolicy::RegOnly,
                                    &mut asm,
                                    0,
                                ),
                            _ => todo!("handle other constant operands"),
                        };
                        let rhs_operand = match rhs {
                            IrOperand::Temp(temp) => format!("{}", self.get_reg(temp)?),
                            IrOperand::Const(IrConst::Int { value, .. }) => self
                                .operand_for_int_with_policy(
                                    *value,
                                    ImmPolicy::RegOnly,
                                    &mut asm,
                                    1,
                                ),
                            _ => todo!("handle other constant operands"),
                        };
                        let op_str = self.get_op_str(op);
                        asm.push_str(&format!(
                            "  {} {}, {}, {}\n",
                            op_str, result_reg, lhs_operand, rhs_operand
                        ));
                    }
                    // Comparison operators
                    BOp::Eq | BOp::Ne | BOp::Lt | BOp::Gt | BOp::LtEq | BOp::GtEq => {
                        let lhs_operand = match lhs {
                            IrOperand::Temp(temp) => format!("{}", self.get_reg(temp)?),
                            IrOperand::Const(IrConst::Int { value, .. }) => self
                                .operand_for_int_with_policy(
                                    *value,
                                    ImmPolicy::AddSubImm,
                                    &mut asm,
                                    0,
                                ),
                            _ => todo!("handle other constant operands"),
                        };
                        let rhs_operand = match rhs {
                            IrOperand::Temp(temp) => format!("{}", self.get_reg(temp)?),
                            IrOperand::Const(IrConst::Int { value, .. }) => self
                                .operand_for_int_with_policy(
                                    *value,
                                    ImmPolicy::AddSubImm,
                                    &mut asm,
                                    1,
                                ),
                            _ => todo!("handle other constant operands"),
                        };
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
                    IrOperand::Const(IrConst::Int { value, .. }) => {
                        // NEG uses a register operand; materialize consts into a register.
                        self.operand_for_int_with_policy(*value, ImmPolicy::RegOnly, &mut asm, 0)
                    }
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
            IrInst::StoreElement {
                array,
                index,
                value,
            } => {
                asm.push_str(&self.emit_store_element(*array, index, value)?);
            }
            IrInst::LoadElement {
                array,
                index,
                result,
            } => {
                asm.push_str(&self.emit_load_element(*result, array, index)?);
            }
        }
        Ok(asm)
    }

    fn emit_element_address(
        &mut self,
        array: IrTempId,
        index: &IrOperand,
        elem_size: usize,
        result_reg: &str,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        match index {
            IrOperand::Const(IrConst::Int { value: idx, .. }) => {
                let elem_offset = (*idx as usize) * elem_size;

                match self.alloc_result.alloc_map.get(&array) {
                    Some(MappedTemp::StackAddr(slot)) => {
                        // Case A: Local array - compute final offset from sp
                        let array_base = self.get_stack_offset(slot)?;
                        let final_offset = array_base + elem_offset as u32;
                        asm.push_str(&format!("  add {result_reg}, sp, #{final_offset}\n"));
                    }
                    Some(MappedTemp::Stack(_)) | Some(MappedTemp::Reg(_)) => {
                        // Case B/C: Param  or allocated array - load base then add offset
                        let base_reg = match self.alloc_result.alloc_map.get(&array) {
                            Some(MappedTemp::Stack(slot)) => {
                                let offset = self.get_stack_offset(slot)?;
                                asm.push_str(&format!("  ldr x16, [sp, #{}]\n", offset));
                                "x16".to_string()
                            }
                            Some(MappedTemp::Reg(reg)) => format!("{}", reg),
                            _ => unreachable!(),
                        };

                        // Common immediate offset logic
                        if elem_offset == 0 {
                            asm.push_str(&format!("  mov {result_reg}, {base_reg}\n"));
                        } else if elem_offset < 4096 {
                            asm.push_str(&format!(
                                "  add {result_reg}, {base_reg}, #{elem_offset}\n"
                            ));
                        } else {
                            asm.push_str(&format!("  mov {result_reg}, #{elem_offset}\n"));
                            asm.push_str(&format!(
                                "  add {result_reg}, {base_reg}, {result_reg}\n"
                            ));
                        }
                    }
                    None => return Err(CodegenError::TempNotFound(array.id())),
                }
            }
            IrOperand::Temp(temp) => {
                let index_reg = self.get_reg(temp)?;

                // Calculate element offset: index * elem_size
                let offset_reg = if elem_size == 1 {
                    index_reg.to_string()
                } else {
                    let size_reg = self.operand_for_int_with_policy(
                        elem_size as i64,
                        ImmPolicy::RegOnly,
                        asm,
                        0,
                    );
                    asm.push_str(&format!("  mul {result_reg}, {index_reg}, {size_reg}\n"));
                    result_reg.to_string()
                };

                match self.alloc_result.alloc_map.get(&array) {
                    Some(MappedTemp::StackAddr(slot)) => {
                        // Case A: Local array - add to sp + array_base
                        let array_base = self.get_stack_offset(slot)?;
                        asm.push_str(&format!(
                            "  add {result_reg}, {offset_reg}, #{array_base}\n"
                        ));
                        asm.push_str(&format!("  add {result_reg}, sp, {result_reg}\n"));
                    }
                    Some(MappedTemp::Stack(slot)) => {
                        // Case B: Spilled array - load base then add offset
                        let array_base = self.get_stack_offset(slot)?;
                        asm.push_str(&format!("  ldr x16, [sp, #{}]\n", array_base));
                        asm.push_str(&format!("  add {result_reg}, x16, {offset_reg}\n"));
                    }
                    Some(MappedTemp::Reg(base_reg)) => {
                        // Case C: Param array - add offset to base register
                        asm.push_str(&format!("  add {result_reg}, {base_reg}, {offset_reg}\n"));
                    }
                    None => return Err(CodegenError::TempNotFound(array.id())),
                }
            }
            _ => todo!("handle other index operand types"),
        }
        Ok(())
    }

    pub fn emit_store_element(
        &mut self,
        array: IrTempId,
        index: &IrOperand,
        value: &IrOperand,
    ) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Get the element type and size
        let array_ty = self.func.temp_type(array);
        let elem_ty = match array_ty {
            IrType::Array { elem_ty, .. } => elem_ty,
            _ => return Err(CodegenError::TempIsNotArray(array, array_ty.clone())),
        };
        let elem_size = elem_ty.size_of();

        // Check if we can use optimized immediate offset
        match (self.alloc_result.alloc_map.get(&array), index) {
            (
                Some(MappedTemp::StackAddr(slot)),
                IrOperand::Const(IrConst::Int { value: idx, .. }),
            ) => {
                // OPTIMIZED PATH: Direct store with immediate offset
                let array_base = self.get_stack_offset(&slot)?;
                let element_offset = (*idx as u32) * 8;
                let final_offset = array_base + element_offset;

                // Load value into register if needed
                let value_reg = match value {
                    IrOperand::Temp(t) => {
                        let value_reg = self.get_reg(t)?.to_string();
                        value_reg
                    }
                    IrOperand::Const(c) => {
                        asm.push_str(&format!("  mov x16, #{}\n", c.int_value()));
                        "x16".to_string()
                    }
                };

                // Direct store: str value_reg, [sp, #offset]
                asm.push_str(&format!("  str {}, [sp, #{}]\n", value_reg, final_offset));
            }
            _ => {
                // Calculate the element address
                self.emit_element_address(array, index, elem_size, "x17", &mut asm)?;
                // Store the value to the element address
                match value {
                    IrOperand::Temp(temp) => {
                        let value_reg = self.get_reg(temp)?;
                        asm.push_str(&format!("  str {}, [x17]\n", value_reg));
                    }
                    IrOperand::Const(c) => {
                        // Materialize the constant into a scratch register
                        let value_reg = self.operand_for_int_with_policy(
                            c.int_value(),
                            ImmPolicy::RegOnly,
                            &mut asm,
                            0,
                        );
                        asm.push_str(&format!("  str {}, [x17]\n", value_reg));
                    }
                }
            }
        }

        Ok(asm)
    }

    pub fn emit_load_element(
        &mut self,
        result: IrTempId,
        array: &IrTempId,
        index: &IrOperand,
    ) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Get the result register
        let result_reg = self.get_reg(&result)?;

        // Get the element type and size
        let array_ty = self.func.temp_type(*array);
        let elem_ty = match array_ty {
            IrType::Array { elem_ty, .. } => elem_ty,
            _ => return Err(CodegenError::TempIsNotArray(*array, array_ty.clone())),
        };
        let elem_size = elem_ty.size_of();

        // Check if we can use optimized immediate offset
        match (self.alloc_result.alloc_map.get(&array), index) {
            (
                Some(MappedTemp::StackAddr(slot)),
                IrOperand::Const(IrConst::Int { value: idx, .. }),
            ) => {
                // OPTIMIZED PATH: Direct load with immediate offset
                let array_base = self.get_stack_offset(&slot)?;
                let element_offset = (*idx as u32) * 8;
                let final_offset = array_base + element_offset;
                asm.push_str(&format!("  ldr {result_reg}, [sp, #{}]\n", final_offset));
            }
            _ => {
                // Calculate the element address
                self.emit_element_address(*array, index, elem_size, "x17", &mut asm)?;
                // Load the value from the element address
                asm.push_str(&format!("  ldr {result_reg}, [x17]\n"));
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
            IrTerminator::Ret { .. } => {
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
        let inst = match (&mov.from, &mov.to) {
            (src, Location::Reg(to_reg)) => {
                // 1. Value -> Register
                match src {
                    Location::Imm(from_imm) => format!("  mov {to_reg}, #{from_imm}\n"),
                    Location::Reg(from_reg) => format!("  mov {to_reg}, {from_reg}\n"),
                    Location::Stack(from_slot) => format!(
                        "  ldr {to_reg}, [sp, #{}]\n",
                        self.get_stack_offset(from_slot)?
                    ),
                    Location::StackAddr(from_slot) => {
                        format!(
                            "  add {to_reg}, sp, #{}\n",
                            self.get_stack_offset(from_slot)?
                        )
                    }
                }
            }

            // 2. Register -> Stack (Store)
            (Location::Reg(from_reg), Location::Stack(to_slot)) => {
                format!(
                    "  str {from_reg}, [sp, #{}]\n",
                    self.get_stack_offset(to_slot)?
                )
            }

            // 3. Imm -> Stack (Store)
            (Location::Imm(from_imm), Location::Stack(to_slot)) => {
                // Materialize the immediate into a scratch register
                let scratch =
                    self.operand_for_int_with_policy(*from_imm, ImmPolicy::RegOnly, &mut asm, 0);
                format!(
                    "  str {scratch}, [sp, #{}]\n",
                    self.get_stack_offset(to_slot)?
                )
            }

            // 4. Invalid / Impossible Moves
            // - Stack -> Stack (Memory-to-memory move not supported natively)
            // - Imm -> Imm, Reg -> Imm (Dest cannot be Imm)
            // - StackAddr -> Stack (Can't store address directly without scratch reg)
            // - Anything -> StackAddr (You can't "write" to an address calculation)
            _ => {
                return Err(CodegenError::UnsupportedMove(
                    mov.from.clone(),
                    mov.to.clone(),
                ));
            }
        };
        asm.push_str(&inst);
        Ok(asm)
    }

    // Helpers

    /// Check whether an integer fits the AArch64 add/sub immediate encoding:
    /// imm12 optionally shifted left by 12 bits (i.e. imm = u12 << {0,12}).
    fn int_fits_add_sub_imm(value: i64) -> bool {
        if value < 0 {
            return false;
        }
        let v = value as u64;
        if v < (1 << 12) {
            true
        } else if v < (1 << 24) && (v & ((1 << 12) - 1)) == 0 {
            true
        } else {
            false
        }
    }

    /// Decide how to encode an integer operand based on the provided policy:
    /// either as an immediate (`#imm`) when it fits, or by materializing it
    /// into a scratch register (`x16` or `x17`) and returning that register.
    fn operand_for_int_with_policy(
        &mut self,
        value: i64,
        policy: ImmPolicy,
        asm: &mut String,
        scratch_index: u8,
    ) -> String {
        match policy {
            ImmPolicy::AddSubImm if Self::int_fits_add_sub_imm(value) => {
                format!("#{}", value)
            }
            ImmPolicy::AddSubImm | ImmPolicy::RegOnly => {
                let scratch = match scratch_index {
                    0 => "x16",
                    1 => "x17",
                    _ => "x16",
                };
                asm.push_str(&format!("  mov {}, #{}\n", scratch, value));
                scratch.to_string()
            }
        }
    }

    fn get_reg(&self, temp: &IrTempId) -> Result<Arm64Reg, CodegenError> {
        match self.alloc_result.alloc_map.get(temp) {
            Some(MappedTemp::Reg(reg)) => Ok(*reg),
            Some(_) => Err(CodegenError::TempIsNotInRegister(temp.id())),
            None => Err(CodegenError::TempNotFound(temp.id())),
        }
    }

    // Get stack offset for a stack slot
    fn get_stack_offset(&self, slot: &StackSlotId) -> Result<u32, CodegenError> {
        if slot.0 >= self.alloc_result.stack_slot_count {
            return Err(CodegenError::StackSlotOutOfBounds(
                slot.0,
                self.alloc_result.stack_slot_count,
            ));
        }
        let offset = self.stack_padding + self.stack_alloc_size - slot.offset_bytes();
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
