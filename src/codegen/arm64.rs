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

    #[error("Unsupported move: {0} -> {1}")]
    UnsupportedMove(Location, Location),

    #[error("Array {0} should be allocated to stack, not register")]
    ArrayIsNotStack(IrTempId),

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
            assert!(frame_size % 16 == 0); // stack must be aligned to 16 bytes
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
            assert!(frame_size % 16 == 0); // stack must be aligned to 16 bytes
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

    pub fn emit_store_element(
        &mut self,
        array: IrTempId,
        index: &IrOperand,
        value: &IrOperand,
    ) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Get the array stack slot
        let array_slot = match self.alloc_result.alloc_map.get(&array) {
            Some(MappedTemp::Stack(slot)) => *slot,
            Some(MappedTemp::Reg(_)) => return Err(CodegenError::ArrayIsNotStack(array)),
            None => return Err(CodegenError::TempNotFound(array.id())),
        };

        // Get the element type and size
        let array_ty = self.func.temp_type(array);
        let elem_ty = match array_ty {
            IrType::Array { elem_ty, .. } => elem_ty,
            _ => return Err(CodegenError::TempIsNotArray(array, array_ty.clone())),
        };
        let elem_size = elem_ty.size_of();

        let array_base = array_slot.offset_bytes();

        match index {
            IrOperand::Const(IrConst::Int { value: idx, .. }) => {
                // Constant index - compute offset at compile time
                let elem_offset = array_base + (*idx as u32 * elem_size as u32);
                match value {
                    IrOperand::Temp(temp) => {
                        let value_reg = self.get_reg(temp)?;
                        asm.push_str(&format!("  str {}, [sp, #{}]\n", value_reg, elem_offset));
                    }
                    IrOperand::Const(c) => {
                        // Materialize the constant into a scratch register
                        let op_reg = self.operand_for_int_with_policy(
                            c.int_value(),
                            ImmPolicy::RegOnly,
                            &mut asm,
                            0,
                        );
                        asm.push_str(&format!("  str {}, [sp, #{}]\n", op_reg, elem_offset));
                    }
                }
            }
            IrOperand::Temp(temp) => {
                // Dynamic index - compute offset at runtime
                let index_reg = self.get_reg(temp)?;
                let elem_size_reg = self.operand_for_int_with_policy(
                    elem_size as i64,
                    ImmPolicy::RegOnly,
                    &mut asm,
                    0,
                );
                let elem_offset_reg = "x17"; // TODO: rework how we allocate scratch registers
                asm.push_str(&format!(
                    "  mul {}, {}, {}\n",
                    elem_offset_reg, index_reg, elem_size_reg
                ));
                asm.push_str(&format!(
                    "  add {}, {}, #{}\n",
                    elem_offset_reg, elem_offset_reg, array_base
                ));

                // Store the value to the computed offset
                let value_reg = match value {
                    IrOperand::Temp(temp) => self.get_reg(temp)?.to_string(),
                    IrOperand::Const(c) => self.operand_for_int_with_policy(
                        c.int_value(),
                        ImmPolicy::RegOnly,
                        &mut asm,
                        0,
                    ),
                };
                asm.push_str(&format!("  str {}, [sp, {}]\n", value_reg, elem_offset_reg));
            }
            _ => todo!("handle other constant operands"),
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

        // Get the array stack slot
        let array_slot = match self.alloc_result.alloc_map.get(&array) {
            Some(MappedTemp::Stack(slot)) => *slot,
            Some(MappedTemp::Reg(_)) => return Err(CodegenError::ArrayIsNotStack(*array)),
            None => return Err(CodegenError::TempNotFound(array.id())),
        };

        // Get the element type and size
        let array_ty = self.func.temp_type(*array);
        let elem_ty = match array_ty {
            IrType::Array { elem_ty, .. } => elem_ty,
            _ => return Err(CodegenError::TempIsNotArray(*array, array_ty.clone())),
        };
        let elem_size = elem_ty.size_of();

        let array_base = array_slot.offset_bytes();

        match index {
            IrOperand::Const(IrConst::Int { value: idx, .. }) => {
                // Constant index - compute offset at compile time
                let elem_offset = array_base + (*idx as u32 * elem_size as u32);
                asm.push_str(&format!("  ldr {}, [sp, #{}]\n", result_reg, elem_offset));
            }
            IrOperand::Temp(temp) => {
                // Dynamic index - compute offset at runtime
                let index_reg = self.get_reg(temp)?;
                let elem_size_reg = self.operand_for_int_with_policy(
                    elem_size as i64,
                    ImmPolicy::RegOnly,
                    &mut asm,
                    0,
                );
                let elem_offset_reg = "x17"; // TODO: rework how we allocate scratch registers
                asm.push_str(&format!(
                    "  mul {}, {}, {}\n",
                    elem_offset_reg, index_reg, elem_size_reg
                ));
                asm.push_str(&format!(
                    "  add {}, {}, #{}\n",
                    elem_offset_reg, elem_offset_reg, array_base
                ));

                // Load the value from the computed offset
                asm.push_str(&format!(
                    "  ldr {}, [sp, {}]\n",
                    result_reg, elem_offset_reg
                ));
            }
            _ => todo!("handle other constant operands"),
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
            (Location::Imm(from_imm), Location::Reg(to_reg)) => {
                asm.push_str(&format!("  mov {}, #{}\n", to_reg, from_imm));
            }
            (from_loc, to_loc) => {
                return Err(CodegenError::UnsupportedMove(
                    from_loc.clone(),
                    to_loc.clone(),
                ));
            }
        }
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
