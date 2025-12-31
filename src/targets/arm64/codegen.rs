use std::collections::HashMap;

use thiserror::Error;

use super::regs::{self, Arm64Reg as R, to_w_reg};
use crate::context::RegAllocatedContext;
use crate::mcir::layout::size_of_ty;
use crate::mcir::types::*;
use crate::regalloc::moves::{Location, Move};
use crate::regalloc::pos::InstPos;
use crate::regalloc::stack::StackSlotId;
use crate::regalloc::target::PhysReg;
use crate::regalloc::{AllocationResult, MappedLocal};
use crate::resolve::def_map::DefId;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Local {0} not found in allocation map")]
    LocalNotFound(u32),

    #[error("Stack slot is out of bounds: offset {0} > {1}")]
    StackSlotOutOfBounds(u32, u32),

    #[error("Unterminated block: {0}")]
    UnterminatedBlock(String),

    #[error("Unsupported move: {0} -> {1}")]
    UnsupportedMove(Location, Location),

    #[error("Unsupported size {0} for store/load operation")]
    UnsupportedSize(usize),

    #[error("Invalid projection on non-aggregate type")]
    InvalidProjectionType,

    #[error("Callee name not found for def {0}")]
    CalleeNameNotFound(DefId),
}

// Controls how integer constants are encoded for a given instruction.
enum ImmPolicy {
    AddSubImm,
    RegOnly,
}

pub struct McFunction<'a> {
    pub name: String,
    pub body: &'a FuncBody,
    pub alloc: &'a AllocationResult,
}

pub struct Arm64Codegen<'a> {
    funcs: Vec<McFunction<'a>>,
    def_names: &'a HashMap<DefId, String>,
    globals: &'a [GlobalItem],
    label_counter: u32,
}

impl<'a> Arm64Codegen<'a> {
    pub fn new(
        funcs: Vec<McFunction<'a>>,
        def_names: &'a HashMap<DefId, String>,
        globals: &'a [GlobalItem],
    ) -> Self {
        Self {
            funcs,
            def_names,
            globals,
            label_counter: 0,
        }
    }

    pub fn from_regalloc_context(ctx: &'a RegAllocatedContext) -> Self {
        let funcs: Vec<_> = ctx
            .func_bodies
            .iter()
            .zip(ctx.symbols.func_ids.iter())
            .zip(ctx.alloc_results.iter())
            .map(|((body, def_id), alloc)| {
                let name = ctx
                    .symbols
                    .def_names
                    .get(def_id)
                    .unwrap_or_else(|| {
                        panic!("Function def {:?} missing from symbol table", def_id);
                    })
                    .clone();
                McFunction { name, body, alloc }
            })
            .collect();
        Self::new(funcs, &ctx.symbols.def_names, &ctx.globals)
    }

    pub fn generate(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        let global_labels = self.emit_globals(&mut asm);
        let _ = self.emit_funcs(&mut asm, &global_labels);

        Ok(asm)
    }

    fn emit_globals(&mut self, asm: &mut String) -> HashMap<GlobalId, String> {
        let mut global_labels = HashMap::new();

        // Mach-O (macOS) uses "__TEXT,__const" for rodata.
        asm.push_str(".section __TEXT,__const\n");

        for global in self.globals {
            if global.kind != GlobalSection::RoData {
                panic!("Non-rodata global are not supported yet: {:?}", global);
            }
            let label = self.global_label(global.id, &mut global_labels);
            asm.push_str("  .p2align 3\n");
            asm.push_str(&format!("  {label}:\n"));
            match &global.payload {
                GlobalPayload::Bytes(bytes) => {
                    asm.push_str("    .byte ");
                    for byte in bytes {
                        asm.push_str(&format!("0x{byte:02x}, "));
                    }
                    asm.push('\n');
                }
                GlobalPayload::String(string) => {
                    asm.push_str(&format!(
                        "    .ascii \"{}\"\n",
                        Self::escape_ascii(string.as_bytes())
                    ));
                }
            }
        }

        asm.push('\n');

        global_labels
    }

    fn emit_funcs(
        &mut self,
        asm: &mut String,
        global_labels: &HashMap<GlobalId, String>,
    ) -> Result<(), CodegenError> {
        asm.push_str(".text\n\n");

        for func in &self.funcs {
            let mut codegen =
                FuncCodegen::new(func, self.def_names, global_labels, self.label_counter)?;
            asm.push_str(&codegen.generate()?);
            asm.push('\n');
            self.label_counter = codegen.label_counter;
        }

        Ok(())
    }

    fn global_label(&mut self, global: GlobalId, labels: &mut HashMap<GlobalId, String>) -> String {
        if let Some(label) = labels.get(&global) {
            return label.clone();
        }
        let label = format!(".G{}", global.index());
        labels.insert(global, label.clone());
        label
    }

    fn escape_ascii(bytes: &[u8]) -> String {
        let mut out = String::new();
        for &b in bytes {
            match b {
                b'\\' => out.push_str("\\\\"),
                b'"' => out.push_str("\\\""),
                0x20..=0x7e => out.push(b as char),
                _ => out.push_str(&format!("\\x{:02x}", b)),
            }
        }
        out
    }
}

struct FuncCodegen<'a> {
    name: &'a str,
    body: &'a FuncBody,
    alloc: &'a AllocationResult,
    def_names: &'a HashMap<DefId, String>,
    global_labels: &'a HashMap<GlobalId, String>,
    stack_alloc_size: u32,
    stack_padding: u32,
    label_counter: u32,
    block_labels: HashMap<BlockId, String>,
}

impl<'a> FuncCodegen<'a> {
    pub fn new(
        func: &'a McFunction<'a>,
        def_names: &'a HashMap<DefId, String>,
        global_labels: &'a HashMap<GlobalId, String>,
        label_start: u32,
    ) -> Result<Self, CodegenError> {
        let stack_alloc_size =
            func.alloc.frame_size - func.alloc.used_callee_saved.len() as u32 * 8;
        let mut stack_padding = 0;
        if !func.alloc.frame_size.is_multiple_of(16) {
            stack_padding = 16 - func.alloc.frame_size % 16;
        }
        Ok(Self {
            name: &func.name,
            body: func.body,
            alloc: func.alloc,
            def_names,
            global_labels,
            stack_alloc_size,
            stack_padding,
            label_counter: label_start,
            block_labels: HashMap::new(),
        })
    }

    pub fn generate(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Pre-assign labels for non-entry blocks so branches can reference them.
        for (idx, _) in self.body.blocks.iter().enumerate() {
            let block_id = BlockId(idx as u32);
            if block_id != self.body.entry {
                let label = self.next_label();
                self.block_labels
                    .insert(block_id, format!("{}_bb{}", label, block_id.0));
            }
        }

        asm.push_str(&self.emit_prologue()?);

        for (idx, block) in self.body.blocks.iter().enumerate() {
            let block_id = BlockId(idx as u32);
            asm.push_str(&self.emit_block(block_id, block)?);
        }

        Ok(asm)
    }

    fn reg(&self, reg: PhysReg) -> R {
        regs::from_phys(reg)
    }

    fn emit_prologue(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Function label
        asm.push_str(&format!(".global _{}\n", self.name));
        asm.push_str(&format!("_{}:\n", self.name));

        // Push frame pointer and link register.
        asm.push_str("  stp x29, x30, [sp, #-16]!\n");
        asm.push_str("  mov x29, sp\n");

        // Allocate the stack frame (including alignment padding).
        let frame_size = self.alloc.frame_size;
        if frame_size > 0 {
            let padded_frame_size = frame_size + self.stack_padding;
            asm.push_str(&format!("  sub sp, sp, #{padded_frame_size}\n"));
        }

        // Save any callee-saved registers used by this function.
        let used_callee = &self.alloc.used_callee_saved;
        let mut offset = frame_size as i32;
        if !used_callee.is_empty() {
            for pair in used_callee.chunks(2) {
                if pair.len() == 2 {
                    offset -= 16;
                    let r0 = self.reg(pair[0]);
                    let r1 = self.reg(pair[1]);
                    asm.push_str(&format!("  stp {}, {}, [sp, #{}]\n", r0, r1, offset));
                } else {
                    offset -= 8;
                    let r0 = self.reg(pair[0]);
                    asm.push_str(&format!("  str {}, [sp, #{}]\n", r0, offset));
                }
            }
        }

        Ok(asm)
    }

    fn emit_epilogue(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Restore callee-saved registers in reverse order.
        let used_callee = &self.alloc.used_callee_saved;
        let mut offset = self.alloc.frame_size as i32;
        if !used_callee.is_empty() {
            for pair in used_callee.chunks(2) {
                if pair.len() == 2 {
                    offset -= 16;
                    let r0 = self.reg(pair[0]);
                    let r1 = self.reg(pair[1]);
                    asm.push_str(&format!("  ldp {}, {}, [sp, #{}]\n", r0, r1, offset));
                } else {
                    offset -= 8;
                    let r0 = self.reg(pair[0]);
                    asm.push_str(&format!("  ldr {}, [sp, #{}]\n", r0, offset));
                }
            }
        }

        // Pop the stack frame.
        if self.alloc.frame_size > 0 {
            let padded_frame_size = self.alloc.frame_size + self.stack_padding;
            asm.push_str(&format!("  add sp, sp, #{padded_frame_size}\n"));
        }

        // Restore frame pointer and link register.
        asm.push_str("  ldp x29, x30, [sp], #16\n");
        asm.push_str("  ret\n");

        Ok(asm)
    }

    fn emit_block(
        &mut self,
        block_id: BlockId,
        block: &BasicBlock,
    ) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Emit block label (skip for entry block)
        if block_id != self.body.entry {
            asm.push_str(&format!("{}:\n", self.block_labels[&block_id]));
        }

        // Emit statements, including any scheduled before/after moves.
        for (idx, stmt) in block.stmts.iter().enumerate() {
            let pos = InstPos::new(block_id, idx);

            // Apply scheduled moves before the instruction.
            if let Some(moves) = self.alloc.moves.get_inst_moves(pos) {
                asm.push_str(&self.emit_moves(&moves.before_moves)?);
            }

            // Emit the instruction (preceded by comment for debugging).
            asm.push_str(&format!("  // {}\n", stmt));
            asm.push_str(&self.emit_stmt(stmt)?);

            // Apply scheduled moves after the instruction.
            if let Some(moves) = self.alloc.moves.get_inst_moves(pos) {
                asm.push_str(&self.emit_moves(&moves.after_moves)?);
            }
        }

        // Final return move for this block if needed.
        if let Some(ret_move) = self.alloc.moves.get_return_move(block_id) {
            asm.push_str(&self.emit_move(ret_move)?);
        }

        // Emit terminator.
        asm.push_str(&format!("  // {}\n", block.terminator));
        asm.push_str(&self.emit_terminator(block_id, &block.terminator)?);

        Ok(asm)
    }

    fn emit_stmt(&mut self, stmt: &Statement) -> Result<String, CodegenError> {
        match stmt {
            Statement::Comment(_) => Ok(String::new()),
            Statement::CopyScalar { dst, src } => self.emit_copy_scalar(dst, src),
            Statement::CopyAggregate { .. } => {
                panic!("CopyAggregate must be lowered before codegen")
            }
            Statement::MemSet { .. } => panic!("MemSet must be lowered before codegen"),
            Statement::Call { callee, .. } => self.emit_call(callee),
        }
    }

    fn emit_copy_scalar(
        &mut self,
        dst: &Place<crate::mcir::types::Scalar>,
        src: &Rvalue,
    ) -> Result<String, CodegenError> {
        let mut asm = String::new();
        let is_zero = Self::is_zero_rvalue(src);
        if dst.projections().is_empty() {
            // Fast-path: direct register assignment for simple rvalues.
            if let Some(MappedLocal::Reg(dst_reg)) = self.alloc.alloc_map.get(&dst.base())
                && matches!(src, Rvalue::Use(_))
            {
                let dst_reg = self.reg(*dst_reg);
                if let Rvalue::Use(op) = src {
                    let value_reg = self.materialize_operand(op, &mut asm, dst_reg)?;
                    if value_reg != dst_reg {
                        asm.push_str(&format!("  mov {}, {}\n", dst_reg, value_reg));
                    }
                    return Ok(asm);
                }
            }
            let value_reg = self.emit_rvalue(src, &mut asm)?;
            self.store_scalar(dst, value_reg, &mut asm)?;
        } else if let Some(offset) = self.const_projection_offset(dst.base(), dst.projections()) {
            // Store to computed address with constant offset.
            if let Some(MappedLocal::StackAddr(slot)) = self.alloc.alloc_map.get(&dst.base()) {
                // Stack slot with constant offset.
                let base_offset = self.get_stack_offset(slot)? as usize;
                let total = base_offset + offset;
                if let Ok(total) = u32::try_from(total) {
                    if is_zero {
                        self.emit_store_to_sp(total, dst.ty(), R::Xzr, &mut asm)?;
                    } else {
                        let value_reg = self.emit_rvalue(src, &mut asm)?;
                        self.emit_store_to_sp(total, dst.ty(), value_reg, &mut asm)?;
                    }
                    return Ok(asm);
                }
            } else if let Some(MappedLocal::Reg(reg)) = self.alloc.alloc_map.get(&dst.base())
                && self.can_use_imm_offset(dst.ty(), offset)
            {
                // Base register with constant offset.
                let base_reg = self.reg(*reg);
                if is_zero {
                    self.emit_store_at_addr_reg_imm(base_reg, offset, dst.ty(), R::Xzr, &mut asm)?;
                } else {
                    let value_reg = self.emit_rvalue(src, &mut asm)?;
                    self.emit_store_at_addr_reg_imm(
                        base_reg,
                        offset,
                        dst.ty(),
                        value_reg,
                        &mut asm,
                    )?;
                }
                return Ok(asm);
            }
        } else {
            // Compute address first, then value; preserve addr across rvalue evaluation.
            let _ = self.emit_place_addr(dst.base(), dst.projections(), &mut asm)?;
            asm.push_str("  mov x14, x17\n");
            if is_zero {
                self.emit_store_at_addr_reg(R::X14, dst.ty(), R::Xzr, &mut asm)?;
            } else {
                let value_reg = self.emit_rvalue(src, &mut asm)?;
                self.emit_store_at_addr_reg(R::X14, dst.ty(), value_reg, &mut asm)?;
            }
        }
        Ok(asm)
    }

    fn is_zero_rvalue(src: &Rvalue) -> bool {
        matches!(
            src,
            Rvalue::Use(Operand::Const(Const::Unit))
                | Rvalue::Use(Operand::Const(Const::Bool(false)))
                | Rvalue::Use(Operand::Const(Const::Int { value: 0, .. }))
        )
    }

    fn emit_call(&mut self, callee: &Callee) -> Result<String, CodegenError> {
        let mut asm = String::new();

        // Lookup callee name.
        let name = match callee {
            Callee::Def(def_id) => self
                .def_names
                .get(def_id)
                .cloned()
                .ok_or(*def_id)
                .map_err(CodegenError::CalleeNameNotFound)?,
            Callee::Runtime(func) => func.sig().name.to_string(),
        };

        // Emit branch to callee.
        asm.push_str(&format!("  bl _{}\n", name));

        Ok(asm)
    }

    fn emit_terminator(
        &mut self,
        block_id: BlockId,
        term: &Terminator,
    ) -> Result<String, CodegenError> {
        let mut asm = String::new();

        match term {
            Terminator::Return => {
                asm.push_str(&self.emit_epilogue()?);
            }
            Terminator::Goto(target) => {
                asm.push_str(&format!("  b {}\n", self.block_labels[target]));
            }
            Terminator::If {
                cond,
                then_bb,
                else_bb,
            } => {
                if let Operand::Const(Const::Bool(value)) = cond {
                    // Constant condition: branch directly to the appropriate block.
                    let target = if *value { then_bb } else { else_bb };
                    asm.push_str(&format!("  b {}\n", self.block_labels[target]));
                } else {
                    // Evaluate condition and branch accordingly.
                    let cond_reg = self.materialize_operand(cond, &mut asm, R::X16)?;
                    asm.push_str(&format!("  cmp {}, #0\n", cond_reg));
                    asm.push_str(&format!("  b.ne {}\n", self.block_labels[then_bb]));
                    asm.push_str(&format!("  b {}\n", self.block_labels[else_bb]));
                }
            }
            Terminator::Switch {
                discr,
                cases,
                default,
            } => {
                // Evaluate discriminator and branch to the appropriate case.
                let discr_reg = self.materialize_operand(discr, &mut asm, R::X16)?;
                for case in cases {
                    asm.push_str(&format!("  cmp {}, #{}\n", discr_reg, case.value));
                    asm.push_str(&format!("  b.eq {}\n", self.block_labels[&case.target]));
                }
                // Branch to default block if no case matches.
                asm.push_str(&format!("  b {}\n", self.block_labels[default]));
            }
            Terminator::Unreachable => {}
            Terminator::Unterminated => {
                // Should not happen in well-formed IR.
                let label = self
                    .block_labels
                    .get(&block_id)
                    .cloned()
                    .unwrap_or_default();
                return Err(CodegenError::UnterminatedBlock(label));
            }
        }
        Ok(asm)
    }

    fn emit_rvalue(&mut self, rv: &Rvalue, asm: &mut String) -> Result<R, CodegenError> {
        match rv {
            Rvalue::Use(op) => self.materialize_operand(op, asm, R::X16),
            Rvalue::UnOp { op, arg } => {
                // Unary ops reuse a scratch register for the result.
                let operand_reg = self.materialize_operand(arg, asm, R::X16)?;
                match op {
                    UnOp::Neg => {
                        asm.push_str(&format!("  neg x16, {}\n", operand_reg));
                        Ok(R::X16)
                    }
                    UnOp::BitNot => {
                        asm.push_str(&format!("  mvn x16, {}\n", operand_reg));
                        Ok(R::X16)
                    }
                }
            }
            Rvalue::BinOp { op, lhs, rhs } => {
                match op {
                    BinOp::Add => {
                        // Prefer immediate forms when one operand is a constant.
                        if let Operand::Const(Const::Int { value, .. }) = rhs {
                            let lhs_reg = self.materialize_operand(lhs, asm, R::X16)?;
                            let scratch_index = if lhs_reg == R::X16 { 1 } else { 0 };
                            let rhs_op = self.operand_for_int_with_policy(
                                *value as i64,
                                ImmPolicy::AddSubImm,
                                asm,
                                scratch_index,
                            );
                            asm.push_str(&format!("  add x16, {}, {}\n", lhs_reg, rhs_op));
                            return Ok(R::X16);
                        }
                        if let Operand::Const(Const::Int { value, .. }) = lhs {
                            let rhs_reg = self.materialize_operand(rhs, asm, R::X16)?;
                            let scratch_index = if rhs_reg == R::X16 { 1 } else { 0 };
                            let lhs_op = self.operand_for_int_with_policy(
                                *value as i64,
                                ImmPolicy::AddSubImm,
                                asm,
                                scratch_index,
                            );
                            asm.push_str(&format!("  add x16, {}, {}\n", rhs_reg, lhs_op));
                            return Ok(R::X16);
                        }
                    }
                    BinOp::Sub => {
                        // Prefer immediate forms when subtracting a constant.
                        if let Operand::Const(Const::Int { value, .. }) = rhs {
                            let lhs_reg = self.materialize_operand(lhs, asm, R::X16)?;
                            let scratch_index = if lhs_reg == R::X16 { 1 } else { 0 };
                            let rhs_op = self.operand_for_int_with_policy(
                                *value as i64,
                                ImmPolicy::AddSubImm,
                                asm,
                                scratch_index,
                            );
                            asm.push_str(&format!("  sub x16, {}, {}\n", lhs_reg, rhs_op));
                            return Ok(R::X16);
                        }
                    }
                    _ => {}
                }

                let lhs_reg = self.materialize_operand(lhs, asm, R::X16)?;
                let rhs_reg = self.materialize_operand(rhs, asm, R::X17)?;
                match op {
                    BinOp::Add => {
                        asm.push_str(&format!("  add x16, {}, {}\n", lhs_reg, rhs_reg));
                    }
                    BinOp::Sub => {
                        asm.push_str(&format!("  sub x16, {}, {}\n", lhs_reg, rhs_reg));
                    }
                    BinOp::Mul => {
                        asm.push_str(&format!("  mul x16, {}, {}\n", lhs_reg, rhs_reg));
                    }
                    BinOp::Div => {
                        asm.push_str(&format!("  udiv x16, {}, {}\n", lhs_reg, rhs_reg));
                    }
                    BinOp::BitOr => {
                        asm.push_str(&format!("  orr x16, {}, {}\n", lhs_reg, rhs_reg));
                    }
                    BinOp::BitXor => {
                        asm.push_str(&format!("  eor x16, {}, {}\n", lhs_reg, rhs_reg));
                    }
                    BinOp::BitAnd => {
                        asm.push_str(&format!("  and x16, {}, {}\n", lhs_reg, rhs_reg));
                    }
                    BinOp::Shl => {
                        asm.push_str(&format!("  lslv x16, {}, {}\n", lhs_reg, rhs_reg));
                    }
                    BinOp::Shr => {
                        asm.push_str(&format!("  lsrv x16, {}, {}\n", lhs_reg, rhs_reg));
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                        let cond = match op {
                            BinOp::Eq => "eq",
                            BinOp::Ne => "ne",
                            BinOp::Lt => "lt",
                            BinOp::Gt => "gt",
                            BinOp::LtEq => "le",
                            BinOp::GtEq => "ge",
                            _ => unreachable!(),
                        };
                        if let Operand::Const(Const::Int { value, .. }) = rhs {
                            let scratch_index = if lhs_reg == R::X16 { 1 } else { 0 };
                            let rhs_op = self.operand_for_int_with_policy(
                                *value as i64,
                                ImmPolicy::AddSubImm,
                                asm,
                                scratch_index,
                            );
                            asm.push_str(&format!("  cmp {}, {}\n", lhs_reg, rhs_op));
                        } else {
                            asm.push_str(&format!("  cmp {}, {}\n", lhs_reg, rhs_reg));
                        }
                        asm.push_str(&format!("  cset w16, {}\n", cond));
                    }
                }
                Ok(R::X16)
            }
            Rvalue::AddrOf(place) => {
                let addr_reg = self.emit_place_addr_any(place, asm)?;
                Ok(addr_reg)
            }
        }
    }

    fn store_scalar(
        &mut self,
        dst: &Place<crate::mcir::types::Scalar>,
        value_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        let dst_ty = dst.ty();
        if dst.projections().is_empty() {
            // Store directly to the base location.
            match self.alloc.alloc_map.get(&dst.base()) {
                Some(MappedLocal::Reg(reg)) => {
                    let reg = self.reg(*reg);
                    if reg != value_reg {
                        asm.push_str(&format!("  mov {}, {}\n", reg, value_reg));
                    }
                }
                Some(MappedLocal::Stack(slot)) => {
                    self.store_stack(*slot, dst_ty, value_reg, asm)?;
                }
                Some(MappedLocal::StackAddr(slot)) => {
                    self.store_at_stack_addr(*slot, dst_ty, value_reg, asm)?;
                }
                None => return Err(CodegenError::LocalNotFound(dst.base().0)),
            }
        } else {
            // For projected stores, compute the address first.
            let addr_reg = self.emit_place_addr(dst.base(), dst.projections(), asm)?;
            self.store_at_addr(addr_reg, 0, dst_ty, value_reg, asm)?;
        }
        Ok(())
    }

    fn materialize_operand(
        &mut self,
        op: &Operand,
        asm: &mut String,
        scratch: R,
    ) -> Result<R, CodegenError> {
        // Load/copy an operand into a register.
        match op {
            Operand::Copy(place) | Operand::Move(place) => {
                self.load_place_value(place, asm, scratch)
            }
            Operand::Const(c) => {
                let val = match c {
                    Const::Unit => 0,
                    Const::Bool(v) => {
                        if *v {
                            1
                        } else {
                            0
                        }
                    }
                    Const::Int { value, .. } => *value as i64,
                    Const::GlobalAddr { id } => {
                        let label = self.global_labels.get(id).unwrap_or_else(|| {
                            panic!("compiler bug: global label not found for id {}", id.index())
                        });

                        // PC-relative load from the global data section.
                        asm.push_str(&format!("  adrp {}, {}@PAGE\n", scratch, label));
                        asm.push_str(&format!(
                            "  add {}, {}, {}@PAGEOFF\n",
                            scratch, scratch, label
                        ));
                        return Ok(scratch);
                    }
                };
                asm.push_str(&format!("  mov {}, #{}\n", scratch, val));
                Ok(scratch)
            }
        }
    }

    fn load_place_value(
        &mut self,
        place: &Place<crate::mcir::types::Scalar>,
        asm: &mut String,
        scratch: R,
    ) -> Result<R, CodegenError> {
        let ty = place.ty();
        if place.projections().is_empty() {
            // Base local: load directly from its assigned location.
            match self.alloc.alloc_map.get(&place.base()) {
                Some(MappedLocal::Reg(reg)) => Ok(self.reg(*reg)),
                Some(MappedLocal::Stack(slot)) => {
                    self.load_stack(*slot, ty, scratch, asm)?;
                    Ok(scratch)
                }
                Some(MappedLocal::StackAddr(slot)) => {
                    self.load_from_stack_addr(*slot, ty, scratch, asm)?;
                    Ok(scratch)
                }
                None => Err(CodegenError::LocalNotFound(place.base().0)),
            }
        } else if let Some(offset) = self.const_projection_offset(place.base(), place.projections())
        {
            // Static projection offsets let us load directly.
            match self.alloc.alloc_map.get(&place.base()) {
                Some(MappedLocal::StackAddr(slot)) => {
                    let base_offset = self.get_stack_offset(slot)? as usize;
                    let total = base_offset + offset;
                    if let Ok(total) = u32::try_from(total) {
                        self.emit_load_from_sp(total, ty, scratch, asm)?;
                        return Ok(scratch);
                    }
                }
                Some(MappedLocal::Reg(reg)) => {
                    if self.can_use_imm_offset(ty, offset) {
                        let base_reg = self.reg(*reg);
                        self.emit_load_at_addr_reg_imm(base_reg, offset, ty, scratch, asm)?;
                        return Ok(scratch);
                    }
                }
                _ => {}
            }
            // Fallback: compute the address dynamically.
            let addr_reg = self.emit_place_addr(place.base(), place.projections(), asm)?;
            self.load_at_addr(addr_reg, 0, ty, scratch, asm)?;
            Ok(scratch)
        } else {
            // Dynamic projections always compute the address first.
            let addr_reg = self.emit_place_addr(place.base(), place.projections(), asm)?;
            self.load_at_addr(addr_reg, 0, ty, scratch, asm)?;
            Ok(scratch)
        }
    }

    fn emit_place_addr_any(
        &mut self,
        place: &PlaceAny,
        asm: &mut String,
    ) -> Result<R, CodegenError> {
        match place {
            PlaceAny::Scalar(p) => self.emit_place_addr(p.base(), p.projections(), asm),
            PlaceAny::Aggregate(p) => self.emit_place_addr(p.base(), p.projections(), asm),
        }
    }

    fn emit_place_addr(
        &mut self,
        base: LocalId,
        projections: &[Projection],
        asm: &mut String,
    ) -> Result<R, CodegenError> {
        // Compute base address in x17, then walk projections.
        self.materialize_base_addr(base, asm)?;
        let mut cur_kind = self.kind_for_local(base);

        for proj in projections {
            match proj {
                Projection::Field { index } => {
                    let (offset, next_kind) = match cur_kind {
                        TyKind::Tuple { ref field_tys } => {
                            let off = self.field_offset(field_tys, *index);
                            let next = self.body.types.kind(field_tys[*index]).clone();
                            (off, next)
                        }
                        TyKind::Struct { ref fields } => {
                            let off = self.struct_field_offset(fields, *index);
                            let next = self.body.types.kind(fields[*index].ty).clone();
                            (off, next)
                        }
                        _ => return Err(CodegenError::InvalidProjectionType),
                    };
                    if offset != 0 {
                        self.add_to_reg(offset as i64, asm)?;
                    }
                    cur_kind = next_kind;
                }
                Projection::Index { index } => {
                    let (elem_ty, dims) = match cur_kind {
                        TyKind::Array { elem_ty, ref dims } => (elem_ty, dims.clone()),
                        _ => return Err(CodegenError::InvalidProjectionType),
                    };
                    let elem_size = size_of_ty(&self.body.types, elem_ty);
                    let stride_elems: usize = if dims.len() > 1 {
                        dims[1..].iter().product()
                    } else {
                        1
                    };
                    let stride = (stride_elems * elem_size) as i64;
                    if let Operand::Const(Const::Int { value, .. }) = index {
                        let offset = (*value as i64) * stride;
                        if offset != 0 {
                            self.add_to_reg(offset, asm)?;
                        }
                    } else {
                        let index_reg = self.materialize_operand(index, asm, R::X16)?;
                        if stride == 1 {
                            asm.push_str(&format!("  add x17, x17, {}\n", index_reg));
                        } else if let Some(shift) = Self::pow2_shift(stride) {
                            asm.push_str(&format!("  add x17, x17, {}, lsl #{shift}\n", index_reg));
                        } else if index_reg == R::X16 {
                            asm.push_str(&format!("  mov x15, #{stride}\n"));
                            asm.push_str("  madd x17, x16, x15, x17\n");
                        } else {
                            asm.push_str(&format!("  mov x16, #{stride}\n"));
                            asm.push_str(&format!("  madd x17, {}, x16, x17\n", index_reg));
                        }
                    }

                    cur_kind = if dims.len() == 1 {
                        self.body.types.kind(elem_ty).clone()
                    } else {
                        TyKind::Array {
                            elem_ty,
                            dims: dims[1..].to_vec(),
                        }
                    };
                }
                Projection::ByteOffset { offset } => {
                    if *offset != 0 {
                        self.add_to_reg(*offset as i64, asm)?;
                    }
                }
            }
        }

        Ok(R::X17)
    }

    fn pow2_shift(stride: i64) -> Option<u32> {
        if stride <= 0 {
            return None;
        }
        let stride_u = stride as u64;
        if stride_u.is_power_of_two() {
            Some(stride_u.trailing_zeros())
        } else {
            None
        }
    }

    fn materialize_base_addr(
        &mut self,
        base: LocalId,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // Resolve a base local into an address in x17.
        match self.alloc.alloc_map.get(&base) {
            Some(MappedLocal::Reg(reg)) => {
                let reg = self.reg(*reg);
                asm.push_str(&format!("  mov x17, {}\n", reg));
            }
            Some(MappedLocal::StackAddr(slot)) => {
                let offset = self.get_stack_offset(slot)?;
                asm.push_str(&format!("  add x17, sp, #{offset}\n"));
            }
            Some(MappedLocal::Stack(slot)) => {
                let offset = self.get_stack_offset(slot)?;
                asm.push_str(&format!("  ldr x17, [sp, #{offset}]\n"));
            }
            None => return Err(CodegenError::LocalNotFound(base.0)),
        }
        Ok(())
    }

    fn add_to_reg(&mut self, offset: i64, asm: &mut String) -> Result<(), CodegenError> {
        if offset == 0 {
            return Ok(());
        }
        // Prefer immediate offsets when possible.
        if Self::int_fits_add_sub_imm(offset) {
            asm.push_str(&format!("  add x17, x17, #{offset}\n"));
        } else {
            asm.push_str(&format!("  mov x16, #{offset}\n"));
            asm.push_str("  add x17, x17, x16\n");
        }
        Ok(())
    }

    fn load_stack(
        &mut self,
        slot: StackSlotId,
        ty: TyId,
        dest_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // Load a scalar from a stack slot.
        let offset = self.get_stack_offset(&slot)?;
        self.emit_load_from_sp(offset, ty, dest_reg, asm)?;
        Ok(())
    }

    fn load_from_stack_addr(
        &mut self,
        slot: StackSlotId,
        ty: TyId,
        dest_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // StackAddr holds an address; load through it.
        let offset = self.get_stack_offset(&slot)?;
        asm.push_str(&format!("  add x17, sp, #{offset}\n"));
        self.emit_load_at_addr_reg(R::X17, ty, dest_reg, asm)?;
        Ok(())
    }

    fn store_stack(
        &mut self,
        slot: StackSlotId,
        ty: TyId,
        src_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // Store a scalar directly to a stack slot.
        let offset = self.get_stack_offset(&slot)?;
        self.emit_store_to_sp(offset, ty, src_reg, asm)
    }

    fn store_at_stack_addr(
        &mut self,
        slot: StackSlotId,
        ty: TyId,
        src_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // StackAddr holds an address; store through it.
        let offset = self.get_stack_offset(&slot)?;
        asm.push_str(&format!("  add x17, sp, #{offset}\n"));
        self.emit_store_at_addr_reg(R::X17, ty, src_reg, asm)
    }

    fn load_at_addr(
        &mut self,
        addr_reg: R,
        offset: usize,
        ty: TyId,
        dest_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // Load from an address plus optional offset.
        if offset != 0 {
            self.add_to_reg(offset as i64, asm)?;
        }
        self.emit_load_at_addr_reg(addr_reg, ty, dest_reg, asm)
    }

    fn store_at_addr(
        &mut self,
        addr_reg: R,
        offset: usize,
        ty: TyId,
        src_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // Store to an address plus optional offset.
        if offset != 0 {
            self.add_to_reg(offset as i64, asm)?;
        }
        self.emit_store_at_addr_reg(addr_reg, ty, src_reg, asm)
    }

    fn emit_load_from_sp(
        &mut self,
        offset: u32,
        ty: TyId,
        dest_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // Load a scalar from [sp + offset] based on size.
        let size = size_of_ty(&self.body.types, ty);
        match size {
            1 => asm.push_str(&format!(
                "  ldrb {}, [sp, #{}]\n",
                to_w_reg(dest_reg),
                offset
            )),
            2 => asm.push_str(&format!(
                "  ldrh {}, [sp, #{}]\n",
                to_w_reg(dest_reg),
                offset
            )),
            4 => asm.push_str(&format!(
                "  ldr {}, [sp, #{}]\n",
                to_w_reg(dest_reg),
                offset
            )),
            8 => asm.push_str(&format!("  ldr {}, [sp, #{}]\n", dest_reg, offset)),
            _ => return Err(CodegenError::UnsupportedSize(size)),
        }
        Ok(())
    }

    fn emit_store_to_sp(
        &mut self,
        offset: u32,
        ty: TyId,
        src_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // Store a scalar to [sp + offset] based on size.
        let size = size_of_ty(&self.body.types, ty);
        match size {
            1 => asm.push_str(&format!(
                "  strb {}, [sp, #{}]\n",
                to_w_reg(src_reg),
                offset
            )),
            2 => asm.push_str(&format!(
                "  strh {}, [sp, #{}]\n",
                to_w_reg(src_reg),
                offset
            )),
            4 => asm.push_str(&format!("  str {}, [sp, #{}]\n", to_w_reg(src_reg), offset)),
            8 => asm.push_str(&format!("  str {}, [sp, #{}]\n", src_reg, offset)),
            _ => return Err(CodegenError::UnsupportedSize(size)),
        }
        Ok(())
    }

    fn emit_load_at_addr_reg(
        &mut self,
        addr_reg: R,
        ty: TyId,
        dest_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // Load from [addr_reg] based on size.
        let size = size_of_ty(&self.body.types, ty);
        match size {
            1 => asm.push_str(&format!("  ldrb {}, [{}]\n", to_w_reg(dest_reg), addr_reg)),
            2 => asm.push_str(&format!("  ldrh {}, [{}]\n", to_w_reg(dest_reg), addr_reg)),
            4 => asm.push_str(&format!("  ldr {}, [{}]\n", to_w_reg(dest_reg), addr_reg)),
            8 => asm.push_str(&format!("  ldr {}, [{}]\n", dest_reg, addr_reg)),
            _ => return Err(CodegenError::UnsupportedSize(size)),
        }
        Ok(())
    }

    fn emit_load_at_addr_reg_imm(
        &mut self,
        addr_reg: R,
        offset: usize,
        ty: TyId,
        dest_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // Load from [addr_reg + imm] based on size.
        let size = size_of_ty(&self.body.types, ty);
        match size {
            1 => asm.push_str(&format!(
                "  ldrb {}, [{}, #{}]\n",
                to_w_reg(dest_reg),
                addr_reg,
                offset
            )),
            2 => asm.push_str(&format!(
                "  ldrh {}, [{}, #{}]\n",
                to_w_reg(dest_reg),
                addr_reg,
                offset
            )),
            4 => asm.push_str(&format!(
                "  ldr {}, [{}, #{}]\n",
                to_w_reg(dest_reg),
                addr_reg,
                offset
            )),
            8 => asm.push_str(&format!(
                "  ldr {}, [{}, #{}]\n",
                dest_reg, addr_reg, offset
            )),
            _ => return Err(CodegenError::UnsupportedSize(size)),
        }
        Ok(())
    }

    fn emit_store_at_addr_reg(
        &mut self,
        addr_reg: R,
        ty: TyId,
        src_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // Store to [addr_reg] based on size.
        let size = size_of_ty(&self.body.types, ty);
        match size {
            1 => asm.push_str(&format!("  strb {}, [{}]\n", to_w_reg(src_reg), addr_reg)),
            2 => asm.push_str(&format!("  strh {}, [{}]\n", to_w_reg(src_reg), addr_reg)),
            4 => asm.push_str(&format!("  str {}, [{}]\n", to_w_reg(src_reg), addr_reg)),
            8 => asm.push_str(&format!("  str {}, [{}]\n", src_reg, addr_reg)),
            _ => return Err(CodegenError::UnsupportedSize(size)),
        }
        Ok(())
    }

    fn emit_store_at_addr_reg_imm(
        &mut self,
        addr_reg: R,
        offset: usize,
        ty: TyId,
        src_reg: R,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        // Store to [addr_reg + imm] based on size.
        let size = size_of_ty(&self.body.types, ty);
        match size {
            1 => asm.push_str(&format!(
                "  strb {}, [{}, #{}]\n",
                to_w_reg(src_reg),
                addr_reg,
                offset
            )),
            2 => asm.push_str(&format!(
                "  strh {}, [{}, #{}]\n",
                to_w_reg(src_reg),
                addr_reg,
                offset
            )),
            4 => asm.push_str(&format!(
                "  str {}, [{}, #{}]\n",
                to_w_reg(src_reg),
                addr_reg,
                offset
            )),
            8 => asm.push_str(&format!("  str {}, [{}, #{}]\n", src_reg, addr_reg, offset)),
            _ => return Err(CodegenError::UnsupportedSize(size)),
        }
        Ok(())
    }

    fn can_use_imm_offset(&self, ty: TyId, offset: usize) -> bool {
        // AArch64 scaled offsets are in units of the access size.
        let size = size_of_ty(&self.body.types, ty);
        if size == 0 || !offset.is_multiple_of(size) {
            return false;
        }
        let scaled = offset / size;
        scaled <= 4095
    }

    fn emit_moves(&mut self, moves: &[Move]) -> Result<String, CodegenError> {
        moves.iter().map(|m| self.emit_move(m)).collect()
    }

    fn emit_move(&mut self, mov: &Move) -> Result<String, CodegenError> {
        let mut asm = String::new();
        // Emit a single move between abstract locations.
        let inst = match (&mov.from, &mov.to) {
            (Location::PlaceAddr(place), Location::Reg(to_reg)) => {
                let _ = self.emit_place_addr_any(place, &mut asm)?;
                let to_reg = self.reg(*to_reg);
                asm.push_str(&format!("  mov {to_reg}, x17\n"));
                return Ok(asm);
            }
            (Location::PlaceValue(place), Location::Reg(to_reg)) => {
                let to_reg = self.reg(*to_reg);
                let _ = self.load_place_value(place, &mut asm, to_reg)?;
                return Ok(asm);
            }
            (Location::Reg(from_reg), Location::Stack(to_slot)) => format!(
                "  str {}, [sp, #{}]\n",
                self.reg(*from_reg),
                self.get_stack_offset(to_slot)?
            ),
            (Location::Imm(value), Location::Stack(to_slot)) => {
                let scratch =
                    self.operand_for_int_with_policy(*value, ImmPolicy::RegOnly, &mut asm, 0);
                format!(
                    "  str {scratch}, [sp, #{}]\n",
                    self.get_stack_offset(to_slot)?
                )
            }
            (src, Location::Reg(to_reg)) => match src {
                Location::Imm(value) => {
                    let to_reg = self.reg(*to_reg);
                    format!("  mov {to_reg}, #{value}\n")
                }
                Location::Reg(from_reg) => {
                    let to_reg = self.reg(*to_reg);
                    format!("  mov {to_reg}, {}\n", self.reg(*from_reg))
                }
                Location::Stack(from_slot) => format!(
                    "  ldr {}, [sp, #{}]\n",
                    self.reg(*to_reg),
                    self.get_stack_offset(from_slot)?
                ),
                Location::StackAddr(from_slot) => format!(
                    "  add {}, sp, #{}\n",
                    self.reg(*to_reg),
                    self.get_stack_offset(from_slot)?
                ),
                Location::PlaceAddr(_) | Location::PlaceValue(_) => {
                    return Err(CodegenError::UnsupportedMove(
                        mov.from.clone(),
                        mov.to.clone(),
                    ));
                }
            },
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

    fn get_stack_offset(&self, slot: &StackSlotId) -> Result<u32, CodegenError> {
        // Stack slots grow down; compute offset from SP.
        if slot.0 >= self.alloc.stack_slot_count {
            return Err(CodegenError::StackSlotOutOfBounds(
                slot.0,
                self.alloc.stack_slot_count,
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

    fn kind_for_local(&self, local: LocalId) -> TyKind {
        let ty = self.body.locals[local.index()].ty;
        self.body.types.kind(ty).clone()
    }

    fn field_offset(&self, field_tys: &[TyId], index: usize) -> usize {
        // Sum sizes of preceding fields.
        field_tys
            .iter()
            .take(index)
            .map(|ty| size_of_ty(&self.body.types, *ty))
            .sum()
    }

    fn struct_field_offset(
        &self,
        fields: &[crate::mcir::types::StructField],
        index: usize,
    ) -> usize {
        // Sum sizes of preceding struct fields.
        fields
            .iter()
            .take(index)
            .map(|field| size_of_ty(&self.body.types, field.ty))
            .sum()
    }

    fn const_projection_offset(&self, base: LocalId, projections: &[Projection]) -> Option<usize> {
        // Compute a static offset when all indices are constant.
        let mut offset: usize = 0;
        let mut cur_kind = self.kind_for_local(base);

        for proj in projections {
            match proj {
                Projection::Field { index } => {
                    let (field_offset, next_kind) = match cur_kind {
                        TyKind::Tuple { ref field_tys } => {
                            let off = self.field_offset(field_tys, *index);
                            let next = self.body.types.kind(field_tys[*index]).clone();
                            (off, next)
                        }
                        TyKind::Struct { ref fields } => {
                            let off = self.struct_field_offset(fields, *index);
                            let next = self.body.types.kind(fields[*index].ty).clone();
                            (off, next)
                        }
                        _ => return None,
                    };
                    offset = offset.checked_add(field_offset)?;
                    cur_kind = next_kind;
                }
                Projection::Index { index } => {
                    let (elem_ty, dims) = match cur_kind {
                        TyKind::Array { elem_ty, ref dims } => (elem_ty, dims.clone()),
                        _ => return None,
                    };
                    let idx = match index {
                        Operand::Const(Const::Int { value, .. }) => *value,
                        _ => return None,
                    };
                    if idx < 0 {
                        return None;
                    }
                    let idx = idx as usize;
                    let elem_size = size_of_ty(&self.body.types, elem_ty);
                    let stride_elems: usize = if dims.len() > 1 {
                        dims[1..].iter().product()
                    } else {
                        1
                    };
                    let stride = stride_elems * elem_size;
                    offset = offset.checked_add(idx.checked_mul(stride)?)?;

                    cur_kind = if dims.len() == 1 {
                        self.body.types.kind(elem_ty).clone()
                    } else {
                        TyKind::Array {
                            elem_ty,
                            dims: dims[1..].to_vec(),
                        }
                    };
                }
                Projection::ByteOffset {
                    offset: byte_offset,
                } => {
                    offset = offset.checked_add(*byte_offset)?;
                }
            }
        }

        Some(offset)
    }

    fn operand_for_int_with_policy(
        &mut self,
        value: i64,
        policy: ImmPolicy,
        asm: &mut String,
        scratch_index: u8,
    ) -> String {
        // Emit immediates when possible, otherwise materialize into a scratch reg.
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

    fn int_fits_add_sub_imm(value: i64) -> bool {
        if value < 0 {
            return false;
        }
        let v = value as u64;
        v < (1 << 12) || (v < (1 << 24) && (v & ((1 << 12) - 1)) == 0)
    }
}

#[cfg(test)]
#[path = "../../tests/t_arm64.rs"]
mod tests;
