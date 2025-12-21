use std::collections::HashMap;

use thiserror::Error;

use crate::ids::DefId;
use crate::mcir::types::{
    BasicBlock, BinOp, BlockId, Body, Callee, Const, LocalId, Operand, Place, PlaceAny,
    Projection, Rvalue, Statement, Terminator, TyId, TyKind, TyTable, UnOp,
};
use crate::mcregalloc::moves::{Location, Move};
use crate::mcregalloc::pos::InstPos;
use crate::mcregalloc::{AllocationResult, MappedLocal};
use crate::regalloc::regs::{to_w_reg, Arm64Reg as R};
use crate::regalloc::stack::StackSlotId;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Local {0} should be in a register, not stack or stack address")]
    LocalIsNotInRegister(u32),

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
    pub body: &'a Body,
    pub alloc: &'a AllocationResult,
}

pub struct Arm64Codegen<'a> {
    funcs: Vec<McFunction<'a>>,
    def_names: &'a HashMap<DefId, String>,
    label_counter: u32,
}

impl<'a> Arm64Codegen<'a> {
    pub fn new(funcs: Vec<McFunction<'a>>, def_names: &'a HashMap<DefId, String>) -> Self {
        Self {
            funcs,
            def_names,
            label_counter: 0,
        }
    }

    pub fn generate(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();
        for func in &self.funcs {
            let mut codegen =
                FuncCodegen::new(func, self.def_names, self.label_counter)?;
            asm.push_str(&codegen.generate()?);
            asm.push('\n');
            self.label_counter = codegen.label_counter;
        }
        Ok(asm)
    }
}

struct FuncCodegen<'a> {
    name: &'a str,
    body: &'a Body,
    alloc: &'a AllocationResult,
    def_names: &'a HashMap<DefId, String>,
    stack_alloc_size: u32,
    stack_padding: u32,
    label_counter: u32,
    block_labels: HashMap<BlockId, String>,
}

impl<'a> FuncCodegen<'a> {
    pub fn new(
        func: &'a McFunction<'a>,
        def_names: &'a HashMap<DefId, String>,
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
            stack_alloc_size,
            stack_padding,
            label_counter: label_start,
            block_labels: HashMap::new(),
        })
    }

    pub fn generate(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

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

    fn emit_prologue(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();
        asm.push_str(&format!(".global _{}\n", self.name));
        asm.push_str(&format!("_{}:\n", self.name));
        asm.push_str("  stp x29, x30, [sp, #-16]!\n");
        asm.push_str("  mov x29, sp\n");

        let frame_size = self.alloc.frame_size;
        if frame_size > 0 {
            let padded_frame_size = frame_size + self.stack_padding;
            asm.push_str(&format!("  sub sp, sp, #{padded_frame_size}\n"));
        }

        let used_callee = &self.alloc.used_callee_saved;
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
                    offset -= 8;
                    asm.push_str(&format!("  str {}, [sp, #{}]\n", pair[0], offset));
                }
            }
        }

        Ok(asm)
    }

    fn emit_epilogue(&mut self) -> Result<String, CodegenError> {
        let mut asm = String::new();

        let used_callee = &self.alloc.used_callee_saved;
        let mut offset = self.alloc.frame_size as i32;
        if !used_callee.is_empty() {
            for pair in used_callee.chunks(2) {
                if pair.len() == 2 {
                    offset -= 16;
                    asm.push_str(&format!(
                        "  ldp {}, {}, [sp, #{}]\n",
                        pair[0], pair[1], offset
                    ));
                } else {
                    offset -= 8;
                    asm.push_str(&format!("  ldr {}, [sp, #{}]\n", pair[0], offset));
                }
            }
        }

        if self.alloc.frame_size > 0 {
            let padded_frame_size = self.alloc.frame_size + self.stack_padding;
            asm.push_str(&format!("  add sp, sp, #{padded_frame_size}\n"));
        }

        asm.push_str("  ldp x29, x30, [sp], #16\n");
        asm.push_str("  ret\n");
        Ok(asm)
    }

    fn emit_block(&mut self, block_id: BlockId, block: &BasicBlock) -> Result<String, CodegenError> {
        let mut asm = String::new();
        if block_id != self.body.entry {
            asm.push_str(&format!("{}:\n", self.block_labels[&block_id]));
        }

        for (idx, stmt) in block.stmts.iter().enumerate() {
            let pos = InstPos::new(block_id, idx);
            if let Some(moves) = self.alloc.moves.get_inst_moves(pos) {
                asm.push_str(&self.emit_moves(&moves.before_moves)?);
            }

            asm.push_str(&format!("  // {}\n", stmt));
            asm.push_str(&self.emit_stmt(stmt)?);

            if let Some(moves) = self.alloc.moves.get_inst_moves(pos) {
                asm.push_str(&self.emit_moves(&moves.after_moves)?);
            }
        }

        if let Some(ret_move) = self.alloc.moves.get_return_move(block_id) {
            asm.push_str(&self.emit_move(ret_move)?);
        }

        asm.push_str(&format!("  // {}\n", block.terminator));
        asm.push_str(&self.emit_terminator(block_id, &block.terminator)?);
        Ok(asm)
    }

    fn emit_stmt(&mut self, stmt: &Statement) -> Result<String, CodegenError> {
        match stmt {
            Statement::AssignScalar { dst, src } => self.emit_assign_scalar(dst, src),
            Statement::InitAggregate { dst, fields } => self.emit_init_aggregate(dst, fields),
            Statement::CopyAggregate { dst, src } => self.emit_copy_aggregate(dst, src),
            Statement::Call { callee, .. } => self.emit_call(callee),
        }
    }

    fn emit_assign_scalar(
        &mut self,
        dst: &Place<crate::mcir::types::Scalar>,
        src: &Rvalue,
    ) -> Result<String, CodegenError> {
        let mut asm = String::new();
        if dst.projections().is_empty() {
            let value_reg = self.emit_rvalue(src, &mut asm)?;
            self.store_scalar(dst, value_reg, &mut asm)?;
        } else {
            // Compute address first, then value; preserve addr across rvalue evaluation.
            let _ = self.emit_place_addr(dst.base(), dst.projections(), &mut asm)?;
            asm.push_str("  mov x15, x17\n");
            let value_reg = self.emit_rvalue(src, &mut asm)?;
            self.emit_store_at_addr_reg(R::X15, dst.ty(), value_reg, &mut asm)?;
        }
        Ok(asm)
    }

    fn emit_init_aggregate(
        &mut self,
        dst: &Place<crate::mcir::types::Aggregate>,
        fields: &[Operand],
    ) -> Result<String, CodegenError> {
        let mut asm = String::new();
        let base_addr = self.emit_place_addr(dst.base(), dst.projections(), &mut asm)?;
        let mut cur_kind = self.kind_for_local(dst.base());

        match &mut cur_kind {
            TyKind::Tuple { field_tys } => {
                for (i, field) in fields.iter().enumerate() {
                    let offset = self.field_offset(field_tys, i);
                    let value_reg = self.materialize_operand(field, &mut asm, R::X16)?;
                    self.store_at_addr(base_addr, offset, field_tys[i], value_reg, &mut asm)?;
                }
            }
            TyKind::Struct { fields: struct_fields } => {
                for (i, field) in fields.iter().enumerate() {
                    let offset = self.struct_field_offset(struct_fields, i);
                    let value_reg = self.materialize_operand(field, &mut asm, R::X16)?;
                    self.store_at_addr(
                        base_addr,
                        offset,
                        struct_fields[i].ty,
                        value_reg,
                        &mut asm,
                    )?;
                }
            }
            TyKind::Array { elem_ty, .. } => {
                let elem_size = size_of_ty(&self.body.types, *elem_ty);
                for (i, field) in fields.iter().enumerate() {
                    let offset = (i * elem_size) as i64;
                    let value_reg = self.materialize_operand(field, &mut asm, R::X16)?;
                    self.store_at_addr(base_addr, offset as usize, *elem_ty, value_reg, &mut asm)?;
                }
            }
            _ => return Err(CodegenError::InvalidProjectionType),
        }

        Ok(asm)
    }

    fn emit_copy_aggregate(
        &mut self,
        dst: &Place<crate::mcir::types::Aggregate>,
        src: &Place<crate::mcir::types::Aggregate>,
    ) -> Result<String, CodegenError> {
        let mut asm = String::new();
        let _ = self.emit_place_addr(dst.base(), dst.projections(), &mut asm)?;
        asm.push_str("  mov x14, x17\n");
        let _ = self.emit_place_addr(src.base(), src.projections(), &mut asm)?;
        let size = size_of_ty(&self.body.types, dst.ty());
        self.emit_memcpy(R::X14, R::X17, size, &mut asm)?;
        Ok(asm)
    }

    fn emit_call(&mut self, callee: &Callee) -> Result<String, CodegenError> {
        let mut asm = String::new();
        let name = match callee {
            Callee::Def(def_id) => self
                .def_names
                .get(def_id)
                .cloned()
                .ok_or(*def_id)
                .map_err(CodegenError::CalleeNameNotFound)?,
        };
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
                    let target = if *value { then_bb } else { else_bb };
                    asm.push_str(&format!("  b {}\n", self.block_labels[target]));
                } else {
                    let cond_reg = self.materialize_operand(cond, &mut asm, R::X16)?;
                    asm.push_str(&format!("  cmp {}, #0\n", cond_reg));
                    asm.push_str(&format!("  b.ne {}\n", self.block_labels[then_bb]));
                    asm.push_str(&format!("  b {}\n", self.block_labels[else_bb]));
                }
            }
            Terminator::Unterminated => {
                let label = self.block_labels.get(&block_id).cloned().unwrap_or_default();
                return Err(CodegenError::UnterminatedBlock(label));
            }
        }
        Ok(asm)
    }

    fn emit_rvalue(&mut self, rv: &Rvalue, asm: &mut String) -> Result<R, CodegenError> {
        match rv {
            Rvalue::Use(op) => self.materialize_operand(op, asm, R::X16),
            Rvalue::UnOp { op, arg } => {
                let operand_reg = self.materialize_operand(arg, asm, R::X16)?;
                match op {
                    UnOp::Neg => {
                        asm.push_str(&format!("  neg x16, {}\n", operand_reg));
                        Ok(R::X16)
                    }
                }
            }
            Rvalue::BinOp { op, lhs, rhs } => {
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
                    BinOp::Eq
                    | BinOp::Ne
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::LtEq
                    | BinOp::GtEq => {
                        let cond = match op {
                            BinOp::Eq => "eq",
                            BinOp::Ne => "ne",
                            BinOp::Lt => "lt",
                            BinOp::Gt => "gt",
                            BinOp::LtEq => "le",
                            BinOp::GtEq => "ge",
                            _ => unreachable!(),
                        };
                        asm.push_str(&format!("  cmp {}, {}\n", lhs_reg, rhs_reg));
                        asm.push_str(&format!("  cset w16, {}\n", cond));
                    }
                }
                Ok(R::X16)
            }
            Rvalue::AddrOf(place) => {
                let _ = self.emit_place_addr_any(place, asm)?;
                asm.push_str("  mov x16, x17\n");
                Ok(R::X16)
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
            match self.alloc.alloc_map.get(&dst.base()) {
                Some(MappedLocal::Reg(reg)) => {
                    if *reg != value_reg {
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
            match self.alloc.alloc_map.get(&place.base()) {
                Some(MappedLocal::Reg(reg)) => Ok(*reg),
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
        } else {
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
                    let index_reg = self.materialize_operand(index, asm, R::X16)?;
                    if stride == 1 {
                        asm.push_str(&format!("  add x17, x17, {}\n", index_reg));
                    } else if index_reg == R::X16 {
                        asm.push_str(&format!("  mov x15, #{stride}\n"));
                        asm.push_str("  mul x16, x16, x15\n");
                        asm.push_str("  add x17, x17, x16\n");
                    } else {
                        asm.push_str(&format!("  mov x16, #{stride}\n"));
                        asm.push_str(&format!("  mul x16, {}, x16\n", index_reg));
                        asm.push_str("  add x17, x17, x16\n");
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
            }
        }

        Ok(R::X17)
    }

    fn materialize_base_addr(&mut self, base: LocalId, asm: &mut String) -> Result<(), CodegenError> {
        match self.alloc.alloc_map.get(&base) {
            Some(MappedLocal::Reg(reg)) => {
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
            4 => asm.push_str(&format!(
                "  str {}, [sp, #{}]\n",
                to_w_reg(src_reg),
                offset
            )),
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
        let size = size_of_ty(&self.body.types, ty);
        match size {
            1 => asm.push_str(&format!(
                "  ldrb {}, [{}]\n",
                to_w_reg(dest_reg),
                addr_reg
            )),
            2 => asm.push_str(&format!(
                "  ldrh {}, [{}]\n",
                to_w_reg(dest_reg),
                addr_reg
            )),
            4 => asm.push_str(&format!(
                "  ldr {}, [{}]\n",
                to_w_reg(dest_reg),
                addr_reg
            )),
            8 => asm.push_str(&format!("  ldr {}, [{}]\n", dest_reg, addr_reg)),
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
        let size = size_of_ty(&self.body.types, ty);
        match size {
            1 => asm.push_str(&format!(
                "  strb {}, [{}]\n",
                to_w_reg(src_reg),
                addr_reg
            )),
            2 => asm.push_str(&format!(
                "  strh {}, [{}]\n",
                to_w_reg(src_reg),
                addr_reg
            )),
            4 => asm.push_str(&format!(
                "  str {}, [{}]\n",
                to_w_reg(src_reg),
                addr_reg
            )),
            8 => asm.push_str(&format!("  str {}, [{}]\n", src_reg, addr_reg)),
            _ => return Err(CodegenError::UnsupportedSize(size)),
        }
        Ok(())
    }

    fn emit_memcpy(
        &mut self,
        dest_addr: R,
        src_addr: R,
        length: usize,
        asm: &mut String,
    ) -> Result<(), CodegenError> {
        asm.push_str(&format!("  mov x19, {}\n", dest_addr));
        asm.push_str(&format!("  mov x20, {}\n", src_addr));

        let loop_label = self.next_label();
        let done_label = self.next_label();

        let count = length / 8;
        if count > 0 {
            asm.push_str(&format!("  mov x21, #{count}\n"));
            asm.push_str(&format!("  cbz x21, {done_label}\n"));
            asm.push_str(&format!("{loop_label}:\n"));
            asm.push_str("  ldr x22, [x20], #8\n");
            asm.push_str("  str x22, [x19], #8\n");
            asm.push_str("  subs x21, x21, #1\n");
            asm.push_str(&format!("  b.ne {loop_label}\n"));
            asm.push_str(&format!("{done_label}:\n"));
        }

        let rem = length % 8;
        if rem >= 4 {
            asm.push_str("  ldr w22, [x20], #4\n");
            asm.push_str("  str w22, [x19], #4\n");
        }
        if rem >= 2 {
            asm.push_str("  ldrh w22, [x20], #2\n");
            asm.push_str("  strh w22, [x19], #2\n");
        }
        if rem >= 1 {
            asm.push_str("  ldrb w22, [x20], #1\n");
            asm.push_str("  strb w22, [x19], #1\n");
        }

        Ok(())
    }

    fn emit_moves(&mut self, moves: &[Move]) -> Result<String, CodegenError> {
        moves.iter().map(|m| self.emit_move(m)).collect()
    }

    fn emit_move(&mut self, mov: &Move) -> Result<String, CodegenError> {
        let mut asm = String::new();
        let inst = match (&mov.from, &mov.to) {
            (src, Location::Reg(to_reg)) => match src {
                Location::Imm(value) => format!("  mov {to_reg}, #{value}\n"),
                Location::Reg(from_reg) => format!("  mov {to_reg}, {from_reg}\n"),
                Location::Stack(from_slot) => format!(
                    "  ldr {to_reg}, [sp, #{}]\n",
                    self.get_stack_offset(from_slot)?
                ),
                Location::StackAddr(from_slot) => format!(
                    "  add {to_reg}, sp, #{}\n",
                    self.get_stack_offset(from_slot)?
                ),
            },
            (Location::Reg(from_reg), Location::Stack(to_slot)) => format!(
                "  str {from_reg}, [sp, #{}]\n",
                self.get_stack_offset(to_slot)?
            ),
            (Location::Imm(value), Location::Stack(to_slot)) => {
                let scratch = self.operand_for_int_with_policy(
                    *value,
                    ImmPolicy::RegOnly,
                    &mut asm,
                    0,
                );
                format!(
                    "  str {scratch}, [sp, #{}]\n",
                    self.get_stack_offset(to_slot)?
                )
            }
            _ => return Err(CodegenError::UnsupportedMove(mov.from, mov.to)),
        };
        asm.push_str(&inst);
        Ok(asm)
    }

    fn get_stack_offset(&self, slot: &StackSlotId) -> Result<u32, CodegenError> {
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
        fields
            .iter()
            .take(index)
            .map(|field| size_of_ty(&self.body.types, field.ty))
            .sum()
    }

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

    fn int_fits_add_sub_imm(value: i64) -> bool {
        if value < 0 {
            return false;
        }
        let v = value as u64;
        v < (1 << 12) || (v < (1 << 24) && (v & ((1 << 12) - 1)) == 0)
    }
}

fn size_of_ty(types: &TyTable, ty: TyId) -> usize {
    match types.kind(ty) {
        TyKind::Unit => 0,
        TyKind::Bool => 1,
        TyKind::Int { bits, .. } => (*bits as usize + 7) / 8,
        TyKind::Array { elem_ty, dims } => {
            let elems: usize = dims.iter().product();
            elems * size_of_ty(types, *elem_ty)
        }
        TyKind::Tuple { field_tys } => field_tys.iter().map(|ty| size_of_ty(types, *ty)).sum(),
        TyKind::Struct { fields } => fields
            .iter()
            .map(|field| size_of_ty(types, field.ty))
            .sum(),
    }
}

#[cfg(test)]
#[path = "tests/t_arm64.rs"]
mod tests;
