use super::*;
use crate::backend::regalloc::StackSlotId;

impl Arm64Emitter {
    pub(super) fn emit_add_imm(&mut self, dst: &str, base: &str, offset: u32) {
        if offset == 0 {
            if dst != base {
                self.emit_line(&format!("mov {}, {}", dst, base));
            }
            return;
        }
        self.emit_line(&format!("add {}, {}, #{}", dst, base, offset));
    }

    pub(super) fn value_dst_typed(
        &mut self,
        locs: &LocationResolver,
        dst: Location,
        scratch: &str,
        context: &str,
        ty: crate::backend::IrTypeId,
    ) -> (String, Option<StackSlotId>) {
        match dst {
            Location::Reg(reg) => (Self::reg_for_type(locs, ty, reg), None),
            Location::Stack(slot) => {
                let size = Self::scalar_size(locs, ty);
                if size > 0 && size <= 4 {
                    (Self::w_reg(scratch), Some(slot))
                } else {
                    (scratch.to_string(), Some(slot))
                }
            }
            other => {
                panic!("backend codegen: invalid {} dst {:?}", context, other);
            }
        }
    }

    pub(super) fn store_if_needed_typed(
        &mut self,
        locs: &LocationResolver,
        slot: Option<StackSlotId>,
        reg: &str,
        ty: crate::backend::IrTypeId,
    ) {
        if let Some(slot) = slot {
            let size = Self::scalar_size(locs, ty);
            let offset = self.stack_offset(slot);
            match size {
                0 => {}
                1 => {
                    let w = Self::w_reg(reg);
                    self.emit_line(&format!("strb {}, [sp, #{}]", w, offset));
                }
                2 => {
                    let w = Self::w_reg(reg);
                    self.emit_line(&format!("strh {}, [sp, #{}]", w, offset));
                }
                4 => {
                    let w = Self::w_reg(reg);
                    self.emit_line(&format!("str {}, [sp, #{}]", w, offset));
                }
                8 => {
                    self.emit_line(&format!("str {}, [sp, #{}]", reg, offset));
                }
                other => {
                    panic!("backend codegen: unsupported scalar store size {other}");
                }
            }
        }
    }
    pub(super) fn load_value_typed(
        &mut self,
        locs: &LocationResolver,
        loc: Location,
        ty: crate::backend::IrTypeId,
        scratch: &str,
    ) -> String {
        let size = Self::scalar_size(locs, ty);
        match loc {
            Location::Reg(reg) => Self::reg_for_type(locs, ty, reg),
            Location::Stack(slot) => {
                let offset = self.stack_offset(slot);
                self.emit_load_sized(scratch, offset, size);
                if size > 0 && size <= 4 {
                    Self::w_reg(scratch)
                } else {
                    scratch.to_string()
                }
            }
            Location::StackOffset(slot, extra) => {
                let offset = self.stack_offset(slot).saturating_add(extra);
                self.emit_load_sized(scratch, offset, size);
                if size > 0 && size <= 4 {
                    Self::w_reg(scratch)
                } else {
                    scratch.to_string()
                }
            }
            Location::IncomingArg(offset) => {
                let offset = self.layout.incoming_offset(offset);
                self.emit_load_sized(scratch, offset, size);
                if size > 0 && size <= 4 {
                    Self::w_reg(scratch)
                } else {
                    scratch.to_string()
                }
            }
            Location::OutgoingArg(offset) => {
                let offset = self.layout.outgoing_offset(offset);
                self.emit_load_sized(scratch, offset, size);
                if size > 0 && size <= 4 {
                    Self::w_reg(scratch)
                } else {
                    scratch.to_string()
                }
            }
            Location::StackAddr(slot) => {
                let offset = self.stack_offset(slot);
                self.emit_add_imm(scratch, "sp", offset);
                scratch.to_string()
            }
        }
    }

    pub(super) fn emit_load_sized(&mut self, scratch: &str, offset: u32, size: u32) {
        match size {
            0 => {}
            1 => {
                let w = Self::w_reg(scratch);
                self.emit_line(&format!("ldrb {}, [sp, #{}]", w, offset));
            }
            2 => {
                let w = Self::w_reg(scratch);
                self.emit_line(&format!("ldrh {}, [sp, #{}]", w, offset));
            }
            4 => {
                let w = Self::w_reg(scratch);
                self.emit_line(&format!("ldr {}, [sp, #{}]", w, offset));
            }
            8 => {
                self.emit_line(&format!("ldr {}, [sp, #{}]", scratch, offset));
            }
            other => {
                panic!("backend codegen: unsupported scalar load size {other}");
            }
        }
    }

    pub(super) fn emit_load_ptr_sized(&mut self, dst_reg: &str, addr_reg: &str, size: u32) {
        match size {
            0 => {}
            1 => {
                let w = Self::w_reg(dst_reg);
                self.emit_line(&format!("ldrb {}, [{}]", w, addr_reg));
            }
            2 => {
                let w = Self::w_reg(dst_reg);
                self.emit_line(&format!("ldrh {}, [{}]", w, addr_reg));
            }
            4 => {
                let w = Self::w_reg(dst_reg);
                self.emit_line(&format!("ldr {}, [{}]", w, addr_reg));
            }
            8 => {
                self.emit_line(&format!("ldr {}, [{}]", dst_reg, addr_reg));
            }
            other => {
                panic!("backend codegen: unsupported scalar load size {other}");
            }
        }
    }

    pub(super) fn emit_store_ptr_sized(&mut self, src_reg: &str, addr_reg: &str, size: u32) {
        match size {
            0 => {}
            1 => {
                let w = Self::w_reg(src_reg);
                self.emit_line(&format!("strb {}, [{}]", w, addr_reg));
            }
            2 => {
                let w = Self::w_reg(src_reg);
                self.emit_line(&format!("strh {}, [{}]", w, addr_reg));
            }
            4 => {
                let w = Self::w_reg(src_reg);
                self.emit_line(&format!("str {}, [{}]", w, addr_reg));
            }
            8 => {
                self.emit_line(&format!("str {}, [{}]", src_reg, addr_reg));
            }
            other => {
                panic!("backend codegen: unsupported scalar store size {other}");
            }
        }
    }

    pub(super) fn emit_store_ptr_sized_offset(
        &mut self,
        src_reg: &str,
        base_reg: &str,
        offset: u32,
        size: u32,
    ) {
        if offset == 0 {
            self.emit_store_ptr_sized(src_reg, base_reg, size);
            return;
        }

        let scaled = offset % size == 0 && (offset / size) <= 4095;
        if scaled {
            match size {
                1 => {
                    let w = Self::w_reg(src_reg);
                    self.emit_line(&format!("strb {}, [{}, #{}]", w, base_reg, offset));
                }
                2 => {
                    let w = Self::w_reg(src_reg);
                    self.emit_line(&format!("strh {}, [{}, #{}]", w, base_reg, offset));
                }
                4 => {
                    let w = Self::w_reg(src_reg);
                    self.emit_line(&format!("str {}, [{}, #{}]", w, base_reg, offset));
                }
                8 => {
                    self.emit_line(&format!("str {}, [{}, #{}]", src_reg, base_reg, offset));
                }
                other => {
                    panic!("backend codegen: unsupported scalar store size {other}");
                }
            }
            return;
        }

        let scratch = if base_reg == "x15" { "x14" } else { "x15" };
        self.emit_add_imm(scratch, base_reg, offset);
        self.emit_store_ptr_sized(src_reg, scratch, size);
    }

    pub(super) fn copy_ptr_to_stack(&mut self, src_reg: &str, dst_offset: u32, size: u32) {
        let mut offset = 0u32;

        while offset + 8 <= size {
            self.emit_line(&format!("ldr x14, [{}, #{}]", src_reg, offset));
            self.emit_line(&format!("str x14, [sp, #{}]", dst_offset + offset));
            offset += 8;
        }

        if offset + 4 <= size {
            self.emit_line(&format!("ldr w14, [{}, #{}]", src_reg, offset));
            self.emit_line(&format!("str w14, [sp, #{}]", dst_offset + offset));
            offset += 4;
        }

        if offset + 2 <= size {
            self.emit_line(&format!("ldrh w14, [{}, #{}]", src_reg, offset));
            self.emit_line(&format!("strh w14, [sp, #{}]", dst_offset + offset));
            offset += 2;
        }

        if offset < size {
            self.emit_line(&format!("ldrb w14, [{}, #{}]", src_reg, offset));
            self.emit_line(&format!("strb w14, [sp, #{}]", dst_offset + offset));
        }
    }

    pub(super) fn copy_stack_to_ptr(&mut self, src_offset: u32, dst_reg: &str, size: u32) {
        let mut offset = 0u32;

        while offset + 8 <= size {
            self.emit_line(&format!("ldr x14, [sp, #{}]", src_offset + offset));
            self.emit_line(&format!("str x14, [{}, #{}]", dst_reg, offset));
            offset += 8;
        }

        if offset + 4 <= size {
            self.emit_line(&format!("ldr w14, [sp, #{}]", src_offset + offset));
            self.emit_line(&format!("str w14, [{}, #{}]", dst_reg, offset));
            offset += 4;
        }

        if offset + 2 <= size {
            self.emit_line(&format!("ldrh w14, [sp, #{}]", src_offset + offset));
            self.emit_line(&format!("strh w14, [{}, #{}]", dst_reg, offset));
            offset += 2;
        }

        if offset < size {
            self.emit_line(&format!("ldrb w14, [sp, #{}]", src_offset + offset));
            self.emit_line(&format!("strb w14, [{}, #{}]", dst_reg, offset));
        }
    }
}
