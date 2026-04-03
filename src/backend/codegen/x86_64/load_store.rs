use super::*;
use crate::backend::regalloc::StackSlotId;
use crate::ir::IrTypeId;

impl X86_64Emitter {
    pub(super) fn emit_lea_rsp_offset(&mut self, dst: &str, offset: u32) {
        if offset == 0 {
            self.emit_line(&format!("movq %rsp, {}", dst));
        } else {
            self.emit_line(&format!("leaq {}, {}", Self::mem("rsp", offset), dst));
        }
    }

    pub(super) fn value_dst_typed(
        &mut self,
        locs: &LocationResolver,
        dst: Location,
        scratch: PhysReg,
        context: &str,
        ty: IrTypeId,
    ) -> (&'static str, Option<StackSlotId>) {
        match dst {
            Location::Reg(reg) => (Self::reg_for_type(locs, ty, reg), None),
            Location::Stack(slot) => (Self::reg_for_type(locs, ty, scratch), Some(slot)),
            other => panic!("backend codegen: invalid {} dst {:?}", context, other),
        }
    }

    pub(super) fn store_if_needed_typed(
        &mut self,
        locs: &LocationResolver,
        slot: Option<StackSlotId>,
        reg: &str,
        ty: IrTypeId,
    ) {
        if let Some(slot) = slot {
            let size = Self::scalar_size(locs, ty);
            let offset = self.stack_offset(slot);
            match size {
                0 => {}
                1 => self.emit_line(&format!("movb {}, {}", reg, Self::mem("rsp", offset))),
                2 => self.emit_line(&format!("movw {}, {}", reg, Self::mem("rsp", offset))),
                4 => self.emit_line(&format!("movl {}, {}", reg, Self::mem("rsp", offset))),
                8 => self.emit_line(&format!("movq {}, {}", reg, Self::mem("rsp", offset))),
                other => panic!("backend codegen: unsupported scalar store size {other}"),
            }
        }
    }

    pub(super) fn load_value_typed(
        &mut self,
        locs: &LocationResolver,
        loc: Location,
        ty: IrTypeId,
        scratch: PhysReg,
    ) -> &'static str {
        let size = Self::scalar_size(locs, ty);
        match loc {
            Location::Reg(reg) => Self::reg_for_type(locs, ty, reg),
            Location::Stack(slot) => {
                let offset = self.stack_offset(slot);
                self.emit_load_sized(scratch, "rsp", offset, size);
                Self::reg_for_type(locs, ty, scratch)
            }
            Location::StackOffset(slot, extra) => {
                let offset = self.stack_offset(slot).saturating_add(extra);
                self.emit_load_sized(scratch, "rsp", offset, size);
                Self::reg_for_type(locs, ty, scratch)
            }
            Location::IncomingArg(offset) => {
                let offset = self.layout.incoming_offset(offset);
                self.emit_load_sized(scratch, "rsp", offset, size);
                Self::reg_for_type(locs, ty, scratch)
            }
            Location::OutgoingArg(offset) => {
                let offset = self.layout.outgoing_offset(offset);
                self.emit_load_sized(scratch, "rsp", offset, size);
                Self::reg_for_type(locs, ty, scratch)
            }
            Location::StackAddr(slot) => {
                let offset = self.stack_offset(slot);
                self.emit_lea_rsp_offset(Self::reg(64, scratch), offset);
                Self::reg(64, scratch)
            }
        }
    }

    pub(super) fn emit_load_sized(&mut self, scratch: PhysReg, base: &str, offset: u32, size: u32) {
        match size {
            0 => {}
            1 => self.emit_line(&format!(
                "movzbq {}, {}",
                Self::mem(base, offset),
                Self::reg(64, scratch)
            )),
            2 => self.emit_line(&format!(
                "movzwq {}, {}",
                Self::mem(base, offset),
                Self::reg(64, scratch)
            )),
            4 => self.emit_line(&format!(
                "movl {}, {}",
                Self::mem(base, offset),
                Self::reg(32, scratch)
            )),
            8 => self.emit_line(&format!(
                "movq {}, {}",
                Self::mem(base, offset),
                Self::reg(64, scratch)
            )),
            other => panic!("backend codegen: unsupported scalar load size {other}"),
        }
    }

    pub(super) fn emit_load_ptr_sized(&mut self, dst_reg: PhysReg, addr_reg: &str, size: u32) {
        let mem = if addr_reg.starts_with('%') {
            format!("({})", addr_reg)
        } else {
            format!("(%{})", addr_reg)
        };
        match size {
            0 => {}
            1 => self.emit_line(&format!("movzbq {}, {}", mem, Self::reg(64, dst_reg))),
            2 => self.emit_line(&format!("movzwq {}, {}", mem, Self::reg(64, dst_reg))),
            4 => self.emit_line(&format!("movl {}, {}", mem, Self::reg(32, dst_reg))),
            8 => self.emit_line(&format!("movq {}, {}", mem, Self::reg(64, dst_reg))),
            other => panic!("backend codegen: unsupported scalar load size {other}"),
        }
    }

    pub(super) fn emit_store_ptr_sized(&mut self, src_reg: &str, addr_reg: &str, size: u32) {
        let mem = if addr_reg.starts_with('%') {
            format!("({})", addr_reg)
        } else {
            format!("(%{})", addr_reg)
        };
        match size {
            0 => {}
            1 => self.emit_line(&format!("movb {}, {}", src_reg, mem)),
            2 => self.emit_line(&format!("movw {}, {}", src_reg, mem)),
            4 => self.emit_line(&format!("movl {}, {}", src_reg, mem)),
            8 => self.emit_line(&format!("movq {}, {}", src_reg, mem)),
            other => panic!("backend codegen: unsupported scalar store size {other}"),
        }
    }

    pub(super) fn emit_store_ptr_sized_offset(
        &mut self,
        src_reg: &str,
        base_reg: &str,
        offset: u32,
        size: u32,
    ) {
        let mem = if base_reg.starts_with('%') {
            if offset == 0 {
                format!("({})", base_reg)
            } else {
                format!("{}({})", offset, base_reg)
            }
        } else if offset == 0 {
            format!("(%{})", base_reg)
        } else {
            format!("{}(%{})", offset, base_reg)
        };
        match size {
            0 => {}
            1 => self.emit_line(&format!("movb {}, {}", src_reg, mem)),
            2 => self.emit_line(&format!("movw {}, {}", src_reg, mem)),
            4 => self.emit_line(&format!("movl {}, {}", src_reg, mem)),
            8 => self.emit_line(&format!("movq {}, {}", src_reg, mem)),
            other => panic!("backend codegen: unsupported scalar store size {other}"),
        }
    }

    pub(super) fn copy_ptr_to_stack(&mut self, src_reg: &str, dst_offset: u32, size: u32) {
        let mut offset = 0u32;
        while offset + 8 <= size {
            self.emit_line(&format!("movq {}({}), %r10", offset, src_reg));
            self.emit_line(&format!(
                "movq %r10, {}",
                Self::mem("rsp", dst_offset + offset)
            ));
            offset += 8;
        }
        if offset + 4 <= size {
            self.emit_line(&format!("movl {}({}), %r10d", offset, src_reg));
            self.emit_line(&format!(
                "movl %r10d, {}",
                Self::mem("rsp", dst_offset + offset)
            ));
            offset += 4;
        }
        if offset + 2 <= size {
            self.emit_line(&format!("movzwq {}({}), %r10", offset, src_reg));
            self.emit_line(&format!(
                "movw %r10w, {}",
                Self::mem("rsp", dst_offset + offset)
            ));
            offset += 2;
        }
        if offset < size {
            self.emit_line(&format!("movzbq {}({}), %r10", offset, src_reg));
            self.emit_line(&format!(
                "movb %r10b, {}",
                Self::mem("rsp", dst_offset + offset)
            ));
        }
    }

    pub(super) fn copy_stack_to_ptr(&mut self, src_offset: u32, dst_reg: &str, size: u32) {
        let mut offset = 0u32;
        while offset + 8 <= size {
            self.emit_line(&format!(
                "movq {}, %r10",
                Self::mem("rsp", src_offset + offset)
            ));
            self.emit_line(&format!("movq %r10, {}({})", offset, dst_reg));
            offset += 8;
        }
        if offset + 4 <= size {
            self.emit_line(&format!(
                "movl {}, %r10d",
                Self::mem("rsp", src_offset + offset)
            ));
            self.emit_line(&format!("movl %r10d, {}({})", offset, dst_reg));
            offset += 4;
        }
        if offset + 2 <= size {
            self.emit_line(&format!(
                "movzwq {}, %r10",
                Self::mem("rsp", src_offset + offset)
            ));
            self.emit_line(&format!("movw %r10w, {}({})", offset, dst_reg));
            offset += 2;
        }
        if offset < size {
            self.emit_line(&format!(
                "movzbq {}, %r10",
                Self::mem("rsp", src_offset + offset)
            ));
            self.emit_line(&format!("movb %r10b, {}({})", offset, dst_reg));
        }
    }
}
