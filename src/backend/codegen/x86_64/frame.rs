use std::fmt::Write;

use crate::backend::regalloc::StackSlotId;
use crate::backend::regalloc::target::PhysReg;

use super::X86_64Emitter;

#[derive(Debug, Clone, Copy)]
pub(super) struct FrameLayout {
    pub(super) aligned_size: u32,
    pub(super) pushed_size: u32,
    pub(super) frame_size: u32,
}

impl FrameLayout {
    pub(super) fn new(frame_size: u32, pushed_size: u32) -> Self {
        let push_count = pushed_size / 8;
        let aligned_size = if push_count.is_multiple_of(2) {
            Self::align_frame(frame_size.saturating_add(8)).saturating_sub(8)
        } else {
            Self::align_frame(frame_size)
        };
        Self {
            aligned_size,
            pushed_size,
            frame_size,
        }
    }

    pub(super) fn align_frame(size: u32) -> u32 {
        (size + 15) & !15
    }

    pub(super) fn slot_offset(self, slot: StackSlotId) -> u32 {
        if self.aligned_size == 0 {
            return slot.offset_bytes();
        }
        self.aligned_size.saturating_sub(slot.offset_bytes())
    }

    pub(super) fn outgoing_base(self) -> u32 {
        self.aligned_size.saturating_sub(self.frame_size)
    }

    pub(super) fn outgoing_offset(self, offset: u32) -> u32 {
        self.outgoing_base().saturating_add(offset)
    }

    pub(super) fn incoming_offset(self, offset: u32) -> u32 {
        self.aligned_size
            .saturating_add(self.pushed_size)
            .saturating_add(8)
            .saturating_add(offset)
    }
}

impl X86_64Emitter {
    pub(super) fn begin_function_impl(
        &mut self,
        name: &str,
        frame_size: u32,
        callee_saved: &[PhysReg],
    ) {
        self.ensure_text();
        if !self.output.is_empty() {
            let _ = writeln!(self.output);
        }
        self.block_prefix = format!(".L{}", name);
        let _ = writeln!(self.output, ".globl {}", name);
        self.callee_saved = callee_saved.to_vec();
        let pushed_size = (self.callee_saved.len() as u32) * 8;
        self.layout = FrameLayout::new(frame_size, pushed_size);

        let _ = writeln!(self.output, "{}:", name);

        let callee_saved = self.callee_saved.clone();
        for reg in &callee_saved {
            self.emit_line(&format!("pushq {}", Self::reg(64, *reg)));
        }
        if self.layout.aligned_size > 0 {
            self.emit_line(&format!("subq ${}, %rsp", self.layout.aligned_size));
        }
    }

    pub(super) fn emit_epilogue(&mut self) {
        if self.layout.aligned_size > 0 {
            self.emit_line(&format!("addq ${}, %rsp", self.layout.aligned_size));
        }
        let callee_saved = self.callee_saved.clone();
        for reg in callee_saved.iter().rev() {
            self.emit_line(&format!("popq {}", Self::reg(64, *reg)));
        }
    }
}
