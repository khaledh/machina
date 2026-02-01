use std::fmt::Write;

use crate::regalloc::target::PhysReg;

use super::Arm64Emitter;

#[derive(Debug, Clone, Copy)]
pub(super) struct FrameLayout {
    pub(super) aligned_size: u32,
    pub(super) saved_size: u32,
    pub(super) frame_size: u32,
}

impl FrameLayout {
    pub(super) fn new(frame_size: u32, saved_size: u32) -> Self {
        let total = frame_size.saturating_add(saved_size);
        let aligned_size = Self::align_frame(total);
        Self {
            aligned_size,
            saved_size,
            frame_size,
        }
    }

    pub(super) fn align_frame(size: u32) -> u32 {
        (size + 15) & !15
    }

    pub(super) fn slot_offset(self, slot: crate::ssa::regalloc::StackSlotId) -> u32 {
        if self.aligned_size == 0 {
            return slot.offset_bytes();
        }
        let slot_base = self.aligned_size.saturating_sub(self.saved_size);
        slot_base.saturating_sub(slot.offset_bytes())
    }

    pub(super) fn saved_base(self) -> u32 {
        self.aligned_size.saturating_sub(self.saved_size)
    }

    pub(super) fn outgoing_base(self) -> u32 {
        self.aligned_size
            .saturating_sub(self.frame_size.saturating_add(self.saved_size))
    }

    pub(super) fn outgoing_offset(self, offset: u32) -> u32 {
        self.outgoing_base().saturating_add(offset)
    }

    pub(super) fn incoming_offset(self, offset: u32) -> u32 {
        self.aligned_size.saturating_add(offset)
    }
}

impl Arm64Emitter {
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
        // Capture the callee-saved set so we can restore it in the epilogue.
        self.callee_saved = callee_saved.to_vec();
        if !self.callee_saved.iter().any(|reg| reg.0 == 30) {
            self.callee_saved.push(PhysReg(30));
        }
        self.callee_saved.sort_by_key(|reg| reg.0);
        let saved_size = (self.callee_saved.len() as u32) * 8;
        // Lay out the frame as: [callee-saved area][local/slot area], aligned to 16 bytes.
        self.layout = FrameLayout::new(frame_size, saved_size);

        let _ = writeln!(self.output, "{}:", name);

        // Reserve the full frame size up front.
        if self.layout.aligned_size > 0 {
            self.emit_line(&format!("sub sp, sp, #{}", self.layout.aligned_size));
        }

        if !self.callee_saved.is_empty() {
            // Save callee-saved registers at the top of the frame.
            let base = self.layout.saved_base();
            let callee_saved = self.callee_saved.clone();
            let pair_count = callee_saved.len() / 2;

            // Emit paired saves for contiguous register pairs.
            for pair in 0..pair_count {
                let idx = pair * 2;
                let reg0 = callee_saved[idx];
                let reg1 = callee_saved[idx + 1];
                let offset = base + (pair as u32) * 16;
                self.emit_stp_sp(reg0, reg1, offset);
            }

            // Handle a trailing register if the count is odd.
            if callee_saved.len() % 2 == 1 {
                let reg = callee_saved[pair_count * 2];
                let offset = base + (pair_count as u32) * 16;
                self.emit_str_sp(reg, offset);
            }
        }
    }

    pub(super) fn emit_epilogue(&mut self) {
        if self.layout.aligned_size == 0 {
            return;
        }

        if !self.callee_saved.is_empty() {
            let base = self.layout.saved_base();
            let callee_saved = self.callee_saved.clone();

            // Restore callee-saved registers in reverse order.
            let pair_count = callee_saved.len() / 2;

            // Restore a trailing odd register first.
            if callee_saved.len() % 2 == 1 {
                let reg = callee_saved[pair_count * 2];
                let offset = base + (pair_count as u32) * 16;
                self.emit_ldr_sp(reg, offset);
            }

            // Restore paired registers in reverse pair order.
            for pair in (0..pair_count).rev() {
                let idx = pair * 2;
                let reg0 = callee_saved[idx];
                let reg1 = callee_saved[idx + 1];
                let offset = base + (pair as u32) * 16;
                self.emit_ldp_sp(reg0, reg1, offset);
            }
        }
        self.emit_line(&format!("add sp, sp, #{}", self.layout.aligned_size));
    }
}
