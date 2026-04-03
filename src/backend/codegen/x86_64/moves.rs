use super::*;

impl X86_64Emitter {
    pub(super) fn emit_move_ops(&mut self, moves: &[MoveOp]) {
        for mov in moves {
            if mov.size == 0 {
                continue;
            }
            match (mov.src, mov.dst) {
                (Location::Reg(src), Location::Reg(dst)) => {
                    let bits = Self::bits_for_size(mov.size);
                    self.emit_line(&format!(
                        "mov{} {}, {}",
                        size_suffix(mov.size),
                        Self::reg(bits, src),
                        Self::reg(bits, dst)
                    ));
                }
                (Location::Reg(src), Location::Stack(dst)) => {
                    let bits = Self::bits_for_size(mov.size);
                    let offset = self.stack_offset(dst);
                    self.emit_line(&format!(
                        "mov{} {}, {}",
                        size_suffix(mov.size),
                        Self::reg(bits, src),
                        Self::mem("rsp", offset)
                    ));
                }
                (Location::Reg(src), Location::StackOffset(dst, extra)) => {
                    let bits = Self::bits_for_size(mov.size);
                    let offset = self.stack_offset(dst).saturating_add(extra);
                    self.emit_line(&format!(
                        "mov{} {}, {}",
                        size_suffix(mov.size),
                        Self::reg(bits, src),
                        Self::mem("rsp", offset)
                    ));
                }
                (Location::Reg(src), Location::OutgoingArg(offset)) => {
                    let bits = Self::bits_for_size(mov.size);
                    let offset = self.layout.outgoing_offset(offset);
                    self.emit_line(&format!(
                        "mov{} {}, {}",
                        size_suffix(mov.size),
                        Self::reg(bits, src),
                        Self::mem("rsp", offset)
                    ));
                }
                (Location::Stack(src), Location::Reg(dst)) => {
                    let offset = self.stack_offset(src);
                    self.emit_load_sized(dst, "rsp", offset, mov.size);
                }
                (Location::StackOffset(src, extra), Location::Reg(dst)) => {
                    let offset = self.stack_offset(src).saturating_add(extra);
                    self.emit_load_sized(dst, "rsp", offset, mov.size);
                }
                (Location::IncomingArg(offset), Location::Reg(dst)) => {
                    let offset = self.layout.incoming_offset(offset);
                    self.emit_load_sized(dst, "rsp", offset, mov.size);
                }
                (Location::StackAddr(src), Location::Reg(dst)) => {
                    let offset = self.stack_offset(src);
                    self.emit_lea_rsp_offset(Self::reg(64, dst), offset);
                }
                (Location::Stack(src), Location::Stack(dst)) => {
                    let src_offset = self.stack_offset(src);
                    let dst_offset = self.stack_offset(dst);
                    self.copy_stack_to_stack(src_offset, dst_offset, mov.size);
                }
                (Location::Stack(src), Location::StackOffset(dst, extra)) => {
                    let src_offset = self.stack_offset(src);
                    let dst_offset = self.stack_offset(dst).saturating_add(extra);
                    self.copy_stack_to_stack(src_offset, dst_offset, mov.size);
                }
                (Location::StackOffset(src, extra), Location::Stack(dst)) => {
                    let src_offset = self.stack_offset(src).saturating_add(extra);
                    let dst_offset = self.stack_offset(dst);
                    self.copy_stack_to_stack(src_offset, dst_offset, mov.size);
                }
                (Location::StackOffset(src, src_extra), Location::StackOffset(dst, dst_extra)) => {
                    let src_offset = self.stack_offset(src).saturating_add(src_extra);
                    let dst_offset = self.stack_offset(dst).saturating_add(dst_extra);
                    self.copy_stack_to_stack(src_offset, dst_offset, mov.size);
                }
                (Location::IncomingArg(offset), Location::Stack(dst)) => {
                    let src_offset = self.layout.incoming_offset(offset);
                    let dst_offset = self.stack_offset(dst);
                    self.copy_stack_to_stack(src_offset, dst_offset, mov.size);
                }
                (Location::IncomingArg(offset), Location::StackOffset(dst, extra)) => {
                    let src_offset = self.layout.incoming_offset(offset);
                    let dst_offset = self.stack_offset(dst).saturating_add(extra);
                    self.copy_stack_to_stack(src_offset, dst_offset, mov.size);
                }
                (Location::StackAddr(src), Location::Stack(dst)) => {
                    let src_offset = self.stack_offset(src);
                    let dst_offset = self.stack_offset(dst);
                    self.emit_lea_rsp_offset("%r10", src_offset);
                    self.emit_line(&format!("movq %r10, {}", Self::mem("rsp", dst_offset)));
                }
                (Location::StackAddr(src), Location::StackOffset(dst, extra)) => {
                    let src_offset = self.stack_offset(src);
                    let dst_offset = self.stack_offset(dst).saturating_add(extra);
                    self.emit_lea_rsp_offset("%r10", src_offset);
                    self.emit_line(&format!("movq %r10, {}", Self::mem("rsp", dst_offset)));
                }
                other => panic!("backend codegen: unsupported move {:?}", other),
            }
        }
    }

    fn copy_stack_to_stack(&mut self, src_offset: u32, dst_offset: u32, size: u32) {
        match size {
            1 => {
                self.emit_line(&format!("movzbq {}, %r10", Self::mem("rsp", src_offset)));
                self.emit_line(&format!("movb %r10b, {}", Self::mem("rsp", dst_offset)));
            }
            2 => {
                self.emit_line(&format!("movzwq {}, %r10", Self::mem("rsp", src_offset)));
                self.emit_line(&format!("movw %r10w, {}", Self::mem("rsp", dst_offset)));
            }
            4 => {
                self.emit_line(&format!("movl {}, %r10d", Self::mem("rsp", src_offset)));
                self.emit_line(&format!("movl %r10d, {}", Self::mem("rsp", dst_offset)));
            }
            8 => {
                self.emit_line(&format!("movq {}, %r10", Self::mem("rsp", src_offset)));
                self.emit_line(&format!("movq %r10, {}", Self::mem("rsp", dst_offset)));
            }
            other => {
                self.emit_lea_rsp_offset("%r10", src_offset);
                self.copy_ptr_to_stack("%r10", dst_offset, other);
            }
        }
    }
}

fn size_suffix(size: u32) -> &'static str {
    match size {
        1 => "b",
        2 => "w",
        4 => "l",
        8 => "q",
        _ => panic!("backend codegen: unsupported move size {size}"),
    }
}
