use super::*;

impl Arm64Emitter {
    pub(super) fn emit_move_ops(&mut self, moves: &[MoveOp]) {
        for mov in moves {
            if mov.size == 0 {
                continue;
            }
            match (mov.src, mov.dst) {
                (Location::Reg(src), Location::Reg(dst)) => {
                    if mov.size <= 4 {
                        let dst = Self::w_reg(Self::reg_name(dst));
                        let src = Self::w_reg(Self::reg_name(src));
                        self.emit_line(&format!("mov {}, {}", dst, src));
                    } else {
                        self.emit_line(&format!(
                            "mov {}, {}",
                            Self::reg_name(dst),
                            Self::reg_name(src)
                        ));
                    }
                }
                (Location::Reg(src), Location::Stack(dst)) => {
                    let offset = self.stack_offset(dst);
                    match mov.size {
                        1 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strb {}, [sp, #{}]", src, offset));
                        }
                        2 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strh {}, [sp, #{}]", src, offset));
                        }
                        4 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("str {}, [sp, #{}]", src, offset));
                        }
                        8 => {
                            self.emit_line(&format!(
                                "str {}, [sp, #{}]",
                                Self::reg_name(src),
                                offset
                            ));
                        }
                        other => {
                            panic!("backend codegen: unsupported reg->stack move size {other}");
                        }
                    }
                }
                (Location::Reg(src), Location::StackOffset(dst, extra)) => {
                    let offset = self.stack_offset(dst).saturating_add(extra);
                    match mov.size {
                        1 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strb {}, [sp, #{}]", src, offset));
                        }
                        2 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strh {}, [sp, #{}]", src, offset));
                        }
                        4 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("str {}, [sp, #{}]", src, offset));
                        }
                        8 => {
                            self.emit_line(&format!(
                                "str {}, [sp, #{}]",
                                Self::reg_name(src),
                                offset
                            ));
                        }
                        other => {
                            panic!("backend codegen: unsupported reg->stack move size {other}");
                        }
                    }
                }
                (Location::Stack(src), Location::Reg(dst)) => {
                    let offset = self.stack_offset(src);
                    match mov.size {
                        1 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldrb {}, [sp, #{}]", dst, offset));
                        }
                        2 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldrh {}, [sp, #{}]", dst, offset));
                        }
                        4 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldr {}, [sp, #{}]", dst, offset));
                        }
                        8 => {
                            self.emit_line(&format!(
                                "ldr {}, [sp, #{}]",
                                Self::reg_name(dst),
                                offset
                            ));
                        }
                        other => {
                            panic!("backend codegen: unsupported stack->reg move size {other}");
                        }
                    }
                }
                (Location::StackOffset(src, extra), Location::Reg(dst)) => {
                    let offset = self.stack_offset(src).saturating_add(extra);
                    match mov.size {
                        1 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldrb {}, [sp, #{}]", dst, offset));
                        }
                        2 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldrh {}, [sp, #{}]", dst, offset));
                        }
                        4 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldr {}, [sp, #{}]", dst, offset));
                        }
                        8 => {
                            self.emit_line(&format!(
                                "ldr {}, [sp, #{}]",
                                Self::reg_name(dst),
                                offset
                            ));
                        }
                        other => {
                            panic!("backend codegen: unsupported stack->reg move size {other}");
                        }
                    }
                }
                (Location::Stack(src), Location::Stack(dst)) => {
                    let src_offset = self.stack_offset(src);
                    let dst_offset = self.stack_offset(dst);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", dst_offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", dst_offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", dst_offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                        }
                        other => {
                            self.emit_line(&format!("add x9, sp, #{}", src_offset));
                            self.copy_ptr_to_stack("x9", dst_offset, other);
                        }
                    }
                }
                (Location::Stack(src), Location::StackOffset(dst, extra)) => {
                    let src_offset = self.stack_offset(src);
                    let dst_offset = self.stack_offset(dst).saturating_add(extra);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", dst_offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", dst_offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", dst_offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                        }
                        other => {
                            self.emit_line(&format!("add x9, sp, #{}", src_offset));
                            self.copy_ptr_to_stack("x9", dst_offset, other);
                        }
                    }
                }
                (Location::StackOffset(src, extra), Location::Stack(dst)) => {
                    let src_offset = self.stack_offset(src).saturating_add(extra);
                    let dst_offset = self.stack_offset(dst);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", dst_offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", dst_offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", dst_offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                        }
                        other => {
                            self.emit_line(&format!("add x9, sp, #{}", src_offset));
                            self.copy_ptr_to_stack("x9", dst_offset, other);
                        }
                    }
                }
                (Location::StackOffset(src, src_extra), Location::StackOffset(dst, dst_extra)) => {
                    let src_offset = self.stack_offset(src).saturating_add(src_extra);
                    let dst_offset = self.stack_offset(dst).saturating_add(dst_extra);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", dst_offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", dst_offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", dst_offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                        }
                        other => {
                            self.emit_line(&format!("add x9, sp, #{}", src_offset));
                            self.copy_ptr_to_stack("x9", dst_offset, other);
                        }
                    }
                }
                (Location::Reg(src), Location::OutgoingArg(offset)) => {
                    let offset = self.layout.outgoing_offset(offset);
                    match mov.size {
                        1 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strb {}, [sp, #{}]", src, offset));
                        }
                        2 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("strh {}, [sp, #{}]", src, offset));
                        }
                        4 => {
                            let src = Self::w_reg(Self::reg_name(src));
                            self.emit_line(&format!("str {}, [sp, #{}]", src, offset));
                        }
                        8 => {
                            self.emit_line(&format!(
                                "str {}, [sp, #{}]",
                                Self::reg_name(src),
                                offset
                            ));
                        }
                        other => {
                            panic!("backend codegen: unsupported reg->outgoing move size {other}");
                        }
                    }
                }
                (Location::Stack(src), Location::OutgoingArg(offset)) => {
                    let src_offset = self.stack_offset(src);
                    let dst_offset = self.layout.outgoing_offset(offset);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", dst_offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", dst_offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", dst_offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                        }
                        other => {
                            self.emit_line(&format!("add x9, sp, #{}", src_offset));
                            self.copy_ptr_to_stack("x9", dst_offset, other);
                        }
                    }
                }
                (Location::StackOffset(src, extra), Location::OutgoingArg(offset)) => {
                    let src_offset = self.stack_offset(src).saturating_add(extra);
                    let dst_offset = self.layout.outgoing_offset(offset);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", dst_offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", dst_offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", dst_offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                        }
                        other => {
                            self.emit_line(&format!("add x9, sp, #{}", src_offset));
                            self.copy_ptr_to_stack("x9", dst_offset, other);
                        }
                    }
                }
                (Location::IncomingArg(offset), Location::Reg(dst)) => {
                    let src_offset = self.layout.incoming_offset(offset);
                    match mov.size {
                        1 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldrb {}, [sp, #{}]", dst, src_offset));
                        }
                        2 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldrh {}, [sp, #{}]", dst, src_offset));
                        }
                        4 => {
                            let dst = Self::w_reg(Self::reg_name(dst));
                            self.emit_line(&format!("ldr {}, [sp, #{}]", dst, src_offset));
                        }
                        8 => {
                            self.emit_line(&format!(
                                "ldr {}, [sp, #{}]",
                                Self::reg_name(dst),
                                src_offset
                            ));
                        }
                        other => {
                            panic!("backend codegen: unsupported incoming->reg move size {other}");
                        }
                    }
                }
                (Location::IncomingArg(offset), Location::Stack(dst)) => {
                    let src_offset = self.layout.incoming_offset(offset);
                    let dst_offset = self.stack_offset(dst);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", dst_offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", dst_offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", dst_offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                        }
                        other => {
                            self.emit_line(&format!("add x9, sp, #{}", src_offset));
                            self.copy_ptr_to_stack("x9", dst_offset, other);
                        }
                    }
                }
                (Location::IncomingArg(offset), Location::StackOffset(dst, extra)) => {
                    let src_offset = self.layout.incoming_offset(offset);
                    let dst_offset = self.stack_offset(dst).saturating_add(extra);
                    match mov.size {
                        1 => {
                            self.emit_line(&format!("ldrb w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strb w9, [sp, #{}]", dst_offset));
                        }
                        2 => {
                            self.emit_line(&format!("ldrh w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("strh w9, [sp, #{}]", dst_offset));
                        }
                        4 => {
                            self.emit_line(&format!("ldr w9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str w9, [sp, #{}]", dst_offset));
                        }
                        8 => {
                            self.emit_line(&format!("ldr x9, [sp, #{}]", src_offset));
                            self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                        }
                        other => {
                            self.emit_line(&format!("add x9, sp, #{}", src_offset));
                            self.copy_ptr_to_stack("x9", dst_offset, other);
                        }
                    }
                }
                (Location::StackAddr(src), Location::Reg(dst)) => {
                    let offset = self.stack_offset(src);
                    if offset == 0 {
                        self.emit_line(&format!("add {}, sp, #0", Self::reg_name(dst)));
                    } else {
                        self.emit_add_imm(Self::reg_name(dst), "sp", offset);
                    }
                }
                (Location::StackAddr(src), Location::Stack(dst)) => {
                    let src_offset = self.stack_offset(src);
                    let dst_offset = self.stack_offset(dst);
                    self.emit_line(&format!("add x9, sp, #{}", src_offset));
                    self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                }
                (Location::StackAddr(src), Location::StackOffset(dst, extra)) => {
                    let src_offset = self.stack_offset(src);
                    let dst_offset = self.stack_offset(dst).saturating_add(extra);
                    self.emit_line(&format!("add x9, sp, #{}", src_offset));
                    self.emit_line(&format!("str x9, [sp, #{}]", dst_offset));
                }
                other => {
                    panic!("backend codegen: unsupported move {:?}", other);
                }
            }
        }
    }
}
