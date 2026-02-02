use std::fmt::Write;

use crate::ir::ir::{GlobalData, GlobalId};

use super::{Arm64Emitter, AsmSection};

impl Arm64Emitter {
    pub(super) fn ensure_text(&mut self) {
        if self.section != AsmSection::Text {
            if !self.output.is_empty() {
                let _ = writeln!(self.output);
            }
            let _ = writeln!(self.output, ".text");
            self.section = AsmSection::Text;
        }
    }

    pub(super) fn ensure_data(&mut self) {
        if self.section != AsmSection::Data {
            if !self.output.is_empty() {
                let _ = writeln!(self.output);
            }
            let _ = writeln!(self.output, ".data");
            self.section = AsmSection::Data;
        }
    }

    pub(super) fn global_label(id: GlobalId) -> String {
        format!("_g{}", id.0)
    }

    pub(super) fn emit_global_impl(&mut self, global: &GlobalData) {
        self.ensure_data();
        if !self.output.is_empty() {
            let _ = writeln!(self.output);
        }

        // Emit a stable label for the global payload.
        let label = Self::global_label(global.id);

        // Align the data so references can use natural alignment.
        let align = global.align.max(1);
        if align.is_power_of_two() {
            let align_log2 = align.trailing_zeros();
            self.emit_line(&format!(".p2align {}", align_log2));
        } else {
            self.emit_line(&format!(".balign {}", align));
        }

        let _ = writeln!(self.output, "{}:", label);

        // Emit ASCII strings when possible, otherwise fall back to raw bytes.
        if let Some(text) = format_bytes_as_ascii(&global.bytes) {
            if text.is_empty() {
                self.emit_line(".space 0");
            } else {
                self.emit_line(&format!(".ascii \"{}\"", text));
            }
            return;
        }

        if global.bytes.is_empty() {
            self.emit_line(".space 0");
            return;
        }

        let data = global
            .bytes
            .iter()
            .map(|b| b.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        self.emit_line(&format!(".byte {}", data));
    }
}

fn format_bytes_as_ascii(bytes: &[u8]) -> Option<String> {
    let text = std::str::from_utf8(bytes).ok()?;
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ' ' => out.push(' '),
            _ if ch.is_ascii_graphic() => out.push(ch),
            _ => return None,
        }
    }
    Some(out)
}
