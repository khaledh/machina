//! Signature-help source synthesis for incomplete call expressions.
//!
//! While a user is typing `foo(` or `foo(a,`, the buffer can be transiently
//! unparsable. We synthesize a few minimal probe variants so analysis can still
//! recover a call site and surface signature help.

use crate::core::diag::Position;

pub(crate) fn synthesize_call_signature_sources(source: &str, cursor: Position) -> Vec<String> {
    let Some(cursor_offset) = offset_for_position(source, cursor) else {
        return Vec::new();
    };
    if cursor_offset == 0 || cursor_offset > source.len() {
        return Vec::new();
    }
    let insert = if let Some(offset) = call_arg_insert_offset(source, cursor_offset) {
        ProbeInsert::new(offset, "0")
    } else if let Some(offset) = call_next_arg_insert_offset(source, cursor_offset) {
        ProbeInsert::new(offset, ", 0")
    } else {
        return Vec::new();
    };
    let offset = insert.offset;

    let mut out = Vec::with_capacity(3);
    out.push(inject_probe_at(source, offset, insert.probe));

    let has_existing_close_paren =
        next_non_whitespace_byte(source, offset).is_some_and(|(_, byte)| byte == b')');
    if !has_existing_close_paren {
        out.push(inject_probe_at(
            source,
            offset,
            &format!("{})", insert.probe),
        ));
    }

    if has_existing_close_paren {
        if let Some((close_idx, _)) = next_non_whitespace_byte(source, offset)
            && should_terminate_call_probe(source, close_idx + 1)
        {
            let with_arg = inject_probe_at(source, offset, insert.probe);
            let semicolon_offset = close_idx + insert.probe_len + 1;
            out.push(inject_probe_at(&with_arg, semicolon_offset, ";"));
        }
    } else if should_terminate_call_probe(source, offset) {
        out.push(inject_probe_at(
            source,
            offset,
            &format!("{});", insert.probe),
        ));
    }
    out
}

struct ProbeInsert<'a> {
    offset: usize,
    probe: &'a str,
    probe_len: usize,
}

impl<'a> ProbeInsert<'a> {
    fn new(offset: usize, probe: &'a str) -> Self {
        Self {
            offset,
            probe,
            probe_len: probe.len(),
        }
    }
}

fn inject_probe_at(source: &str, offset: usize, probe: &str) -> String {
    let mut synthesized = String::with_capacity(source.len() + probe.len());
    synthesized.push_str(&source[..offset]);
    synthesized.push_str(probe);
    synthesized.push_str(&source[offset..]);
    synthesized
}

fn call_arg_insert_offset(source: &str, cursor_offset: usize) -> Option<usize> {
    let bytes = source.as_bytes();

    let mut i = cursor_offset;
    while i > 0 && bytes[i - 1].is_ascii_whitespace() {
        i -= 1;
    }
    if i > 0 && matches!(bytes[i - 1], b'(' | b',') {
        return Some(cursor_offset);
    }

    let mut j = cursor_offset;
    while j < bytes.len() && bytes[j].is_ascii_whitespace() {
        j += 1;
    }
    if j < bytes.len() && matches!(bytes[j], b'(' | b',') {
        return Some(j + 1);
    }

    None
}

fn call_next_arg_insert_offset(source: &str, cursor_offset: usize) -> Option<usize> {
    let bytes = source.as_bytes();
    if cursor_offset > bytes.len() {
        return None;
    }

    // Cursor must be at (or before whitespace leading to) the call's closing
    // `)`, with a non-empty preceding argument.
    let mut right = cursor_offset;
    while right < bytes.len() && bytes[right].is_ascii_whitespace() {
        right += 1;
    }
    if right >= bytes.len() || bytes[right] != b')' {
        return None;
    }

    let mut left = cursor_offset;
    while left > 0 && bytes[left - 1].is_ascii_whitespace() {
        left -= 1;
    }
    if left == 0 {
        return None;
    }
    let prev = bytes[left - 1];
    if prev == b'(' || prev == b',' {
        return None;
    }
    Some(cursor_offset)
}

fn should_terminate_call_probe(source: &str, offset: usize) -> bool {
    let bytes = source.as_bytes();
    let mut i = offset;
    while i < bytes.len() {
        match bytes[i] {
            b' ' | b'\t' => i += 1,
            b'/' if i + 1 < bytes.len() && bytes[i + 1] == b'/' => return true,
            b'\r' | b'\n' | b'}' => return true,
            b';' => return false,
            _ => return false,
        }
    }
    true
}

fn next_non_whitespace_byte(source: &str, offset: usize) -> Option<(usize, u8)> {
    let bytes = source.as_bytes();
    let mut i = offset;
    while i < bytes.len() && matches!(bytes[i], b' ' | b'\t' | b'\r' | b'\n') {
        i += 1;
    }
    (i < bytes.len()).then_some((i, bytes[i]))
}

pub(crate) fn offset_for_position(source: &str, pos: Position) -> Option<usize> {
    if pos.line == 0 || pos.column == 0 {
        return Some(0);
    }
    let mut line = 1usize;
    let mut col = 1usize;
    for (offset, ch) in source.char_indices() {
        if line == pos.line && col == pos.column {
            return Some(offset);
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line == pos.line && col == pos.column).then_some(source.len())
}
