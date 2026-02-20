//! Source/text probing helpers for completion classification and synthesis.

use crate::core::diag::{Position, Span};

pub(super) fn synthesize_member_completion_source(
    source: &str,
    cursor: Position,
) -> Option<String> {
    let offset = offset_for_position(source, cursor)?;
    if offset == 0 || offset > source.len() {
        return None;
    }

    let bytes = source.as_bytes();
    let mut dot_probe = offset;
    while dot_probe > 0 && bytes[dot_probe - 1].is_ascii_whitespace() {
        dot_probe -= 1;
    }
    if dot_probe == 0 || bytes[dot_probe - 1] != b'.' {
        return None;
    }

    let mut synthesized = String::with_capacity(source.len() + 16);
    synthesized.push_str(&source[..offset]);
    synthesized.push_str("__mc_completion");
    if should_terminate_member_probe(source, offset) {
        synthesized.push(';');
    }
    synthesized.push_str(&source[offset..]);
    Some(synthesized)
}

pub(super) fn is_ident_byte(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphanumeric()
}

pub(super) fn offset_for_position(source: &str, pos: Position) -> Option<usize> {
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

pub(super) fn position_for_offset(source: &str, target_offset: usize) -> Option<Position> {
    if target_offset > source.len() {
        return None;
    }
    let mut line = 1usize;
    let mut column = 1usize;
    for (offset, ch) in source.char_indices() {
        if offset == target_offset {
            return Some(Position {
                offset: target_offset,
                line,
                column,
            });
        }
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    if target_offset == source.len() {
        return Some(Position {
            offset: target_offset,
            line,
            column,
        });
    }
    None
}

pub(super) fn single_char_span(source: &str, offset: usize) -> Option<Span> {
    let start = position_for_offset(source, offset)?;
    let mut next_offset = source.len();
    for (idx, _) in source[offset..].char_indices().skip(1) {
        next_offset = offset + idx;
        break;
    }
    if next_offset == source.len() && offset < source.len() {
        next_offset = source.len();
    }
    let end = position_for_offset(source, next_offset)?;
    Some(Span { start, end })
}

fn should_terminate_member_probe(source: &str, offset: usize) -> bool {
    let mut i = offset;
    let bytes = source.as_bytes();
    while i < bytes.len() {
        match bytes[i] {
            b' ' | b'\t' => {
                i += 1;
            }
            b'/' if i + 1 < bytes.len() && bytes[i + 1] == b'/' => {
                return true;
            }
            b'\r' | b'\n' | b'}' => return true,
            b';' => return false,
            _ => return false,
        }
    }
    true
}
