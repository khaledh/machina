//! Diagnostic-driven code action generation.
//!
//! This keeps quick-fix mapping separate from `AnalysisDb` orchestration so new
//! actions can be added without growing query plumbing code.

use crate::core::diag::Span;
use crate::services::analysis::diagnostics::{Diagnostic, DiagnosticValue};
use crate::services::analysis::results::{CodeAction, CodeActionKind, TextEdit};

pub(crate) fn code_actions_for_diagnostic_with_source(
    diag: &Diagnostic,
    source: Option<&str>,
) -> Vec<CodeAction> {
    match diag.code.as_str() {
        "MC-PARSE-SET-MISSING-COMMA" | "MC-PARSE-TUPLE-MISSING-COMMA" => vec![CodeAction {
            title: "Insert missing comma".to_string(),
            kind: CodeActionKind::QuickFix,
            diagnostic_code: diag.code.clone(),
            edits: vec![TextEdit {
                span: Span {
                    start: diag.span.end,
                    end: diag.span.end,
                },
                new_text: ",".to_string(),
            }],
        }],
        "MC-TYPECHECK-TryErrorNotInReturn" => {
            let missing = diag
                .metadata
                .get("missing")
                .and_then(|v| match v {
                    DiagnosticValue::StringList(items) => Some(items.join(" | ")),
                    _ => None,
                })
                .unwrap_or_else(|| "missing error variants".to_string());
            vec![CodeAction {
                title: format!("Add `{missing}` to function return error union"),
                kind: CodeActionKind::QuickFix,
                diagnostic_code: diag.code.clone(),
                edits: Vec::new(),
            }]
        }
        "MC-TYPECHECK-TryReturnTypeNotErrorUnion" => vec![CodeAction {
            title: "Change function return type to an error union (or remove `?`)".to_string(),
            kind: CodeActionKind::QuickFix,
            diagnostic_code: diag.code.clone(),
            edits: Vec::new(),
        }],
        "MC-SEMCK-NonExhaustiveUnionMatch" => {
            let missing = diag
                .metadata
                .get("missing")
                .and_then(|v| match v {
                    DiagnosticValue::StringList(items) => Some(items.clone()),
                    _ => None,
                })
                .unwrap_or_default();
            let missing_label = if missing.is_empty() {
                "missing union variants".to_string()
            } else {
                missing.join(" | ")
            };
            let edits = source
                .and_then(|src| build_union_match_arm_edits(diag, src, &missing))
                .unwrap_or_default();
            vec![CodeAction {
                title: format!("Add match arms for missing union variants: {missing_label}"),
                kind: CodeActionKind::QuickFix,
                diagnostic_code: diag.code.clone(),
                edits,
            }]
        }
        "MC-SEMCK-NonExhaustiveMatch" => {
            let edits = source
                .and_then(|src| build_wildcard_match_arm_edit(diag, src))
                .unwrap_or_default();
            vec![CodeAction {
                title: "Add wildcard match arm".to_string(),
                kind: CodeActionKind::QuickFix,
                diagnostic_code: diag.code.clone(),
                edits,
            }]
        }
        _ => Vec::new(),
    }
}

fn build_union_match_arm_edits(
    diag: &Diagnostic,
    source: &str,
    missing: &[String],
) -> Option<Vec<TextEdit>> {
    if missing.is_empty() {
        return None;
    }
    let insertion_offset = find_match_closing_brace_offset(source, diag.span)?;
    let close_indent = line_indent_before_offset(source, insertion_offset);
    let arm_indent = format!("{close_indent}    ");
    let mut new_text = String::new();
    if !source[..insertion_offset].ends_with('\n') {
        new_text.push('\n');
    }
    for (idx, ty) in missing.iter().enumerate() {
        new_text.push_str(&format!(
            "{arm_indent}v{idx}: {ty} => {{\n{arm_indent}    // TODO: handle {ty}\n{arm_indent}}},\n"
        ));
    }
    let insert_pos = offset_to_position(source, insertion_offset);
    Some(vec![TextEdit {
        span: Span {
            start: insert_pos,
            end: insert_pos,
        },
        new_text,
    }])
}

fn build_wildcard_match_arm_edit(diag: &Diagnostic, source: &str) -> Option<Vec<TextEdit>> {
    let insertion_offset = find_match_closing_brace_offset(source, diag.span)?;
    let close_indent = line_indent_before_offset(source, insertion_offset);
    let arm_indent = format!("{close_indent}    ");
    let mut new_text = String::new();
    if !source[..insertion_offset].ends_with('\n') {
        new_text.push('\n');
    }
    new_text.push_str(&format!(
        "{arm_indent}_ => {{\n{arm_indent}    // TODO: handle remaining cases\n{arm_indent}}},\n"
    ));
    let insert_pos = offset_to_position(source, insertion_offset);
    Some(vec![TextEdit {
        span: Span {
            start: insert_pos,
            end: insert_pos,
        },
        new_text,
    }])
}

fn find_match_closing_brace_offset(source: &str, span: Span) -> Option<usize> {
    let start = span_start_offset(source, span);
    let end = span_end_offset(source, span);
    if start >= end {
        return None;
    }

    let bytes = source.as_bytes();
    let mut open = None;
    if let Some(match_kw) = find_match_keyword_offset(source, start, end)
        && let Some(rel) = source[match_kw..].find('{')
    {
        open = Some(match_kw + rel);
    }
    if open.is_none() {
        for (idx, byte) in bytes.iter().enumerate().take(end).skip(start) {
            if *byte == b'{' {
                open = Some(idx);
                break;
            }
        }
    }
    let open = open?;
    let mut depth = 0usize;
    for (idx, byte) in bytes.iter().enumerate().skip(open) {
        match *byte {
            b'{' => depth += 1,
            b'}' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
    }
    None
}

fn find_match_keyword_offset(source: &str, start: usize, end: usize) -> Option<usize> {
    if start >= end || end > source.len() {
        return None;
    }
    let window = &source[start..end];
    let mut rel = 0usize;
    while let Some(found) = window[rel..].find("match") {
        let idx = rel + found;
        let abs = start + idx;
        let prev_ok = abs == 0
            || !source[..abs]
                .chars()
                .next_back()
                .is_some_and(|ch| ch == '_' || ch.is_ascii_alphanumeric());
        let next_abs = abs + "match".len();
        let next_ok = next_abs >= source.len()
            || !source[next_abs..]
                .chars()
                .next()
                .is_some_and(|ch| ch == '_' || ch.is_ascii_alphanumeric());
        if prev_ok && next_ok {
            return Some(abs);
        }
        rel = idx + "match".len();
    }
    None
}

fn span_start_offset(source: &str, span: Span) -> usize {
    if span.start.offset > 0 && span.start.offset <= source.len() {
        span.start.offset
    } else {
        position_to_offset(source, span.start.line, span.start.column)
    }
}

fn span_end_offset(source: &str, span: Span) -> usize {
    if span.end.offset > 0 && span.end.offset <= source.len() {
        span.end.offset
    } else {
        position_to_offset(source, span.end.line, span.end.column)
    }
}

fn position_to_offset(source: &str, line: usize, column: usize) -> usize {
    if line <= 1 {
        return column.saturating_sub(1).min(source.len());
    }
    let mut current_line = 1usize;
    let mut line_start = 0usize;
    for (idx, ch) in source.char_indices() {
        if ch == '\n' {
            current_line += 1;
            line_start = idx + 1;
            if current_line == line {
                break;
            }
        }
    }
    line_start
        .saturating_add(column.saturating_sub(1))
        .min(source.len())
}

fn line_indent_before_offset(source: &str, offset: usize) -> String {
    let line_start = source[..offset].rfind('\n').map(|idx| idx + 1).unwrap_or(0);
    source[line_start..offset]
        .chars()
        .take_while(|c| c.is_ascii_whitespace())
        .collect()
}

fn offset_to_position(source: &str, offset: usize) -> crate::core::diag::Position {
    let mut line = 1usize;
    let mut column = 1usize;
    for ch in source[..offset.min(source.len())].chars() {
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    crate::core::diag::Position {
        offset,
        line,
        column,
    }
}
