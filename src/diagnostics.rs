use crate::codegen::CodegenError;
use crate::lexer::LexError;
use crate::parser::ParseError;
use crate::resolver::ResolveError;
use crate::type_check::TypeCheckError;
use std::fmt::{Display, Formatter, Result};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompileError {
    #[error(transparent)]
    LexError(#[from] LexError),

    #[error(transparent)]
    ParserError(#[from] ParseError),

    #[error(transparent)]
    ResolveError(#[from] ResolveError),

    #[error(transparent)]
    TypeCheckError(#[from] TypeCheckError),

    #[error(transparent)]
    CodegenError(#[from] CodegenError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn merge_all(spans: Vec<Span>) -> Span {
        if spans.is_empty() {
            // Return a harmless 1:1 zero-length span instead of line 0.
            return Span::new(
                Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                Position {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
            );
        }
        // Assume spans are in source order; take start from first, end from last.
        let start = spans[0].start;
        let end = spans.last().unwrap().end;
        Span::new(start, end)
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: Position {
                offset: 0,
                line: 1,
                column: 1,
            },
            end: Position {
                offset: 0,
                line: 1,
                column: 1,
            },
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

fn build_marker(len: usize, single_line: bool) -> String {
    if single_line && len == 1 {
        "^".to_string()
    } else {
        "-".repeat(len.max(1))
    }
}

/// Formats an error message with a source snippet and marker lines highlighting
/// the span.
///
/// Current style (simplified):
/// - Single-line spans: caret (^) for a single char, dashes for multi-char.
/// - Multi-line spans: each covered line gets an underline of dashes aligned
///   to the span start/end columns (no corner or ribbon glyphs).
///
/// Example (single char):
/// ```text
/// (3:9) Unexpected token
/// │ 3 │ let a = @;
/// │   │         ^
/// ```
/// Example (multi-line):
/// ```text
/// (12:9) Then and else branches have different types: UInt32 != Bool
/// │ 12 │         42 / 2
/// │    │         ------
/// │ 13 │     } else {
/// │    │ ------------
/// │ 14 │        false
/// │    │ ------------
/// ```
pub fn format_error(source: &str, span: Span, error: impl Display) -> String {
    let start_line = span.start.line.max(1);
    let end_line = span.end.line.max(start_line);
    let lines: Vec<&str> = source.lines().collect();
    let number_width = end_line.to_string().len();

    let mut out = String::new();
    out.push_str(&format!(
        "({}:{}) {}\n",
        span.start.line, span.start.column, error
    ));

    let single_line = start_line == end_line;

    for line_no in start_line..=end_line {
        let content = lines.get(line_no - 1).copied().unwrap_or("");
        out.push_str(&format!(
            "│ {:>number_width$} │ {}\n",
            line_no,
            content,
            number_width = number_width
        ));

        let start_col = if line_no == span.start.line {
            span.start.column.max(1)
        } else {
            1
        };
        let end_col_excl = if line_no == span.end.line {
            span.end.column.max(start_col)
        } else {
            content.chars().count() + 1
        };

        if end_col_excl > start_col {
            let len = end_col_excl - start_col;
            let mut marker = String::with_capacity(start_col - 1 + len);
            marker.push_str(&" ".repeat(start_col - 1));
            marker.push_str(&build_marker(len, single_line));
            out.push_str(&format!(
                "│ {:>number_width$} │ {}\n",
                "",
                marker,
                number_width = number_width
            ));
        }
    }
    out
}
