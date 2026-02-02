use std::path::PathBuf;

use crate::lexer::LexError;
use crate::parse::ParseError;
use crate::resolve::ResolveError;
use crate::semck::SemCheckError;
use crate::ssa::lower::LowerToIrError as SsaLoweringError;
use crate::ssa::verify::VerifyIrError as SsaVerifyError;
use crate::typeck::TypeCheckError;
use std::fmt::{Display, Formatter, Result};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompileError {
    #[error(transparent)]
    Lex(#[from] LexError),

    #[error(transparent)]
    Parse(#[from] ParseError),

    #[error(transparent)]
    Resolve(#[from] ResolveError),

    #[error(transparent)]
    TypeCheck(#[from] TypeCheckError),

    #[error(transparent)]
    SemCheck(#[from] SemCheckError),

    #[error("IR lowering error")]
    LowerToIr(#[from] SsaLoweringError),

    #[error(transparent)]
    VerifyIr(#[from] SsaVerifyError),

    #[error("IO error: {0}")]
    Io(PathBuf, std::io::Error),
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
/// - Shows one line before and one line after the error span for context.
///
/// Example (single char):
/// ```text
/// (3:9) Unexpected token
/// │ 2 │ fn main() {
/// │ 3 │ let a = @;
/// │   │         ^
/// │ 4 │     let b = 42;
/// ```
/// Example (multi-line):
/// ```text
/// (12:9) Then and else branches have different types: i32 != bool
/// │ 11 │     if cond {
/// │ 12 │         42 / 2
/// │    │         ------
/// │ 13 │     } else {
/// │    │ ------------
/// │ 14 │        false
/// │    │ ------------
/// │ 15 │     }
/// ```
pub fn format_error(source: &str, span: Span, error: impl Display) -> String {
    let start_line = span.start.line.max(1);
    let end_line = span.end.line.max(start_line);
    let lines: Vec<&str> = source.lines().collect();

    // Include one line before and after for context
    let first_line = start_line.saturating_sub(1).max(1);
    let last_line = (end_line + 1).min(lines.len());

    let number_width = last_line.to_string().len();

    let mut out = String::new();
    out.push_str(&format!(
        "({}:{}) {}\n",
        span.start.line, span.start.column, error
    ));

    let single_line = start_line == end_line;

    for line_no in first_line..=last_line {
        let content = lines.get(line_no - 1).copied().unwrap_or("");
        out.push_str(&format!(
            "│ {:>number_width$} │ {}\n",
            line_no,
            content,
            number_width = number_width
        ));

        // Only show markers for lines within the actual error span
        if line_no < start_line || line_no > end_line {
            continue;
        }

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
