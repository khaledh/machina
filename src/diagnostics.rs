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

    pub fn len(&self) -> usize {
        self.end.offset - self.start.offset
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

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

fn get_line(source: &str, line: usize) -> &str {
    source.lines().nth(line - 1).unwrap_or("")
}

/// Formats an error message with a marker line that points to the error location.
///
/// If the span is a single character, it will be marked with a caret.
/// ```
/// let a = @;
///         ^
/// ```
/// Otherwise, it will be marked with dashes.
/// ```
/// if 1 + 2 {
///    -----
/// ```
pub fn format_error(source: &str, span: Span, error: impl Display) -> String {
    let line = span.start.line;
    let column = span.start.column;
    let line_contents = get_line(source, line);

    let marker_line = if span.len() == 1 {
        " ".repeat(span.start.column - 1) + "^"
    } else {
        " ".repeat(span.start.column - 1) + &"-".repeat(span.len() - 1)
    };
    format!("({line}:{column}): {error}\n\n{line_contents}\n{marker_line}")
}
