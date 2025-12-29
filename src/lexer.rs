use crate::diag::{Position, Span};
use enum_display::EnumDisplay;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::num::ParseIntError;
use std::str::Chars;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, EnumDisplay)]
pub enum TokenKind {
    #[display("Ident({0})")]
    Ident(String),
    #[display("IntLit({0})")]
    IntLit(u64),
    #[display("CharLit({0})")]
    CharLit(char),
    #[display("StringLit({0})")]
    StringLit(String),
    #[display("[")]
    LBracket,
    #[display("]")]
    RBracket,
    #[display("(")]
    LParen,
    #[display(")")]
    RParen,
    #[display("{{")]
    LBrace,
    #[display("}}")]
    RBrace,
    #[display("->")]
    Arrow,
    #[display("=>")]
    FatArrow,
    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[display("*")]
    Star,
    #[display("/")]
    Slash,
    #[display(",")]
    Comma,
    #[display(".")]
    Dot,
    #[display("..")]
    DotDot,
    #[display(":")]
    Colon,
    #[display("::")]
    DoubleColon,
    #[display(";")]
    Semicolon,
    #[display("|")]
    Pipe,
    #[display("_")]
    Underscore,
    #[display("=")]
    Equals,
    #[display("==")]
    EqEq,
    #[display("!=")]
    NotEq,
    #[display("<")]
    LessThan,
    #[display(">")]
    GreaterThan,
    #[display("<=")]
    LessThanEq,
    #[display(">=")]
    GreaterThanEq,
    #[display("EOF")]
    Eof,
}

#[derive(Debug, Error)]
pub enum LexError {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char, Span),

    #[error("Invalid integer: {0}")]
    InvalidInteger(ParseIntError, Span),

    #[error("Invalid escape sequence: {0}")]
    InvalidEscapeSequence(String, Span),

    #[error("Unterminated string literal")]
    UnterminatedString(Span),
}

impl LexError {
    pub fn span(&self) -> Span {
        match self {
            LexError::UnexpectedCharacter(_, span) => *span,
            LexError::InvalidInteger(_, span) => *span,
            LexError::InvalidEscapeSequence(_, span) => *span,
            LexError::UnterminatedString(span) => *span,
        }
    }
}

pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    pos: Position,
    at_eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source: source.chars().peekable(),
            pos: Position {
                offset: 0,
                line: 1,
                column: 1,
            },
            at_eof: false,
        }
    }

    fn advance(&mut self) {
        match self.source.next() {
            Some(ch) => {
                self.pos.offset += 1;
                if ch == '\n' {
                    self.pos.line += 1;
                    self.pos.column = 1;
                } else {
                    self.pos.column += 1;
                }
            }
            None => {
                self.at_eof = true;
            }
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            while let Some(&ch) = self.source.peek()
                && ch.is_whitespace()
            {
                self.advance();
            }

            if self.peek_line_comment() {
                // Consume '//' and skip until the end of the line.
                self.advance();
                self.advance();
                while let Some(&ch) = self.source.peek()
                    && ch != '\n'
                {
                    self.advance();
                }
                if matches!(self.source.peek(), Some(&'\n')) {
                    self.advance();
                }
                continue;
            }

            break;
        }
    }

    fn peek_line_comment(&mut self) -> bool {
        let mut iter = self.source.clone();
        matches!((iter.next(), iter.next()), (Some('/'), Some('/')))
    }

    fn lex_char_lit(&mut self, start: Position) -> Result<TokenKind, LexError> {
        // Accept exactly one Unicode scalar value after escapes.
        // Escapes: \n, \r, \t, \\, \', \0, \xNN.
        // Reject empty ' or multi‑char.
        let next_char = |lexer: &mut Lexer<'a>| -> Result<char, LexError> {
            match lexer.source.peek().copied() {
                Some(ch) => {
                    lexer.advance();
                    Ok(ch)
                }
                None => Err(LexError::UnexpectedCharacter(
                    '\'',
                    Span::new(start, lexer.pos),
                )),
            }
        };

        self.advance(); // consume opening quote
        let ch = next_char(self)?;
        if ch == '\'' {
            return Err(LexError::InvalidEscapeSequence(
                "empty char literal".to_string(),
                Span::new(start, self.pos),
            ));
        }

        let value = if ch == '\\' {
            self.parse_escape(start)?
        } else {
            ch
        };

        // expect closing quote
        match self.source.peek() {
            Some('\'') => self.advance(),
            Some(&ch) => {
                return Err(LexError::UnexpectedCharacter(
                    ch,
                    Span::new(start, self.pos),
                ));
            }
            None => {
                return Err(LexError::UnexpectedCharacter(
                    '\'',
                    Span::new(start, self.pos),
                ));
            }
        }

        Ok(TokenKind::CharLit(value))
    }

    fn lex_string_lit(&mut self, start: Position) -> Result<TokenKind, LexError> {
        self.advance(); // consume opening quote
        let mut buf = String::new();

        loop {
            let Some(&ch) = self.source.peek() else {
                return Err(LexError::UnterminatedString(Span::new(start, self.pos)));
            };

            self.advance();

            match ch {
                '"' => break,
                '\\' => {
                    let unescaped = self.parse_escape(start)?;
                    buf.push(unescaped);
                }
                _ => buf.push(ch),
            }
        }

        Ok(TokenKind::StringLit(buf))
    }

    fn parse_escape(&mut self, start: Position) -> Result<char, LexError> {
        let esc = match self.source.peek().copied() {
            Some(c) => c,
            None => return Err(LexError::UnterminatedString(Span::new(start, self.pos))),
        };
        self.advance();

        let ch =
            match esc {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                '0' => '\0',
                'x' => {
                    let h1 =
                        self.source.peek().copied().ok_or_else(|| {
                            LexError::UnterminatedString(Span::new(start, self.pos))
                        })?;
                    self.advance();
                    let h2 =
                        self.source.peek().copied().ok_or_else(|| {
                            LexError::UnterminatedString(Span::new(start, self.pos))
                        })?;
                    self.advance();
                    let hex = format!("{}{}", h1, h2);
                    let byte = u8::from_str_radix(&hex, 16).map_err(|_| {
                        LexError::InvalidEscapeSequence(hex.clone(), Span::new(start, self.pos))
                    })?;
                    char::from(byte)
                }
                _ => {
                    return Err(LexError::InvalidEscapeSequence(
                        format!("\\{}", esc),
                        Span::new(start, self.pos),
                    ));
                }
            };

        Ok(ch)
    }

    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();

        let start = Position {
            offset: self.pos.offset,
            line: self.pos.line,
            column: self.pos.column,
        };
        let kind = match self.source.peek() {
            Some(&ch) if ch.is_alphabetic() || ch == '_' => {
                let mut ident = String::new();
                while let Some(&ch) = self.source.peek()
                    && (ch.is_alphanumeric() || ch == '_')
                {
                    ident.push(ch);
                    self.advance();
                }
                if ident == "_" {
                    Ok(TokenKind::Underscore)
                } else {
                    Ok(TokenKind::Ident(ident))
                }
            }
            Some(&ch) if ch.is_ascii_digit() => {
                let mut num_str = String::new();
                while let Some(&ch) = self.source.peek()
                    && ch.is_ascii_digit()
                {
                    num_str.push(ch);
                    self.advance();
                }
                let value = num_str
                    .parse::<u64>()
                    .map_err(|e| LexError::InvalidInteger(e, Span::new(start, self.pos)))?;
                Ok(TokenKind::IntLit(value))
            }
            Some('\'') => self.lex_char_lit(start),
            Some('"') => self.lex_string_lit(start),
            Some(&'-') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'>')) {
                    self.advance();
                    Ok(TokenKind::Arrow)
                } else {
                    Ok(TokenKind::Minus)
                }
            }
            Some(&'+') => {
                self.advance();
                Ok(TokenKind::Plus)
            }
            Some(&'*') => {
                self.advance();
                Ok(TokenKind::Star)
            }
            Some(&'/') => {
                self.advance();
                Ok(TokenKind::Slash)
            }
            Some(&'(') => {
                self.advance();
                Ok(TokenKind::LParen)
            }
            Some(&')') => {
                self.advance();
                Ok(TokenKind::RParen)
            }
            Some(&'[') => {
                self.advance();
                Ok(TokenKind::LBracket)
            }
            Some(&']') => {
                self.advance();
                Ok(TokenKind::RBracket)
            }
            Some(&'|') => {
                self.advance();
                Ok(TokenKind::Pipe)
            }
            Some(&'_') => {
                self.advance();
                Ok(TokenKind::Underscore)
            }
            Some(&',') => {
                self.advance();
                Ok(TokenKind::Comma)
            }
            Some(&'.') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'.')) {
                    self.advance();
                    Ok(TokenKind::DotDot)
                } else {
                    Ok(TokenKind::Dot)
                }
            }
            Some(&':') => {
                self.advance();
                if matches!(self.source.peek(), Some(&':')) {
                    self.advance();
                    Ok(TokenKind::DoubleColon)
                } else {
                    Ok(TokenKind::Colon)
                }
            }
            Some(&';') => {
                self.advance();
                Ok(TokenKind::Semicolon)
            }
            Some(&'{') => {
                self.advance();
                Ok(TokenKind::LBrace)
            }
            Some(&'}') => {
                self.advance();
                Ok(TokenKind::RBrace)
            }
            Some(&'=') => {
                self.advance();
                match self.source.peek() {
                    Some(&'=') => {
                        self.advance();
                        Ok(TokenKind::EqEq)
                    }
                    Some(&'>') => {
                        self.advance();
                        Ok(TokenKind::FatArrow)
                    }
                    _ => Ok(TokenKind::Equals),
                }
            }
            Some(&'!') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::NotEq)
                } else {
                    Err(LexError::UnexpectedCharacter(
                        '!',
                        Span::new(start, self.pos),
                    ))
                }
            }
            Some(&'<') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::LessThanEq)
                } else {
                    Ok(TokenKind::LessThan)
                }
            }
            Some(&'>') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::GreaterThanEq)
                } else {
                    Ok(TokenKind::GreaterThan)
                }
            }
            Some(&ch) => Err(LexError::UnexpectedCharacter(
                ch,
                Span::new(start, self.pos),
            )),
            None => {
                self.at_eof = true;
                Ok(TokenKind::Eof)
            }
        }?;
        let end = Position {
            offset: self.pos.offset,
            line: self.pos.line,
            column: self.pos.column,
        };
        Ok(Token {
            kind,
            span: Span::new(start, end),
        })
    }

    pub fn tokenize(self) -> impl Iterator<Item = Result<Token, LexError>> {
        self
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.at_eof {
            return None;
        }
        match self.next_token() {
            Ok(token) => Some(Ok(token)),
            Err(error) => Some(Err(error)),
        }
    }
}

#[cfg(test)]
#[path = "tests/t_lexer.rs"]
mod tests;
