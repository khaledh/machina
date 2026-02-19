use crate::core::diag::{Position, Span, SpannedError};
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
    // Identifiers
    #[display("Ident({0})")]
    Ident(String),

    // Literals
    #[display("IntLit({0})")]
    IntLit(u64),
    #[display("BoolLit({0})")]
    BoolLit(bool),
    #[display("CharLit({0})")]
    CharLit(char),
    #[display("StringLit({0})")]
    StringLit(String),

    // Keywords
    #[display("fn")]
    KwFn,
    #[display("type")]
    KwType,
    #[display("trait")]
    KwTrait,
    #[display("protocol")]
    KwProtocol,
    #[display("typestate")]
    KwTypestate,
    #[display("range")]
    KwRange,
    #[display("bounds")]
    KwBounds,
    #[display("nonzero")]
    KwNonzero,
    #[display("let")]
    KwLet,
    #[display("var")]
    KwVar,
    #[display("if")]
    KwIf,
    #[display("else")]
    KwElse,
    #[display("match")]
    KwMatch,
    #[display("while")]
    KwWhile,
    #[display("for")]
    KwFor,
    #[display("break")]
    KwBreak,
    #[display("continue")]
    KwContinue,
    #[display("return")]
    KwReturn,
    #[display("in")]
    KwIn,
    #[display("inout")]
    KwInOut,
    #[display("out")]
    KwOut,
    #[display("sink")]
    KwSink,
    #[display("move")]
    KwMove,
    #[display("self")]
    KwSelf,
    #[display("prop")]
    KwProp,
    #[display("get")]
    KwGet,
    #[display("set")]
    KwSet,
    #[display("map")]
    KwMap,
    #[display("requires")]
    KwRequires,
    #[display("role")]
    KwRole,
    #[display("on")]
    KwOn,
    #[display("emit")]
    KwEmit,
    #[display("reply")]
    KwReply,

    // Brackets
    #[display("[")]
    LBracket,
    #[display("]")]
    RBracket,

    // Parentheses
    #[display("(")]
    LParen,
    #[display(")")]
    RParen,

    // Braces
    #[display("{{")]
    LBrace,
    #[display("}}")]
    RBrace,

    // Punctuation
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
    #[display("@")]
    At,
    #[display("?")]
    Question,
    #[display("|")]
    Pipe,
    #[display("_")]
    Underscore,

    // Arrows
    #[display("->")]
    Arrow,
    #[display("=>")]
    FatArrow,

    // Arithmetic operators
    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
    #[display("*")]
    Star,
    #[display("/")]
    Slash,
    #[display("%")]
    Percent,

    // Bitwise operators
    #[display("&")]
    Ampersand,
    #[display("^")]
    Caret,
    #[display("~")]
    Tilde,
    #[display("<<")]
    ShiftLeft,
    #[display(">>")]
    ShiftRight,

    // Assignment operator
    #[display("=")]
    Equals,
    #[display("+=")]
    PlusEquals,
    #[display("-=")]
    MinusEquals,
    #[display("*=")]
    StarEquals,
    #[display("/=")]
    SlashEquals,
    #[display("%=")]
    PercentEquals,
    #[display("&=")]
    AmpersandEquals,
    #[display("|=")]
    PipeEquals,
    #[display("^=")]
    CaretEquals,
    #[display("<<=")]
    ShiftLeftEquals,
    #[display(">>=")]
    ShiftRightEquals,

    // Comparison operators
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

    // Logical operators
    #[display("&&")]
    LogicalAnd,
    #[display("||")]
    LogicalOr,
    #[display("!")]
    LogicalNot,

    // End of file
    #[display("EOF")]
    Eof,
}

#[derive(Debug, Error)]
pub enum LexErrorKind {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),

    #[error("Invalid integer: {0}")]
    InvalidInteger(ParseIntError),

    #[error("Invalid escape sequence: {0}")]
    InvalidEscapeSequence(String),

    #[error("Unterminated string literal")]
    UnterminatedString,
}

pub type LexError = SpannedError<LexErrorKind>;
pub use LexErrorKind as LEK;

impl LexErrorKind {
    pub fn at(self, span: Span) -> LexError {
        LexError::new(self, span)
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

    fn is_digit_for_base(ch: char, base: u32) -> bool {
        match base {
            2 => matches!(ch, '0' | '1'),
            8 => matches!(ch, '0'..='7'),
            16 => ch.is_ascii_hexdigit(),
            _ => ch.is_ascii_digit(),
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
                None => Err(LEK::UnexpectedCharacter('\'').at(Span::new(start, lexer.pos))),
            }
        };

        self.advance(); // consume opening quote
        let ch = next_char(self)?;
        if ch == '\'' {
            return Err(LEK::InvalidEscapeSequence("empty char literal".to_string())
                .at(Span::new(start, self.pos)));
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
                return Err(LEK::UnexpectedCharacter(ch).at(Span::new(start, self.pos)));
            }
            None => {
                return Err(LEK::UnexpectedCharacter('\'').at(Span::new(start, self.pos)));
            }
        }

        Ok(TokenKind::CharLit(value))
    }

    fn lex_string_lit(&mut self, start: Position) -> Result<TokenKind, LexError> {
        self.advance(); // consume opening quote
        let mut buf = String::new();

        loop {
            let Some(&ch) = self.source.peek() else {
                return Err(LEK::UnterminatedString.at(Span::new(start, self.pos)));
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
            None => return Err(LEK::UnterminatedString.at(Span::new(start, self.pos))),
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
                            LEK::UnterminatedString.at(Span::new(start, self.pos))
                        })?;
                    self.advance();
                    let h2 =
                        self.source.peek().copied().ok_or_else(|| {
                            LEK::UnterminatedString.at(Span::new(start, self.pos))
                        })?;
                    self.advance();
                    let hex = format!("{}{}", h1, h2);
                    let byte = u8::from_str_radix(&hex, 16).map_err(|_| {
                        LEK::InvalidEscapeSequence(hex.clone()).at(Span::new(start, self.pos))
                    })?;
                    char::from(byte)
                }
                _ => {
                    return Err(LEK::InvalidEscapeSequence(format!("\\{}", esc))
                        .at(Span::new(start, self.pos)));
                }
            };

        Ok(ch)
    }

    fn consume_digits_with_separators(
        &mut self,
        base: u32,
        digits: &mut String,
        saw_digit: &mut bool,
    ) -> bool {
        // Collect digits while skipping '_' separators; returns true if the last char was '_'.
        let mut last_underscore = false;
        while let Some(&ch) = self.source.peek() {
            if Self::is_digit_for_base(ch, base) {
                digits.push(ch);
                self.advance();
                *saw_digit = true;
                last_underscore = false;
            } else if ch == '_' && *saw_digit {
                self.advance();
                last_underscore = true;
            } else {
                break;
            }
        }
        last_underscore
    }

    fn consume_invalid_digits(&mut self, digits: &mut String) {
        // Consume trailing garbage to surface a single InvalidInteger error.
        while let Some(&ch) = self.source.peek()
            && (ch.is_ascii_alphanumeric() || ch == '_')
        {
            digits.push(ch);
            self.advance();
        }
    }

    fn lex_int_literal(&mut self, start: Position) -> Result<TokenKind, LexError> {
        // Parse decimal or base-prefixed integer literals with '_' separators.
        //   Decimal: <decimal digits>
        //    Binary: 0b<binary digits> (e.g. 0b10101010)
        //     Octal: 0o<octal digits> (e.g. 0o474)
        //       Hex: 0x<hex digits> (e.g. 0x4f)
        let mut base = 10u32;
        let mut digits = String::new();

        if matches!(self.source.peek(), Some(&'0')) {
            digits.push('0');
            self.advance();

            if let Some(&next) = self.source.peek() {
                match next {
                    'b' | 'B' => {
                        base = 2;
                        self.advance();
                        digits.clear();
                    }
                    'o' | 'O' => {
                        base = 8;
                        self.advance();
                        digits.clear();
                    }
                    'x' | 'X' => {
                        base = 16;
                        self.advance();
                        digits.clear();
                    }
                    _ => {}
                }
            }
        }

        let mut saw_digit = !digits.is_empty();
        let last_underscore =
            self.consume_digits_with_separators(base, &mut digits, &mut saw_digit);

        if !saw_digit || last_underscore {
            // Disallow empty literals and trailing underscores.
            let err = u64::from_str_radix("", base).err().unwrap();
            return Err(LEK::InvalidInteger(err).at(Span::new(start, self.pos)));
        }

        if base != 10
            && matches!(self.source.peek(), Some(ch) if ch.is_ascii_alphanumeric() || *ch == '_')
        {
            // Reject invalid digits after base prefixes (e.g., 0b102).
            self.consume_invalid_digits(&mut digits);
            let err = u64::from_str_radix(&digits, base).err().unwrap();
            return Err(LEK::InvalidInteger(err).at(Span::new(start, self.pos)));
        }

        let value = if base == 10 {
            digits.parse::<u64>()
        } else {
            u64::from_str_radix(&digits, base)
        }
        .map_err(|e| LEK::InvalidInteger(e).at(Span::new(start, self.pos)))?;

        Ok(TokenKind::IntLit(value))
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
                    Ok(match ident.as_str() {
                        "fn" => TokenKind::KwFn,
                        "type" => TokenKind::KwType,
                        "trait" => TokenKind::KwTrait,
                        "protocol" => TokenKind::KwProtocol,
                        "typestate" => TokenKind::KwTypestate,
                        "range" => TokenKind::KwRange,
                        "bounds" => TokenKind::KwBounds,
                        "nonzero" => TokenKind::KwNonzero,
                        "let" => TokenKind::KwLet,
                        "var" => TokenKind::KwVar,
                        "if" => TokenKind::KwIf,
                        "else" => TokenKind::KwElse,
                        "match" => TokenKind::KwMatch,
                        "while" => TokenKind::KwWhile,
                        "for" => TokenKind::KwFor,
                        "break" => TokenKind::KwBreak,
                        "continue" => TokenKind::KwContinue,
                        "return" => TokenKind::KwReturn,
                        "in" => TokenKind::KwIn,
                        "inout" => TokenKind::KwInOut,
                        "out" => TokenKind::KwOut,
                        "sink" => TokenKind::KwSink,
                        "move" => TokenKind::KwMove,
                        "self" => TokenKind::KwSelf,
                        "prop" => TokenKind::KwProp,
                        "get" => TokenKind::KwGet,
                        "set" => TokenKind::KwSet,
                        "map" => TokenKind::KwMap,
                        "requires" => TokenKind::KwRequires,
                        "role" => TokenKind::KwRole,
                        "on" => TokenKind::KwOn,
                        "emit" => TokenKind::KwEmit,
                        "reply" => TokenKind::KwReply,
                        "true" => TokenKind::BoolLit(true),
                        "false" => TokenKind::BoolLit(false),
                        _ => TokenKind::Ident(ident),
                    })
                }
            }
            Some(&ch) if ch.is_ascii_digit() => self.lex_int_literal(start),
            Some('\'') => self.lex_char_lit(start),
            Some('"') => self.lex_string_lit(start),
            Some(&'-') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'>')) {
                    self.advance();
                    Ok(TokenKind::Arrow)
                } else if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::MinusEquals)
                } else {
                    Ok(TokenKind::Minus)
                }
            }
            Some(&'+') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::PlusEquals)
                } else {
                    Ok(TokenKind::Plus)
                }
            }
            Some(&'*') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::StarEquals)
                } else {
                    Ok(TokenKind::Star)
                }
            }
            Some(&'/') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::SlashEquals)
                } else {
                    Ok(TokenKind::Slash)
                }
            }
            Some(&'%') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::PercentEquals)
                } else {
                    Ok(TokenKind::Percent)
                }
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
                if matches!(self.source.peek(), Some(&'|')) {
                    self.advance();
                    Ok(TokenKind::LogicalOr)
                } else if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::PipeEquals)
                } else {
                    Ok(TokenKind::Pipe)
                }
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
            Some(&'@') => {
                self.advance();
                Ok(TokenKind::At)
            }
            Some(&'?') => {
                self.advance();
                Ok(TokenKind::Question)
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
                    Ok(TokenKind::LogicalNot)
                }
            }
            Some(&'<') => {
                self.advance();
                match self.source.peek() {
                    Some(&'<') => {
                        self.advance();
                        if matches!(self.source.peek(), Some(&'=')) {
                            self.advance();
                            Ok(TokenKind::ShiftLeftEquals)
                        } else {
                            Ok(TokenKind::ShiftLeft)
                        }
                    }
                    Some(&'=') => {
                        self.advance();
                        Ok(TokenKind::LessThanEq)
                    }
                    _ => Ok(TokenKind::LessThan),
                }
            }
            Some(&'>') => {
                self.advance();
                match self.source.peek() {
                    Some(&'>') => {
                        self.advance();
                        if matches!(self.source.peek(), Some(&'=')) {
                            self.advance();
                            Ok(TokenKind::ShiftRightEquals)
                        } else {
                            Ok(TokenKind::ShiftRight)
                        }
                    }
                    Some(&'=') => {
                        self.advance();
                        Ok(TokenKind::GreaterThanEq)
                    }
                    _ => Ok(TokenKind::GreaterThan),
                }
            }
            Some(&'&') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'&')) {
                    self.advance();
                    Ok(TokenKind::LogicalAnd)
                } else if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::AmpersandEquals)
                } else {
                    Ok(TokenKind::Ampersand)
                }
            }
            Some(&'^') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::CaretEquals)
                } else {
                    Ok(TokenKind::Caret)
                }
            }
            Some(&'~') => {
                self.advance();
                Ok(TokenKind::Tilde)
            }
            Some(&ch) => Err(LEK::UnexpectedCharacter(ch).at(Span::new(start, self.pos))),
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
#[path = "../tests/t_lexer.rs"]
mod tests;
