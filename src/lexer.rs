use std::iter::Peekable;
use std::num::ParseIntError;
use std::str::Chars;
use thiserror::Error;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TokenKind {
    Ident(String),
    IntLit(u32),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Arrow,
    Plus,
    Minus,
    Star,
    Slash,
    Comma,
    Colon,
    Semicolon,
    Equals,
    EqEq,
    NotEq,
    LessThan,
    GreaterThan,
    LessThanEq,
    GreaterThanEq,
    Eof,
}

#[derive(Debug, Error)]
pub enum LexError {
    #[error("Unexpected character: {0}")]
    UnexpectedCharacter(char),

    #[error("Invalid integer: {0}")]
    InvalidInteger(ParseIntError),
}

pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source: source.chars().peekable(),
            pos: 0,
        }
    }

    fn advance(&mut self) {
        self.source.next();
        self.pos += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(&ch) = self.source.peek()
            && ch.is_whitespace()
        {
            self.advance();
        }
    }

    pub fn next_token(&mut self) -> Result<TokenKind, LexError> {
        self.skip_whitespace();
        match self.source.peek() {
            Some(&ch) if ch.is_alphabetic() => {
                let mut ident = String::new();
                while let Some(&ch) = self.source.peek()
                    && ch.is_alphanumeric()
                {
                    ident.push(ch);
                    self.advance();
                }
                Ok(TokenKind::Ident(ident))
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
                    .parse::<u32>()
                    .map_err(|e| LexError::InvalidInteger(e))?;
                Ok(TokenKind::IntLit(value))
            }
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
            Some(&',') => {
                self.advance();
                Ok(TokenKind::Comma)
            }
            Some(&':') => {
                self.advance();
                Ok(TokenKind::Colon)
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
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::EqEq)
                } else {
                    Ok(TokenKind::Equals)
                }
            }
            Some(&'!') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    Ok(TokenKind::NotEq)
                } else {
                    Err(LexError::UnexpectedCharacter('!'))
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
            Some(&ch) => Err(LexError::UnexpectedCharacter(ch)),
            None => Ok(TokenKind::Eof),
        }
    }

    pub fn tokens(self) -> impl Iterator<Item = Result<TokenKind, LexError>> {
        self
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<TokenKind, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(TokenKind::Eof) => None,
            Ok(token) => Some(Ok(token)),
            Err(error) => Some(Err(error)),
        }
    }
}
