use std::iter::Peekable;
use std::str::Chars;

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

    pub fn next_token(&mut self) -> TokenKind {
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
                TokenKind::Ident(ident)
            }
            Some(&ch) if ch.is_ascii_digit() => {
                let mut num_str = String::new();
                while let Some(&ch) = self.source.peek()
                    && ch.is_ascii_digit()
                {
                    num_str.push(ch);
                    self.advance();
                }
                let value = num_str.parse::<u32>().unwrap();
                TokenKind::IntLit(value)
            }
            Some(&'-') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'>')) {
                    self.advance();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            Some(&'+') => {
                self.advance();
                TokenKind::Plus
            }
            Some(&'*') => {
                self.advance();
                TokenKind::Star
            }
            Some(&'/') => {
                self.advance();
                TokenKind::Slash
            }
            Some(&'(') => {
                self.advance();
                TokenKind::LParen
            }
            Some(&')') => {
                self.advance();
                TokenKind::RParen
            }
            Some(&'{') => {
                self.advance();
                TokenKind::LBrace
            }
            Some(&'}') => {
                self.advance();
                TokenKind::RBrace
            }
            Some(&';') => {
                self.advance();
                TokenKind::Semicolon
            }
            Some(&'=') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    TokenKind::EqEq
                } else {
                    TokenKind::Equals
                }
            }
            Some(&'!') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    TokenKind::NotEq
                } else {
                    panic!("Unexpected character: {}", self.source.peek().unwrap());
                }
            }
            Some(&'<') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    TokenKind::LessThanEq
                } else {
                    TokenKind::LessThan
                }
            }
            Some(&'>') => {
                self.advance();
                if matches!(self.source.peek(), Some(&'=')) {
                    self.advance();
                    TokenKind::GreaterThanEq
                } else {
                    TokenKind::GreaterThan
                }
            }
            Some(&ch) => {
                panic!("Unexpected character: {ch}");
            }
            None => TokenKind::Eof,
        }
    }

    pub fn tokens(self) -> impl Iterator<Item = TokenKind> {
        self
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = TokenKind;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token == TokenKind::Eof {
            None
        } else {
            Some(token)
        }
    }
}
