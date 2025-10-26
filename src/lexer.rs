use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Ident(String),
    IntLit(u32),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Arrow,
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
            Some(&ch) if ch.is_digit(10) => {
                let mut num_str = String::new();
                while let Some(&ch) = self.source.peek()
                    && ch.is_digit(10)
                {
                    num_str.push(ch);
                    self.advance();
                }
                let value = num_str.parse::<u32>().unwrap();
                TokenKind::IntLit(value)
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
            Some(&'-') => {
                self.advance();
                if let Some(&'>') = self.source.peek() {
                    self.advance();
                    TokenKind::Arrow
                } else {
                    panic!("Unexpected character after '-'");
                }
            }
            Some(&ch) => {
                panic!("Unexpected character: {ch}");
            }
            None => TokenKind::Eof,
        }
    }
}
