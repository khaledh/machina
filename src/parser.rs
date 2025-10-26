use crate::ast::{Expr, Function, Type};
use crate::lexer::TokenKind;
use std::iter::Peekable;

pub struct Parser<I>
where
    I: Iterator<Item = TokenKind>,
{
    tokens: Peekable<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = TokenKind>,
{
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
        }
    }

    fn advance(&mut self) -> Option<TokenKind> {
        self.tokens.next()
    }

    fn expect(&mut self, expected: &TokenKind) {
        match self.advance() {
            Some(token) if &token == expected => {}
            other => panic!("Expected {expected:?}, found: {other:?}"),
        }
    }

    fn parse_function(&mut self) -> Function {
        // Expect 'fn'
        match self.advance() {
            Some(TokenKind::Ident(name)) if name == "fn" => {}
            other => panic!("Expected 'fn', found: {other:?}"),
        }

        // Expect function name
        let name = match self.advance() {
            Some(TokenKind::Ident(name)) => name,
            other => panic!("Expected function name, found: {other:?}"),
        };

        // Expect '('
        self.expect(&TokenKind::LParen);

        // Expect ')'
        self.expect(&TokenKind::RParen);

        // Expect '->'
        self.expect(&TokenKind::Arrow);

        // Parse return type
        let typ = self.parse_type();

        // Expect '{'
        self.expect(&TokenKind::LBrace);

        // Parse function body
        let body = self.parse_expr();

        // Expect '}'
        self.expect(&TokenKind::RBrace);

        Function {
            name,
            return_type: typ,
            body,
        }
    }

    fn parse_type(&mut self) -> Type {
        match self.advance() {
            Some(TokenKind::Ident(name)) if name == "u32" => Type::UInt32,
            other => panic!("Expected type, found: {other:?}"),
        }
    }

    fn parse_expr(&mut self) -> Expr {
        match self.advance() {
            Some(TokenKind::IntLit(value)) => Expr::UInt32Lit(value),
            Some(token) => panic!("Unexpected token in expression: {token:?}"),
            None => panic!("Unexpected eof while parsing expression"),
        }
    }

    pub fn parse(&mut self) -> Function {
        self.parse_function()
    }
}
