use crate::ast::{BinOp, Expr, Function, Type};
use crate::lexer::TokenKind;
use std::collections::HashMap;
use std::iter::Peekable;
use std::sync::LazyLock;

static BINARY_OPS_MAP: LazyLock<HashMap<TokenKind, BinOp>> = LazyLock::new(|| {
    HashMap::from([
        (TokenKind::Plus, BinOp::Add),
        (TokenKind::Minus, BinOp::Sub),
        (TokenKind::Star, BinOp::Mul),
        (TokenKind::Slash, BinOp::Div),
    ])
});

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

    fn peek(&mut self) -> Option<&TokenKind> {
        self.tokens.peek()
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
            Some(TokenKind::IntLit(value)) => {
                let lit = Expr::UInt32Lit(value);
                let is_binop = matches!(
                    self.peek(),
                    Some(TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash)
                );
                if is_binop {
                    let op_kind = self.advance().unwrap();
                    let right = self.parse_expr();
                    Expr::BinOp {
                        left: Box::new(lit),
                        op: BINARY_OPS_MAP[&op_kind],
                        right: Box::new(right),
                    }
                } else {
                    lit
                }
            }
            Some(token) => panic!("Unexpected token in expression: {token:?}"),
            None => panic!("Unexpected eof while parsing expression"),
        }
    }

    pub fn parse(&mut self) -> Function {
        self.parse_function()
    }
}
