use crate::ast::{BinOp, Expr, Function, Type, UnaryOp};
use crate::lexer::TokenKind;
use std::collections::HashMap;
use std::iter::Peekable;
use std::sync::LazyLock;

static BINARY_OPS_MAP: LazyLock<HashMap<TokenKind, BinOp>> = LazyLock::new(|| {
    HashMap::from([
        // Arithmetic operators
        (TokenKind::Plus, BinOp::Add),
        (TokenKind::Minus, BinOp::Sub),
        (TokenKind::Star, BinOp::Mul),
        (TokenKind::Slash, BinOp::Div),
        // Comparison operators
        (TokenKind::EqEq, BinOp::Eq),
        (TokenKind::NotEq, BinOp::Ne),
        (TokenKind::LessThan, BinOp::Lt),
        (TokenKind::GreaterThan, BinOp::Gt),
        (TokenKind::LessThanEq, BinOp::LtEq),
        (TokenKind::GreaterThanEq, BinOp::GtEq),
    ])
});

pub struct Parser<I>
where
    I: Iterator<Item = TokenKind>,
{
    tokens: Peekable<I>,
    curr_token: Option<TokenKind>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = TokenKind>,
{
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
            curr_token: None,
        }
    }

    fn advance(&mut self) {
        self.curr_token = self.tokens.next();
    }

    fn consume(&mut self, expected: &TokenKind) {
        match &self.curr_token {
            Some(token) if token == expected => {
                self.advance();
            }
            other => panic!("Expected {expected:?}, found: {other:?}"),
        }
    }

    fn consume_keyword(&mut self, expected: &str) {
        match &self.curr_token {
            Some(TokenKind::Ident(name)) if name == expected => {
                self.advance();
            }
            other => panic!("Expected {expected}, found: {other:?}"),
        }
    }

    fn parse_ident(&mut self) -> String {
        let name = match &self.curr_token {
            Some(TokenKind::Ident(name)) => name.clone(),
            other => panic!("Expected identifier, found: {other:?}"),
        };
        self.advance();
        name
    }

    fn parse_function(&mut self) -> Function {
        // Expect 'fn'
        self.consume_keyword("fn");

        // Expect function name
        let name = self.parse_ident();

        // Expect '('
        self.consume(&TokenKind::LParen);

        // Expect ')'
        self.consume(&TokenKind::RParen);

        // Expect '->'
        self.consume(&TokenKind::Arrow);

        // Parse return type
        let return_type = self.parse_type();

        // Parse function body
        let body = self.parse_expr(0);

        Function {
            name,
            return_type,
            body,
        }
    }

    fn parse_type(&mut self) -> Type {
        let typ = match &self.curr_token {
            Some(TokenKind::Ident(name)) if name == "u32" => Type::UInt32,
            Some(TokenKind::Ident(name)) if name == "bool" => Type::Bool,
            other => panic!("Expected type, found: {other:?}"),
        };
        self.advance();
        typ
    }

    fn parse_block(&mut self) -> Vec<Expr> {
        let mut body = Vec::new();
        while self.curr_token != Some(TokenKind::RBrace) {
            let expr = match &self.curr_token {
                Some(TokenKind::Ident(name)) if name == "let" => self.parse_let(),
                Some(TokenKind::Ident(name)) if name == "var" => self.parse_var(),
                Some(TokenKind::Ident(_)) if self.tokens.peek() == Some(&TokenKind::Equals) => {
                    self.parse_assign()
                }
                _ => self.parse_expr(0),
            };
            body.push(expr);
            if self.curr_token == Some(TokenKind::Semicolon) {
                self.advance();
            }
        }
        body
    }

    fn parse_let(&mut self) -> Expr {
        self.consume_keyword("let");
        let name = self.parse_ident();
        self.consume(&TokenKind::Equals);
        let value = self.parse_expr(0);
        Expr::Let {
            name,
            value: Box::new(value),
        }
    }

    fn parse_var(&mut self) -> Expr {
        self.consume_keyword("var");
        let name = self.parse_ident();
        self.consume(&TokenKind::Equals);
        let value = self.parse_expr(0);
        Expr::Var {
            name,
            value: Box::new(value),
        }
    }

    fn parse_assign(&mut self) -> Expr {
        let name = self.parse_ident();
        self.consume(&TokenKind::Equals);
        let value = self.parse_expr(0);
        Expr::Assign {
            name,
            value: Box::new(value),
        }
    }

    fn parse_if(&mut self) -> Expr {
        self.consume_keyword("if");
        let cond = self.parse_expr(0);
        let then_body = self.parse_expr(0);
        self.consume_keyword("else");
        let else_body = self.parse_expr(0);
        Expr::If {
            cond: Box::new(cond),
            then_body: Box::new(then_body),
            else_body: Box::new(else_body),
        }
    }

    fn parse_while(&mut self) -> Expr {
        self.consume_keyword("while");
        let cond = self.parse_expr(0);
        let body = self.parse_expr(0);
        Expr::While {
            cond: Box::new(cond),
            body: Box::new(body),
        }
    }

    fn parse_primary(&mut self) -> Expr {
        match self.curr_token.clone() {
            Some(TokenKind::Ident(name)) if name == "if" => self.parse_if(),
            Some(TokenKind::Ident(name)) if name == "while" => self.parse_while(),
            Some(TokenKind::Ident(name)) => {
                self.advance();
                if name == "true" {
                    Expr::BoolLit(true)
                } else if name == "false" {
                    Expr::BoolLit(false)
                } else {
                    Expr::VarRef(name)
                }
            }
            Some(TokenKind::IntLit(value)) => {
                self.advance();
                Expr::UInt32Lit(value)
            }
            Some(TokenKind::LParen) if self.tokens.peek() == Some(&TokenKind::RParen) => {
                self.advance();
                self.advance();
                Expr::UnitLit
            }
            Some(TokenKind::LParen) => {
                // Parenthesized expression
                self.advance();
                let inner = self.parse_expr(0);
                self.consume(&TokenKind::RParen);
                inner
            }
            Some(TokenKind::LBrace) => {
                // Block expression
                self.advance();
                let body = self.parse_block();
                self.consume(&TokenKind::RBrace);
                Expr::Block(body)
            }
            other => panic!("Expected primary expression, found: {other:?}"),
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> Expr {
        let mut left = if self.curr_token == Some(TokenKind::Minus) {
            self.advance();
            let operand = self.parse_expr(10); // highest binding power
            Expr::UnaryOp {
                op: UnaryOp::Neg,
                expr: Box::new(operand),
            }
        } else {
            self.parse_primary()
        };

        while let Some(token) = &self.curr_token {
            if let Some(&op) = BINARY_OPS_MAP.get(token) {
                let bp = Self::binding_power(op);
                if bp < min_bp {
                    break;
                }
                self.advance();
                let right = self.parse_expr(bp + 1);
                left = Expr::BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        left
    }

    fn binding_power(op: BinOp) -> u8 {
        match op {
            BinOp::Add | BinOp::Sub => 1,
            BinOp::Mul | BinOp::Div => 2,
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => 3,
        }
    }

    pub fn parse(&mut self) -> Function {
        self.advance();
        self.parse_function()
    }
}
