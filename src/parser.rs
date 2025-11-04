use crate::ast::{BinOp, Expr, Function, FunctionParam, Module, Type, UnaryOp};
use crate::lexer::{LexError, Lexer, TokenKind};
use std::collections::HashMap;
use std::iter::Peekable;
use std::sync::LazyLock;
use thiserror::Error;

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

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Expected {0:?}, found: {1:?}")]
    ExpectedFound(TokenKind, TokenKind),

    #[error("Expected {0}, found: {1:?}")]
    ExpectedKeyword(String, TokenKind),

    #[error("Expected identifier, found: {0:?}")]
    ExpectedIdent(TokenKind),

    #[error("Expected type, found: {0:?}")]
    ExpectedType(TokenKind),

    #[error("Expected primary expression, found: {0:?}")]
    ExpectedPrimary(TokenKind),

    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(TokenKind),
}

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

    fn consume(&mut self, expected: &TokenKind) -> Result<(), ParserError> {
        match &self.curr_token {
            Some(token) if token == expected => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(ParserError::ExpectedFound(expected.clone(), token.clone())),
            None => Err(ParserError::ExpectedFound(expected.clone(), TokenKind::Eof)),
        }
    }

    fn consume_keyword(&mut self, expected: &str) -> Result<(), ParserError> {
        match &self.curr_token {
            Some(TokenKind::Ident(name)) if name == expected => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(ParserError::ExpectedKeyword(
                expected.to_string(),
                token.clone(),
            )),
            None => Err(ParserError::ExpectedKeyword(
                expected.to_string(),
                TokenKind::Eof,
            )),
        }
    }

    fn parse_ident(&mut self) -> Result<String, ParserError> {
        match &self.curr_token {
            Some(TokenKind::Ident(name)) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            Some(token) => Err(ParserError::ExpectedIdent(token.clone())),
            None => Err(ParserError::ExpectedIdent(TokenKind::Eof)),
        }
    }

    fn parse_list<T>(
        &mut self,
        sep_token: TokenKind,
        end_token: TokenKind,
        mut parse_item: impl FnMut(&mut Self) -> Result<T, ParserError>,
    ) -> Result<Vec<T>, ParserError> {
        let mut items = Vec::new();
        while self.curr_token != Some(end_token.clone()) {
            items.push(parse_item(self)?);
            if self.curr_token == Some(sep_token.clone()) {
                self.advance();
            }
        }
        Ok(items)
    }

    fn parse_function(&mut self) -> Result<Function, ParserError> {
        // Expect 'fn'
        self.consume_keyword("fn")?;

        // Expect function name
        let name = self.parse_ident()?;

        // Parse function params
        self.consume(&TokenKind::LParen)?;
        let params = self.parse_func_params()?;
        self.consume(&TokenKind::RParen)?;

        // Parse return type (default to unit if not specified)
        let return_type = match self.curr_token {
            Some(TokenKind::Arrow) => {
                self.advance();
                self.parse_type()?
            }
            _ => Type::Unit,
        };

        // Parse function body
        let body = self.parse_expr(0)?;

        Ok(Function {
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_func_params(&mut self) -> Result<Vec<FunctionParam>, ParserError> {
        self.parse_list(TokenKind::Comma, TokenKind::RParen, |parser| {
            let name = parser.parse_ident()?;
            parser.consume(&TokenKind::Colon)?;
            let typ = parser.parse_type()?;
            Ok(FunctionParam { name, typ })
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let typ = match &self.curr_token {
            Some(TokenKind::LParen) if self.tokens.peek() == Some(&TokenKind::RParen) => {
                self.advance();
                self.advance();
                Ok(Type::Unit)
            }
            Some(TokenKind::Ident(name)) if name == "u32" => Ok(Type::UInt32),
            Some(TokenKind::Ident(name)) if name == "bool" => Ok(Type::Bool),
            Some(token) => Err(ParserError::ExpectedType(token.clone())),
            None => Err(ParserError::ExpectedType(TokenKind::Eof)),
        };
        match typ {
            Ok(typ) => {
                self.advance();
                Ok(typ)
            }
            Err(e) => Err(e),
        }
    }

    fn parse_block(&mut self) -> Result<Vec<Expr>, ParserError> {
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
        body.into_iter().collect()
    }

    fn parse_let(&mut self) -> Result<Expr, ParserError> {
        self.consume_keyword("let")?;
        let name = self.parse_ident()?;
        self.consume(&TokenKind::Equals)?;
        let value = self.parse_expr(0)?;
        Ok(Expr::Let {
            name,
            value: Box::new(value),
        })
    }

    fn parse_var(&mut self) -> Result<Expr, ParserError> {
        self.consume_keyword("var")?;
        let name = self.parse_ident()?;
        self.consume(&TokenKind::Equals)?;
        let value = self.parse_expr(0)?;
        Ok(Expr::Var {
            name,
            value: Box::new(value),
        })
    }

    fn parse_assign(&mut self) -> Result<Expr, ParserError> {
        let name = self.parse_ident()?;
        self.consume(&TokenKind::Equals)?;
        let value = self.parse_expr(0)?;
        Ok(Expr::Assign {
            name,
            value: Box::new(value),
        })
    }

    fn parse_if(&mut self) -> Result<Expr, ParserError> {
        self.consume_keyword("if")?;
        let cond = self.parse_expr(0)?;
        let then_body = self.parse_expr(0)?;
        self.consume_keyword("else")?;
        let else_body = self.parse_expr(0)?;
        Ok(Expr::If {
            cond: Box::new(cond),
            then_body: Box::new(then_body),
            else_body: Box::new(else_body),
        })
    }

    fn parse_while(&mut self) -> Result<Expr, ParserError> {
        self.consume_keyword("while")?;
        let cond = self.parse_expr(0)?;
        let body = self.parse_expr(0)?;
        Ok(Expr::While {
            cond: Box::new(cond),
            body: Box::new(body),
        })
    }

    fn parse_call(&mut self, name: String) -> Result<Expr, ParserError> {
        self.consume(&TokenKind::LParen)?;
        let args = self.parse_list(TokenKind::Comma, TokenKind::RParen, |parser| {
            parser.parse_expr(0)
        })?;
        self.consume(&TokenKind::RParen)?;
        Ok(Expr::Call { name, args })
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        match self.curr_token.clone() {
            Some(TokenKind::Ident(name)) if name == "if" => self.parse_if(),
            Some(TokenKind::Ident(name)) if name == "while" => self.parse_while(),
            Some(TokenKind::Ident(name)) if self.tokens.peek() == Some(&TokenKind::LParen) => {
                self.advance();
                self.parse_call(name)
            }
            Some(TokenKind::Ident(name)) => {
                self.advance();
                match name.as_str() {
                    "true" => Ok(Expr::BoolLit(true)),
                    "false" => Ok(Expr::BoolLit(false)),
                    _ => Ok(Expr::VarRef(name)),
                }
            }
            Some(TokenKind::IntLit(value)) => {
                self.advance();
                Ok(Expr::UInt32Lit(value))
            }
            Some(TokenKind::LParen) if self.tokens.peek() == Some(&TokenKind::RParen) => {
                self.advance();
                self.advance();
                Ok(Expr::UnitLit)
            }
            Some(TokenKind::LParen) => {
                // Parenthesized expression
                self.advance();
                let inner = self.parse_expr(0)?;
                self.consume(&TokenKind::RParen)?;
                Ok(inner)
            }
            Some(TokenKind::LBrace) => {
                // Block expression
                self.advance();
                let body = self.parse_block()?;
                self.consume(&TokenKind::RBrace)?;
                Ok(Expr::Block(body))
            }
            Some(token) => Err(ParserError::ExpectedPrimary(token.clone())),
            None => Err(ParserError::ExpectedPrimary(TokenKind::Eof)),
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr, ParserError> {
        let mut left = if self.curr_token == Some(TokenKind::Minus) {
            self.advance();
            let operand = self.parse_expr(10)?; // highest binding power
            Expr::UnaryOp {
                op: UnaryOp::Neg,
                expr: Box::new(operand),
            }
        } else {
            self.parse_primary()?
        };

        while let Some(token) = &self.curr_token {
            if let Some(&op) = BINARY_OPS_MAP.get(token) {
                let bp = Self::binding_power(op);
                if bp < min_bp {
                    break;
                }
                self.advance();
                let right = self.parse_expr(bp + 1)?;
                left = Expr::BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn binding_power(op: BinOp) -> u8 {
        match op {
            BinOp::Add | BinOp::Sub => 1,
            BinOp::Mul | BinOp::Div => 2,
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => 3,
        }
    }

    pub fn parse(&mut self) -> Result<Module, ParserError> {
        self.advance();
        let mut functions = Vec::new();
        while let Some(TokenKind::Ident(name)) = &self.curr_token
            && name == "fn"
        {
            functions.push(self.parse_function()?);
        }
        match &self.curr_token {
            None => Ok(Module { funcs: functions }),
            Some(token) => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }
}
