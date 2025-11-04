use crate::ast::{BinOp, Expr, Function, FunctionParam, Module, Type, UnaryOp};
use crate::lexer::{Token, TokenKind};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Expected {0:?}, found: {1:?}")]
    ExpectedToken(TokenKind, Token),

    #[error("Expected {0}, found: {1:?}")]
    ExpectedKeyword(String, Token),

    #[error("Expected identifier, found: {0:?}")]
    ExpectedIdent(Token),

    #[error("Expected type, found: {0:?}")]
    ExpectedType(Token),

    #[error("Expected primary expression, found: {0:?}")]
    ExpectedPrimary(Token),

    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(Token),
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    curr_token: &'a Token,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            pos: 0,
            curr_token: &tokens[0],
        }
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
            self.curr_token = &self.tokens[self.pos];
        }
    }

    fn peek(&self) -> Option<&Token> {
        if self.pos + 1 < self.tokens.len() {
            Some(&self.tokens[self.pos + 1])
        } else {
            None
        }
    }

    fn consume(&mut self, expected: &TokenKind) -> Result<(), ParserError> {
        if self.curr_token.kind == *expected {
            self.advance();
            Ok(())
        } else {
            Err(ParserError::ExpectedToken(
                expected.clone(),
                self.curr_token.clone(),
            ))
        }
    }

    fn consume_keyword(&mut self, expected: &str) -> Result<(), ParserError> {
        if self.curr_token.kind == TokenKind::Ident(expected.to_string()) {
            self.advance();
            Ok(())
        } else {
            Err(ParserError::ExpectedKeyword(
                expected.to_string(),
                self.curr_token.clone(),
            ))
        }
    }

    fn parse_ident(&mut self) -> Result<String, ParserError> {
        if let TokenKind::Ident(name) = &self.curr_token.kind {
            self.advance();
            Ok(name.clone())
        } else {
            Err(ParserError::ExpectedIdent(self.curr_token.clone()))
        }
    }

    fn parse_list<T>(
        &mut self,
        sep_token: TokenKind,
        end_token: TokenKind,
        mut parse_item: impl FnMut(&mut Self) -> Result<T, ParserError>,
    ) -> Result<Vec<T>, ParserError> {
        let mut items = Vec::new();
        while self.curr_token.kind != end_token {
            items.push(parse_item(self)?);
            if self.curr_token.kind == sep_token {
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
        let return_type = match self.curr_token.kind {
            TokenKind::Arrow => {
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
        let result = match &self.curr_token.kind {
            TokenKind::LParen if self.peek().map(|t| &t.kind) == Some(&TokenKind::RParen) => {
                self.advance();
                Ok(Type::Unit)
            }
            TokenKind::Ident(name) if name == "u32" => Ok(Type::UInt32),
            TokenKind::Ident(name) if name == "bool" => Ok(Type::Bool),
            _ => Err(ParserError::ExpectedType(self.curr_token.clone())),
        };
        result.map(|typ| {
            self.advance();
            typ
        })
    }

    fn parse_block(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut body = Vec::new();
        while self.curr_token.kind != TokenKind::RBrace {
            let expr = match &self.curr_token.kind {
                TokenKind::Ident(name) if name == "let" => self.parse_let()?,
                TokenKind::Ident(name) if name == "var" => self.parse_var()?,
                TokenKind::Ident(_) if self.peek().map(|t| &t.kind) == Some(&TokenKind::Equals) => {
                    self.parse_assign()?
                }
                _ => self.parse_expr(0)?,
            };
            body.push(expr);
            if self.curr_token.kind == TokenKind::Semicolon {
                self.advance();
            }
        }
        Ok(body)
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
        match &self.curr_token.kind {
            TokenKind::Ident(name) if name == "if" => self.parse_if(),
            TokenKind::Ident(name) if name == "while" => self.parse_while(),
            TokenKind::Ident(name) if self.peek().map(|t| &t.kind) == Some(&TokenKind::LParen) => {
                let func_name = name.to_string();
                self.advance();
                self.parse_call(func_name)
            }
            TokenKind::Ident(name) => {
                self.advance();
                match name.as_str() {
                    "true" => Ok(Expr::BoolLit(true)),
                    "false" => Ok(Expr::BoolLit(false)),
                    _ => Ok(Expr::VarRef(name.clone())),
                }
            }
            TokenKind::IntLit(value) => {
                self.advance();
                Ok(Expr::UInt32Lit(*value))
            }
            TokenKind::LParen if self.peek().map(|t| &t.kind) == Some(&TokenKind::RParen) => {
                self.advance();
                self.advance();
                Ok(Expr::UnitLit)
            }
            TokenKind::LParen => {
                // Parenthesized expression
                self.advance();
                let inner = self.parse_expr(0)?;
                self.consume(&TokenKind::RParen)?;
                Ok(inner)
            }
            TokenKind::LBrace => {
                // Block expression
                self.advance();
                let body = self.parse_block()?;
                self.consume(&TokenKind::RBrace)?;
                Ok(Expr::Block(body))
            }
            _ => Err(ParserError::ExpectedPrimary(self.curr_token.clone())),
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr, ParserError> {
        let mut lhs = if self.curr_token.kind == TokenKind::Minus {
            self.advance();
            let operand = self.parse_expr(10)?; // highest binding power
            Expr::UnaryOp {
                op: UnaryOp::Neg,
                expr: Box::new(operand),
            }
        } else {
            self.parse_primary()?
        };

        while let Some((op, bp)) = Self::bin_op_from_token(&self.curr_token.kind) {
            if bp < min_bp {
                break;
            }
            self.advance();
            let rhs = self.parse_expr(bp + 1)?;
            lhs = Expr::BinOp {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    fn bin_op_from_token(token: &TokenKind) -> Option<(BinOp, u8)> {
        match token {
            TokenKind::Plus => Some((BinOp::Add, 1)),
            TokenKind::Minus => Some((BinOp::Sub, 1)),
            TokenKind::Star => Some((BinOp::Mul, 2)),
            TokenKind::Slash => Some((BinOp::Div, 2)),
            TokenKind::EqEq => Some((BinOp::Eq, 3)),
            TokenKind::NotEq => Some((BinOp::Ne, 3)),
            TokenKind::LessThan => Some((BinOp::Lt, 3)),
            TokenKind::GreaterThan => Some((BinOp::Gt, 3)),
            TokenKind::LessThanEq => Some((BinOp::LtEq, 3)),
            TokenKind::GreaterThanEq => Some((BinOp::GtEq, 3)),
            _ => None,
        }
    }

    pub fn parse(&mut self) -> Result<Module, ParserError> {
        let mut functions = Vec::new();
        while self.curr_token.kind == TokenKind::Ident("fn".to_string()) {
            functions.push(self.parse_function()?);
        }
        if self.curr_token.kind != TokenKind::Eof {
            return Err(ParserError::UnexpectedToken(self.curr_token.clone()));
        }
        Ok(Module { funcs: functions })
    }
}
