use crate::ast::{BinOp, Expr, ExprKind, Function, FunctionParam, Module, Type, UnaryOp};
use crate::diagnostics::{Position, Span};
use crate::lexer::{Token, TokenKind, TokenKind as TK};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Expected {0}, found: {1}")]
    ExpectedToken(TokenKind, Token),

    #[error("Expected keyword {0}, found: {1}")]
    ExpectedKeyword(String, Token),

    #[error("Expected identifier, found: {0}")]
    ExpectedIdent(Token),

    #[error("Expected type, found: {0}")]
    ExpectedType(Token),

    #[error("Expected primary expression, found: {0}")]
    ExpectedPrimary(Token),
}

impl ParserError {
    pub fn span(&self) -> Span {
        match self {
            ParserError::ExpectedToken(_, token) => token.span,
            ParserError::ExpectedKeyword(_, token) => token.span,
            ParserError::ExpectedIdent(token) => token.span,
            ParserError::ExpectedType(token) => token.span,
            ParserError::ExpectedPrimary(token) => token.span,
        }
    }
}

#[derive(Clone, Debug)]
struct Marker {
    pos: Position,
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
        if self.pos + 1 < self.tokens.len() {
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

    fn mark(&self) -> Marker {
        Marker {
            pos: self.curr_token.span.start,
        }
    }

    fn close(&self, marker: Marker) -> Span {
        Span::new(marker.pos, self.curr_token.span.end)
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
        if self.curr_token.kind == TK::Ident(expected.to_string()) {
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
        if let TK::Ident(name) = &self.curr_token.kind {
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
        self.consume(&TK::LParen)?;
        let params = self.parse_func_params()?;
        self.consume(&TK::RParen)?;

        // Parse return type (default to unit if not specified)
        let return_type = match self.curr_token.kind {
            TK::Arrow => {
                self.advance();
                self.parse_type()?
            }
            _ => Type::Unit,
        };

        // Parse function body
        let body = self.parse_block()?;

        Ok(Function {
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_func_params(&mut self) -> Result<Vec<FunctionParam>, ParserError> {
        self.parse_list(TK::Comma, TK::RParen, |parser| {
            let name = parser.parse_ident()?;
            parser.consume(&TK::Colon)?;
            let typ = parser.parse_type()?;
            Ok(FunctionParam { name, typ })
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let result = match &self.curr_token.kind {
            TK::LParen if self.peek().map(|t| &t.kind) == Some(&TK::RParen) => {
                self.advance();
                Ok(Type::Unit)
            }
            TK::Ident(name) if name == "u32" => Ok(Type::UInt32),
            TK::Ident(name) if name == "bool" => Ok(Type::Bool),
            _ => Err(ParserError::ExpectedType(self.curr_token.clone())),
        };
        result.map(|typ| {
            self.advance();
            typ
        })
    }

    fn parse_block(&mut self) -> Result<Expr, ParserError> {
        let marker = self.mark();
        self.consume(&TK::LBrace)?;
        let mut items = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            let expr = match &self.curr_token.kind {
                TK::Ident(name) if name == "let" => self.parse_let()?,
                TK::Ident(name) if name == "var" => self.parse_var()?,
                TK::Ident(_) if self.peek().map(|t| &t.kind) == Some(&TK::Equals) => {
                    self.parse_assign()?
                }
                _ => self.parse_expr(0)?,
            };
            items.push(expr);
            if self.curr_token.kind == TK::Semicolon {
                self.advance();
            }
        }
        self.consume(&TK::RBrace)?;
        Ok(Expr {
            kind: ExprKind::Block(items),
            span: self.close(marker),
        })
    }

    fn parse_let(&mut self) -> Result<Expr, ParserError> {
        let marker = self.mark();
        self.consume_keyword("let")?;
        let name = self.parse_ident()?;
        self.consume(&TK::Equals)?;
        let value = self.parse_expr(0)?;
        Ok(Expr {
            kind: ExprKind::Let {
                name,
                value: Box::new(value),
            },
            span: self.close(marker),
        })
    }

    fn parse_var(&mut self) -> Result<Expr, ParserError> {
        let marker = self.mark();
        self.consume_keyword("var")?;
        let name = self.parse_ident()?;
        self.consume(&TK::Equals)?;
        let value = self.parse_expr(0)?;
        Ok(Expr {
            kind: ExprKind::Var {
                name,
                value: Box::new(value),
            },
            span: self.close(marker),
        })
    }

    fn parse_assign(&mut self) -> Result<Expr, ParserError> {
        let marker = self.mark();
        let name = self.parse_ident()?;
        self.consume(&TK::Equals)?;
        let value = self.parse_expr(0)?;
        Ok(Expr {
            kind: ExprKind::Assign {
                name,
                value: Box::new(value),
            },
            span: self.close(marker),
        })
    }

    fn parse_if(&mut self) -> Result<Expr, ParserError> {
        let marker = self.mark();
        self.consume_keyword("if")?;
        let cond = self.parse_expr(0)?;
        let then_body = self.parse_expr(0)?;
        self.consume_keyword("else")?;
        let else_body = self.parse_expr(0)?;
        Ok(Expr {
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_body: Box::new(then_body),
                else_body: Box::new(else_body),
            },
            span: self.close(marker),
        })
    }

    fn parse_while(&mut self) -> Result<Expr, ParserError> {
        let marker = self.mark();
        self.consume_keyword("while")?;
        let cond = self.parse_expr(0)?;
        let body = self.parse_expr(0)?;
        Ok(Expr {
            kind: ExprKind::While {
                cond: Box::new(cond),
                body: Box::new(body),
            },
            span: self.close(marker),
        })
    }

    fn parse_call(&mut self) -> Result<Expr, ParserError> {
        let marker = self.mark();
        let name = self.parse_ident()?;
        self.consume(&TK::LParen)?;
        let args = self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_expr(0))?;
        self.consume(&TK::RParen)?;
        Ok(Expr {
            kind: ExprKind::Call { name, args },
            span: self.close(marker),
        })
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        match &self.curr_token.kind {
            TK::Ident(name) if name == "if" => self.parse_if(),
            TK::Ident(name) if name == "while" => self.parse_while(),
            TK::Ident(_)
                if matches!(
                    self.peek(),
                    Some(&Token {
                        kind: TK::LParen,
                        ..
                    })
                ) =>
            {
                self.parse_call()
            }
            TK::Ident(name) => {
                let span = self.curr_token.span;
                self.advance();
                match name.as_str() {
                    "true" => Ok(Expr {
                        kind: ExprKind::BoolLit(true),
                        span: span,
                    }),
                    "false" => Ok(Expr {
                        kind: ExprKind::BoolLit(false),
                        span: span,
                    }),
                    _ => Ok(Expr {
                        kind: ExprKind::VarRef(name.clone()),
                        span: span,
                    }),
                }
            }
            TK::IntLit(value) => {
                let span = self.curr_token.span;
                self.advance();
                Ok(Expr {
                    kind: ExprKind::UInt32Lit(*value),
                    span: span,
                })
            }
            TK::LParen
                if matches!(
                    self.peek(),
                    Some(&Token {
                        kind: TK::RParen,
                        ..
                    })
                ) =>
            {
                let marker = self.mark();
                self.advance(); // consume '('
                let span = self.close(marker);
                self.advance(); // consume ')'
                Ok(Expr {
                    kind: ExprKind::UnitLit,
                    span: span,
                })
            }
            TK::LParen => {
                // Parenthesized expression
                self.advance();
                let inner = self.parse_expr(0)?;
                self.consume(&TK::RParen)?;
                Ok(inner)
            }
            TK::LBrace => {
                // Block expression
                self.parse_block()
            }
            _ => Err(ParserError::ExpectedPrimary(self.curr_token.clone())),
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr, ParserError> {
        let marker = self.mark();
        let mut lhs = if self.curr_token.kind == TK::Minus {
            self.advance();
            let operand = self.parse_expr(10)?; // highest binding power
            Expr {
                kind: ExprKind::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: Box::new(operand),
                },
                span: self.close(marker.clone()),
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
            lhs = Expr {
                kind: ExprKind::BinOp {
                    left: Box::new(lhs),
                    op,
                    right: Box::new(rhs),
                },
                span: self.close(marker.clone()),
            };
        }
        Ok(lhs)
    }

    fn bin_op_from_token(token: &TokenKind) -> Option<(BinOp, u8)> {
        match token {
            TK::Plus => Some((BinOp::Add, 1)),
            TK::Minus => Some((BinOp::Sub, 1)),
            TK::Star => Some((BinOp::Mul, 2)),
            TK::Slash => Some((BinOp::Div, 2)),
            TK::EqEq => Some((BinOp::Eq, 3)),
            TK::NotEq => Some((BinOp::Ne, 3)),
            TK::LessThan => Some((BinOp::Lt, 3)),
            TK::GreaterThan => Some((BinOp::Gt, 3)),
            TK::LessThanEq => Some((BinOp::LtEq, 3)),
            TK::GreaterThanEq => Some((BinOp::GtEq, 3)),
            _ => None,
        }
    }

    pub fn parse(&mut self) -> Result<Module, ParserError> {
        let mut functions = Vec::new();
        while self.curr_token.kind != TK::Eof {
            functions.push(self.parse_function()?);
        }
        Ok(Module { funcs: functions })
    }
}
