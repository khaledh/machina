use crate::ast::{BinaryOp, Expr, ExprKind, Function, FunctionParam, Module, UnaryOp};
use crate::diagnostics::{Position, Span};
use crate::ids::NodeIdGen;
use crate::lexer::{Token, TokenKind, TokenKind as TK};
use crate::types::Type;
use thiserror::Error;

#[derive(Debug, Error)]
#[allow(clippy::enum_variant_names)]
pub enum ParseError {
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

    #[error("Expected integer literal, found: {0}")]
    ExpectedIntLit(Token),
}

impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            ParseError::ExpectedToken(_, token) => token.span,
            ParseError::ExpectedKeyword(_, token) => token.span,
            ParseError::ExpectedIdent(token) => token.span,
            ParseError::ExpectedType(token) => token.span,
            ParseError::ExpectedPrimary(token) => token.span,
            ParseError::ExpectedIntLit(token) => token.span,
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
    id_gen: NodeIdGen,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            pos: 0,
            curr_token: &tokens[0],
            id_gen: NodeIdGen::new(),
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

    fn consume(&mut self, expected: &TokenKind) -> Result<(), ParseError> {
        if self.curr_token.kind == *expected {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::ExpectedToken(
                expected.clone(),
                self.curr_token.clone(),
            ))
        }
    }

    fn consume_keyword(&mut self, expected: &str) -> Result<(), ParseError> {
        if self.curr_token.kind == TK::Ident(expected.to_string()) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::ExpectedKeyword(
                expected.to_string(),
                self.curr_token.clone(),
            ))
        }
    }

    fn parse_ident(&mut self) -> Result<String, ParseError> {
        if let TK::Ident(name) = &self.curr_token.kind {
            self.advance();
            Ok(name.clone())
        } else {
            Err(ParseError::ExpectedIdent(self.curr_token.clone()))
        }
    }

    fn parse_int_lit(&mut self) -> Result<u64, ParseError> {
        if let TK::IntLit(value) = &self.curr_token.kind {
            self.advance();
            Ok(*value)
        } else {
            Err(ParseError::ExpectedIntLit(self.curr_token.clone()))
        }
    }

    fn parse_list<T>(
        &mut self,
        sep_token: TokenKind,
        end_token: TokenKind,
        mut parse_item: impl FnMut(&mut Self) -> Result<T, ParseError>,
    ) -> Result<Vec<T>, ParseError> {
        let mut items = Vec::new();
        while self.curr_token.kind != end_token {
            items.push(parse_item(self)?);
            if self.curr_token.kind == sep_token {
                self.advance();
            }
        }
        Ok(items)
    }

    fn parse_function(&mut self) -> Result<Function, ParseError> {
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
            id: self.id_gen.new_id(),
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_func_params(&mut self) -> Result<Vec<FunctionParam>, ParseError> {
        self.parse_list(TK::Comma, TK::RParen, |parser| {
            let name = parser.parse_ident()?;
            parser.consume(&TK::Colon)?;
            let typ = parser.parse_type()?;
            Ok(FunctionParam {
                id: parser.id_gen.new_id(),
                name,
                typ,
            })
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        // Parse simple type
        let mut typ = self.parse_simple_type()?;

        // Check for array type
        if self.curr_token.kind == TK::LBracket {
            self.advance();
            let dims = self.parse_list(TK::Comma, TK::RBracket, |parser| parser.parse_int_lit())?;
            self.consume(&TK::RBracket)?;
            typ = Type::Array {
                elem_ty: Box::new(typ),
                dims: dims.into_iter().map(|d| d as usize).collect(),
            };
        }

        Ok(typ)
    }

    fn parse_simple_type(&mut self) -> Result<Type, ParseError> {
        let result = match &self.curr_token.kind {
            TK::LParen if self.peek().map(|t| &t.kind) == Some(&TK::RParen) => {
                self.advance();
                Ok(Type::Unit)
            }
            TK::Ident(name) if name == "u64" => Ok(Type::UInt64),
            TK::Ident(name) if name == "bool" => Ok(Type::Bool),
            _ => Err(ParseError::ExpectedType(self.curr_token.clone())),
        };
        result.inspect(|_| self.advance())
    }

    fn parse_block(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();
        self.consume(&TK::LBrace)?;

        let mut items = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            let expr = match &self.curr_token.kind {
                TK::Ident(name) if name == "let" => self.parse_let()?,
                TK::Ident(name) if name == "var" => self.parse_var()?,
                _ => {
                    let expr = self.parse_expr(0)?;
                    if self.curr_token.kind == TK::Equals {
                        self.parse_assign(expr)?
                    } else {
                        expr
                    }
                }
            };
            items.push(expr);
            if self.curr_token.kind == TK::Semicolon {
                self.advance();
            }
        }

        self.consume(&TK::RBrace)?;
        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Block(items),
            span: self.close(marker),
        })
    }

    fn parse_let(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();
        self.consume_keyword("let")?;
        let name = self.parse_ident()?;
        self.consume(&TK::Equals)?;
        let value = self.parse_expr(0)?;
        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Let {
                name,
                value: Box::new(value),
            },
            span: self.close(marker),
        })
    }

    fn parse_var(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();
        self.consume_keyword("var")?;
        let name = self.parse_ident()?;
        self.consume(&TK::Equals)?;
        let value = self.parse_expr(0)?;
        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Var {
                name,
                value: Box::new(value),
            },
            span: self.close(marker),
        })
    }

    fn parse_assign(&mut self, assignee: Expr) -> Result<Expr, ParseError> {
        let marker = self.mark();
        self.consume(&TK::Equals)?;
        let value = self.parse_expr(0)?;
        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Assign {
                assignee: Box::new(assignee),
                value: Box::new(value),
            },
            span: self.close(marker),
        })
    }

    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();
        self.consume_keyword("if")?;
        let cond = self.parse_expr(0)?;
        let then_body = self.parse_expr(0)?;
        self.consume_keyword("else")?;
        let else_body = self.parse_expr(0)?;
        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_body: Box::new(then_body),
                else_body: Box::new(else_body),
            },
            span: self.close(marker),
        })
    }

    fn parse_while(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();
        self.consume_keyword("while")?;
        let cond = self.parse_expr(0)?;
        let body = self.parse_expr(0)?;
        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::While {
                cond: Box::new(cond),
                body: Box::new(body),
            },
            span: self.close(marker),
        })
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();
        let mut expr = self.parse_primary()?;

        loop {
            match self.curr_token.kind {
                TK::LParen => {
                    // Call expression
                    self.advance();
                    let args =
                        self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_expr(0))?;
                    self.consume(&TK::RParen)?;
                    expr = Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::Call {
                            callee: Box::new(expr),
                            args,
                        },
                        span: self.close(marker.clone()),
                    };
                }
                TK::LBracket => {
                    // Index expression
                    self.consume(&TK::LBracket)?;
                    let indices =
                        self.parse_list(TK::Comma, TK::RBracket, |parser| parser.parse_expr(0))?;
                    self.consume(&TK::RBracket)?;
                    expr = Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::Index {
                            target: Box::new(expr),
                            indices,
                        },
                        span: self.close(marker.clone()),
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match &self.curr_token.kind {
            TK::Ident(name) if name == "if" => self.parse_if(),
            TK::Ident(name) if name == "while" => self.parse_while(),
            TK::Ident(name) => {
                let span = self.curr_token.span;
                self.advance();
                match name.as_str() {
                    "true" => Ok(Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::BoolLit(true),
                        span,
                    }),
                    "false" => Ok(Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::BoolLit(false),
                        span,
                    }),
                    _ => Ok(Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::VarRef(name.clone()),
                        span,
                    }),
                }
            }
            TK::IntLit(value) => {
                let span = self.curr_token.span;
                self.advance();
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::UInt64Lit(*value),
                    span,
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
                    id: self.id_gen.new_id(),
                    kind: ExprKind::UnitLit,
                    span,
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
            TK::LBracket => {
                // Array literal
                let marker = self.mark();
                self.advance(); // consume '['
                let elems =
                    self.parse_list(TK::Comma, TK::RBracket, |parser| parser.parse_expr(0))?;
                self.consume(&TK::RBracket)?; // consume ']'
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::ArrayLit(elems),
                    span: self.close(marker),
                })
            }
            _ => Err(ParseError::ExpectedPrimary(self.curr_token.clone())),
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let marker = self.mark();
        let mut lhs = if self.curr_token.kind == TK::Minus {
            self.advance();
            let operand = self.parse_expr(10)?; // highest binding power
            Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: Box::new(operand),
                },
                span: self.close(marker.clone()),
            }
        } else {
            self.parse_postfix()?
        };

        while let Some((op, bp)) = Self::bin_op_from_token(&self.curr_token.kind) {
            if bp < min_bp {
                break;
            }
            self.advance();
            let rhs = self.parse_expr(bp + 1)?;
            lhs = Expr {
                id: self.id_gen.new_id(),
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

    fn bin_op_from_token(token: &TokenKind) -> Option<(BinaryOp, u8)> {
        match token {
            TK::Plus => Some((BinaryOp::Add, 1)),
            TK::Minus => Some((BinaryOp::Sub, 1)),
            TK::Star => Some((BinaryOp::Mul, 2)),
            TK::Slash => Some((BinaryOp::Div, 2)),
            TK::EqEq => Some((BinaryOp::Eq, 3)),
            TK::NotEq => Some((BinaryOp::Ne, 3)),
            TK::LessThan => Some((BinaryOp::Lt, 3)),
            TK::GreaterThan => Some((BinaryOp::Gt, 3)),
            TK::LessThanEq => Some((BinaryOp::LtEq, 3)),
            TK::GreaterThanEq => Some((BinaryOp::GtEq, 3)),
            _ => None,
        }
    }

    pub fn parse(&mut self) -> Result<Module, ParseError> {
        let mut functions = Vec::new();
        while self.curr_token.kind != TK::Eof {
            functions.push(self.parse_function()?);
        }
        Ok(Module { funcs: functions })
    }
}

#[cfg(test)]
#[path = "tests/t_parser.rs"]
mod tests;
