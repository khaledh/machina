use thiserror::Error;

use crate::ast::NodeIdGen;
use crate::ast::{
    BinaryOp, Decl, EnumVariant, Expr, ExprKind, Function, FunctionParam, Module, Pattern,
    PatternKind, StructField, StructLitField, StructPatternField, TypeDecl, TypeDeclKind, TypeExpr,
    TypeExprKind, UnaryOp,
};
use crate::diagnostics::{Position, Span};
use crate::lexer::{Token, TokenKind, TokenKind as TK};

#[derive(Debug, Error)]
#[allow(clippy::enum_variant_names)]
pub enum ParseError {
    #[error("Expected declaration, found: {0}")]
    ExpectedDecl(Token),

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

    #[error("Expected pattern, found: {0}")]
    ExpectedPattern(Token),

    #[error("Single field tuple missing trailing comma: {0}")]
    SingleFieldTupleMissingComma(Token),

    #[error("Expected struct field, found: {0}")]
    ExpectedStructField(Token),
}

impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            ParseError::ExpectedDecl(token) => token.span,
            ParseError::ExpectedToken(_, token) => token.span,
            ParseError::ExpectedKeyword(_, token) => token.span,
            ParseError::ExpectedIdent(token) => token.span,
            ParseError::ExpectedType(token) => token.span,
            ParseError::ExpectedPrimary(token) => token.span,
            ParseError::ExpectedIntLit(token) => token.span,
            ParseError::ExpectedPattern(token) => token.span,
            ParseError::SingleFieldTupleMissingComma(token) => token.span,
            ParseError::ExpectedStructField(token) => token.span,
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
        let mut items = vec![];
        while self.curr_token.kind != end_token {
            items.push(parse_item(self)?);
            if self.curr_token.kind == sep_token {
                self.advance();
            }
        }
        Ok(items)
    }

    fn parse_type_decl(&mut self) -> Result<TypeDecl, ParseError> {
        // Expect 'type'
        self.consume_keyword("type")?;

        // Expect type name
        let name = self.parse_ident()?;
        self.consume(&TK::Equals)?;

        // Branch based on the next token
        let kind = if self.curr_token.kind == TK::LBrace {
            // Struct definition: type Foo = { ... }
            self.parse_struct_def()?
        } else if matches!(self.curr_token.kind, TK::Ident(_))
            && self.peek().map(|t| &t.kind) == Some(&TK::Pipe)
        {
            // Enum definition: type Foo = Bar | Baz
            self.parse_enum_def()?
        } else {
            // Type alias: type Foo = Bar
            let ty = self.parse_type_expr()?;

            // Consume optional ';'
            if self.curr_token.kind == TK::Semicolon {
                self.advance();
            }

            TypeDeclKind::Alias { aliased_ty: ty }
        };

        Ok(TypeDecl {
            id: self.id_gen.new_id(),
            name,
            kind,
        })
    }

    fn parse_struct_def(&mut self) -> Result<TypeDeclKind, ParseError> {
        self.consume(&TK::LBrace)?;

        // Parse struct fields
        let fields = self.parse_list(TK::Comma, TK::RBrace, |parser| {
            let marker = parser.mark();
            // Parse field name
            let name = parser.parse_ident()?;

            // Expect ':'
            parser.consume(&TK::Colon)?;

            // Parse field type
            let ty = parser.parse_type_expr()?;

            Ok(StructField {
                id: parser.id_gen.new_id(),
                name,
                ty,
                span: parser.close(marker),
            })
        })?;

        self.consume(&TK::RBrace)?;
        Ok(TypeDeclKind::Struct { fields })
    }

    fn parse_enum_def(&mut self) -> Result<TypeDeclKind, ParseError> {
        let mut variants = Vec::new();

        let marker = self.mark();

        // Parse first variant
        let name = self.parse_ident()?;
        variants.push(EnumVariant {
            id: self.id_gen.new_id(),
            name,
            span: self.close(marker),
        });

        // Parse remaining variants
        while self.curr_token.kind == TK::Pipe {
            self.advance(); // consume '|'
            let marker = self.mark();
            let name = self.parse_ident()?;
            variants.push(EnumVariant {
                id: self.id_gen.new_id(),
                name,
                span: self.close(marker),
            });
        }

        // Consume optional ';'
        if self.curr_token.kind == TK::Semicolon {
            self.advance();
        }

        Ok(TypeDeclKind::Enum { variants })
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
                self.parse_type_expr()?
            }
            _ => TypeExpr {
                id: self.id_gen.new_id(),
                kind: TypeExprKind::Named("()".to_string()),
                span: self.close(self.mark()),
            },
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
            let typ = parser.parse_type_expr()?;
            Ok(FunctionParam {
                id: parser.id_gen.new_id(),
                name,
                typ,
            })
        })
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        // Parse base type (named or tuple)
        let mut typ = if self.curr_token.kind == TK::LParen
            && self.peek().map(|t| &t.kind) != Some(&TK::RParen)
        {
            self.advance();
            self.parse_tuple_type()?
        } else {
            self.parse_named_type()?
        };

        // Check for array type
        if self.curr_token.kind == TK::LBracket {
            typ = self.parse_array_type(typ)?;
        }

        Ok(typ)
    }

    fn parse_named_type(&mut self) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();

        let result = match &self.curr_token.kind {
            // Special case for unit type
            TK::LParen if self.peek().map(|t| &t.kind) == Some(&TK::RParen) => {
                self.advance();
                Ok(TypeExpr {
                    id: self.id_gen.new_id(),
                    kind: TypeExprKind::Named("()".to_string()),
                    span: self.close(marker),
                })
            }
            // General case for other types
            TK::Ident(name) => Ok(TypeExpr {
                id: self.id_gen.new_id(),
                kind: TypeExprKind::Named(name.clone()),
                span: self.close(marker),
            }),
            _ => Err(ParseError::ExpectedType(self.curr_token.clone())),
        };
        result.inspect(|_| self.advance())
    }

    fn parse_tuple_type(&mut self) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();

        let mut fields = Vec::new();

        // Parse at least one field
        fields.push(self.parse_type_expr()?);

        // Parse remaining fields
        while self.curr_token.kind == TK::Comma {
            self.advance();
            // After a comma, require another type, else trailing comma is not permitted
            if self.curr_token.kind == TK::RParen {
                break;
            }
            fields.push(self.parse_type_expr()?);
        }

        self.consume(&TK::RParen)?;

        // Forbid (T) syntax (single type, no trailing comma) to avoid ambiguity with parenthesized type
        if fields.len() == 1 {
            return Err(ParseError::SingleFieldTupleMissingComma(
                self.curr_token.clone(),
            ));
        }

        Ok(TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Tuple { fields },
            span: self.close(marker),
        })
    }

    fn parse_array_type(&mut self, elem_ty: TypeExpr) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();

        self.advance(); // consume '['
        let dims = self.parse_list(TK::Comma, TK::RBracket, |parser| parser.parse_int_lit())?;
        self.consume(&TK::RBracket)?; // consume ']'

        Ok(TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Array {
                elem_ty: Box::new(elem_ty),
                dims: dims.into_iter().map(|d| d as usize).collect(),
            },
            span: self.close(marker),
        })
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

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let marker = self.mark();

        match &self.curr_token.kind {
            TK::Ident(name) if self.peek().map(|t| &t.kind) == Some(&TK::LBrace) => {
                // Struct pattern: Ident { ... }
                self.advance();
                let name = name.clone();
                self.parse_struct_pattern(marker, name)
            }
            TK::Ident(name) => {
                // Identifier pattern
                let name = name.clone();
                let span = self.curr_token.span;
                self.advance();
                Ok(Pattern {
                    id: self.id_gen.new_id(),
                    kind: PatternKind::Ident { name },
                    span,
                })
            }
            TK::LBracket => {
                // Array pattern
                self.advance();
                let patterns =
                    self.parse_list(TK::Comma, TK::RBracket, |parser| parser.parse_pattern())?;
                self.consume(&TK::RBracket)?;
                Ok(Pattern {
                    id: self.id_gen.new_id(),
                    kind: PatternKind::Array { patterns },
                    span: self.close(marker),
                })
            }
            TK::LParen => {
                // Tuple pattern (or just a parenthesized pattern)
                self.advance();

                let first_pattern = self.parse_pattern()?;

                if self.curr_token.kind != TK::Comma {
                    // Just a parenthesized pattern
                    self.consume(&TK::RParen)?;
                    return Ok(first_pattern);
                }

                // It's a tuple pattern - collect remaining patterns
                let mut patterns = vec![first_pattern];
                while self.curr_token.kind == TK::Comma {
                    self.advance();
                    // Allow trailing comma
                    if self.curr_token.kind == TK::RParen {
                        break;
                    }
                    let pattern = self.parse_pattern()?;
                    patterns.push(pattern);
                }
                self.consume(&TK::RParen)?;

                Ok(Pattern {
                    id: self.id_gen.new_id(),
                    kind: PatternKind::Tuple { patterns },
                    span: self.close(marker),
                })
            }
            _ => Err(ParseError::ExpectedPattern(self.curr_token.clone())),
        }
    }

    fn parse_struct_pattern(
        &mut self,
        marker: Marker,
        name: String,
    ) -> Result<Pattern, ParseError> {
        // Struct pattern fields are parsed as either:
        // `name: <pattern>` (explicit) or
        // `name` (shorthand)

        self.consume(&TK::LBrace)?; // consume '{'

        // Parse fields
        let fields = self.parse_list(TK::Comma, TK::RBrace, |parser| {
            let field_marker = parser.mark();

            // Parse field name
            let name = parser.parse_ident()?;

            let pattern = if parser.curr_token.kind == TK::Colon {
                // Explicit field pattern
                parser.advance();
                parser.parse_pattern()?
            } else {
                // Shorthand field pattern (expand to `Ident { name }` pattern)
                Pattern {
                    id: parser.id_gen.new_id(),
                    kind: PatternKind::Ident { name: name.clone() },
                    span: parser.close(field_marker.clone()),
                }
            };
            Ok(StructPatternField {
                name,
                pattern,
                span: parser.close(field_marker),
            })
        })?;

        self.consume(&TK::RBrace)?; // consume '}'

        Ok(Pattern {
            id: self.id_gen.new_id(),
            kind: PatternKind::Struct { name, fields },
            span: self.close(marker),
        })
    }

    fn parse_let(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        // Expect 'let'
        self.consume_keyword("let")?;
        let pattern = self.parse_pattern()?;

        // Parse declaration type (optional)
        let decl_ty = if self.curr_token.kind == TK::Colon {
            self.advance();
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        // Expect '='
        self.consume(&TK::Equals)?;

        // Parse value
        let value = self.parse_expr(0)?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::LetBind {
                pattern,
                decl_ty,
                value: Box::new(value),
            },
            span: self.close(marker),
        })
    }

    fn parse_var(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        // Expect 'var'
        self.consume_keyword("var")?;

        // Parse pattern
        let pattern = self.parse_pattern()?;

        // Parse declaration type (optional)
        let decl_ty = if self.curr_token.kind == TK::Colon {
            self.advance();
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        self.consume(&TK::Equals)?;

        // Parse value
        let value = self.parse_expr(0)?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::VarBind {
                pattern,
                decl_ty,
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
                    // ArrayIndex expression
                    self.advance(); // consume '['

                    let indices =
                        self.parse_list(TK::Comma, TK::RBracket, |parser| parser.parse_expr(0))?;

                    self.consume(&TK::RBracket)?; // consume ']'

                    expr = Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::ArrayIndex {
                            target: Box::new(expr),
                            indices,
                        },
                        span: self.close(marker.clone()),
                    };
                }
                TK::Dot => {
                    // Field access expression
                    self.consume(&TK::Dot)?;

                    match &self.curr_token.kind {
                        TK::IntLit(index) => {
                            // Tuple field access: .0, .1, etc.
                            self.advance();
                            expr = Expr {
                                id: self.id_gen.new_id(),
                                kind: ExprKind::TupleField {
                                    target: Box::new(expr),
                                    index: *index as usize,
                                },
                                span: self.close(marker.clone()),
                            };
                        }
                        TK::Ident(name) => {
                            // Struct field access: .name
                            self.advance();
                            expr = Expr {
                                id: self.id_gen.new_id(),
                                kind: ExprKind::StructField {
                                    target: Box::new(expr),
                                    field: name.clone(),
                                },
                                span: self.close(marker.clone()),
                            };
                        }
                        _ => return Err(ParseError::ExpectedStructField(self.curr_token.clone())),
                    }
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
            TK::Ident(name) if self.peek().map(|t| &t.kind) == Some(&TK::DoubleColon) => {
                // Enum variant: Ident :: Variant
                let marker = self.mark();
                let enum_name = name.clone();
                self.advance(); // ident
                self.consume(&TK::DoubleColon)?;
                let variant = self.parse_ident()?;
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::EnumVariant { enum_name, variant },
                    span: self.close(marker),
                })
            }
            TK::Ident(name) => {
                let marker = self.mark();
                let name = name.clone();
                self.advance();

                match name.as_str() {
                    "true" => Ok(Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::BoolLit(true),
                        span: self.close(marker),
                    }),
                    "false" => Ok(Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::BoolLit(false),
                        span: self.close(marker),
                    }),
                    _ => {
                        // Check for struct literal: Ident { ....}
                        if self.curr_token.kind == TK::LBrace {
                            self.parse_struct_lit(marker, name)
                        } else {
                            // Regular variable reference
                            Ok(Expr {
                                id: self.id_gen.new_id(),
                                kind: ExprKind::Var(name.clone()),
                                span: self.close(marker),
                            })
                        }
                    }
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
            TK::LParen => self.parse_paren_or_tuple(),
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

    fn parse_struct_lit(&mut self, marker: Marker, name: String) -> Result<Expr, ParseError> {
        self.consume(&TK::LBrace)?; // consume '{'

        // Parse struct literal fields
        let fields = self.parse_list(TK::Comma, TK::RBrace, |parser| {
            let field_marker = parser.mark();
            // Parse field name
            let name = parser.parse_ident()?;

            // Expect ':'
            parser.consume(&TK::Colon)?;

            // Parse field value
            let value = parser.parse_expr(0)?;

            Ok(StructLitField {
                id: parser.id_gen.new_id(),
                name,
                value,
                span: parser.close(field_marker),
            })
        })?;

        self.consume(&TK::RBrace)?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::StructLit { name, fields },
            span: self.close(marker),
        })
    }

    fn parse_paren_or_tuple(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();
        self.advance();

        // Case 1: Unit literal ()
        if self.curr_token.kind == TK::RParen {
            let span = self.close(marker);
            self.advance();
            return Ok(Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::UnitLit,
                span,
            });
        }

        // Parse first expression
        let first_expr = self.parse_expr(0)?;

        // Case 2: Tuple literal (has comma)
        if self.curr_token.kind == TK::Comma {
            let mut fields = vec![first_expr];
            while self.curr_token.kind == TK::Comma {
                self.advance();
                // Allow trailing comma
                if self.curr_token.kind == TK::RParen {
                    break;
                }
                let field = self.parse_expr(0)?;
                fields.push(field);
            }
            self.consume(&TK::RParen)?;
            return Ok(Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::TupleLit(fields),
                span: self.close(marker),
            });
        }

        // Case 3: Parenthesized expression (no comma)
        self.consume(&TK::RParen)?;
        Ok(first_expr)
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

    fn parse_decl(&mut self) -> Result<Decl, ParseError> {
        match &self.curr_token.kind {
            TK::Ident(name) if name == "type" => self.parse_type_decl().map(Decl::TypeDecl),
            TK::Ident(name) if name == "fn" => self.parse_function().map(Decl::Function),
            _ => Err(ParseError::ExpectedDecl(self.curr_token.clone())),
        }
    }

    pub fn parse(&mut self) -> Result<Module, ParseError> {
        let mut decls = Vec::new();
        while self.curr_token.kind != TK::Eof {
            decls.push(self.parse_decl()?);
        }
        Ok(Module { decls })
    }
}

#[cfg(test)]
#[path = "tests/t_parser.rs"]
mod tests;
