use thiserror::Error;

use crate::ast::NodeIdGen;
use crate::ast::*;
use crate::diag::{Position, Span};
use crate::lexer::Lexer;
use crate::lexer::{LexError, Token, TokenKind, TokenKind as TK};
use crate::types::is_builtin_type_name;

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

    #[error("Expected match arm, found: {0}")]
    ExpectedMatchArm(Token),

    #[error("Expected match pattern, found: {0}")]
    ExpectedMatchPattern(Token),

    #[error("Expected array index or slice range, found: {0}")]
    ExpectedArrayIndexOrRange(Token),

    #[error("Unmatched format brace at {0}")]
    UnmatchedFormatBrace(Span),

    #[error("Invalid format expression at {0}")]
    InvalidFormatExpr(Span),

    #[error("Empty format expression: {0}")]
    EmptyFormatExpr(Span),

    #[error("Unterminated format expression: {0}")]
    UnterminatedFormatExpr(Span),
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
            ParseError::ExpectedMatchArm(token) => token.span,
            ParseError::ExpectedMatchPattern(token) => token.span,
            ParseError::ExpectedArrayIndexOrRange(token) => token.span,
            ParseError::UnmatchedFormatBrace(span) => *span,
            ParseError::InvalidFormatExpr(span) => *span,
            ParseError::EmptyFormatExpr(span) => *span,
            ParseError::UnterminatedFormatExpr(span) => *span,
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
    allow_struct_lit: bool,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            pos: 0,
            curr_token: &tokens[0],
            id_gen: NodeIdGen::new(),
            allow_struct_lit: true,
        }
    }

    pub fn new_with_id_gen(tokens: &'a [Token], id_gen: NodeIdGen) -> Self {
        Parser {
            tokens,
            pos: 0,
            curr_token: &tokens[0],
            id_gen,
            allow_struct_lit: true,
        }
    }

    pub fn into_id_gen(self) -> NodeIdGen {
        self.id_gen
    }

    pub fn parse(&mut self) -> Result<Module, ParseError> {
        let mut decls = Vec::new();
        while self.curr_token.kind != TK::Eof {
            decls.push(self.parse_decl()?);
        }
        Ok(Module { decls })
    }

    // --- Top-level ---

    fn parse_decl(&mut self) -> Result<Decl, ParseError> {
        match &self.curr_token.kind {
            TK::Ident(name) if name == "type" => self.parse_type_decl().map(Decl::TypeDecl),
            TK::Ident(name) if name == "fn" => self.parse_func(),
            _ => Err(ParseError::ExpectedDecl(self.curr_token.clone())),
        }
    }

    // --- Type declarations ---

    fn parse_type_decl(&mut self) -> Result<TypeDecl, ParseError> {
        let marker = self.mark();

        // Expect 'type'
        self.consume_keyword("type")?;

        // Expect type name
        let name = self.parse_ident()?;
        self.consume(&TK::Equals)?;

        // Branch based on the next token
        let kind = if self.curr_token.kind == TK::LBrace {
            // Struct definition: type Foo = { ... }
            self.parse_struct_def()?
        } else if matches!(&self.curr_token.kind, TK::Ident(name) if name != "range")
            && matches!(
                self.peek().map(|t| &t.kind),
                Some(TK::Pipe) | Some(TK::LParen)
            )
        {
            // Enum definition: type Foo = Bar | Baz (or type Foo = Bar(...) | Baz(...))
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
            span: self.close(marker),
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

        fn parse_payload(parser: &mut Parser) -> Result<Vec<TypeExpr>, ParseError> {
            if parser.curr_token.kind == TK::LParen {
                parser.advance();
                let tys =
                    parser.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_type_expr())?;
                parser.consume(&TK::RParen)?;
                Ok(tys)
            } else {
                Ok(vec![])
            }
        }

        // Parse payload (if any)
        let payload = parse_payload(self)?;

        variants.push(EnumVariant {
            id: self.id_gen.new_id(),
            name,
            payload,
            span: self.close(marker),
        });

        // Parse remaining variants
        while self.curr_token.kind == TK::Pipe {
            self.advance(); // consume '|'
            let marker = self.mark();
            let name = self.parse_ident()?;
            let payload = parse_payload(self)?;
            variants.push(EnumVariant {
                id: self.id_gen.new_id(),
                name,
                payload,
                span: self.close(marker),
            });
        }

        // Consume optional ';'
        if self.curr_token.kind == TK::Semicolon {
            self.advance();
        }

        Ok(TypeDeclKind::Enum { variants })
    }

    // --- Type expressions ---

    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        let mut typ = self.parse_type_atom()?;

        // Check for array type suffix
        if self.curr_token.kind == TK::LBracket {
            typ = self.parse_array_type(typ)?;
        }

        Ok(typ)
    }

    fn parse_type_atom(&mut self) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();

        if self.curr_token.kind == TK::Caret {
            self.advance();
            let elem_ty = self.parse_type_atom()?;
            return Ok(TypeExpr {
                id: self.id_gen.new_id(),
                kind: TypeExprKind::Heap {
                    elem_ty: Box::new(elem_ty),
                },
                span: self.close(marker),
            });
        }

        // Check for range type first, then tuple, then named type
        if let TK::Ident(name) = &self.curr_token.kind
            && name == "range"
        {
            return self.parse_range_type();
        }

        if self.curr_token.kind == TK::LParen && self.peek().map(|t| &t.kind) != Some(&TK::RParen) {
            self.advance();
            return self.parse_tuple_type();
        }

        self.parse_named_type()
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

        if dims.is_empty() {
            // Slice type
            return Ok(TypeExpr {
                id: self.id_gen.new_id(),
                kind: TypeExprKind::Slice {
                    elem_ty: Box::new(elem_ty),
                },
                span: self.close(marker),
            });
        }

        // Array type
        Ok(TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Array {
                elem_ty: Box::new(elem_ty),
                dims: dims.into_iter().map(|d| d as usize).collect(),
            },
            span: self.close(marker),
        })
    }

    fn parse_range_type(&mut self) -> Result<TypeExpr, ParseError> {
        // Range Type: "range(min, max)" or "range(max)"
        let marker = self.mark();

        self.consume_keyword("range")?;
        self.consume(&TK::LParen)?;

        let first = self.parse_int_lit()?;
        let (min, max) = if self.curr_token.kind == TK::Comma {
            self.advance();
            let second = self.parse_int_lit()?;
            (first, second)
        } else {
            (0, first)
        };

        self.consume(&TK::RParen)?;

        Ok(TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Range { min, max },
            span: self.close(marker),
        })
    }

    // --- Functions ---

    fn parse_function_sig(&mut self) -> Result<FunctionSig, ParseError> {
        let marker = self.mark();

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

        Ok(FunctionSig {
            name,
            params,
            return_type,
            span: self.close(marker),
        })
    }

    fn parse_func_params(&mut self) -> Result<Vec<FunctionParam>, ParseError> {
        self.parse_list(TK::Comma, TK::RParen, |parser| {
            let marker = parser.mark();

            // Parse an optional param mode
            let mode = if matches!(&parser.curr_token.kind, TK::Ident(name) if name == "inout") {
                parser.advance();
                FunctionParamMode::Inout
            } else if matches!(&parser.curr_token.kind, TK::Ident(name) if name == "sink") {
                parser.advance();
                FunctionParamMode::Sink
            } else if matches!(&parser.curr_token.kind, TK::Ident(name) if name == "out") {
                parser.advance();
                FunctionParamMode::Out
            } else {
                FunctionParamMode::In
            };

            // Parse parameter name
            let name = parser.parse_ident()?;

            // Expect ':'
            parser.consume(&TK::Colon)?;

            // Parse parameter type
            let typ = parser.parse_type_expr()?;

            Ok(FunctionParam {
                id: parser.id_gen.new_id(),
                name,
                typ,
                mode,
                span: parser.close(marker),
            })
        })
    }

    fn parse_func(&mut self) -> Result<Decl, ParseError> {
        let marker = self.mark();

        // Parse function signature
        let sig = self.parse_function_sig()?;

        if self.curr_token.kind == TK::Semicolon {
            // Function declaration
            self.advance();
            Ok(Decl::FunctionDecl(FunctionDecl {
                id: self.id_gen.new_id(),
                sig,
                span: self.close(marker),
            }))
        } else {
            // Function definition
            let body = self.parse_block()?;
            Ok(Decl::Function(Function {
                id: self.id_gen.new_id(),
                sig,
                body,
                span: self.close(marker),
            }))
        }
    }

    // --- Blocks / Statement Expressions ---

    fn parse_block(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        // Consume '{'
        self.consume(&TK::LBrace)?;

        let mut items = Vec::new();
        let mut tail = None;

        // Parse block items
        while self.curr_token.kind != TK::RBrace {
            match &self.curr_token.kind {
                TK::Ident(name) if name == "let" => {
                    let stmt = self.parse_let()?;
                    items.push(BlockItem::Stmt(stmt));
                }
                TK::Ident(name) if name == "var" => {
                    let stmt = self.parse_var()?;
                    items.push(BlockItem::Stmt(stmt));
                }
                TK::Ident(name) if name == "while" => {
                    let stmt = self.parse_while()?;
                    items.push(BlockItem::Stmt(stmt));
                }
                TK::Ident(name) if name == "for" => {
                    let stmt = self.parse_for()?;
                    items.push(BlockItem::Stmt(stmt));
                }
                _ => {
                    let expr = self.parse_expr(0)?;
                    match self.curr_token.kind {
                        TK::Equals => {
                            // Assignment
                            let stmt = self.parse_assign(expr)?;
                            items.push(BlockItem::Stmt(stmt));
                        }
                        TK::Semicolon => {
                            // Expression with trailing semicolon
                            self.advance();
                            items.push(BlockItem::Expr(expr));
                        }
                        _ => {
                            // No semicolon -> last expression in the block
                            tail = Some(Box::new(expr));
                            break;
                        }
                    }
                }
            }
        }

        // Consume '}'
        self.consume(&TK::RBrace)?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Block { items, tail },
            span: self.close(marker),
        })
    }

    fn parse_let(&mut self) -> Result<StmtExpr, ParseError> {
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

        // Consume ';'
        self.consume(&TK::Semicolon)?;

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind: StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value: Box::new(value),
            },
            span: self.close(marker),
        })
    }

    fn parse_var(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        // Expect 'var'
        self.consume_keyword("var")?;

        let is_binding = self.lookahead_for(TK::Equals, TK::Semicolon);

        if is_binding {
            // var <pattern> (":" <type>)? = <expr>;
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

            // Consume ';'
            self.consume(&TK::Semicolon)?;

            Ok(StmtExpr {
                id: self.id_gen.new_id(),
                kind: StmtExprKind::VarBind {
                    pattern,
                    decl_ty,
                    value: Box::new(value),
                },
                span: self.close(marker),
            })
        } else {
            // var <name> ":" <type>;
            let name = self.parse_ident()?;

            // Expect ':'
            self.consume(&TK::Colon)?;

            // Parse type
            let decl_ty = self.parse_type_expr()?;

            // Consume ';'
            self.consume(&TK::Semicolon)?;

            Ok(StmtExpr {
                id: self.id_gen.new_id(),
                kind: StmtExprKind::VarDecl { name, decl_ty },
                span: self.close(marker),
            })
        }
    }

    fn parse_assign(&mut self, assignee: Expr) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        // Consume '='
        self.consume(&TK::Equals)?;

        // Parse value
        let value = self.parse_expr(0)?;

        // Consume ';'
        self.consume(&TK::Semicolon)?;

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind: StmtExprKind::Assign {
                assignee: Box::new(assignee),
                value: Box::new(value),
            },
            span: self.close(marker),
        })
    }

    fn parse_while(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        // Consume 'while'
        self.consume_keyword("while")?;

        // Parse condition
        let cond = self.parse_expr(0)?;

        // Parse body
        let body = self.parse_block()?;

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind: StmtExprKind::While {
                cond: Box::new(cond),
                body: Box::new(body),
            },
            span: self.close(marker),
        })
    }

    fn parse_for(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        // Consume 'for'
        self.consume_keyword("for")?;

        // Parse pattern
        let pattern = self.parse_pattern()?;

        // Consume 'in'
        self.consume_keyword("in")?;

        // Parse range literal or general iterator expression
        // (disallow struct literals to avoid ambiguity with the loop body block)
        self.allow_struct_lit = false;
        let iter = if matches!(self.curr_token.kind, TK::IntLit(_))
            && matches!(self.peek().map(|t| &t.kind), Some(TK::DotDot))
        {
            self.parse_range_expr()?
        } else {
            self.parse_expr(0)?
        };
        self.allow_struct_lit = true;

        // Parse body
        let body = self.parse_block()?;

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind: StmtExprKind::For {
                pattern,
                iter: Box::new(iter),
                body: Box::new(body),
            },
            span: self.close(marker),
        })
    }

    // --- Patterns ---

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

    // --- Control flow (If) ---

    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        // Expect 'if'
        self.consume_keyword("if")?;

        // Parse condition
        // (disallow struct literals to avoid ambiguity between struct literals and blocks)
        self.allow_struct_lit = false;
        let cond = self.parse_expr(0)?;
        self.allow_struct_lit = true;

        // Parse then body
        let then_body = if self.curr_token.kind == TK::LBrace {
            self.parse_block()?
        } else {
            self.parse_expr(0)?
        };

        // Expect 'else'
        self.consume_keyword("else")?;

        // Parse else body
        let else_body = if self.curr_token.kind == TK::LBrace {
            self.parse_block()?
        } else {
            self.parse_expr(0)?
        };

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

    fn parse_range_expr(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        let start = self.parse_int_lit()?;
        self.consume(&TK::DotDot)?;
        let end = self.parse_int_lit()?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Range { start, end },
            span: self.close(marker),
        })
    }

    // --- Control flow (Match) ---

    fn parse_match_expr(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        // Expect 'match'
        self.consume_keyword("match")?;

        // Parse scrutinee
        // (disallow struct literals to avoid ambiguity between struct literals and match arms)
        self.allow_struct_lit = false;
        let scrutinee = self.parse_expr(0)?;
        self.allow_struct_lit = true;

        // Expect '{'
        self.consume(&TK::LBrace)?;

        // Parse match arms
        let arms = self.parse_list(TK::Comma, TK::RBrace, |parser| parser.parse_match_arm())?;
        if arms.is_empty() {
            return Err(ParseError::ExpectedMatchArm(self.curr_token.clone()));
        }

        // Expect '}'
        self.consume(&TK::RBrace)?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            },
            span: self.close(marker),
        })
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
        let marker = self.mark();

        // Parse match pattern
        let pattern = self.parse_match_pattern()?;

        // Expect '=>'
        self.consume(&TK::FatArrow)?;

        // Parse match body
        let body = self.parse_expr(0)?;

        Ok(MatchArm {
            id: self.id_gen.new_id(),
            pattern,
            body,
            span: self.close(marker),
        })
    }

    fn parse_match_pattern(&mut self) -> Result<MatchPattern, ParseError> {
        let marker = self.mark();

        // Case 1: Wildcard
        if self.curr_token.kind == TK::Underscore {
            self.advance();
            return Ok(MatchPattern::Wildcard {
                span: self.close(marker),
            });
        }

        // Case 2: Enum variant (either Enum::Variant or Variant)
        if !matches!(self.curr_token.kind, TK::Ident(_)) {
            return Err(ParseError::ExpectedMatchPattern(self.curr_token.clone()));
        }

        let mut enum_name = None;
        let mut variant_name = self.parse_ident()?;

        if self.curr_token.kind == TK::DoubleColon {
            self.advance();
            enum_name = Some(variant_name);
            variant_name = self.parse_ident()?;
        }

        // Parse bindings (if any)
        let mut bindings = vec![];
        if self.curr_token.kind == TK::LParen {
            self.advance();
            bindings = self.parse_list(TK::Comma, TK::RParen, |parser| {
                let marker = parser.mark();
                let name = parser.parse_ident()?;
                Ok(MatchPatternBinding {
                    id: parser.id_gen.new_id(),
                    name,
                    span: parser.close(marker),
                })
            })?;
            self.consume(&TK::RParen)?;
        }

        Ok(MatchPattern::EnumVariant {
            enum_name,
            variant_name,
            bindings,
            span: self.close(marker),
        })
    }

    // --- Expressions ---

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let marker = self.mark();
        let mut lhs = if let TK::Ident(name) = &self.curr_token.kind
            && name == "move"
        {
            // Unary move
            self.advance();
            let operand = self.parse_expr(10)?;
            Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::Move {
                    expr: Box::new(operand),
                },
                span: self.close(marker.clone()),
            }
        } else if self.curr_token.kind == TK::Minus {
            // Unary minus
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
        } else if self.curr_token.kind == TK::LogicalNot {
            // Unary logical not
            self.advance();
            let operand = self.parse_expr(10)?; // highest binding power
            Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::UnaryOp {
                    op: UnaryOp::LogicalNot,
                    expr: Box::new(operand),
                },
                span: self.close(marker.clone()),
            }
        } else if self.curr_token.kind == TK::Tilde {
            // Unary bitwise not
            self.advance();
            let operand = self.parse_expr(10)?; // highest binding power
            Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::UnaryOp {
                    op: UnaryOp::BitNot,
                    expr: Box::new(operand),
                },
                span: self.close(marker.clone()),
            }
        } else if self.curr_token.kind == TK::Caret {
            // Heap allocation
            self.advance();
            let operand = self.parse_expr(10)?; // highest binding power
            Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::HeapAlloc {
                    expr: Box::new(operand),
                },
                span: self.close(marker.clone()),
            }
        } else {
            // Postfix expression
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

    fn parse_call_arg(&mut self) -> Result<CallArg, ParseError> {
        let marker = self.mark();
        let mode = if let TK::Ident(name) = &self.curr_token.kind {
            match name.as_str() {
                "inout" => {
                    self.advance();
                    CallArgMode::Inout
                }
                "out" => {
                    self.advance();
                    CallArgMode::Out
                }
                "move" => {
                    self.advance();
                    CallArgMode::Move
                }
                _ => CallArgMode::Default,
            }
        } else {
            CallArgMode::Default
        };

        let expr = self.parse_expr(0)?;

        Ok(CallArg {
            mode,
            expr,
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

    fn parse_ident(&mut self) -> Result<String, ParseError> {
        if let TK::Ident(name) = &self.curr_token.kind {
            self.advance();
            Ok(name.clone())
        } else {
            Err(ParseError::ExpectedIdent(self.curr_token.clone()))
        }
    }

    // --- Formatted string ---

    fn parse_fstring(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        // Consume 'f'
        self.advance();

        // Read the string literal token
        let TK::StringLit(value) = &self.curr_token.kind else {
            unreachable!("");
        };

        let span = self.curr_token.span;
        let value = value.clone();
        self.advance();

        let segments = self.parse_fstring_segments(&value, span)?;

        if let Some(value) = Self::fold_constant_fstring(&segments) {
            return Ok(Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::StringLit { value },
                span: self.close(marker),
            });
        }

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::StringFmt { segments },
            span: self.close(marker),
        })
    }

    fn parse_fstring_segments(
        &mut self,
        value: &String,
        span: Span,
    ) -> Result<Vec<StringFmtSegment>, ParseError> {
        let mut segments = vec![];
        let mut literal = String::new();
        let mut literal_start: Option<Position> = None;
        let mut i = 0;
        let chars: Vec<char> = value.chars().collect();
        let mut pos = Self::advance_position(span.start, '"');

        while i < chars.len() {
            match chars[i] {
                '{' => {
                    if i + 1 < chars.len() && chars[i + 1] == '{' {
                        if literal_start.is_none() {
                            literal_start = Some(pos);
                        }
                        literal.push('{');
                        pos = Self::advance_position(pos, '{');
                        pos = Self::advance_position(pos, '{');
                        i += 2;
                        continue;
                    }
                    // Start of {expr}
                    if !literal.is_empty() {
                        let start = literal_start.unwrap_or(pos);
                        segments.push(StringFmtSegment::Literal {
                            value: literal.clone(),
                            span: Span::new(start, pos),
                        });
                        literal.clear();
                        literal_start = None;
                    }
                    let (expr_src, consumed) = Self::extract_fmt_expr(&chars[i + 1..], span)?;
                    let expr_end = Self::advance_position_by(pos, &chars[i..i + consumed + 1]);
                    let expr_span = Span::new(pos, expr_end);
                    let expr = self.parse_expr_from_str(&expr_src, expr_span)?;
                    segments.push(StringFmtSegment::Expr {
                        expr: Box::new(expr),
                        span: expr_span,
                    });
                    pos = expr_end;
                    i += consumed + 1;
                }
                '}' => {
                    if i + 1 < chars.len() && chars[i + 1] == '}' {
                        if literal_start.is_none() {
                            literal_start = Some(pos);
                        }
                        literal.push('}');
                        pos = Self::advance_position(pos, '}');
                        pos = Self::advance_position(pos, '}');
                        i += 2;
                    } else {
                        return Err(ParseError::UnmatchedFormatBrace(span));
                    }
                }
                ch => {
                    if literal_start.is_none() {
                        literal_start = Some(pos);
                    }
                    literal.push(ch);
                    pos = Self::advance_position(pos, ch);
                    i += 1;
                }
            }
        }

        if !literal.is_empty() {
            let start = literal_start.unwrap_or(pos);
            segments.push(StringFmtSegment::Literal {
                value: literal.clone(),
                span: Span::new(start, pos),
            });
        }

        Ok(segments)
    }

    fn fold_constant_fstring(segments: &[StringFmtSegment]) -> Option<String> {
        let mut out = String::new();
        for segment in segments {
            match segment {
                StringFmtSegment::Literal { value, .. } => out.push_str(value),
                StringFmtSegment::Expr { expr, .. } => match &expr.kind {
                    ExprKind::StringLit { value, .. } => out.push_str(value),
                    _ => return None,
                },
            }
        }
        Some(out)
    }

    fn advance_position(mut pos: Position, ch: char) -> Position {
        pos.offset += 1;
        if ch == '\n' {
            pos.line += 1;
            pos.column = 1;
        } else {
            pos.column += 1;
        }
        pos
    }

    fn advance_position_by(mut pos: Position, chars: &[char]) -> Position {
        for ch in chars {
            pos = Self::advance_position(pos, *ch);
        }
        pos
    }

    fn parse_expr_from_str(&mut self, src: &str, span: Span) -> Result<Expr, ParseError> {
        // Use a temporary lexer and sub-parser to parse the expression
        let tokens = Lexer::new(src)
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .map_err(|_| ParseError::InvalidFormatExpr(span))?;

        // Use the same id_gen as the main parser to ensure consistent node ids
        let id_gen = std::mem::take(&mut self.id_gen);

        // Create a new parser with the same id_gen
        let mut sub_parser = Parser::new_with_id_gen(&tokens, id_gen);
        let expr = sub_parser.parse_expr(0);
        let has_trailing = sub_parser.curr_token.kind != TK::Eof;

        // Always restore the id_gen, even on error
        self.id_gen = sub_parser.into_id_gen();

        let expr = expr?;

        // Ensure no trailing tokens
        if has_trailing {
            return Err(ParseError::InvalidFormatExpr(span));
        }

        Ok(expr)
    }

    // input: chars slice after the opening '{'
    fn extract_fmt_expr(chars: &[char], span: Span) -> Result<(String, usize), ParseError> {
        let mut depth = 0;
        let mut in_string = false;
        let mut in_char = false;
        let mut escape = false;

        for (i, ch) in chars.iter().enumerate() {
            if in_string {
                if escape {
                    escape = false;
                    continue;
                }
                if *ch == '\\' {
                    escape = true;
                    continue;
                }
                if *ch == '"' {
                    in_string = false;
                }
                continue;
            }
            if in_char {
                if escape {
                    escape = false;
                    continue;
                }
                if *ch == '\\' {
                    escape = true;
                    continue;
                }
                if *ch == '\'' {
                    in_char = false;
                }
                continue;
            }

            match *ch {
                '"' => in_string = true,
                '\'' => in_char = true,
                '{' => depth += 1,
                '}' => {
                    if depth == 0 {
                        if i == 0 {
                            return Err(ParseError::EmptyFormatExpr(span));
                        }
                        let expr = chars[..i].iter().collect::<String>();
                        return Ok((expr, i + 1)); // consumed includes the closing '}'
                    }
                    depth -= 1;
                }
                _ => {}
            }
        }

        Err(ParseError::UnterminatedFormatExpr(span))
    }

    // --- Binary operators ---

    fn bin_op_from_token(token: &TokenKind) -> Option<(BinaryOp, u8)> {
        match token {
            // 0 = Logical OR
            TK::LogicalOr => Some((BinaryOp::LogicalOr, 0)),

            // 1 = Logical AND
            TK::LogicalAnd => Some((BinaryOp::LogicalAnd, 1)),

            // 2 = Bitwise OR
            TK::Pipe => Some((BinaryOp::BitOr, 2)),

            // 3 = Bitwise XOR
            TK::Caret => Some((BinaryOp::BitXor, 3)),

            // 4 = Bitwise AND
            TK::Ampersand => Some((BinaryOp::BitAnd, 4)),

            // 5 = Comparison
            TK::EqEq => Some((BinaryOp::Eq, 5)),
            TK::NotEq => Some((BinaryOp::Ne, 5)),
            TK::LessThan => Some((BinaryOp::Lt, 5)),
            TK::GreaterThan => Some((BinaryOp::Gt, 5)),
            TK::LessThanEq => Some((BinaryOp::LtEq, 5)),
            TK::GreaterThanEq => Some((BinaryOp::GtEq, 5)),

            // 6 = Shift
            TK::ShiftLeft => Some((BinaryOp::Shl, 6)),
            TK::ShiftRight => Some((BinaryOp::Shr, 6)),

            // 7 = Additive
            TK::Plus => Some((BinaryOp::Add, 7)),
            TK::Minus => Some((BinaryOp::Sub, 7)),

            // 8 = Multiplicative
            TK::Star => Some((BinaryOp::Mul, 8)),
            TK::Slash => Some((BinaryOp::Div, 8)),
            TK::Percent => Some((BinaryOp::Mod, 8)),

            _ => None,
        }
    }

    // --- Lookahead ---

    fn lookahead_for(&self, target: TokenKind, stop_at: TokenKind) -> bool {
        let mut brace = 0usize;
        let mut bracket = 0usize;
        let mut paren = 0usize;

        let mut idx = self.pos;
        while idx < self.tokens.len() {
            let token = &self.tokens[idx].kind;

            match token {
                TK::LBrace => brace += 1,
                TK::RBrace => {
                    brace = brace.saturating_sub(1);
                    if brace == 0 && stop_at == TK::RBrace {
                        return false;
                    }
                }
                TK::LBracket => bracket += 1,
                TK::RBracket => {
                    bracket = bracket.saturating_sub(1);
                    if bracket == 0 && stop_at == TK::RBracket {
                        return false;
                    }
                }
                TK::LParen => paren += 1,
                TK::RParen => {
                    paren = paren.saturating_sub(1);
                    if paren == 0 && stop_at == TK::RParen {
                        return false;
                    }
                }
                _ => {}
            }

            let at_target_level = match stop_at {
                TK::RBrace => brace == 1 && bracket == 0 && paren == 0,
                TK::RBracket => brace == 0 && bracket == 1 && paren == 0,
                TK::RParen => brace == 0 && bracket == 0 && paren == 1,
                _ => brace == 0 && bracket == 0 && paren == 0,
            };

            // Found target at target level -> success
            if *token == target && at_target_level {
                return true;
            }

            // Found stop_at at top level -> failure
            if *token == stop_at && brace == 0 && bracket == 0 && paren == 0 {
                return false;
            }

            idx += 1;
        }

        false
    }

    // --- Postfix operators ---

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();
        let mut expr = self.parse_primary()?;

        loop {
            match self.curr_token.kind {
                TK::LParen => {
                    // Call expression
                    self.advance();
                    let args =
                        self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_call_arg())?;
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
                    // ArrayIndex or Slice expression

                    let is_slice = self.lookahead_for(TK::DotDot, TK::RBracket);
                    self.advance(); // consume '['

                    // Check if no index or range is provided
                    if self.curr_token.kind == TK::RBracket {
                        return Err(ParseError::ExpectedArrayIndexOrRange(
                            self.curr_token.clone(),
                        ));
                    }

                    if is_slice {
                        // Slice expression
                        let mut start = None;
                        let mut end = None;

                        // Parse start (if any)
                        if self.curr_token.kind != TK::DotDot {
                            start = Some(Box::new(self.parse_expr(0)?));
                        }

                        // Consume '..'
                        self.consume(&TK::DotDot)?;

                        // Parse end (if any)
                        if self.curr_token.kind != TK::RBracket {
                            end = Some(Box::new(self.parse_expr(0)?));
                        }

                        self.consume(&TK::RBracket)?; // consume ']'

                        expr = Expr {
                            id: self.id_gen.new_id(),
                            kind: ExprKind::Slice {
                                target: Box::new(expr),
                                start,
                                end,
                            },
                            span: self.close(marker.clone()),
                        };
                    } else {
                        // ArrayIndex expression
                        let indices = self
                            .parse_list(TK::Comma, TK::RBracket, |parser| parser.parse_expr(0))?;

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

    // --- Primary expressions ---

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match &self.curr_token.kind {
            TK::IntLit(value) => {
                let span = self.curr_token.span;
                self.advance();
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::IntLit(*value),
                    span,
                })
            }

            TK::CharLit(value) => {
                let span = self.curr_token.span;
                self.advance();
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::CharLit(*value),
                    span,
                })
            }

            TK::StringLit(s) => {
                let span = self.curr_token.span;
                self.advance();
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::StringLit { value: s.clone() },
                    span,
                })
            }

            // Formatted string
            TK::Ident(name)
                if name == "f"
                    && matches!(self.peek().map(|t| &t.kind), Some(TK::StringLit(_))) =>
            {
                self.parse_fstring()
            }

            // Typed array literal
            TK::Ident(name)
                if is_builtin_type_name(name)
                    && self.peek().map(|t| &t.kind) == Some(&TK::LBracket) =>
            {
                self.advance(); // consume ident
                self.parse_typed_array_lit(name.clone())
            }

            // Boolean literal
            TK::Ident(name) if name == "true" || name == "false" => {
                let span = self.curr_token.span;
                let value = name == "true";
                self.advance();
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::BoolLit(value),
                    span,
                })
            }

            // If expression
            TK::Ident(name) if name == "if" => self.parse_if(),

            // Match expression
            TK::Ident(name) if name == "match" => self.parse_match_expr(),

            // Enum variant expression
            TK::Ident(name) if self.peek().map(|t| &t.kind) == Some(&TK::DoubleColon) => {
                self.parse_enum_variant(name.clone())
            }

            // Struct literal
            TK::Ident(name)
                if self.allow_struct_lit && self.peek().map(|t| &t.kind) == Some(&TK::LBrace) =>
            {
                self.parse_struct_lit(name.clone())
            }

            // Variable reference
            TK::Ident(name) => {
                let marker = self.mark();
                self.advance();

                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::Var(name.clone()),
                    span: self.close(marker),
                })
            }

            // Parenthesized expression or tuple literal
            TK::LParen => self.parse_paren_or_tuple(),

            // Struct update expression
            TK::LBrace if self.lookahead_for(TK::Pipe, TK::RBrace) => self.parse_struct_update(),

            // Block expression
            TK::LBrace => self.parse_block(),

            // Array literal expression
            TK::LBracket => self.parse_array_lit(),

            _ => Err(ParseError::ExpectedPrimary(self.curr_token.clone())),
        }
    }

    // --- Literals ---

    fn parse_int_lit(&mut self) -> Result<u64, ParseError> {
        if let TK::IntLit(value) = &self.curr_token.kind {
            self.advance();
            Ok(*value)
        } else {
            Err(ParseError::ExpectedIntLit(self.curr_token.clone()))
        }
    }

    fn parse_array_lit(&mut self) -> Result<Expr, ParseError> {
        self.parse_array_lit_common(None)
    }

    fn parse_typed_array_lit(&mut self, type_name: String) -> Result<Expr, ParseError> {
        let marker = self.mark();

        // Create the named type expression for the element type
        let elem_ty = TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Named(type_name),
            span: self.close(marker.clone()),
        };

        self.parse_array_lit_common(Some(elem_ty))
    }

    fn parse_array_lit_common(&mut self, elem_ty: Option<TypeExpr>) -> Result<Expr, ParseError> {
        let marker = self.mark();

        // Consume '['
        self.advance();

        // Case 1: Empty array literal: []
        if self.curr_token.kind == TK::RBracket {
            self.advance();
            return Ok(Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::ArrayLit {
                    elem_ty,
                    init: ArrayLitInit::Elems(Vec::new()),
                },
                span: self.close(marker),
            });
        }

        let first = self.parse_expr(0)?;
        let init = if self.curr_token.kind == TK::Semicolon {
            // Case 2: Repeat initializer: [expr; count]

            // Consume ';'
            self.advance();

            // Parse count
            let count = self.parse_int_lit()?;

            // Consume ']'
            self.consume(&TK::RBracket)?;

            ArrayLitInit::Repeat(Box::new(first), count)
        } else {
            // Case 3: Elements initializer: [elem1, elem2, ...]
            let mut elems = vec![first];

            // Parse remaining elements
            if self.curr_token.kind == TK::Comma {
                self.advance();
                let rest = self.parse_list(TK::Comma, TK::RBracket, |p| p.parse_expr(0))?;
                elems.extend(rest);
            }

            // Consume ']'
            self.consume(&TK::RBracket)?;

            ArrayLitInit::Elems(elems)
        };

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::ArrayLit { elem_ty, init },
            span: self.close(marker),
        })
    }

    fn parse_enum_variant(&mut self, enum_name: String) -> Result<Expr, ParseError> {
        let marker = self.mark();
        self.advance(); // ident
        self.consume(&TK::DoubleColon)?;
        let variant = self.parse_ident()?;

        // Parse payload (if any)
        let payload = if self.curr_token.kind == TK::LParen {
            self.advance();
            let payload = self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_expr(0))?;
            self.consume(&TK::RParen)?;
            payload
        } else {
            vec![]
        };

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
            },
            span: self.close(marker),
        })
    }

    fn parse_struct_lit(&mut self, name: String) -> Result<Expr, ParseError> {
        let marker = self.mark();

        self.advance(); // consume struct name

        // Consume '{'
        self.consume(&TK::LBrace)?;

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

    fn parse_struct_update(&mut self) -> Result<Expr, ParseError> {
        // "{ base | field: expr, ... }"
        let marker = self.mark();

        // Consume opening brace
        self.consume(&TK::LBrace)?;

        // Parse base expression
        // Avoid consuming the struct update separator as a bitwise OR operator.
        let base = self.parse_expr(3)?;

        // Expect pipe
        self.consume(&TK::Pipe)?;

        // Parse field updates
        let fields = self.parse_list(TK::Comma, TK::RBrace, |parser| {
            let field_marker = parser.mark();
            let name = parser.parse_ident()?;
            parser.consume(&TK::Colon)?;
            let value = parser.parse_expr(0)?;
            Ok(StructUpdateField {
                id: parser.id_gen.new_id(),
                name,
                value,
                span: parser.close(field_marker),
            })
        })?;

        // Consume closing brace
        self.consume(&TK::RBrace)?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::StructUpdate {
                target: Box::new(base),
                fields,
            },
            span: self.close(marker),
        })
    }

    // --- Utility methods ---

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
}

#[cfg(test)]
#[path = "tests/t_parser.rs"]
mod tests;
