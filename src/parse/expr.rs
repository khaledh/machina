use super::*;
use crate::types::is_builtin_type_name;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum GenericLitKind {
    EnumVariant,
    StructLit,
}

impl<'a> Parser<'a> {
    /// Expression parsing (using Pratt parsing for operator precedence)
    pub(super) fn parse_expr(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let marker = self.mark();

        // Unary operators / keywords
        let mut lhs = if self.curr_token.kind == TK::Minus {
            // -<expr>
            self.advance();
            let operand = self.parse_expr(10)?;
            Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: Box::new(operand),
                },
                ty: (),
                span: self.close(marker),
            }
        } else if self.curr_token.kind == TK::LogicalNot {
            // !<expr>
            self.advance();
            let operand = self.parse_expr(10)?;
            Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::UnaryOp {
                    op: UnaryOp::LogicalNot,
                    expr: Box::new(operand),
                },
                ty: (),
                span: self.close(marker),
            }
        } else if self.curr_token.kind == TK::Tilde {
            // ~<expr>
            self.advance();
            let operand = self.parse_expr(10)?;
            Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::UnaryOp {
                    op: UnaryOp::BitNot,
                    expr: Box::new(operand),
                },
                ty: (),
                span: self.close(marker),
            }
        } else if self.curr_token.kind == TK::Caret {
            // ^<expr>
            self.advance();
            let operand = self.parse_expr(10)?;
            Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::HeapAlloc {
                    expr: Box::new(operand),
                },
                ty: (),
                span: self.close(marker),
            }
        } else if self.curr_token.kind == TK::KwMove {
            // move <expr>
            self.advance();
            let operand = self.parse_expr(10)?;
            Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::Move {
                    expr: Box::new(operand),
                },
                ty: (),
                span: self.close(marker),
            }
        } else {
            // <expr>
            self.parse_postfix()?
        };

        if self.allow_range_expr && matches!(self.curr_token.kind, TK::DotDot) {
            self.advance();
            let allow_range_expr = self.allow_range_expr;
            self.allow_range_expr = false;
            let end = self.parse_expr(0)?;
            self.allow_range_expr = allow_range_expr;
            lhs = Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::Range {
                    start: Box::new(lhs),
                    end: Box::new(end),
                },
                ty: (),
                span: self.close(marker),
            };
        }

        while let Some((op, bp)) = Self::token_to_binary_op(&self.curr_token.kind) {
            // Binary operators
            if bp < min_bp {
                break;
            }
            self.advance();

            // Parse rhs
            let rhs = self.parse_expr(bp + 1)?;

            lhs = Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::BinOp {
                    left: Box::new(lhs),
                    op,
                    right: Box::new(rhs),
                },
                ty: (),
                span: self.close(marker),
            };
        }

        Ok(lhs)
    }

    /// Maps a token to a binary operator and its precedence
    fn token_to_binary_op(token: &TokenKind) -> Option<(BinaryOp, u8)> {
        match token {
            TK::LogicalOr => Some((BinaryOp::LogicalOr, 0)),
            TK::LogicalAnd => Some((BinaryOp::LogicalAnd, 1)),
            TK::Pipe => Some((BinaryOp::BitOr, 2)),
            TK::Caret => Some((BinaryOp::BitXor, 3)),
            TK::Ampersand => Some((BinaryOp::BitAnd, 4)),
            TK::EqEq => Some((BinaryOp::Eq, 5)),
            TK::NotEq => Some((BinaryOp::Ne, 5)),
            TK::LessThan => Some((BinaryOp::Lt, 5)),
            TK::GreaterThan => Some((BinaryOp::Gt, 5)),
            TK::LessThanEq => Some((BinaryOp::LtEq, 5)),
            TK::GreaterThanEq => Some((BinaryOp::GtEq, 5)),
            TK::ShiftLeft => Some((BinaryOp::Shl, 6)),
            TK::ShiftRight => Some((BinaryOp::Shr, 6)),
            TK::Plus => Some((BinaryOp::Add, 7)),
            TK::Minus => Some((BinaryOp::Sub, 7)),
            TK::Star => Some((BinaryOp::Mul, 8)),
            TK::Slash => Some((BinaryOp::Div, 8)),
            TK::Percent => Some((BinaryOp::Mod, 8)),
            _ => None,
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();
        let mut expr = self.parse_primary()?;

        loop {
            let next = match self.curr_token.kind {
                TK::LParen => self.parse_call_postfix(expr, marker)?,
                TK::LBracket => self.parse_index_or_slice_postfix(expr, marker)?,
                TK::Dot => self.parse_dot_postfix(expr, marker)?,
                TK::Question => self.parse_try_postfix(expr, marker)?,
                _ => break,
            };
            expr = next;
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match &self.curr_token.kind {
            TK::IntLit(value) => {
                let span = self.curr_token.span;
                self.advance();
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::IntLit(*value),
                    ty: (),
                    span,
                })
            }

            TK::CharLit(value) => {
                let span = self.curr_token.span;
                self.advance();
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::CharLit(*value),
                    ty: (),
                    span,
                })
            }

            TK::StringLit(s) => {
                let span = self.curr_token.span;
                self.advance();
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::StringLit { value: s.clone() },
                    ty: (),
                    span,
                })
            }

            TK::Ident(name)
                if name == "f"
                    && matches!(self.peek().map(|t| &t.kind), Some(TK::StringLit(_))) =>
            {
                self.parse_fstring()
            }

            TK::Ident(name)
                if is_builtin_type_name(name)
                    && self.peek().map(|t| &t.kind) == Some(&TK::LBracket) =>
            {
                self.advance();
                self.parse_typed_array_lit(name.clone())
            }

            TK::BoolLit(value) => {
                let span = self.curr_token.span;
                self.advance();
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::BoolLit(*value),
                    ty: (),
                    span,
                })
            }

            TK::KwIf => self.parse_if(),

            TK::KwMatch => self.parse_match_expr(),

            TK::Ident(name) => {
                let marker = self.mark();

                if self.peek().map(|t| &t.kind) == Some(&TK::LessThan) {
                    if let Some(kind) = self.peek_generic_lit_kind() {
                        let (type_name, type_args) = self.parse_type_name_with_args()?;
                        return match kind {
                            GenericLitKind::EnumVariant => {
                                self.parse_enum_variant_with_args(marker, type_name, type_args)
                            }
                            GenericLitKind::StructLit => {
                                self.parse_struct_lit_with_args(marker, type_name, type_args)
                            }
                        };
                    }
                }

                if self.peek().map(|t| &t.kind) == Some(&TK::DoubleColon) {
                    if self.require_aliases.contains(name) {
                        let alias = self.parse_ident()?;
                        self.consume(&TK::DoubleColon)?;
                        let member = self.parse_ident()?;
                        return Ok(Expr {
                            id: self.id_gen.new_id(),
                            kind: ExprKind::Var {
                                ident: format!("{alias}::{member}"),
                                def_id: (),
                            },
                            ty: (),
                            span: self.close(marker),
                        });
                    }
                    let name = self.parse_ident()?;
                    return self.parse_enum_variant_with_args(marker, name, Vec::new());
                }

                if self.allow_struct_lit && self.peek().map(|t| &t.kind) == Some(&TK::LBrace) {
                    let name = self.parse_ident()?;
                    return self.parse_struct_lit_with_args(marker, name, Vec::new());
                }

                let name = name.clone();
                self.advance();

                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::Var {
                        ident: name,
                        def_id: (),
                    },
                    ty: (),
                    span: self.close(marker),
                })
            }
            TK::KwSet => {
                let marker = self.mark();
                if self.peek().map(|t| &t.kind) == Some(&TK::LessThan)
                    && let Some(GenericLitKind::StructLit) = self.peek_generic_lit_kind()
                {
                    let (_type_name, type_args) = self.parse_type_name_with_args()?;
                    return self.parse_typed_set_lit_with_args(marker, type_args);
                }
                Err(ParseError::ExpectedPrimary(self.curr_token.clone()))
            }
            TK::KwMap => {
                let marker = self.mark();
                if self.peek().map(|t| &t.kind) == Some(&TK::LessThan)
                    && let Some(GenericLitKind::StructLit) = self.peek_generic_lit_kind()
                {
                    let (_type_name, type_args) = self.parse_type_name_with_args()?;
                    return self.parse_typed_map_lit_with_args(marker, type_args);
                }
                Err(ParseError::ExpectedPrimary(self.curr_token.clone()))
            }
            TK::KwSelf => {
                let marker = self.mark();
                self.advance();

                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::Var {
                        ident: "self".to_string(),
                        def_id: (),
                    },
                    ty: (),
                    span: self.close(marker),
                })
            }

            TK::LParen => self.parse_paren_or_tuple(),

            TK::LBrace if self.lookahead_for(TK::Pipe, TK::RBrace) => self.parse_struct_update(),

            TK::LBrace if self.lookahead_for(TK::Colon, TK::RBrace) => {
                self.parse_map_lit(None, None)
            }

            TK::LBrace if self.lookahead_for(TK::Comma, TK::RBrace) => self.parse_set_lit(None),

            TK::LBrace => self.parse_block(),

            TK::Pipe | TK::LogicalOr => self.parse_closure(),

            TK::LBracket if self.is_closure_capture_list() => self.parse_closure(),

            TK::LBracket => self.parse_array_lit(),

            _ => Err(ParseError::ExpectedPrimary(self.curr_token.clone())),
        }
    }

    fn parse_call_postfix(&mut self, expr: Expr, marker: Marker) -> Result<Expr, ParseError> {
        self.advance();
        let args = self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_call_arg())?;
        self.consume(&TK::RParen)?;
        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Call {
                callee: Box::new(expr),
                args,
            },
            ty: (),
            span: self.close(marker),
        })
    }

    fn parse_call_arg(&mut self) -> Result<CallArg, ParseError> {
        let marker = self.mark();
        let mode = match &self.curr_token.kind {
            TK::KwInOut => {
                self.advance();
                CallArgMode::InOut
            }
            TK::KwOut => {
                self.advance();
                CallArgMode::Out
            }
            TK::KwMove => {
                self.advance();
                CallArgMode::Move
            }
            _ => CallArgMode::Default,
        };

        let expr = self.parse_expr(0)?;

        Ok(CallArg {
            mode,
            expr,
            init: InitInfo::default(),
            span: self.close(marker),
        })
    }

    fn parse_index_or_slice_postfix(
        &mut self,
        expr: Expr,
        marker: Marker,
    ) -> Result<Expr, ParseError> {
        let is_slice = self.lookahead_for(TK::DotDot, TK::RBracket);
        self.advance();

        if self.curr_token.kind == TK::RBracket {
            return Err(ParseError::ExpectedArrayIndexOrRange(
                self.curr_token.clone(),
            ));
        }

        if is_slice {
            let mut start = None;
            let mut end = None;

            if self.curr_token.kind != TK::DotDot {
                let allow_range_expr = self.allow_range_expr;
                self.allow_range_expr = false;
                let result = self.parse_expr(0);
                self.allow_range_expr = allow_range_expr;
                start = Some(Box::new(result?));
            }

            self.consume(&TK::DotDot)?;

            if self.curr_token.kind != TK::RBracket {
                let allow_range_expr = self.allow_range_expr;
                self.allow_range_expr = false;
                let result = self.parse_expr(0);
                self.allow_range_expr = allow_range_expr;
                end = Some(Box::new(result?));
            }

            self.consume(&TK::RBracket)?;

            Ok(Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::Slice {
                    target: Box::new(expr),
                    start,
                    end,
                },
                ty: (),
                span: self.close(marker),
            })
        } else {
            let indices =
                self.parse_list(TK::Comma, TK::RBracket, |parser| parser.parse_expr(0))?;

            self.consume(&TK::RBracket)?;

            Ok(Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::ArrayIndex {
                    target: Box::new(expr),
                    indices,
                },
                ty: (),
                span: self.close(marker),
            })
        }
    }

    fn parse_dot_postfix(&mut self, expr: Expr, marker: Marker) -> Result<Expr, ParseError> {
        self.consume(&TK::Dot)?;

        match &self.curr_token.kind {
            TK::IntLit(index) => {
                self.advance();
                Ok(Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::TupleField {
                        target: Box::new(expr),
                        index: *index as usize,
                    },
                    ty: (),
                    span: self.close(marker),
                })
            }
            TK::Ident(name) => {
                self.advance();
                if self.curr_token.kind == TK::LParen {
                    self.advance();
                    let args =
                        self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_call_arg())?;
                    self.consume(&TK::RParen)?;
                    Ok(Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::MethodCall {
                            callee: Box::new(expr),
                            method_name: name.clone(),
                            args,
                        },
                        ty: (),
                        span: self.close(marker),
                    })
                } else {
                    Ok(Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::StructField {
                            target: Box::new(expr),
                            field: name.clone(),
                        },
                        ty: (),
                        span: self.close(marker),
                    })
                }
            }
            TK::KwGet => {
                self.advance();
                let name = "get".to_string();
                if self.curr_token.kind == TK::LParen {
                    self.advance();
                    let args =
                        self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_call_arg())?;
                    self.consume(&TK::RParen)?;
                    Ok(Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::MethodCall {
                            callee: Box::new(expr),
                            method_name: name,
                            args,
                        },
                        ty: (),
                        span: self.close(marker),
                    })
                } else {
                    Ok(Expr {
                        id: self.id_gen.new_id(),
                        kind: ExprKind::StructField {
                            target: Box::new(expr),
                            field: name,
                        },
                        ty: (),
                        span: self.close(marker),
                    })
                }
            }
            _ => Err(ParseError::ExpectedStructField(self.curr_token.clone())),
        }
    }

    fn parse_try_postfix(&mut self, expr: Expr, marker: Marker) -> Result<Expr, ParseError> {
        self.consume(&TK::Question)?;
        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::UnaryOp {
                op: UnaryOp::Try,
                expr: Box::new(expr),
            },
            ty: (),
            span: self.close(marker),
        })
    }

    pub(super) fn parse_if(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwIf)?;

        // Parse condition
        self.allow_struct_lit = false;
        let cond = self.parse_expr(0)?;
        self.allow_struct_lit = true;

        // Parse then body
        let then_body = self.parse_block()?;

        // Parse else body (if present)
        let else_body = if self.curr_token.kind == TK::KwElse {
            self.advance();
            if self.curr_token.kind == TK::KwIf {
                // Recursively parse else-if and wrap in a block expression.
                let nested = self.parse_if()?;
                let nested_span = nested.span;
                Expr {
                    id: self.id_gen.new_id(),
                    kind: ExprKind::Block {
                        items: Vec::new(),
                        tail: Some(Box::new(nested)),
                    },
                    ty: (),
                    span: nested_span,
                }
            } else {
                self.parse_block()?
            }
        } else {
            // No else branch; create an empty block
            Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::Block {
                    items: Vec::new(),
                    tail: None,
                },
                ty: (),
                span: then_body.span,
            }
        };

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_body: Box::new(then_body),
                else_body: Box::new(else_body),
            },
            ty: (),
            span: self.close(marker),
        })
    }

    fn parse_array_lit(&mut self) -> Result<Expr, ParseError> {
        self.parse_array_lit_common(None)
    }

    fn parse_typed_set_lit_with_args(
        &mut self,
        marker: Marker,
        type_args: Vec<TypeExpr>,
    ) -> Result<Expr, ParseError> {
        let elem_ty = match type_args.as_slice() {
            [elem_ty] => Some(elem_ty.clone()),
            _ => None,
        };
        self.parse_set_lit_with_marker(elem_ty, marker)
    }

    fn parse_typed_map_lit_with_args(
        &mut self,
        marker: Marker,
        type_args: Vec<TypeExpr>,
    ) -> Result<Expr, ParseError> {
        let (key_ty, value_ty) = match type_args.as_slice() {
            [key_ty, value_ty] => (Some(key_ty.clone()), Some(value_ty.clone())),
            _ => (None, None),
        };
        self.parse_map_lit_with_marker(key_ty, value_ty, marker)
    }

    fn parse_set_lit(&mut self, elem_ty: Option<TypeExpr>) -> Result<Expr, ParseError> {
        let marker = self.mark();
        self.parse_set_lit_with_marker(elem_ty, marker)
    }

    fn parse_map_lit(
        &mut self,
        key_ty: Option<TypeExpr>,
        value_ty: Option<TypeExpr>,
    ) -> Result<Expr, ParseError> {
        let marker = self.mark();
        self.parse_map_lit_with_marker(key_ty, value_ty, marker)
    }

    fn parse_set_lit_with_marker(
        &mut self,
        elem_ty: Option<TypeExpr>,
        marker: Marker,
    ) -> Result<Expr, ParseError> {
        self.consume(&TK::LBrace)?;
        let elems = self.parse_set_lit_elems(
            elem_ty.is_some(),
            /* bare set requires comma */ elem_ty.is_none(),
        )?;
        self.consume(&TK::RBrace)?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::SetLit { elem_ty, elems },
            ty: (),
            span: self.close(marker),
        })
    }

    fn parse_set_lit_elems(
        &mut self,
        allow_empty: bool,
        require_singleton_trailing_comma: bool,
    ) -> Result<Vec<Expr>, ParseError> {
        if self.curr_token.kind == TK::RBrace {
            if allow_empty {
                return Ok(Vec::new());
            }
            return Err(ParseError::ExpectedPrimary(self.curr_token.clone()));
        }

        let first = self.parse_expr(0)?;
        if self.curr_token.kind != TK::Comma {
            if require_singleton_trailing_comma {
                return Err(ParseError::SingleElementSetMissingComma(
                    self.curr_token.clone(),
                ));
            }
            return Ok(vec![first]);
        }
        self.advance();

        let mut elems = vec![first];
        let rest = self.parse_list(TK::Comma, TK::RBrace, |p| p.parse_expr(0))?;
        elems.extend(rest);
        Ok(elems)
    }

    fn parse_map_lit_with_marker(
        &mut self,
        key_ty: Option<TypeExpr>,
        value_ty: Option<TypeExpr>,
        marker: Marker,
    ) -> Result<Expr, ParseError> {
        self.consume(&TK::LBrace)?;
        let allow_empty = key_ty.is_some() && value_ty.is_some();
        let entries = self.parse_map_lit_entries(allow_empty)?;
        self.consume(&TK::RBrace)?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::MapLit {
                key_ty,
                value_ty,
                entries,
            },
            ty: (),
            span: self.close(marker),
        })
    }

    fn parse_map_lit_entries(&mut self, allow_empty: bool) -> Result<Vec<MapLitEntry>, ParseError> {
        if self.curr_token.kind == TK::RBrace {
            if allow_empty {
                return Ok(Vec::new());
            }
            return Err(ParseError::ExpectedPrimary(self.curr_token.clone()));
        }

        let first = self.parse_map_lit_entry()?;
        if self.curr_token.kind != TK::Comma {
            return Ok(vec![first]);
        }
        self.advance();

        let mut entries = vec![first];
        let rest = self.parse_list(TK::Comma, TK::RBrace, |p| p.parse_map_lit_entry())?;
        entries.extend(rest);
        Ok(entries)
    }

    fn parse_map_lit_entry(&mut self) -> Result<MapLitEntry, ParseError> {
        let marker = self.mark();
        let key = self.parse_expr(0)?;
        self.consume(&TK::Colon)?;
        let value = self.parse_expr(0)?;
        Ok(MapLitEntry {
            id: self.id_gen.new_id(),
            key,
            value,
            span: self.close(marker),
        })
    }

    fn parse_typed_array_lit(&mut self, type_name: String) -> Result<Expr, ParseError> {
        let marker = self.mark();

        let elem_ty = TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Named {
                ident: type_name,
                def_id: (),
                type_args: Vec::new(),
            },
            span: self.close(marker),
        };

        self.parse_array_lit_common(Some(elem_ty))
    }

    fn parse_array_lit_common(&mut self, elem_ty: Option<TypeExpr>) -> Result<Expr, ParseError> {
        let marker = self.mark();

        self.advance();

        if self.curr_token.kind == TK::RBracket {
            self.advance();
            return Ok(Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::ArrayLit {
                    elem_ty,
                    init: ArrayLitInit::Elems(Vec::new()),
                },
                ty: (),
                span: self.close(marker),
            });
        }

        let first = self.parse_expr(0)?;
        let init = if self.curr_token.kind == TK::Semicolon {
            self.advance();

            let count = self.parse_int_lit()?;

            self.consume(&TK::RBracket)?;

            ArrayLitInit::Repeat(Box::new(first), count)
        } else {
            let mut elems = vec![first];

            if self.curr_token.kind == TK::Comma {
                self.advance();
                let rest = self.parse_list(TK::Comma, TK::RBracket, |p| p.parse_expr(0))?;
                elems.extend(rest);
            }

            self.consume(&TK::RBracket)?;

            ArrayLitInit::Elems(elems)
        };

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::ArrayLit { elem_ty, init },
            ty: (),
            span: self.close(marker),
        })
    }

    fn parse_enum_variant_with_args(
        &mut self,
        marker: Marker,
        enum_name: String,
        type_args: Vec<TypeExpr>,
    ) -> Result<Expr, ParseError> {
        self.consume(&TK::DoubleColon)?;
        let variant = self.parse_ident()?;

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
                type_args,
                variant,
                payload,
            },
            ty: (),
            span: self.close(marker),
        })
    }

    fn parse_struct_lit_with_args(
        &mut self,
        marker: Marker,
        name: String,
        type_args: Vec<TypeExpr>,
    ) -> Result<Expr, ParseError> {
        self.consume(&TK::LBrace)?;

        let fields = self.parse_list(TK::Comma, TK::RBrace, |parser| {
            let field_marker = parser.mark();
            let name = parser.parse_ident()?;

            parser.consume(&TK::Colon)?;

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
            kind: ExprKind::StructLit {
                name,
                type_args,
                fields,
            },
            ty: (),
            span: self.close(marker),
        })
    }

    fn parse_type_name_with_args(&mut self) -> Result<(String, Vec<TypeExpr>), ParseError> {
        let name = match &self.curr_token.kind {
            TK::Ident(_) => self.parse_ident()?,
            TK::KwSet => {
                self.advance();
                "set".to_string()
            }
            TK::KwMap => {
                self.advance();
                "map".to_string()
            }
            _ => return Err(ParseError::ExpectedIdent(self.curr_token.clone())),
        };
        let type_args = self.parse_type_args()?;
        Ok((name, type_args))
    }

    fn peek_generic_lit_kind(&self) -> Option<GenericLitKind> {
        if self.peek().map(|t| &t.kind) != Some(&TK::LessThan) {
            return None;
        }

        let mut depth = 0usize;
        let mut index = self.pos + 1;
        while index < self.tokens.len() {
            match self.tokens[index].kind {
                TK::LessThan => depth += 1,
                TK::GreaterThan => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        break;
                    }
                }
                TK::ShiftRight => return None,
                TK::Eof => return None,
                _ => {}
            }
            index += 1;
        }

        if depth != 0 {
            return None;
        }

        match self.tokens.get(index + 1).map(|t| &t.kind) {
            Some(TK::DoubleColon) => Some(GenericLitKind::EnumVariant),
            Some(TK::LBrace) => Some(GenericLitKind::StructLit),
            _ => None,
        }
    }

    fn parse_struct_update(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        self.consume(&TK::LBrace)?;

        let base = self.parse_expr(3)?;

        self.consume(&TK::Pipe)?;

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

        self.consume(&TK::RBrace)?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::StructUpdate {
                target: Box::new(base),
                fields,
            },
            ty: (),
            span: self.close(marker),
        })
    }

    fn parse_paren_or_tuple(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();
        self.advance();

        if self.curr_token.kind == TK::RParen {
            let span = self.close(marker);
            self.advance();
            return Ok(Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::UnitLit,
                ty: (),
                span,
            });
        }

        let first_expr = self.parse_expr(0)?;

        if self.curr_token.kind == TK::Comma {
            let mut fields = vec![first_expr];
            while self.curr_token.kind == TK::Comma {
                self.advance();
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
                ty: (),
                span: self.close(marker),
            });
        }

        self.consume(&TK::RParen)?;
        Ok(first_expr)
    }
}
