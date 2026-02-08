use super::*;
use crate::tree::RefinementKind;

impl<'a> Parser<'a> {
    pub(super) fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();
        let first = self.parse_type_term()?;

        if self.curr_token.kind != TK::Pipe || !self.pipe_starts_type_union() {
            return Ok(first);
        }

        let mut variants = vec![first];
        while self.curr_token.kind == TK::Pipe && self.pipe_starts_type_union() {
            self.advance();
            variants.push(self.parse_type_term()?);
        }

        Ok(TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Union { variants },
            span: self.close(marker),
        })
    }

    fn pipe_starts_type_union(&self) -> bool {
        self.peek()
            .is_some_and(|tok| Self::token_can_start_type(&tok.kind, self.tokens, self.pos + 1))
    }

    fn token_can_start_type(kind: &TK, tokens: &[Token], pos: usize) -> bool {
        match kind {
            TK::Caret | TK::KwFn | TK::Ident(_) => true,
            TK::LParen => true,
            TK::LBracket => false,
            _ => {
                // Keep this conservative for now. We only need to prevent
                // consuming closure `| ... |` delimiters as union operators.
                let _ = (tokens, pos);
                false
            }
        }
    }

    fn parse_type_term(&mut self) -> Result<TypeExpr, ParseError> {
        let mut typ = self.parse_type_atom()?;

        if self.curr_token.kind == TK::Colon {
            typ = self.parse_refined_type(typ)?;
        }

        if self.curr_token.kind == TK::LBracket {
            typ = self.parse_array_type(typ)?;
        }

        Ok(typ)
    }

    fn parse_type_atom(&mut self) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();

        if self.curr_token.kind == TK::Caret {
            self.advance();
            let elem_ty_expr = self.parse_type_atom()?;
            return Ok(TypeExpr {
                id: self.id_gen.new_id(),
                kind: TypeExprKind::Heap {
                    elem_ty_expr: Box::new(elem_ty_expr),
                },
                span: self.close(marker),
            });
        }

        if self.curr_token.kind == TK::KwFn {
            return self.parse_fn_type();
        }

        if self.curr_token.kind == TK::LParen && self.peek().map(|t| &t.kind) != Some(&TK::RParen) {
            self.advance();
            return self.parse_tuple_type();
        }

        self.parse_named_type()
    }

    fn parse_fn_type(&mut self) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();
        self.consume_keyword(TK::KwFn)?;
        self.consume(&TK::LParen)?;

        let params = if self.curr_token.kind == TK::RParen {
            Vec::new()
        } else {
            self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_fn_type_param())?
        };
        self.consume(&TK::RParen)?;

        self.consume(&TK::Arrow)?;
        let ret_ty_expr = self.parse_type_expr()?;

        Ok(TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Fn {
                params,
                ret_ty_expr: Box::new(ret_ty_expr),
            },
            span: self.close(marker),
        })
    }

    fn parse_fn_type_param(&mut self) -> Result<FnTypeParam, ParseError> {
        let mode = self.parse_param_mode();
        let ty_expr = self.parse_type_expr()?;
        Ok(FnTypeParam { mode, ty_expr })
    }

    fn parse_named_type(&mut self) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();

        match &self.curr_token.kind {
            TK::LParen if self.peek().map(|t| &t.kind) == Some(&TK::RParen) => {
                self.advance();
                self.consume(&TK::RParen)?;
                Ok(TypeExpr {
                    id: self.id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: "()".to_string(),
                        def_id: (),
                        type_args: Vec::new(),
                    },
                    span: self.close(marker),
                })
            }
            TK::Ident(name) => {
                let mut ident = name.clone();
                self.advance();

                while self.curr_token.kind == TK::DoubleColon {
                    self.advance();
                    let seg = self.parse_ident()?;
                    ident.push_str("::");
                    ident.push_str(&seg);
                }

                let type_args = self.parse_type_args()?;
                Ok(TypeExpr {
                    id: self.id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident,
                        def_id: (),
                        type_args,
                    },
                    span: self.close(marker),
                })
            }
            _ => Err(ParseError::ExpectedType(self.curr_token.clone())),
        }
    }

    pub(super) fn parse_type_args(&mut self) -> Result<Vec<TypeExpr>, ParseError> {
        if self.curr_token.kind != TK::LessThan {
            return Ok(Vec::new());
        }

        self.consume(&TK::LessThan)?;
        let args = self.parse_list(TK::Comma, TK::GreaterThan, |parser| {
            parser.parse_type_expr()
        })?;
        self.consume(&TK::GreaterThan)?;

        Ok(args)
    }

    fn parse_tuple_type(&mut self) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();

        let mut field_ty_exprs = Vec::new();
        field_ty_exprs.push(self.parse_type_expr()?);

        while self.curr_token.kind == TK::Comma {
            self.advance();
            if self.curr_token.kind == TK::RParen {
                break;
            }
            field_ty_exprs.push(self.parse_type_expr()?);
        }

        self.consume(&TK::RParen)?;

        if field_ty_exprs.len() == 1 {
            return Err(ParseError::SingleFieldTupleMissingComma(
                self.curr_token.clone(),
            ));
        }

        Ok(TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Tuple { field_ty_exprs },
            span: self.close(marker),
        })
    }

    fn parse_array_type(&mut self, elem_ty_expr: TypeExpr) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();

        self.advance();
        let dims = self.parse_list(TK::Comma, TK::RBracket, |parser| parser.parse_int_lit())?;
        self.consume(&TK::RBracket)?;

        if dims.is_empty() {
            return Ok(TypeExpr {
                id: self.id_gen.new_id(),
                kind: TypeExprKind::Slice {
                    elem_ty_expr: Box::new(elem_ty_expr),
                },
                span: self.close(marker),
            });
        }

        Ok(TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Array {
                elem_ty_expr: Box::new(elem_ty_expr),
                dims: dims.into_iter().map(|d| d as usize).collect(),
            },
            span: self.close(marker),
        })
    }

    fn parse_refined_type(&mut self, base_ty_expr: TypeExpr) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();

        self.consume(&TK::Colon)?;
        let mut refinements = Vec::new();
        refinements.push(self.parse_refinement()?);
        while self.curr_token.kind == TK::Ampersand {
            self.advance();
            refinements.push(self.parse_refinement()?);
        }

        let refinements_span = self.close(marker);
        let span = Span::merge_all(vec![base_ty_expr.span, refinements_span]);
        Ok(TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Refined {
                base_ty_expr: Box::new(base_ty_expr),
                refinements,
            },
            span,
        })
    }

    fn parse_refinement(&mut self) -> Result<RefinementKind, ParseError> {
        match &self.curr_token.kind {
            TK::KwBounds => {
                self.advance();
                self.consume(&TK::LParen)?;

                let first = self.parse_signed_int_lit()?;
                let (min, max) = if self.curr_token.kind == TK::Comma {
                    self.advance();
                    let second = self.parse_signed_int_lit()?;
                    (first, second)
                } else {
                    (0, first)
                };

                self.consume(&TK::RParen)?;
                Ok(RefinementKind::Bounds { min, max })
            }
            TK::KwNonzero => {
                self.advance();
                Ok(RefinementKind::NonZero)
            }
            _ => Err(ParseError::ExpectedRefinement(self.curr_token.clone())),
        }
    }
}
