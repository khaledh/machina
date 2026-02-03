use super::*;
use crate::tree::RefinementKind;

impl<'a> Parser<'a> {
    pub(super) fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        let mut typ = self.parse_type_atom()?;

        if self.curr_token.kind == TK::Colon {
            typ = self.parse_bounded_type(typ)?;
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

        let result = match &self.curr_token.kind {
            TK::LParen if self.peek().map(|t| &t.kind) == Some(&TK::RParen) => {
                self.advance();
                Ok(TypeExpr {
                    id: self.id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: "()".to_string(),
                        def_id: (),
                    },
                    span: self.close(marker),
                })
            }
            TK::Ident(name) => Ok(TypeExpr {
                id: self.id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: name.clone(),
                    def_id: (),
                },
                span: self.close(marker),
            }),
            _ => Err(ParseError::ExpectedType(self.curr_token.clone())),
        };
        result.inspect(|_| self.advance())
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

    fn parse_bounded_type(&mut self, base_ty_expr: TypeExpr) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();

        self.consume(&TK::Colon)?;
        self.consume_keyword(TK::KwBounds)?;
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

        let bounds_span = self.close(marker);
        let span = Span::merge_all(vec![base_ty_expr.span, bounds_span]);
        Ok(TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Refined {
                base_ty_expr: Box::new(base_ty_expr),
                refinement: RefinementKind::Bounds { min, max },
            },
            span,
        })
    }
}
