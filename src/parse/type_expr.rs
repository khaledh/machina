use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        let mut typ = self.parse_type_atom()?;

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

        if self.curr_token.kind == TK::KwFn {
            return self.parse_fn_type();
        }

        if self.curr_token.kind == TK::KwRange {
            return self.parse_range_type();
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
        let return_ty = self.parse_type_expr()?;

        Ok(TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Fn {
                params,
                return_ty: Box::new(return_ty),
            },
            span: self.close(marker),
        })
    }

    fn parse_fn_type_param(&mut self) -> Result<FnTypeParam, ParseError> {
        let mode = self.parse_param_mode();
        let ty = self.parse_type_expr()?;
        Ok(FnTypeParam { mode, ty })
    }

    fn parse_named_type(&mut self) -> Result<TypeExpr, ParseError> {
        let marker = self.mark();

        let result = match &self.curr_token.kind {
            TK::LParen if self.peek().map(|t| &t.kind) == Some(&TK::RParen) => {
                self.advance();
                Ok(TypeExpr {
                    id: self.id_gen.new_id(),
                    kind: TypeExprKind::Named("()".to_string()),
                    span: self.close(marker),
                })
            }
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
        fields.push(self.parse_type_expr()?);

        while self.curr_token.kind == TK::Comma {
            self.advance();
            if self.curr_token.kind == TK::RParen {
                break;
            }
            fields.push(self.parse_type_expr()?);
        }

        self.consume(&TK::RParen)?;

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

        self.advance();
        let dims = self.parse_list(TK::Comma, TK::RBracket, |parser| parser.parse_int_lit())?;
        self.consume(&TK::RBracket)?;

        if dims.is_empty() {
            return Ok(TypeExpr {
                id: self.id_gen.new_id(),
                kind: TypeExprKind::Slice {
                    elem_ty: Box::new(elem_ty),
                },
                span: self.close(marker),
            });
        }

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
        let marker = self.mark();

        self.consume_keyword(TK::KwRange)?;
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
}
