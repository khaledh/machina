use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_bind_pattern(&mut self) -> Result<BindPattern, ParseError> {
        let marker = self.mark();

        match &self.curr_token.kind {
            TK::Ident(name) if self.peek().map(|t| &t.kind) == Some(&TK::LBrace) => {
                self.parse_struct_bind_pattern(marker, name.clone())
            }
            TK::Ident(name) => self.parse_name_bind_pattern(marker, name.clone()),
            TK::Underscore => self.parse_wildcard_bind_pattern(marker),
            TK::LBracket => self.parse_array_bind_pattern(marker),
            TK::LParen => self.parse_tuple_bind_pattern(marker),
            _ => self.err_here(PEK::ExpectedPattern(self.curr_token.clone())),
        }
    }

    fn parse_wildcard_bind_pattern(&mut self, marker: Marker) -> Result<BindPattern, ParseError> {
        self.advance();
        Ok(BindPattern {
            id: self.id_gen.new_id(),
            kind: BindPatternKind::Wildcard,
            span: self.close(marker),
        })
    }

    fn parse_name_bind_pattern(
        &mut self,
        marker: Marker,
        var_name: String,
    ) -> Result<BindPattern, ParseError> {
        self.advance(); // consume the ident
        Ok(BindPattern {
            id: self.id_gen.new_id(),
            kind: BindPatternKind::Name { ident: var_name },
            span: self.close(marker),
        })
    }

    fn parse_struct_bind_pattern(
        &mut self,
        marker: Marker,
        struct_name: String,
    ) -> Result<BindPattern, ParseError> {
        self.advance(); // consume the ident
        self.consume(&TK::LBrace)?;

        let struct_fields = self.parse_list(TK::Comma, TK::RBrace, |parser| {
            let field_marker = parser.mark();

            // Field name
            let field_name = parser.parse_ident()?;

            let pattern = if parser.curr_token.kind == TK::Colon {
                // Field pattern
                parser.advance();
                parser.parse_bind_pattern()?
            } else {
                // Shorthand field pattern
                BindPattern {
                    id: parser.id_gen.new_id(),
                    kind: BindPatternKind::Name {
                        ident: field_name.clone(),
                    },
                    span: parser.close(field_marker),
                }
            };

            Ok(StructFieldBindPattern {
                name: field_name,
                pattern,
                span: parser.close(field_marker),
            })
        })?;

        self.consume(&TK::RBrace)?;

        Ok(BindPattern {
            id: self.id_gen.new_id(),
            kind: BindPatternKind::Struct {
                name: struct_name,
                fields: struct_fields,
            },
            span: self.close(marker),
        })
    }

    fn parse_array_bind_pattern(&mut self, marker: Marker) -> Result<BindPattern, ParseError> {
        self.consume(&TK::LBracket)?;

        let mut prefix = Vec::new();
        let mut suffix = Vec::new();
        let mut rest = None;

        while self.curr_token.kind != TK::RBracket {
            if rest.is_some() {
                if self.curr_token.kind == TK::DotDot
                    && self.peek().map(|token| token.kind.clone()) == Some(TK::Dot)
                {
                    return self.err_here(PEK::DuplicateArrayRestPattern);
                }
                suffix.push(self.parse_bind_pattern()?);
            } else if self.curr_token.kind == TK::DotDot
                && self.peek().map(|token| token.kind.clone()) == Some(TK::Dot)
            {
                let rest_marker = self.mark();
                self.consume(&TK::DotDot)?;
                self.consume(&TK::Dot)?;

                let pattern = match self.curr_token.kind {
                    TK::Comma | TK::RBracket => None,
                    TK::Ident(_) | TK::Underscore => Some(Box::new(self.parse_bind_pattern()?)),
                    _ => return self.err_here(PEK::InvalidArrayRestPattern),
                };

                rest = Some(ArrayRestBindPattern {
                    pattern,
                    span: self.close(rest_marker),
                });
            } else {
                prefix.push(self.parse_bind_pattern()?);
            }

            if self.curr_token.kind != TK::Comma {
                break;
            }
            self.advance();
            if self.curr_token.kind == TK::RBracket {
                break;
            }
        }

        self.consume(&TK::RBracket)?;

        Ok(BindPattern {
            id: self.id_gen.new_id(),
            kind: BindPatternKind::Array {
                prefix,
                rest,
                suffix,
            },
            span: self.close(marker),
        })
    }

    fn parse_tuple_bind_pattern(&mut self, marker: Marker) -> Result<BindPattern, ParseError> {
        self.consume(&TK::LParen)?;

        let first_pattern = self.parse_bind_pattern()?;

        if self.curr_token.kind != TK::Comma {
            self.consume(&TK::RParen)?;
            return Ok(first_pattern);
        }

        let mut patterns = vec![first_pattern];
        while self.curr_token.kind == TK::Comma {
            self.advance();
            if self.curr_token.kind == TK::RParen {
                break;
            }
            let pattern = self.parse_bind_pattern()?;
            patterns.push(pattern);
        }

        self.consume(&TK::RParen)?;

        Ok(BindPattern {
            id: self.id_gen.new_id(),
            kind: BindPatternKind::Tuple { patterns },
            span: self.close(marker),
        })
    }
}
