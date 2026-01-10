use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_match_expr(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwMatch)?;

        self.allow_struct_lit = false;
        let scrutinee = self.parse_expr(0)?;
        self.allow_struct_lit = true;

        self.consume(&TK::LBrace)?;

        let arms = self.parse_list(TK::Comma, TK::RBrace, |parser| parser.parse_match_arm())?;
        if arms.is_empty() {
            return Err(ParseError::ExpectedMatchArm(self.curr_token.clone()));
        }

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

        let pattern = self.parse_match_pattern()?;

        self.consume(&TK::FatArrow)?;

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

        if self.curr_token.kind == TK::Underscore {
            self.advance();
            return Ok(MatchPattern::Wildcard {
                span: self.close(marker),
            });
        }

        if let TK::BoolLit(value) = &self.curr_token.kind {
            let value = *value;
            self.advance();
            return Ok(MatchPattern::BoolLit {
                value,
                span: self.close(marker),
            });
        }

        if let TK::IntLit(value) = &self.curr_token.kind {
            let value = *value;
            self.advance();
            return Ok(MatchPattern::IntLit {
                value,
                span: self.close(marker),
            });
        }

        if self.curr_token.kind == TK::LParen {
            self.advance();
            return self.parse_tuple_match_pattern(marker);
        }

        if !matches!(self.curr_token.kind, TK::Ident(_)) {
            return Err(ParseError::ExpectedMatchPattern(self.curr_token.clone()));
        }
        let variant_name = self.parse_ident()?;
        self.parse_enum_variant_pattern(marker, variant_name)
    }

    fn parse_tuple_match_pattern(&mut self, marker: Marker) -> Result<MatchPattern, ParseError> {
        let parse_tuple_pattern_elem = |parser: &mut Parser| {
            let marker = parser.mark();

            if parser.curr_token.kind == TK::Underscore {
                parser.advance();
                return Ok(MatchPattern::Wildcard {
                    span: parser.close(marker),
                });
            }

            if let TK::BoolLit(value) = &parser.curr_token.kind {
                let value = *value;
                parser.advance();
                return Ok(MatchPattern::BoolLit {
                    value,
                    span: parser.close(marker),
                });
            }

            if let TK::IntLit(value) = &parser.curr_token.kind {
                let value = *value;
                parser.advance();
                return Ok(MatchPattern::IntLit {
                    value,
                    span: parser.close(marker),
                });
            }

            if parser.curr_token.kind == TK::LParen {
                parser.advance();
                return parser.parse_tuple_match_pattern(marker);
            }

            let TK::Ident(_) = &parser.curr_token.kind else {
                return Err(ParseError::ExpectedMatchPattern(parser.curr_token.clone()));
            };

            let is_enum_variant = matches!(
                parser.tokens.get(parser.pos + 1).map(|tok| &tok.kind),
                Some(TK::DoubleColon | TK::LParen)
            );

            let ident = parser.parse_ident()?;
            if is_enum_variant {
                return parser.parse_enum_variant_pattern(marker, ident);
            }

            Ok(MatchPattern::Binding {
                id: parser.id_gen.new_id(),
                ident,
                span: parser.close(marker),
            })
        };

        let fields = self.parse_list(TK::Comma, TK::RParen, parse_tuple_pattern_elem)?;

        if fields.len() == 1 {
            return Err(ParseError::SingleFieldTupleMissingComma(
                self.curr_token.clone(),
            ));
        }

        self.consume(&TK::RParen)?;

        Ok(MatchPattern::Tuple {
            patterns: fields,
            span: self.close(marker),
        })
    }

    fn parse_enum_variant_pattern(
        &mut self,
        marker: Marker,
        mut variant_name: String,
    ) -> Result<MatchPattern, ParseError> {
        let mut enum_name = None;

        if self.curr_token.kind == TK::DoubleColon {
            self.advance();
            enum_name = Some(variant_name);
            variant_name = self.parse_ident()?;
        }

        let parse_match_binding = |parser: &mut Parser| {
            let marker = parser.mark();
            if parser.curr_token.kind == TK::Underscore {
                parser.advance();
                return Ok(MatchPatternBinding::Wildcard {
                    span: parser.close(marker),
                });
            }

            let ident = parser.parse_ident()?;
            Ok(MatchPatternBinding::Named {
                id: parser.id_gen.new_id(),
                ident,
                span: parser.close(marker),
            })
        };

        let bindings = if self.curr_token.kind == TK::LParen {
            self.advance();
            let bindings = self.parse_list(TK::Comma, TK::RParen, parse_match_binding)?;
            self.consume(&TK::RParen)?;
            bindings
        } else {
            vec![]
        };

        Ok(MatchPattern::EnumVariant {
            enum_name,
            variant_name,
            bindings,
            span: self.close(marker),
        })
    }
}
