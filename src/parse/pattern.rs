use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let marker = self.mark();

        match &self.curr_token.kind {
            TK::Ident(name) if self.peek().map(|t| &t.kind) == Some(&TK::LBrace) => {
                self.advance();
                let name = name.clone();
                self.parse_struct_pattern(marker, name)
            }
            TK::Ident(name) => {
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
                self.advance();

                let first_pattern = self.parse_pattern()?;

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
        self.consume(&TK::LBrace)?;

        let fields = self.parse_list(TK::Comma, TK::RBrace, |parser| {
            let field_marker = parser.mark();

            let name = parser.parse_ident()?;

            let pattern = if parser.curr_token.kind == TK::Colon {
                parser.advance();
                parser.parse_pattern()?
            } else {
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

        self.consume(&TK::RBrace)?;

        Ok(Pattern {
            id: self.id_gen.new_id(),
            kind: PatternKind::Struct { name, fields },
            span: self.close(marker),
        })
    }
}
