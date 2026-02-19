use super::*;
use crate::core::lexer::{LexError, Lexer, Token};

impl<'a> Parser<'a> {
    pub(super) fn parse_fstring(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        self.advance();

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
                ty: (),
                span: self.close(marker),
            });
        }

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::StringFmt { segments },
            ty: (),
            span: self.close(marker),
        })
    }

    fn parse_fstring_segments(
        &mut self,
        value: &str,
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
                        return Err(PEK::UnmatchedFormatBrace.at(span));
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
        let tokens = Lexer::new(src)
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .map_err(|_| PEK::InvalidFormatExpr.at(span))?;

        let id_gen = std::mem::take(&mut self.id_gen);

        let mut sub_parser = Parser::new_with_id_gen(&tokens, id_gen);
        let expr = sub_parser.parse_expr(0);
        let has_trailing = sub_parser.curr_token.kind != TK::Eof;

        self.id_gen = sub_parser.into_id_gen();

        let expr = expr?;

        if has_trailing {
            return Err(PEK::InvalidFormatExpr.at(span));
        }

        Ok(expr)
    }

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
                            return Err(PEK::EmptyFormatExpr.at(span));
                        }
                        let expr = chars[..i].iter().collect::<String>();
                        return Ok((expr, i + 1));
                    }
                    depth -= 1;
                }
                _ => {}
            }
        }

        Err(PEK::UnterminatedFormatExpr.at(span))
    }
}
