use crate::diag::{Position, Span};
use crate::lexer::{Token, TokenKind, TokenKind as TK};
use crate::tree::parsed::*;
use crate::tree::{BinaryOp, CallArgMode, InitInfo, ParamMode, UnaryOp};

mod bind_pattern;
mod block;
mod decl;
mod errors;
mod expr;
mod fstring;
mod func;
mod match_expr;
mod type_expr;

pub use errors::ParseError;

#[derive(Debug, Clone, Copy)]
struct Marker {
    pos: Position,
    token_index: usize,
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    curr_token: &'a Token,
    id_gen: NodeIdGen,
    allow_struct_lit: bool,
    closure_base: Option<String>,
    closure_index: u32,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            pos: 0,
            curr_token: &tokens[0],
            id_gen: NodeIdGen::new(),
            allow_struct_lit: true,
            closure_base: None,
            closure_index: 0,
        }
    }

    pub fn new_with_id_gen(tokens: &'a [Token], id_gen: NodeIdGen) -> Self {
        Parser {
            tokens,
            pos: 0,
            curr_token: &tokens[0],
            id_gen,
            allow_struct_lit: true,
            closure_base: None,
            closure_index: 0,
        }
    }

    pub fn into_id_gen(self) -> NodeIdGen {
        self.id_gen
    }

    pub fn parse(&mut self) -> Result<Module, ParseError> {
        let mut top_level_items = Vec::new();

        // Parse the top-level items
        while self.curr_token.kind != TK::Eof {
            top_level_items.push(self.parse_top_level_item()?);
        }

        Ok(Module { top_level_items })
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
            token_index: self.pos,
        }
    }

    fn close(&self, marker: Marker) -> Span {
        let end = if self.pos == marker.token_index {
            self.curr_token.span.end
        } else {
            self.tokens[self.pos - 1].span.end
        };
        Span::new(marker.pos, end)
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

    fn consume_keyword(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        if self.curr_token.kind == expected {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::ExpectedToken(expected, self.curr_token.clone()))
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

    fn parse_string_lit(&mut self) -> Result<String, ParseError> {
        if let TK::StringLit(value) = &self.curr_token.kind {
            self.advance();
            Ok(value.clone())
        } else {
            Err(ParseError::ExpectedStringLit(self.curr_token.clone()))
        }
    }

    fn parse_attribute_list(&mut self) -> Result<Vec<Attribute>, ParseError> {
        if self.curr_token.kind != TK::At {
            return Ok(Vec::new());
        }

        self.advance();
        self.consume(&TK::LBracket)?;

        let attrs = self.parse_list(TK::Comma, TK::RBracket, |parser| {
            let marker = parser.mark();
            let name = parser.parse_ident()?;
            let mut args = Vec::new();
            if parser.curr_token.kind == TK::LParen {
                parser.advance();
                args = parser.parse_list(TK::Comma, TK::RParen, |parser| {
                    let value = parser.parse_string_lit()?;
                    Ok(AttrArg::String(value))
                })?;
                parser.consume(&TK::RParen)?;
            }
            Ok(Attribute {
                name,
                args,
                span: parser.close(marker),
            })
        })?;

        self.consume(&TK::RBracket)?;
        Ok(attrs)
    }

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

            if *token == target && at_target_level {
                return true;
            }

            if *token == stop_at && brace == 0 && bracket == 0 && paren == 0 {
                return false;
            }

            idx += 1;
        }

        false
    }

    fn is_closure_capture_list(&self) -> bool {
        if self.curr_token.kind != TK::LBracket {
            return false;
        }
        if self.tokens.get(self.pos + 1).map(|t| &t.kind) != Some(&TK::KwMove) {
            return false;
        }
        let Some(close_idx) = self.matching_rbracket_index(self.pos) else {
            return false;
        };
        matches!(
            self.tokens.get(close_idx + 1).map(|t| &t.kind),
            Some(TK::Pipe | TK::LogicalOr)
        )
    }

    fn matching_rbracket_index(&self, start: usize) -> Option<usize> {
        let mut depth = 0isize;
        for (idx, token) in self.tokens.iter().enumerate().skip(start) {
            match token.kind {
                TK::LBracket => depth += 1,
                TK::RBracket => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(idx);
                    }
                }
                _ => {}
            }
        }
        None
    }
}

#[cfg(test)]
#[path = "../tests/parse/t_parse.rs"]
mod tests;
