use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_block(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        self.consume(&TK::LBrace)?;

        let mut items = Vec::new();
        let mut tail = None;

        while self.curr_token.kind != TK::RBrace {
            match &self.curr_token.kind {
                TK::KwLet => {
                    let stmt = self.parse_let()?;
                    items.push(BlockItem::Stmt(stmt));
                }
                TK::KwVar => {
                    let stmt = self.parse_var()?;
                    items.push(BlockItem::Stmt(stmt));
                }
                TK::KwWhile => {
                    let stmt = self.parse_while()?;
                    items.push(BlockItem::Stmt(stmt));
                }
                TK::KwFor => {
                    let stmt = self.parse_for()?;
                    items.push(BlockItem::Stmt(stmt));
                }
                _ => {
                    let expr = self.parse_expr(0)?;
                    match self.curr_token.kind {
                        TK::Equals => {
                            let stmt = self.parse_assign(expr)?;
                            items.push(BlockItem::Stmt(stmt));
                        }
                        TK::Semicolon => {
                            let semi_span = self.curr_token.span;
                            self.advance();
                            let expr = Expr {
                                span: Span::new(expr.span.start, semi_span.end),
                                ..expr
                            };
                            items.push(BlockItem::Expr(expr));
                        }
                        _ => {
                            tail = Some(Box::new(expr));
                            break;
                        }
                    }
                }
            }
        }

        self.consume(&TK::RBrace)?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Block { items, tail },
            span: self.close(marker),
        })
    }

    pub(super) fn parse_let(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwLet)?;
        let pattern = self.parse_pattern()?;

        let decl_ty = if self.curr_token.kind == TK::Colon {
            self.advance();
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.consume(&TK::Equals)?;

        let value = self.parse_expr(0)?;

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

    pub(super) fn parse_var(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwVar)?;

        let is_binding = self.lookahead_for(TK::Equals, TK::Semicolon);

        if is_binding {
            let pattern = self.parse_pattern()?;

            let decl_ty = if self.curr_token.kind == TK::Colon {
                self.advance();
                Some(self.parse_type_expr()?)
            } else {
                None
            };

            self.consume(&TK::Equals)?;

            let value = self.parse_expr(0)?;

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
            let name = self.parse_ident()?;

            self.consume(&TK::Colon)?;

            let decl_ty = self.parse_type_expr()?;

            self.consume(&TK::Semicolon)?;

            Ok(StmtExpr {
                id: self.id_gen.new_id(),
                kind: StmtExprKind::VarDecl { name, decl_ty },
                span: self.close(marker),
            })
        }
    }

    pub(super) fn parse_assign(&mut self, assignee: Expr) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume(&TK::Equals)?;

        let value = self.parse_expr(0)?;

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

    pub(super) fn parse_while(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwWhile)?;

        let cond = self.parse_expr(0)?;

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

    pub(super) fn parse_for(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwFor)?;

        let pattern = self.parse_pattern()?;

        self.consume_keyword(TK::KwIn)?;

        self.allow_struct_lit = false;
        let iter = if matches!(self.curr_token.kind, TK::IntLit(_))
            && matches!(self.peek().map(|t| &t.kind), Some(TK::DotDot))
        {
            self.parse_range_expr()?
        } else {
            self.parse_expr(0)?
        };
        self.allow_struct_lit = true;

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
}
