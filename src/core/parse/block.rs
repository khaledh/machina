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
                TK::KwBreak => {
                    let stmt = self.parse_break()?;
                    items.push(BlockItem::Stmt(stmt));
                }
                TK::KwContinue => {
                    let stmt = self.parse_continue()?;
                    items.push(BlockItem::Stmt(stmt));
                }
                TK::KwReturn => {
                    let stmt = self.parse_return()?;
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
                                ty: (),
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
            ty: (),
            span: self.close(marker),
        })
    }

    pub(super) fn parse_let(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwLet)?;
        let pattern = self.parse_bind_pattern()?;

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
            ty: (),
            span: self.close(marker),
        })
    }

    pub(super) fn parse_var(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwVar)?;

        let is_binding = self.lookahead_for(TK::Equals, TK::Semicolon);

        if is_binding {
            let pattern = self.parse_bind_pattern()?;

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
                ty: (),
                span: self.close(marker),
            })
        } else {
            let ident = self.parse_ident()?;

            self.consume(&TK::Colon)?;

            let decl_ty = self.parse_type_expr()?;

            self.consume(&TK::Semicolon)?;

            Ok(StmtExpr {
                id: self.id_gen.new_id(),
                kind: StmtExprKind::VarDecl {
                    ident,
                    def_id: (),
                    decl_ty,
                },
                ty: (),
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
                init: InitInfo::default(),
            },
            ty: (),
            span: self.close(marker),
        })
    }

    pub(super) fn parse_while(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwWhile)?;

        self.allow_struct_lit = false;
        let cond = self.parse_expr(0)?;
        self.allow_struct_lit = true;

        let body = self.parse_block()?;

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind: StmtExprKind::While {
                cond: Box::new(cond),
                body: Box::new(body),
            },
            ty: (),
            span: self.close(marker),
        })
    }

    pub(super) fn parse_for(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwFor)?;

        let pattern = self.parse_bind_pattern()?;

        self.consume_keyword(TK::KwIn)?;

        self.allow_struct_lit = false;
        let iter = self.parse_expr(0)?;
        self.allow_struct_lit = true;

        let body = self.parse_block()?;

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind: StmtExprKind::For {
                pattern,
                iter: Box::new(iter),
                body: Box::new(body),
            },
            ty: (),
            span: self.close(marker),
        })
    }

    pub(super) fn parse_break(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwBreak)?;
        self.consume(&TK::Semicolon)?;

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind: StmtExprKind::Break,
            ty: (),
            span: self.close(marker),
        })
    }

    pub(super) fn parse_continue(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwContinue)?;
        self.consume(&TK::Semicolon)?;

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind: StmtExprKind::Continue,
            ty: (),
            span: self.close(marker),
        })
    }

    pub(super) fn parse_return(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwReturn)?;

        let value = if self.curr_token.kind == TK::Semicolon {
            None
        } else {
            Some(Box::new(self.parse_expr(0)?))
        };

        self.consume(&TK::Semicolon)?;

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind: StmtExprKind::Return { value },
            ty: (),
            span: self.close(marker),
        })
    }
}
