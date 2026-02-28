use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AssignOp {
    Simple,
    Compound(BinaryOp),
}

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
                TK::KwDefer => {
                    let stmt = self.parse_defer()?;
                    items.push(BlockItem::Stmt(stmt));
                }
                TK::KwUsing => {
                    let stmt = self.parse_using()?;
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
                    if let Some(assign_op) = self.current_assign_op() {
                        let stmt = self.parse_assign(expr, assign_op)?;
                        items.push(BlockItem::Stmt(stmt));
                    } else {
                        match self.curr_token.kind {
                            TK::Semicolon => {
                                self.advance();
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
        }

        self.consume(&TK::RBrace)?;

        Ok(Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Block { items, tail },
            span: self.close(marker),
        })
    }

    fn current_assign_op(&self) -> Option<AssignOp> {
        match self.curr_token.kind {
            TK::Equals => Some(AssignOp::Simple),
            TK::PlusEquals => Some(AssignOp::Compound(BinaryOp::Add)),
            TK::MinusEquals => Some(AssignOp::Compound(BinaryOp::Sub)),
            TK::StarEquals => Some(AssignOp::Compound(BinaryOp::Mul)),
            TK::SlashEquals => Some(AssignOp::Compound(BinaryOp::Div)),
            TK::PercentEquals => Some(AssignOp::Compound(BinaryOp::Mod)),
            TK::AmpersandEquals => Some(AssignOp::Compound(BinaryOp::BitAnd)),
            TK::PipeEquals => Some(AssignOp::Compound(BinaryOp::BitOr)),
            TK::CaretEquals => Some(AssignOp::Compound(BinaryOp::BitXor)),
            TK::ShiftLeftEquals => Some(AssignOp::Compound(BinaryOp::Shl)),
            TK::ShiftRightEquals => Some(AssignOp::Compound(BinaryOp::Shr)),
            _ => None,
        }
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
                span: self.close(marker),
            })
        } else {
            let ident = self.parse_ident()?;

            self.consume(&TK::Colon)?;

            let decl_ty = self.parse_type_expr()?;

            self.consume(&TK::Semicolon)?;

            Ok(StmtExpr {
                id: self.id_gen.new_id(),
                kind: StmtExprKind::VarDecl { ident, decl_ty },
                span: self.close(marker),
            })
        }
    }

    fn parse_assign(
        &mut self,
        assignee: Expr,
        assign_op: AssignOp,
    ) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.advance();

        let value = self.parse_expr(0)?;

        self.consume(&TK::Semicolon)?;

        let kind = match assign_op {
            AssignOp::Simple => StmtExprKind::Assign {
                assignee: Box::new(assignee),
                value: Box::new(value),
                init: InitInfo::default(),
            },
            AssignOp::Compound(op) => StmtExprKind::CompoundAssign {
                assignee: Box::new(assignee),
                op,
                value: Box::new(value),
                init: InitInfo::default(),
            },
        };

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind,
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
            span: self.close(marker),
        })
    }

    pub(super) fn parse_defer(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwDefer)?;
        let value = self.parse_expr(0)?;
        self.consume(&TK::Semicolon)?;

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind: StmtExprKind::Defer {
                value: Box::new(value),
            },
            span: self.close(marker),
        })
    }

    pub(super) fn parse_using(&mut self) -> Result<StmtExpr, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwUsing)?;
        let ident = self.parse_ident()?;
        self.consume(&TK::Equals)?;
        let allow_struct_lit = self.allow_struct_lit;
        let allow_ternary_block_branch = self.allow_ternary_block_branch;
        self.allow_struct_lit = false;
        self.allow_ternary_block_branch = false;
        let value = self.parse_expr(0)?;
        self.allow_struct_lit = allow_struct_lit;
        self.allow_ternary_block_branch = allow_ternary_block_branch;
        let body = self.parse_block()?;

        Ok(StmtExpr {
            id: self.id_gen.new_id(),
            kind: StmtExprKind::Using {
                ident,
                value: Box::new(value),
                body: Box::new(body),
            },
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
            span: self.close(marker),
        })
    }
}
