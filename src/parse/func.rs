use super::*;

impl<'a> Parser<'a> {
    // --- Functions ---

    pub(super) fn parse_func(&mut self) -> Result<TopLevelItem, ParseError> {
        let marker = self.mark();

        let sig = self.parse_func_sig()?;

        if self.curr_token.kind == TK::Semicolon {
            self.advance();
            Ok(TopLevelItem::FuncDecl(FuncDecl {
                id: self.id_gen.new_id(),
                def_id: (),
                sig,
                span: self.close(marker),
            }))
        } else {
            let prev_base = self.closure_base.clone();
            let prev_index = self.closure_index;
            self.closure_base = Some(sig.name.clone());
            self.closure_index = 0;

            let body = self.parse_block()?;

            self.closure_base = prev_base;
            self.closure_index = prev_index;

            Ok(TopLevelItem::FuncDef(FuncDef {
                id: self.id_gen.new_id(),
                def_id: (),
                sig,
                body,
                span: self.close(marker),
            }))
        }
    }

    fn parse_func_sig(&mut self) -> Result<FunctionSig, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwFn)?;

        let name = self.parse_ident()?;

        self.consume(&TK::LParen)?;
        let params = self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_param())?;
        self.consume(&TK::RParen)?;

        let ret_ty_expr = self.parse_ret_type()?;

        Ok(FunctionSig {
            name,
            params,
            ret_ty_expr,
            span: self.close(marker),
        })
    }

    // --- Methods ---

    pub(super) fn parse_method_block(&mut self) -> Result<TopLevelItem, ParseError> {
        let marker = self.mark();
        let type_name = self.parse_ident()?;
        self.consume(&TK::DoubleColon)?;
        self.consume(&TK::LBrace)?;

        let mut methods = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            methods.push(self.parse_method(&type_name)?);
        }
        self.consume(&TK::RBrace)?;

        Ok(TopLevelItem::MethodBlock(MethodBlock {
            id: self.id_gen.new_id(),
            type_name,
            method_defs: methods,
            span: self.close(marker),
        }))
    }

    fn parse_method(&mut self, type_name: &str) -> Result<MethodDef, ParseError> {
        let marker = self.mark();
        let sig = self.parse_method_sig()?;

        let prev_base = self.closure_base.clone();
        let prev_index = self.closure_index;
        self.closure_base = Some(format!("{}${}", type_name, sig.name));
        self.closure_index = 0;

        let body = self.parse_block()?;

        self.closure_base = prev_base;
        self.closure_index = prev_index;

        Ok(MethodDef {
            id: self.id_gen.new_id(),
            def_id: (),
            sig,
            body,
            span: self.close(marker),
        })
    }

    fn parse_method_sig(&mut self) -> Result<MethodSig, ParseError> {
        let marker = self.mark();
        self.consume_keyword(TK::KwFn)?;
        let name = self.parse_ident()?;
        self.consume(&TK::LParen)?;

        let self_marker = self.mark();
        let self_mode = self.parse_param_mode();
        if self.curr_token.kind != TK::KwSelf {
            return Err(ParseError::ExpectedSelf(self.curr_token.clone()));
        }
        self.advance();
        let self_param = SelfParam {
            id: self.id_gen.new_id(),
            def_id: (),
            mode: self_mode,
            span: self.close(self_marker),
        };

        let params = if self.curr_token.kind == TK::Comma {
            self.advance();
            self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_param())?
        } else {
            Vec::new()
        };

        self.consume(&TK::RParen)?;
        let ret_ty_expr = self.parse_ret_type()?;

        Ok(MethodSig {
            name,
            self_param,
            params,
            ret_ty_expr,
            span: self.close(marker),
        })
    }

    // --- Closures ---

    pub(super) fn parse_closure(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        let params = if self.curr_token.kind == TK::LogicalOr {
            self.advance();
            Vec::new()
        } else {
            self.consume(&TK::Pipe)?;
            if self.curr_token.kind == TK::Pipe {
                self.advance();
                Vec::new()
            } else {
                let params = self.parse_list(TK::Comma, TK::Pipe, |parser| parser.parse_param())?;
                self.consume(&TK::Pipe)?;
                params
            }
        };

        let return_ty = self.parse_ret_type()?;

        let body = self.parse_expr(0)?;
        let ident = self.next_closure_ident();

        let closure_expr = Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Closure {
                ident: ident.clone(),
                def_id: (),
                params: params.clone(),
                return_ty: return_ty.clone(),
                body: Box::new(body.clone()), // TODO: see if we can restructure this to avoid cloning
            },
            ty: (),
            span: self.close(marker),
        };

        // Record the closure declaration (to be included in the module decls)
        let closure_decl = ClosureDecl {
            id: self.id_gen.new_id(),
            def_id: (),
            sig: ClosureSig {
                name: ident,
                params,
                return_ty,
                span: self.close(marker),
            },
            body: body,
            span: self.close(marker),
        };
        self.closure_decls
            .push(TopLevelItem::ClosureDecl(closure_decl.clone()));

        Ok(closure_expr)
    }

    fn next_closure_ident(&mut self) -> String {
        self.closure_index += 1;
        let base = self.closure_base.as_deref().unwrap_or("anon").to_string();
        format!("{base}$closure${}", self.closure_index)
    }

    // --- Params & Return Type ---

    pub(super) fn parse_param(&mut self) -> Result<Param, ParseError> {
        let marker = self.mark();
        let mode = self.parse_param_mode();
        let name = self.parse_ident()?;
        self.consume(&TK::Colon)?;
        let typ = self.parse_type_expr()?;

        Ok(Param {
            id: self.id_gen.new_id(),
            ident: name,
            def_id: (),
            typ,
            mode,
            span: self.close(marker),
        })
    }

    pub(super) fn parse_param_mode(&mut self) -> ParamMode {
        match &self.curr_token.kind {
            TK::KwInOut => {
                self.advance();
                ParamMode::InOut
            }
            TK::KwSink => {
                self.advance();
                ParamMode::Sink
            }
            TK::KwOut => {
                self.advance();
                ParamMode::Out
            }
            _ => ParamMode::In,
        }
    }

    fn parse_ret_type(&mut self) -> Result<TypeExpr, ParseError> {
        Ok(match self.curr_token.kind {
            TK::Arrow => {
                self.advance();
                self.parse_type_expr()?
            }
            _ => TypeExpr {
                id: self.id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: "()".to_string(),
                    def_id: (),
                },
                span: self.close(self.mark()),
            },
        })
    }
}
