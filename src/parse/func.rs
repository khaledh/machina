use super::*;

impl<'a> Parser<'a> {
    fn parse_function_sig(&mut self) -> Result<FunctionSig, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwFn)?;

        let name = self.parse_ident()?;

        self.consume(&TK::LParen)?;
        let params = self.parse_params()?;
        self.consume(&TK::RParen)?;

        let return_type = self.parse_return_type()?;

        Ok(FunctionSig {
            name,
            params,
            return_type,
            span: self.close(marker),
        })
    }

    fn parse_return_type(&mut self) -> Result<TypeExpr, ParseError> {
        Ok(match self.curr_token.kind {
            TK::Arrow => {
                self.advance();
                self.parse_type_expr()?
            }
            _ => TypeExpr {
                id: self.id_gen.new_id(),
                kind: TypeExprKind::Named("()".to_string()),
                span: self.close(self.mark()),
            },
        })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_param())
    }

    fn parse_param(&mut self) -> Result<Param, ParseError> {
        let marker = self.mark();
        let mode = self.parse_param_mode();
        let name = self.parse_ident()?;
        self.consume(&TK::Colon)?;
        let typ = self.parse_type_expr()?;

        Ok(Param {
            id: self.id_gen.new_id(),
            name,
            typ,
            mode,
            span: self.close(marker),
        })
    }

    fn parse_param_mode(&mut self) -> ParamMode {
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

    pub(super) fn parse_func(&mut self) -> Result<Decl, ParseError> {
        let marker = self.mark();

        let sig = self.parse_function_sig()?;

        if self.curr_token.kind == TK::Semicolon {
            self.advance();
            Ok(Decl::FunctionDecl(FunctionDecl {
                id: self.id_gen.new_id(),
                sig,
                span: self.close(marker),
            }))
        } else {
            let body = self.parse_block()?;
            Ok(Decl::Function(Function {
                id: self.id_gen.new_id(),
                sig,
                body,
                span: self.close(marker),
            }))
        }
    }

    pub(super) fn parse_method_block(&mut self) -> Result<Decl, ParseError> {
        let marker = self.mark();
        let type_name = self.parse_ident()?;
        self.consume(&TK::DoubleColon)?;
        self.consume(&TK::LBrace)?;

        let mut methods = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            methods.push(self.parse_method()?);
        }
        self.consume(&TK::RBrace)?;

        Ok(Decl::MethodBlock(MethodBlock {
            id: self.id_gen.new_id(),
            type_name,
            methods,
            span: self.close(marker),
        }))
    }

    fn parse_method(&mut self) -> Result<Method, ParseError> {
        let marker = self.mark();
        let sig = self.parse_method_sig()?;
        let body = self.parse_block()?;

        Ok(Method {
            id: self.id_gen.new_id(),
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
            mode: self_mode,
            span: self.close(self_marker),
        };

        let params = if self.curr_token.kind == TK::Comma {
            self.advance();
            self.parse_params()?
        } else {
            Vec::new()
        };

        self.consume(&TK::RParen)?;
        let return_type = self.parse_return_type()?;

        Ok(MethodSig {
            name,
            self_param,
            params,
            return_type,
            span: self.close(marker),
        })
    }
}
