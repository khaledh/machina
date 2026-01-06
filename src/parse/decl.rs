use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_decl(&mut self) -> Result<Decl, ParseError> {
        match &self.curr_token.kind {
            TK::KwType => self.parse_type_decl().map(Decl::TypeDecl),
            TK::KwFn => self.parse_func(),
            TK::Ident(_) if self.peek().map(|t| &t.kind) == Some(&TK::DoubleColon) => {
                self.parse_method_block()
            }
            _ => Err(ParseError::ExpectedDecl(self.curr_token.clone())),
        }
    }

    fn parse_type_decl(&mut self) -> Result<TypeDecl, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwType)?;

        let name = self.parse_ident()?;
        self.consume(&TK::Equals)?;

        let kind = if self.curr_token.kind == TK::LBrace {
            self.parse_struct_def()?
        } else if matches!(&self.curr_token.kind, TK::Ident(_))
            && matches!(
                self.peek().map(|t| &t.kind),
                Some(TK::Pipe) | Some(TK::LParen)
            )
        {
            self.parse_enum_def()?
        } else {
            let ty = self.parse_type_expr()?;

            if self.curr_token.kind == TK::Semicolon {
                self.advance();
            }

            TypeDeclKind::Alias { aliased_ty: ty }
        };

        Ok(TypeDecl {
            id: self.id_gen.new_id(),
            name,
            kind,
            span: self.close(marker),
        })
    }

    fn parse_struct_def(&mut self) -> Result<TypeDeclKind, ParseError> {
        self.consume(&TK::LBrace)?;

        let fields = self.parse_list(TK::Comma, TK::RBrace, |parser| {
            let marker = parser.mark();
            let name = parser.parse_ident()?;
            parser.consume(&TK::Colon)?;
            let ty = parser.parse_type_expr()?;

            Ok(StructField {
                id: parser.id_gen.new_id(),
                name,
                ty,
                span: parser.close(marker),
            })
        })?;

        self.consume(&TK::RBrace)?;
        Ok(TypeDeclKind::Struct { fields })
    }

    fn parse_enum_def(&mut self) -> Result<TypeDeclKind, ParseError> {
        let mut variants = Vec::new();

        let marker = self.mark();

        let name = self.parse_ident()?;

        fn parse_payload(parser: &mut Parser) -> Result<Vec<TypeExpr>, ParseError> {
            if parser.curr_token.kind == TK::LParen {
                parser.advance();
                let tys =
                    parser.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_type_expr())?;
                parser.consume(&TK::RParen)?;
                Ok(tys)
            } else {
                Ok(vec![])
            }
        }

        let payload = parse_payload(self)?;

        variants.push(EnumVariant {
            id: self.id_gen.new_id(),
            name,
            payload,
            span: self.close(marker),
        });

        while self.curr_token.kind == TK::Pipe {
            self.advance();
            let marker = self.mark();
            let name = self.parse_ident()?;
            let payload = parse_payload(self)?;
            variants.push(EnumVariant {
                id: self.id_gen.new_id(),
                name,
                payload,
                span: self.close(marker),
            });
        }

        if self.curr_token.kind == TK::Semicolon {
            self.advance();
        }

        Ok(TypeDeclKind::Enum { variants })
    }
}
