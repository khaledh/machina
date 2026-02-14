use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_top_level_item(&mut self) -> Result<TopLevelItem, ParseError> {
        let attrs = self.parse_attribute_list()?;
        match &self.curr_token.kind {
            TK::KwType => self.parse_type_def(attrs).map(TopLevelItem::TypeDef),
            TK::KwTrait => self.parse_trait_def(attrs).map(TopLevelItem::TraitDef),
            TK::KwTypestate => {
                if !self.options.experimental_typestate {
                    return Err(ParseError::FeatureDisabled {
                        feature: "typestate",
                        span: self.curr_token.span,
                    });
                }
                if attrs.is_empty() {
                    self.parse_typestate_def().map(TopLevelItem::TypestateDef)
                } else {
                    Err(ParseError::AttributeNotAllowed(attrs[0].span))
                }
            }
            TK::KwFn => self.parse_func(attrs),
            TK::Ident(_) if self.peek().map(|t| &t.kind) == Some(&TK::DoubleColon) => {
                if attrs.is_empty() {
                    self.parse_method_block()
                } else {
                    Err(ParseError::AttributeNotAllowed(attrs[0].span))
                }
            }
            _ => Err(ParseError::ExpectedDecl(self.curr_token.clone())),
        }
    }

    fn parse_type_def(&mut self, attrs: Vec<Attribute>) -> Result<TypeDef, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwType)?;

        let name = self.parse_ident()?;
        let type_params = self.parse_type_params()?;
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

            TypeDefKind::Alias { aliased_ty: ty }
        };

        Ok(TypeDef {
            id: self.id_gen.new_id(),
            def_id: (),
            attrs,
            name,
            type_params,
            kind,
            span: self.close(marker),
        })
    }

    fn parse_trait_def(&mut self, attrs: Vec<Attribute>) -> Result<TraitDef, ParseError> {
        let marker = self.mark();
        self.consume_keyword(TK::KwTrait)?;
        let name = self.parse_ident()?;
        self.consume(&TK::LBrace)?;

        let mut methods = Vec::new();
        let mut properties = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            match self.curr_token.kind {
                TK::KwFn => {
                    let method_marker = self.mark();
                    let sig = self.parse_method_sig()?;
                    self.consume(&TK::Semicolon)?;
                    methods.push(TraitMethod {
                        id: self.id_gen.new_id(),
                        sig,
                        span: self.close(method_marker),
                    });
                }
                TK::KwProp => {
                    let prop_marker = self.mark();
                    self.consume_keyword(TK::KwProp)?;
                    let prop_name = self.parse_ident()?;
                    self.consume(&TK::Colon)?;
                    let prop_ty = self.parse_type_expr()?;
                    self.consume(&TK::LBrace)?;

                    let mut has_get = false;
                    let mut has_set = false;
                    while self.curr_token.kind != TK::RBrace {
                        match self.curr_token.kind {
                            TK::KwGet => {
                                has_get = true;
                                self.advance();
                                self.consume(&TK::Semicolon)?;
                            }
                            TK::KwSet => {
                                has_set = true;
                                self.advance();
                                self.consume(&TK::Semicolon)?;
                            }
                            _ => {
                                return Err(ParseError::ExpectedToken(
                                    TK::RBrace,
                                    self.curr_token.clone(),
                                ));
                            }
                        }
                    }
                    self.consume(&TK::RBrace)?;

                    properties.push(TraitProperty {
                        id: self.id_gen.new_id(),
                        name: prop_name,
                        ty: prop_ty,
                        has_get,
                        has_set,
                        span: self.close(prop_marker),
                    });
                }
                _ => {
                    return Err(ParseError::ExpectedToken(
                        TK::RBrace,
                        self.curr_token.clone(),
                    ));
                }
            }
        }

        self.consume(&TK::RBrace)?;
        Ok(TraitDef {
            id: self.id_gen.new_id(),
            def_id: (),
            attrs,
            name,
            methods,
            properties,
            span: self.close(marker),
        })
    }

    fn parse_typestate_def(&mut self) -> Result<TypestateDef, ParseError> {
        let marker = self.mark();
        self.consume_keyword(TK::KwTypestate)?;
        let name = self.parse_ident()?;
        self.consume(&TK::LBrace)?;

        let mut items = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            if self.is_contextual_keyword("fields") {
                items.push(TypestateItem::Fields(self.parse_typestate_fields_block()?));
                continue;
            }
            if self.curr_token.kind == TK::KwFn {
                let func = self.parse_typestate_func_def(format!("{name}$new"))?;
                items.push(TypestateItem::Constructor(func));
                continue;
            }
            if self.is_contextual_keyword("state") {
                items.push(TypestateItem::State(self.parse_typestate_state(&name)?));
                continue;
            }
            return Err(ParseError::ExpectedToken(
                TK::RBrace,
                self.curr_token.clone(),
            ));
        }

        self.consume(&TK::RBrace)?;
        Ok(TypestateDef {
            id: self.id_gen.new_id(),
            def_id: (),
            name,
            items,
            span: self.close(marker),
        })
    }

    fn parse_typestate_state(
        &mut self,
        typestate_name: &str,
    ) -> Result<TypestateState, ParseError> {
        let marker = self.mark();
        self.consume_contextual_keyword("state")?;
        let name = self.parse_ident()?;
        self.consume(&TK::LBrace)?;

        let mut items = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            if self.is_contextual_keyword("fields") {
                items.push(TypestateStateItem::Fields(
                    self.parse_typestate_fields_block()?,
                ));
                continue;
            }
            if self.curr_token.kind == TK::KwFn {
                let base = format!("{typestate_name}${name}");
                let method = self.parse_typestate_func_def(base)?;
                items.push(TypestateStateItem::Method(method));
                continue;
            }
            return Err(ParseError::ExpectedToken(
                TK::RBrace,
                self.curr_token.clone(),
            ));
        }

        self.consume(&TK::RBrace)?;
        Ok(TypestateState {
            id: self.id_gen.new_id(),
            name,
            items,
            span: self.close(marker),
        })
    }

    fn parse_typestate_fields_block(&mut self) -> Result<TypestateFields, ParseError> {
        let marker = self.mark();
        self.consume_contextual_keyword("fields")?;
        self.consume(&TK::LBrace)?;
        let fields = self.parse_list(TK::Comma, TK::RBrace, |parser| {
            parser.parse_struct_def_field()
        })?;
        self.consume(&TK::RBrace)?;
        Ok(TypestateFields {
            id: self.id_gen.new_id(),
            fields,
            span: self.close(marker),
        })
    }

    fn parse_typestate_func_def(&mut self, closure_base: String) -> Result<FuncDef, ParseError> {
        let marker = self.mark();
        let sig = self.parse_func_sig()?;
        if self.curr_token.kind == TK::Semicolon {
            return Err(ParseError::ExpectedToken(
                TK::LBrace,
                self.curr_token.clone(),
            ));
        }

        let prev_base = self.closure_base.clone();
        let prev_index = self.closure_index;
        self.closure_base = Some(format!("{closure_base}${}", sig.name));
        self.closure_index = 0;
        let body = self.parse_block()?;
        self.closure_base = prev_base;
        self.closure_index = prev_index;

        Ok(FuncDef {
            id: self.id_gen.new_id(),
            def_id: (),
            attrs: Vec::new(),
            sig,
            body,
            span: self.close(marker),
        })
    }

    fn parse_struct_def(&mut self) -> Result<TypeDefKind, ParseError> {
        self.consume(&TK::LBrace)?;

        let fields = self.parse_list(TK::Comma, TK::RBrace, |parser| {
            parser.parse_struct_def_field()
        })?;

        self.consume(&TK::RBrace)?;
        Ok(TypeDefKind::Struct { fields })
    }

    fn parse_struct_def_field(&mut self) -> Result<StructDefField, ParseError> {
        let marker = self.mark();
        let name = self.parse_ident()?;
        self.consume(&TK::Colon)?;
        let ty = self.parse_type_expr()?;
        Ok(StructDefField {
            id: self.id_gen.new_id(),
            name,
            ty,
            span: self.close(marker),
        })
    }

    fn parse_enum_def(&mut self) -> Result<TypeDefKind, ParseError> {
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

        variants.push(EnumDefVariant {
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
            variants.push(EnumDefVariant {
                id: self.id_gen.new_id(),
                name,
                payload,
                span: self.close(marker),
            });
        }

        if self.curr_token.kind == TK::Semicolon {
            self.advance();
        }

        Ok(TypeDefKind::Enum { variants })
    }
}
