use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_top_level_item(&mut self) -> Result<TopLevelItem, ParseError> {
        let attrs = self.parse_attribute_list()?;
        match &self.curr_token.kind {
            TK::KwType => self.parse_type_def(attrs).map(TopLevelItem::TypeDef),
            TK::KwTrait => self.parse_trait_def(attrs).map(TopLevelItem::TraitDef),
            TK::KwProtocol => {
                if !self.options.experimental_typestate {
                    return Err(ParseError::FeatureDisabled {
                        feature: "typestate",
                        span: self.curr_token.span,
                    });
                }
                if attrs.is_empty() {
                    self.parse_protocol_def().map(TopLevelItem::ProtocolDef)
                } else {
                    Err(ParseError::AttributeNotAllowed(attrs[0].span))
                }
            }
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

    fn parse_protocol_def(&mut self) -> Result<ProtocolDef, ParseError> {
        let marker = self.mark();
        self.consume_keyword(TK::KwProtocol)?;
        let name = self.parse_ident()?;
        self.consume(&TK::LBrace)?;

        let mut roles = Vec::new();
        let mut flows = Vec::new();

        while self.curr_token.kind != TK::RBrace {
            match self.curr_token.kind {
                TK::KwRole => {
                    let role_marker = self.mark();
                    self.consume_keyword(TK::KwRole)?;
                    let role_name = self.parse_ident()?;
                    self.consume(&TK::Semicolon)?;
                    roles.push(ProtocolRole {
                        id: self.id_gen.new_id(),
                        def_id: (),
                        name: role_name,
                        span: self.close(role_marker),
                    });
                }
                TK::KwFlow => {
                    let flow_marker = self.mark();
                    self.consume_keyword(TK::KwFlow)?;
                    let from_role = self.parse_ident()?;
                    self.consume(&TK::Arrow)?;
                    let to_role = self.parse_ident()?;
                    self.consume(&TK::Colon)?;
                    let payload_ty = self.parse_type_expr()?;

                    let response_tys = if self.curr_token.kind == TK::Arrow {
                        self.consume(&TK::Arrow)?;
                        let response_union = self.parse_type_expr()?;
                        match response_union.kind {
                            TypeExprKind::Union { variants } => variants,
                            _ => vec![response_union],
                        }
                    } else {
                        Vec::new()
                    };

                    self.consume(&TK::Semicolon)?;
                    flows.push(ProtocolFlow {
                        id: self.id_gen.new_id(),
                        from_role,
                        to_role,
                        payload_ty,
                        response_tys,
                        span: self.close(flow_marker),
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
        Ok(ProtocolDef {
            id: self.id_gen.new_id(),
            def_id: (),
            name,
            roles,
            flows,
            span: self.close(marker),
        })
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
        let role_impls = if self.curr_token.kind == TK::Colon {
            self.consume(&TK::Colon)?;
            self.parse_typestate_role_impls()?
        } else {
            Vec::new()
        };
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
            if self.curr_token.kind == TK::KwOn {
                items.push(TypestateItem::Handler(self.parse_typestate_on_handler()?));
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
            role_impls,
            items,
            span: self.close(marker),
        })
    }

    fn parse_typestate_role_impls(&mut self) -> Result<Vec<TypestateRoleImpl>, ParseError> {
        self.parse_list(TK::Comma, TK::LBrace, |parser| {
            let marker = parser.mark();
            let mut path = vec![parser.parse_ident()?];
            while parser.curr_token.kind == TK::DoubleColon {
                parser.consume(&TK::DoubleColon)?;
                path.push(parser.parse_ident()?);
            }
            Ok(TypestateRoleImpl {
                id: parser.id_gen.new_id(),
                def_id: (),
                path,
                span: parser.close(marker),
            })
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
            if self.curr_token.kind == TK::KwOn {
                items.push(TypestateStateItem::Handler(
                    self.parse_typestate_on_handler()?,
                ));
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

    fn parse_typestate_on_handler(&mut self) -> Result<TypestateOnHandler, ParseError> {
        let marker = self.mark();
        self.consume_keyword(TK::KwOn)?;
        let selector_ty = self.parse_type_expr()?;
        let params = if self.curr_token.kind == TK::LParen {
            self.consume(&TK::LParen)?;
            let params = self.parse_typestate_on_handler_params(&selector_ty)?;
            self.consume(&TK::RParen)?;
            params
        } else {
            Vec::new()
        };
        let provenance = if self.curr_token.kind == TK::KwFor {
            Some(self.parse_typestate_on_handler_provenance()?)
        } else {
            None
        };
        let ret_ty_expr = if self.curr_token.kind == TK::Arrow {
            self.consume(&TK::Arrow)?;
            self.parse_type_expr()?
        } else {
            // Handler shorthand without `->` defaults to same-state (`stay`).
            self.typestate_stay_type_expr(selector_ty.span)
        };
        let body = self.parse_block()?;
        Ok(TypestateOnHandler {
            id: self.id_gen.new_id(),
            selector_ty,
            params,
            provenance,
            ret_ty_expr,
            body,
            span: self.close(marker),
        })
    }

    fn parse_typestate_on_handler_provenance(
        &mut self,
    ) -> Result<TypestateHandlerProvenance, ParseError> {
        let marker = self.mark();
        self.consume_keyword(TK::KwFor)?;
        // Parse provenance request type in a mode that leaves `:label(...)`
        // available for request-site disambiguation syntax.
        let request_ty = self.parse_type_expr_no_refinement()?;
        let request_site_label = if self.curr_token.kind == TK::Colon {
            self.consume(&TK::Colon)?;
            Some(self.parse_ident()?)
        } else {
            None
        };
        self.consume(&TK::LParen)?;
        let binding = self.parse_ident()?;
        self.consume(&TK::RParen)?;
        Ok(TypestateHandlerProvenance {
            param: Param {
                id: self.id_gen.new_id(),
                ident: binding,
                def_id: (),
                typ: request_ty,
                mode: ParamMode::In,
                span: self.close(marker),
            },
            request_site_label,
        })
    }

    fn parse_typestate_on_handler_params(
        &mut self,
        selector_ty: &TypeExpr,
    ) -> Result<Vec<Param>, ParseError> {
        if self.curr_token.kind == TK::RParen {
            return Ok(Vec::new());
        }

        // Pattern-form sugar:
        //   on Response(pending, AuthApproved) -> ...
        // Desugars to canonical params:
        //   (pending: Pending<AuthApproved>, __response: AuthApproved)
        if matches!(self.curr_token.kind, TK::Ident(_))
            && self.peek().map(|t| &t.kind) == Some(&TK::Comma)
        {
            let pending_marker = self.mark();
            let pending_ident = self.parse_ident()?;
            let pending_span = self.close(pending_marker);
            self.consume(&TK::Comma)?;
            let response_ty = self.parse_type_expr()?;
            let response_span = response_ty.span;

            return Ok(vec![
                Param {
                    id: self.id_gen.new_id(),
                    ident: pending_ident,
                    def_id: (),
                    typ: TypeExpr {
                        id: self.id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: "Pending".to_string(),
                            def_id: (),
                            type_args: vec![response_ty.clone()],
                        },
                        span: pending_span,
                    },
                    mode: ParamMode::In,
                    span: pending_span,
                },
                Param {
                    id: self.id_gen.new_id(),
                    ident: "__response".to_string(),
                    def_id: (),
                    typ: response_ty,
                    mode: ParamMode::In,
                    span: response_span,
                },
            ]);
        }

        self.parse_list(TK::Comma, TK::RParen, |parser| {
            parser.parse_typestate_handler_param(selector_ty)
        })
    }

    fn parse_typestate_handler_param(
        &mut self,
        selector_ty: &TypeExpr,
    ) -> Result<Param, ParseError> {
        let marker = self.mark();
        if matches!(self.curr_token.kind, TK::Ident(_))
            && self.peek().map(|t| &t.kind) != Some(&TK::Colon)
        {
            let ident = self.parse_ident()?;
            let span = self.close(marker);
            return Ok(Param {
                id: self.id_gen.new_id(),
                ident,
                def_id: (),
                // `on Ping(p)` shorthand: payload type defaults to selector type.
                typ: self.clone_type_expr_with_new_ids(selector_ty),
                mode: ParamMode::In,
                span,
            });
        }
        self.parse_param()
    }

    fn typestate_stay_type_expr(&mut self, span: Span) -> TypeExpr {
        TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Named {
                // Internal marker consumed by typestate desugaring.
                ident: "stay".to_string(),
                def_id: (),
                type_args: Vec::new(),
            },
            span,
        }
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
