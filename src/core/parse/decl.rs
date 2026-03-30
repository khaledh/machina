use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_top_level_item(&mut self) -> Result<TopLevelItem, ParseError> {
        let doc = self.parse_doc_comment_block();
        let attrs = self.parse_attribute_list()?;
        match &self.curr_token.kind {
            TK::KwType => self.parse_type_def(doc, attrs).map(TopLevelItem::TypeDef),
            TK::KwTrait => self.parse_trait_def(doc, attrs).map(TopLevelItem::TraitDef),
            TK::Ident(ident) if ident == "machine" => {
                if attrs.is_empty() {
                    self.parse_machine_def(doc).map(TopLevelItem::MachineDef)
                } else {
                    Err(PEK::AttributeNotAllowed.at(attrs[0].span))
                }
            }
            TK::KwStatic => self
                .parse_static_def(doc, attrs)
                .map(TopLevelItem::StaticDef),
            TK::KwFn => self.parse_func(doc, attrs),
            TK::Ident(_) if self.is_method_block_start() => {
                if attrs.is_empty() {
                    self.parse_method_block()
                } else {
                    Err(PEK::AttributeNotAllowed.at(attrs[0].span))
                }
            }
            _ => self.err_here(PEK::ExpectedDecl(self.curr_token.clone())),
        }
    }

    fn parse_static_def(
        &mut self,
        doc: Option<DocComment>,
        attrs: Vec<Attribute>,
    ) -> Result<StaticDef, ParseError> {
        let marker = self.mark();
        self.consume_keyword(TK::KwStatic)?;
        let mutability = match self.curr_token.kind {
            TK::KwLet => {
                self.advance();
                StaticMutability::Let
            }
            TK::KwVar => {
                self.advance();
                StaticMutability::Var
            }
            _ => {
                return Err(
                    PEK::ExpectedToken(TK::KwLet, self.curr_token.clone()).at(self.curr_token.span)
                );
            }
        };

        let name = self.parse_ident()?;
        let ty = if self.curr_token.kind == TK::Colon {
            self.advance();
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        self.consume(&TK::Equals)?;
        let init = self.parse_expr(0)?;
        if self.curr_token.kind == TK::Semicolon {
            self.advance();
        }

        Ok(StaticDef {
            id: self.id_gen.new_id(),
            doc,
            attrs,
            name,
            mutability,
            ty,
            init,
            span: self.close(marker),
        })
    }

    fn is_method_block_start(&self) -> bool {
        let mut index = self.pos + 1;
        if self.tokens.get(index).map(|t| &t.kind) == Some(&TK::LessThan) {
            let mut depth = 0usize;
            while index < self.tokens.len() {
                match self.tokens[index].kind {
                    TK::LessThan => depth += 1,
                    TK::GreaterThan => {
                        depth = depth.saturating_sub(1);
                        if depth == 0 {
                            index += 1;
                            break;
                        }
                    }
                    TK::Eof => return false,
                    _ => {}
                }
                index += 1;
            }
        }
        self.tokens.get(index).map(|t| &t.kind) == Some(&TK::DoubleColon)
    }

    fn parse_type_def(
        &mut self,
        doc: Option<DocComment>,
        attrs: Vec<Attribute>,
    ) -> Result<TypeDef, ParseError> {
        let marker = self.mark();

        self.consume_keyword(TK::KwType)?;

        let name = self.parse_ident()?;
        let type_params = self.parse_type_params()?;
        self.consume(&TK::Equals)?;

        let is_linear = attrs.iter().any(|attr| attr.name == "linear");

        let kind = if is_linear {
            TypeDefKind::Linear {
                linear: self.parse_linear_type_def()?,
            }
        } else if self.curr_token.kind == TK::LBrace {
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
            doc,
            attrs,
            name,
            type_params,
            kind,
            span: self.close(marker),
        })
    }

    fn parse_machine_def(&mut self, doc: Option<DocComment>) -> Result<MachineDef, ParseError> {
        let marker = self.mark();
        self.consume_contextual_keyword("machine")?;
        let name = self.parse_ident()?;
        self.consume_contextual_keyword("hosts")?;
        let host = self.parse_machine_host()?;
        self.consume(&TK::LBrace)?;

        let mut items = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            let attrs = self.parse_attribute_list()?;
            if !attrs.is_empty() {
                return Err(PEK::AttributeNotAllowed.at(attrs[0].span));
            }

            if self.is_contextual_keyword("fields") {
                items.push(MachineItem::Fields(self.parse_machine_fields()?));
            } else if self.is_contextual_keyword("action") {
                items.push(MachineItem::Action(
                    self.parse_machine_transition_handler("action", &name)?,
                ));
            } else if self.is_contextual_keyword("trigger") {
                items.push(MachineItem::Trigger(
                    self.parse_machine_transition_handler("trigger", &name)?,
                ));
            } else if self.curr_token.kind == TK::KwOn {
                items.push(MachineItem::On(self.parse_machine_on_handler(&name)?));
            } else if self.curr_token.kind == TK::KwFn {
                items.push(MachineItem::Constructor(
                    self.parse_machine_func_def(&name)?,
                ));
            } else {
                return self.err_here(PEK::ExpectedDecl(self.curr_token.clone()));
            }

            if matches!(self.curr_token.kind, TK::Comma | TK::Semicolon) {
                self.advance();
            }
        }

        self.consume(&TK::RBrace)?;
        Ok(MachineDef {
            id: self.id_gen.new_id(),
            doc,
            name,
            host,
            items,
            span: self.close(marker),
        })
    }

    fn parse_machine_host(&mut self) -> Result<MachineHost, ParseError> {
        let marker = self.mark();
        let type_name = self.parse_ident()?;
        self.consume(&TK::LParen)?;
        self.consume_contextual_keyword("key")?;
        self.consume(&TK::Colon)?;
        let key_field = self.parse_ident()?;
        self.consume(&TK::RParen)?;
        Ok(MachineHost {
            id: self.id_gen.new_id(),
            type_name,
            key_field,
            span: self.close(marker),
        })
    }

    fn parse_machine_fields(&mut self) -> Result<MachineFields, ParseError> {
        let marker = self.mark();
        self.consume_contextual_keyword("fields")?;
        self.consume(&TK::LBrace)?;
        let fields = self.parse_list(TK::Comma, TK::RBrace, |parser| {
            parser.parse_struct_def_field()
        })?;
        self.consume(&TK::RBrace)?;
        Ok(MachineFields {
            id: self.id_gen.new_id(),
            fields,
            span: self.close(marker),
        })
    }

    fn parse_machine_func_def(&mut self, machine_name: &str) -> Result<FuncDef, ParseError> {
        let marker = self.mark();
        let sig = self.parse_func_sig()?;
        if self.curr_token.kind == TK::Semicolon {
            return self.expected_token(TK::LBrace);
        }

        let prev_base = self.closure_base.clone();
        let prev_index = self.closure_index;
        self.closure_base = Some(format!("{machine_name}${}", sig.name));
        self.closure_index = 0;
        let body = self.parse_block()?;
        self.closure_base = prev_base;
        self.closure_index = prev_index;

        Ok(FuncDef {
            id: self.id_gen.new_id(),
            doc: None,
            attrs: Vec::new(),
            sig,
            body,
            span: self.close(marker),
        })
    }

    fn parse_machine_transition_handler(
        &mut self,
        keyword: &str,
        machine_name: &str,
    ) -> Result<MachineTransitionHandler, ParseError> {
        let marker = self.mark();
        self.consume_contextual_keyword(keyword)?;
        let name = self.parse_ident()?;
        self.consume(&TK::LParen)?;
        let instance_param = self.parse_ident()?;
        let params = if self.curr_token.kind == TK::Comma {
            self.advance();
            self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_param())?
        } else {
            Vec::new()
        };
        self.consume(&TK::RParen)?;
        let ret_ty_expr = if keyword == "action" {
            self.consume(&TK::Arrow)?;
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        let prev_base = self.closure_base.clone();
        let prev_index = self.closure_index;
        self.closure_base = Some(format!("{machine_name}${keyword}${name}"));
        self.closure_index = 0;
        let body = self.parse_block()?;
        self.closure_base = prev_base;
        self.closure_index = prev_index;

        Ok(MachineTransitionHandler {
            id: self.id_gen.new_id(),
            name,
            instance_param,
            params,
            ret_ty_expr,
            body,
            span: self.close(marker),
        })
    }

    fn parse_machine_on_handler(
        &mut self,
        machine_name: &str,
    ) -> Result<MachineOnHandler, ParseError> {
        let marker = self.mark();
        self.consume_keyword(TK::KwOn)?;
        let selector_ty = self.parse_type_expr()?;
        let params = if self.curr_token.kind == TK::LParen {
            self.consume(&TK::LParen)?;
            let params = self.parse_on_handler_params(&selector_ty)?;
            self.consume(&TK::RParen)?;
            params
        } else {
            Vec::new()
        };
        let provenance = if self.curr_token.kind == TK::KwFor {
            Some(self.parse_on_handler_provenance()?)
        } else {
            None
        };

        let prev_base = self.closure_base.clone();
        let prev_index = self.closure_index;
        self.closure_base = Some(format!("{machine_name}$on"));
        self.closure_index = 0;
        let body = self.parse_block()?;
        self.closure_base = prev_base;
        self.closure_index = prev_index;

        Ok(MachineOnHandler {
            id: self.id_gen.new_id(),
            selector_ty,
            params,
            provenance,
            body,
            span: self.close(marker),
        })
    }

    fn parse_trait_def(
        &mut self,
        doc: Option<DocComment>,
        attrs: Vec<Attribute>,
    ) -> Result<TraitDef, ParseError> {
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
                            _ => return self.expected_token(TK::RBrace),
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
                _ => return self.expected_token(TK::RBrace),
            }
        }

        self.consume(&TK::RBrace)?;
        Ok(TraitDef {
            id: self.id_gen.new_id(),
            doc,
            attrs,
            name,
            methods,
            properties,
            span: self.close(marker),
        })
    }

    fn parse_on_handler_provenance(&mut self) -> Result<OnHandlerProvenance, ParseError> {
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
        Ok(OnHandlerProvenance {
            param: Param {
                id: self.id_gen.new_id(),
                ident: binding,
                typ: request_ty,
                mode: ParamMode::In,
                default: None,
                span: self.close(marker),
            },
            request_site_label,
        })
    }

    fn parse_on_handler_params(
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
                    typ: TypeExpr {
                        id: self.id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: "Pending".to_string(),
                            type_args: vec![response_ty.clone()],
                        },
                        span: pending_span,
                    },
                    mode: ParamMode::In,
                    default: None,
                    span: pending_span,
                },
                Param {
                    id: self.id_gen.new_id(),
                    ident: "__response".to_string(),
                    typ: response_ty,
                    mode: ParamMode::In,
                    default: None,
                    span: response_span,
                },
            ]);
        }

        self.parse_list(TK::Comma, TK::RParen, |parser| {
            parser.parse_on_handler_param(selector_ty)
        })
    }

    fn parse_on_handler_param(&mut self, selector_ty: &TypeExpr) -> Result<Param, ParseError> {
        let marker = self.mark();
        if matches!(self.curr_token.kind, TK::Ident(_))
            && self.peek().map(|t| &t.kind) != Some(&TK::Colon)
        {
            let ident = self.parse_ident()?;
            let span = self.close(marker);
            return Ok(Param {
                id: self.id_gen.new_id(),
                ident,
                // `on Ping(p)` shorthand: payload type defaults to selector type.
                typ: self.clone_type_expr_with_new_ids(selector_ty),
                mode: ParamMode::In,
                default: None,
                span,
            });
        }
        self.parse_param()
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
        let attrs = self.parse_attribute_list()?;
        let name = self.parse_ident()?;
        self.consume(&TK::Colon)?;
        let ty = self.parse_type_expr()?;
        Ok(StructDefField {
            id: self.id_gen.new_id(),
            attrs,
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

    fn parse_linear_type_def(&mut self) -> Result<LinearTypeDef, ParseError> {
        self.consume(&TK::LBrace)?;

        let mut fields = Vec::new();
        let mut states = Vec::new();
        let mut actions = Vec::new();
        let mut triggers = Vec::new();
        let mut roles = Vec::new();

        while self.curr_token.kind != TK::RBrace {
            let attrs = self.parse_attribute_list()?;
            if self.is_contextual_keyword("states") {
                if !attrs.is_empty() {
                    return Err(PEK::AttributeNotAllowed.at(attrs[0].span));
                }
                states = self.parse_linear_states_block()?;
            } else if self.is_contextual_keyword("actions") {
                if !attrs.is_empty() {
                    return Err(PEK::AttributeNotAllowed.at(attrs[0].span));
                }
                actions = self.parse_linear_transition_block("actions")?;
            } else if self.is_contextual_keyword("triggers") {
                if !attrs.is_empty() {
                    return Err(PEK::AttributeNotAllowed.at(attrs[0].span));
                }
                triggers = self.parse_linear_transition_block("triggers")?;
            } else if self.is_contextual_keyword("roles") {
                if !attrs.is_empty() {
                    return Err(PEK::AttributeNotAllowed.at(attrs[0].span));
                }
                roles = self.parse_linear_roles_block()?;
            } else {
                if !attrs.is_empty() {
                    return Err(PEK::AttributeNotAllowed.at(attrs[0].span));
                }
                fields.push(self.parse_struct_def_field()?);
            }

            if matches!(self.curr_token.kind, TK::Comma | TK::Semicolon) {
                self.advance();
            }
        }

        self.consume(&TK::RBrace)?;
        Ok(LinearTypeDef {
            fields,
            states,
            actions,
            triggers,
            roles,
        })
    }

    fn parse_linear_states_block(&mut self) -> Result<Vec<LinearStateVariant>, ParseError> {
        self.consume_contextual_keyword("states")?;
        self.consume(&TK::LBrace)?;

        let mut states = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            let attrs = self.parse_attribute_list()?;
            let marker = self.mark();
            let name = self.parse_ident()?;
            let payload = if self.curr_token.kind == TK::LParen {
                self.advance();
                let tys =
                    self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_type_expr())?;
                self.consume(&TK::RParen)?;
                tys
            } else {
                Vec::new()
            };
            states.push(LinearStateVariant {
                id: self.id_gen.new_id(),
                attrs,
                name,
                payload,
                span: self.close(marker),
            });
            if self.curr_token.kind == TK::Comma {
                self.advance();
            }
        }

        self.consume(&TK::RBrace)?;
        Ok(states)
    }

    fn parse_linear_transition_block(
        &mut self,
        keyword: &str,
    ) -> Result<Vec<LinearTransitionDecl>, ParseError> {
        self.consume_contextual_keyword(keyword)?;
        self.consume(&TK::LBrace)?;

        let mut decls = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            decls.push(self.parse_linear_transition_decl()?);
            if self.curr_token.kind == TK::Comma {
                self.advance();
            }
        }

        self.consume(&TK::RBrace)?;
        Ok(decls)
    }

    fn parse_linear_transition_decl(&mut self) -> Result<LinearTransitionDecl, ParseError> {
        let marker = self.mark();
        let name = self.parse_ident()?;
        let params = if self.curr_token.kind == TK::LParen {
            self.advance();
            let params = self.parse_list(TK::Comma, TK::RParen, |parser| {
                let marker = parser.mark();
                let name = parser.parse_ident()?;
                parser.consume(&TK::Colon)?;
                let ty = parser.parse_type_expr()?;
                Ok(LinearTransitionParam {
                    id: parser.id_gen.new_id(),
                    name,
                    ty,
                    span: parser.close(marker),
                })
            })?;
            self.consume(&TK::RParen)?;
            params
        } else {
            Vec::new()
        };
        self.consume(&TK::Colon)?;
        let source_state = self.parse_ident()?;
        self.consume(&TK::Arrow)?;
        let target_state = self.parse_ident()?;
        let error_ty_expr = if self.curr_token.kind == TK::Pipe {
            self.advance();
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        Ok(LinearTransitionDecl {
            id: self.id_gen.new_id(),
            name,
            params,
            source_state,
            target_state,
            error_ty_expr,
            span: self.close(marker),
        })
    }

    fn parse_linear_roles_block(&mut self) -> Result<Vec<LinearRoleDecl>, ParseError> {
        self.consume_contextual_keyword("roles")?;
        self.consume(&TK::LBrace)?;

        let mut roles = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            let marker = self.mark();
            let name = self.parse_ident()?;
            self.consume(&TK::LBrace)?;
            let allowed_actions =
                self.parse_list(TK::Comma, TK::RBrace, |parser| parser.parse_ident())?;
            self.consume(&TK::RBrace)?;
            roles.push(LinearRoleDecl {
                id: self.id_gen.new_id(),
                name,
                allowed_actions,
                span: self.close(marker),
            });
            if self.curr_token.kind == TK::Comma {
                self.advance();
            }
        }

        self.consume(&TK::RBrace)?;
        Ok(roles)
    }
}
