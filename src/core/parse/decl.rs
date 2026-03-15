use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_top_level_item(&mut self) -> Result<TopLevelItem, ParseError> {
        let attrs = self.parse_attribute_list()?;
        match &self.curr_token.kind {
            TK::KwType => self.parse_type_def(attrs).map(TopLevelItem::TypeDef),
            TK::KwTrait => self.parse_trait_def(attrs).map(TopLevelItem::TraitDef),
            TK::Ident(ident) if ident == "machine" => {
                if attrs.is_empty() {
                    self.parse_machine_def().map(TopLevelItem::MachineDef)
                } else {
                    Err(PEK::AttributeNotAllowed.at(attrs[0].span))
                }
            }
            TK::KwProtocol => {
                if attrs.is_empty() {
                    self.err_here(PEK::FeatureRetired {
                        feature: "typestate",
                    })
                } else {
                    Err(PEK::AttributeNotAllowed.at(attrs[0].span))
                }
            }
            TK::KwTypestate => {
                if !self.options.experimental_typestate {
                    return self.err_here(PEK::FeatureRetired {
                        feature: "typestate",
                    });
                }
                if attrs.is_empty() {
                    self.parse_typestate_def().map(TopLevelItem::TypestateDef)
                } else {
                    Err(PEK::AttributeNotAllowed.at(attrs[0].span))
                }
            }
            TK::KwFn => self.parse_func(attrs),
            TK::Ident(_) if self.peek().map(|t| &t.kind) == Some(&TK::DoubleColon) => {
                if attrs.is_empty() {
                    self.parse_method_block()
                } else {
                    Err(PEK::AttributeNotAllowed.at(attrs[0].span))
                }
            }
            _ => self.err_here(PEK::ExpectedDecl(self.curr_token.clone())),
        }
    }

    fn parse_type_def(&mut self, attrs: Vec<Attribute>) -> Result<TypeDef, ParseError> {
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
            attrs,
            name,
            type_params,
            kind,
            span: self.close(marker),
        })
    }

    fn parse_machine_def(&mut self) -> Result<MachineDef, ParseError> {
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
            let attrs = self.parse_attribute_list()?;
            if self.is_contextual_keyword("fields") {
                if !attrs.is_empty() {
                    return Err(PEK::AttributeNotAllowed.at(attrs[0].span));
                }
                items.push(TypestateItem::Fields(self.parse_typestate_fields_block()?));
                continue;
            }
            if self.curr_token.kind == TK::KwFn {
                if !attrs.is_empty() {
                    return Err(PEK::AttributeNotAllowed.at(attrs[0].span));
                }
                let func = self.parse_typestate_func_def(format!("{name}$new"))?;
                items.push(TypestateItem::Constructor(func));
                continue;
            }
            if self.curr_token.kind == TK::KwOn {
                if !attrs.is_empty() {
                    return Err(PEK::AttributeNotAllowed.at(attrs[0].span));
                }
                items.push(TypestateItem::Handler(self.parse_typestate_on_handler()?));
                continue;
            }
            if self.is_contextual_keyword("state") {
                items.push(TypestateItem::State(
                    self.parse_typestate_state(attrs, &name)?,
                ));
                continue;
            }
            return self.expected_token(TK::RBrace);
        }

        self.consume(&TK::RBrace)?;
        Ok(TypestateDef {
            id: self.id_gen.new_id(),
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
                path,
                span: parser.close(marker),
            })
        })
    }

    fn parse_typestate_state(
        &mut self,
        attrs: Vec<Attribute>,
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
            return self.expected_token(TK::RBrace);
        }

        self.consume(&TK::RBrace)?;
        Ok(TypestateState {
            id: self.id_gen.new_id(),
            attrs,
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
                    typ: TypeExpr {
                        id: self.id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: "Pending".to_string(),
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
                type_args: Vec::new(),
            },
            span,
        }
    }

    fn parse_typestate_fields_block(&mut self) -> Result<TypestateFields, ParseError> {
        let marker = self.mark();
        self.consume_contextual_keyword("fields")?;
        self.consume(&TK::LBrace)?;
        let mut fields = Vec::new();
        let mut role_bindings = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            let (field, maybe_binding) = self.parse_typestate_struct_field()?;
            fields.push(field);
            if let Some(binding) = maybe_binding {
                role_bindings.push(binding);
            }
            if self.curr_token.kind == TK::Comma {
                self.advance();
            }
        }
        self.consume(&TK::RBrace)?;
        Ok(TypestateFields {
            id: self.id_gen.new_id(),
            fields,
            role_bindings,
            span: self.close(marker),
        })
    }

    fn parse_typestate_struct_field(
        &mut self,
    ) -> Result<(StructDefField, Option<TypestateFieldRoleBinding>), ParseError> {
        let field = self.parse_struct_def_field()?;
        // Typestate fields optionally annotate peer-role intent:
        // `peer: Machine<PeerType> as Server`.
        let maybe_binding = if self.is_contextual_keyword("as") {
            let marker = self.mark();
            self.consume_contextual_keyword("as")?;
            let role_name = self.parse_ident()?;
            Some(TypestateFieldRoleBinding {
                id: self.id_gen.new_id(),
                field_name: field.name.clone(),
                role_name,
                span: self.close(marker),
            })
        } else {
            None
        };
        Ok((field, maybe_binding))
    }

    fn parse_typestate_func_def(&mut self, closure_base: String) -> Result<FuncDef, ParseError> {
        let marker = self.mark();
        let sig = self.parse_func_sig()?;
        if self.curr_token.kind == TK::Semicolon {
            return self.expected_token(TK::LBrace);
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
