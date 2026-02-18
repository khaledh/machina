use super::*;
use std::collections::HashMap;

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

        let mut messages = Vec::new();
        let mut request_contracts = Vec::new();
        let mut roles = Vec::new();
        let mut message_aliases: HashMap<String, TypeExpr> = HashMap::new();

        while self.curr_token.kind != TK::RBrace {
            match self.curr_token.kind {
                TK::KwRole => {
                    self.parse_protocol_role_decl_or_block(&mut roles, &message_aliases)?;
                }
                TK::KwFlow => {
                    self.parse_protocol_flow_decl(&mut request_contracts, &message_aliases)?;
                }
                TK::Ident(_) if self.is_contextual_keyword("msg") => {
                    self.parse_protocol_msg_decl(&mut messages, &mut message_aliases)?;
                }
                TK::Ident(_) if self.is_contextual_keyword("req") => {
                    self.parse_protocol_req_decl(&mut request_contracts, &message_aliases)?;
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
            messages,
            request_contracts,
            roles,
            span: self.close(marker),
        })
    }

    fn parse_protocol_role_decl_or_block(
        &mut self,
        roles: &mut Vec<ProtocolRole>,
        messages: &HashMap<String, TypeExpr>,
    ) -> Result<(), ParseError> {
        let role_marker = self.mark();
        self.consume_keyword(TK::KwRole)?;
        let role_name = self.parse_ident()?;
        let role_span = self.close(role_marker);
        let role_index =
            Self::ensure_protocol_role(roles, &role_name, role_span, self.id_gen.new_id());

        if self.curr_token.kind == TK::Semicolon {
            self.consume(&TK::Semicolon)?;
            return Ok(());
        }

        self.consume(&TK::LBrace)?;
        while self.curr_token.kind != TK::RBrace {
            let state = self.parse_protocol_state_block(messages)?;
            roles[role_index].states.push(state);
        }
        self.consume(&TK::RBrace)?;
        Ok(())
    }

    fn parse_protocol_state_block(
        &mut self,
        messages: &HashMap<String, TypeExpr>,
    ) -> Result<ProtocolState, ParseError> {
        let marker = self.mark();
        self.consume_contextual_keyword("state")?;
        let state_name = self.parse_ident()?;

        if self.curr_token.kind == TK::Semicolon {
            self.consume(&TK::Semicolon)?;
            return Ok(ProtocolState {
                id: self.id_gen.new_id(),
                name: state_name,
                transitions: Vec::new(),
                span: self.close(marker),
            });
        }

        self.consume(&TK::LBrace)?;
        let mut transitions = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            transitions.push(self.parse_protocol_transition(messages)?);
        }
        self.consume(&TK::RBrace)?;
        Ok(ProtocolState {
            id: self.id_gen.new_id(),
            name: state_name,
            transitions,
            span: self.close(marker),
        })
    }

    fn parse_protocol_transition(
        &mut self,
        messages: &HashMap<String, TypeExpr>,
    ) -> Result<ProtocolTransition, ParseError> {
        let transition_marker = self.mark();
        self.consume_keyword(TK::KwOn)?;
        let trigger_ty = self.parse_type_expr()?;
        let trigger_from_role = if self.curr_token.kind == TK::At {
            self.consume(&TK::At)?;
            Some(self.parse_ident()?)
        } else {
            None
        };
        if trigger_from_role.is_none() && !Self::is_protocol_start_trigger(&trigger_ty) {
            return Err(ParseError::ExpectedToken(TK::At, self.curr_token.clone()));
        }

        self.consume(&TK::Arrow)?;
        let next_state = self.parse_ident()?;
        let effects = self.parse_protocol_transition_body(messages)?;
        let trigger_payload_ty = self.resolve_protocol_message_ty(&trigger_ty, messages);

        Ok(ProtocolTransition {
            id: self.id_gen.new_id(),
            trigger: ProtocolTrigger {
                selector_ty: self.clone_type_expr_with_new_ids(&trigger_payload_ty),
                from_role: trigger_from_role,
            },
            next_state,
            effects,
            span: self.close(transition_marker),
        })
    }

    fn parse_protocol_transition_body(
        &mut self,
        messages: &HashMap<String, TypeExpr>,
    ) -> Result<Vec<ProtocolEffect>, ParseError> {
        if self.curr_token.kind == TK::Semicolon {
            self.consume(&TK::Semicolon)?;
            return Ok(Vec::new());
        }

        self.consume(&TK::LBrace)?;
        let mut effects = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            self.consume_contextual_keyword("effects")?;
            self.consume(&TK::Colon)?;
            self.consume(&TK::LBracket)?;
            let parsed_effects = self.parse_list(TK::Comma, TK::RBracket, |parser| {
                let effect_marker = parser.mark();
                let effect_ty = parser.parse_type_expr()?;
                parser.consume(&TK::Tilde)?;
                parser.consume(&TK::GreaterThan)?;
                let to_role = parser.parse_ident()?;
                Ok(ProtocolEffect {
                    payload_ty: parser.resolve_protocol_message_ty(&effect_ty, messages),
                    to_role,
                    span: parser.close(effect_marker),
                })
            })?;
            self.consume(&TK::RBracket)?;
            effects.extend(parsed_effects);
            if matches!(self.curr_token.kind, TK::Comma | TK::Semicolon) {
                self.advance();
            }
        }
        self.consume(&TK::RBrace)?;
        Ok(effects)
    }

    fn parse_protocol_flow_decl(
        &mut self,
        request_contracts: &mut Vec<ProtocolRequestContract>,
        messages: &HashMap<String, TypeExpr>,
    ) -> Result<(), ParseError> {
        let marker = self.mark();
        self.consume_keyword(TK::KwFlow)?;
        let from_role = self.parse_ident()?;
        self.consume(&TK::Arrow)?;
        let to_role = self.parse_ident()?;
        self.consume(&TK::Colon)?;
        let request_ty = self.parse_type_expr()?;

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
        request_contracts.push(ProtocolRequestContract {
            id: self.id_gen.new_id(),
            from_role,
            to_role,
            request_ty: self.resolve_protocol_message_ty(&request_ty, messages),
            response_tys: response_tys
                .iter()
                .map(|ty| self.resolve_protocol_message_ty(ty, messages))
                .collect(),
            span: self.close(marker),
        });
        Ok(())
    }

    fn parse_protocol_msg_decl(
        &mut self,
        messages: &mut Vec<ProtocolMessage>,
        message_aliases: &mut HashMap<String, TypeExpr>,
    ) -> Result<(), ParseError> {
        self.consume_contextual_keyword("msg")?;
        let marker = self.mark();
        let msg_name = self.parse_ident()?;
        let msg_span = self.close(marker);
        let msg_ty = if self.curr_token.kind == TK::Equals {
            self.consume(&TK::Equals)?;
            self.parse_type_expr()?
        } else {
            TypeExpr {
                id: self.id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: msg_name.clone(),
                    def_id: (),
                    type_args: Vec::new(),
                },
                span: msg_span,
            }
        };
        message_aliases.insert(msg_name.clone(), self.clone_type_expr_with_new_ids(&msg_ty));
        messages.push(ProtocolMessage {
            id: self.id_gen.new_id(),
            def_id: (),
            name: msg_name,
            ty: msg_ty,
            span: msg_span,
        });
        if self.curr_token.kind == TK::Semicolon {
            self.consume(&TK::Semicolon)?;
        }
        Ok(())
    }

    fn parse_protocol_req_decl(
        &mut self,
        request_contracts: &mut Vec<ProtocolRequestContract>,
        messages: &HashMap<String, TypeExpr>,
    ) -> Result<(), ParseError> {
        let marker = self.mark();
        self.consume_contextual_keyword("req")?;
        let from_role = self.parse_ident()?;
        self.consume(&TK::Arrow)?;
        let to_role = self.parse_ident()?;
        self.consume(&TK::Colon)?;
        let request_ty = self.parse_type_expr()?;
        self.consume(&TK::FatArrow)?;
        let response_union = self.parse_type_expr()?;
        let response_tys = match response_union.kind {
            TypeExprKind::Union { variants } => variants
                .iter()
                .map(|variant| self.resolve_protocol_message_ty(variant, messages))
                .collect(),
            _ => vec![self.resolve_protocol_message_ty(&response_union, messages)],
        };

        if self.curr_token.kind == TK::Semicolon {
            self.consume(&TK::Semicolon)?;
        }

        let resolved_request_ty = self.resolve_protocol_message_ty(&request_ty, messages);
        let resolved_response_tys: Vec<TypeExpr> = response_tys
            .iter()
            .map(|ty| self.clone_type_expr_with_new_ids(ty))
            .collect();

        request_contracts.push(ProtocolRequestContract {
            id: self.id_gen.new_id(),
            from_role,
            to_role,
            request_ty: resolved_request_ty,
            response_tys: resolved_response_tys,
            span: self.close(marker),
        });
        Ok(())
    }

    fn resolve_protocol_message_ty(
        &mut self,
        ty: &TypeExpr,
        messages: &HashMap<String, TypeExpr>,
    ) -> TypeExpr {
        if let TypeExprKind::Named {
            ident,
            type_args,
            def_id: _,
        } = &ty.kind
            && type_args.is_empty()
            && let Some(alias) = messages.get(ident)
        {
            return self.clone_type_expr_with_new_ids(alias);
        }
        self.clone_type_expr_with_new_ids(ty)
    }

    fn is_protocol_start_trigger(ty: &TypeExpr) -> bool {
        matches!(
            &ty.kind,
            TypeExprKind::Named { ident, type_args, .. }
                if ident == "Start" && type_args.is_empty()
        )
    }

    fn ensure_protocol_role(
        roles: &mut Vec<ProtocolRole>,
        role_name: &str,
        span: Span,
        id: NodeId,
    ) -> usize {
        if let Some(idx) = roles.iter().position(|role| role.name == role_name) {
            return idx;
        }
        roles.push(ProtocolRole {
            id,
            def_id: (),
            name: role_name.to_string(),
            states: Vec::new(),
            span,
        });
        roles.len() - 1
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
            let attrs = self.parse_attribute_list()?;
            if self.is_contextual_keyword("fields") {
                if !attrs.is_empty() {
                    return Err(ParseError::AttributeNotAllowed(attrs[0].span));
                }
                items.push(TypestateItem::Fields(self.parse_typestate_fields_block()?));
                continue;
            }
            if self.curr_token.kind == TK::KwFn {
                if !attrs.is_empty() {
                    return Err(ParseError::AttributeNotAllowed(attrs[0].span));
                }
                let func = self.parse_typestate_func_def(format!("{name}$new"))?;
                items.push(TypestateItem::Constructor(func));
                continue;
            }
            if self.curr_token.kind == TK::KwOn {
                if !attrs.is_empty() {
                    return Err(ParseError::AttributeNotAllowed(attrs[0].span));
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
            return Err(ParseError::ExpectedToken(
                TK::RBrace,
                self.curr_token.clone(),
            ));
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
