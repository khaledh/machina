use super::*;
use crate::tree::parsed::{TypeParam, TypeParamBound};

impl<'a> Parser<'a> {
    // --- Functions ---

    pub(super) fn parse_func(&mut self, attrs: Vec<Attribute>) -> Result<TopLevelItem, ParseError> {
        let marker = self.mark();

        let sig = self.parse_func_sig()?;

        if self.curr_token.kind == TK::Semicolon {
            self.advance();
            Ok(TopLevelItem::FuncDecl(FuncDecl {
                id: self.id_gen.new_id(),
                def_id: (),
                attrs,
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
                attrs,
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
        let type_params = self.parse_type_params()?;

        self.consume(&TK::LParen)?;
        let params = self.parse_list(TK::Comma, TK::RParen, |parser| parser.parse_param())?;
        self.consume(&TK::RParen)?;

        let ret_ty_expr = self.parse_ret_type()?;

        Ok(FunctionSig {
            name,
            type_params,
            params,
            ret_ty_expr,
            span: self.close(marker),
        })
    }

    pub(super) fn parse_type_params(&mut self) -> Result<Vec<TypeParam>, ParseError> {
        if self.curr_token.kind != TK::LessThan {
            return Ok(Vec::new());
        }

        self.consume(&TK::LessThan)?;
        let params = self.parse_list(TK::Comma, TK::GreaterThan, |parser| {
            let marker = parser.mark();
            let ident = parser.parse_ident()?;
            let bound = if parser.curr_token.kind == TK::Colon {
                parser.advance();
                let bound_marker = parser.mark();
                let mut name = parser.parse_ident()?;
                while parser.curr_token.kind == TK::DoubleColon {
                    parser.advance();
                    let segment = parser.parse_ident()?;
                    name.push_str("::");
                    name.push_str(&segment);
                }
                Some(TypeParamBound {
                    id: parser.id_gen.new_id(),
                    name,
                    def_id: (),
                    span: parser.close(bound_marker),
                })
            } else {
                None
            };
            Ok(TypeParam {
                id: parser.id_gen.new_id(),
                ident,
                bound,
                def_id: (),
                span: parser.close(marker),
            })
        })?;
        self.consume(&TK::GreaterThan)?;

        Ok(params)
    }

    // --- Methods ---

    pub(super) fn parse_method_block(&mut self) -> Result<TopLevelItem, ParseError> {
        let marker = self.mark();
        let type_name = self.parse_ident()?;
        self.consume(&TK::DoubleColon)?;
        let trait_name = if self.curr_token.kind == TK::LBrace {
            None
        } else {
            let mut name = self.parse_ident()?;
            while self.curr_token.kind == TK::DoubleColon {
                self.advance();
                let segment = self.parse_ident()?;
                name.push_str("::");
                name.push_str(&segment);
            }
            Some(name)
        };
        self.consume(&TK::LBrace)?;

        let mut method_items = Vec::new();
        while self.curr_token.kind != TK::RBrace {
            method_items.extend(self.parse_method_item(&type_name)?);
        }
        self.consume(&TK::RBrace)?;

        Ok(TopLevelItem::MethodBlock(MethodBlock {
            id: self.id_gen.new_id(),
            type_name,
            trait_name,
            method_items,
            span: self.close(marker),
        }))
    }

    fn parse_method_item(&mut self, type_name: &str) -> Result<Vec<MethodItem>, ParseError> {
        let marker = self.mark();
        let attrs = self.parse_attribute_list()?;
        if self.curr_token.kind == TK::KwProp {
            return self.parse_property_def(type_name, marker, attrs);
        }

        let sig = self.parse_method_sig()?;

        if self.curr_token.kind == TK::Semicolon {
            self.advance();
            Ok(vec![MethodItem::Decl(MethodDecl {
                id: self.id_gen.new_id(),
                def_id: (),
                attrs,
                sig,
                span: self.close(marker),
            })])
        } else {
            let prev_base = self.closure_base.clone();
            let prev_index = self.closure_index;
            self.closure_base = Some(format!("{}${}", type_name, sig.name));
            self.closure_index = 0;

            let body = self.parse_block()?;

            self.closure_base = prev_base;
            self.closure_index = prev_index;

            Ok(vec![MethodItem::Def(MethodDef {
                id: self.id_gen.new_id(),
                def_id: (),
                attrs,
                sig,
                body,
                span: self.close(marker),
            })])
        }
    }

    pub(super) fn parse_method_sig(&mut self) -> Result<MethodSig, ParseError> {
        let marker = self.mark();
        self.consume_keyword(TK::KwFn)?;
        let name = self.parse_ident()?;
        let type_params = self.parse_type_params()?;
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
            type_params,
            self_param,
            params,
            ret_ty_expr,
            span: self.close(marker),
        })
    }

    fn parse_property_def(
        &mut self,
        type_name: &str,
        marker: Marker,
        attrs: Vec<Attribute>,
    ) -> Result<Vec<MethodItem>, ParseError> {
        // Parse the property header (name + type) and capture a tight span for diagnostics.
        let head_marker = self.mark();
        self.consume_keyword(TK::KwProp)?;
        let prop_name = self.parse_ident()?;
        self.consume(&TK::Colon)?;
        let prop_ty = self.parse_type_expr()?;
        let prop_head_span = self.close(head_marker);
        self.consume(&TK::LBrace)?;

        // Parse accessor blocks. Each accessor becomes a synthetic method definition with
        // a special attribute so later stages can treat `obj.prop` as a method call.
        let mut getter = None;
        let mut setter = None;
        while self.curr_token.kind != TK::RBrace {
            let accessor_marker = self.mark();
            match self.curr_token.kind {
                TK::KwGet => {
                    // `get { ... }` => method `prop_name(self) -> prop_ty`.
                    if getter.is_some() {
                        return Err(ParseError::ExpectedToken(
                            TK::RBrace,
                            self.curr_token.clone(),
                        ));
                    }
                    self.advance();

                    // Getter has only `self` and returns the property type.
                    let sig = MethodSig {
                        name: prop_name.clone(),
                        type_params: Vec::new(),
                        self_param: SelfParam {
                            id: self.id_gen.new_id(),
                            def_id: (),
                            mode: ParamMode::In,
                            span: self.close(accessor_marker),
                        },
                        params: Vec::new(),
                        ret_ty_expr: self.clone_type_expr_with_new_ids(&prop_ty),
                        span: self.close(accessor_marker),
                    };

                    // Mark the method as a property getter.
                    let mut accessor_attrs = attrs.clone();
                    accessor_attrs.push(Attribute {
                        name: "__property_get".to_string(),
                        args: Vec::new(),
                        span: sig.span,
                    });
                    if self.curr_token.kind == TK::Semicolon {
                        self.advance();
                        getter = Some(MethodItem::Decl(MethodDecl {
                            id: self.id_gen.new_id(),
                            def_id: (),
                            attrs: accessor_attrs,
                            sig,
                            span: self.close(accessor_marker),
                        }));
                    } else {
                        // Use a stable closure base so any closures inside the accessor are named.
                        let prev_base = self.closure_base.clone();
                        let prev_index = self.closure_index;
                        self.closure_base = Some(format!("{}${}#get", type_name, prop_name));
                        self.closure_index = 0;

                        let body = self.parse_block()?;

                        self.closure_base = prev_base;
                        self.closure_index = prev_index;

                        getter = Some(MethodItem::Def(MethodDef {
                            id: self.id_gen.new_id(),
                            def_id: (),
                            attrs: accessor_attrs,
                            sig,
                            body,
                            span: self.close(accessor_marker),
                        }));
                    }
                }
                TK::KwSet => {
                    // `set(v) { ... }` => method `prop_name(inout self, v: prop_ty) -> ()`.
                    if setter.is_some() {
                        return Err(ParseError::ExpectedToken(
                            TK::RBrace,
                            self.curr_token.clone(),
                        ));
                    }
                    self.advance();
                    self.consume(&TK::LParen)?;
                    let param_name = self.parse_ident()?;
                    self.consume(&TK::RParen)?;

                    // Setter takes one param of the property type and returns unit.
                    let param = Param {
                        id: self.id_gen.new_id(),
                        def_id: (),
                        ident: param_name,
                        mode: ParamMode::In,
                        typ: self.clone_type_expr_with_new_ids(&prop_ty),
                        span: self.close(accessor_marker),
                    };

                    let sig = MethodSig {
                        name: prop_name.clone(),
                        type_params: Vec::new(),
                        self_param: SelfParam {
                            id: self.id_gen.new_id(),
                            def_id: (),
                            mode: ParamMode::InOut,
                            span: self.close(accessor_marker),
                        },
                        params: vec![param],
                        ret_ty_expr: self.unit_type_expr(),
                        span: self.close(accessor_marker),
                    };

                    // Mark the method as a property setter.
                    let mut accessor_attrs = attrs.clone();
                    accessor_attrs.push(Attribute {
                        name: "__property_set".to_string(),
                        args: Vec::new(),
                        span: sig.span,
                    });
                    if self.curr_token.kind == TK::Semicolon {
                        self.advance();
                        setter = Some(MethodItem::Decl(MethodDecl {
                            id: self.id_gen.new_id(),
                            def_id: (),
                            attrs: accessor_attrs,
                            sig,
                            span: self.close(accessor_marker),
                        }));
                    } else {
                        // Use a stable closure base so any closures inside the accessor are named.
                        let prev_base = self.closure_base.clone();
                        let prev_index = self.closure_index;
                        self.closure_base = Some(format!("{}${}#set", type_name, prop_name));
                        self.closure_index = 0;

                        let body = self.parse_block()?;

                        self.closure_base = prev_base;
                        self.closure_index = prev_index;

                        setter = Some(MethodItem::Def(MethodDef {
                            id: self.id_gen.new_id(),
                            def_id: (),
                            attrs: accessor_attrs,
                            sig,
                            body,
                            span: self.close(accessor_marker),
                        }));
                    }
                }
                _ => {
                    return Err(ParseError::ExpectedToken(
                        TK::KwGet,
                        self.curr_token.clone(),
                    ));
                }
            }
        }
        self.consume(&TK::RBrace)?;

        // Attribute spans should point at the `prop name: Type` header, not the accessor body.
        if let Some(getter) = getter.as_mut() {
            match getter {
                MethodItem::Def(def) => {
                    for attr in &mut def.attrs {
                        if attr.name == "__property_get" {
                            attr.span = prop_head_span;
                        }
                    }
                }
                MethodItem::Decl(decl) => {
                    for attr in &mut decl.attrs {
                        if attr.name == "__property_get" {
                            attr.span = prop_head_span;
                        }
                    }
                }
            }
        }
        if let Some(setter) = setter.as_mut() {
            match setter {
                MethodItem::Def(def) => {
                    for attr in &mut def.attrs {
                        if attr.name == "__property_set" {
                            attr.span = prop_head_span;
                        }
                    }
                }
                MethodItem::Decl(decl) => {
                    for attr in &mut decl.attrs {
                        if attr.name == "__property_set" {
                            attr.span = prop_head_span;
                        }
                    }
                }
            }
        }

        // Emit the synthesized methods. At least one accessor must be present.
        let mut items = Vec::new();
        if let Some(getter) = getter {
            items.push(getter);
        }
        if let Some(setter) = setter {
            items.push(setter);
        }
        if items.is_empty() {
            return Err(ParseError::ExpectedToken(
                TK::KwGet,
                self.curr_token.clone(),
            ));
        }

        let _ = self.close(marker);
        Ok(items)
    }

    fn unit_type_expr(&mut self) -> TypeExpr {
        TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Named {
                ident: "()".to_string(),
                def_id: (),
                type_args: Vec::new(),
            },
            span: self.close(self.mark()),
        }
    }

    /// Clone a type expression while assigning fresh node IDs.
    ///
    /// Property accessors synthesize new method signatures that reuse the
    /// declared property type. Reusing the original `TypeExpr` IDs would
    /// violate the parser's uniqueness invariant, so we clone with new IDs.
    fn clone_type_expr_with_new_ids(&mut self, ty: &TypeExpr) -> TypeExpr {
        let kind = match &ty.kind {
            TypeExprKind::Infer => TypeExprKind::Infer,
            TypeExprKind::Union { variants } => TypeExprKind::Union {
                variants: variants
                    .iter()
                    .map(|variant| self.clone_type_expr_with_new_ids(variant))
                    .collect(),
            },
            TypeExprKind::Named {
                ident,
                def_id,
                type_args,
            } => TypeExprKind::Named {
                ident: ident.clone(),
                def_id: *def_id,
                type_args: type_args
                    .iter()
                    .map(|arg| self.clone_type_expr_with_new_ids(arg))
                    .collect(),
            },
            TypeExprKind::Refined {
                base_ty_expr,
                refinements,
            } => TypeExprKind::Refined {
                base_ty_expr: Box::new(self.clone_type_expr_with_new_ids(base_ty_expr)),
                refinements: refinements.clone(),
            },
            TypeExprKind::Array { elem_ty_expr, dims } => TypeExprKind::Array {
                elem_ty_expr: Box::new(self.clone_type_expr_with_new_ids(elem_ty_expr)),
                dims: dims.clone(),
            },
            TypeExprKind::Tuple { field_ty_exprs } => TypeExprKind::Tuple {
                field_ty_exprs: field_ty_exprs
                    .iter()
                    .map(|expr| self.clone_type_expr_with_new_ids(expr))
                    .collect(),
            },
            TypeExprKind::Slice { elem_ty_expr } => TypeExprKind::Slice {
                elem_ty_expr: Box::new(self.clone_type_expr_with_new_ids(elem_ty_expr)),
            },
            TypeExprKind::Heap { elem_ty_expr } => TypeExprKind::Heap {
                elem_ty_expr: Box::new(self.clone_type_expr_with_new_ids(elem_ty_expr)),
            },
            TypeExprKind::Ref {
                mutable,
                elem_ty_expr,
            } => TypeExprKind::Ref {
                mutable: *mutable,
                elem_ty_expr: Box::new(self.clone_type_expr_with_new_ids(elem_ty_expr)),
            },
            TypeExprKind::Fn {
                params,
                ret_ty_expr,
            } => TypeExprKind::Fn {
                params: params
                    .iter()
                    .map(|param| FnTypeParam {
                        mode: param.mode.clone(),
                        ty_expr: self.clone_type_expr_with_new_ids(&param.ty_expr),
                    })
                    .collect(),
                ret_ty_expr: Box::new(self.clone_type_expr_with_new_ids(ret_ty_expr)),
            },
        };

        TypeExpr {
            id: self.id_gen.new_id(),
            kind,
            span: ty.span,
        }
    }

    // --- Closures ---

    pub(super) fn parse_closure(&mut self) -> Result<Expr, ParseError> {
        let marker = self.mark();

        let captures = if self.curr_token.kind == TK::LBracket {
            self.parse_capture_list()?
        } else {
            Vec::new()
        };

        let params = if self.curr_token.kind == TK::LogicalOr {
            self.advance();
            Vec::new()
        } else {
            self.consume(&TK::Pipe)?;
            if self.curr_token.kind == TK::Pipe {
                self.advance();
                Vec::new()
            } else {
                let params =
                    self.parse_list(TK::Comma, TK::Pipe, |parser| parser.parse_closure_param())?;
                self.consume(&TK::Pipe)?;
                params
            }
        };

        let return_ty = self.parse_closure_ret_type()?;

        let mut body = self.parse_expr(0)?;
        if !matches!(body.kind, ExprKind::Block { .. }) {
            // Normalize closure bodies to blocks so downstream passes have a
            // consistent shape.
            let span = body.span;
            body = Expr {
                id: self.id_gen.new_id(),
                kind: ExprKind::Block {
                    items: Vec::new(),
                    tail: Some(Box::new(body)),
                },
                ty: (),
                span,
            };
        }
        let ident = self.next_closure_ident();

        let closure_expr = Expr {
            id: self.id_gen.new_id(),
            kind: ExprKind::Closure {
                ident: ident.clone(),
                def_id: (),
                captures,
                params: params.clone(),
                return_ty: return_ty.clone(),
                body: Box::new(body.clone()), // TODO: see if we can restructure this to avoid cloning
            },
            ty: (),
            span: self.close(marker),
        };

        Ok(closure_expr)
    }

    fn parse_capture_list(&mut self) -> Result<Vec<CaptureSpec>, ParseError> {
        self.consume(&TK::LBracket)?;
        self.consume(&TK::KwMove)?;
        if self.curr_token.kind == TK::RBracket {
            return Err(ParseError::ExpectedIdent(self.curr_token.clone()));
        }
        let captures = self.parse_list(TK::Comma, TK::RBracket, |parser| {
            let marker = parser.mark();
            let ident = parser.parse_ident()?;
            let id = parser.id_gen.new_id();
            let span = parser.close(marker);
            Ok(CaptureSpec::Move {
                id,
                ident,
                def_id: (),
                span,
            })
        })?;
        self.consume(&TK::RBracket)?;
        Ok(captures)
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

    fn parse_closure_param(&mut self) -> Result<Param, ParseError> {
        let marker = self.mark();
        let mode = self.parse_param_mode();
        let name = self.parse_ident()?;
        let typ = if self.curr_token.kind == TK::Colon {
            self.advance();
            self.parse_type_expr()?
        } else {
            self.infer_type_expr(self.close(self.mark()))
        };

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
                    type_args: Vec::new(),
                },
                span: self.close(self.mark()),
            },
        })
    }

    fn parse_closure_ret_type(&mut self) -> Result<TypeExpr, ParseError> {
        Ok(match self.curr_token.kind {
            TK::Arrow => {
                self.advance();
                self.parse_type_expr()?
            }
            _ => self.infer_type_expr(self.close(self.mark())),
        })
    }

    fn infer_type_expr(&mut self, span: Span) -> TypeExpr {
        TypeExpr {
            id: self.id_gen.new_id(),
            kind: TypeExprKind::Infer,
            span,
        }
    }
}
