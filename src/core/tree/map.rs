use crate::core::tree::*;

/// Tree mapper with context-aware traversal helpers.
///
/// Implement the `map_*` methods you care about and call the corresponding
/// `walk_*` function to recursively map children.
pub trait TreeMapper {
    type Context;
    type InD;
    type InT;
    type OutD;
    type OutT;

    fn map_def_id(
        &mut self,
        node_id: NodeId,
        def_id: &Self::InD,
        ctx: &mut Self::Context,
    ) -> Self::OutD;

    fn map_type_payload(
        &mut self,
        node_id: NodeId,
        payload: &Self::InT,
        ctx: &mut Self::Context,
    ) -> Self::OutT;

    fn map_module(
        &mut self,
        module: &Module<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> Module<Self::OutD, Self::OutT> {
        walk_module(self, module, ctx)
    }

    fn map_top_level_item(
        &mut self,
        item: &TopLevelItem<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> TopLevelItem<Self::OutD, Self::OutT> {
        walk_top_level_item(self, item, ctx)
    }

    fn map_protocol_def(
        &mut self,
        protocol_def: &ProtocolDef<Self::InD>,
        ctx: &mut Self::Context,
    ) -> ProtocolDef<Self::OutD> {
        walk_protocol_def(self, protocol_def, ctx)
    }

    fn map_protocol_role(
        &mut self,
        role: &ProtocolRole<Self::InD>,
        ctx: &mut Self::Context,
    ) -> ProtocolRole<Self::OutD> {
        walk_protocol_role(self, role, ctx)
    }

    fn map_protocol_message(
        &mut self,
        message: &ProtocolMessage<Self::InD>,
        ctx: &mut Self::Context,
    ) -> ProtocolMessage<Self::OutD> {
        walk_protocol_message(self, message, ctx)
    }

    fn map_protocol_request_contract(
        &mut self,
        contract: &ProtocolRequestContract<Self::InD>,
        ctx: &mut Self::Context,
    ) -> ProtocolRequestContract<Self::OutD> {
        walk_protocol_request_contract(self, contract, ctx)
    }

    fn map_protocol_state(
        &mut self,
        state: &ProtocolState<Self::InD>,
        ctx: &mut Self::Context,
    ) -> ProtocolState<Self::OutD> {
        walk_protocol_state(self, state, ctx)
    }

    fn map_protocol_transition(
        &mut self,
        transition: &ProtocolTransition<Self::InD>,
        ctx: &mut Self::Context,
    ) -> ProtocolTransition<Self::OutD> {
        walk_protocol_transition(self, transition, ctx)
    }

    fn map_protocol_trigger(
        &mut self,
        trigger: &ProtocolTrigger<Self::InD>,
        ctx: &mut Self::Context,
    ) -> ProtocolTrigger<Self::OutD> {
        walk_protocol_trigger(self, trigger, ctx)
    }

    fn map_protocol_effect(
        &mut self,
        effect: &ProtocolEffect<Self::InD>,
        ctx: &mut Self::Context,
    ) -> ProtocolEffect<Self::OutD> {
        walk_protocol_effect(self, effect, ctx)
    }

    fn map_protocol_flow(
        &mut self,
        flow: &ProtocolFlow<Self::InD>,
        ctx: &mut Self::Context,
    ) -> ProtocolFlow<Self::OutD> {
        walk_protocol_flow(self, flow, ctx)
    }

    fn map_typestate_def(
        &mut self,
        typestate_def: &TypestateDef<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> TypestateDef<Self::OutD, Self::OutT> {
        walk_typestate_def(self, typestate_def, ctx)
    }

    fn map_typestate_item(
        &mut self,
        item: &TypestateItem<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> TypestateItem<Self::OutD, Self::OutT> {
        walk_typestate_item(self, item, ctx)
    }

    fn map_typestate_role_impl(
        &mut self,
        role_impl: &TypestateRoleImpl<Self::InD>,
        ctx: &mut Self::Context,
    ) -> TypestateRoleImpl<Self::OutD> {
        walk_typestate_role_impl(self, role_impl, ctx)
    }

    fn map_typestate_fields(
        &mut self,
        fields: &TypestateFields<Self::InD>,
        ctx: &mut Self::Context,
    ) -> TypestateFields<Self::OutD> {
        walk_typestate_fields(self, fields, ctx)
    }

    fn map_typestate_state(
        &mut self,
        state: &TypestateState<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> TypestateState<Self::OutD, Self::OutT> {
        walk_typestate_state(self, state, ctx)
    }

    fn map_typestate_state_item(
        &mut self,
        item: &TypestateStateItem<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> TypestateStateItem<Self::OutD, Self::OutT> {
        walk_typestate_state_item(self, item, ctx)
    }

    fn map_typestate_on_handler(
        &mut self,
        handler: &TypestateOnHandler<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> TypestateOnHandler<Self::OutD, Self::OutT> {
        walk_typestate_on_handler(self, handler, ctx)
    }

    fn map_type_def(
        &mut self,
        type_def: &TypeDef<Self::InD>,
        ctx: &mut Self::Context,
    ) -> TypeDef<Self::OutD> {
        walk_type_def(self, type_def, ctx)
    }

    fn map_trait_def(
        &mut self,
        trait_def: &TraitDef<Self::InD>,
        ctx: &mut Self::Context,
    ) -> TraitDef<Self::OutD> {
        walk_trait_def(self, trait_def, ctx)
    }

    fn map_trait_method(
        &mut self,
        method: &TraitMethod<Self::InD>,
        ctx: &mut Self::Context,
    ) -> TraitMethod<Self::OutD> {
        walk_trait_method(self, method, ctx)
    }

    fn map_trait_property(
        &mut self,
        property: &TraitProperty<Self::InD>,
        ctx: &mut Self::Context,
    ) -> TraitProperty<Self::OutD> {
        walk_trait_property(self, property, ctx)
    }

    fn map_struct_def_field(
        &mut self,
        field: &StructDefField<Self::InD>,
        ctx: &mut Self::Context,
    ) -> StructDefField<Self::OutD> {
        walk_struct_def_field(self, field, ctx)
    }

    fn map_enum_def_variant(
        &mut self,
        variant: &EnumDefVariant<Self::InD>,
        ctx: &mut Self::Context,
    ) -> EnumDefVariant<Self::OutD> {
        walk_enum_def_variant(self, variant, ctx)
    }

    fn map_type_expr(
        &mut self,
        type_expr: &TypeExpr<Self::InD>,
        ctx: &mut Self::Context,
    ) -> TypeExpr<Self::OutD> {
        walk_type_expr(self, type_expr, ctx)
    }

    fn map_fn_type_param(
        &mut self,
        param: &FnTypeParam<Self::InD>,
        ctx: &mut Self::Context,
    ) -> FnTypeParam<Self::OutD> {
        walk_fn_type_param(self, param, ctx)
    }

    fn map_type_param(
        &mut self,
        param: &TypeParam<Self::InD>,
        ctx: &mut Self::Context,
    ) -> TypeParam<Self::OutD> {
        walk_type_param(self, param, ctx)
    }

    fn map_func_decl(
        &mut self,
        func_decl: &FuncDecl<Self::InD>,
        ctx: &mut Self::Context,
    ) -> FuncDecl<Self::OutD> {
        walk_func_decl(self, func_decl, ctx)
    }

    fn map_func_def(
        &mut self,
        func_def: &FuncDef<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> FuncDef<Self::OutD, Self::OutT> {
        walk_func_def(self, func_def, ctx)
    }

    fn map_func_sig(
        &mut self,
        func_sig: &FunctionSig<Self::InD>,
        ctx: &mut Self::Context,
    ) -> FunctionSig<Self::OutD> {
        walk_func_sig(self, func_sig, ctx)
    }

    fn map_method_block(
        &mut self,
        method_block: &MethodBlock<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> MethodBlock<Self::OutD, Self::OutT> {
        walk_method_block(self, method_block, ctx)
    }

    fn map_method_item(
        &mut self,
        method_item: &MethodItem<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> MethodItem<Self::OutD, Self::OutT> {
        walk_method_item(self, method_item, ctx)
    }

    fn map_method_decl(
        &mut self,
        method_decl: &MethodDecl<Self::InD>,
        ctx: &mut Self::Context,
    ) -> MethodDecl<Self::OutD> {
        walk_method_decl(self, method_decl, ctx)
    }

    fn map_method_def(
        &mut self,
        method_def: &MethodDef<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> MethodDef<Self::OutD, Self::OutT> {
        walk_method_def(self, method_def, ctx)
    }

    fn map_method_sig(
        &mut self,
        method_sig: &MethodSig<Self::InD>,
        ctx: &mut Self::Context,
    ) -> MethodSig<Self::OutD> {
        walk_method_sig(self, method_sig, ctx)
    }

    fn map_self_param(
        &mut self,
        self_param: &SelfParam<Self::InD>,
        ctx: &mut Self::Context,
    ) -> SelfParam<Self::OutD> {
        walk_self_param(self, self_param, ctx)
    }

    fn map_closure_def(
        &mut self,
        closure_def: &ClosureDef<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> ClosureDef<Self::OutD, Self::OutT> {
        walk_closure_def(self, closure_def, ctx)
    }

    fn map_closure_sig(
        &mut self,
        closure_sig: &ClosureSig<Self::InD>,
        ctx: &mut Self::Context,
    ) -> ClosureSig<Self::OutD> {
        walk_closure_sig(self, closure_sig, ctx)
    }

    fn map_param(
        &mut self,
        param: &Param<Self::InD>,
        ctx: &mut Self::Context,
    ) -> Param<Self::OutD> {
        walk_param(self, param, ctx)
    }

    fn map_call_arg(
        &mut self,
        arg: &CallArg<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> CallArg<Self::OutD, Self::OutT> {
        walk_call_arg(self, arg, ctx)
    }

    fn map_bind_pattern(
        &mut self,
        pattern: &BindPattern<Self::InD>,
        ctx: &mut Self::Context,
    ) -> BindPattern<Self::OutD> {
        walk_bind_pattern(self, pattern, ctx)
    }

    fn map_struct_field_bind_pattern(
        &mut self,
        field: &StructFieldBindPattern<Self::InD>,
        ctx: &mut Self::Context,
    ) -> StructFieldBindPattern<Self::OutD> {
        walk_struct_field_bind_pattern(self, field, ctx)
    }

    fn map_match_arm(
        &mut self,
        arm: &MatchArm<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> MatchArm<Self::OutD, Self::OutT> {
        walk_match_arm(self, arm, ctx)
    }

    fn map_match_pattern(
        &mut self,
        pattern: &MatchPattern<Self::InD>,
        ctx: &mut Self::Context,
    ) -> MatchPattern<Self::OutD> {
        walk_match_pattern(self, pattern, ctx)
    }

    fn map_match_pattern_binding(
        &mut self,
        binding: &MatchPatternBinding<Self::InD>,
        ctx: &mut Self::Context,
    ) -> MatchPatternBinding<Self::OutD> {
        walk_match_pattern_binding(self, binding, ctx)
    }

    fn map_block_item(
        &mut self,
        item: &BlockItem<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> BlockItem<Self::OutD, Self::OutT> {
        walk_block_item(self, item, ctx)
    }

    fn map_stmt_expr(
        &mut self,
        stmt: &StmtExpr<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> StmtExpr<Self::OutD, Self::OutT> {
        walk_stmt_expr(self, stmt, ctx)
    }

    fn map_expr(
        &mut self,
        expr: &Expr<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> Expr<Self::OutD, Self::OutT> {
        walk_expr(self, expr, ctx)
    }

    fn map_expr_kind(
        &mut self,
        expr_id: NodeId,
        expr: &ExprKind<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> ExprKind<Self::OutD, Self::OutT> {
        walk_expr_kind(self, expr_id, expr, ctx)
    }

    fn map_array_lit_init(
        &mut self,
        init: &ArrayLitInit<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> ArrayLitInit<Self::OutD, Self::OutT> {
        walk_array_lit_init(self, init, ctx)
    }

    fn map_struct_lit_field(
        &mut self,
        field: &StructLitField<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> StructLitField<Self::OutD, Self::OutT> {
        walk_struct_lit_field(self, field, ctx)
    }

    fn map_map_lit_entry(
        &mut self,
        entry: &MapLitEntry<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> MapLitEntry<Self::OutD, Self::OutT> {
        walk_map_lit_entry(self, entry, ctx)
    }

    fn map_struct_update_field(
        &mut self,
        field: &StructUpdateField<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> StructUpdateField<Self::OutD, Self::OutT> {
        walk_struct_update_field(self, field, ctx)
    }

    fn map_string_fmt_segment(
        &mut self,
        seg: &StringFmtSegment<Self::InD, Self::InT>,
        ctx: &mut Self::Context,
    ) -> StringFmtSegment<Self::OutD, Self::OutT> {
        walk_string_fmt_segment(self, seg, ctx)
    }
}

pub fn walk_module<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    module: &Module<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> Module<M::OutD, M::OutT> {
    Module {
        requires: module.requires.clone(),
        top_level_items: module
            .top_level_items
            .iter()
            .map(|item| mapper.map_top_level_item(item, ctx))
            .collect(),
    }
}

pub fn walk_top_level_item<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    item: &TopLevelItem<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> TopLevelItem<M::OutD, M::OutT> {
    match item {
        TopLevelItem::ProtocolDef(protocol_def) => {
            TopLevelItem::ProtocolDef(mapper.map_protocol_def(protocol_def, ctx))
        }
        TopLevelItem::TraitDef(trait_def) => {
            TopLevelItem::TraitDef(mapper.map_trait_def(trait_def, ctx))
        }
        TopLevelItem::TypeDef(type_def) => {
            TopLevelItem::TypeDef(mapper.map_type_def(type_def, ctx))
        }
        TopLevelItem::TypestateDef(typestate_def) => {
            TopLevelItem::TypestateDef(mapper.map_typestate_def(typestate_def, ctx))
        }
        TopLevelItem::FuncDecl(func_decl) => {
            TopLevelItem::FuncDecl(mapper.map_func_decl(func_decl, ctx))
        }
        TopLevelItem::FuncDef(func_def) => {
            TopLevelItem::FuncDef(mapper.map_func_def(func_def, ctx))
        }
        TopLevelItem::MethodBlock(method_block) => {
            TopLevelItem::MethodBlock(mapper.map_method_block(method_block, ctx))
        }
        TopLevelItem::ClosureDef(closure_def) => {
            TopLevelItem::ClosureDef(mapper.map_closure_def(closure_def, ctx))
        }
    }
}

pub fn walk_protocol_def<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    protocol_def: &ProtocolDef<M::InD>,
    ctx: &mut M::Context,
) -> ProtocolDef<M::OutD> {
    ProtocolDef {
        id: protocol_def.id,
        def_id: mapper.map_def_id(protocol_def.id, &protocol_def.def_id, ctx),
        name: protocol_def.name.clone(),
        messages: protocol_def
            .messages
            .iter()
            .map(|message| mapper.map_protocol_message(message, ctx))
            .collect(),
        request_contracts: protocol_def
            .request_contracts
            .iter()
            .map(|contract| mapper.map_protocol_request_contract(contract, ctx))
            .collect(),
        roles: protocol_def
            .roles
            .iter()
            .map(|role| mapper.map_protocol_role(role, ctx))
            .collect(),
        flows: protocol_def
            .flows
            .iter()
            .map(|flow| mapper.map_protocol_flow(flow, ctx))
            .collect(),
        span: protocol_def.span,
    }
}

pub fn walk_protocol_role<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    role: &ProtocolRole<M::InD>,
    ctx: &mut M::Context,
) -> ProtocolRole<M::OutD> {
    ProtocolRole {
        id: role.id,
        def_id: mapper.map_def_id(role.id, &role.def_id, ctx),
        name: role.name.clone(),
        states: role
            .states
            .iter()
            .map(|state| mapper.map_protocol_state(state, ctx))
            .collect(),
        span: role.span,
    }
}

pub fn walk_protocol_message<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    message: &ProtocolMessage<M::InD>,
    ctx: &mut M::Context,
) -> ProtocolMessage<M::OutD> {
    ProtocolMessage {
        id: message.id,
        def_id: mapper.map_def_id(message.id, &message.def_id, ctx),
        name: message.name.clone(),
        ty: mapper.map_type_expr(&message.ty, ctx),
        span: message.span,
    }
}

pub fn walk_protocol_request_contract<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    contract: &ProtocolRequestContract<M::InD>,
    ctx: &mut M::Context,
) -> ProtocolRequestContract<M::OutD> {
    ProtocolRequestContract {
        id: contract.id,
        from_role: contract.from_role.clone(),
        to_role: contract.to_role.clone(),
        request_ty: mapper.map_type_expr(&contract.request_ty, ctx),
        response_tys: contract
            .response_tys
            .iter()
            .map(|ty| mapper.map_type_expr(ty, ctx))
            .collect(),
        span: contract.span,
    }
}

pub fn walk_protocol_state<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    state: &ProtocolState<M::InD>,
    ctx: &mut M::Context,
) -> ProtocolState<M::OutD> {
    ProtocolState {
        id: state.id,
        name: state.name.clone(),
        transitions: state
            .transitions
            .iter()
            .map(|transition| mapper.map_protocol_transition(transition, ctx))
            .collect(),
        span: state.span,
    }
}

pub fn walk_protocol_transition<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    transition: &ProtocolTransition<M::InD>,
    ctx: &mut M::Context,
) -> ProtocolTransition<M::OutD> {
    ProtocolTransition {
        id: transition.id,
        trigger: mapper.map_protocol_trigger(&transition.trigger, ctx),
        next_state: transition.next_state.clone(),
        effects: transition
            .effects
            .iter()
            .map(|effect| mapper.map_protocol_effect(effect, ctx))
            .collect(),
        span: transition.span,
    }
}

pub fn walk_protocol_trigger<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    trigger: &ProtocolTrigger<M::InD>,
    ctx: &mut M::Context,
) -> ProtocolTrigger<M::OutD> {
    ProtocolTrigger {
        selector_ty: mapper.map_type_expr(&trigger.selector_ty, ctx),
        from_role: trigger.from_role.clone(),
    }
}

pub fn walk_protocol_effect<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    effect: &ProtocolEffect<M::InD>,
    ctx: &mut M::Context,
) -> ProtocolEffect<M::OutD> {
    ProtocolEffect {
        payload_ty: mapper.map_type_expr(&effect.payload_ty, ctx),
        to_role: effect.to_role.clone(),
        span: effect.span,
    }
}

pub fn walk_protocol_flow<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    flow: &ProtocolFlow<M::InD>,
    ctx: &mut M::Context,
) -> ProtocolFlow<M::OutD> {
    ProtocolFlow {
        id: flow.id,
        from_role: flow.from_role.clone(),
        to_role: flow.to_role.clone(),
        payload_ty: mapper.map_type_expr(&flow.payload_ty, ctx),
        response_tys: flow
            .response_tys
            .iter()
            .map(|ty| mapper.map_type_expr(ty, ctx))
            .collect(),
        span: flow.span,
    }
}

pub fn walk_typestate_def<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    typestate_def: &TypestateDef<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> TypestateDef<M::OutD, M::OutT> {
    TypestateDef {
        id: typestate_def.id,
        def_id: mapper.map_def_id(typestate_def.id, &typestate_def.def_id, ctx),
        name: typestate_def.name.clone(),
        role_impls: typestate_def
            .role_impls
            .iter()
            .map(|role_impl| mapper.map_typestate_role_impl(role_impl, ctx))
            .collect(),
        items: typestate_def
            .items
            .iter()
            .map(|item| mapper.map_typestate_item(item, ctx))
            .collect(),
        span: typestate_def.span,
    }
}

pub fn walk_typestate_role_impl<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    role_impl: &TypestateRoleImpl<M::InD>,
    ctx: &mut M::Context,
) -> TypestateRoleImpl<M::OutD> {
    TypestateRoleImpl {
        id: role_impl.id,
        def_id: mapper.map_def_id(role_impl.id, &role_impl.def_id, ctx),
        path: role_impl.path.clone(),
        span: role_impl.span,
    }
}

pub fn walk_typestate_item<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    item: &TypestateItem<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> TypestateItem<M::OutD, M::OutT> {
    match item {
        TypestateItem::Fields(fields) => {
            TypestateItem::Fields(mapper.map_typestate_fields(fields, ctx))
        }
        TypestateItem::Constructor(constructor) => {
            TypestateItem::Constructor(mapper.map_func_def(constructor, ctx))
        }
        TypestateItem::Handler(handler) => {
            TypestateItem::Handler(mapper.map_typestate_on_handler(handler, ctx))
        }
        TypestateItem::State(state) => TypestateItem::State(mapper.map_typestate_state(state, ctx)),
    }
}

pub fn walk_typestate_fields<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    fields: &TypestateFields<M::InD>,
    ctx: &mut M::Context,
) -> TypestateFields<M::OutD> {
    TypestateFields {
        id: fields.id,
        fields: fields
            .fields
            .iter()
            .map(|field| mapper.map_struct_def_field(field, ctx))
            .collect(),
        span: fields.span,
    }
}

pub fn walk_typestate_state<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    state: &TypestateState<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> TypestateState<M::OutD, M::OutT> {
    TypestateState {
        id: state.id,
        attrs: state.attrs.clone(),
        name: state.name.clone(),
        items: state
            .items
            .iter()
            .map(|item| mapper.map_typestate_state_item(item, ctx))
            .collect(),
        span: state.span,
    }
}

pub fn walk_typestate_state_item<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    item: &TypestateStateItem<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> TypestateStateItem<M::OutD, M::OutT> {
    match item {
        TypestateStateItem::Fields(fields) => {
            TypestateStateItem::Fields(mapper.map_typestate_fields(fields, ctx))
        }
        TypestateStateItem::Method(method) => {
            TypestateStateItem::Method(mapper.map_func_def(method, ctx))
        }
        TypestateStateItem::Handler(handler) => {
            TypestateStateItem::Handler(mapper.map_typestate_on_handler(handler, ctx))
        }
    }
}

pub fn walk_typestate_on_handler<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    handler: &TypestateOnHandler<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> TypestateOnHandler<M::OutD, M::OutT> {
    TypestateOnHandler {
        id: handler.id,
        selector_ty: mapper.map_type_expr(&handler.selector_ty, ctx),
        params: handler
            .params
            .iter()
            .map(|param| mapper.map_param(param, ctx))
            .collect(),
        provenance: handler
            .provenance
            .as_ref()
            .map(|provenance| TypestateHandlerProvenance {
                param: mapper.map_param(&provenance.param, ctx),
                request_site_label: provenance.request_site_label.clone(),
            }),
        ret_ty_expr: mapper.map_type_expr(&handler.ret_ty_expr, ctx),
        body: mapper.map_expr(&handler.body, ctx),
        span: handler.span,
    }
}

pub fn walk_trait_def<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    trait_def: &TraitDef<M::InD>,
    ctx: &mut M::Context,
) -> TraitDef<M::OutD> {
    TraitDef {
        id: trait_def.id,
        def_id: mapper.map_def_id(trait_def.id, &trait_def.def_id, ctx),
        attrs: trait_def.attrs.clone(),
        name: trait_def.name.clone(),
        methods: trait_def
            .methods
            .iter()
            .map(|method| mapper.map_trait_method(method, ctx))
            .collect(),
        properties: trait_def
            .properties
            .iter()
            .map(|property| mapper.map_trait_property(property, ctx))
            .collect(),
        span: trait_def.span,
    }
}

pub fn walk_trait_method<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    method: &TraitMethod<M::InD>,
    ctx: &mut M::Context,
) -> TraitMethod<M::OutD> {
    TraitMethod {
        id: method.id,
        sig: mapper.map_method_sig(&method.sig, ctx),
        span: method.span,
    }
}

pub fn walk_trait_property<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    property: &TraitProperty<M::InD>,
    ctx: &mut M::Context,
) -> TraitProperty<M::OutD> {
    TraitProperty {
        id: property.id,
        name: property.name.clone(),
        ty: mapper.map_type_expr(&property.ty, ctx),
        has_get: property.has_get,
        has_set: property.has_set,
        span: property.span,
    }
}

pub fn walk_type_def<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    type_def: &TypeDef<M::InD>,
    ctx: &mut M::Context,
) -> TypeDef<M::OutD> {
    TypeDef {
        id: type_def.id,
        def_id: mapper.map_def_id(type_def.id, &type_def.def_id, ctx),
        attrs: type_def.attrs.clone(),
        name: type_def.name.clone(),
        type_params: type_def
            .type_params
            .iter()
            .map(|param| mapper.map_type_param(param, ctx))
            .collect(),
        kind: match &type_def.kind {
            TypeDefKind::Alias { aliased_ty } => TypeDefKind::Alias {
                aliased_ty: mapper.map_type_expr(aliased_ty, ctx),
            },
            TypeDefKind::Struct { fields } => TypeDefKind::Struct {
                fields: fields
                    .iter()
                    .map(|field| mapper.map_struct_def_field(field, ctx))
                    .collect(),
            },
            TypeDefKind::Enum { variants } => TypeDefKind::Enum {
                variants: variants
                    .iter()
                    .map(|variant| mapper.map_enum_def_variant(variant, ctx))
                    .collect(),
            },
        },
        span: type_def.span,
    }
}

pub fn walk_struct_def_field<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    field: &StructDefField<M::InD>,
    ctx: &mut M::Context,
) -> StructDefField<M::OutD> {
    StructDefField {
        id: field.id,
        name: field.name.clone(),
        ty: mapper.map_type_expr(&field.ty, ctx),
        span: field.span,
    }
}

pub fn walk_enum_def_variant<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    variant: &EnumDefVariant<M::InD>,
    ctx: &mut M::Context,
) -> EnumDefVariant<M::OutD> {
    EnumDefVariant {
        id: variant.id,
        name: variant.name.clone(),
        payload: variant
            .payload
            .iter()
            .map(|ty| mapper.map_type_expr(ty, ctx))
            .collect(),
        span: variant.span,
    }
}

pub fn walk_type_expr<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    type_expr: &TypeExpr<M::InD>,
    ctx: &mut M::Context,
) -> TypeExpr<M::OutD> {
    TypeExpr {
        id: type_expr.id,
        kind: match &type_expr.kind {
            TypeExprKind::Infer => TypeExprKind::Infer,
            TypeExprKind::Union { variants } => TypeExprKind::Union {
                variants: variants
                    .iter()
                    .map(|variant| mapper.map_type_expr(variant, ctx))
                    .collect(),
            },
            TypeExprKind::Named {
                ident,
                def_id,
                type_args,
            } => TypeExprKind::Named {
                ident: ident.clone(),
                def_id: mapper.map_def_id(type_expr.id, def_id, ctx),
                type_args: type_args
                    .iter()
                    .map(|arg| mapper.map_type_expr(arg, ctx))
                    .collect(),
            },
            TypeExprKind::Refined {
                base_ty_expr,
                refinements,
            } => TypeExprKind::Refined {
                base_ty_expr: Box::new(mapper.map_type_expr(base_ty_expr, ctx)),
                refinements: refinements.clone(),
            },
            TypeExprKind::Array { elem_ty_expr, dims } => TypeExprKind::Array {
                elem_ty_expr: Box::new(mapper.map_type_expr(elem_ty_expr, ctx)),
                dims: dims.clone(),
            },
            TypeExprKind::DynArray { elem_ty_expr } => TypeExprKind::DynArray {
                elem_ty_expr: Box::new(mapper.map_type_expr(elem_ty_expr, ctx)),
            },
            TypeExprKind::Tuple { field_ty_exprs } => TypeExprKind::Tuple {
                field_ty_exprs: field_ty_exprs
                    .iter()
                    .map(|expr| mapper.map_type_expr(expr, ctx))
                    .collect(),
            },
            TypeExprKind::Slice { elem_ty_expr } => TypeExprKind::Slice {
                elem_ty_expr: Box::new(mapper.map_type_expr(elem_ty_expr, ctx)),
            },
            TypeExprKind::Heap { elem_ty_expr } => TypeExprKind::Heap {
                elem_ty_expr: Box::new(mapper.map_type_expr(elem_ty_expr, ctx)),
            },
            TypeExprKind::Ref {
                mutable,
                elem_ty_expr,
            } => TypeExprKind::Ref {
                mutable: *mutable,
                elem_ty_expr: Box::new(mapper.map_type_expr(elem_ty_expr, ctx)),
            },
            TypeExprKind::Fn {
                params,
                ret_ty_expr,
            } => TypeExprKind::Fn {
                params: params
                    .iter()
                    .map(|param| mapper.map_fn_type_param(param, ctx))
                    .collect(),
                ret_ty_expr: Box::new(mapper.map_type_expr(ret_ty_expr, ctx)),
            },
        },
        span: type_expr.span,
    }
}

pub fn walk_fn_type_param<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    param: &FnTypeParam<M::InD>,
    ctx: &mut M::Context,
) -> FnTypeParam<M::OutD> {
    FnTypeParam {
        mode: param.mode.clone(),
        ty_expr: mapper.map_type_expr(&param.ty_expr, ctx),
    }
}

pub fn walk_func_decl<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    func_decl: &FuncDecl<M::InD>,
    ctx: &mut M::Context,
) -> FuncDecl<M::OutD> {
    FuncDecl {
        id: func_decl.id,
        def_id: mapper.map_def_id(func_decl.id, &func_decl.def_id, ctx),
        attrs: func_decl.attrs.clone(),
        sig: mapper.map_func_sig(&func_decl.sig, ctx),
        span: func_decl.span,
    }
}

pub fn walk_func_def<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    func_def: &FuncDef<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> FuncDef<M::OutD, M::OutT> {
    FuncDef {
        id: func_def.id,
        def_id: mapper.map_def_id(func_def.id, &func_def.def_id, ctx),
        attrs: func_def.attrs.clone(),
        sig: mapper.map_func_sig(&func_def.sig, ctx),
        body: mapper.map_expr(&func_def.body, ctx),
        span: func_def.span,
    }
}

pub fn walk_func_sig<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    func_sig: &FunctionSig<M::InD>,
    ctx: &mut M::Context,
) -> FunctionSig<M::OutD> {
    FunctionSig {
        name: func_sig.name.clone(),
        type_params: func_sig
            .type_params
            .iter()
            .map(|param| mapper.map_type_param(param, ctx))
            .collect(),
        params: func_sig
            .params
            .iter()
            .map(|param| mapper.map_param(param, ctx))
            .collect(),
        ret_ty_expr: mapper.map_type_expr(&func_sig.ret_ty_expr, ctx),
        span: func_sig.span,
    }
}

pub fn walk_type_param<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    param: &TypeParam<M::InD>,
    ctx: &mut M::Context,
) -> TypeParam<M::OutD> {
    TypeParam {
        id: param.id,
        ident: param.ident.clone(),
        bound: param.bound.as_ref().map(|bound| TypeParamBound {
            id: bound.id,
            name: bound.name.clone(),
            def_id: mapper.map_def_id(bound.id, &bound.def_id, ctx),
            span: bound.span,
        }),
        def_id: mapper.map_def_id(param.id, &param.def_id, ctx),
        span: param.span,
    }
}

pub fn walk_method_block<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    method_block: &MethodBlock<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> MethodBlock<M::OutD, M::OutT> {
    MethodBlock {
        id: method_block.id,
        type_name: method_block.type_name.clone(),
        trait_name: method_block.trait_name.clone(),
        method_items: method_block
            .method_items
            .iter()
            .map(|method_item| mapper.map_method_item(method_item, ctx))
            .collect(),
        span: method_block.span,
    }
}

pub fn walk_method_item<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    method_item: &MethodItem<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> MethodItem<M::OutD, M::OutT> {
    match method_item {
        MethodItem::Decl(method_decl) => MethodItem::Decl(mapper.map_method_decl(method_decl, ctx)),
        MethodItem::Def(method_def) => MethodItem::Def(mapper.map_method_def(method_def, ctx)),
    }
}

pub fn walk_method_decl<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    method_decl: &MethodDecl<M::InD>,
    ctx: &mut M::Context,
) -> MethodDecl<M::OutD> {
    MethodDecl {
        id: method_decl.id,
        def_id: mapper.map_def_id(method_decl.id, &method_decl.def_id, ctx),
        attrs: method_decl.attrs.clone(),
        sig: mapper.map_method_sig(&method_decl.sig, ctx),
        span: method_decl.span,
    }
}

pub fn walk_method_def<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    method_def: &MethodDef<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> MethodDef<M::OutD, M::OutT> {
    MethodDef {
        id: method_def.id,
        def_id: mapper.map_def_id(method_def.id, &method_def.def_id, ctx),
        attrs: method_def.attrs.clone(),
        sig: mapper.map_method_sig(&method_def.sig, ctx),
        body: mapper.map_expr(&method_def.body, ctx),
        span: method_def.span,
    }
}

pub fn walk_method_sig<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    method_sig: &MethodSig<M::InD>,
    ctx: &mut M::Context,
) -> MethodSig<M::OutD> {
    MethodSig {
        name: method_sig.name.clone(),
        type_params: method_sig
            .type_params
            .iter()
            .map(|param| mapper.map_type_param(param, ctx))
            .collect(),
        self_param: mapper.map_self_param(&method_sig.self_param, ctx),
        params: method_sig
            .params
            .iter()
            .map(|param| mapper.map_param(param, ctx))
            .collect(),
        ret_ty_expr: mapper.map_type_expr(&method_sig.ret_ty_expr, ctx),
        span: method_sig.span,
    }
}

pub fn walk_self_param<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    self_param: &SelfParam<M::InD>,
    ctx: &mut M::Context,
) -> SelfParam<M::OutD> {
    SelfParam {
        id: self_param.id,
        def_id: mapper.map_def_id(self_param.id, &self_param.def_id, ctx),
        mode: self_param.mode.clone(),
        span: self_param.span,
    }
}

pub fn walk_closure_def<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    closure_def: &ClosureDef<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> ClosureDef<M::OutD, M::OutT> {
    ClosureDef {
        id: closure_def.id,
        def_id: mapper.map_def_id(closure_def.id, &closure_def.def_id, ctx),
        sig: mapper.map_closure_sig(&closure_def.sig, ctx),
        body: mapper.map_expr(&closure_def.body, ctx),
        span: closure_def.span,
    }
}

pub fn walk_closure_sig<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    closure_sig: &ClosureSig<M::InD>,
    ctx: &mut M::Context,
) -> ClosureSig<M::OutD> {
    ClosureSig {
        name: closure_sig.name.clone(),
        params: closure_sig
            .params
            .iter()
            .map(|param| mapper.map_param(param, ctx))
            .collect(),
        return_ty: mapper.map_type_expr(&closure_sig.return_ty, ctx),
        span: closure_sig.span,
    }
}

pub fn walk_param<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    param: &Param<M::InD>,
    ctx: &mut M::Context,
) -> Param<M::OutD> {
    Param {
        id: param.id,
        ident: param.ident.clone(),
        def_id: mapper.map_def_id(param.id, &param.def_id, ctx),
        typ: mapper.map_type_expr(&param.typ, ctx),
        mode: param.mode.clone(),
        span: param.span,
    }
}

pub fn walk_call_arg<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    arg: &CallArg<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> CallArg<M::OutD, M::OutT> {
    CallArg {
        mode: arg.mode,
        expr: mapper.map_expr(&arg.expr, ctx),
        init: arg.init,
        span: arg.span,
    }
}

pub fn walk_bind_pattern<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    pattern: &BindPattern<M::InD>,
    ctx: &mut M::Context,
) -> BindPattern<M::OutD> {
    BindPattern {
        id: pattern.id,
        kind: match &pattern.kind {
            BindPatternKind::Name { ident, def_id } => BindPatternKind::Name {
                ident: ident.clone(),
                def_id: mapper.map_def_id(pattern.id, def_id, ctx),
            },
            BindPatternKind::Array { patterns } => BindPatternKind::Array {
                patterns: patterns
                    .iter()
                    .map(|pattern| mapper.map_bind_pattern(pattern, ctx))
                    .collect(),
            },
            BindPatternKind::Tuple { patterns } => BindPatternKind::Tuple {
                patterns: patterns
                    .iter()
                    .map(|pattern| mapper.map_bind_pattern(pattern, ctx))
                    .collect(),
            },
            BindPatternKind::Struct { name, fields } => BindPatternKind::Struct {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|field| mapper.map_struct_field_bind_pattern(field, ctx))
                    .collect(),
            },
        },
        span: pattern.span,
    }
}

pub fn walk_struct_field_bind_pattern<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    field: &StructFieldBindPattern<M::InD>,
    ctx: &mut M::Context,
) -> StructFieldBindPattern<M::OutD> {
    StructFieldBindPattern {
        name: field.name.clone(),
        pattern: mapper.map_bind_pattern(&field.pattern, ctx),
        span: field.span,
    }
}

pub fn walk_match_arm<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    arm: &MatchArm<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> MatchArm<M::OutD, M::OutT> {
    MatchArm {
        id: arm.id,
        pattern: mapper.map_match_pattern(&arm.pattern, ctx),
        body: mapper.map_expr(&arm.body, ctx),
        span: arm.span,
    }
}

pub fn walk_match_pattern<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    pattern: &MatchPattern<M::InD>,
    ctx: &mut M::Context,
) -> MatchPattern<M::OutD> {
    match pattern {
        MatchPattern::Wildcard { span } => MatchPattern::Wildcard { span: *span },
        MatchPattern::BoolLit { value, span } => MatchPattern::BoolLit {
            value: *value,
            span: *span,
        },
        MatchPattern::IntLit { value, span } => MatchPattern::IntLit {
            value: *value,
            span: *span,
        },
        MatchPattern::Binding {
            id,
            ident,
            def_id,
            span,
        } => MatchPattern::Binding {
            id: *id,
            ident: ident.clone(),
            def_id: mapper.map_def_id(*id, def_id, ctx),
            span: *span,
        },
        MatchPattern::TypedBinding {
            id,
            ident,
            def_id,
            ty_expr,
            span,
        } => MatchPattern::TypedBinding {
            id: *id,
            ident: ident.clone(),
            def_id: mapper.map_def_id(*id, def_id, ctx),
            ty_expr: mapper.map_type_expr(ty_expr, ctx),
            span: *span,
        },
        MatchPattern::Tuple { patterns, span } => MatchPattern::Tuple {
            patterns: patterns
                .iter()
                .map(|pattern| mapper.map_match_pattern(pattern, ctx))
                .collect(),
            span: *span,
        },
        MatchPattern::EnumVariant {
            id,
            enum_name,
            type_args,
            variant_name,
            bindings,
            span,
        } => MatchPattern::EnumVariant {
            id: *id,
            enum_name: enum_name.clone(),
            type_args: type_args
                .iter()
                .map(|arg| mapper.map_type_expr(arg, ctx))
                .collect(),
            variant_name: variant_name.clone(),
            bindings: bindings
                .iter()
                .map(|binding| mapper.map_match_pattern_binding(binding, ctx))
                .collect(),
            span: *span,
        },
    }
}

pub fn walk_match_pattern_binding<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    binding: &MatchPatternBinding<M::InD>,
    ctx: &mut M::Context,
) -> MatchPatternBinding<M::OutD> {
    match binding {
        MatchPatternBinding::Named {
            id,
            ident,
            def_id,
            span,
        } => MatchPatternBinding::Named {
            id: *id,
            ident: ident.clone(),
            def_id: mapper.map_def_id(*id, def_id, ctx),
            span: *span,
        },
        MatchPatternBinding::Wildcard { span } => MatchPatternBinding::Wildcard { span: *span },
    }
}

pub fn walk_block_item<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    item: &BlockItem<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> BlockItem<M::OutD, M::OutT> {
    match item {
        BlockItem::Stmt(stmt) => BlockItem::Stmt(mapper.map_stmt_expr(stmt, ctx)),
        BlockItem::Expr(expr) => BlockItem::Expr(mapper.map_expr(expr, ctx)),
    }
}

pub fn walk_stmt_expr<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    stmt: &StmtExpr<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> StmtExpr<M::OutD, M::OutT> {
    StmtExpr {
        id: stmt.id,
        kind: match &stmt.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => StmtExprKind::LetBind {
                pattern: mapper.map_bind_pattern(pattern, ctx),
                decl_ty: decl_ty.as_ref().map(|ty| mapper.map_type_expr(ty, ctx)),
                value: Box::new(mapper.map_expr(value, ctx)),
            },
            StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => StmtExprKind::VarBind {
                pattern: mapper.map_bind_pattern(pattern, ctx),
                decl_ty: decl_ty.as_ref().map(|ty| mapper.map_type_expr(ty, ctx)),
                value: Box::new(mapper.map_expr(value, ctx)),
            },
            StmtExprKind::VarDecl {
                ident,
                def_id,
                decl_ty,
            } => StmtExprKind::VarDecl {
                ident: ident.clone(),
                def_id: mapper.map_def_id(stmt.id, def_id, ctx),
                decl_ty: mapper.map_type_expr(decl_ty, ctx),
            },
            StmtExprKind::Assign {
                assignee,
                value,
                init,
            } => StmtExprKind::Assign {
                assignee: Box::new(mapper.map_expr(assignee, ctx)),
                value: Box::new(mapper.map_expr(value, ctx)),
                init: *init,
            },
            StmtExprKind::While { cond, body } => StmtExprKind::While {
                cond: Box::new(mapper.map_expr(cond, ctx)),
                body: Box::new(mapper.map_expr(body, ctx)),
            },
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => StmtExprKind::For {
                pattern: mapper.map_bind_pattern(pattern, ctx),
                iter: Box::new(mapper.map_expr(iter, ctx)),
                body: Box::new(mapper.map_expr(body, ctx)),
            },
            StmtExprKind::Break => StmtExprKind::Break,
            StmtExprKind::Continue => StmtExprKind::Continue,
            StmtExprKind::Return { value } => StmtExprKind::Return {
                value: value
                    .as_ref()
                    .map(|expr| Box::new(mapper.map_expr(expr, ctx))),
            },
        },
        ty: mapper.map_type_payload(stmt.id, &stmt.ty, ctx),
        span: stmt.span,
    }
}

pub fn walk_expr<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    expr: &Expr<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> Expr<M::OutD, M::OutT> {
    Expr {
        id: expr.id,
        kind: mapper.map_expr_kind(expr.id, &expr.kind, ctx),
        ty: mapper.map_type_payload(expr.id, &expr.ty, ctx),
        span: expr.span,
    }
}

pub fn walk_expr_kind<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    expr_id: NodeId,
    expr: &ExprKind<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> ExprKind<M::OutD, M::OutT> {
    match expr {
        ExprKind::Block { items, tail } => ExprKind::Block {
            items: items
                .iter()
                .map(|item| mapper.map_block_item(item, ctx))
                .collect(),
            tail: tail
                .as_ref()
                .map(|tail| Box::new(mapper.map_expr(tail, ctx))),
        },
        ExprKind::UnitLit => ExprKind::UnitLit,
        ExprKind::IntLit(value) => ExprKind::IntLit(*value),
        ExprKind::BoolLit(value) => ExprKind::BoolLit(*value),
        ExprKind::CharLit(value) => ExprKind::CharLit(*value),
        ExprKind::StringLit { value } => ExprKind::StringLit {
            value: value.clone(),
        },
        ExprKind::StringFmt { segments } => ExprKind::StringFmt {
            segments: segments
                .iter()
                .map(|seg| mapper.map_string_fmt_segment(seg, ctx))
                .collect(),
        },
        ExprKind::ArrayLit { elem_ty, init } => ExprKind::ArrayLit {
            elem_ty: elem_ty.as_ref().map(|ty| mapper.map_type_expr(ty, ctx)),
            init: mapper.map_array_lit_init(init, ctx),
        },
        ExprKind::SetLit { elem_ty, elems } => ExprKind::SetLit {
            elem_ty: elem_ty.as_ref().map(|ty| mapper.map_type_expr(ty, ctx)),
            elems: elems
                .iter()
                .map(|elem| mapper.map_expr(elem, ctx))
                .collect(),
        },
        ExprKind::MapLit {
            key_ty,
            value_ty,
            entries,
        } => ExprKind::MapLit {
            key_ty: key_ty.as_ref().map(|ty| mapper.map_type_expr(ty, ctx)),
            value_ty: value_ty.as_ref().map(|ty| mapper.map_type_expr(ty, ctx)),
            entries: entries
                .iter()
                .map(|entry| mapper.map_map_lit_entry(entry, ctx))
                .collect(),
        },
        ExprKind::TupleLit(items) => ExprKind::TupleLit(
            items
                .iter()
                .map(|item| mapper.map_expr(item, ctx))
                .collect(),
        ),
        ExprKind::StructLit {
            name,
            type_args,
            fields,
        } => ExprKind::StructLit {
            name: name.clone(),
            type_args: type_args
                .iter()
                .map(|arg| mapper.map_type_expr(arg, ctx))
                .collect(),
            fields: fields
                .iter()
                .map(|field| mapper.map_struct_lit_field(field, ctx))
                .collect(),
        },
        ExprKind::EnumVariant {
            enum_name,
            type_args,
            variant,
            payload,
        } => ExprKind::EnumVariant {
            enum_name: enum_name.clone(),
            type_args: type_args
                .iter()
                .map(|arg| mapper.map_type_expr(arg, ctx))
                .collect(),
            variant: variant.clone(),
            payload: payload
                .iter()
                .map(|expr| mapper.map_expr(expr, ctx))
                .collect(),
        },
        ExprKind::StructUpdate { target, fields } => ExprKind::StructUpdate {
            target: Box::new(mapper.map_expr(target, ctx)),
            fields: fields
                .iter()
                .map(|field| mapper.map_struct_update_field(field, ctx))
                .collect(),
        },
        ExprKind::BinOp { left, op, right } => ExprKind::BinOp {
            left: Box::new(mapper.map_expr(left, ctx)),
            op: *op,
            right: Box::new(mapper.map_expr(right, ctx)),
        },
        ExprKind::UnaryOp { op, expr } => ExprKind::UnaryOp {
            op: *op,
            expr: Box::new(mapper.map_expr(expr, ctx)),
        },
        ExprKind::HeapAlloc { expr } => ExprKind::HeapAlloc {
            expr: Box::new(mapper.map_expr(expr, ctx)),
        },
        ExprKind::Move { expr } => ExprKind::Move {
            expr: Box::new(mapper.map_expr(expr, ctx)),
        },
        ExprKind::Coerce { kind, expr } => ExprKind::Coerce {
            kind: *kind,
            expr: Box::new(mapper.map_expr(expr, ctx)),
        },
        ExprKind::ImplicitMove { expr } => ExprKind::ImplicitMove {
            expr: Box::new(mapper.map_expr(expr, ctx)),
        },
        ExprKind::AddrOf { expr } => ExprKind::AddrOf {
            expr: Box::new(mapper.map_expr(expr, ctx)),
        },
        ExprKind::Deref { expr } => ExprKind::Deref {
            expr: Box::new(mapper.map_expr(expr, ctx)),
        },
        ExprKind::Var { ident, def_id } => ExprKind::Var {
            ident: ident.clone(),
            def_id: mapper.map_def_id(expr_id, def_id, ctx),
        },
        ExprKind::ArrayIndex { target, indices } => ExprKind::ArrayIndex {
            target: Box::new(mapper.map_expr(target, ctx)),
            indices: indices
                .iter()
                .map(|expr| mapper.map_expr(expr, ctx))
                .collect(),
        },
        ExprKind::TupleField { target, index } => ExprKind::TupleField {
            target: Box::new(mapper.map_expr(target, ctx)),
            index: *index,
        },
        ExprKind::StructField { target, field } => ExprKind::StructField {
            target: Box::new(mapper.map_expr(target, ctx)),
            field: field.clone(),
        },
        ExprKind::If {
            cond,
            then_body,
            else_body,
        } => ExprKind::If {
            cond: Box::new(mapper.map_expr(cond, ctx)),
            then_body: Box::new(mapper.map_expr(then_body, ctx)),
            else_body: Box::new(mapper.map_expr(else_body, ctx)),
        },
        ExprKind::Range { start, end } => ExprKind::Range {
            start: Box::new(mapper.map_expr(start, ctx)),
            end: Box::new(mapper.map_expr(end, ctx)),
        },
        ExprKind::Slice { target, start, end } => ExprKind::Slice {
            target: Box::new(mapper.map_expr(target, ctx)),
            start: start
                .as_ref()
                .map(|expr| Box::new(mapper.map_expr(expr, ctx))),
            end: end
                .as_ref()
                .map(|expr| Box::new(mapper.map_expr(expr, ctx))),
        },
        ExprKind::Match { scrutinee, arms } => ExprKind::Match {
            scrutinee: Box::new(mapper.map_expr(scrutinee, ctx)),
            arms: arms
                .iter()
                .map(|arm| mapper.map_match_arm(arm, ctx))
                .collect(),
        },
        ExprKind::Call { callee, args } => ExprKind::Call {
            callee: Box::new(mapper.map_expr(callee, ctx)),
            args: args
                .iter()
                .map(|arg| mapper.map_call_arg(arg, ctx))
                .collect(),
        },
        ExprKind::MethodCall {
            callee,
            method_name,
            args,
        } => ExprKind::MethodCall {
            callee: Box::new(mapper.map_expr(callee, ctx)),
            method_name: method_name.clone(),
            args: args
                .iter()
                .map(|arg| mapper.map_call_arg(arg, ctx))
                .collect(),
        },
        ExprKind::Emit { kind } => ExprKind::Emit {
            kind: match kind {
                EmitKind::Send { to, payload } => EmitKind::Send {
                    to: Box::new(mapper.map_expr(to, ctx)),
                    payload: Box::new(mapper.map_expr(payload, ctx)),
                },
                EmitKind::Request {
                    to,
                    payload,
                    request_site_label,
                } => EmitKind::Request {
                    to: Box::new(mapper.map_expr(to, ctx)),
                    payload: Box::new(mapper.map_expr(payload, ctx)),
                    request_site_label: request_site_label.clone(),
                },
            },
        },
        ExprKind::Reply { cap, value } => ExprKind::Reply {
            cap: Box::new(mapper.map_expr(cap, ctx)),
            value: Box::new(mapper.map_expr(value, ctx)),
        },
        ExprKind::Closure {
            ident,
            def_id,
            captures,
            params,
            return_ty,
            body,
        } => ExprKind::Closure {
            ident: ident.clone(),
            def_id: mapper.map_def_id(expr_id, def_id, ctx),
            captures: captures
                .iter()
                .map(|spec| map_capture_spec(mapper, spec, ctx))
                .collect(),
            params: params
                .iter()
                .map(|param| mapper.map_param(param, ctx))
                .collect(),
            return_ty: mapper.map_type_expr(return_ty, ctx),
            body: Box::new(mapper.map_expr(body, ctx)),
        },
    }
}

fn map_capture_spec<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    spec: &CaptureSpec<M::InD>,
    ctx: &mut M::Context,
) -> CaptureSpec<M::OutD> {
    match spec {
        CaptureSpec::Move {
            id,
            ident,
            def_id,
            span,
        } => CaptureSpec::Move {
            id: *id,
            ident: ident.clone(),
            def_id: mapper.map_def_id(*id, def_id, ctx),
            span: *span,
        },
    }
}

pub fn walk_array_lit_init<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    init: &ArrayLitInit<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> ArrayLitInit<M::OutD, M::OutT> {
    match init {
        ArrayLitInit::Elems(elems) => ArrayLitInit::Elems(
            elems
                .iter()
                .map(|expr| mapper.map_expr(expr, ctx))
                .collect(),
        ),
        ArrayLitInit::Repeat(expr, count) => {
            ArrayLitInit::Repeat(Box::new(mapper.map_expr(expr, ctx)), *count)
        }
    }
}

pub fn walk_struct_lit_field<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    field: &StructLitField<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> StructLitField<M::OutD, M::OutT> {
    StructLitField {
        id: field.id,
        name: field.name.clone(),
        value: mapper.map_expr(&field.value, ctx),
        span: field.span,
    }
}

pub fn walk_map_lit_entry<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    entry: &MapLitEntry<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> MapLitEntry<M::OutD, M::OutT> {
    MapLitEntry {
        id: entry.id,
        key: mapper.map_expr(&entry.key, ctx),
        value: mapper.map_expr(&entry.value, ctx),
        span: entry.span,
    }
}

pub fn walk_struct_update_field<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    field: &StructUpdateField<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> StructUpdateField<M::OutD, M::OutT> {
    StructUpdateField {
        id: field.id,
        name: field.name.clone(),
        value: mapper.map_expr(&field.value, ctx),
        span: field.span,
    }
}

pub fn walk_string_fmt_segment<M: TreeMapper + ?Sized>(
    mapper: &mut M,
    seg: &StringFmtSegment<M::InD, M::InT>,
    ctx: &mut M::Context,
) -> StringFmtSegment<M::OutD, M::OutT> {
    match seg {
        StringFmtSegment::Literal { value, span } => StringFmtSegment::Literal {
            value: value.clone(),
            span: *span,
        },
        StringFmtSegment::Expr { expr, span } => StringFmtSegment::Expr {
            expr: Box::new(mapper.map_expr(expr, ctx)),
            span: *span,
        },
    }
}
