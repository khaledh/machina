//! Frontend validation and direct-mode lowering for `@linear type` declarations.
//!
//! Validation runs before resolve so we can reject invalid linear-type
//! declarations while the source shape is still intact. After validation, the
//! direct-mode form is lowered into ordinary enums + method blocks so the rest
//! of the compiler can reuse existing type/method machinery.

use std::collections::{HashMap, HashSet};

use crate::core::ast::{
    ArrayLitInit, BlockItem, Expr, ExprKind, LinearRoleDecl, LinearStateVariant,
    LinearTransitionDecl, MethodBlock, MethodDef, MethodItem, Module, NodeIdGen, Param, ParamMode,
    StmtExpr, StmtExprKind, TopLevelItem, TypeDef, TypeDefKind, TypeExpr, TypeExprKind,
};
use crate::core::resolve::{REK, ResolveError};

pub fn validate_module(module: &Module) -> Vec<ResolveError> {
    let method_blocks = method_blocks_by_type(module);
    let mut errors = Vec::new();
    for type_def in module.type_defs() {
        let TypeDefKind::Linear { linear } = &type_def.kind else {
            continue;
        };
        validate_linear_type(
            type_def,
            linear,
            method_blocks.get(&type_def.name),
            &mut errors,
        );
    }
    errors
}

pub fn desugar_module(module: &mut Module, node_id_gen: &mut NodeIdGen) -> Vec<ResolveError> {
    let infos = collect_direct_linear_infos(module);
    if infos.is_empty() {
        return Vec::new();
    }

    rewrite_linear_type_defs(module, &infos);
    rewrite_linear_method_blocks(module, &infos, node_id_gen);
    rewrite_linear_exprs(module, &infos, node_id_gen)
}

#[derive(Clone, Debug)]
struct DirectLinearInfo {
    type_name: String,
    state_names: HashSet<String>,
    action_by_source_and_name: HashMap<(String, String), DirectActionInfo>,
}

#[derive(Clone, Debug)]
struct DirectActionInfo {
    internal_name: String,
    target_state: String,
}

#[derive(Clone, Debug)]
struct LinearValueState {
    type_name: String,
    state_name: String,
}

#[derive(Clone, Debug)]
struct LinearBindingState {
    actual_ident: String,
    value_state: LinearValueState,
    consumed: bool,
}

fn validate_linear_type(
    type_def: &TypeDef,
    linear: &crate::core::ast::LinearTypeDef,
    method_blocks: Option<&Vec<&MethodBlock>>,
    errors: &mut Vec<ResolveError>,
) {
    let type_name = &type_def.name;
    let final_states = collect_states(type_name, &linear.states, type_def.span, errors);
    let state_names: HashSet<String> = linear
        .states
        .iter()
        .map(|state| state.name.clone())
        .collect();

    validate_transition_decls(
        type_name,
        &linear.actions,
        "action",
        &state_names,
        &final_states,
        errors,
    );
    validate_transition_decls(
        type_name,
        &linear.triggers,
        "trigger",
        &state_names,
        &final_states,
        errors,
    );
    validate_roles(type_name, &linear.roles, &linear.actions, errors);
    validate_action_methods(type_name, &linear.actions, method_blocks, errors);
}

fn collect_states(
    type_name: &str,
    states: &[LinearStateVariant],
    type_span: crate::core::diag::Span,
    errors: &mut Vec<ResolveError>,
) -> HashSet<String> {
    let mut names = HashSet::new();
    let mut final_states = HashSet::new();
    for state in states {
        if !names.insert(state.name.clone()) {
            errors.push(
                REK::LinearDuplicateState(type_name.to_string(), state.name.clone()).at(state.span),
            );
        }
        if parse_final_attr(type_name, state, errors) {
            final_states.insert(state.name.clone());
        }
    }

    if states.is_empty() {
        errors.push(REK::LinearNoStates(type_name.to_string()).at(type_span));
    } else if states
        .iter()
        .all(|state| final_states.contains(&state.name))
    {
        errors.push(REK::LinearNoNonFinalStates(type_name.to_string()).at(type_span));
    }

    final_states
}

fn parse_final_attr(
    type_name: &str,
    state: &LinearStateVariant,
    errors: &mut Vec<ResolveError>,
) -> bool {
    let mut seen = HashSet::new();
    let mut is_final = false;
    for attr in &state.attrs {
        if !seen.insert(attr.name.clone()) {
            errors.push(REK::AttrDuplicate(attr.name.clone()).at(attr.span));
            continue;
        }
        match attr.name.as_str() {
            "final" => {
                if !attr.args.is_empty() {
                    errors.push(
                        REK::AttrWrongArgCount(attr.name.clone(), 0, attr.args.len()).at(attr.span),
                    );
                } else {
                    is_final = true;
                }
            }
            _ => errors.push(
                REK::LinearUnknownStateAttribute(
                    type_name.to_string(),
                    state.name.clone(),
                    attr.name.clone(),
                )
                .at(attr.span),
            ),
        }
    }
    is_final
}

fn validate_transition_decls(
    type_name: &str,
    decls: &[LinearTransitionDecl],
    kind: &'static str,
    state_names: &HashSet<String>,
    final_states: &HashSet<String>,
    errors: &mut Vec<ResolveError>,
) {
    let mut seen = HashSet::new();
    for decl in decls {
        let key = (decl.name.clone(), decl.source_state.clone());
        if !seen.insert(key) {
            errors.push(match kind {
                "action" => REK::LinearDuplicateAction(
                    type_name.to_string(),
                    decl.name.clone(),
                    decl.source_state.clone(),
                )
                .at(decl.span),
                "trigger" => REK::LinearDuplicateTrigger(
                    type_name.to_string(),
                    decl.name.clone(),
                    decl.source_state.clone(),
                )
                .at(decl.span),
                _ => unreachable!(),
            });
        }

        if !state_names.contains(&decl.source_state) {
            errors.push(match kind {
                "action" => REK::LinearUnknownStateInAction(
                    type_name.to_string(),
                    decl.name.clone(),
                    decl.source_state.clone(),
                )
                .at(decl.span),
                "trigger" => REK::LinearUnknownStateInTrigger(
                    type_name.to_string(),
                    decl.name.clone(),
                    decl.source_state.clone(),
                )
                .at(decl.span),
                _ => unreachable!(),
            });
        }

        if !state_names.contains(&decl.target_state) {
            errors.push(match kind {
                "action" => REK::LinearUnknownStateInAction(
                    type_name.to_string(),
                    decl.name.clone(),
                    decl.target_state.clone(),
                )
                .at(decl.span),
                "trigger" => REK::LinearUnknownStateInTrigger(
                    type_name.to_string(),
                    decl.name.clone(),
                    decl.target_state.clone(),
                )
                .at(decl.span),
                _ => unreachable!(),
            });
        }

        if final_states.contains(&decl.source_state) {
            errors.push(
                REK::LinearFinalStateAsSource(
                    type_name.to_string(),
                    decl.name.clone(),
                    decl.source_state.clone(),
                )
                .at(decl.span),
            );
        }
    }
}

fn validate_roles(
    type_name: &str,
    roles: &[LinearRoleDecl],
    actions: &[LinearTransitionDecl],
    errors: &mut Vec<ResolveError>,
) {
    let action_names: HashSet<&str> = actions.iter().map(|action| action.name.as_str()).collect();
    for role in roles {
        for action_name in &role.allowed_actions {
            if !action_names.contains(action_name.as_str()) {
                errors.push(
                    REK::LinearUnknownActionInRole(
                        type_name.to_string(),
                        role.name.clone(),
                        action_name.clone(),
                    )
                    .at(role.span),
                );
            }
        }
    }
}

fn validate_action_methods(
    type_name: &str,
    actions: &[LinearTransitionDecl],
    method_blocks: Option<&Vec<&MethodBlock>>,
    errors: &mut Vec<ResolveError>,
) {
    let mut actions_by_name: HashMap<&str, Vec<&LinearTransitionDecl>> = HashMap::new();
    for action in actions {
        actions_by_name
            .entry(action.name.as_str())
            .or_default()
            .push(action);
    }

    let mut methods_by_name: HashMap<&str, Vec<&MethodDef>> = HashMap::new();
    if let Some(method_blocks) = method_blocks {
        for block in method_blocks {
            for item in &block.method_items {
                if let MethodItem::Def(method_def) = item {
                    methods_by_name
                        .entry(method_def.sig.name.as_str())
                        .or_default()
                        .push(method_def);
                }
            }
        }
    }

    let mut matched_actions = HashSet::new();
    let mut ambiguous_names = HashSet::new();

    for (name, methods) in &methods_by_name {
        let Some(candidates) = actions_by_name.get(name) else {
            continue;
        };

        for method in methods {
            if candidates.len() > 1 {
                let Some(receiver_state) = receiver_state_name(method) else {
                    errors.push(
                        REK::LinearMethodAmbiguousReceiver(
                            type_name.to_string(),
                            (*name).to_string(),
                        )
                        .at(method.sig.self_param.span),
                    );
                    ambiguous_names.insert((*name).to_string());
                    continue;
                };

                let Some(action) = candidates
                    .iter()
                    .copied()
                    .find(|action| action.source_state == receiver_state)
                else {
                    errors.push(
                        REK::LinearMethodSourceStateMismatch(
                            type_name.to_string(),
                            (*name).to_string(),
                            format_action_sources(candidates),
                            receiver_state,
                        )
                        .at(method.sig.self_param.span),
                    );
                    continue;
                };

                matched_actions.insert(action_key(action));
                validate_action_method_signature(type_name, action, method, errors);
            } else {
                let action = candidates[0];
                if let Some(receiver_state) = receiver_state_name(method)
                    && receiver_state != action.source_state
                {
                    errors.push(
                        REK::LinearMethodSourceStateMismatch(
                            type_name.to_string(),
                            (*name).to_string(),
                            action.source_state.clone(),
                            receiver_state,
                        )
                        .at(method.sig.self_param.span),
                    );
                    continue;
                }

                matched_actions.insert(action_key(action));
                validate_action_method_signature(type_name, action, method, errors);
            }
        }
    }

    for action in actions {
        if matched_actions.contains(&action_key(action)) || ambiguous_names.contains(&action.name) {
            continue;
        }

        let methods = methods_by_name.get(action.name.as_str());
        let has_matching_receiver = methods.is_some_and(|methods| {
            methods.iter().any(|method| {
                receiver_state_name(method)
                    .is_some_and(|receiver_state| receiver_state == action.source_state)
            })
        });

        if methods.is_none()
            || (actions_by_name[action.name.as_str()].len() > 1 && !has_matching_receiver)
        {
            errors.push(
                REK::LinearMethodMissingAction(
                    type_name.to_string(),
                    action.name.clone(),
                    action.source_state.clone(),
                )
                .at(action.span),
            );
        }
    }
}

fn validate_action_method_signature(
    type_name: &str,
    action: &LinearTransitionDecl,
    method: &MethodDef,
    errors: &mut Vec<ResolveError>,
) {
    if !params_match(&action.params, &method.sig.params) {
        errors.push(
            REK::LinearMethodParamMismatch(type_name.to_string(), action.name.clone())
                .at(method.sig.span),
        );
    }

    if !return_type_matches(action, &method.sig.ret_ty_expr) {
        errors.push(
            REK::LinearMethodTargetStateMismatch(
                type_name.to_string(),
                action.name.clone(),
                render_action_return(action),
                format!("{}", method.sig.ret_ty_expr),
            )
            .at(method.sig.ret_ty_expr.span),
        );
    }
}

fn params_match(
    action_params: &[crate::core::ast::LinearTransitionParam],
    method_params: &[Param],
) -> bool {
    action_params.len() == method_params.len()
        && action_params
            .iter()
            .zip(method_params)
            .all(|(action, method)| same_type_expr(&action.ty, &method.typ))
}

fn return_type_matches(action: &LinearTransitionDecl, ret_ty_expr: &TypeExpr) -> bool {
    if action.error_ty_expr.is_none() {
        return named_type_name(ret_ty_expr).is_some_and(|name| name == action.target_state);
    }

    let Some(error_ty_expr) = &action.error_ty_expr else {
        unreachable!();
    };
    let TypeExprKind::Union { variants } = &ret_ty_expr.kind else {
        return false;
    };
    let Some((ok, errs)) = variants.split_first() else {
        return false;
    };
    if !named_type_name(ok).is_some_and(|name| name == action.target_state) {
        return false;
    }

    let expected_errs = flatten_union_variants(error_ty_expr);
    errs.len() == expected_errs.len()
        && errs
            .iter()
            .zip(expected_errs.iter())
            .all(|(lhs, rhs)| same_type_expr(lhs, rhs))
}

fn flatten_union_variants<'a>(ty_expr: &'a TypeExpr) -> Vec<&'a TypeExpr> {
    match &ty_expr.kind {
        TypeExprKind::Union { variants } => variants.iter().collect(),
        _ => vec![ty_expr],
    }
}

fn same_type_expr(lhs: &TypeExpr, rhs: &TypeExpr) -> bool {
    match (&lhs.kind, &rhs.kind) {
        (TypeExprKind::Infer, TypeExprKind::Infer) => true,
        (TypeExprKind::Union { variants: lhs }, TypeExprKind::Union { variants: rhs }) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs)
                    .all(|(lhs, rhs)| same_type_expr(lhs, rhs))
        }
        (
            TypeExprKind::Named {
                ident: lhs_ident,
                type_args: lhs_args,
            },
            TypeExprKind::Named {
                ident: rhs_ident,
                type_args: rhs_args,
            },
        ) => {
            lhs_ident == rhs_ident
                && lhs_args.len() == rhs_args.len()
                && lhs_args
                    .iter()
                    .zip(rhs_args)
                    .all(|(lhs, rhs)| same_type_expr(lhs, rhs))
        }
        (
            TypeExprKind::Refined {
                base_ty_expr: lhs_base,
                refinements: lhs_refs,
            },
            TypeExprKind::Refined {
                base_ty_expr: rhs_base,
                refinements: rhs_refs,
            },
        ) => lhs_refs == rhs_refs && same_type_expr(lhs_base, rhs_base),
        (
            TypeExprKind::Array {
                elem_ty_expr: lhs_elem,
                dims: lhs_dims,
            },
            TypeExprKind::Array {
                elem_ty_expr: rhs_elem,
                dims: rhs_dims,
            },
        ) => lhs_dims == rhs_dims && same_type_expr(lhs_elem, rhs_elem),
        (
            TypeExprKind::DynArray { elem_ty_expr: lhs },
            TypeExprKind::DynArray { elem_ty_expr: rhs },
        ) => same_type_expr(lhs, rhs),
        (
            TypeExprKind::Tuple {
                field_ty_exprs: lhs,
            },
            TypeExprKind::Tuple {
                field_ty_exprs: rhs,
            },
        ) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs)
                    .all(|(lhs, rhs)| same_type_expr(lhs, rhs))
        }
        (TypeExprKind::Slice { elem_ty_expr: lhs }, TypeExprKind::Slice { elem_ty_expr: rhs }) => {
            same_type_expr(lhs, rhs)
        }
        (TypeExprKind::Heap { elem_ty_expr: lhs }, TypeExprKind::Heap { elem_ty_expr: rhs }) => {
            same_type_expr(lhs, rhs)
        }
        (
            TypeExprKind::Ref {
                mutable: lhs_mut,
                elem_ty_expr: lhs_elem,
            },
            TypeExprKind::Ref {
                mutable: rhs_mut,
                elem_ty_expr: rhs_elem,
            },
        ) => lhs_mut == rhs_mut && same_type_expr(lhs_elem, rhs_elem),
        (
            TypeExprKind::Fn {
                params: lhs_params,
                ret_ty_expr: lhs_ret,
            },
            TypeExprKind::Fn {
                params: rhs_params,
                ret_ty_expr: rhs_ret,
            },
        ) => {
            lhs_params.len() == rhs_params.len()
                && lhs_params.iter().zip(rhs_params).all(|(lhs, rhs)| {
                    lhs.mode == rhs.mode && same_type_expr(&lhs.ty_expr, &rhs.ty_expr)
                })
                && same_type_expr(lhs_ret, rhs_ret)
        }
        _ => false,
    }
}

fn receiver_state_name(method: &MethodDef) -> Option<String> {
    let receiver_ty_expr = method.sig.self_param.receiver_ty_expr.as_ref()?;
    named_type_name(receiver_ty_expr)
}

fn named_type_name(ty_expr: &TypeExpr) -> Option<String> {
    match &ty_expr.kind {
        TypeExprKind::Named { ident, type_args } if type_args.is_empty() => Some(ident.clone()),
        _ => None,
    }
}

fn method_blocks_by_type(module: &Module) -> HashMap<String, Vec<&MethodBlock>> {
    let mut blocks = HashMap::new();
    for item in &module.top_level_items {
        let TopLevelItem::MethodBlock(method_block) = item else {
            continue;
        };
        if method_block.trait_name.is_some() {
            continue;
        }
        blocks
            .entry(method_block.type_name.clone())
            .or_insert_with(Vec::new)
            .push(method_block);
    }
    blocks
}

fn collect_direct_linear_infos(module: &Module) -> HashMap<String, DirectLinearInfo> {
    let mut infos = HashMap::new();
    for type_def in module.type_defs() {
        let TypeDefKind::Linear { linear } = &type_def.kind else {
            continue;
        };

        let state_names = linear
            .states
            .iter()
            .map(|state| state.name.clone())
            .collect::<HashSet<_>>();
        let mut action_by_source_and_name = HashMap::new();
        for action in &linear.actions {
            action_by_source_and_name.insert(
                (action.source_state.clone(), action.name.clone()),
                DirectActionInfo {
                    internal_name: direct_action_method_name(&action.source_state, &action.name),
                    target_state: action.target_state.clone(),
                },
            );
        }

        infos.insert(
            type_def.name.clone(),
            DirectLinearInfo {
                type_name: type_def.name.clone(),
                state_names,
                action_by_source_and_name,
            },
        );
    }
    infos
}

fn rewrite_linear_type_defs(module: &mut Module, infos: &HashMap<String, DirectLinearInfo>) {
    for type_def in &mut module.top_level_items {
        let TopLevelItem::TypeDef(type_def) = type_def else {
            continue;
        };
        let Some(info) = infos.get(&type_def.name) else {
            continue;
        };
        let TypeDefKind::Linear { linear } = &type_def.kind else {
            continue;
        };
        type_def.kind = TypeDefKind::Enum {
            variants: linear
                .states
                .iter()
                .map(|state| crate::core::ast::EnumDefVariant {
                    id: state.id,
                    name: state.name.clone(),
                    payload: state.payload.clone(),
                    span: state.span,
                })
                .collect(),
        };
        let _ = info;
    }
}

fn rewrite_linear_method_blocks(
    module: &mut Module,
    infos: &HashMap<String, DirectLinearInfo>,
    node_id_gen: &mut NodeIdGen,
) {
    for item in &mut module.top_level_items {
        let TopLevelItem::MethodBlock(method_block) = item else {
            continue;
        };
        let Some(info) = infos.get(&method_block.type_name) else {
            continue;
        };

        for method_item in &mut method_block.method_items {
            let method = match method_item {
                MethodItem::Decl(method) => {
                    rewrite_linear_method_sig(&mut method.sig, info);
                    continue;
                }
                MethodItem::Def(method) => method,
            };

            let source_state = method_source_state(&method.sig, info);
            rewrite_linear_method_sig(&mut method.sig, info);
            if let Some(source_state) = source_state {
                rewrite_expr_with_linear_env(
                    &mut method.body,
                    infos,
                    Some(&info.type_name),
                    Some(&source_state),
                    node_id_gen,
                );
            } else {
                rewrite_expr_with_linear_env(
                    &mut method.body,
                    infos,
                    Some(&info.type_name),
                    None,
                    node_id_gen,
                );
            }
        }
    }
}

fn rewrite_linear_method_sig(sig: &mut crate::core::ast::MethodSig, info: &DirectLinearInfo) {
    if let Some(source_state) = method_source_state(sig, info)
        && let Some(action) = info
            .action_by_source_and_name
            .get(&(source_state, sig.name.clone()))
    {
        sig.name = action.internal_name.clone();
        sig.self_param.mode = ParamMode::Sink;
        sig.self_param.receiver_ty_expr = None;
        rewrite_return_type_to_linear_enum(&mut sig.ret_ty_expr, info);
    }
}

fn method_source_state(
    sig: &crate::core::ast::MethodSig,
    info: &DirectLinearInfo,
) -> Option<String> {
    if let Some(receiver_ty) = sig.self_param.receiver_ty_expr.as_ref()
        && let Some(name) = named_type_name(receiver_ty)
    {
        return Some(name);
    }

    let mut matches = info
        .action_by_source_and_name
        .keys()
        .filter_map(|(source_state, action_name)| {
            if action_name == &sig.name {
                Some(source_state.clone())
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    if matches.len() == 1 {
        return matches.pop();
    }
    None
}

fn rewrite_return_type_to_linear_enum(ret_ty_expr: &mut TypeExpr, info: &DirectLinearInfo) {
    match &mut ret_ty_expr.kind {
        TypeExprKind::Named { ident, type_args } if type_args.is_empty() => {
            if info.state_names.contains(ident) {
                *ident = info.type_name.clone();
            }
        }
        TypeExprKind::Union { variants } => {
            if let Some(first) = variants.first_mut() {
                rewrite_return_type_to_linear_enum(first, info);
            }
        }
        _ => {}
    }
}

fn rewrite_linear_exprs(
    module: &mut Module,
    infos: &HashMap<String, DirectLinearInfo>,
    node_id_gen: &mut NodeIdGen,
) -> Vec<ResolveError> {
    let mut errors = Vec::new();
    for item in &mut module.top_level_items {
        match item {
            TopLevelItem::FuncDef(func) => {
                errors.extend(rewrite_expr_with_linear_env(
                    &mut func.body,
                    infos,
                    None,
                    None,
                    node_id_gen,
                ));
            }
            TopLevelItem::MethodBlock(method_block) => {
                if infos.contains_key(&method_block.type_name) {
                    continue;
                }
                for method_item in &mut method_block.method_items {
                    if let MethodItem::Def(method) = method_item {
                        errors.extend(rewrite_expr_with_linear_env(
                            &mut method.body,
                            infos,
                            None,
                            None,
                            node_id_gen,
                        ));
                    }
                }
            }
            _ => {}
        }
    }
    errors
}

fn rewrite_expr_with_linear_env(
    expr: &mut Expr,
    infos: &HashMap<String, DirectLinearInfo>,
    current_linear_type: Option<&str>,
    self_state: Option<&str>,
    node_id_gen: &mut NodeIdGen,
) -> Vec<ResolveError> {
    let mut env = HashMap::new();
    let mut errors = Vec::new();
    if let (Some(type_name), Some(state_name)) = (current_linear_type, self_state) {
        env.insert(
            "self".to_string(),
            LinearBindingState {
                actual_ident: "self".to_string(),
                value_state: LinearValueState {
                    type_name: type_name.to_string(),
                    state_name: state_name.to_string(),
                },
                consumed: false,
            },
        );
    }
    let _ = rewrite_expr_in_scope(
        expr,
        infos,
        current_linear_type,
        &mut env,
        &mut errors,
        node_id_gen,
    );
    errors
}

fn rewrite_expr_in_scope(
    expr: &mut Expr,
    infos: &HashMap<String, DirectLinearInfo>,
    current_linear_type: Option<&str>,
    env: &mut HashMap<String, LinearBindingState>,
    errors: &mut Vec<ResolveError>,
    node_id_gen: &mut NodeIdGen,
) -> Option<LinearValueState> {
    match &mut expr.kind {
        ExprKind::Block { items, tail } => {
            let mut scope_env = env.clone();
            for item in items {
                match item {
                    BlockItem::Stmt(stmt) => rewrite_stmt_in_scope(
                        stmt,
                        infos,
                        current_linear_type,
                        &mut scope_env,
                        errors,
                        node_id_gen,
                    ),
                    BlockItem::Expr(expr) => {
                        let _ = rewrite_expr_in_scope(
                            expr,
                            infos,
                            current_linear_type,
                            &mut scope_env,
                            errors,
                            node_id_gen,
                        );
                    }
                }
            }
            tail.as_mut().and_then(|tail| {
                rewrite_expr_in_scope(
                    tail,
                    infos,
                    current_linear_type,
                    &mut scope_env,
                    errors,
                    node_id_gen,
                )
            })
        }
        ExprKind::MethodCall {
            callee,
            method_name,
            args,
        } => {
            let callee_source_ident = match &callee.kind {
                ExprKind::Var { ident } => Some(ident.clone()),
                _ => None,
            };
            let callee_state =
                rewrite_expr_in_scope(callee, infos, current_linear_type, env, errors, node_id_gen);
            for arg in args {
                let _ = rewrite_expr_in_scope(
                    &mut arg.expr,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
            let Some(callee_state) = callee_state else {
                return None;
            };
            let Some(info) = infos.get(&callee_state.type_name) else {
                return None;
            };
            let Some(action) = info
                .action_by_source_and_name
                .get(&(callee_state.state_name.clone(), method_name.clone()))
            else {
                return None;
            };
            *method_name = action.internal_name.clone();
            if let Some(source_ident) = callee_source_ident
                && let Some(binding) = env.get_mut(&source_ident)
            {
                binding.consumed = true;
            }
            Some(LinearValueState {
                type_name: info.type_name.clone(),
                state_name: action.target_state.clone(),
            })
        }
        ExprKind::StructLit { name, fields, .. } => {
            for field in fields.iter_mut() {
                let _ = rewrite_expr_in_scope(
                    &mut field.value,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
            if !fields.is_empty() {
                return None;
            }

            if let Some((type_name, state_name)) = parse_qualified_linear_state_name(name, infos) {
                expr.kind = ExprKind::EnumVariant {
                    enum_name: type_name.clone(),
                    type_args: Vec::new(),
                    variant: state_name.clone(),
                    payload: Vec::new(),
                };
                return Some(LinearValueState {
                    type_name,
                    state_name,
                });
            }

            let Some(type_name) = current_linear_type else {
                return None;
            };
            let Some(info) = infos.get(type_name) else {
                return None;
            };
            if info.state_names.contains(name) {
                let state_name = name.clone();
                expr.kind = ExprKind::EnumVariant {
                    enum_name: type_name.to_string(),
                    type_args: Vec::new(),
                    variant: state_name.clone(),
                    payload: Vec::new(),
                };
                return Some(LinearValueState {
                    type_name: type_name.to_string(),
                    state_name,
                });
            }
            None
        }
        ExprKind::Call { callee, args } => {
            let _ =
                rewrite_expr_in_scope(callee, infos, current_linear_type, env, errors, node_id_gen);
            for arg in args.iter_mut() {
                let _ = rewrite_expr_in_scope(
                    &mut arg.expr,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }

            let ExprKind::Var { ident } = &callee.kind else {
                return None;
            };
            let state_name = ident.clone();
            let Some(type_name) = current_linear_type else {
                return None;
            };
            let Some(info) = infos.get(type_name) else {
                return None;
            };
            if !info.state_names.contains(&state_name) {
                return None;
            }

            let payload = args.iter().map(|arg| arg.expr.clone()).collect::<Vec<_>>();
            expr.kind = ExprKind::EnumVariant {
                enum_name: type_name.to_string(),
                type_args: Vec::new(),
                variant: state_name.clone(),
                payload,
            };
            Some(LinearValueState {
                type_name: type_name.to_string(),
                state_name,
            })
        }
        ExprKind::EnumVariant {
            enum_name,
            variant,
            payload,
            ..
        } => {
            for value in payload {
                let _ = rewrite_expr_in_scope(
                    value,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
            if infos.contains_key(enum_name) {
                return Some(LinearValueState {
                    type_name: enum_name.clone(),
                    state_name: variant.clone(),
                });
            }
            None
        }
        ExprKind::Match { scrutinee, arms } => {
            let _ = rewrite_expr_in_scope(
                scrutinee,
                infos,
                current_linear_type,
                env,
                errors,
                node_id_gen,
            );
            for arm in arms {
                let mut arm_env = env.clone();
                let _ = rewrite_expr_in_scope(
                    &mut arm.body,
                    infos,
                    current_linear_type,
                    &mut arm_env,
                    errors,
                    node_id_gen,
                );
            }
            None
        }
        ExprKind::If {
            cond,
            then_body,
            else_body,
        } => {
            let _ =
                rewrite_expr_in_scope(cond, infos, current_linear_type, env, errors, node_id_gen);
            let mut then_env = env.clone();
            let mut else_env = env.clone();
            let _ = rewrite_expr_in_scope(
                then_body,
                infos,
                current_linear_type,
                &mut then_env,
                errors,
                node_id_gen,
            );
            let _ = rewrite_expr_in_scope(
                else_body,
                infos,
                current_linear_type,
                &mut else_env,
                errors,
                node_id_gen,
            );
            None
        }
        ExprKind::TupleLit(values) => {
            for value in values {
                let _ = rewrite_expr_in_scope(
                    value,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
            None
        }
        ExprKind::ArrayLit { init, .. } => {
            match init {
                ArrayLitInit::Elems(values) => {
                    for value in values {
                        let _ = rewrite_expr_in_scope(
                            value,
                            infos,
                            current_linear_type,
                            env,
                            errors,
                            node_id_gen,
                        );
                    }
                }
                ArrayLitInit::Repeat(value, _) => {
                    let _ = rewrite_expr_in_scope(
                        value,
                        infos,
                        current_linear_type,
                        env,
                        errors,
                        node_id_gen,
                    );
                }
            }
            None
        }
        ExprKind::SetLit { elems, .. } => {
            for elem in elems {
                let _ = rewrite_expr_in_scope(
                    elem,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
            None
        }
        ExprKind::MapLit { entries, .. } => {
            for entry in entries {
                let _ = rewrite_expr_in_scope(
                    &mut entry.key,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
                let _ = rewrite_expr_in_scope(
                    &mut entry.value,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
            None
        }
        ExprKind::Var { ident } => {
            let source_ident = ident.clone();
            let Some(binding) = env.get(&source_ident) else {
                return None;
            };
            if binding.actual_ident != source_ident {
                *ident = binding.actual_ident.clone();
            }
            if binding.consumed {
                errors.push(REK::LinearUseAfterConsume(source_ident).at(expr.span));
            }
            Some(binding.value_state.clone())
        }
        _ => {
            walk_child_exprs(expr, infos, current_linear_type, env, errors, node_id_gen);
            None
        }
    }
}

fn rewrite_stmt_in_scope(
    stmt: &mut StmtExpr,
    infos: &HashMap<String, DirectLinearInfo>,
    current_linear_type: Option<&str>,
    env: &mut HashMap<String, LinearBindingState>,
    errors: &mut Vec<ResolveError>,
    node_id_gen: &mut NodeIdGen,
) {
    match &mut stmt.kind {
        StmtExprKind::LetBind { pattern, value, .. }
        | StmtExprKind::VarBind { pattern, value, .. } => {
            let result_state =
                rewrite_expr_in_scope(value, infos, current_linear_type, env, errors, node_id_gen);
            if let Some(ident) = bind_pattern_name_mut(pattern) {
                let source_ident = ident.clone();
                if let Some(result_state) = result_state {
                    let actual_ident = if env.contains_key(&source_ident) {
                        fresh_linear_binding_name(&source_ident, node_id_gen)
                    } else {
                        source_ident.clone()
                    };
                    if actual_ident != source_ident {
                        *ident = actual_ident.clone();
                    }
                    env.insert(
                        source_ident,
                        LinearBindingState {
                            actual_ident,
                            value_state: result_state,
                            consumed: false,
                        },
                    );
                } else {
                    env.remove(&source_ident);
                }
            }
        }
        StmtExprKind::Assign {
            assignee, value, ..
        }
        | StmtExprKind::CompoundAssign {
            assignee, value, ..
        } => {
            let _ = rewrite_expr_in_scope(
                assignee,
                infos,
                current_linear_type,
                env,
                errors,
                node_id_gen,
            );
            let _ =
                rewrite_expr_in_scope(value, infos, current_linear_type, env, errors, node_id_gen);
        }
        StmtExprKind::While { cond, body } => {
            let _ =
                rewrite_expr_in_scope(cond, infos, current_linear_type, env, errors, node_id_gen);
            let mut body_env = env.clone();
            let _ = rewrite_expr_in_scope(
                body,
                infos,
                current_linear_type,
                &mut body_env,
                errors,
                node_id_gen,
            );
        }
        StmtExprKind::For { iter, body, .. } => {
            let _ =
                rewrite_expr_in_scope(iter, infos, current_linear_type, env, errors, node_id_gen);
            let mut body_env = env.clone();
            let _ = rewrite_expr_in_scope(
                body,
                infos,
                current_linear_type,
                &mut body_env,
                errors,
                node_id_gen,
            );
        }
        StmtExprKind::Defer { value } => {
            let _ =
                rewrite_expr_in_scope(value, infos, current_linear_type, env, errors, node_id_gen);
        }
        StmtExprKind::Using {
            value,
            body,
            binding,
        } => {
            let result_state =
                rewrite_expr_in_scope(value, infos, current_linear_type, env, errors, node_id_gen);
            let mut body_env = env.clone();
            if let Some(result_state) = result_state {
                body_env.insert(
                    binding.ident.clone(),
                    LinearBindingState {
                        actual_ident: binding.ident.clone(),
                        value_state: result_state,
                        consumed: false,
                    },
                );
            }
            let _ = rewrite_expr_in_scope(
                body,
                infos,
                current_linear_type,
                &mut body_env,
                errors,
                node_id_gen,
            );
        }
        StmtExprKind::Return { value } => {
            if let Some(value) = value {
                let _ = rewrite_expr_in_scope(
                    value,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
        }
        StmtExprKind::VarDecl { ident, .. } => {
            env.remove(ident);
        }
        StmtExprKind::Break | StmtExprKind::Continue => {}
    }
}

fn walk_child_exprs(
    expr: &mut Expr,
    infos: &HashMap<String, DirectLinearInfo>,
    current_linear_type: Option<&str>,
    env: &mut HashMap<String, LinearBindingState>,
    errors: &mut Vec<ResolveError>,
    node_id_gen: &mut NodeIdGen,
) {
    match &mut expr.kind {
        ExprKind::BinOp { left, right, .. } => {
            let _ =
                rewrite_expr_in_scope(left, infos, current_linear_type, env, errors, node_id_gen);
            let _ =
                rewrite_expr_in_scope(right, infos, current_linear_type, env, errors, node_id_gen);
        }
        ExprKind::UnaryOp { expr, .. }
        | ExprKind::HeapAlloc { expr }
        | ExprKind::Move { expr }
        | ExprKind::Coerce { expr, .. }
        | ExprKind::ImplicitMove { expr }
        | ExprKind::AddrOf { expr }
        | ExprKind::Deref { expr }
        | ExprKind::Load { expr }
        | ExprKind::Len { expr } => {
            let _ =
                rewrite_expr_in_scope(expr, infos, current_linear_type, env, errors, node_id_gen);
        }
        ExprKind::Try {
            fallible_expr,
            on_error,
        } => {
            let _ = rewrite_expr_in_scope(
                fallible_expr,
                infos,
                current_linear_type,
                env,
                errors,
                node_id_gen,
            );
            if let Some(on_error) = on_error {
                let _ = rewrite_expr_in_scope(
                    on_error,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
        }
        ExprKind::StructUpdate { target, fields } => {
            let _ =
                rewrite_expr_in_scope(target, infos, current_linear_type, env, errors, node_id_gen);
            for field in fields {
                let _ = rewrite_expr_in_scope(
                    &mut field.value,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
        }
        ExprKind::Range { start, end } => {
            let _ =
                rewrite_expr_in_scope(start, infos, current_linear_type, env, errors, node_id_gen);
            let _ =
                rewrite_expr_in_scope(end, infos, current_linear_type, env, errors, node_id_gen);
        }
        ExprKind::Slice { target, start, end } => {
            let _ =
                rewrite_expr_in_scope(target, infos, current_linear_type, env, errors, node_id_gen);
            if let Some(start) = start {
                let _ = rewrite_expr_in_scope(
                    start,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
            if let Some(end) = end {
                let _ = rewrite_expr_in_scope(
                    end,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
        }
        ExprKind::TupleField { target, .. } | ExprKind::StructField { target, .. } => {
            let _ =
                rewrite_expr_in_scope(target, infos, current_linear_type, env, errors, node_id_gen);
        }
        ExprKind::ArrayIndex { target, indices } => {
            let _ =
                rewrite_expr_in_scope(target, infos, current_linear_type, env, errors, node_id_gen);
            for index in indices {
                let _ = rewrite_expr_in_scope(
                    index,
                    infos,
                    current_linear_type,
                    env,
                    errors,
                    node_id_gen,
                );
            }
        }
        ExprKind::Reply { cap, value } => {
            let _ =
                rewrite_expr_in_scope(cap, infos, current_linear_type, env, errors, node_id_gen);
            let _ =
                rewrite_expr_in_scope(value, infos, current_linear_type, env, errors, node_id_gen);
        }
        ExprKind::Closure { body, .. } => {
            let mut closure_env = HashMap::new();
            let _ = rewrite_expr_in_scope(
                body,
                infos,
                current_linear_type,
                &mut closure_env,
                errors,
                node_id_gen,
            );
        }
        ExprKind::MapGet { target, key } => {
            let _ =
                rewrite_expr_in_scope(target, infos, current_linear_type, env, errors, node_id_gen);
            let _ =
                rewrite_expr_in_scope(key, infos, current_linear_type, env, errors, node_id_gen);
        }
        ExprKind::StringFmt { segments } => {
            for segment in segments {
                if let crate::core::ast::StringFmtSegment::Expr { expr, .. } = segment {
                    let _ = rewrite_expr_in_scope(
                        expr,
                        infos,
                        current_linear_type,
                        env,
                        errors,
                        node_id_gen,
                    );
                }
            }
        }
        ExprKind::Call { .. }
        | ExprKind::MethodCall { .. }
        | ExprKind::StructLit { .. }
        | ExprKind::EnumVariant { .. }
        | ExprKind::Match { .. }
        | ExprKind::If { .. }
        | ExprKind::Block { .. }
        | ExprKind::TupleLit(..)
        | ExprKind::ArrayLit { .. }
        | ExprKind::SetLit { .. }
        | ExprKind::MapLit { .. }
        | ExprKind::Var { .. }
        | ExprKind::UnitLit
        | ExprKind::IntLit(_)
        | ExprKind::BoolLit(_)
        | ExprKind::CharLit(_)
        | ExprKind::StringLit { .. }
        | ExprKind::Emit { .. }
        | ExprKind::ClosureRef { .. } => {}
    }
}

fn parse_qualified_linear_state_name(
    name: &str,
    infos: &HashMap<String, DirectLinearInfo>,
) -> Option<(String, String)> {
    let (type_name, state_name) = name.split_once("::")?;
    let info = infos.get(type_name)?;
    if info.state_names.contains(state_name) {
        Some((type_name.to_string(), state_name.to_string()))
    } else {
        None
    }
}

fn bind_pattern_name_mut(pattern: &mut crate::core::ast::BindPattern) -> Option<&mut String> {
    match &mut pattern.kind {
        crate::core::ast::BindPatternKind::Name { ident } => Some(ident),
        _ => None,
    }
}

fn direct_action_method_name(source_state: &str, action_name: &str) -> String {
    format!("__linear__{source_state}__{action_name}")
}

fn fresh_linear_binding_name(source_ident: &str, node_id_gen: &mut NodeIdGen) -> String {
    format!("{source_ident}$linear${}", node_id_gen.new_id().0)
}

fn action_key(action: &LinearTransitionDecl) -> (String, String) {
    (action.name.clone(), action.source_state.clone())
}

fn format_action_sources(actions: &[&LinearTransitionDecl]) -> String {
    actions
        .iter()
        .map(|action| action.source_state.as_str())
        .collect::<Vec<_>>()
        .join(" | ")
}

fn render_action_return(action: &LinearTransitionDecl) -> String {
    if let Some(error_ty_expr) = &action.error_ty_expr {
        format!("{} | {}", action.target_state, error_ty_expr)
    } else {
        action.target_state.clone()
    }
}
