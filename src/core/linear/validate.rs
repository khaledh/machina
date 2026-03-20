//! Validation of `@linear type` and `machine` declarations.
//!
//! Runs before resolve on the raw parsed AST. Checks:
//! - States: at least one, no duplicates, at least one non-final
//! - Transitions (actions/triggers): known states, no final source, no duplicates
//! - Roles: all referenced actions exist
//! - Method blocks: each declared action has a matching method with correct
//!   receiver, return type, and parameters; overloaded actions require explicit
//!   receiver state annotations
//! - Machines: hosted type exists, is linear, key field is valid, and machine
//!   handlers match the hosted action/trigger declarations

use std::collections::{HashMap, HashSet};

use crate::core::ast::visit::{self, Visitor};
use crate::core::ast::{EmitKind, Expr, ExprKind, LinearTransitionParam, LinearTypeDef, Param};
use crate::core::ast::{
    LinearRoleDecl, LinearStateVariant, LinearTransitionDecl, MachineDef, MachineItem,
    MachineOnHandler, MachineTransitionHandler, MethodBlock, MethodDef, MethodItem, Module,
    TopLevelItem, TypeDef, TypeDefKind, TypeExpr, TypeExprKind,
};
use crate::core::diag::Span;
use crate::core::resolve::{REK, ResolveError};

pub fn validate_module(module: &Module) -> Vec<ResolveError> {
    let method_blocks = method_blocks_by_type(module);
    let type_defs = super::type_defs_by_name(module);
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
    for machine_def in module.machine_defs() {
        validate_machine_def(machine_def, &type_defs, &mut errors);
    }
    errors
}

// -- Linear type validation ------------------------------------------

fn validate_linear_type(
    type_def: &TypeDef,
    linear: &LinearTypeDef,
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
    type_span: Span,
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

// -- Transition declarations (actions and triggers) ------------------

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

// -- Role validation -------------------------------------------------

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

// -- Action <-> method block matching ----------------------------------
//
// Each declared action must have exactly one matching method in the type's
// method block(s). When an action name is unique (one source state), bare
// `self` is allowed. When multiple actions share the same name, the method
// must use an explicit receiver annotation (`self: StateName`) to disambiguate.

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
                // Overloaded action: receiver annotation required.
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
                // Unique action: bare `self` is fine, but if annotated, must match.
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

    // Report actions that have no matching method at all.
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

// -- Machine validation ----------------------------------------------

fn validate_machine_def(
    machine_def: &MachineDef,
    type_defs: &HashMap<String, &TypeDef>,
    errors: &mut Vec<ResolveError>,
) {
    let Some(type_def) = type_defs.get(&machine_def.host.type_name) else {
        errors.push(
            REK::MachineHostedTypeUndefined(
                machine_def.name.clone(),
                machine_def.host.type_name.clone(),
            )
            .at(machine_def.host.span),
        );
        return;
    };

    let TypeDefKind::Linear { linear } = &type_def.kind else {
        errors.push(
            REK::MachineHostedTypeNotLinear(
                machine_def.name.clone(),
                machine_def.host.type_name.clone(),
            )
            .at(machine_def.host.span),
        );
        return;
    };

    let has_key_field = linear
        .fields
        .iter()
        .any(|field| field.name == machine_def.host.key_field);
    if !has_key_field {
        errors.push(
            REK::MachineInvalidKeyField(
                machine_def.name.clone(),
                machine_def.host.type_name.clone(),
                machine_def.host.key_field.clone(),
            )
            .at(machine_def.host.span),
        );
    }

    validate_machine_handlers(machine_def, linear, type_defs, errors);
}

fn validate_machine_handlers(
    machine_def: &MachineDef,
    linear: &LinearTypeDef,
    type_defs: &HashMap<String, &TypeDef>,
    errors: &mut Vec<ResolveError>,
) {
    let actions_by_name: HashMap<&str, &LinearTransitionDecl> = linear
        .actions
        .iter()
        .map(|decl| (decl.name.as_str(), decl))
        .collect();
    let triggers_by_name: HashMap<&str, &LinearTransitionDecl> = linear
        .triggers
        .iter()
        .map(|decl| (decl.name.as_str(), decl))
        .collect();
    let mut seen_trigger_handlers = HashSet::new();

    for item in &machine_def.items {
        match item {
            MachineItem::Action(handler) => {
                let Some(action) = actions_by_name.get(handler.name.as_str()).copied() else {
                    errors.push(
                        REK::MachineExtraHandler(
                            machine_def.name.clone(),
                            "action",
                            handler.name.clone(),
                        )
                        .at(handler.span),
                    );
                    continue;
                };
                validate_machine_transition_handler(
                    machine_def,
                    "action",
                    action,
                    handler,
                    true,
                    errors,
                );
                validate_hosted_action_emit_usage(machine_def, handler, errors);
            }
            MachineItem::Trigger(handler) => {
                let Some(trigger) = triggers_by_name.get(handler.name.as_str()).copied() else {
                    errors.push(
                        REK::MachineExtraHandler(
                            machine_def.name.clone(),
                            "trigger",
                            handler.name.clone(),
                        )
                        .at(handler.span),
                    );
                    continue;
                };
                seen_trigger_handlers.insert(trigger.name.clone());
                validate_machine_transition_handler(
                    machine_def,
                    "trigger",
                    trigger,
                    handler,
                    false,
                    errors,
                );
            }
            MachineItem::On(handler) => {
                validate_hosted_on_handler_payload_usage(machine_def, handler, type_defs, errors);
            }
            _ => {}
        }
    }

    for trigger in &linear.triggers {
        if !seen_trigger_handlers.contains(&trigger.name) {
            errors.push(
                REK::MachineMissingTriggerHandler(machine_def.name.clone(), trigger.name.clone())
                    .at(machine_def.host.span),
            );
        }
    }
}

fn validate_hosted_action_emit_usage(
    machine_def: &MachineDef,
    handler: &MachineTransitionHandler,
    errors: &mut Vec<ResolveError>,
) {
    for span in collect_hosted_action_emit_unsupported_spans(&handler.body) {
        errors.push(
            REK::MachineHostedActionEmitUnsupported(machine_def.name.clone(), handler.name.clone())
                .at(span),
        );
    }
}

fn validate_hosted_on_handler_payload_usage(
    machine_def: &MachineDef,
    handler: &MachineOnHandler,
    type_defs: &HashMap<String, &TypeDef>,
    errors: &mut Vec<ResolveError>,
) {
    if handler.provenance.is_some() {
        return;
    }

    let Some(selector_type_name) = named_type_name(&handler.selector_ty) else {
        return;
    };
    let Some(type_def) = type_defs.get(&selector_type_name) else {
        return;
    };
    if hosted_on_payload_shape_supported(type_def) {
        return;
    }

    errors.push(
        REK::MachineHostedOnPayloadUnsupported(machine_def.name.clone(), selector_type_name)
            .at(handler.selector_ty.span),
    );
}

fn validate_machine_transition_handler(
    machine_def: &MachineDef,
    kind: &'static str,
    decl: &LinearTransitionDecl,
    handler: &MachineTransitionHandler,
    check_error_superset: bool,
    errors: &mut Vec<ResolveError>,
) {
    // The leading machine handler binder always stands for the declared source
    // state, so only the trailing params and return shape need structural
    // validation here.
    if !params_match(&decl.params, &handler.params) {
        errors.push(
            REK::MachineHandlerTypeMismatch(machine_def.name.clone(), kind, handler.name.clone())
                .at(handler.span),
        );
        return;
    }

    if kind == "action" {
        let Some(ret_ty_expr) = &handler.ret_ty_expr else {
            errors.push(
                REK::MachineHandlerTypeMismatch(
                    machine_def.name.clone(),
                    kind,
                    handler.name.clone(),
                )
                .at(handler.span),
            );
            return;
        };

        if !return_type_matches(decl, ret_ty_expr) {
            if check_error_superset
                && target_state_matches(decl, ret_ty_expr)
                && !error_superset_matches(decl, ret_ty_expr)
            {
                errors.push(
                    REK::MachineOverrideErrorSubset(machine_def.name.clone(), handler.name.clone())
                        .at(ret_ty_expr.span),
                );
            } else {
                errors.push(
                    REK::MachineHandlerTypeMismatch(
                        machine_def.name.clone(),
                        kind,
                        handler.name.clone(),
                    )
                    .at(ret_ty_expr.span),
                );
            }
        }
    }
}

// -- Helpers ---------------------------------------------------------

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

fn hosted_on_payload_shape_supported(type_def: &TypeDef) -> bool {
    let TypeDefKind::Struct { fields } = &type_def.kind else {
        return false;
    };
    match fields.as_slice() {
        [] => true,
        [field0] => is_u64_named_type(&field0.ty),
        [field0, field1] => is_u64_named_type(&field0.ty) && is_u64_named_type(&field1.ty),
        _ => false,
    }
}

fn is_u64_named_type(ty_expr: &TypeExpr) -> bool {
    matches!(
        &ty_expr.kind,
        TypeExprKind::Named { ident, type_args } if ident == "u64" && type_args.is_empty()
    )
}

fn collect_hosted_action_emit_unsupported_spans(expr: &Expr) -> Vec<Span> {
    struct EmitCollector {
        spans: Vec<Span>,
    }

    impl Visitor for EmitCollector {
        fn visit_expr(&mut self, expr: &Expr) {
            match &expr.kind {
                ExprKind::Emit {
                    kind: EmitKind::Request { .. },
                }
                | ExprKind::Reply { .. } => {
                    self.spans.push(expr.span);
                }
                _ => {}
            }
            visit::walk_expr(self, expr);
        }
    }

    let mut collector = EmitCollector { spans: Vec::new() };
    collector.visit_expr(expr);
    collector.spans
}

fn params_match(action_params: &[LinearTransitionParam], method_params: &[Param]) -> bool {
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
    if named_type_name(ok).is_none_or(|name| name != action.target_state) {
        return false;
    }

    let expected_errs = flatten_union_variants(error_ty_expr);
    errs.len() == expected_errs.len()
        && errs
            .iter()
            .zip(expected_errs.iter())
            .all(|(lhs, rhs)| same_type_expr(lhs, rhs))
}

fn target_state_matches(action: &LinearTransitionDecl, ret_ty_expr: &TypeExpr) -> bool {
    if action.error_ty_expr.is_none() {
        return named_type_name(ret_ty_expr).is_some_and(|name| name == action.target_state);
    }

    let TypeExprKind::Union { variants } = &ret_ty_expr.kind else {
        return false;
    };
    let Some((ok, _)) = variants.split_first() else {
        return false;
    };
    named_type_name(ok).is_some_and(|name| name == action.target_state)
}

fn error_superset_matches(action: &LinearTransitionDecl, ret_ty_expr: &TypeExpr) -> bool {
    let Some(error_ty_expr) = &action.error_ty_expr else {
        return true;
    };
    let TypeExprKind::Union { variants } = &ret_ty_expr.kind else {
        return false;
    };
    let Some((_, errs)) = variants.split_first() else {
        return false;
    };
    let expected_errs = flatten_union_variants(error_ty_expr);
    errs.len() >= expected_errs.len()
        && expected_errs
            .iter()
            .all(|expected| errs.iter().any(|found| same_type_expr(found, expected)))
}

fn flatten_union_variants(ty_expr: &TypeExpr) -> Vec<&TypeExpr> {
    match &ty_expr.kind {
        TypeExprKind::Union { variants } => variants.iter().collect(),
        _ => vec![ty_expr],
    }
}

/// Structural equality of type expressions (AST-level, pre-resolve).
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
