//! Pass 4 of the type checker: validate control-flow semantics.
//!
//! This pass handles semantic checks that are not pure type equalities or
//! assignability relations (e.g. `break`/`continue` scope and return rules).

use std::collections::{HashMap, HashSet};

use crate::core::analysis::dataflow::{DataflowGraph, solve_forward};
use crate::core::machine::naming::parse_generated_handler_site_label;
use crate::core::resolve::DefId;
use crate::core::tree::NodeId;
use crate::core::tree::cfg::{AstBlockId, TreeCfgBuilder, TreeCfgItem, TreeCfgNode};
use crate::core::tree::visit::{self, Visitor};
use crate::core::tree::{ExprKind, MethodItem};
use crate::core::typecheck::constraints::ControlFact;
use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::core::typecheck::type_map::resolve_type_expr;
use crate::core::types::{Type, TypeAssignability, type_assignable};

/// Pass 4: semantic checks that are not pure type equalities/assignability.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let mut errors = Vec::new();
    for fact in &engine.state().constrain.control_facts {
        match fact {
            ControlFact::Break {
                loop_depth, span, ..
            } if *loop_depth == 0 => {
                errors.push(TypeCheckErrorKind::BreakOutsideLoop(*span).into())
            }
            ControlFact::Continue {
                loop_depth, span, ..
            } if *loop_depth == 0 => {
                errors.push(TypeCheckErrorKind::ContinueOutsideLoop(*span).into());
            }
            ControlFact::Return {
                has_value,
                expected_return_ty,
                span,
                ..
            } => match expected_return_ty {
                None => {
                    errors.push(TypeCheckErrorKind::ReturnOutsideFunction(*span).into());
                }
                Some(expected_ty) => {
                    let expected_ty = engine.type_vars().apply(expected_ty);
                    if *has_value && expected_ty == Type::Unit {
                        errors.push(TypeCheckErrorKind::ReturnValueUnexpected(*span).into());
                    } else if !*has_value && expected_ty != Type::Unit {
                        errors.push(
                            TypeCheckErrorKind::ReturnValueMissing(expected_ty, *span).into(),
                        );
                    }
                }
            },
            _ => {}
        }
    }
    errors.extend(check_protocol_shape_conformance(engine));
    errors.extend(check_typestate_handler_overlap(engine));
    errors.extend(check_typestate_request_response_shape(engine));
    errors.extend(check_reply_cap_usage(engine));

    if errors.is_empty() {
        Ok(())
    } else {
        engine.state_mut().diags.extend(errors.clone());
        Err(errors)
    }
}

fn check_protocol_shape_conformance(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
    let resolved = engine.context();
    if resolved.typestate_role_impls.is_empty() {
        return Vec::new();
    }

    let typestate_names: HashSet<String> = resolved
        .typestate_role_impls
        .iter()
        .map(|binding| binding.typestate_name.clone())
        .collect();
    let handler_payloads_by_state =
        collect_typestate_handler_payloads_by_state(engine, &typestate_names);
    let outgoing_emits_by_state =
        collect_typestate_outgoing_payloads_by_state(engine, &typestate_names);
    let handler_payloads = collect_typestate_handler_payloads(&handler_payloads_by_state);
    let outgoing_payloads = collect_typestate_outgoing_payloads(&outgoing_emits_by_state);

    let mut errors = Vec::new();
    for binding in &resolved.typestate_role_impls {
        // Role path/def validity is resolver's responsibility. Skip malformed
        // entries to avoid duplicate/symptom diagnostics here.
        if binding.path.len() < 2 || binding.role_def_id.is_none() {
            continue;
        }

        let protocol_name = &binding.path[0];
        let role_name = &binding.path[1];
        let role_label = binding.path.join("::");
        let Some(protocol_fact) = resolved.protocol_index.protocols.get(protocol_name) else {
            continue;
        };
        let Some(role_fact) = protocol_fact.roles.get(role_name) else {
            continue;
        };
        let peer_role_by_field: HashMap<&str, &str> = binding
            .peer_role_bindings
            .iter()
            .map(|peer| (peer.field_name.as_str(), peer.role_name.as_str()))
            .collect();

        if !role_fact.states.is_empty() {
            for state_fact in role_fact.states.values() {
                let key = TypestateStateKey {
                    typestate_name: binding.typestate_name.clone(),
                    state_name: state_fact.name.clone(),
                };
                let seen_handlers = handler_payloads_by_state
                    .get(&key)
                    .cloned()
                    .unwrap_or_default();
                for required in &state_fact.shape.required_incoming {
                    if !seen_handlers.contains(required) {
                        errors.push(
                            TypeCheckErrorKind::ProtocolStateHandlerMissing(
                                binding.typestate_name.clone(),
                                role_label.clone(),
                                state_fact.name.clone(),
                                required.clone(),
                                binding.span,
                            )
                            .into(),
                        );
                    }
                }

                if let Some(emits) = outgoing_emits_by_state.get(&key) {
                    for emit in emits {
                        if !state_fact.shape.allowed_outgoing.contains(&emit.payload_ty) {
                            errors.push(
                                TypeCheckErrorKind::ProtocolStateOutgoingPayloadNotAllowed(
                                    binding.typestate_name.clone(),
                                    role_label.clone(),
                                    state_fact.name.clone(),
                                    emit.payload_ty.clone(),
                                    emit.span,
                                )
                                .into(),
                            );
                            continue;
                        }

                        let mut expected_roles =
                            state_expected_roles_for_payload(state_fact, &emit.payload_ty);
                        if emit.is_request {
                            expected_roles.extend(request_contract_to_roles_for_payload(
                                protocol_fact,
                                role_name,
                                &emit.payload_ty,
                            ));
                            expected_roles.sort();
                            expected_roles.dedup();
                        }
                        if expected_roles.is_empty() {
                            continue;
                        }
                        let Some((field_name, bound_role_name)) =
                            resolve_emit_destination_role(emit, &peer_role_by_field)
                        else {
                            errors.push(
                                TypeCheckErrorKind::ProtocolStateEmitDestinationRoleUnbound(
                                    binding.typestate_name.clone(),
                                    role_label.clone(),
                                    state_fact.name.clone(),
                                    emit.payload_ty.clone(),
                                    expected_roles.join(" | "),
                                    emit.span,
                                )
                                .into(),
                            );
                            continue;
                        };
                        if !expected_roles.iter().any(|role| role == bound_role_name) {
                            errors.push(
                                TypeCheckErrorKind::ProtocolStateEmitDestinationRoleMismatch(
                                    binding.typestate_name.clone(),
                                    role_label.clone(),
                                    state_fact.name.clone(),
                                    emit.payload_ty.clone(),
                                    expected_roles.join(" | "),
                                    field_name.to_string(),
                                    bound_role_name.to_string(),
                                    emit.span,
                                )
                                .into(),
                            );
                            continue;
                        }

                        if !emit.is_request {
                            continue;
                        }
                        let Some((_, to_role_name)) =
                            resolve_emit_destination_role(emit, &peer_role_by_field)
                        else {
                            continue;
                        };
                        let matching_contracts = matching_request_contracts(
                            protocol_fact,
                            role_name,
                            to_role_name,
                            &emit.payload_ty,
                        );
                        if matching_contracts.is_empty() {
                            errors.push(
                                TypeCheckErrorKind::ProtocolRequestContractMissing(
                                    binding.typestate_name.clone(),
                                    role_label.clone(),
                                    emit.payload_ty.clone(),
                                    to_role_name.to_string(),
                                    emit.span,
                                )
                                .into(),
                            );
                            continue;
                        }
                        if matching_contracts.len() > 1 {
                            errors.push(
                                TypeCheckErrorKind::ProtocolRequestContractAmbiguous(
                                    binding.typestate_name.clone(),
                                    role_label.clone(),
                                    emit.payload_ty.clone(),
                                    to_role_name.to_string(),
                                    emit.span,
                                )
                                .into(),
                            );
                            continue;
                        }
                        let contract_responses = &matching_contracts[0].response_tys;
                        let response_tys = emit.request_response_tys.as_deref().unwrap_or_default();
                        for response_ty in response_tys {
                            if !contract_responses.contains(response_ty) {
                                errors.push(
                                    TypeCheckErrorKind::ProtocolRequestResponseNotInContract(
                                        binding.typestate_name.clone(),
                                        role_label.clone(),
                                        emit.payload_ty.clone(),
                                        to_role_name.to_string(),
                                        response_ty.clone(),
                                        emit.span,
                                    )
                                    .into(),
                                );
                            }
                        }
                    }
                }
            }

            // State-oriented protocols are validated state-by-state only.
            // Role-wide flow checks are reserved for legacy flow-only roles.
            continue;
        }

        let required_incoming = &role_fact.shape.required_incoming;
        let allowed_outgoing = &role_fact.shape.allowed_outgoing;

        let seen_handlers = handler_payloads
            .get(&binding.typestate_name)
            .cloned()
            .unwrap_or_default();
        for required in required_incoming {
            if !seen_handlers.contains(required) {
                errors.push(
                    TypeCheckErrorKind::ProtocolFlowHandlerMissing(
                        binding.typestate_name.clone(),
                        role_label.clone(),
                        required.clone(),
                        binding.span,
                    )
                    .into(),
                );
            }
        }

        if let Some(emits) = outgoing_payloads.get(&binding.typestate_name) {
            for emit in emits {
                if !allowed_outgoing.contains(&emit.payload_ty) {
                    errors.push(
                        TypeCheckErrorKind::ProtocolOutgoingPayloadNotAllowed(
                            binding.typestate_name.clone(),
                            role_label.clone(),
                            emit.payload_ty.clone(),
                            emit.span,
                        )
                        .into(),
                    );
                }
            }
        }
    }

    errors
}

#[derive(Clone, Debug)]
struct HandlerResponsePattern {
    selector_ty: Type,
    response_tys: Vec<Type>,
    request_site_label: Option<String>,
    span: crate::core::diag::Span,
}

fn check_typestate_handler_overlap(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
    let resolved = engine.context();
    let mut errors = Vec::new();

    for method_block in resolved.module.method_blocks() {
        let Some((typestate_name, state_name)) =
            parse_typestate_and_state_from_generated_state(&method_block.type_name)
        else {
            continue;
        };

        let patterns =
            collect_handler_response_patterns(&resolved.def_table, &resolved.module, method_block);
        for i in 0..patterns.len() {
            for j in (i + 1)..patterns.len() {
                let left = &patterns[i];
                let right = &patterns[j];
                if left.selector_ty != right.selector_ty {
                    continue;
                }

                let mut overlap = Vec::new();
                for ty in &left.response_tys {
                    if right.response_tys.contains(ty) && !overlap.contains(ty) {
                        overlap.push(ty.clone());
                    }
                }
                if overlap.is_empty() {
                    continue;
                }

                // Distinct labeled provenance handlers are deterministic:
                // runtime picks exact request-site key first.
                if let (Some(left_label), Some(right_label)) =
                    (&left.request_site_label, &right.request_site_label)
                    && left_label != right_label
                {
                    continue;
                }
                if left.request_site_label.is_none() && right.request_site_label.is_none() {
                    errors.push(
                        TypeCheckErrorKind::TypestateOverlappingOnHandlers(
                            typestate_name.clone(),
                            state_name.clone(),
                            left.selector_ty.clone(),
                            overlap,
                            right.span,
                        )
                        .into(),
                    );
                } else {
                    errors.push(
                        TypeCheckErrorKind::TypestateAmbiguousResponseProvenance(
                            typestate_name.clone(),
                            state_name.clone(),
                            left.selector_ty.clone(),
                            overlap,
                            right.span,
                        )
                        .into(),
                    );
                }
            }
        }
    }

    errors
}

fn collect_handler_response_patterns(
    def_table: &crate::core::resolve::DefTable,
    module: &crate::core::tree::resolved::Module,
    method_block: &crate::core::tree::resolved::MethodBlock,
) -> Vec<HandlerResponsePattern> {
    let mut out = Vec::new();
    for method_item in &method_block.method_items {
        let MethodItem::Def(method_def) = method_item else {
            continue;
        };
        if !method_def.sig.name.starts_with("__ts_on_") {
            continue;
        }

        // Pattern-form `on Response(pending, Variant)` lowers to:
        //   (__event, pending: Pending<...>, __response: ...)
        // We only consider these handlers for overlap checks.
        if method_def.sig.params.len() < 3 {
            continue;
        }
        let event_param = &method_def.sig.params[0];
        let pending_param = &method_def.sig.params[1];

        let Ok(selector_ty) = resolve_type_expr(def_table, module, &event_param.typ) else {
            continue;
        };
        let Ok(pending_ty) = resolve_type_expr(def_table, module, &pending_param.typ) else {
            continue;
        };
        let Type::Pending { response_tys } = pending_ty else {
            continue;
        };
        if response_tys.is_empty() {
            continue;
        }

        out.push(HandlerResponsePattern {
            selector_ty,
            response_tys,
            request_site_label: parse_generated_handler_site_label(&method_def.sig.name)
                .map(ToString::to_string),
            span: method_def.sig.span,
        });
    }
    out
}

#[derive(Clone, Debug)]
struct RequestSiteShape {
    typestate_name: String,
    request_ty: Type,
    response_tys: Vec<Type>,
    request_site_label: Option<String>,
    span: crate::core::diag::Span,
}

#[derive(Clone, Debug)]
struct ProvenanceHandlerShape {
    typestate_name: String,
    request_ty: Type,
    response_ty: Type,
    request_site_label: Option<String>,
    span: crate::core::diag::Span,
}

fn check_typestate_request_response_shape(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
    let request_sites = collect_typestate_request_sites(engine);
    let handler_shapes = collect_provenance_handler_shapes(engine);
    if request_sites.is_empty() && handler_shapes.is_empty() {
        return Vec::new();
    }

    let mut errors = Vec::new();
    let typestates_with_requests: HashSet<String> = request_sites
        .iter()
        .map(|site| site.typestate_name.clone())
        .collect();

    for site in &request_sites {
        let label_suffix = site
            .request_site_label
            .as_ref()
            .map(|label| format!(":{label}"))
            .unwrap_or_default();

        for response_ty in &site.response_tys {
            let has_handler = handler_shapes.iter().any(|handler| {
                if handler.typestate_name != site.typestate_name
                    || handler.request_ty != site.request_ty
                    || handler.response_ty != *response_ty
                {
                    return false;
                }
                match (&site.request_site_label, &handler.request_site_label) {
                    // Unlabeled request sites can only route to unlabeled handlers.
                    (None, None) => true,
                    // Labeled sites route to exact label first, then unlabeled fallback.
                    (Some(site_label), Some(handler_label)) => site_label == handler_label,
                    (Some(_), None) => true,
                    (None, Some(_)) => false,
                }
            });
            if !has_handler {
                errors.push(
                    TypeCheckErrorKind::TypestateRequestMissingResponseHandler(
                        site.typestate_name.clone(),
                        site.request_ty.clone(),
                        label_suffix.clone(),
                        response_ty.clone(),
                        site.span,
                    )
                    .into(),
                );
            }
        }
    }

    for handler in &handler_shapes {
        if !typestates_with_requests.contains(&handler.typestate_name) {
            continue;
        }
        let label_suffix = handler
            .request_site_label
            .as_ref()
            .map(|label| format!(":{label}"))
            .unwrap_or_default();
        let supported = request_sites.iter().any(|site| {
            if site.typestate_name != handler.typestate_name
                || site.request_ty != handler.request_ty
            {
                return false;
            }
            let label_matches = match (&site.request_site_label, &handler.request_site_label) {
                (Some(site_label), Some(handler_label)) => site_label == handler_label,
                (None, None) => true,
                (Some(_), None) => true,
                (None, Some(_)) => false,
            };
            label_matches && site.response_tys.contains(&handler.response_ty)
        });
        if !supported {
            errors.push(
                TypeCheckErrorKind::TypestateHandlerUnsupportedResponseVariant(
                    handler.typestate_name.clone(),
                    handler.request_ty.clone(),
                    label_suffix,
                    handler.response_ty.clone(),
                    handler.span,
                )
                .into(),
            );
        }
    }

    errors
}

fn collect_typestate_request_sites(engine: &TypecheckEngine) -> Vec<RequestSiteShape> {
    let mut collector = TypestateRequestCollector {
        node_types: &engine.state().solve.resolved_node_types,
        current_typestate: None,
        sites: Vec::new(),
    };
    collector.visit_module(&engine.context().module);
    collector.sites
}

struct TypestateRequestCollector<'a> {
    node_types: &'a HashMap<NodeId, Type>,
    current_typestate: Option<String>,
    sites: Vec<RequestSiteShape>,
}

impl Visitor<DefId, ()> for TypestateRequestCollector<'_> {
    fn visit_method_block(&mut self, method_block: &crate::core::tree::resolved::MethodBlock) {
        let prev = self.current_typestate.clone();
        self.current_typestate =
            parse_typestate_and_state_from_generated_state(&method_block.type_name)
                .map(|(typestate_name, _)| typestate_name);
        visit::walk_method_block(self, method_block);
        self.current_typestate = prev;
    }

    fn visit_expr(&mut self, expr: &crate::core::tree::resolved::Expr) {
        if let Some(typestate_name) = &self.current_typestate
            && let ExprKind::Emit {
                kind:
                    crate::core::tree::EmitKind::Request {
                        payload,
                        request_site_label,
                        ..
                    },
            } = &expr.kind
            && let Some(request_ty) = self.node_types.get(&payload.id)
            && let Some(Type::Pending { response_tys }) = self.node_types.get(&expr.id)
        {
            self.sites.push(RequestSiteShape {
                typestate_name: typestate_name.clone(),
                request_ty: request_ty.clone(),
                response_tys: response_tys.clone(),
                request_site_label: request_site_label.clone(),
                span: expr.span,
            });
        }
        visit::walk_expr(self, expr);
    }
}

fn collect_provenance_handler_shapes(engine: &TypecheckEngine) -> Vec<ProvenanceHandlerShape> {
    let mut out = Vec::new();
    for method_block in engine.context().module.method_blocks() {
        let Some((typestate_name, _state_name)) =
            parse_typestate_and_state_from_generated_state(&method_block.type_name)
        else {
            continue;
        };
        for method_item in &method_block.method_items {
            let MethodItem::Def(method_def) = method_item else {
                continue;
            };
            if !method_def.sig.name.starts_with("__ts_on_") {
                continue;
            }
            // `for RequestType(binding)` handlers lower to:
            //   (__event, __pending: Pending<Selector>, request_binding, ...)
            if method_def.sig.params.len() < 3 || method_def.sig.params[1].ident != "__pending" {
                continue;
            }
            let Ok(pending_ty) = resolve_type_expr(
                &engine.context().def_table,
                &engine.context().module,
                &method_def.sig.params[1].typ,
            ) else {
                continue;
            };
            let Type::Pending { .. } = pending_ty else {
                continue;
            };
            let Ok(response_ty) = resolve_type_expr(
                &engine.context().def_table,
                &engine.context().module,
                &method_def.sig.params[0].typ,
            ) else {
                continue;
            };
            let Ok(request_ty) = resolve_type_expr(
                &engine.context().def_table,
                &engine.context().module,
                &method_def.sig.params[2].typ,
            ) else {
                continue;
            };

            out.push(ProvenanceHandlerShape {
                typestate_name: typestate_name.clone(),
                request_ty,
                response_ty,
                request_site_label: parse_generated_handler_site_label(&method_def.sig.name)
                    .map(ToString::to_string),
                span: method_def.sig.span,
            });
        }
    }
    out
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct TypestateStateKey {
    typestate_name: String,
    state_name: String,
}

fn collect_typestate_handler_payloads_by_state(
    engine: &TypecheckEngine,
    typestate_names: &HashSet<String>,
) -> HashMap<TypestateStateKey, HashSet<Type>> {
    let mut out = HashMap::<TypestateStateKey, HashSet<Type>>::new();
    for method_block in engine.context().module.method_blocks() {
        let Some((typestate_name, state_name)) =
            parse_typestate_and_state_from_generated_state(&method_block.type_name)
        else {
            continue;
        };
        if !typestate_names.contains(&typestate_name) {
            continue;
        }
        let key = TypestateStateKey {
            typestate_name,
            state_name,
        };
        for method_item in &method_block.method_items {
            let MethodItem::Def(method_def) = method_item else {
                continue;
            };
            if !method_def.sig.name.starts_with("__ts_on_") {
                continue;
            }
            let Some(event_param) = method_def.sig.params.first() else {
                continue;
            };
            let Ok(handler_ty) = resolve_type_expr(
                &engine.context().def_table,
                &engine.context().module,
                &event_param.typ,
            ) else {
                continue;
            };
            out.entry(key.clone()).or_default().insert(handler_ty);
        }
    }
    out
}

fn collect_typestate_handler_payloads(
    payloads_by_state: &HashMap<TypestateStateKey, HashSet<Type>>,
) -> HashMap<String, HashSet<Type>> {
    let mut out = HashMap::<String, HashSet<Type>>::new();
    for (key, payloads) in payloads_by_state {
        out.entry(key.typestate_name.clone())
            .or_default()
            .extend(payloads.iter().cloned());
    }
    out
}

#[derive(Clone, Debug)]
struct EmitPayload {
    payload_ty: Type,
    to_field_name: Option<String>,
    is_request: bool,
    request_response_tys: Option<Vec<Type>>,
    span: crate::core::diag::Span,
}

fn collect_typestate_outgoing_payloads_by_state(
    engine: &TypecheckEngine,
    typestate_names: &HashSet<String>,
) -> HashMap<TypestateStateKey, Vec<EmitPayload>> {
    let mut collector = TypestateEmitCollector {
        typestate_names,
        node_types: &engine.state().solve.resolved_node_types,
        current_state: None,
        emits_by_state: HashMap::new(),
    };
    collector.visit_module(&engine.context().module);
    collector.emits_by_state
}

fn collect_typestate_outgoing_payloads(
    outgoing_by_state: &HashMap<TypestateStateKey, Vec<EmitPayload>>,
) -> HashMap<String, Vec<EmitPayload>> {
    let mut out = HashMap::<String, Vec<EmitPayload>>::new();
    for (key, payloads) in outgoing_by_state {
        out.entry(key.typestate_name.clone())
            .or_default()
            .extend(payloads.iter().cloned());
    }
    out
}

struct TypestateEmitCollector<'a> {
    typestate_names: &'a HashSet<String>,
    node_types: &'a HashMap<NodeId, Type>,
    current_state: Option<TypestateStateKey>,
    emits_by_state: HashMap<TypestateStateKey, Vec<EmitPayload>>,
}

impl Visitor<DefId, ()> for TypestateEmitCollector<'_> {
    fn visit_method_block(&mut self, method_block: &crate::core::tree::resolved::MethodBlock) {
        let prev = self.current_state.clone();
        self.current_state = parse_typestate_and_state_from_generated_state(
            &method_block.type_name,
        )
        .and_then(|(typestate_name, state_name)| {
            if self.typestate_names.contains(&typestate_name) {
                Some(TypestateStateKey {
                    typestate_name,
                    state_name,
                })
            } else {
                None
            }
        });
        visit::walk_method_block(self, method_block);
        self.current_state = prev;
    }

    fn visit_expr(&mut self, expr: &crate::core::tree::resolved::Expr) {
        if let Some(state_key) = &self.current_state
            && let ExprKind::Emit { kind } = &expr.kind
        {
            let (payload, to, is_request) = match kind {
                crate::core::tree::EmitKind::Send { to, payload } => (payload, to, false),
                crate::core::tree::EmitKind::Request { to, payload, .. } => (payload, to, true),
            };
            if let Some(payload_ty) = self.node_types.get(&payload.id) {
                let request_response_tys = if is_request {
                    match self.node_types.get(&expr.id) {
                        Some(Type::Pending { response_tys }) => Some(response_tys.clone()),
                        _ => None,
                    }
                } else {
                    None
                };
                self.emits_by_state
                    .entry(state_key.clone())
                    .or_default()
                    .push(EmitPayload {
                        payload_ty: payload_ty.clone(),
                        to_field_name: emit_destination_field_name(to),
                        is_request,
                        request_response_tys,
                        span: payload.span,
                    });
            }
        }
        visit::walk_expr(self, expr);
    }
}

fn check_reply_cap_usage(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
    let mut errors = Vec::new();
    let node_types = &engine.state().solve.resolved_node_types;

    let mut outside_collector = ReplyOutsideHandlerCollector::default();
    outside_collector.visit_module(&engine.context().module);
    errors.extend(outside_collector.errors);

    for method_block in engine.context().module.method_blocks() {
        if !method_block.type_name.starts_with("__ts_") {
            continue;
        }
        for method_item in &method_block.method_items {
            let MethodItem::Def(method_def) = method_item else {
                continue;
            };
            if !method_def.sig.name.starts_with("__ts_on_") {
                continue;
            }
            let cap_params = collect_handler_reply_caps(
                &engine.context().def_table,
                &engine.context().module,
                method_def,
            );
            errors.extend(check_handler_reply_calls(
                method_def,
                &cap_params,
                node_types,
            ));
            errors.extend(check_handler_reply_cap_linearity(method_def, &cap_params));
        }
    }

    errors
}

#[derive(Default)]
struct ReplyOutsideHandlerCollector {
    errors: Vec<TypeCheckError>,
    in_typestate_method_block: bool,
    in_typestate_handler: bool,
}

impl Visitor<DefId, ()> for ReplyOutsideHandlerCollector {
    fn visit_method_block(&mut self, method_block: &crate::core::tree::resolved::MethodBlock) {
        let prev = self.in_typestate_method_block;
        self.in_typestate_method_block = method_block.type_name.starts_with("__ts_");
        visit::walk_method_block(self, method_block);
        self.in_typestate_method_block = prev;
    }

    fn visit_method_def(&mut self, method_def: &crate::core::tree::resolved::MethodDef) {
        let prev = self.in_typestate_handler;
        self.in_typestate_handler =
            self.in_typestate_method_block && method_def.sig.name.starts_with("__ts_on_");
        visit::walk_method_def(self, method_def);
        self.in_typestate_handler = prev;
    }

    fn visit_expr(&mut self, expr: &crate::core::tree::resolved::Expr) {
        if matches!(expr.kind, ExprKind::Reply { .. }) && !self.in_typestate_handler {
            self.errors
                .push(TypeCheckErrorKind::ReplyOutsideHandler(expr.span).into());
        }
        visit::walk_expr(self, expr);
    }
}

#[derive(Clone, Debug)]
struct ReplyCapParam {
    def_id: DefId,
    name: String,
    response_tys: Vec<Type>,
    span: crate::core::diag::Span,
}

fn collect_handler_reply_caps(
    def_table: &crate::core::resolve::DefTable,
    module: &crate::core::tree::resolved::Module,
    method_def: &crate::core::tree::resolved::MethodDef,
) -> Vec<ReplyCapParam> {
    let mut caps = Vec::new();
    for param in &method_def.sig.params {
        let Ok(param_ty) = resolve_type_expr(def_table, module, &param.typ) else {
            continue;
        };
        if let Type::ReplyCap { response_tys } = param_ty {
            caps.push(ReplyCapParam {
                def_id: param.def_id,
                name: param.ident.clone(),
                response_tys,
                span: param.span,
            });
        }
    }
    caps
}

#[derive(Clone, Debug)]
struct ReplySite {
    span: crate::core::diag::Span,
    cap_node: NodeId,
    cap_span: crate::core::diag::Span,
    cap_def_id: Option<DefId>,
    value_node: NodeId,
}

fn check_handler_reply_calls(
    method_def: &crate::core::tree::resolved::MethodDef,
    cap_params: &[ReplyCapParam],
    node_types: &HashMap<NodeId, Type>,
) -> Vec<TypeCheckError> {
    let mut errors = Vec::new();
    let cap_params_by_id: HashMap<DefId, &ReplyCapParam> =
        cap_params.iter().map(|cap| (cap.def_id, cap)).collect();
    let mut sites = Vec::new();
    collect_reply_sites_from_expr(&method_def.body, &mut sites);

    for site in sites {
        let Some(cap_ty) = node_types.get(&site.cap_node) else {
            continue;
        };
        let Type::ReplyCap { .. } = cap_ty else {
            errors.push(TypeCheckErrorKind::ReplyCapExpected(cap_ty.clone(), site.cap_span).into());
            continue;
        };

        let Some(cap_def_id) = site.cap_def_id else {
            errors.push(TypeCheckErrorKind::ReplyCapParamRequired(site.cap_span).into());
            continue;
        };
        let Some(cap_param) = cap_params_by_id.get(&cap_def_id).copied() else {
            errors.push(TypeCheckErrorKind::ReplyCapParamRequired(site.cap_span).into());
            continue;
        };

        let Some(value_ty) = node_types.get(&site.value_node) else {
            continue;
        };
        let allowed = cap_param
            .response_tys
            .iter()
            .any(|expected| type_assignable(value_ty, expected) != TypeAssignability::Incompatible);
        if !allowed {
            errors.push(
                TypeCheckErrorKind::ReplyPayloadNotAllowed(
                    value_ty.clone(),
                    cap_param.response_tys.clone(),
                    site.span,
                )
                .into(),
            );
        }
    }

    errors
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ReplyCapFlowState {
    Available,
    Consumed,
    MaybeConsumed,
    InvalidDoubleConsume,
}

fn check_handler_reply_cap_linearity(
    method_def: &crate::core::tree::resolved::MethodDef,
    cap_params: &[ReplyCapParam],
) -> Vec<TypeCheckError> {
    if cap_params.is_empty() {
        return Vec::new();
    }

    let cfg = TreeCfgBuilder::new().build_from_expr(&method_def.body);
    if cfg.num_nodes() == 0 {
        return Vec::new();
    }

    let reachable = reachable_cfg_nodes(&cfg, AstBlockId(0));
    let mut errors = Vec::new();
    for cap in cap_params {
        let consumes_by_node: Vec<Vec<crate::core::diag::Span>> = cfg
            .nodes
            .iter()
            .map(|node| collect_reply_consume_spans_for_cap(node, cap.def_id))
            .collect();

        let dataflow = solve_forward(
            &cfg,
            AstBlockId(0),
            ReplyCapFlowState::Available,
            ReplyCapFlowState::Available,
            meet_reply_cap_state,
            |node, in_state| {
                let mut state = *in_state;
                for _ in 0..consumes_by_node[node.0].len() {
                    state = apply_reply_cap_consume(state);
                }
                state
            },
        );

        for (idx, spans) in consumes_by_node.iter().enumerate() {
            if !reachable[idx] {
                continue;
            }
            let mut state = dataflow.in_map[idx];
            for span in spans {
                if matches!(
                    state,
                    ReplyCapFlowState::Consumed
                        | ReplyCapFlowState::MaybeConsumed
                        | ReplyCapFlowState::InvalidDoubleConsume
                ) {
                    errors.push(
                        TypeCheckErrorKind::ReplyCapConsumedMultipleTimes(cap.name.clone(), *span)
                            .into(),
                    );
                    state = ReplyCapFlowState::InvalidDoubleConsume;
                } else {
                    state = apply_reply_cap_consume(state);
                }
            }
        }

        let mut missing_on_some_path = false;
        for idx in 0..cfg.num_nodes() {
            if !reachable[idx] {
                continue;
            }
            let node = AstBlockId(idx);
            if !cfg.succs(node).is_empty() {
                continue;
            }
            if !matches!(
                dataflow.out_map[idx],
                ReplyCapFlowState::Consumed | ReplyCapFlowState::InvalidDoubleConsume
            ) {
                missing_on_some_path = true;
                break;
            }
        }
        if missing_on_some_path {
            errors.push(
                TypeCheckErrorKind::ReplyCapMustBeConsumed(cap.name.clone(), cap.span).into(),
            );
        }
    }

    errors
}

fn collect_reply_consume_spans_for_cap(
    node: &TreeCfgNode<'_>,
    cap_def_id: DefId,
) -> Vec<crate::core::diag::Span> {
    let mut out = Vec::new();
    for item in &node.items {
        let mut sites = Vec::new();
        match item {
            TreeCfgItem::Stmt(stmt) => collect_reply_sites_from_stmt(stmt, &mut sites),
            TreeCfgItem::Expr(expr) => collect_reply_sites_from_expr(expr, &mut sites),
        }
        out.extend(
            sites
                .into_iter()
                .filter(|site| site.cap_def_id == Some(cap_def_id))
                .map(|site| site.span),
        );
    }
    out
}

fn meet_reply_cap_state(states: &[ReplyCapFlowState]) -> ReplyCapFlowState {
    let mut has_available = false;
    let mut has_consumed = false;
    let mut has_maybe = false;
    for state in states {
        match state {
            ReplyCapFlowState::InvalidDoubleConsume => {
                return ReplyCapFlowState::InvalidDoubleConsume;
            }
            ReplyCapFlowState::Available => has_available = true,
            ReplyCapFlowState::Consumed => has_consumed = true,
            ReplyCapFlowState::MaybeConsumed => has_maybe = true,
        }
    }
    if has_maybe || (has_available && has_consumed) {
        ReplyCapFlowState::MaybeConsumed
    } else if has_consumed {
        ReplyCapFlowState::Consumed
    } else {
        ReplyCapFlowState::Available
    }
}

fn apply_reply_cap_consume(state: ReplyCapFlowState) -> ReplyCapFlowState {
    match state {
        ReplyCapFlowState::Available => ReplyCapFlowState::Consumed,
        ReplyCapFlowState::Consumed
        | ReplyCapFlowState::MaybeConsumed
        | ReplyCapFlowState::InvalidDoubleConsume => ReplyCapFlowState::InvalidDoubleConsume,
    }
}

fn reachable_cfg_nodes(cfg: &crate::core::tree::cfg::TreeCfg<'_>, entry: AstBlockId) -> Vec<bool> {
    let mut reachable = vec![false; cfg.num_nodes()];
    if cfg.num_nodes() == 0 {
        return reachable;
    }

    let mut stack = vec![entry];
    while let Some(node) = stack.pop() {
        if reachable[node.0] {
            continue;
        }
        reachable[node.0] = true;
        stack.extend(cfg.succs(node).iter().copied());
    }
    reachable
}

fn collect_reply_sites_from_stmt(
    stmt: &crate::core::tree::resolved::StmtExpr,
    out: &mut Vec<ReplySite>,
) {
    let mut collector = ReplySiteCollector { out };
    collector.visit_stmt_expr(stmt);
}

fn collect_reply_sites_from_expr(
    expr: &crate::core::tree::resolved::Expr,
    out: &mut Vec<ReplySite>,
) {
    let mut collector = ReplySiteCollector { out };
    collector.visit_expr(expr);
}

struct ReplySiteCollector<'a> {
    out: &'a mut Vec<ReplySite>,
}

impl Visitor<DefId, ()> for ReplySiteCollector<'_> {
    fn visit_expr(&mut self, expr: &crate::core::tree::resolved::Expr) {
        if let ExprKind::Reply { cap, value } = &expr.kind {
            let cap_def_id = match &cap.kind {
                ExprKind::Var { def_id, .. } => Some(*def_id),
                _ => None,
            };
            self.out.push(ReplySite {
                span: expr.span,
                cap_node: cap.id,
                cap_span: cap.span,
                cap_def_id,
                value_node: value.id,
            });
        }
        visit::walk_expr(self, expr);
    }
}

fn parse_typestate_and_state_from_generated_state(type_name: &str) -> Option<(String, String)> {
    let rest = type_name.strip_prefix("__ts_")?;
    let (typestate_name, state_name) = rest.rsplit_once('_')?;
    Some((typestate_name.to_string(), state_name.to_string()))
}

fn emit_destination_field_name(expr: &crate::core::tree::resolved::Expr) -> Option<String> {
    let ExprKind::StructField { target, field } = &expr.kind else {
        return None;
    };
    let ExprKind::Var { ident, .. } = &target.kind else {
        return None;
    };
    if ident == "self" {
        Some(field.clone())
    } else {
        None
    }
}

fn state_expected_roles_for_payload(
    state_fact: &crate::core::protocol::ProtocolStateFact,
    payload_ty: &Type,
) -> Vec<String> {
    let mut roles = Vec::new();
    for transition in &state_fact.transitions {
        for effect in &transition.effects {
            if effect.payload_ty.as_ref() == Some(payload_ty) && !roles.contains(&effect.to_role) {
                roles.push(effect.to_role.clone());
            }
        }
    }
    roles.sort();
    roles
}

fn request_contract_to_roles_for_payload(
    protocol_fact: &crate::core::protocol::ProtocolFact,
    from_role: &str,
    request_ty: &Type,
) -> Vec<String> {
    let mut roles = Vec::new();
    for contract in &protocol_fact.request_contracts {
        if contract.from_role == from_role
            && contract.request_ty.as_ref() == Some(request_ty)
            && !roles.contains(&contract.to_role)
        {
            roles.push(contract.to_role.clone());
        }
    }
    roles.sort();
    roles
}

fn matching_request_contracts<'a>(
    protocol_fact: &'a crate::core::protocol::ProtocolFact,
    from_role: &str,
    to_role: &str,
    request_ty: &Type,
) -> Vec<&'a crate::core::protocol::ProtocolRequestContractFact> {
    protocol_fact
        .request_contracts
        .iter()
        .filter(|contract| {
            contract.from_role == from_role
                && contract.to_role == to_role
                && contract.request_ty.as_ref() == Some(request_ty)
        })
        .collect()
}

fn resolve_emit_destination_role<'a>(
    emit: &'a EmitPayload,
    peer_role_by_field: &'a HashMap<&str, &str>,
) -> Option<(&'a str, &'a str)> {
    let field_name = emit.to_field_name.as_deref()?;
    let role_name = peer_role_by_field.get(field_name).copied()?;
    Some((field_name, role_name))
}

#[cfg(test)]
#[path = "../../tests/typecheck/t_validate.rs"]
mod tests;
