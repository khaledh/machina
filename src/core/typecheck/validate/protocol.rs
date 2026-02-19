use std::collections::{HashMap, HashSet};

use crate::core::machine::naming::parse_generated_handler_site_label;
use crate::core::protocol::event_extract::extract_emit_from_expr;
use crate::core::resolve::DefId;
use crate::core::tree::NodeId;
use crate::core::tree::visit::{self, Visitor};
use crate::core::tree::{ExprKind, MethodItem};
use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::core::typecheck::type_map::resolve_type_expr;
use crate::core::types::Type;

pub(super) fn check_protocol_shape_conformance(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
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

    let mut errors = Vec::new();
    for binding in &resolved.typestate_role_impls {
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

        let ctx = EmitCheckCtx {
            typestate_name: &binding.typestate_name,
            role_label: &role_label,
            role_name,
            protocol_fact,
            peer_role_by_field: &peer_role_by_field,
        };

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
                    check_emit_conformance(&ctx, state_fact, emit, &mut errors);
                }
            }
        }
    }

    errors
}

struct EmitCheckCtx<'a> {
    typestate_name: &'a str,
    role_label: &'a str,
    role_name: &'a str,
    protocol_fact: &'a crate::core::protocol::ProtocolFact,
    peer_role_by_field: &'a HashMap<&'a str, &'a str>,
}

fn check_emit_conformance(
    ctx: &EmitCheckCtx<'_>,
    state_fact: &crate::core::protocol::ProtocolStateFact,
    emit: &EmitPayload,
    errors: &mut Vec<TypeCheckError>,
) {
    if !state_fact.shape.allowed_outgoing.contains(&emit.payload_ty) {
        errors.push(
            TypeCheckErrorKind::ProtocolStateOutgoingPayloadNotAllowed(
                ctx.typestate_name.to_string(),
                ctx.role_label.to_string(),
                state_fact.name.clone(),
                emit.payload_ty.clone(),
                emit.span,
            )
            .into(),
        );
        return;
    }

    let mut expected_roles = state_expected_roles_for_payload(state_fact, &emit.payload_ty);
    if emit.is_request {
        expected_roles.extend(request_contract_to_roles_for_payload(
            ctx.protocol_fact,
            ctx.role_name,
            &emit.payload_ty,
        ));
        expected_roles.sort();
        expected_roles.dedup();
    }
    if expected_roles.is_empty() || emit.destination_implicit {
        return;
    }

    let Some((field_name, bound_role_name)) =
        resolve_emit_destination_role(emit, ctx.peer_role_by_field)
    else {
        errors.push(
            TypeCheckErrorKind::ProtocolStateEmitDestinationRoleUnbound(
                ctx.typestate_name.to_string(),
                ctx.role_label.to_string(),
                state_fact.name.clone(),
                emit.payload_ty.clone(),
                expected_roles.join(" | "),
                emit.span,
            )
            .into(),
        );
        return;
    };
    if !expected_roles.iter().any(|role| role == bound_role_name) {
        errors.push(
            TypeCheckErrorKind::ProtocolStateEmitDestinationRoleMismatch(
                ctx.typestate_name.to_string(),
                ctx.role_label.to_string(),
                state_fact.name.clone(),
                emit.payload_ty.clone(),
                expected_roles.join(" | "),
                field_name.to_string(),
                bound_role_name.to_string(),
                emit.span,
            )
            .into(),
        );
        return;
    }

    if emit.is_request {
        check_request_contract(ctx, state_fact, emit, bound_role_name, errors);
    }
}

fn check_request_contract(
    ctx: &EmitCheckCtx<'_>,
    _state_fact: &crate::core::protocol::ProtocolStateFact,
    emit: &EmitPayload,
    to_role_name: &str,
    errors: &mut Vec<TypeCheckError>,
) {
    let matching_contracts = matching_request_contracts(
        ctx.protocol_fact,
        ctx.role_name,
        to_role_name,
        &emit.payload_ty,
    );
    if matching_contracts.is_empty() {
        errors.push(
            TypeCheckErrorKind::ProtocolRequestContractMissing(
                ctx.typestate_name.to_string(),
                ctx.role_label.to_string(),
                emit.payload_ty.clone(),
                to_role_name.to_string(),
                emit.span,
            )
            .into(),
        );
        return;
    }
    if matching_contracts.len() > 1 {
        errors.push(
            TypeCheckErrorKind::ProtocolRequestContractAmbiguous(
                ctx.typestate_name.to_string(),
                ctx.role_label.to_string(),
                emit.payload_ty.clone(),
                to_role_name.to_string(),
                emit.span,
            )
            .into(),
        );
        return;
    }
    let contract_responses = &matching_contracts[0].response_tys;
    let response_tys = emit.request_response_tys.as_deref().unwrap_or_default();
    for response_ty in response_tys {
        if !contract_responses.contains(response_ty) {
            errors.push(
                TypeCheckErrorKind::ProtocolRequestResponseNotInContract(
                    ctx.typestate_name.to_string(),
                    ctx.role_label.to_string(),
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

#[derive(Clone, Debug)]
struct HandlerResponsePattern {
    selector_ty: Type,
    response_tys: Vec<Type>,
    request_site_label: Option<String>,
    span: crate::core::diag::Span,
}

pub(super) fn check_typestate_handler_overlap(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
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

pub(super) fn check_typestate_request_response_shape(
    engine: &TypecheckEngine,
) -> Vec<TypeCheckError> {
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

#[derive(Clone, Debug)]
struct EmitPayload {
    payload_ty: Type,
    to_field_name: Option<String>,
    // Capability-based replies route implicitly to the origin role and do not
    // expose a concrete `self.<peer>` destination field.
    destination_implicit: bool,
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
            && let Some(emit) =
                extract_emit_from_expr(expr, |node_id| self.node_types.get(&node_id).cloned())
        {
            self.emits_by_state
                .entry(state_key.clone())
                .or_default()
                .push(EmitPayload {
                    payload_ty: emit.payload_ty,
                    to_field_name: emit.to_field_name,
                    destination_implicit: emit.destination_implicit,
                    is_request: emit.is_request,
                    request_response_tys: if emit.is_request {
                        Some(emit.request_response_tys)
                    } else {
                        None
                    },
                    span: emit.span,
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
