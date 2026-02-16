//! Elaborate-time machine descriptor + dispatch-thunk planning.
//!
//! This module derives managed-runtime plans from typestate-desugared semantic
//! items (`__ts_*` generated state types/methods). The output is purely a
//! semantic side table and does not mutate the semantic tree.

use std::collections::{BTreeMap, HashMap};

use crate::core::analysis::facts::{DefTableOverlay, TypeMapOverlay};
use crate::core::context::TypestateRoleImplBinding;
use crate::core::machine::naming::{
    GENERATED_FINAL_STATE_MARKER, GENERATED_HANDLER_PREFIX, GENERATED_STATE_PREFIX,
    parse_generated_handler_site_label,
};
use crate::core::machine::request_site::labeled_request_site_key;
use crate::core::tree::semantic as sem;
use crate::core::typecheck::type_map::resolve_type_expr;
use crate::core::types::{StructField, Type, TypeId};

/// Build managed-machine plans for all typestates present in the semantic
/// module. Plans are deterministic by typestate/state/event lexical ordering.
pub fn build_machine_plans(
    module: &sem::Module,
    def_table: &DefTableOverlay,
    type_map: &TypeMapOverlay,
    typestate_role_impls: &[TypestateRoleImplBinding],
) -> sem::MachinePlanMap {
    let mut builders = collect_typestate_builders(module, def_table, type_map);
    attach_role_impls(&mut builders, typestate_role_impls);
    materialize_machine_plans(builders)
}

#[derive(Clone, Debug)]
struct HandlerPlanSeed {
    def_id: crate::core::resolve::DefId,
    ordinal: usize,
    state_name: String,
    state_layout_ty: TypeId,
    event_key: sem::MachineEventKeyPlan,
    request_site_key_match: Option<u64>,
    provenance_param_index: Option<usize>,
    payload_layout_ty: TypeId,
    next_state_layout_ty: TypeId,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
struct DispatchKey {
    event_stable_key: String,
    site: Option<u64>, // None = wildcard
}

impl HandlerPlanSeed {
    fn dispatch_key(&self) -> DispatchKey {
        DispatchKey {
            event_stable_key: self.event_key.stable_key(),
            site: self.request_site_key_match,
        }
    }
}

#[derive(Clone, Debug)]
struct StatePlanSeed {
    state_name: String,
    state_type_def_id: crate::core::resolve::DefId,
    state_layout_ty: TypeId,
    // Source `@final` marker propagated from typestate desugaring.
    is_final: bool,
    handlers: Vec<HandlerPlanSeed>,
}

#[derive(Clone, Debug, Default)]
struct TypestatePlanSeed {
    states: BTreeMap<String, StatePlanSeed>,
    role_impls: Vec<sem::MachineRoleImplPlan>,
}

fn collect_typestate_builders(
    module: &sem::Module,
    def_table: &DefTableOverlay,
    type_map: &TypeMapOverlay,
) -> BTreeMap<String, TypestatePlanSeed> {
    let mut out = BTreeMap::<String, TypestatePlanSeed>::new();

    // First pass: collect generated typestate state-type facts.
    for type_def in module.type_defs() {
        let Some((typestate_name, state_name)) = parse_generated_state_name(&type_def.name) else {
            continue;
        };

        let state_layout_ty = resolve_state_layout_type_id(module, def_table, type_map, type_def);

        let typestate = out.entry(typestate_name).or_default();
        typestate.states.insert(
            state_name.clone(),
            StatePlanSeed {
                state_name,
                state_type_def_id: type_def.def_id,
                state_layout_ty,
                is_final: false,
                handlers: Vec::new(),
            },
        );
    }

    // Second pass: collect generated typestate handler facts per state.
    for method_block in module.method_blocks() {
        let Some((typestate_name, state_name)) =
            parse_generated_state_name(&method_block.type_name)
        else {
            continue;
        };
        let Some(typestate) = out.get_mut(&typestate_name) else {
            continue;
        };
        let Some(state_seed) = typestate.states.get_mut(&state_name) else {
            continue;
        };

        for item in &method_block.method_items {
            let sem::MethodItem::Def(method_def) = item else {
                continue;
            };
            // Typestate lowering injects a synthetic marker method in `@final`
            // states so later stages can recover terminal-state facts without
            // carrying source attributes through semantic trees.
            if method_def.sig.name == GENERATED_FINAL_STATE_MARKER {
                state_seed.is_final = true;
                continue;
            }
            let Some(ordinal) = parse_generated_handler_ordinal(&method_def.sig.name) else {
                continue;
            };
            let Some(event_param) = method_def.sig.params.first() else {
                continue;
            };

            let selector_ty =
                resolve_type_expr(def_table, module, &event_param.typ).unwrap_or_else(|err| {
                    panic!(
                        "compiler bug: cannot resolve typestate handler selector type for {}: {err}",
                        method_def.sig.name
                    )
                });
            let selector_ty_id = resolve_type_id(
                module,
                def_table,
                type_map,
                &event_param.typ,
                &method_def.sig.name,
                "selector",
            );

            let (event_key, payload_layout_ty) = resolve_handler_event_key(
                module,
                def_table,
                type_map,
                method_def,
                selector_ty.clone(),
            );
            let provenance_param_index =
                resolve_handler_provenance_param_index(method_def, &selector_ty);
            let request_site_key_match = parse_generated_handler_site_label(&method_def.sig.name)
                .map(labeled_request_site_key);

            let next_state_layout_ty = resolve_type_id(
                module,
                def_table,
                type_map,
                &method_def.sig.ret_ty_expr,
                &method_def.sig.name,
                "return",
            );

            state_seed.handlers.push(HandlerPlanSeed {
                def_id: method_def.def_id,
                ordinal,
                state_name: state_seed.state_name.clone(),
                state_layout_ty: state_seed.state_layout_ty,
                event_key,
                request_site_key_match,
                provenance_param_index,
                // Selector payload layout is used when no response override applies.
                payload_layout_ty: payload_layout_ty.unwrap_or(selector_ty_id),
                next_state_layout_ty,
            });
        }
    }

    // Keep handler order stable/deterministic per state.
    for typestate in out.values_mut() {
        for state in typestate.states.values_mut() {
            state.handlers.sort_by_key(|handler| handler.ordinal);
        }
    }

    out
}

fn resolve_state_layout_type_id(
    module: &sem::Module,
    def_table: &DefTableOverlay,
    type_map: &TypeMapOverlay,
    type_def: &sem::TypeDef,
) -> TypeId {
    if let Some(def) = def_table.lookup_def(type_def.def_id)
        && let Some(id) = type_map.lookup_def_type_id(def)
        && !type_map.type_table().get(id).contains_unresolved()
    {
        return id;
    }

    let sem::TypeDefKind::Struct { fields } = &type_def.kind else {
        panic!(
            "compiler bug: expected generated typestate state to be struct, found {}",
            type_def.name
        );
    };
    let struct_fields = fields
        .iter()
        .map(|field| StructField {
            name: field.name.clone(),
            ty: resolve_type_expr(def_table, module, &field.ty).unwrap_or_else(|err| {
                panic!(
                    "compiler bug: cannot resolve generated state field type {}.{}: {err}",
                    type_def.name, field.name
                )
            }),
        })
        .collect::<Vec<_>>();
    let state_ty = Type::Struct {
        name: type_def.name.clone(),
        fields: struct_fields,
    };
    type_map
        .type_table()
        .lookup_id(&state_ty)
        .unwrap_or_else(|| {
            panic!(
                "compiler bug: missing interned generated state type id for {}",
                type_def.name
            )
        })
}

fn resolve_handler_event_key(
    module: &sem::Module,
    def_table: &DefTableOverlay,
    type_map: &TypeMapOverlay,
    method_def: &sem::MethodDef,
    selector_ty: Type,
) -> (sem::MachineEventKeyPlan, Option<TypeId>) {
    // Response forms include a hidden/explicit `Pending<...>` parameter.
    // We always keep selector metadata, and choose payload event key from the
    // concrete response type:
    // - legacy pattern form: `on Response(pending, Variant)` => param[2]
    // - `for RequestType(binding)` form: selector itself is response payload.
    if method_def.sig.params.len() >= 3 {
        let pending_param = &method_def.sig.params[1];
        let pending_ty = resolve_type_expr(def_table, module, &pending_param.typ).ok();
        if matches!(pending_ty, Some(Type::Pending { .. })) {
            let (response_ty, response_ty_id) = if is_response_selector_type(&selector_ty) {
                let response_param = &method_def.sig.params[2];
                let response_ty = resolve_type_expr(def_table, module, &response_param.typ)
                    .unwrap_or_else(|err| {
                        panic!(
                            "compiler bug: cannot resolve typestate response payload type in {}: {err}",
                            method_def.sig.name
                        )
                    });
                let response_ty_id = resolve_type_id(
                    module,
                    def_table,
                    type_map,
                    &response_param.typ,
                    &method_def.sig.name,
                    "response payload",
                );
                (response_ty, response_ty_id)
            } else {
                let event_param = &method_def.sig.params[0];
                let response_ty = selector_ty.clone();
                let response_ty_id = resolve_type_id(
                    module,
                    def_table,
                    type_map,
                    &event_param.typ,
                    &method_def.sig.name,
                    "response payload",
                );
                (response_ty, response_ty_id)
            };
            return (
                sem::MachineEventKeyPlan::Response {
                    selector_ty,
                    response_ty,
                },
                Some(response_ty_id),
            );
        }
    }

    (
        sem::MachineEventKeyPlan::Payload {
            payload_ty: selector_ty,
        },
        None,
    )
}

fn is_response_selector_type(selector_ty: &Type) -> bool {
    match selector_ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name == "Response",
        _ => false,
    }
}

fn resolve_handler_provenance_param_index(
    method_def: &sem::MethodDef,
    selector_ty: &Type,
) -> Option<usize> {
    // `for RequestType(binding)` lowering injects hidden `__pending` as param[1]
    // and places provenance binding at param[2].
    if method_def.sig.params.len() >= 3
        && method_def.sig.params[1].ident == "__pending"
        && !is_response_selector_type(selector_ty)
    {
        Some(2)
    } else {
        None
    }
}

fn resolve_type_id(
    module: &sem::Module,
    def_table: &DefTableOverlay,
    type_map: &TypeMapOverlay,
    ty_expr: &sem::TypeExpr,
    handler_name: &str,
    label: &str,
) -> TypeId {
    if let Some(id) = type_map.lookup_node_type_id(ty_expr.id) {
        let ty = type_map.type_table().get(id);
        if !ty.contains_unresolved() {
            return id;
        }
    }
    let ty = resolve_type_expr(def_table, module, ty_expr).unwrap_or_else(|err| {
        panic!(
            "compiler bug: cannot resolve {label} type for typestate handler {handler_name}: {err}"
        )
    });
    type_map.type_table().lookup_id(&ty).unwrap_or_else(|| {
        panic!(
            "compiler bug: missing interned {label} type id for typestate handler {handler_name}"
        )
    })
}

fn attach_role_impls(
    builders: &mut BTreeMap<String, TypestatePlanSeed>,
    typestate_role_impls: &[TypestateRoleImplBinding],
) {
    for binding in typestate_role_impls {
        let typestate = builders.entry(binding.typestate_name.clone()).or_default();
        typestate.role_impls.push(sem::MachineRoleImplPlan {
            path: binding.path.clone(),
            role_def_id: binding.role_def_id,
            span: binding.span,
        });
    }
}

fn materialize_machine_plans(builders: BTreeMap<String, TypestatePlanSeed>) -> sem::MachinePlanMap {
    let mut descriptors = HashMap::new();
    let mut thunks = HashMap::new();

    for (typestate_name, typestate_seed) in builders {
        if typestate_seed.states.is_empty() {
            continue;
        }

        let state_rows: Vec<&StatePlanSeed> = typestate_seed.states.values().collect();
        let mut state_tags = Vec::with_capacity(state_rows.len());
        let mut state_tag_by_name = HashMap::<String, u64>::new();
        let mut state_tag_by_layout = HashMap::<TypeId, u64>::new();
        let mut state_is_final_by_layout = HashMap::<TypeId, bool>::new();
        for (idx, state) in state_rows.iter().enumerate() {
            let tag = (idx + 1) as u64;
            state_tag_by_name.insert(state.state_name.clone(), tag);
            state_tag_by_layout.insert(state.state_layout_ty, tag);
            state_is_final_by_layout.insert(state.state_layout_ty, state.is_final);
            state_tags.push(sem::MachineStateTagPlan {
                state_name: state.state_name.clone(),
                state_type_def_id: state.state_type_def_id,
                state_layout_ty: state.state_layout_ty,
                tag,
            });
        }

        let fallback_prefix_len = shared_fallback_prefix_len(&state_rows);
        let fallback_map = build_fallback_map(&state_rows, fallback_prefix_len);
        let event_defs = collect_event_defs(&state_rows);
        let event_kinds = assign_event_kinds(&event_defs);
        let dispatch_table = build_dispatch_table(
            &state_rows,
            &state_tag_by_name,
            &event_kinds,
            &fallback_map,
            fallback_prefix_len,
        );

        // Emit one thunk symbol plan per concrete handler def referenced by the
        // descriptor. Backend lowering can materialize ABI shims from this.
        for state in &state_rows {
            for handler in &state.handlers {
                thunks
                    .entry(handler.def_id)
                    .or_insert_with(|| sem::MachineDispatchThunkPlan {
                        handler_def_id: handler.def_id,
                        symbol: format!("__mc_machine_dispatch_thunk_{}", handler.def_id.0),
                        typestate_name: typestate_name.clone(),
                        state_name: handler.state_name.clone(),
                        event_key: handler.event_key.clone(),
                        provenance_param_index: handler.provenance_param_index,
                        next_state_tag: *state_tag_by_layout.get(&handler.next_state_layout_ty).unwrap_or_else(|| {
                            panic!(
                                "compiler bug: missing state tag for typestate {} handler {} next-state layout {:?}",
                                typestate_name, handler.def_id.0, handler.next_state_layout_ty
                            )
                        }),
                        transitions_to_final: *state_is_final_by_layout
                            .get(&handler.next_state_layout_ty)
                            .unwrap_or_else(|| {
                                panic!(
                                    "compiler bug: missing final-state marker for typestate {} handler {} next-state layout {:?}",
                                    typestate_name, handler.def_id.0, handler.next_state_layout_ty
                                )
                            }),
                        state_layout_ty: handler.state_layout_ty,
                        payload_layout_ty: handler.payload_layout_ty,
                        next_state_layout_ty: handler.next_state_layout_ty,
                    });
            }
        }

        descriptors.insert(
            typestate_name.clone(),
            sem::MachineDescriptorPlan {
                typestate_name,
                state_tags,
                event_kinds,
                dispatch_table,
                role_impls: typestate_seed.role_impls,
            },
        );
    }

    sem::MachinePlanMap {
        descriptors,
        thunks,
    }
}

fn shared_fallback_prefix_len(states: &[&StatePlanSeed]) -> usize {
    let Some(min_len) = states.iter().map(|state| state.handlers.len()).min() else {
        return 0;
    };
    if states.is_empty() {
        return 0;
    }
    for idx in 0..min_len {
        let Some(first) = states.first().and_then(|state| state.handlers.get(idx)) else {
            return idx;
        };
        // Only treat a prefix as typestate-level fallback when both the event
        // key and the concrete handler identity are identical across states.
        // Matching on key alone can incorrectly route state-specific handlers
        // through one state's implementation.
        let key = first.dispatch_key();
        let def_id = first.def_id;
        if states.iter().skip(1).any(|state| {
            state
                .handlers
                .get(idx)
                .map(|h| (h.dispatch_key(), h.def_id))
                != Some((key.clone(), def_id))
        }) {
            return idx;
        }
    }
    min_len
}

fn build_fallback_map(
    states: &[&StatePlanSeed],
    prefix_len: usize,
) -> HashMap<DispatchKey, crate::core::resolve::DefId> {
    let mut out = HashMap::new();
    let Some(first) = states.first() else {
        return out;
    };
    for handler in first.handlers.iter().take(prefix_len) {
        out.insert(handler.dispatch_key(), handler.def_id);
    }
    out
}

#[derive(Clone, Debug)]
struct EventDef {
    key: sem::MachineEventKeyPlan,
    payload_layout_ty: TypeId,
}

fn collect_event_defs(states: &[&StatePlanSeed]) -> BTreeMap<String, EventDef> {
    let mut out = BTreeMap::<String, EventDef>::new();
    for state in states {
        for handler in &state.handlers {
            // Event-kind assignment is payload-driven. Site-label disambiguation
            // is handled at dispatch-row matching, not by splitting kind space.
            let stable_key = handler.event_key.stable_key();
            out.entry(stable_key).or_insert_with(|| EventDef {
                key: handler.event_key.clone(),
                payload_layout_ty: handler.payload_layout_ty,
            });
        }
    }
    out
}

fn assign_event_kinds(events: &BTreeMap<String, EventDef>) -> Vec<sem::MachineEventKindPlan> {
    events
        .values()
        .enumerate()
        .map(|(idx, event)| sem::MachineEventKindPlan {
            key: event.key.clone(),
            payload_layout_ty: event.payload_layout_ty,
            kind: (idx + 1) as u64,
        })
        .collect()
}

fn build_dispatch_table(
    states: &[&StatePlanSeed],
    state_tag_by_name: &HashMap<String, u64>,
    event_kinds: &[sem::MachineEventKindPlan],
    fallback_map: &HashMap<DispatchKey, crate::core::resolve::DefId>,
    fallback_prefix_len: usize,
) -> Vec<sem::MachineDispatchEntryPlan> {
    let mut rows = Vec::new();

    for state in states {
        let state_tag = *state_tag_by_name.get(&state.state_name).unwrap_or_else(|| {
            panic!(
                "compiler bug: missing state tag mapping for {}",
                state.state_name
            )
        });
        let local_handlers = state.handlers.iter().skip(fallback_prefix_len).fold(
            HashMap::<DispatchKey, crate::core::resolve::DefId>::new(),
            |mut acc, handler| {
                acc.entry(handler.dispatch_key()).or_insert(handler.def_id);
                acc
            },
        );

        for event in event_kinds {
            // Each event kind may have:
            // - one wildcard row (`site: None`), and/or
            // - one or more site-specific rows (`site: Some(key)`).
            //
            // Runtime lookup resolves exact site first, then wildcard.
            let event_stable_key = event.key.stable_key();
            let wildcard_key = DispatchKey {
                event_stable_key: event_stable_key.clone(),
                site: None,
            };
            let mut site_keys: Vec<u64> = local_handlers
                .keys()
                .chain(fallback_map.keys())
                .filter(|dk| dk.event_stable_key == event_stable_key)
                .filter_map(|dk| dk.site)
                .collect();
            site_keys.sort_unstable();
            site_keys.dedup();

            rows.push(sem::MachineDispatchEntryPlan {
                state_tag,
                event_kind: event.kind,
                request_site_key: None,
                state_local_thunk: local_handlers.get(&wildcard_key).copied(),
                typestate_fallback_thunk: fallback_map.get(&wildcard_key).copied(),
            });

            for site_key in site_keys {
                let key = DispatchKey {
                    event_stable_key: event_stable_key.clone(),
                    site: Some(site_key),
                };
                rows.push(sem::MachineDispatchEntryPlan {
                    state_tag,
                    event_kind: event.kind,
                    request_site_key: Some(site_key),
                    state_local_thunk: local_handlers.get(&key).copied(),
                    typestate_fallback_thunk: fallback_map.get(&key).copied(),
                });
            }
        }
    }

    rows
}

fn parse_generated_state_name(type_name: &str) -> Option<(String, String)> {
    let rest = type_name.strip_prefix(GENERATED_STATE_PREFIX)?;
    let (typestate_name, state_name) = rest.rsplit_once('_')?;
    Some((typestate_name.to_string(), state_name.to_string()))
}

fn parse_generated_handler_ordinal(method_name: &str) -> Option<usize> {
    let suffix = method_name.strip_prefix(GENERATED_HANDLER_PREFIX)?;
    let digits_len = suffix
        .bytes()
        .take_while(|byte| byte.is_ascii_digit())
        .count();
    if digits_len == 0 {
        return None;
    }
    suffix[..digits_len].parse::<usize>().ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::resolve::DefId;
    use crate::core::types::TypeCache;

    fn dummy_type_id() -> TypeId {
        let mut cache = TypeCache::new();
        cache.intern(Type::Bool)
    }

    fn named_payload(name: &str) -> Type {
        Type::Struct {
            name: name.to_string(),
            fields: Vec::new(),
        }
    }

    #[test]
    fn fallback_prefix_len_stops_at_first_state_divergence() {
        let make_handler = |ordinal, event| HandlerPlanSeed {
            def_id: DefId(ordinal as u32),
            ordinal,
            state_name: "S".to_string(),
            state_layout_ty: dummy_type_id(),
            event_key: sem::MachineEventKeyPlan::Payload {
                payload_ty: named_payload(event),
            },
            request_site_key_match: None,
            provenance_param_index: None,
            payload_layout_ty: dummy_type_id(),
            next_state_layout_ty: dummy_type_id(),
        };

        let state_a = StatePlanSeed {
            state_name: "A".to_string(),
            state_type_def_id: DefId(1),
            state_layout_ty: dummy_type_id(),
            is_final: false,
            handlers: vec![make_handler(0, "Ping"), make_handler(1, "Pong")],
        };
        let state_b = StatePlanSeed {
            state_name: "B".to_string(),
            state_type_def_id: DefId(2),
            state_layout_ty: dummy_type_id(),
            is_final: false,
            handlers: vec![make_handler(0, "Ping")],
        };

        assert_eq!(shared_fallback_prefix_len(&[&state_a, &state_b]), 1);
    }

    #[test]
    fn dispatch_rows_include_local_and_fallback_channels() {
        let state_tag_by_name = HashMap::from([("A".to_string(), 1), ("B".to_string(), 2)]);
        let event_kinds = vec![
            sem::MachineEventKindPlan {
                key: sem::MachineEventKeyPlan::Payload {
                    payload_ty: named_payload("Ping"),
                },
                payload_layout_ty: dummy_type_id(),
                kind: 1,
            },
            sem::MachineEventKindPlan {
                key: sem::MachineEventKeyPlan::Payload {
                    payload_ty: named_payload("Pong"),
                },
                payload_layout_ty: dummy_type_id(),
                kind: 2,
            },
        ];

        let ping = HandlerPlanSeed {
            def_id: DefId(100),
            ordinal: 0,
            state_name: "A".to_string(),
            state_layout_ty: dummy_type_id(),
            event_key: event_kinds[0].key.clone(),
            request_site_key_match: None,
            provenance_param_index: None,
            payload_layout_ty: dummy_type_id(),
            next_state_layout_ty: dummy_type_id(),
        };
        let pong_local = HandlerPlanSeed {
            def_id: DefId(101),
            ordinal: 1,
            state_name: "A".to_string(),
            state_layout_ty: dummy_type_id(),
            event_key: event_kinds[1].key.clone(),
            request_site_key_match: None,
            provenance_param_index: None,
            payload_layout_ty: dummy_type_id(),
            next_state_layout_ty: dummy_type_id(),
        };

        let state_a = StatePlanSeed {
            state_name: "A".to_string(),
            state_type_def_id: DefId(1),
            state_layout_ty: dummy_type_id(),
            is_final: false,
            handlers: vec![ping.clone(), pong_local],
        };
        let state_b = StatePlanSeed {
            state_name: "B".to_string(),
            state_type_def_id: DefId(2),
            state_layout_ty: dummy_type_id(),
            is_final: false,
            handlers: vec![HandlerPlanSeed {
                state_name: "B".to_string(),
                ..ping
            }],
        };

        let fallback_map = HashMap::from([(
            DispatchKey {
                event_stable_key: event_kinds[0].key.stable_key(),
                site: None,
            },
            DefId(100),
        )]);
        let rows = build_dispatch_table(
            &[&state_a, &state_b],
            &state_tag_by_name,
            &event_kinds,
            &fallback_map,
            1,
        );

        // A/Ping: fallback only; A/Pong: local only.
        assert_eq!(
            rows.iter()
                .find(|row| row.state_tag == 1 && row.event_kind == 1)
                .and_then(|row| row.typestate_fallback_thunk),
            Some(DefId(100))
        );
        assert_eq!(
            rows.iter()
                .find(|row| row.state_tag == 1 && row.event_kind == 2)
                .and_then(|row| row.state_local_thunk),
            Some(DefId(101))
        );
        // B/Pong: no local and no fallback.
        let b_pong = rows
            .iter()
            .find(|row| row.state_tag == 2 && row.event_kind == 2)
            .expect("expected B/Pong row");
        assert!(b_pong.state_local_thunk.is_none());
        assert!(b_pong.typestate_fallback_thunk.is_none());
    }

    #[test]
    fn fallback_prefix_len_requires_shared_handler_identity() {
        let event_key = sem::MachineEventKeyPlan::Payload {
            payload_ty: named_payload("Ping"),
        };
        let state_a = StatePlanSeed {
            state_name: "A".to_string(),
            state_type_def_id: DefId(1),
            state_layout_ty: dummy_type_id(),
            is_final: false,
            handlers: vec![HandlerPlanSeed {
                def_id: DefId(100),
                ordinal: 0,
                state_name: "A".to_string(),
                state_layout_ty: dummy_type_id(),
                event_key: event_key.clone(),
                request_site_key_match: None,
                provenance_param_index: None,
                payload_layout_ty: dummy_type_id(),
                next_state_layout_ty: dummy_type_id(),
            }],
        };
        let state_b = StatePlanSeed {
            state_name: "B".to_string(),
            state_type_def_id: DefId(2),
            state_layout_ty: dummy_type_id(),
            is_final: false,
            handlers: vec![HandlerPlanSeed {
                def_id: DefId(200),
                ordinal: 0,
                state_name: "B".to_string(),
                state_layout_ty: dummy_type_id(),
                event_key,
                request_site_key_match: None,
                provenance_param_index: None,
                payload_layout_ty: dummy_type_id(),
                next_state_layout_ty: dummy_type_id(),
            }],
        };

        assert_eq!(shared_fallback_prefix_len(&[&state_a, &state_b]), 0);
    }
}
