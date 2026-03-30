//! Machine hosting surface: generated types and functions for hosted linear types.
//!
//! When a `machine` declaration hosts a linear type, this module generates:
//!
//! - **Support types**: `MachineError` and `SessionError` enums (created once,
//!   shared across all machines in the module).
//!
//! - **Handle type**: a struct per machine (e.g., `__mc_machine_handle_PRService`)
//!   representing a reference to a running machine instance.
//!
//! - **Spawn function**: a per-machine function that returns `Handle | MachineError`.
//!   `PRService::spawn()` is rewritten to call this.
//!
//! - **Create function**: a per-machine-per-role function that takes a handle and
//!   returns `HostedType | SessionError`. `service.create(PullRequest as Author)`
//!   is rewritten to call this.
//!
//! - **Lookup function**: a per-machine-per-hosted-type function that takes a
//!   handle and key and returns `HostedType | SessionError` without opening a
//!   session. `service.lookup(PullRequest, key)` is rewritten during typecheck
//!   finalize to call this helper.
//!
//! - **On-handler helpers**: a per-machine-per-handler function that carries the
//!   body of each `on` handler through ordinary resolve/typecheck. Machine bodies
//!   are still parse-only overall, so generated helpers are the narrowest way to
//!   make `on` bodies semantic without committing to full machine execution yet.
//!
//! - **Deliver helpers**: a per-machine-per-trigger function used as the typed
//!   surface for `self.deliver(key, event)`. These currently return a placeholder
//!   `DeliverResult` until instance-backed delivery lands.
//!
//! - **Wait helpers**: a per-machine-per-state function used as the typed
//!   surface for hosted `session.wait()`. These drive the managed runtime until
//!   the hosted instance leaves the expected source state, then reconstruct the
//!   resumed linear value from the observed runtime state tag.
//!
//! - **Self-type rewriting**: `Self` references in machine constructor bodies are
//!   rewritten to the generated handle type, so `Self {}` returns the right struct.
//!
//! All generated functions are currently placeholders — they return minimal valid
//! values. Real runtime-backed implementations will replace these bodies when
//! machine execution lands.

use std::collections::{HashMap, HashSet};

use super::LinearIndex;
use crate::core::ast::visit_mut::{self, VisitorMut};
use crate::core::ast::*;
use crate::core::diag::Span;
use crate::core::machine::request_site::labeled_request_site_key;
use crate::core::machine::runtime_intrinsics::ensure_u64_runtime_intrinsics;

// ── Internal data structures ────────────────────────────────────────

#[derive(Clone, Debug)]
pub(super) struct MachineSpawnInfo {
    pub machine_name: String,
    pub machine_kind: u64,
    pub hosted_type_name: String,
    pub initial_state: Option<String>,
    pub state_names: Vec<String>,
    pub role_names: Vec<String>,
    pub key_field_name: String,
    pub shared_fields: Vec<(String, TypeExpr)>,
    pub key_ty: TypeExpr,
    pub machine_fields: Vec<StructDefField>,
    pub constructor: Option<FuncDef>,
    pub handle_type_name: String,
    pub spawn_fn_name: String,
}

#[derive(Clone, Debug)]
pub(super) struct MachineActionOverrideInfo {
    pub hosted_type_name: String,
    pub handle_type_name: String,
    pub instance_param_name: String,
    pub params: Vec<Param>,
    pub ret_ty_expr: TypeExpr,
    pub body: Expr,
    pub fn_name: String,
}

#[derive(Clone, Debug)]
pub(super) struct MachineActionSessionInfo {
    pub machine_name: String,
    pub action_name: String,
    pub hosted_type_name: String,
    pub handle_type_name: String,
    pub key_field_name: String,
    pub source_state_tag: u64,
    pub target_state_tag: u64,
    pub instance_param_name: String,
    pub params: Vec<Param>,
    pub ret_ty_expr: TypeExpr,
    pub body: Expr,
    pub fn_name: String,
}

#[derive(Clone, Debug)]
pub(super) struct MachineTriggerHandlerInfo {
    pub machine_name: String,
    pub machine_kind: u64,
    pub hosted_type_name: String,
    pub handle_type_name: String,
    pub key_field_name: String,
    pub shared_fields: Vec<(String, TypeExpr)>,
    pub state_names: Vec<String>,
    pub event_type_name: String,
    pub event_kind: u64,
    pub payload_shape: Option<HostedOnPayloadShape>,
    pub instance_param_name: String,
    pub params: Vec<Param>,
    pub body: Expr,
    pub fn_name: String,
    pub dispatch_wrapper_fn_name: String,
}

#[derive(Clone, Debug)]
pub(super) struct MachineOnHandlerInfo {
    pub machine_name: String,
    pub machine_kind: u64,
    pub handle_type_name: String,
    pub selector_type_name: String,
    pub event_kind: u64,
    pub payload_shape: Option<HostedOnPayloadShape>,
    pub params: Vec<Param>,
    pub body: Expr,
    pub fn_name: String,
    pub dispatch_wrapper_fn_name: String,
}

#[derive(Clone, Debug)]
pub(super) enum HostedOnPayloadShape {
    Empty,
    OneU64 { field0: String },
    TwoU64 { field0: String, field1: String },
}

#[derive(Clone, Debug)]
pub(super) struct MachineDeliverInfo {
    pub handle_type_name: String,
    pub key_ty: TypeExpr,
    pub event_type_name: String,
    pub source_state_tag: u64,
    pub target_state_tag: u64,
    pub event_kind: u64,
    pub payload_shape: Option<HostedOnPayloadShape>,
    pub fn_name: String,
}

#[derive(Clone, Debug)]
pub(super) struct MachineWaitInfo {
    pub hosted_type_name: String,
    pub handle_type_name: String,
    pub key_field_name: String,
    pub shared_fields: Vec<(String, TypeExpr)>,
    pub source_state_tag: u64,
    pub state_names: Vec<String>,
    pub fn_name: String,
}

#[derive(Clone, Debug)]
struct DerivedInteractionLoweringInfo {
    request_site_key: u64,
    reply_event_kinds: Vec<u64>,
}

// ── Name generation ─────────────────────────────────────────────────

pub(crate) fn machine_handle_type_name(machine_name: &str) -> String {
    format!("__mc_machine_handle_{machine_name}")
}

pub(crate) fn machine_spawn_fn_name(machine_name: &str) -> String {
    format!("__mc_machine_spawn_{machine_name}")
}

pub(crate) fn machine_create_fn_name(
    machine_name: &str,
    type_name: &str,
    role_name: &str,
) -> String {
    format!("__mc_machine_create_{machine_name}_{type_name}_{role_name}")
}

pub(crate) fn machine_resume_fn_name(
    machine_name: &str,
    type_name: &str,
    role_name: &str,
) -> String {
    format!("__mc_machine_resume_{machine_name}_{type_name}_{role_name}")
}

pub(crate) fn machine_lookup_fn_name(machine_name: &str, type_name: &str) -> String {
    format!("__mc_machine_lookup_{machine_name}_{type_name}")
}

pub(crate) fn machine_action_override_fn_name(machine_name: &str, action_name: &str) -> String {
    format!("__mc_machine_action_{machine_name}_{action_name}")
}

pub(crate) fn machine_action_session_fn_name(
    machine_name: &str,
    source_state: &str,
    action_name: &str,
) -> String {
    format!("__mc_machine_session_action_{machine_name}_{source_state}_{action_name}")
}

pub(crate) fn machine_trigger_handler_fn_name(machine_name: &str, trigger_name: &str) -> String {
    format!("__mc_machine_trigger_{machine_name}_{trigger_name}")
}

pub(crate) fn machine_trigger_dispatch_wrapper_fn_name(
    machine_name: &str,
    trigger_name: &str,
) -> String {
    format!("__mc_machine_trigger_dispatch_{machine_name}_{trigger_name}")
}

pub(crate) fn machine_on_handler_fn_name(machine_name: &str, handler_index: usize) -> String {
    format!("__mc_machine_on_{machine_name}_{handler_index}")
}

pub(crate) fn machine_on_dispatch_wrapper_fn_name(
    machine_name: &str,
    handler_index: usize,
) -> String {
    format!("__mc_machine_on_dispatch_{machine_name}_{handler_index}")
}

pub(crate) fn machine_deliver_fn_name(machine_name: &str, event_type_name: &str) -> String {
    format!("__mc_machine_deliver_{machine_name}_{event_type_name}")
}

pub(crate) fn machine_wait_fn_name(
    machine_name: &str,
    type_name: &str,
    source_state: &str,
) -> String {
    format!("__mc_machine_wait_{machine_name}_{type_name}_{source_state}")
}

const MANAGED_RUNTIME_CURRENT_FN: &str = "__mc_machine_runtime_managed_current_u64";
const MANAGED_RUNTIME_STEP_FN: &str = "__mc_machine_runtime_step_u64";
const MACHINE_RUNTIME_SEND_FN: &str = "__mc_machine_runtime_send_u64";
const HOSTED_LINEAR_SPAWN_FN: &str = "__mc_hosted_linear_spawn_u64";
const HOSTED_LINEAR_CREATE_FN: &str = "__mc_hosted_linear_create_u64";
const HOSTED_LINEAR_RESUME_STATE_FN: &str = "__mc_hosted_linear_resume_state_u64";
const HOSTED_LINEAR_DELIVER_FN: &str = "__mc_hosted_linear_deliver_u64";
const HOSTED_LINEAR_WAIT_STATE_FN: &str = "__mc_hosted_linear_wait_state_u64";
const HOSTED_LINEAR_BEGIN_DERIVED_INTERACTION_FN: &str =
    "__mc_hosted_linear_begin_derived_interaction_u64";
const HOSTED_LINEAR_BIND_DERIVED_INTERACTION_FN: &str =
    "__mc_hosted_linear_bind_derived_interaction_u64";
const HOSTED_LINEAR_ALLOW_REPLY_KIND_FN: &str = "__mc_hosted_linear_allow_reply_kind_u64";
const HOSTED_LINEAR_BIND_MACHINE_CONFIG_WORD_FN: &str =
    "__mc_hosted_linear_bind_machine_config_word_u64";
const HOSTED_LINEAR_MACHINE_CONFIG_WORD_FN: &str = "__mc_hosted_linear_machine_config_word_u64";
const HOSTED_ACTION_EMIT_BEGIN_FN: &str = "__mc_hosted_action_emit_begin_u64";
const HOSTED_ACTION_EMIT_ENABLE_DERIVED_REQUEST_FN: &str =
    "__mc_hosted_action_emit_enable_derived_request_u64";
const HOSTED_ACTION_EMIT_COMMIT_FN: &str = "__mc_hosted_action_emit_commit_u64";
const HOSTED_ACTION_EMIT_ABORT_FN: &str = "__mc_hosted_action_emit_abort_u64";
const HOSTED_LINEAR_ON_DISPATCH_FN: &str = "__mc_hosted_linear_on_dispatch_u64";
const HOSTED_LINEAR_TRIGGER_DISPATCH_FN: &str = "__mc_hosted_linear_trigger_dispatch_u64";
const HOSTED_LINEAR_REPLY_TRIGGER_KIND_FN: &str = "__mc_hosted_linear_reply_trigger_kind_u64";
const DEFAULT_MACHINE_MAILBOX_CAP: u64 = 64;
const HOSTED_UPDATE_OK: u64 = 0;
const HOSTED_UPDATE_STALE: u64 = 1;
const HOSTED_LINEAR_ON_KIND_BASE: u64 = 1024;
pub(super) const HOSTED_LINEAR_TRIGGER_KIND_BASE: u64 = 2048;

// ── Collection ──────────────────────────────────────────────────────

pub(super) fn collect_machine_spawn_infos(module: &Module) -> Vec<MachineSpawnInfo> {
    let type_defs = super::type_defs_by_name(module);
    module
        .machine_defs()
        .into_iter()
        .enumerate()
        .filter_map(|(machine_index, machine_def)| {
            let type_def = type_defs.get(&machine_def.host.type_name)?;
            let TypeDefKind::Linear { linear } = &type_def.kind else {
                return None;
            };
            let key_field = linear
                .fields
                .iter()
                .find(|field| field.name == machine_def.host.key_field)?;
            let machine_fields = machine_def
                .items
                .iter()
                .find_map(|item| match item {
                    MachineItem::Fields(fields) => Some(fields.fields.clone()),
                    _ => None,
                })
                .unwrap_or_default();
            let constructor = machine_def.items.iter().find_map(|item| match item {
                MachineItem::Constructor(constructor) => Some(constructor.clone()),
                _ => None,
            });
            Some(MachineSpawnInfo {
                machine_name: machine_def.name.clone(),
                machine_kind: machine_index as u64 + 1,
                hosted_type_name: machine_def.host.type_name.clone(),
                initial_state: linear.states.first().map(|state| state.name.clone()),
                state_names: linear
                    .states
                    .iter()
                    .map(|state| state.name.clone())
                    .collect(),
                role_names: linear.roles.iter().map(|role| role.name.clone()).collect(),
                key_field_name: machine_def.host.key_field.clone(),
                shared_fields: linear
                    .fields
                    .iter()
                    .map(|field| (field.name.clone(), field.ty.clone()))
                    .collect(),
                key_ty: key_field.ty.clone(),
                machine_fields,
                constructor,
                handle_type_name: machine_handle_type_name(&machine_def.name),
                spawn_fn_name: machine_spawn_fn_name(&machine_def.name),
            })
        })
        .collect()
}

pub(super) fn collect_machine_action_override_infos(
    module: &Module,
) -> Vec<MachineActionOverrideInfo> {
    let type_defs = super::type_defs_by_name(module);
    module
        .machine_defs()
        .into_iter()
        .flat_map(|machine_def| {
            let Some(type_def) = type_defs.get(&machine_def.host.type_name) else {
                return Vec::new();
            };
            let TypeDefKind::Linear { linear } = &type_def.kind else {
                return Vec::new();
            };
            let action_by_name = linear
                .actions
                .iter()
                .map(|action| (action.name.as_str(), action))
                .collect::<HashMap<_, _>>();
            let handle_type_name = machine_handle_type_name(&machine_def.name);
            machine_def
                .items
                .iter()
                .filter_map(|item| {
                    let MachineItem::Action(handler) = item else {
                        return None;
                    };
                    action_by_name.get(handler.name.as_str())?;
                    let ret_ty_expr = handler.ret_ty_expr.clone()?;
                    Some(MachineActionOverrideInfo {
                        hosted_type_name: machine_def.host.type_name.clone(),
                        handle_type_name: handle_type_name.clone(),
                        instance_param_name: handler.instance_param.clone(),
                        params: handler.params.clone(),
                        ret_ty_expr,
                        body: handler.body.clone(),
                        fn_name: machine_action_override_fn_name(&machine_def.name, &handler.name),
                    })
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub(super) fn collect_machine_action_session_infos(
    module: &Module,
) -> Vec<MachineActionSessionInfo> {
    let type_defs = super::type_defs_by_name(module);
    let base_action_impls = collect_base_action_impls(module);
    module
        .machine_defs()
        .into_iter()
        .flat_map(|machine_def| {
            let Some(type_def) = type_defs.get(&machine_def.host.type_name) else {
                return Vec::new();
            };
            let TypeDefKind::Linear { linear } = &type_def.kind else {
                return Vec::new();
            };
            let Some(key_field) = linear
                .fields
                .iter()
                .find(|field| field.name == machine_def.host.key_field)
            else {
                return Vec::new();
            };
            let handle_type_name = machine_handle_type_name(&machine_def.name);
            let state_tags = linear
                .states
                .iter()
                .enumerate()
                .map(|(index, state)| (state.name.as_str(), (index + 1) as u64))
                .collect::<HashMap<_, _>>();
            let override_handlers = machine_def
                .items
                .iter()
                .filter_map(|item| {
                    let MachineItem::Action(handler) = item else {
                        return None;
                    };
                    Some((handler.name.as_str(), handler))
                })
                .collect::<HashMap<_, _>>();

            linear
                .actions
                .iter()
                .filter_map(|action| {
                    let source_state_tag = *state_tags.get(action.source_state.as_str())?;
                    let target_state_tag = *state_tags.get(action.target_state.as_str())?;
                    let (instance_param_name, params, ret_ty_expr, body) = if let Some(handler) =
                        override_handlers.get(action.name.as_str()).copied()
                    {
                        (
                            handler.instance_param.clone(),
                            handler.params.clone(),
                            handler.ret_ty_expr.clone()?,
                            handler.body.clone(),
                        )
                    } else {
                        let (ret_ty_expr, body) = base_action_impls
                            .get(&(
                                machine_def.host.type_name.clone(),
                                action.source_state.clone(),
                                action.name.clone(),
                            ))?
                            .clone();
                        let params = action
                            .params
                            .iter()
                            .map(|param| Param {
                                id: param.id,
                                ident: param.name.clone(),
                                typ: param.ty.clone(),
                                mode: ParamMode::In,
                                default: None,
                                span: param.span,
                            })
                            .collect::<Vec<_>>();
                        (
                            "instance".to_string(),
                            params,
                            ret_ty_expr,
                            rename_var_in_expr(body, "self", "instance"),
                        )
                    };

                    Some(MachineActionSessionInfo {
                        machine_name: machine_def.name.clone(),
                        action_name: action.name.clone(),
                        hosted_type_name: machine_def.host.type_name.clone(),
                        handle_type_name: handle_type_name.clone(),
                        key_field_name: key_field.name.clone(),
                        source_state_tag,
                        target_state_tag,
                        instance_param_name,
                        params,
                        ret_ty_expr,
                        body,
                        fn_name: machine_action_session_fn_name(
                            &machine_def.name,
                            &action.source_state,
                            &action.name,
                        ),
                    })
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub(super) fn collect_machine_trigger_handler_infos(
    module: &Module,
) -> Vec<MachineTriggerHandlerInfo> {
    let type_defs = super::type_defs_by_name(module);
    module
        .machine_defs()
        .into_iter()
        .enumerate()
        .flat_map(|(machine_index, machine_def)| {
            let Some(type_def) = type_defs.get(&machine_def.host.type_name) else {
                return Vec::new();
            };
            let TypeDefKind::Linear { linear } = &type_def.kind else {
                return Vec::new();
            };
            let triggers_by_name = linear
                .triggers
                .iter()
                .map(|trigger| (trigger.name.as_str(), trigger))
                .collect::<HashMap<_, _>>();
            let handle_type_name = machine_handle_type_name(&machine_def.name);
            let shared_fields = linear
                .fields
                .iter()
                .map(|field| (field.name.clone(), field.ty.clone()))
                .collect::<Vec<_>>();
            let state_names = linear
                .states
                .iter()
                .map(|state| state.name.clone())
                .collect::<Vec<_>>();
            machine_def
                .items
                .iter()
                .filter_map(|item| {
                    let MachineItem::Trigger(handler) = item else {
                        return None;
                    };
                    let trigger = triggers_by_name.get(handler.name.as_str())?;
                    let event_type_name = trigger.name.clone();
                    Some(MachineTriggerHandlerInfo {
                        machine_name: machine_def.name.clone(),
                        machine_kind: machine_index as u64 + 1,
                        hosted_type_name: machine_def.host.type_name.clone(),
                        handle_type_name: handle_type_name.clone(),
                        key_field_name: machine_def.host.key_field.clone(),
                        shared_fields: shared_fields.clone(),
                        state_names: state_names.clone(),
                        event_type_name: event_type_name.clone(),
                        event_kind: HOSTED_LINEAR_TRIGGER_KIND_BASE
                            + linear
                                .triggers
                                .iter()
                                .position(|candidate| candidate.name == event_type_name)
                                .unwrap_or(0) as u64
                            + 1,
                        payload_shape: collect_supported_on_payload_shape(
                            type_defs.get(&event_type_name),
                        ),
                        instance_param_name: handler.instance_param.clone(),
                        params: handler.params.clone(),
                        body: handler.body.clone(),
                        fn_name: machine_trigger_handler_fn_name(&machine_def.name, &handler.name),
                        dispatch_wrapper_fn_name: machine_trigger_dispatch_wrapper_fn_name(
                            &machine_def.name,
                            &handler.name,
                        ),
                    })
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub(super) fn collect_machine_on_handler_infos(module: &Module) -> Vec<MachineOnHandlerInfo> {
    let type_defs = super::type_defs_by_name(module);
    module
        .machine_defs()
        .into_iter()
        .enumerate()
        .flat_map(|(machine_index, machine_def)| {
            let handle_type_name = machine_handle_type_name(&machine_def.name);
            machine_def
                .items
                .iter()
                .enumerate()
                .filter_map(|(index, item)| {
                    let MachineItem::On(handler) = item else {
                        return None;
                    };
                    let mut params = handler.params.clone();
                    if let Some(provenance) = &handler.provenance {
                        params.push(provenance.param.clone());
                    }
                    let selector_type_name = named_type_name(&handler.selector_ty)?;
                    Some(MachineOnHandlerInfo {
                        machine_name: machine_def.name.clone(),
                        machine_kind: machine_index as u64 + 1,
                        handle_type_name: handle_type_name.clone(),
                        selector_type_name: selector_type_name.clone(),
                        event_kind: HOSTED_LINEAR_ON_KIND_BASE + index as u64 + 1,
                        payload_shape: collect_supported_on_payload_shape(
                            type_defs.get(&selector_type_name),
                        )
                        .filter(|_| handler.provenance.is_none()),
                        params,
                        body: handler.body.clone(),
                        fn_name: machine_on_handler_fn_name(&machine_def.name, index),
                        dispatch_wrapper_fn_name: machine_on_dispatch_wrapper_fn_name(
                            &machine_def.name,
                            index,
                        ),
                    })
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn named_type_name(ty: &TypeExpr) -> Option<String> {
    let TypeExprKind::Named { ident, type_args } = &ty.kind else {
        return None;
    };
    if !type_args.is_empty() {
        return None;
    }
    Some(ident.clone())
}

fn collect_supported_on_payload_shape(type_def: Option<&&TypeDef>) -> Option<HostedOnPayloadShape> {
    let type_def = type_def?;
    let TypeDefKind::Struct { fields } = &type_def.kind else {
        return None;
    };
    match fields.as_slice() {
        [] => Some(HostedOnPayloadShape::Empty),
        [field0] if is_u64_named_type(&field0.ty) => Some(HostedOnPayloadShape::OneU64 {
            field0: field0.name.clone(),
        }),
        [field0, field1] if is_u64_named_type(&field0.ty) && is_u64_named_type(&field1.ty) => {
            Some(HostedOnPayloadShape::TwoU64 {
                field0: field0.name.clone(),
                field1: field1.name.clone(),
            })
        }
        _ => None,
    }
}

fn is_u64_named_type(ty: &TypeExpr) -> bool {
    matches!(
        &ty.kind,
        TypeExprKind::Named { ident, type_args } if ident == "u64" && type_args.is_empty()
    )
}

pub(super) fn collect_machine_deliver_infos(module: &Module) -> Vec<MachineDeliverInfo> {
    let type_defs = super::type_defs_by_name(module);
    module
        .machine_defs()
        .into_iter()
        .flat_map(|machine_def| {
            let Some(type_def) = type_defs.get(&machine_def.host.type_name) else {
                return Vec::new();
            };
            let TypeDefKind::Linear { linear } = &type_def.kind else {
                return Vec::new();
            };
            let Some(key_field) = linear
                .fields
                .iter()
                .find(|field| field.name == machine_def.host.key_field)
            else {
                return Vec::new();
            };
            let handle_type_name = machine_handle_type_name(&machine_def.name);
            let state_tags = linear
                .states
                .iter()
                .enumerate()
                .map(|(index, state)| (state.name.clone(), (index + 1) as u64))
                .collect::<HashMap<_, _>>();
            linear
                .triggers
                .iter()
                .enumerate()
                .filter_map(|(trigger_index, trigger)| {
                    let source_state_tag = *state_tags.get(&trigger.source_state)?;
                    let target_state_tag = *state_tags.get(&trigger.target_state)?;
                    Some(MachineDeliverInfo {
                        handle_type_name: handle_type_name.clone(),
                        key_ty: key_field.ty.clone(),
                        event_type_name: trigger.name.clone(),
                        source_state_tag,
                        target_state_tag,
                        event_kind: HOSTED_LINEAR_TRIGGER_KIND_BASE + trigger_index as u64 + 1,
                        payload_shape: collect_supported_on_payload_shape(
                            type_defs.get(&trigger.name),
                        ),
                        fn_name: machine_deliver_fn_name(&machine_def.name, &trigger.name),
                    })
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub(super) fn collect_machine_wait_infos(module: &Module) -> Vec<MachineWaitInfo> {
    let type_defs = super::type_defs_by_name(module);
    module
        .machine_defs()
        .into_iter()
        .flat_map(|machine_def| {
            let Some(type_def) = type_defs.get(&machine_def.host.type_name) else {
                return Vec::new();
            };
            let TypeDefKind::Linear { linear } = &type_def.kind else {
                return Vec::new();
            };
            let handle_type_name = machine_handle_type_name(&machine_def.name);
            let shared_fields = linear
                .fields
                .iter()
                .map(|field| (field.name.clone(), field.ty.clone()))
                .collect::<Vec<_>>();
            let state_tags = linear
                .states
                .iter()
                .enumerate()
                .map(|(index, state)| (state.name.clone(), (index + 1) as u64))
                .collect::<HashMap<_, _>>();
            let state_names = linear
                .states
                .iter()
                .map(|state| state.name.clone())
                .collect::<Vec<_>>();
            let mut waits = Vec::new();
            let mut seen_states = HashSet::<String>::new();
            for trigger in &linear.triggers {
                seen_states.insert(trigger.source_state.clone());
            }
            for source_state in linear
                .states
                .iter()
                .map(|state| state.name.clone())
                .filter(|state| seen_states.contains(state))
            {
                let Some(source_state_tag) = state_tags.get(&source_state).copied() else {
                    continue;
                };
                waits.push(MachineWaitInfo {
                    hosted_type_name: machine_def.host.type_name.clone(),
                    handle_type_name: handle_type_name.clone(),
                    key_field_name: machine_def.host.key_field.clone(),
                    shared_fields: shared_fields.clone(),
                    source_state_tag,
                    state_names: state_names.clone(),
                    fn_name: machine_wait_fn_name(
                        &machine_def.name,
                        &machine_def.host.type_name,
                        &source_state,
                    ),
                });
            }
            waits
        })
        .collect()
}

// ── Support type generation ─────────────────────────────────────────

/// Ensure `MachineError` and `SessionError` enum types exist in the module.
/// These are shared across all machines and inserted only once.
pub(super) fn ensure_hosted_support_types(module: &mut Module, node_id_gen: &mut NodeIdGen) {
    ensure_type_def(
        module,
        "MachineError",
        TopLevelItem::TypeDef(TypeDef {
            id: node_id_gen.new_id(),
            doc: None,
            attrs: Vec::new(),
            name: "MachineError".to_string(),
            type_params: Vec::new(),
            kind: TypeDefKind::Enum {
                variants: vec![
                    "SpawnFailed",
                    "BindFailed",
                    "StartFailed",
                    "RuntimeUnavailable",
                    "Unknown",
                    "NotRunning",
                    "MailboxFull",
                    "RequestFailed",
                ]
                .into_iter()
                .map(|name| EnumDefVariant {
                    id: node_id_gen.new_id(),
                    name: name.to_string(),
                    payload: Vec::new(),
                    span: Span::default(),
                })
                .collect(),
            },
            span: Span::default(),
        }),
    );
    ensure_type_def(
        module,
        "SessionError",
        TopLevelItem::TypeDef(TypeDef {
            id: node_id_gen.new_id(),
            doc: None,
            attrs: Vec::new(),
            name: "SessionError".to_string(),
            type_params: Vec::new(),
            kind: TypeDefKind::Enum {
                variants: vec![
                    EnumDefVariant {
                        id: node_id_gen.new_id(),
                        name: "InstanceNotFound".to_string(),
                        payload: Vec::new(),
                        span: Span::default(),
                    },
                    EnumDefVariant {
                        id: node_id_gen.new_id(),
                        name: "InvalidState".to_string(),
                        payload: Vec::new(),
                        span: Span::default(),
                    },
                ],
            },
            span: Span::default(),
        }),
    );
    ensure_type_def(
        module,
        "DeliverResult",
        TopLevelItem::TypeDef(TypeDef {
            id: node_id_gen.new_id(),
            doc: None,
            attrs: Vec::new(),
            name: "DeliverResult".to_string(),
            type_params: Vec::new(),
            kind: TypeDefKind::Enum {
                variants: vec![
                    EnumDefVariant {
                        id: node_id_gen.new_id(),
                        name: "Delivered".to_string(),
                        payload: Vec::new(),
                        span: Span::default(),
                    },
                    EnumDefVariant {
                        id: node_id_gen.new_id(),
                        name: "InstanceNotFound".to_string(),
                        payload: Vec::new(),
                        span: Span::default(),
                    },
                    EnumDefVariant {
                        id: node_id_gen.new_id(),
                        name: "InvalidState".to_string(),
                        payload: Vec::new(),
                        span: Span::default(),
                    },
                ],
            },
            span: Span::default(),
        }),
    );
}

/// Declare the small hosted runtime bridge used by generated `spawn`/`create`
/// helpers. Keeping these as ordinary callable declarations lets the existing
/// frontend and backend treat them like any other runtime bridge call.
pub(super) fn ensure_hosted_runtime_intrinsics(module: &mut Module, node_id_gen: &mut NodeIdGen) {
    let intrinsics: &[(&str, &[&str])] = &[
        (MANAGED_RUNTIME_CURRENT_FN, &[]),
        (MANAGED_RUNTIME_STEP_FN, &["runtime"]),
        (
            HOSTED_LINEAR_SPAWN_FN,
            &["runtime", "mailbox_cap", "machine_kind"],
        ),
        (
            MACHINE_RUNTIME_SEND_FN,
            &["runtime", "dst", "kind", "payload0", "payload1"],
        ),
        (
            HOSTED_LINEAR_CREATE_FN,
            &[
                "runtime",
                "machine_id",
                "initial_state_tag",
                "initial_payload",
            ],
        ),
        (
            HOSTED_LINEAR_RESUME_STATE_FN,
            &["runtime", "machine_id", "key"],
        ),
        (
            HOSTED_LINEAR_DELIVER_FN,
            &[
                "runtime",
                "machine_id",
                "key",
                "expected_state_tag",
                "target_state_tag",
                "trigger_kind",
                "payload0",
                "payload1",
            ],
        ),
        (
            HOSTED_LINEAR_WAIT_STATE_FN,
            &["runtime", "machine_id", "key", "expected_state_tag"],
        ),
        (
            HOSTED_LINEAR_BEGIN_DERIVED_INTERACTION_FN,
            &["runtime", "machine_id", "key"],
        ),
        (
            HOSTED_LINEAR_BIND_DERIVED_INTERACTION_FN,
            &["runtime", "machine_id", "key", "interaction_id"],
        ),
        (
            HOSTED_LINEAR_ALLOW_REPLY_KIND_FN,
            &["runtime", "pending_id", "kind"],
        ),
        (
            HOSTED_LINEAR_BIND_MACHINE_CONFIG_WORD_FN,
            &["runtime", "machine_id", "index", "value"],
        ),
        (
            HOSTED_LINEAR_MACHINE_CONFIG_WORD_FN,
            &["runtime", "machine_id", "index"],
        ),
        (HOSTED_ACTION_EMIT_BEGIN_FN, &["runtime", "machine_id"]),
        (
            HOSTED_ACTION_EMIT_ENABLE_DERIVED_REQUEST_FN,
            &["scope", "request_site_key"],
        ),
        (HOSTED_ACTION_EMIT_COMMIT_FN, &["scope"]),
        (HOSTED_ACTION_EMIT_ABORT_FN, &["scope"]),
    ];

    ensure_u64_runtime_intrinsics(module, node_id_gen, intrinsics);
}

fn ensure_type_def(module: &mut Module, type_name: &str, item: TopLevelItem) {
    let already_exists = module
        .top_level_items
        .iter()
        .any(|existing| matches!(existing, TopLevelItem::TypeDef(def) if def.name == type_name));
    if !already_exists {
        module.top_level_items.insert(0, item);
    }
}

// ── Handle type + spawn/create function generation ──────────────────

/// Generate handle struct types and spawn/create functions for each machine.
pub(super) fn append_machine_spawn_support(
    module: &mut Module,
    linear_index: &mut LinearIndex,
    machine_infos: &[MachineSpawnInfo],
    action_override_infos: &[MachineActionOverrideInfo],
    action_session_infos: &[MachineActionSessionInfo],
    trigger_handler_infos: &[MachineTriggerHandlerInfo],
    on_handler_infos: &[MachineOnHandlerInfo],
    deliver_infos: &[MachineDeliverInfo],
    wait_infos: &[MachineWaitInfo],
    node_id_gen: &mut NodeIdGen,
) {
    let machine_field_layouts = machine_field_layouts_by_handle_type(machine_infos);
    for info in machine_infos {
        module
            .top_level_items
            .push(build_machine_handle_type_def(info, node_id_gen));
        if let Some(method_block) =
            build_machine_handle_send_method_block(info, on_handler_infos, node_id_gen)
        {
            module.top_level_items.push(method_block);
        }
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_spawn_func(
                info,
                &machine_field_layouts,
                node_id_gen,
            )));
        // Generate a create helper for each role the hosted type declares.
        for role_name in &info.role_names {
            if info.initial_state.is_none() {
                continue;
            }
            module
                .top_level_items
                .push(TopLevelItem::FuncDef(build_machine_create_func(
                    info,
                    role_name,
                    node_id_gen,
                )));
            module
                .top_level_items
                .push(TopLevelItem::FuncDef(build_machine_resume_func(
                    info,
                    role_name,
                    node_id_gen,
                )));
        }
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_lookup_func(
                info,
                node_id_gen,
            )));
    }

    for override_info in action_override_infos {
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_action_override_func(
                override_info,
                node_id_gen,
            )));
    }

    for action_info in action_session_infos {
        let derived_interaction = linear_index
            .derived_interactions
            .get(&(
                action_info.machine_name.clone(),
                action_info.action_name.clone(),
            ))
            .and_then(|info| derive_interaction_lowering_info(linear_index, info));
        let action_func =
            build_machine_action_session_func(action_info, derived_interaction, node_id_gen);
        linear_index
            .hosted_dispatch_handler_machines
            .insert(action_func.id, action_info.machine_name.clone());
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(action_func));
    }

    for trigger_info in trigger_handler_infos {
        let trigger_func = build_machine_trigger_handler_func(trigger_info, node_id_gen);
        linear_index
            .hosted_dispatch_handler_machines
            .insert(trigger_func.id, trigger_info.machine_name.clone());
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(trigger_func));
        if trigger_info.payload_shape.is_some() {
            module.top_level_items.push(TopLevelItem::FuncDef(
                build_machine_trigger_dispatch_wrapper_func(
                    trigger_info,
                    &machine_field_layouts,
                    node_id_gen,
                ),
            ));
        }
    }

    for on_info in on_handler_infos {
        let on_func = build_machine_on_handler_func(on_info, node_id_gen);
        linear_index
            .hosted_dispatch_handler_machines
            .insert(on_func.id, on_info.machine_name.clone());
        module.top_level_items.push(TopLevelItem::FuncDef(on_func));
        if on_info.payload_shape.is_some() {
            module.top_level_items.push(TopLevelItem::FuncDef(
                build_machine_on_dispatch_wrapper_func(
                    on_info,
                    &machine_field_layouts,
                    node_id_gen,
                ),
            ));
        }
    }

    if on_handler_infos
        .iter()
        .any(|info| info.payload_shape.is_some())
    {
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_hosted_linear_on_dispatch_func(
                on_handler_infos,
                node_id_gen,
            )));
    }

    if trigger_handler_infos
        .iter()
        .any(|info| info.payload_shape.is_some())
    {
        module.top_level_items.push(TopLevelItem::FuncDef(
            build_hosted_linear_trigger_dispatch_func(trigger_handler_infos, node_id_gen),
        ));
    }

    if !linear_index.derived_interactions.is_empty() {
        module.top_level_items.push(TopLevelItem::FuncDef(
            build_hosted_linear_reply_trigger_kind_func(
                linear_index,
                machine_infos,
                deliver_infos,
                node_id_gen,
            ),
        ));
    }

    for deliver_info in deliver_infos {
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_deliver_func(
                deliver_info,
                node_id_gen,
            )));
    }

    for wait_info in wait_infos {
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_wait_func(
                wait_info,
                node_id_gen,
            )));
    }
}

fn derive_interaction_lowering_info(
    linear_index: &LinearIndex,
    info: &crate::core::linear::index::DerivedInteractionInfo,
) -> Option<DerivedInteractionLoweringInfo> {
    let host = linear_index.machine_hosts.get(&info.machine_name)?;
    let reply_event_kinds = info
        .reply_types
        .iter()
        .map(|reply_type| host.trigger_event_kinds.get(reply_type).copied())
        .collect::<Option<Vec<_>>>()?;
    Some(DerivedInteractionLoweringInfo {
        request_site_key: labeled_request_site_key(&format!(
            "derived_interaction:{}:{}",
            info.machine_name, info.action_name
        )),
        reply_event_kinds,
    })
}

fn machine_field_layouts_by_handle_type(
    machine_infos: &[MachineSpawnInfo],
) -> HashMap<String, Vec<StructDefField>> {
    machine_infos
        .iter()
        .map(|info| (info.handle_type_name.clone(), info.machine_fields.clone()))
        .collect()
}

fn build_machine_handle_type_def(
    info: &MachineSpawnInfo,
    node_id_gen: &mut NodeIdGen,
) -> TopLevelItem {
    let mut fields = vec![StructDefField {
        id: node_id_gen.new_id(),
        attrs: Vec::new(),
        name: "_id".to_string(),
        ty: TypeExpr {
            id: node_id_gen.new_id(),
            kind: TypeExprKind::Named {
                ident: "u64".to_string(),
                type_args: Vec::new(),
            },
            span: Span::default(),
        },
        span: Span::default(),
    }];
    fields.extend(info.machine_fields.iter().cloned());

    TopLevelItem::TypeDef(TypeDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        name: info.handle_type_name.clone(),
        type_params: Vec::new(),
        kind: TypeDefKind::Struct { fields },
        span: Span::default(),
    })
}

fn build_machine_handle_send_method_block(
    info: &MachineSpawnInfo,
    on_handler_infos: &[MachineOnHandlerInfo],
    node_id_gen: &mut NodeIdGen,
) -> Option<TopLevelItem> {
    let mut seen_selector_types = HashSet::<String>::new();
    let mut method_items = Vec::new();
    for handler in on_handler_infos.iter().filter(|handler| {
        handler.machine_name == info.machine_name && handler.payload_shape.is_some()
    }) {
        if seen_selector_types.insert(handler.selector_type_name.clone()) {
            method_items.push(MethodItem::Def(build_machine_handle_send_method(
                handler,
                node_id_gen,
            )));
        }
    }

    if method_items.is_empty() {
        return None;
    }

    Some(TopLevelItem::MethodBlock(MethodBlock {
        id: node_id_gen.new_id(),
        type_name: info.handle_type_name.clone(),
        type_args: Vec::new(),
        trait_name: None,
        method_items,
        span: Span::default(),
    }))
}

fn build_machine_handle_send_method(
    info: &MachineOnHandlerInfo,
    node_id_gen: &mut NodeIdGen,
) -> MethodDef {
    let span = Span::default();
    let Some(payload_shape) = info.payload_shape.as_ref() else {
        panic!("compiler bug: hosted send method requires supported payload shape");
    };
    let payload_ty = TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Named {
            ident: info.selector_type_name.clone(),
            type_args: Vec::new(),
        },
        span,
    };

    MethodDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: MethodSig {
            name: "send".to_string(),
            type_params: Vec::new(),
            self_param: SelfParam {
                id: node_id_gen.new_id(),
                mode: ParamMode::In,
                receiver_ty_expr: None,
                span,
            },
            params: vec![Param {
                id: node_id_gen.new_id(),
                ident: "payload".to_string(),
                typ: payload_ty,
                mode: ParamMode::In,
                default: None,
                span,
            }],
            ret_ty_expr: machine_send_result_union_type(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_rt",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_zero(
                        "__mc_rt",
                        "MachineError",
                        "RuntimeUnavailable",
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_status",
                        call_expr(
                            MACHINE_RUNTIME_SEND_FN,
                            vec![
                                var_expr("__mc_rt", node_id_gen, span),
                                self_field_expr("_id", node_id_gen, span),
                                int_expr(info.event_kind, node_id_gen, span),
                                payload_word_expr(payload_shape, "payload", 0, node_id_gen, span),
                                payload_word_expr(payload_shape, "payload", 1, node_id_gen, span),
                            ],
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_eq(
                        "__mc_status",
                        1,
                        "MachineError",
                        "Unknown",
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_eq(
                        "__mc_status",
                        2,
                        "MachineError",
                        "NotRunning",
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_eq(
                        "__mc_status",
                        3,
                        "MachineError",
                        "MailboxFull",
                        node_id_gen,
                        span,
                    )),
                ],
                tail: Some(Box::new(unit_expr(node_id_gen, span))),
            },
            span,
        },
        span,
    }
}

fn build_machine_on_dispatch_wrapper_func(
    info: &MachineOnHandlerInfo,
    machine_field_layouts: &HashMap<String, Vec<StructDefField>>,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    let Some(payload_shape) = info.payload_shape.as_ref() else {
        panic!("compiler bug: hosted on-dispatch wrapper requires supported payload shape");
    };
    let mut call_args = vec![var_expr("__mc_self", node_id_gen, span)];
    if !info.params.is_empty() {
        call_args.push(var_expr("__mc_event", node_id_gen, span));
    }
    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: info.dispatch_wrapper_fn_name.clone(),
            type_params: Vec::new(),
            params: vec![
                u64_param("machine_id", node_id_gen, span),
                u64_param("payload0", node_id_gen, span),
                u64_param("payload1", node_id_gen, span),
            ],
            ret_ty_expr: u64_type_expr(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_rt",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_u64_if_zero("__mc_rt", node_id_gen, span)),
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_self",
                        build_machine_handle_from_runtime_expr(
                            &info.handle_type_name,
                            "__mc_rt",
                            var_expr("machine_id", node_id_gen, span),
                            machine_field_layouts,
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_event",
                        build_on_payload_expr(
                            &info.selector_type_name,
                            payload_shape,
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(call_expr(&info.fn_name, call_args, node_id_gen, span)),
                ],
                tail: Some(Box::new(int_expr(1, node_id_gen, span))),
            },
            span,
        },
        span,
    }
}

fn build_hosted_linear_on_dispatch_func(
    on_handler_infos: &[MachineOnHandlerInfo],
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    let mut items = Vec::new();
    for info in on_handler_infos
        .iter()
        .filter(|info| info.payload_shape.is_some())
    {
        items.push(BlockItem::Expr(Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::If {
                cond: Box::new(and_expr(
                    eq_var_to_int("machine_kind", info.machine_kind, node_id_gen, span),
                    eq_var_to_int("kind", info.event_kind, node_id_gen, span),
                    node_id_gen,
                    span,
                )),
                then_body: Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::Block {
                        items: vec![BlockItem::Stmt(StmtExpr {
                            id: node_id_gen.new_id(),
                            kind: StmtExprKind::Return {
                                value: Some(Box::new(call_expr(
                                    &info.dispatch_wrapper_fn_name,
                                    vec![
                                        var_expr("machine_id", node_id_gen, span),
                                        var_expr("payload0", node_id_gen, span),
                                        var_expr("payload1", node_id_gen, span),
                                    ],
                                    node_id_gen,
                                    span,
                                ))),
                            },
                            span,
                        })],
                        tail: None,
                    },
                    span,
                }),
                else_body: Box::new(empty_block_expr(node_id_gen, span)),
            },
            span,
        }));
    }

    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: HOSTED_LINEAR_ON_DISPATCH_FN.to_string(),
            type_params: Vec::new(),
            params: vec![
                u64_param("machine_kind", node_id_gen, span),
                u64_param("machine_id", node_id_gen, span),
                u64_param("kind", node_id_gen, span),
                u64_param("payload0", node_id_gen, span),
                u64_param("payload1", node_id_gen, span),
            ],
            ret_ty_expr: u64_type_expr(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items,
                tail: Some(Box::new(int_expr(0, node_id_gen, span))),
            },
            span,
        },
        span,
    }
}

fn build_machine_trigger_dispatch_wrapper_func(
    info: &MachineTriggerHandlerInfo,
    machine_field_layouts: &HashMap<String, Vec<StructDefField>>,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    let Some(payload_shape) = info.payload_shape.as_ref() else {
        panic!("compiler bug: hosted trigger dispatch wrapper requires supported payload shape");
    };
    let mut next_state_expr = int_expr(0, node_id_gen, span);
    for (index, state_name) in info.state_names.iter().enumerate().rev() {
        let mut call_args = vec![
            var_expr("__mc_self", node_id_gen, span),
            var_expr("__mc_instance", node_id_gen, span),
        ];
        if !info.params.is_empty() {
            call_args.push(var_expr("__mc_event", node_id_gen, span));
        }
        let state_expr = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::EnumVariant {
                enum_name: info.hosted_type_name.clone(),
                variant: state_name.clone(),
                type_args: Vec::new(),
                payload: placeholder_shared_field_payload_with_key_var(
                    &info.shared_fields,
                    &info.key_field_name,
                    "key",
                    node_id_gen,
                ),
            },
            span,
        };
        next_state_expr = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::If {
                cond: Box::new(eq_var_to_int(
                    "current_state_tag",
                    (index + 1) as u64,
                    node_id_gen,
                    span,
                )),
                then_body: Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::Block {
                        items: vec![
                            BlockItem::Stmt(let_bind_stmt(
                                "__mc_instance",
                                state_expr,
                                node_id_gen,
                                span,
                            )),
                            BlockItem::Stmt(let_bind_stmt(
                                "__mc_next",
                                call_expr(&info.fn_name, call_args, node_id_gen, span),
                                node_id_gen,
                                span,
                            )),
                        ],
                        tail: Some(Box::new(build_machine_state_tag_expr(
                            &info.hosted_type_name,
                            &info.state_names,
                            "__mc_next",
                            node_id_gen,
                        ))),
                    },
                    span,
                }),
                else_body: Box::new(next_state_expr),
            },
            span,
        };
    }
    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: info.dispatch_wrapper_fn_name.clone(),
            type_params: Vec::new(),
            params: vec![
                u64_param("machine_id", node_id_gen, span),
                u64_param("current_state_tag", node_id_gen, span),
                u64_param("key", node_id_gen, span),
                u64_param("payload0", node_id_gen, span),
                u64_param("payload1", node_id_gen, span),
            ],
            ret_ty_expr: u64_type_expr(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_rt",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_u64_if_zero("__mc_rt", node_id_gen, span)),
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_self",
                        build_machine_handle_from_runtime_expr(
                            &info.handle_type_name,
                            "__mc_rt",
                            var_expr("machine_id", node_id_gen, span),
                            machine_field_layouts,
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_event",
                        build_on_payload_expr(
                            &info.event_type_name,
                            payload_shape,
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                ],
                tail: Some(Box::new(next_state_expr)),
            },
            span,
        },
        span,
    }
}

fn build_hosted_linear_trigger_dispatch_func(
    trigger_handler_infos: &[MachineTriggerHandlerInfo],
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    let mut items = Vec::new();
    for info in trigger_handler_infos
        .iter()
        .filter(|info| info.payload_shape.is_some())
    {
        items.push(BlockItem::Expr(Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::If {
                cond: Box::new(and_expr(
                    eq_var_to_int("machine_kind", info.machine_kind, node_id_gen, span),
                    eq_var_to_int("kind", info.event_kind, node_id_gen, span),
                    node_id_gen,
                    span,
                )),
                then_body: Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::Block {
                        items: vec![BlockItem::Stmt(StmtExpr {
                            id: node_id_gen.new_id(),
                            kind: StmtExprKind::Return {
                                value: Some(Box::new(call_expr(
                                    &info.dispatch_wrapper_fn_name,
                                    vec![
                                        var_expr("machine_id", node_id_gen, span),
                                        var_expr("current_state_tag", node_id_gen, span),
                                        var_expr("key", node_id_gen, span),
                                        var_expr("payload0", node_id_gen, span),
                                        var_expr("payload1", node_id_gen, span),
                                    ],
                                    node_id_gen,
                                    span,
                                ))),
                            },
                            span,
                        })],
                        tail: None,
                    },
                    span,
                }),
                else_body: Box::new(empty_block_expr(node_id_gen, span)),
            },
            span,
        }));
    }

    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: HOSTED_LINEAR_TRIGGER_DISPATCH_FN.to_string(),
            type_params: Vec::new(),
            params: vec![
                u64_param("machine_kind", node_id_gen, span),
                u64_param("machine_id", node_id_gen, span),
                u64_param("kind", node_id_gen, span),
                u64_param("current_state_tag", node_id_gen, span),
                u64_param("key", node_id_gen, span),
                u64_param("payload0", node_id_gen, span),
                u64_param("payload1", node_id_gen, span),
            ],
            ret_ty_expr: u64_type_expr(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items,
                tail: Some(Box::new(int_expr(0, node_id_gen, span))),
            },
            span,
        },
        span,
    }
}

/// Generate: `fn __mc_machine_spawn_X() -> HandleType | MachineError { HandleType { _id: 1 } }`
fn build_machine_spawn_func(
    info: &MachineSpawnInfo,
    machine_field_layouts: &HashMap<String, Vec<StructDefField>>,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    let mut items = vec![
        BlockItem::Stmt(let_bind_stmt(
            "__mc_rt",
            call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
            node_id_gen,
            span,
        )),
        BlockItem::Expr(return_enum_error_if_zero(
            "__mc_rt",
            "MachineError",
            "RuntimeUnavailable",
            node_id_gen,
            span,
        )),
        BlockItem::Stmt(let_bind_stmt(
            "__mc_machine_id",
            call_expr(
                HOSTED_LINEAR_SPAWN_FN,
                vec![
                    var_expr("__mc_rt", node_id_gen, span),
                    int_expr(DEFAULT_MACHINE_MAILBOX_CAP, node_id_gen, span),
                    int_expr(info.machine_kind, node_id_gen, span),
                ],
                node_id_gen,
                span,
            ),
            node_id_gen,
            span,
        )),
        BlockItem::Expr(return_enum_error_if_zero(
            "__mc_machine_id",
            "MachineError",
            "SpawnFailed",
            node_id_gen,
            span,
        )),
    ];

    if let Some(constructor) = &info.constructor {
        items.push(BlockItem::Stmt(let_bind_stmt(
            "__mc_self",
            freshen_expr_ids(constructor.body.clone(), node_id_gen),
            node_id_gen,
            span,
        )));
    }
    if info.constructor.is_some() {
        items.extend(build_machine_config_bind_items(
            info,
            "__mc_self",
            "__mc_rt",
            "__mc_machine_id",
            machine_field_layouts,
            node_id_gen,
            span,
        ));
    }

    let tail = if info.constructor.is_some() {
        build_machine_handle_value_from_source(
            info,
            "__mc_self",
            "__mc_machine_id",
            node_id_gen,
            span,
        )
    } else {
        build_machine_handle_value(info, None, "__mc_machine_id", node_id_gen, span)
    };

    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: info.spawn_fn_name.clone(),
            type_params: Vec::new(),
            params: info
                .constructor
                .as_ref()
                .map(|constructor| freshen_params(&constructor.sig.params, node_id_gen))
                .unwrap_or_default(),
            ret_ty_expr: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Union {
                    variants: vec![
                        TypeExpr {
                            id: node_id_gen.new_id(),
                            kind: TypeExprKind::Named {
                                ident: info.handle_type_name.clone(),
                                type_args: Vec::new(),
                            },
                            span,
                        },
                        TypeExpr {
                            id: node_id_gen.new_id(),
                            kind: TypeExprKind::Named {
                                ident: "MachineError".to_string(),
                                type_args: Vec::new(),
                            },
                            span,
                        },
                    ],
                },
                span,
            },
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items,
                tail: Some(Box::new(tail)),
            },
            span,
        },
        span,
    }
}

/// Generate: `fn __mc_machine_create_X_T_R(self: HandleType) -> T | SessionError { T::InitialState }`
fn build_machine_create_func(
    info: &MachineSpawnInfo,
    role_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    let initial_state = info
        .initial_state
        .as_ref()
        .expect("hosted linear types must have an initial state");
    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: machine_create_fn_name(&info.machine_name, &info.hosted_type_name, role_name),
            type_params: Vec::new(),
            params: vec![Param {
                id: node_id_gen.new_id(),
                ident: "self".to_string(),
                mode: ParamMode::In,
                typ: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: info.handle_type_name.clone(),
                        type_args: Vec::new(),
                    },
                    span,
                },
                default: None,
                span,
            }],
            ret_ty_expr: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Union {
                    variants: vec![
                        TypeExpr {
                            id: node_id_gen.new_id(),
                            kind: TypeExprKind::Named {
                                ident: info.hosted_type_name.clone(),
                                type_args: Vec::new(),
                            },
                            span,
                        },
                        TypeExpr {
                            id: node_id_gen.new_id(),
                            kind: TypeExprKind::Named {
                                ident: "SessionError".to_string(),
                                type_args: Vec::new(),
                            },
                            span,
                        },
                    ],
                },
                span,
            },
            span,
        },
        // Hosted create now allocates a real instance in the machine-backed
        // instance table and threads the assigned key into the returned state's
        // shared key field. Other shared fields still use placeholder values
        // until fuller runtime-backed construction lands.
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_rt",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_zero(
                        "__mc_rt",
                        "SessionError",
                        "InstanceNotFound",
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_key",
                        call_expr(
                            HOSTED_LINEAR_CREATE_FN,
                            vec![
                                var_expr("__mc_rt", node_id_gen, span),
                                self_field_expr("_id", node_id_gen, span),
                                int_expr(1, node_id_gen, span),
                                int_expr(0, node_id_gen, span),
                            ],
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_zero(
                        "__mc_key",
                        "SessionError",
                        "InstanceNotFound",
                        node_id_gen,
                        span,
                    )),
                ],
                tail: Some(Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::EnumVariant {
                        enum_name: info.hosted_type_name.clone(),
                        variant: initial_state.clone(),
                        type_args: Vec::new(),
                        payload: placeholder_shared_field_payload_with_key(
                            &info.shared_fields,
                            &info.key_field_name,
                            node_id_gen,
                        ),
                    },
                    span,
                })),
            },
            span,
        },
        span,
    }
}

/// Generate: `fn __mc_machine_resume_X_T_R(self: HandleType, key: KeyTy) -> T | SessionError { T::InitialState }`
fn build_machine_resume_func(
    info: &MachineSpawnInfo,
    role_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: machine_resume_fn_name(&info.machine_name, &info.hosted_type_name, role_name),
            type_params: Vec::new(),
            params: vec![
                Param {
                    id: node_id_gen.new_id(),
                    ident: "self".to_string(),
                    mode: ParamMode::In,
                    typ: TypeExpr {
                        id: node_id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: info.handle_type_name.clone(),
                            type_args: Vec::new(),
                        },
                        span,
                    },
                    default: None,
                    span,
                },
                Param {
                    id: node_id_gen.new_id(),
                    ident: "key".to_string(),
                    mode: ParamMode::In,
                    typ: info.key_ty.clone(),
                    default: None,
                    span,
                },
            ],
            ret_ty_expr: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Union {
                    variants: vec![
                        TypeExpr {
                            id: node_id_gen.new_id(),
                            kind: TypeExprKind::Named {
                                ident: info.hosted_type_name.clone(),
                                type_args: Vec::new(),
                            },
                            span,
                        },
                        TypeExpr {
                            id: node_id_gen.new_id(),
                            kind: TypeExprKind::Named {
                                ident: "SessionError".to_string(),
                                type_args: Vec::new(),
                            },
                            span,
                        },
                    ],
                },
                span,
            },
            span,
        },
        // Resume now consults the hosted runtime bridge for the current
        // instance state tag, then reconstructs the matching state variant with
        // the resumed key threaded into the shared-field payload.
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_rt",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_zero(
                        "__mc_rt",
                        "SessionError",
                        "InstanceNotFound",
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_state_tag",
                        call_expr(
                            HOSTED_LINEAR_RESUME_STATE_FN,
                            vec![
                                var_expr("__mc_rt", node_id_gen, span),
                                self_field_expr("_id", node_id_gen, span),
                                var_expr("key", node_id_gen, span),
                            ],
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_zero(
                        "__mc_state_tag",
                        "SessionError",
                        "InstanceNotFound",
                        node_id_gen,
                        span,
                    )),
                ],
                tail: Some(Box::new(build_machine_resume_state_expr(
                    info,
                    "__mc_state_tag",
                    "key",
                    node_id_gen,
                ))),
            },
            span,
        },
        span,
    }
}

fn build_machine_lookup_func(info: &MachineSpawnInfo, node_id_gen: &mut NodeIdGen) -> FuncDef {
    let span = Span::default();
    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: machine_lookup_fn_name(&info.machine_name, &info.hosted_type_name),
            type_params: Vec::new(),
            params: vec![
                Param {
                    id: node_id_gen.new_id(),
                    ident: "self".to_string(),
                    mode: ParamMode::In,
                    typ: TypeExpr {
                        id: node_id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: info.handle_type_name.clone(),
                            type_args: Vec::new(),
                        },
                        span,
                    },
                    default: None,
                    span,
                },
                Param {
                    id: node_id_gen.new_id(),
                    ident: "key".to_string(),
                    mode: ParamMode::In,
                    typ: info.key_ty.clone(),
                    default: None,
                    span,
                },
            ],
            ret_ty_expr: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Union {
                    variants: vec![
                        TypeExpr {
                            id: node_id_gen.new_id(),
                            kind: TypeExprKind::Named {
                                ident: info.hosted_type_name.clone(),
                                type_args: Vec::new(),
                            },
                            span,
                        },
                        TypeExpr {
                            id: node_id_gen.new_id(),
                            kind: TypeExprKind::Named {
                                ident: "SessionError".to_string(),
                                type_args: Vec::new(),
                            },
                            span,
                        },
                    ],
                },
                span,
            },
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_rt",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_zero(
                        "__mc_rt",
                        "SessionError",
                        "InstanceNotFound",
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_state_tag",
                        call_expr(
                            HOSTED_LINEAR_RESUME_STATE_FN,
                            vec![
                                var_expr("__mc_rt", node_id_gen, span),
                                self_field_expr("_id", node_id_gen, span),
                                var_expr("key", node_id_gen, span),
                            ],
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_zero(
                        "__mc_state_tag",
                        "SessionError",
                        "InstanceNotFound",
                        node_id_gen,
                        span,
                    )),
                ],
                tail: Some(Box::new(build_machine_resume_state_expr(
                    info,
                    "__mc_state_tag",
                    "key",
                    node_id_gen,
                ))),
            },
            span,
        },
        span,
    }
}

fn build_machine_action_override_func(
    info: &MachineActionOverrideInfo,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: info.fn_name.clone(),
            type_params: Vec::new(),
            params: std::iter::once(Param {
                id: node_id_gen.new_id(),
                ident: "self".to_string(),
                mode: ParamMode::In,
                typ: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: info.handle_type_name.clone(),
                        type_args: Vec::new(),
                    },
                    span,
                },
                default: None,
                span,
            })
            .chain(std::iter::once(Param {
                id: node_id_gen.new_id(),
                ident: info.instance_param_name.clone(),
                // The generated helper takes the source-state value by ordinary
                // input. Linearity has already been enforced in the source
                // program; using `sink` here would trigger the structural
                // checker for an internal helper over the lowered enum type.
                mode: ParamMode::In,
                typ: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: info.hosted_type_name.clone(),
                        type_args: Vec::new(),
                    },
                    span,
                },
                default: None,
                span,
            }))
            .chain(info.params.iter().cloned())
            .collect(),
            // Hosted overrides always behave like session operations, so the
            // generated helper widens the base action return to include
            // `SessionError`.
            ret_ty_expr: widen_machine_override_return_type(
                &info.ret_ty_expr,
                &info.hosted_type_name,
                node_id_gen,
            ),
            span,
        },
        body: info.body.clone(),
        span,
    }
}

fn build_machine_action_session_func(
    info: &MachineActionSessionInfo,
    derived_interaction: Option<DerivedInteractionLoweringInfo>,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    let session_params = freshen_params(&info.params, node_id_gen);
    let action_body = freshen_expr_ids(info.body.clone(), node_id_gen);
    let mut items = vec![
        BlockItem::Stmt(let_bind_stmt(
            "__mc_rt",
            call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
            node_id_gen,
            span,
        )),
        BlockItem::Expr(return_enum_error_if_zero(
            "__mc_rt",
            "SessionError",
            "InstanceNotFound",
            node_id_gen,
            span,
        )),
        BlockItem::Stmt(let_bind_stmt(
            "__mc_state_tag",
            call_expr(
                HOSTED_LINEAR_RESUME_STATE_FN,
                vec![
                    var_expr("__mc_rt", node_id_gen, span),
                    self_field_expr("_id", node_id_gen, span),
                    struct_field_expr(
                        &info.instance_param_name,
                        &info.key_field_name,
                        node_id_gen,
                        span,
                    ),
                ],
                node_id_gen,
                span,
            ),
            node_id_gen,
            span,
        )),
        BlockItem::Expr(return_enum_error_if_zero(
            "__mc_state_tag",
            "SessionError",
            "InstanceNotFound",
            node_id_gen,
            span,
        )),
        BlockItem::Expr(return_enum_error_if_ne(
            "__mc_state_tag",
            info.source_state_tag,
            "SessionError",
            "InvalidState",
            node_id_gen,
            span,
        )),
        BlockItem::Stmt(let_bind_stmt(
            "__mc_emit_scope",
            call_expr(
                HOSTED_ACTION_EMIT_BEGIN_FN,
                vec![
                    var_expr("__mc_rt", node_id_gen, span),
                    self_field_expr("_id", node_id_gen, span),
                ],
                node_id_gen,
                span,
            ),
            node_id_gen,
            span,
        )),
        BlockItem::Expr(return_enum_error_if_zero(
            "__mc_emit_scope",
            "SessionError",
            "InstanceNotFound",
            node_id_gen,
            span,
        )),
    ];
    if let Some(derived_interaction) = derived_interaction.as_ref() {
        items.push(BlockItem::Stmt(let_bind_stmt(
            "__mc_request_armed",
            call_expr(
                HOSTED_ACTION_EMIT_ENABLE_DERIVED_REQUEST_FN,
                vec![
                    var_expr("__mc_emit_scope", node_id_gen, span),
                    int_expr(derived_interaction.request_site_key, node_id_gen, span),
                ],
                node_id_gen,
                span,
            ),
            node_id_gen,
            span,
        )));
        items.push(BlockItem::Expr(return_enum_error_if_zero(
            "__mc_request_armed",
            "SessionError",
            "InstanceNotFound",
            node_id_gen,
            span,
        )));
    }
    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: info.fn_name.clone(),
            type_params: Vec::new(),
            params: std::iter::once(Param {
                id: node_id_gen.new_id(),
                ident: "self".to_string(),
                mode: ParamMode::In,
                typ: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: info.handle_type_name.clone(),
                        type_args: Vec::new(),
                    },
                    span,
                },
                default: None,
                span,
            })
            .chain(std::iter::once(Param {
                id: node_id_gen.new_id(),
                ident: info.instance_param_name.clone(),
                mode: ParamMode::In,
                typ: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: info.hosted_type_name.clone(),
                        type_args: Vec::new(),
                    },
                    span,
                },
                default: None,
                span,
            }))
            .chain(session_params)
            .collect(),
            ret_ty_expr: widen_machine_override_return_type(
                &info.ret_ty_expr,
                &info.hosted_type_name,
                node_id_gen,
            ),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items,
                tail: Some(Box::new(build_machine_action_commit_expr(
                    info,
                    action_body,
                    derived_interaction.as_ref(),
                    node_id_gen,
                ))),
            },
            span,
        },
        span,
    }
}

fn build_machine_trigger_handler_func(
    info: &MachineTriggerHandlerInfo,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: info.fn_name.clone(),
            type_params: Vec::new(),
            params: std::iter::once(Param {
                id: node_id_gen.new_id(),
                ident: "self".to_string(),
                mode: ParamMode::In,
                typ: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: info.handle_type_name.clone(),
                        type_args: Vec::new(),
                    },
                    span,
                },
                default: None,
                span,
            })
            .chain(std::iter::once(Param {
                id: node_id_gen.new_id(),
                ident: info.instance_param_name.clone(),
                mode: ParamMode::In,
                typ: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: info.hosted_type_name.clone(),
                        type_args: Vec::new(),
                    },
                    span,
                },
                default: None,
                span,
            }))
            .chain(info.params.iter().cloned())
            .collect(),
            // Trigger handlers are machine-internal and authoritative, so they
            // return only the next hosted state, not a fallible session result.
            ret_ty_expr: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: info.hosted_type_name.clone(),
                    type_args: Vec::new(),
                },
                span,
            },
            span,
        },
        body: info.body.clone(),
        span,
    }
}

fn build_machine_on_handler_func(
    info: &MachineOnHandlerInfo,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: info.fn_name.clone(),
            type_params: Vec::new(),
            params: std::iter::once(Param {
                id: node_id_gen.new_id(),
                ident: "self".to_string(),
                mode: ParamMode::In,
                typ: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: info.handle_type_name.clone(),
                        type_args: Vec::new(),
                    },
                    span,
                },
                default: None,
                span,
            })
            .chain(info.params.iter().cloned())
            .collect(),
            // `on` handlers are machine-level message handlers. For now they
            // are modeled as side-effecting helpers that return unit.
            ret_ty_expr: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: "()".to_string(),
                    type_args: Vec::new(),
                },
                span,
            },
            span,
        },
        body: info.body.clone(),
        span,
    }
}

fn build_machine_deliver_func(info: &MachineDeliverInfo, node_id_gen: &mut NodeIdGen) -> FuncDef {
    let span = Span::default();
    let payload0_expr = info
        .payload_shape
        .as_ref()
        .map(|shape| payload_word_expr(shape, "event", 0, node_id_gen, span))
        .unwrap_or_else(|| int_expr(0, node_id_gen, span));
    let payload1_expr = info
        .payload_shape
        .as_ref()
        .map(|shape| payload_word_expr(shape, "event", 1, node_id_gen, span))
        .unwrap_or_else(|| int_expr(0, node_id_gen, span));
    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: info.fn_name.clone(),
            type_params: Vec::new(),
            params: vec![
                Param {
                    id: node_id_gen.new_id(),
                    ident: "self".to_string(),
                    mode: ParamMode::In,
                    typ: TypeExpr {
                        id: node_id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: info.handle_type_name.clone(),
                            type_args: Vec::new(),
                        },
                        span,
                    },
                    default: None,
                    span,
                },
                Param {
                    id: node_id_gen.new_id(),
                    ident: "key".to_string(),
                    mode: ParamMode::In,
                    typ: info.key_ty.clone(),
                    default: None,
                    span,
                },
                Param {
                    id: node_id_gen.new_id(),
                    ident: "event".to_string(),
                    mode: ParamMode::In,
                    typ: TypeExpr {
                        id: node_id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: info.event_type_name.clone(),
                            type_args: Vec::new(),
                        },
                        span,
                    },
                    default: None,
                    span,
                },
            ],
            ret_ty_expr: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: "DeliverResult".to_string(),
                    type_args: Vec::new(),
                },
                span,
            },
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_rt",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_zero(
                        "__mc_rt",
                        "DeliverResult",
                        "InstanceNotFound",
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_result",
                        call_expr(
                            HOSTED_LINEAR_DELIVER_FN,
                            vec![
                                var_expr("__mc_rt", node_id_gen, span),
                                self_field_expr("_id", node_id_gen, span),
                                var_expr("key", node_id_gen, span),
                                int_expr(info.source_state_tag, node_id_gen, span),
                                int_expr(info.target_state_tag, node_id_gen, span),
                                int_expr(info.event_kind, node_id_gen, span),
                                payload0_expr.clone(),
                                payload1_expr.clone(),
                            ],
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                ],
                tail: Some(Box::new(build_machine_deliver_result_expr(
                    "__mc_result",
                    node_id_gen,
                ))),
            },
            span,
        },
        span,
    }
}

fn build_machine_wait_func(info: &MachineWaitInfo, node_id_gen: &mut NodeIdGen) -> FuncDef {
    let span = Span::default();
    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: info.fn_name.clone(),
            type_params: Vec::new(),
            params: vec![
                Param {
                    id: node_id_gen.new_id(),
                    ident: "self".to_string(),
                    mode: ParamMode::In,
                    typ: TypeExpr {
                        id: node_id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: info.handle_type_name.clone(),
                            type_args: Vec::new(),
                        },
                        span,
                    },
                    default: None,
                    span,
                },
                Param {
                    id: node_id_gen.new_id(),
                    ident: "instance".to_string(),
                    mode: ParamMode::In,
                    typ: TypeExpr {
                        id: node_id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: info.hosted_type_name.clone(),
                            type_args: Vec::new(),
                        },
                        span,
                    },
                    default: None,
                    span,
                },
            ],
            // Waiting on a hosted session is still a session operation, so the
            // helper returns the hosted state enum widened with `SessionError`.
            ret_ty_expr: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Union {
                    variants: vec![
                        TypeExpr {
                            id: node_id_gen.new_id(),
                            kind: TypeExprKind::Named {
                                ident: info.hosted_type_name.clone(),
                                type_args: Vec::new(),
                            },
                            span,
                        },
                        TypeExpr {
                            id: node_id_gen.new_id(),
                            kind: TypeExprKind::Named {
                                ident: "SessionError".to_string(),
                                type_args: Vec::new(),
                            },
                            span,
                        },
                    ],
                },
                span,
            },
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_runtime",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_zero(
                        "__mc_runtime",
                        "SessionError",
                        "InstanceNotFound",
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_key",
                        Expr {
                            id: node_id_gen.new_id(),
                            kind: ExprKind::StructField {
                                target: Box::new(var_expr("instance", node_id_gen, span)),
                                field: info.key_field_name.clone(),
                            },
                            span,
                        },
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Stmt(let_bind_stmt(
                        "__mc_tag",
                        call_expr(
                            HOSTED_LINEAR_WAIT_STATE_FN,
                            vec![
                                var_expr("__mc_runtime", node_id_gen, span),
                                Expr {
                                    id: node_id_gen.new_id(),
                                    kind: ExprKind::StructField {
                                        target: Box::new(var_expr("self", node_id_gen, span)),
                                        field: "_id".to_string(),
                                    },
                                    span,
                                },
                                var_expr("__mc_key", node_id_gen, span),
                                int_expr(info.source_state_tag, node_id_gen, span),
                            ],
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    BlockItem::Expr(return_enum_error_if_zero(
                        "__mc_tag",
                        "SessionError",
                        "InstanceNotFound",
                        node_id_gen,
                        span,
                    )),
                ],
                tail: Some(Box::new(build_machine_wait_state_expr(
                    info,
                    "__mc_tag",
                    "__mc_key",
                    node_id_gen,
                ))),
            },
            span,
        },
        span,
    }
}

fn u64_type_expr(node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
    TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Named {
            ident: "u64".to_string(),
            type_args: Vec::new(),
        },
        span,
    }
}

fn u64_param(name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Param {
    Param {
        id: node_id_gen.new_id(),
        ident: name.to_string(),
        typ: u64_type_expr(node_id_gen, span),
        mode: ParamMode::In,
        default: None,
        span,
    }
}

fn machine_send_result_union_type(node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
    TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Union {
            variants: vec![
                named_type_expr("()", node_id_gen, span),
                named_type_expr("MachineError", node_id_gen, span),
            ],
        },
        span,
    }
}

fn named_type_expr(name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
    TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Named {
            ident: name.to_string(),
            type_args: Vec::new(),
        },
        span,
    }
}

fn unit_expr(node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::UnitLit,
        span,
    }
}

fn var_expr(name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Var {
            ident: name.to_string(),
        },
        span,
    }
}

fn int_expr(value: u64, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::IntLit(value),
        span,
    }
}

fn self_field_expr(field: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::StructField {
            target: Box::new(var_expr("self", node_id_gen, span)),
            field: field.to_string(),
        },
        span,
    }
}

fn struct_field_expr(
    target_name: &str,
    field: &str,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::StructField {
            target: Box::new(var_expr(target_name, node_id_gen, span)),
            field: field.to_string(),
        },
        span,
    }
}

fn expr_field_expr(target: Expr, field: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::StructField {
            target: Box::new(target),
            field: field.to_string(),
        },
        span,
    }
}

fn call_expr(callee_name: &str, args: Vec<Expr>, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Call {
            callee: Box::new(var_expr(callee_name, node_id_gen, span)),
            args: args
                .into_iter()
                .map(|expr| CallArg {
                    label: None,
                    mode: CallArgMode::Default,
                    expr,
                    init: InitInfo::default(),
                    span,
                })
                .collect(),
        },
        span,
    }
}

fn build_hosted_linear_reply_trigger_kind_func(
    linear_index: &LinearIndex,
    machine_infos: &[MachineSpawnInfo],
    deliver_infos: &[MachineDeliverInfo],
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    let mut items = Vec::new();

    for info in linear_index.derived_interactions.values() {
        let Some(machine_info) = machine_infos
            .iter()
            .find(|machine| machine.machine_name == info.machine_name)
        else {
            continue;
        };
        let Some(host_info) = linear_index.machine_hosts.get(&info.machine_name) else {
            continue;
        };
        let Some(type_info) = linear_index.types.get(&info.hosted_type_name) else {
            continue;
        };
        let Some(waiting_state_index) = type_info
            .state_names
            .iter()
            .position(|state| state == &info.waiting_state)
        else {
            continue;
        };
        let waiting_state_tag = waiting_state_index as u64 + 1;

        for reply_type in &info.reply_types {
            let Some(reply_event_kind) = host_info.trigger_event_kinds.get(reply_type).copied()
            else {
                continue;
            };
            let Some(deliver_info) = deliver_infos.iter().find(|deliver| {
                deliver.handle_type_name == host_info.handle_type_name
                    && deliver.event_type_name == *reply_type
                    && deliver.source_state_tag == waiting_state_tag
            }) else {
                continue;
            };
            items.push(BlockItem::Expr(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::If {
                    cond: Box::new(and_expr(
                        and_expr(
                            eq_var_to_int(
                                "machine_kind",
                                machine_info.machine_kind,
                                node_id_gen,
                                span,
                            ),
                            eq_var_to_int("event_kind", reply_event_kind, node_id_gen, span),
                            node_id_gen,
                            span,
                        ),
                        eq_var_to_int("current_state_tag", waiting_state_tag, node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    then_body: Box::new(Expr {
                        id: node_id_gen.new_id(),
                        kind: ExprKind::Block {
                            items: vec![BlockItem::Stmt(StmtExpr {
                                id: node_id_gen.new_id(),
                                kind: StmtExprKind::Return {
                                    value: Some(Box::new(int_expr(
                                        deliver_info.event_kind,
                                        node_id_gen,
                                        span,
                                    ))),
                                },
                                span,
                            })],
                            tail: None,
                        },
                        span,
                    }),
                    else_body: Box::new(empty_block_expr(node_id_gen, span)),
                },
                span,
            }));
        }
    }

    FuncDef {
        id: node_id_gen.new_id(),
        doc: None,
        attrs: Vec::new(),
        sig: FunctionSig {
            name: HOSTED_LINEAR_REPLY_TRIGGER_KIND_FN.to_string(),
            type_params: Vec::new(),
            params: vec![
                u64_param("machine_kind", node_id_gen, span),
                u64_param("event_kind", node_id_gen, span),
                u64_param("current_state_tag", node_id_gen, span),
            ],
            ret_ty_expr: u64_type_expr(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items,
                tail: Some(Box::new(int_expr(0, node_id_gen, span))),
            },
            span,
        },
        span,
    }
}

fn payload_word_expr(
    payload_shape: &HostedOnPayloadShape,
    source_var_name: &str,
    word_index: usize,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    match (payload_shape, word_index) {
        (HostedOnPayloadShape::Empty, _) => int_expr(0, node_id_gen, span),
        (HostedOnPayloadShape::OneU64 { field0 }, 0) => {
            struct_field_expr(source_var_name, field0, node_id_gen, span)
        }
        (HostedOnPayloadShape::OneU64 { .. }, _) => int_expr(0, node_id_gen, span),
        (HostedOnPayloadShape::TwoU64 { field0, .. }, 0) => {
            struct_field_expr(source_var_name, field0, node_id_gen, span)
        }
        (HostedOnPayloadShape::TwoU64 { field1, .. }, 1) => {
            struct_field_expr(source_var_name, field1, node_id_gen, span)
        }
        (HostedOnPayloadShape::TwoU64 { .. }, _) => int_expr(0, node_id_gen, span),
    }
}

fn build_on_payload_expr(
    selector_type_name: &str,
    payload_shape: &HostedOnPayloadShape,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    let mut fields = Vec::new();
    match payload_shape {
        HostedOnPayloadShape::Empty => {}
        HostedOnPayloadShape::OneU64 { field0 } => fields.push(StructLitField {
            id: node_id_gen.new_id(),
            name: field0.clone(),
            value: var_expr("payload0", node_id_gen, span),
            span,
        }),
        HostedOnPayloadShape::TwoU64 { field0, field1 } => {
            fields.push(StructLitField {
                id: node_id_gen.new_id(),
                name: field0.clone(),
                value: var_expr("payload0", node_id_gen, span),
                span,
            });
            fields.push(StructLitField {
                id: node_id_gen.new_id(),
                name: field1.clone(),
                value: var_expr("payload1", node_id_gen, span),
                span,
            });
        }
    }
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::StructLit {
            name: selector_type_name.to_string(),
            type_args: Vec::new(),
            fields,
        },
        span,
    }
}

fn build_machine_handle_value(
    info: &MachineSpawnInfo,
    source_var_name: Option<&str>,
    machine_id_var_name: &str,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    let mut fields = vec![StructLitField {
        id: node_id_gen.new_id(),
        name: "_id".to_string(),
        value: var_expr(machine_id_var_name, node_id_gen, span),
        span,
    }];

    if let Some(source_var_name) = source_var_name {
        fields.extend(info.machine_fields.iter().map(|field| StructLitField {
            id: node_id_gen.new_id(),
            name: field.name.clone(),
            value: struct_field_expr(source_var_name, &field.name, node_id_gen, span),
            span,
        }));
    }

    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::StructLit {
            name: info.handle_type_name.clone(),
            type_args: Vec::new(),
            fields,
        },
        span,
    }
}

fn build_machine_handle_value_from_source(
    info: &MachineSpawnInfo,
    source_var_name: &str,
    machine_id_var_name: &str,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    build_machine_handle_value(
        info,
        Some(source_var_name),
        machine_id_var_name,
        node_id_gen,
        span,
    )
}

fn build_machine_config_bind_items(
    info: &MachineSpawnInfo,
    source_var_name: &str,
    runtime_var_name: &str,
    machine_id_var_name: &str,
    machine_field_layouts: &HashMap<String, Vec<StructDefField>>,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Vec<BlockItem> {
    info.machine_fields
        .iter()
        .enumerate()
        .flat_map(|(index, field)| {
            let status_var = format!("__mc_machine_cfg_status_{index}");
            let value_expr = machine_config_word_from_source_expr(
                field,
                source_var_name,
                machine_field_layouts,
                node_id_gen,
                span,
            );
            [
                BlockItem::Stmt(let_bind_stmt(
                    &status_var,
                    call_expr(
                        HOSTED_LINEAR_BIND_MACHINE_CONFIG_WORD_FN,
                        vec![
                            var_expr(runtime_var_name, node_id_gen, span),
                            var_expr(machine_id_var_name, node_id_gen, span),
                            int_expr(index as u64, node_id_gen, span),
                            value_expr,
                        ],
                        node_id_gen,
                        span,
                    ),
                    node_id_gen,
                    span,
                )),
                BlockItem::Expr(return_enum_error_if_zero(
                    &status_var,
                    "MachineError",
                    "SpawnFailed",
                    node_id_gen,
                    span,
                )),
            ]
        })
        .collect()
}

fn machine_config_word_from_source_expr(
    field: &StructDefField,
    source_var_name: &str,
    machine_field_layouts: &HashMap<String, Vec<StructDefField>>,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    match &field.ty.kind {
        TypeExprKind::Named { ident, type_args } if ident == "u64" && type_args.is_empty() => {
            struct_field_expr(source_var_name, &field.name, node_id_gen, span)
        }
        TypeExprKind::Named { ident, type_args }
            if type_args.is_empty() && machine_field_layouts.contains_key(ident) =>
        {
            expr_field_expr(
                struct_field_expr(source_var_name, &field.name, node_id_gen, span),
                "_id",
                node_id_gen,
                span,
            )
        }
        _ => panic!(
            "compiler bug: unsupported machine config field type `{}` on `{}`",
            field.ty, field.name
        ),
    }
}

fn machine_config_word_expr(
    runtime_var_name: &str,
    machine_id_expr: Expr,
    index: u64,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    call_expr(
        HOSTED_LINEAR_MACHINE_CONFIG_WORD_FN,
        vec![
            var_expr(runtime_var_name, node_id_gen, span),
            machine_id_expr,
            int_expr(index, node_id_gen, span),
        ],
        node_id_gen,
        span,
    )
}

fn build_machine_handle_from_runtime_expr(
    handle_type_name: &str,
    runtime_var_name: &str,
    machine_id_expr: Expr,
    machine_field_layouts: &HashMap<String, Vec<StructDefField>>,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    let mut fields = vec![StructLitField {
        id: node_id_gen.new_id(),
        name: "_id".to_string(),
        value: machine_id_expr.clone(),
        span,
    }];

    let machine_fields = machine_field_layouts
        .get(handle_type_name)
        .cloned()
        .unwrap_or_default();
    for (index, field) in machine_fields.iter().enumerate() {
        fields.push(StructLitField {
            id: node_id_gen.new_id(),
            name: field.name.clone(),
            value: build_machine_config_field_value_from_runtime_expr(
                field,
                runtime_var_name,
                machine_id_expr.clone(),
                index as u64,
                machine_field_layouts,
                node_id_gen,
                span,
            ),
            span,
        });
    }

    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::StructLit {
            name: handle_type_name.to_string(),
            type_args: Vec::new(),
            fields,
        },
        span,
    }
}

fn build_machine_config_field_value_from_runtime_expr(
    field: &StructDefField,
    runtime_var_name: &str,
    owner_machine_id_expr: Expr,
    index: u64,
    machine_field_layouts: &HashMap<String, Vec<StructDefField>>,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    match &field.ty.kind {
        TypeExprKind::Named { ident, type_args } if ident == "u64" && type_args.is_empty() => {
            machine_config_word_expr(
                runtime_var_name,
                owner_machine_id_expr,
                index,
                node_id_gen,
                span,
            )
        }
        TypeExprKind::Named { ident, type_args }
            if type_args.is_empty() && machine_field_layouts.contains_key(ident) =>
        {
            let nested_machine_id = machine_config_word_expr(
                runtime_var_name,
                owner_machine_id_expr,
                index,
                node_id_gen,
                span,
            );
            build_machine_handle_from_runtime_expr(
                ident,
                runtime_var_name,
                nested_machine_id,
                machine_field_layouts,
                node_id_gen,
                span,
            )
        }
        _ => panic!(
            "compiler bug: unsupported runtime-dispatched machine field type `{}` on `{}`",
            field.ty, field.name
        ),
    }
}

fn eq_var_to_int(name: &str, value: u64, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::BinOp {
            left: Box::new(var_expr(name, node_id_gen, span)),
            op: BinaryOp::Eq,
            right: Box::new(int_expr(value, node_id_gen, span)),
        },
        span,
    }
}

fn and_expr(left: Expr, right: Expr, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::BinOp {
            left: Box::new(left),
            op: BinaryOp::LogicalAnd,
            right: Box::new(right),
        },
        span,
    }
}

fn empty_block_expr(node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items: Vec::new(),
            tail: None,
        },
        span,
    }
}

fn let_bind_stmt(ident: &str, value: Expr, node_id_gen: &mut NodeIdGen, span: Span) -> StmtExpr {
    StmtExpr {
        id: node_id_gen.new_id(),
        kind: StmtExprKind::LetBind {
            pattern: BindPattern {
                id: node_id_gen.new_id(),
                kind: BindPatternKind::Name {
                    ident: ident.to_string(),
                },
                span,
            },
            decl_ty: None,
            value: Box::new(value),
        },
        span,
    }
}

fn return_enum_error_if_zero(
    value_var: &str,
    enum_name: &str,
    variant: &str,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::If {
            cond: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::BinOp {
                    left: Box::new(var_expr(value_var, node_id_gen, span)),
                    op: BinaryOp::Eq,
                    right: Box::new(int_expr(0, node_id_gen, span)),
                },
                span,
            }),
            then_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: vec![BlockItem::Stmt(StmtExpr {
                        id: node_id_gen.new_id(),
                        kind: StmtExprKind::Return {
                            value: Some(Box::new(Expr {
                                id: node_id_gen.new_id(),
                                kind: ExprKind::EnumVariant {
                                    enum_name: enum_name.to_string(),
                                    type_args: Vec::new(),
                                    variant: variant.to_string(),
                                    payload: Vec::new(),
                                },
                                span,
                            })),
                        },
                        span,
                    })],
                    tail: None,
                },
                span,
            }),
            else_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: Vec::new(),
                    tail: None,
                },
                span,
            }),
        },
        span,
    }
}

fn return_u64_if_zero(value_var: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::If {
            cond: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::BinOp {
                    left: Box::new(var_expr(value_var, node_id_gen, span)),
                    op: BinaryOp::Eq,
                    right: Box::new(int_expr(0, node_id_gen, span)),
                },
                span,
            }),
            then_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: vec![BlockItem::Stmt(StmtExpr {
                        id: node_id_gen.new_id(),
                        kind: StmtExprKind::Return {
                            value: Some(Box::new(int_expr(0, node_id_gen, span))),
                        },
                        span,
                    })],
                    tail: None,
                },
                span,
            }),
            else_body: Box::new(empty_block_expr(node_id_gen, span)),
        },
        span,
    }
}

fn return_enum_error_if_ne(
    value_var: &str,
    expected_value: u64,
    enum_name: &str,
    variant: &str,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::If {
            cond: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::BinOp {
                    left: Box::new(var_expr(value_var, node_id_gen, span)),
                    op: BinaryOp::Ne,
                    right: Box::new(int_expr(expected_value, node_id_gen, span)),
                },
                span,
            }),
            then_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: vec![BlockItem::Stmt(StmtExpr {
                        id: node_id_gen.new_id(),
                        kind: StmtExprKind::Return {
                            value: Some(Box::new(Expr {
                                id: node_id_gen.new_id(),
                                kind: ExprKind::EnumVariant {
                                    enum_name: enum_name.to_string(),
                                    type_args: Vec::new(),
                                    variant: variant.to_string(),
                                    payload: Vec::new(),
                                },
                                span,
                            })),
                        },
                        span,
                    })],
                    tail: None,
                },
                span,
            }),
            else_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: Vec::new(),
                    tail: None,
                },
                span,
            }),
        },
        span,
    }
}

fn return_enum_error_if_eq(
    value_var: &str,
    expected_value: u64,
    enum_name: &str,
    variant: &str,
    node_id_gen: &mut NodeIdGen,
    span: Span,
) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::If {
            cond: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::BinOp {
                    left: Box::new(var_expr(value_var, node_id_gen, span)),
                    op: BinaryOp::Eq,
                    right: Box::new(int_expr(expected_value, node_id_gen, span)),
                },
                span,
            }),
            then_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: vec![BlockItem::Stmt(StmtExpr {
                        id: node_id_gen.new_id(),
                        kind: StmtExprKind::Return {
                            value: Some(Box::new(Expr {
                                id: node_id_gen.new_id(),
                                kind: ExprKind::EnumVariant {
                                    enum_name: enum_name.to_string(),
                                    type_args: Vec::new(),
                                    variant: variant.to_string(),
                                    payload: Vec::new(),
                                },
                                span,
                            })),
                        },
                        span,
                    })],
                    tail: None,
                },
                span,
            }),
            else_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: Vec::new(),
                    tail: None,
                },
                span,
            }),
        },
        span,
    }
}

fn placeholder_shared_field_payload_with_key(
    fields: &[(String, TypeExpr)],
    key_field_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> Vec<Expr> {
    placeholder_shared_field_payload_with_key_var(fields, key_field_name, "__mc_key", node_id_gen)
}

fn placeholder_shared_field_payload_with_key_var(
    fields: &[(String, TypeExpr)],
    key_field_name: &str,
    key_var_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> Vec<Expr> {
    let span = Span::default();
    fields
        .iter()
        .map(|(name, ty)| {
            if name == key_field_name {
                var_expr(key_var_name, node_id_gen, span)
            } else {
                placeholder_expr_for_type(ty, node_id_gen)
            }
        })
        .collect()
}

fn build_machine_resume_state_expr(
    info: &MachineSpawnInfo,
    tag_var_name: &str,
    key_var_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> Expr {
    let span = Span::default();
    let mut expr = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::EnumVariant {
            enum_name: "SessionError".to_string(),
            variant: "InstanceNotFound".to_string(),
            type_args: Vec::new(),
            payload: Vec::new(),
        },
        span,
    };

    for (index, state_name) in info.state_names.iter().enumerate().rev() {
        let state_expr = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::EnumVariant {
                enum_name: info.hosted_type_name.clone(),
                variant: state_name.clone(),
                type_args: Vec::new(),
                payload: placeholder_shared_field_payload_with_key_var(
                    &info.shared_fields,
                    &info.key_field_name,
                    key_var_name,
                    node_id_gen,
                ),
            },
            span,
        };
        expr = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::If {
                cond: Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::BinOp {
                        left: Box::new(var_expr(tag_var_name, node_id_gen, span)),
                        op: BinaryOp::Eq,
                        right: Box::new(int_expr((index + 1) as u64, node_id_gen, span)),
                    },
                    span,
                }),
                then_body: Box::new(state_expr),
                else_body: Box::new(expr),
            },
            span,
        };
    }

    expr
}

fn build_machine_state_tag_expr(
    enum_name: &str,
    state_names: &[String],
    state_var_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> Expr {
    let span = Span::default();
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Match {
            scrutinee: Box::new(var_expr(state_var_name, node_id_gen, span)),
            arms: state_names
                .iter()
                .enumerate()
                .map(|(index, state_name)| MatchArm {
                    id: node_id_gen.new_id(),
                    patterns: vec![MatchPattern::EnumVariant {
                        id: node_id_gen.new_id(),
                        enum_name: Some(enum_name.to_string()),
                        type_args: Vec::new(),
                        variant_name: state_name.clone(),
                        bindings: vec![MatchPatternBinding::Wildcard { span }],
                        span,
                    }],
                    body: int_expr((index + 1) as u64, node_id_gen, span),
                    span,
                })
                .collect(),
        },
        span,
    }
}

fn build_machine_wait_state_expr(
    info: &MachineWaitInfo,
    tag_var_name: &str,
    key_var_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> Expr {
    let span = Span::default();
    let mut expr = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::EnumVariant {
            enum_name: "SessionError".to_string(),
            variant: "InstanceNotFound".to_string(),
            type_args: Vec::new(),
            payload: Vec::new(),
        },
        span,
    };

    for (index, state_name) in info.state_names.iter().enumerate().rev() {
        let state_expr = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::EnumVariant {
                enum_name: info.hosted_type_name.clone(),
                variant: state_name.clone(),
                type_args: Vec::new(),
                payload: placeholder_shared_field_payload_with_key_var(
                    &info.shared_fields,
                    &info.key_field_name,
                    key_var_name,
                    node_id_gen,
                ),
            },
            span,
        };
        expr = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::If {
                cond: Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::BinOp {
                        left: Box::new(var_expr(tag_var_name, node_id_gen, span)),
                        op: BinaryOp::Eq,
                        right: Box::new(int_expr((index + 1) as u64, node_id_gen, span)),
                    },
                    span,
                }),
                then_body: Box::new(state_expr),
                else_body: Box::new(expr),
            },
            span,
        };
    }

    expr
}

fn build_machine_action_commit_expr(
    info: &MachineActionSessionInfo,
    action_body: Expr,
    derived_interaction: Option<&DerivedInteractionLoweringInfo>,
    node_id_gen: &mut NodeIdGen,
) -> Expr {
    let span = Span::default();
    if matches!(&info.ret_ty_expr.kind, TypeExprKind::Union { .. }) {
        return build_machine_action_match_expr(
            info,
            action_body,
            derived_interaction,
            node_id_gen,
        );
    }
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items: vec![BlockItem::Stmt(let_bind_stmt(
                "__mc_ok",
                action_body,
                node_id_gen,
                span,
            ))],
            tail: Some(Box::new(build_machine_action_deliver_then_return(
                info,
                "__mc_ok",
                derived_interaction,
                node_id_gen,
            ))),
        },
        span,
    }
}

fn build_machine_action_match_expr(
    info: &MachineActionSessionInfo,
    action_body: Expr,
    derived_interaction: Option<&DerivedInteractionLoweringInfo>,
    node_id_gen: &mut NodeIdGen,
) -> Expr {
    let span = Span::default();
    let mut arms = vec![MatchArm {
        id: node_id_gen.new_id(),
        patterns: vec![MatchPattern::TypedBinding {
            id: node_id_gen.new_id(),
            ident: "__mc_ok".to_string(),
            ty_expr: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: info.hosted_type_name.clone(),
                    type_args: Vec::new(),
                },
                span,
            },
            span,
        }],
        body: build_machine_action_deliver_then_return(
            info,
            "__mc_ok",
            derived_interaction,
            node_id_gen,
        ),
        span,
    }];

    for (index, err_ty_expr) in machine_action_error_type_exprs(info, node_id_gen)
        .into_iter()
        .enumerate()
    {
        let err_name = format!("__mc_err_{index}");
        arms.push(MatchArm {
            id: node_id_gen.new_id(),
            patterns: vec![MatchPattern::TypedBinding {
                id: node_id_gen.new_id(),
                ident: err_name.clone(),
                ty_expr: err_ty_expr,
                span,
            }],
            body: Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: vec![BlockItem::Expr(call_expr(
                        HOSTED_ACTION_EMIT_ABORT_FN,
                        vec![var_expr("__mc_emit_scope", node_id_gen, span)],
                        node_id_gen,
                        span,
                    ))],
                    tail: Some(Box::new(var_expr(&err_name, node_id_gen, span))),
                },
                span,
            },
            span,
        });
    }

    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Match {
            scrutinee: Box::new(action_body),
            arms,
        },
        span,
    }
}

fn build_machine_action_deliver_then_return(
    info: &MachineActionSessionInfo,
    ok_var_name: &str,
    derived_interaction: Option<&DerivedInteractionLoweringInfo>,
    node_id_gen: &mut NodeIdGen,
) -> Expr {
    let span = Span::default();
    let mut items = vec![
        BlockItem::Stmt(let_bind_stmt(
            "__mc_result",
            call_expr(
                HOSTED_LINEAR_DELIVER_FN,
                vec![
                    var_expr("__mc_rt", node_id_gen, span),
                    self_field_expr("_id", node_id_gen, span),
                    struct_field_expr(
                        &info.instance_param_name,
                        &info.key_field_name,
                        node_id_gen,
                        span,
                    ),
                    int_expr(info.source_state_tag, node_id_gen, span),
                    int_expr(info.target_state_tag, node_id_gen, span),
                    int_expr(0, node_id_gen, span),
                    int_expr(0, node_id_gen, span),
                    int_expr(0, node_id_gen, span),
                ],
                node_id_gen,
                span,
            ),
            node_id_gen,
            span,
        )),
        BlockItem::Expr(return_enum_error_if_eq(
            "__mc_result",
            HOSTED_UPDATE_STALE,
            "SessionError",
            "InvalidState",
            node_id_gen,
            span,
        )),
        BlockItem::Expr(return_enum_error_if_ne(
            "__mc_result",
            HOSTED_UPDATE_OK,
            "SessionError",
            "InstanceNotFound",
            node_id_gen,
            span,
        )),
        BlockItem::Stmt(let_bind_stmt(
            "__mc_emit_committed",
            call_expr(
                HOSTED_ACTION_EMIT_COMMIT_FN,
                vec![var_expr("__mc_emit_scope", node_id_gen, span)],
                node_id_gen,
                span,
            ),
            node_id_gen,
            span,
        )),
        BlockItem::Expr(return_enum_error_if_zero(
            "__mc_emit_committed",
            "SessionError",
            "InstanceNotFound",
            node_id_gen,
            span,
        )),
    ];
    if let Some(derived_interaction) = derived_interaction {
        items.push(BlockItem::Stmt(let_bind_stmt(
            "__mc_interaction_id",
            call_expr(
                HOSTED_LINEAR_BIND_DERIVED_INTERACTION_FN,
                vec![
                    var_expr("__mc_rt", node_id_gen, span),
                    self_field_expr("_id", node_id_gen, span),
                    struct_field_expr(
                        &info.instance_param_name,
                        &info.key_field_name,
                        node_id_gen,
                        span,
                    ),
                    var_expr("__mc_emit_committed", node_id_gen, span),
                ],
                node_id_gen,
                span,
            ),
            node_id_gen,
            span,
        )));
        items.push(BlockItem::Expr(return_enum_error_if_zero(
            "__mc_interaction_id",
            "SessionError",
            "InstanceNotFound",
            node_id_gen,
            span,
        )));
        for reply_kind in &derived_interaction.reply_event_kinds {
            let allow_var_name = format!("__mc_reply_kind_allowed_{reply_kind}");
            items.push(BlockItem::Stmt(let_bind_stmt(
                &allow_var_name,
                call_expr(
                    HOSTED_LINEAR_ALLOW_REPLY_KIND_FN,
                    vec![
                        var_expr("__mc_rt", node_id_gen, span),
                        var_expr("__mc_emit_committed", node_id_gen, span),
                        int_expr(*reply_kind, node_id_gen, span),
                    ],
                    node_id_gen,
                    span,
                ),
                node_id_gen,
                span,
            )));
            items.push(BlockItem::Expr(return_enum_error_if_zero(
                &allow_var_name,
                "SessionError",
                "InstanceNotFound",
                node_id_gen,
                span,
            )));
        }
    }
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items,
            tail: Some(Box::new(var_expr(ok_var_name, node_id_gen, span))),
        },
        span,
    }
}

fn machine_action_error_type_exprs(
    info: &MachineActionSessionInfo,
    node_id_gen: &mut NodeIdGen,
) -> Vec<TypeExpr> {
    let span = Span::default();
    let mut out = Vec::new();
    let push_if_missing = |out: &mut Vec<TypeExpr>, ident: &str, node_id_gen: &mut NodeIdGen| {
        let already_present = out.iter().any(|ty| {
            matches!(
                &ty.kind,
                TypeExprKind::Named { ident: existing, type_args } if existing == ident && type_args.is_empty()
            )
        });
        if !already_present {
            out.push(TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: ident.to_string(),
                    type_args: Vec::new(),
                },
                span,
            });
        }
    };

    if let TypeExprKind::Union { variants } = &info.ret_ty_expr.kind {
        for variant in variants.iter().skip(1) {
            let TypeExprKind::Named { ident, type_args } = &variant.kind else {
                continue;
            };
            if !type_args.is_empty() {
                continue;
            }
            push_if_missing(&mut out, ident, node_id_gen);
        }
    }

    push_if_missing(&mut out, "SessionError", node_id_gen);
    out
}

fn build_machine_deliver_result_expr(result_var_name: &str, node_id_gen: &mut NodeIdGen) -> Expr {
    let span = Span::default();
    let delivered = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::EnumVariant {
            enum_name: "DeliverResult".to_string(),
            variant: "Delivered".to_string(),
            type_args: Vec::new(),
            payload: Vec::new(),
        },
        span,
    };
    let invalid_state = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::EnumVariant {
            enum_name: "DeliverResult".to_string(),
            variant: "InvalidState".to_string(),
            type_args: Vec::new(),
            payload: Vec::new(),
        },
        span,
    };
    let instance_not_found = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::EnumVariant {
            enum_name: "DeliverResult".to_string(),
            variant: "InstanceNotFound".to_string(),
            type_args: Vec::new(),
            payload: Vec::new(),
        },
        span,
    };

    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::If {
            cond: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::BinOp {
                    left: Box::new(var_expr(result_var_name, node_id_gen, span)),
                    op: BinaryOp::Eq,
                    right: Box::new(int_expr(HOSTED_UPDATE_OK, node_id_gen, span)),
                },
                span,
            }),
            then_body: Box::new(delivered),
            else_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::If {
                    cond: Box::new(Expr {
                        id: node_id_gen.new_id(),
                        kind: ExprKind::BinOp {
                            left: Box::new(var_expr(result_var_name, node_id_gen, span)),
                            op: BinaryOp::Eq,
                            right: Box::new(int_expr(HOSTED_UPDATE_STALE, node_id_gen, span)),
                        },
                        span,
                    }),
                    then_body: Box::new(invalid_state),
                    else_body: Box::new(instance_not_found),
                },
                span,
            }),
        },
        span,
    }
}

fn collect_base_action_impls(
    module: &Module,
) -> HashMap<(String, String, String), (TypeExpr, Expr)> {
    let type_defs = super::type_defs_by_name(module);
    let mut result = HashMap::new();
    for method_block in module.method_blocks() {
        let Some(type_def) = type_defs.get(&method_block.type_name) else {
            continue;
        };
        let TypeDefKind::Linear { linear } = &type_def.kind else {
            continue;
        };
        let mut action_counts = HashMap::<&str, usize>::new();
        for action in &linear.actions {
            *action_counts.entry(action.name.as_str()).or_default() += 1;
        }

        for item in &method_block.method_items {
            let MethodItem::Def(method) = item else {
                continue;
            };
            let source_state = if let Some(receiver_ty) = &method.sig.self_param.receiver_ty_expr {
                match &receiver_ty.kind {
                    TypeExprKind::Named { ident, type_args } if type_args.is_empty() => {
                        Some(ident.clone())
                    }
                    _ => None,
                }
            } else if action_counts
                .get(method.sig.name.as_str())
                .copied()
                .unwrap_or_default()
                == 1
            {
                linear
                    .actions
                    .iter()
                    .find(|action| action.name == method.sig.name)
                    .map(|action| action.source_state.clone())
            } else {
                None
            };
            let Some(source_state) = source_state else {
                continue;
            };
            result.insert(
                (
                    method_block.type_name.clone(),
                    source_state,
                    method.sig.name.clone(),
                ),
                (method.sig.ret_ty_expr.clone(), method.body.clone()),
            );
        }
    }
    result
}

fn rename_var_in_expr(mut expr: Expr, from: &str, to: &str) -> Expr {
    struct Renamer<'a> {
        from: &'a str,
        to: &'a str,
    }

    impl VisitorMut for Renamer<'_> {
        fn visit_expr(&mut self, expr: &mut Expr) {
            if let ExprKind::Var { ident } = &mut expr.kind
                && ident == self.from
            {
                *ident = self.to.to_string();
            }
            visit_mut::walk_expr(self, expr);
        }
    }

    let mut renamer = Renamer { from, to };
    renamer.visit_expr(&mut expr);
    expr
}

fn freshen_params(params: &[Param], node_id_gen: &mut NodeIdGen) -> Vec<Param> {
    params
        .iter()
        .cloned()
        .map(|mut param| {
            param.id = node_id_gen.new_id();
            param.typ = freshen_type_expr_ids(param.typ, node_id_gen);
            param
        })
        .collect()
}

fn freshen_expr_ids(mut expr: Expr, node_id_gen: &mut NodeIdGen) -> Expr {
    struct Freshener<'a> {
        node_id_gen: &'a mut NodeIdGen,
    }

    impl VisitorMut for Freshener<'_> {
        fn visit_expr(&mut self, expr: &mut Expr) {
            expr.id = self.node_id_gen.new_id();
            visit_mut::walk_expr(self, expr);
        }

        fn visit_stmt_expr(&mut self, stmt: &mut StmtExpr) {
            stmt.id = self.node_id_gen.new_id();
            visit_mut::walk_stmt_expr(self, stmt);
        }

        fn visit_bind_pattern(&mut self, pattern: &mut BindPattern) {
            pattern.id = self.node_id_gen.new_id();
            visit_mut::walk_bind_pattern(self, pattern);
        }

        fn visit_match_arm(&mut self, arm: &mut MatchArm) {
            arm.id = self.node_id_gen.new_id();
            visit_mut::walk_match_arm(self, arm);
        }

        fn visit_match_pattern(&mut self, pattern: &mut MatchPattern) {
            match pattern {
                MatchPattern::Binding { id, .. }
                | MatchPattern::TypedBinding { id, .. }
                | MatchPattern::EnumVariant { id, .. } => {
                    *id = self.node_id_gen.new_id();
                }
                _ => {}
            }
            visit_mut::walk_match_pattern(self, pattern);
        }

        fn visit_match_pattern_binding(&mut self, binding: &mut MatchPatternBinding) {
            if let MatchPatternBinding::Named { id, .. } = binding {
                *id = self.node_id_gen.new_id();
            }
            visit_mut::walk_match_pattern_binding(self, binding);
        }

        fn visit_type_expr(&mut self, ty_expr: &mut TypeExpr) {
            ty_expr.id = self.node_id_gen.new_id();
            visit_mut::walk_type_expr(self, ty_expr);
        }
    }

    let mut freshener = Freshener { node_id_gen };
    freshener.visit_expr(&mut expr);
    expr
}

fn freshen_type_expr_ids(mut ty_expr: TypeExpr, node_id_gen: &mut NodeIdGen) -> TypeExpr {
    struct TypeFreshener<'a> {
        node_id_gen: &'a mut NodeIdGen,
    }

    impl VisitorMut for TypeFreshener<'_> {
        fn visit_type_expr(&mut self, ty_expr: &mut TypeExpr) {
            ty_expr.id = self.node_id_gen.new_id();
            visit_mut::walk_type_expr(self, ty_expr);
        }
    }

    let mut freshener = TypeFreshener { node_id_gen };
    freshener.visit_type_expr(&mut ty_expr);
    ty_expr
}

fn placeholder_expr_for_type(ty: &TypeExpr, node_id_gen: &mut NodeIdGen) -> Expr {
    let span = Span::default();
    let kind = match &ty.kind {
        TypeExprKind::Named { ident, type_args } if type_args.is_empty() => match ident.as_str() {
            "bool" => ExprKind::BoolLit(false),
            "char" => ExprKind::CharLit('\0'),
            "string" => ExprKind::StringLit {
                value: String::new(),
            },
            _ if is_builtin_int_name(ident) => ExprKind::IntLit(0),
            _ => ExprKind::UnitLit,
        },
        _ => ExprKind::UnitLit,
    };
    Expr {
        id: node_id_gen.new_id(),
        kind,
        span,
    }
}

fn is_builtin_int_name(name: &str) -> bool {
    matches!(
        name,
        "u8" | "u16" | "u32" | "u64" | "usize" | "i8" | "i16" | "i32" | "i64" | "isize"
    )
}

fn widen_machine_override_return_type(
    ret_ty_expr: &TypeExpr,
    hosted_type_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> TypeExpr {
    let span = ret_ty_expr.span;
    match &ret_ty_expr.kind {
        TypeExprKind::Named { .. } => TypeExpr {
            id: node_id_gen.new_id(),
            kind: TypeExprKind::Union {
                variants: vec![
                    TypeExpr {
                        id: node_id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: hosted_type_name.to_string(),
                            type_args: Vec::new(),
                        },
                        span,
                    },
                    TypeExpr {
                        id: node_id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: "SessionError".to_string(),
                            type_args: Vec::new(),
                        },
                        span,
                    },
                ],
            },
            span,
        },
        TypeExprKind::Union { variants } => {
            // Linear action declarations use the first union variant as the
            // success/target state and the remaining variants as errors. The
            // generated machine helper widens that success arm to the hosted
            // enum type while preserving the declared error arms.
            let mut widened = Vec::with_capacity(variants.len() + 1);
            widened.push(TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: hosted_type_name.to_string(),
                    type_args: Vec::new(),
                },
                span,
            });
            widened.extend(variants.iter().skip(1).cloned());
            if !union_contains_named_type(variants, "SessionError") {
                widened.push(TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Named {
                        ident: "SessionError".to_string(),
                        type_args: Vec::new(),
                    },
                    span,
                });
            }
            TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Union { variants: widened },
                span,
            }
        }
        _ => ret_ty_expr.clone(),
    }
}

fn union_contains_named_type(variants: &[TypeExpr], name: &str) -> bool {
    variants.iter().any(|variant| {
        matches!(
            &variant.kind,
            TypeExprKind::Named { ident, type_args } if ident == name && type_args.is_empty()
        )
    })
}

// ── Call rewriting ──────────────────────────────────────────────────

/// Rewrite `PRService::spawn()` calls to `__mc_machine_spawn_PRService()`.
pub(super) fn rewrite_machine_spawn_calls(
    module: &mut Module,
    machine_infos: &[MachineSpawnInfo],
    node_id_gen: &mut NodeIdGen,
) {
    let spawn_fns = machine_infos
        .iter()
        .map(|info| (info.machine_name.clone(), info.spawn_fn_name.clone()))
        .collect::<HashMap<_, _>>();
    let mut rewriter = MachineSpawnCallRewriter {
        spawn_fns: &spawn_fns,
        node_id_gen,
    };
    rewriter.visit_module(module);
}

struct MachineSpawnCallRewriter<'a> {
    spawn_fns: &'a HashMap<String, String>,
    node_id_gen: &'a mut NodeIdGen,
}

impl VisitorMut for MachineSpawnCallRewriter<'_> {
    fn visit_expr(&mut self, expr: &mut Expr) {
        visit_mut::walk_expr(self, expr);

        // `PRService::spawn()` parses as `EnumVariant { enum_name: "PRService", variant: "spawn" }`.
        // Rewrite it to a plain function call to the generated spawn function.
        if let ExprKind::EnumVariant {
            enum_name,
            variant,
            payload,
            ..
        } = &mut expr.kind
            && variant == "spawn"
            && let Some(spawn_fn_name) = self.spawn_fns.get(enum_name)
        {
            let args = std::mem::take(payload)
                .into_iter()
                .map(|arg| CallArg {
                    label: None,
                    mode: CallArgMode::Default,
                    expr: arg,
                    init: InitInfo::default(),
                    span: expr.span,
                })
                .collect();
            expr.kind = ExprKind::Call {
                callee: Box::new(Expr {
                    id: self.node_id_gen.new_id(),
                    kind: ExprKind::Var {
                        ident: spawn_fn_name.clone(),
                    },
                    span: expr.span,
                }),
                args,
            };
        }
    }
}

// ── Self-type rewriting in machine constructors ─────────────────────

/// Rewrite `Self` type references and `Self {}` struct literals inside machine
/// constructor bodies to use the generated handle type name.
pub(super) fn rewrite_machine_constructor_self_types(
    module: &mut Module,
    machine_infos: &[MachineSpawnInfo],
) {
    let handle_types = machine_infos
        .iter()
        .map(|info| (info.machine_name.clone(), info.handle_type_name.clone()))
        .collect::<HashMap<_, _>>();
    let mut rewriter = MachineConstructorSelfRewriter {
        handle_types: &handle_types,
        current_handle_type: None,
    };
    rewriter.visit_module(module);
}

/// Rewrite source-level `Machine<HostedMachine>` handle annotations to the
/// generated concrete handle structs used by hosted linear lowering.
pub(super) fn rewrite_public_machine_handle_types(
    module: &mut Module,
    machine_infos: &[MachineSpawnInfo],
) {
    let handle_types = machine_infos
        .iter()
        .map(|info| (info.machine_name.clone(), info.handle_type_name.clone()))
        .collect::<HashMap<_, _>>();
    let mut rewriter = PublicMachineHandleRewriter {
        handle_types: &handle_types,
    };
    rewriter.visit_module(module);
}

struct MachineConstructorSelfRewriter<'a> {
    handle_types: &'a HashMap<String, String>,
    current_handle_type: Option<String>,
}

impl VisitorMut for MachineConstructorSelfRewriter<'_> {
    fn visit_machine_def(&mut self, machine_def: &mut MachineDef) {
        let prev = self.current_handle_type.clone();
        self.current_handle_type = self.handle_types.get(&machine_def.name).cloned();
        visit_mut::walk_machine_def(self, machine_def);
        self.current_handle_type = prev;
    }

    fn visit_func_def(&mut self, func_def: &mut FuncDef) {
        if self.current_handle_type.is_none() {
            visit_mut::walk_func_def(self, func_def);
            return;
        }
        visit_mut::walk_func_def(self, func_def);
    }

    fn visit_type_expr(&mut self, type_expr: &mut TypeExpr) {
        if let (Some(handle_type), TypeExprKind::Named { ident, type_args }) =
            (&self.current_handle_type, &mut type_expr.kind)
            && ident == "Self"
        {
            *ident = handle_type.clone();
            type_args.clear();
            return;
        }
        visit_mut::walk_type_expr(self, type_expr);
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        if let (
            Some(handle_type),
            ExprKind::StructLit {
                name,
                type_args,
                fields,
                ..
            },
        ) = (&self.current_handle_type, &mut expr.kind)
            && name == "Self"
        {
            *name = handle_type.clone();
            type_args.clear();
            if !fields.iter().any(|field| field.name == "_id") {
                // Placeholder: synthesize the runtime-managed machine id so
                // constructor `Self { ... }` literals remain valid before the
                // generated spawn helper replaces it with the actual slot id.
                fields.push(StructLitField {
                    id: expr.id,
                    name: "_id".to_string(),
                    value: Expr {
                        id: expr.id,
                        kind: ExprKind::IntLit(0),
                        span: expr.span,
                    },
                    span: expr.span,
                });
            }
        }
        visit_mut::walk_expr(self, expr);
    }
}

struct PublicMachineHandleRewriter<'a> {
    handle_types: &'a HashMap<String, String>,
}

impl VisitorMut for PublicMachineHandleRewriter<'_> {
    fn visit_stmt_expr(&mut self, stmt: &mut StmtExpr) {
        match &mut stmt.kind {
            StmtExprKind::LetBind {
                decl_ty: Some(decl_ty),
                ..
            }
            | StmtExprKind::VarBind {
                decl_ty: Some(decl_ty),
                ..
            } => self.visit_type_expr(decl_ty),
            StmtExprKind::VarDecl { decl_ty, .. } => self.visit_type_expr(decl_ty),
            _ => {}
        }
        visit_mut::walk_stmt_expr(self, stmt);
    }

    fn visit_type_expr(&mut self, type_expr: &mut TypeExpr) {
        if let TypeExprKind::Named { ident, type_args } = &mut type_expr.kind
            && ident == "Machine"
            && type_args.len() == 1
            && let Some(type_arg) = type_args.first()
            && let TypeExprKind::Named {
                ident: machine_name,
                type_args: machine_args,
            } = &type_arg.kind
            && machine_args.is_empty()
            && let Some(handle_type) = self.handle_types.get(machine_name)
        {
            *ident = handle_type.clone();
            type_args.clear();
            return;
        }
        visit_mut::walk_type_expr(self, type_expr);
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        if let ExprKind::StructLit {
            name, type_args, ..
        } = &mut expr.kind
            && name == "Machine"
            && type_args.len() == 1
            && let Some(type_arg) = type_args.first()
            && let TypeExprKind::Named {
                ident: machine_name,
                type_args: machine_args,
            } = &type_arg.kind
            && machine_args.is_empty()
            && let Some(handle_type) = self.handle_types.get(machine_name)
        {
            *name = handle_type.clone();
            type_args.clear();
            return;
        }
        visit_mut::walk_expr(self, expr);
    }
}
