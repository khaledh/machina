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

use crate::core::ast::visit_mut::{self, VisitorMut};
use crate::core::ast::{
    BindPattern, BindPatternKind, BlockItem, CallArg, CallArgMode, EnumDefVariant, Expr, ExprKind,
    FuncDecl, FuncDef, FunctionSig, MachineDef, MachineItem, MethodBlock, MethodDef, MethodItem,
    MethodSig, Module, NodeIdGen, Param, ParamMode, SelfParam, StmtExpr, StmtExprKind,
    StructLitField, TopLevelItem, TypeDef, TypeDefKind, TypeExpr, TypeExprKind,
};
use crate::core::diag::Span;

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
const MACHINE_RUNTIME_SEND_FN: &str = "__mc_machine_runtime_send_u64";
const HOSTED_LINEAR_SPAWN_FN: &str = "__mc_hosted_linear_spawn_u64";
const HOSTED_LINEAR_CREATE_FN: &str = "__mc_hosted_linear_create_u64";
const HOSTED_LINEAR_RESUME_STATE_FN: &str = "__mc_hosted_linear_resume_state_u64";
const HOSTED_LINEAR_DELIVER_FN: &str = "__mc_hosted_linear_deliver_u64";
const HOSTED_LINEAR_WAIT_STATE_FN: &str = "__mc_hosted_linear_wait_state_u64";
const HOSTED_LINEAR_ON_DISPATCH_FN: &str = "__mc_hosted_linear_on_dispatch_u64";
const HOSTED_LINEAR_TRIGGER_DISPATCH_FN: &str = "__mc_hosted_linear_trigger_dispatch_u64";
const DEFAULT_MACHINE_MAILBOX_CAP: u64 = 64;
const HOSTED_UPDATE_OK: u64 = 0;
const HOSTED_UPDATE_STALE: u64 = 1;
const HOSTED_LINEAR_ON_KIND_BASE: u64 = 1024;
const HOSTED_LINEAR_TRIGGER_KIND_BASE: u64 = 2048;

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
    let existing_callables: HashMap<String, ()> = module
        .top_level_items
        .iter()
        .filter_map(|item| match item {
            TopLevelItem::FuncDecl(decl) => Some((decl.sig.name.clone(), ())),
            TopLevelItem::FuncDef(def) => Some((def.sig.name.clone(), ())),
            _ => None,
        })
        .collect();

    let span = Span::default();
    let mut decls = Vec::new();

    let intrinsics = [
        (MANAGED_RUNTIME_CURRENT_FN, Vec::new()),
        (
            HOSTED_LINEAR_SPAWN_FN,
            vec![
                ("runtime", u64_type_expr(node_id_gen, span)),
                ("mailbox_cap", u64_type_expr(node_id_gen, span)),
                ("machine_kind", u64_type_expr(node_id_gen, span)),
            ],
        ),
        (
            MACHINE_RUNTIME_SEND_FN,
            vec![
                ("runtime", u64_type_expr(node_id_gen, span)),
                ("dst", u64_type_expr(node_id_gen, span)),
                ("kind", u64_type_expr(node_id_gen, span)),
                ("payload0", u64_type_expr(node_id_gen, span)),
                ("payload1", u64_type_expr(node_id_gen, span)),
            ],
        ),
        (
            HOSTED_LINEAR_CREATE_FN,
            vec![
                ("runtime", u64_type_expr(node_id_gen, span)),
                ("machine_id", u64_type_expr(node_id_gen, span)),
                ("initial_state_tag", u64_type_expr(node_id_gen, span)),
                ("initial_payload", u64_type_expr(node_id_gen, span)),
            ],
        ),
        (
            HOSTED_LINEAR_RESUME_STATE_FN,
            vec![
                ("runtime", u64_type_expr(node_id_gen, span)),
                ("machine_id", u64_type_expr(node_id_gen, span)),
                ("key", u64_type_expr(node_id_gen, span)),
            ],
        ),
        (
            HOSTED_LINEAR_DELIVER_FN,
            vec![
                ("runtime", u64_type_expr(node_id_gen, span)),
                ("machine_id", u64_type_expr(node_id_gen, span)),
                ("key", u64_type_expr(node_id_gen, span)),
                ("expected_state_tag", u64_type_expr(node_id_gen, span)),
                ("target_state_tag", u64_type_expr(node_id_gen, span)),
                ("trigger_kind", u64_type_expr(node_id_gen, span)),
                ("payload0", u64_type_expr(node_id_gen, span)),
                ("payload1", u64_type_expr(node_id_gen, span)),
            ],
        ),
        (
            HOSTED_LINEAR_WAIT_STATE_FN,
            vec![
                ("runtime", u64_type_expr(node_id_gen, span)),
                ("machine_id", u64_type_expr(node_id_gen, span)),
                ("key", u64_type_expr(node_id_gen, span)),
                ("expected_state_tag", u64_type_expr(node_id_gen, span)),
            ],
        ),
    ];

    for (name, params) in intrinsics {
        if existing_callables.contains_key(name) {
            continue;
        }
        let params = if params.is_empty() {
            Vec::new()
        } else {
            params
                .into_iter()
                .map(|(param_name, typ)| Param {
                    id: node_id_gen.new_id(),
                    ident: param_name.to_string(),
                    typ,
                    mode: ParamMode::In,
                    span,
                })
                .collect()
        };
        decls.push(TopLevelItem::FuncDecl(FuncDecl {
            id: node_id_gen.new_id(),
            attrs: Vec::new(),
            sig: FunctionSig {
                name: name.to_string(),
                type_params: Vec::new(),
                params,
                ret_ty_expr: u64_type_expr(node_id_gen, span),
                span,
            },
            span,
        }));
    }

    if !decls.is_empty() {
        module.top_level_items.splice(0..0, decls);
    }
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
    machine_infos: &[MachineSpawnInfo],
    action_override_infos: &[MachineActionOverrideInfo],
    action_session_infos: &[MachineActionSessionInfo],
    trigger_handler_infos: &[MachineTriggerHandlerInfo],
    on_handler_infos: &[MachineOnHandlerInfo],
    deliver_infos: &[MachineDeliverInfo],
    wait_infos: &[MachineWaitInfo],
    node_id_gen: &mut NodeIdGen,
) {
    for info in machine_infos {
        module.top_level_items.push(build_machine_handle_type_def(
            &info.handle_type_name,
            node_id_gen,
        ));
        if let Some(method_block) =
            build_machine_handle_send_method_block(info, on_handler_infos, node_id_gen)
        {
            module.top_level_items.push(method_block);
        }
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_spawn_func(
                info,
                on_handler_infos.iter().any(|handler| {
                    handler.machine_name == info.machine_name && handler.payload_shape.is_some()
                }),
                trigger_handler_infos.iter().any(|handler| {
                    handler.machine_name == info.machine_name && handler.payload_shape.is_some()
                }),
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
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_action_session_func(
                action_info,
                node_id_gen,
            )));
    }

    for trigger_info in trigger_handler_infos {
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_trigger_handler_func(
                trigger_info,
                node_id_gen,
            )));
        if trigger_info.payload_shape.is_some() {
            module.top_level_items.push(TopLevelItem::FuncDef(
                build_machine_trigger_dispatch_wrapper_func(trigger_info, node_id_gen),
            ));
        }
    }

    for on_info in on_handler_infos {
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_on_handler_func(
                on_info,
                node_id_gen,
            )));
        if on_info.payload_shape.is_some() {
            module.top_level_items.push(TopLevelItem::FuncDef(
                build_machine_on_dispatch_wrapper_func(on_info, node_id_gen),
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

fn build_machine_handle_type_def(type_name: &str, node_id_gen: &mut NodeIdGen) -> TopLevelItem {
    TopLevelItem::TypeDef(TypeDef {
        id: node_id_gen.new_id(),
        attrs: Vec::new(),
        name: type_name.to_string(),
        type_params: Vec::new(),
        kind: TypeDefKind::Struct {
            fields: vec![crate::core::ast::StructDefField {
                id: node_id_gen.new_id(),
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
            }],
        },
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
                        "__mc_self",
                        Expr {
                            id: node_id_gen.new_id(),
                            kind: ExprKind::StructLit {
                                name: info.handle_type_name.clone(),
                                type_args: Vec::new(),
                                fields: vec![StructLitField {
                                    id: node_id_gen.new_id(),
                                    name: "_id".to_string(),
                                    value: var_expr("machine_id", node_id_gen, span),
                                    span,
                                }],
                            },
                            span,
                        },
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
                        "__mc_self",
                        Expr {
                            id: node_id_gen.new_id(),
                            kind: ExprKind::StructLit {
                                name: info.handle_type_name.clone(),
                                type_args: Vec::new(),
                                fields: vec![StructLitField {
                                    id: node_id_gen.new_id(),
                                    name: "_id".to_string(),
                                    value: var_expr("machine_id", node_id_gen, span),
                                    span,
                                }],
                            },
                            span,
                        },
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
    has_on_dispatch: bool,
    has_trigger_dispatch: bool,
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
    if has_on_dispatch {
        items.push(BlockItem::Expr(call_expr(
            HOSTED_LINEAR_ON_DISPATCH_FN,
            vec![
                int_expr(0, node_id_gen, span),
                int_expr(0, node_id_gen, span),
                int_expr(0, node_id_gen, span),
                int_expr(0, node_id_gen, span),
                int_expr(0, node_id_gen, span),
            ],
            node_id_gen,
            span,
        )));
    }
    if has_trigger_dispatch {
        items.push(BlockItem::Expr(call_expr(
            HOSTED_LINEAR_TRIGGER_DISPATCH_FN,
            vec![
                int_expr(0, node_id_gen, span),
                int_expr(0, node_id_gen, span),
                int_expr(0, node_id_gen, span),
                int_expr(0, node_id_gen, span),
                int_expr(0, node_id_gen, span),
                int_expr(0, node_id_gen, span),
                int_expr(0, node_id_gen, span),
            ],
            node_id_gen,
            span,
        )));
    }

    FuncDef {
        id: node_id_gen.new_id(),
        attrs: Vec::new(),
        sig: crate::core::ast::FunctionSig {
            name: info.spawn_fn_name.clone(),
            type_params: Vec::new(),
            params: Vec::new(),
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
                tail: Some(Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::StructLit {
                        name: info.handle_type_name.clone(),
                        type_args: Vec::new(),
                        fields: vec![crate::core::ast::StructLitField {
                            id: node_id_gen.new_id(),
                            name: "_id".to_string(),
                            value: var_expr("__mc_machine_id", node_id_gen, span),
                            span,
                        }],
                    },
                    span,
                })),
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
        attrs: Vec::new(),
        sig: crate::core::ast::FunctionSig {
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
        attrs: Vec::new(),
        sig: crate::core::ast::FunctionSig {
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
                    span,
                },
                Param {
                    id: node_id_gen.new_id(),
                    ident: "key".to_string(),
                    mode: ParamMode::In,
                    typ: info.key_ty.clone(),
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

fn build_machine_action_override_func(
    info: &MachineActionOverrideInfo,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    FuncDef {
        id: node_id_gen.new_id(),
        attrs: Vec::new(),
        sig: crate::core::ast::FunctionSig {
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
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    let span = Span::default();
    let session_params = freshen_params(&info.params, node_id_gen);
    let action_body = freshen_expr_ids(info.body.clone(), node_id_gen);
    FuncDef {
        id: node_id_gen.new_id(),
        attrs: Vec::new(),
        sig: crate::core::ast::FunctionSig {
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
                ],
                tail: Some(Box::new(build_machine_action_commit_expr(
                    info,
                    action_body,
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
        attrs: Vec::new(),
        sig: crate::core::ast::FunctionSig {
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
        attrs: Vec::new(),
        sig: crate::core::ast::FunctionSig {
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
        attrs: Vec::new(),
        sig: crate::core::ast::FunctionSig {
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
                    span,
                },
                Param {
                    id: node_id_gen.new_id(),
                    ident: "key".to_string(),
                    mode: ParamMode::In,
                    typ: info.key_ty.clone(),
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
        attrs: Vec::new(),
        sig: crate::core::ast::FunctionSig {
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

fn call_expr(callee_name: &str, args: Vec<Expr>, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Call {
            callee: Box::new(var_expr(callee_name, node_id_gen, span)),
            args: args
                .into_iter()
                .map(|expr| CallArg {
                    mode: CallArgMode::Default,
                    expr,
                    init: crate::core::ast::InitInfo::default(),
                    span,
                })
                .collect(),
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

fn eq_var_to_int(name: &str, value: u64, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::BinOp {
            left: Box::new(var_expr(name, node_id_gen, span)),
            op: crate::core::ast::BinaryOp::Eq,
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
            op: crate::core::ast::BinaryOp::LogicalAnd,
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
                    op: crate::core::ast::BinaryOp::Eq,
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
                    op: crate::core::ast::BinaryOp::Ne,
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
                    op: crate::core::ast::BinaryOp::Eq,
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
                        op: crate::core::ast::BinaryOp::Eq,
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
                .map(|(index, state_name)| crate::core::ast::MatchArm {
                    id: node_id_gen.new_id(),
                    pattern: crate::core::ast::MatchPattern::EnumVariant {
                        id: node_id_gen.new_id(),
                        enum_name: Some(enum_name.to_string()),
                        type_args: Vec::new(),
                        variant_name: state_name.clone(),
                        bindings: vec![crate::core::ast::MatchPatternBinding::Wildcard { span }],
                        span,
                    },
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
                        op: crate::core::ast::BinaryOp::Eq,
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
    node_id_gen: &mut NodeIdGen,
) -> Expr {
    let span = Span::default();
    let ok_value = if matches!(&info.ret_ty_expr.kind, TypeExprKind::Union { .. }) {
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Try {
                fallible_expr: Box::new(action_body),
                on_error: None,
            },
            span,
        }
    } else {
        action_body
    };
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items: vec![BlockItem::Stmt(let_bind_stmt(
                "__mc_ok",
                ok_value,
                node_id_gen,
                span,
            ))],
            tail: Some(Box::new(build_machine_action_deliver_then_return(
                info,
                "__mc_ok",
                node_id_gen,
            ))),
        },
        span,
    }
}

fn build_machine_action_deliver_then_return(
    info: &MachineActionSessionInfo,
    ok_var_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> Expr {
    let span = Span::default();
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items: vec![
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
            ],
            tail: Some(Box::new(var_expr(ok_var_name, node_id_gen, span))),
        },
        span,
    }
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
                    op: crate::core::ast::BinaryOp::Eq,
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
                            op: crate::core::ast::BinaryOp::Eq,
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
            let crate::core::ast::MethodItem::Def(method) = item else {
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

        fn visit_match_arm(&mut self, arm: &mut crate::core::ast::MatchArm) {
            arm.id = self.node_id_gen.new_id();
            visit_mut::walk_match_arm(self, arm);
        }

        fn visit_match_pattern(&mut self, pattern: &mut crate::core::ast::MatchPattern) {
            match pattern {
                crate::core::ast::MatchPattern::Binding { id, .. }
                | crate::core::ast::MatchPattern::TypedBinding { id, .. }
                | crate::core::ast::MatchPattern::EnumVariant { id, .. } => {
                    *id = self.node_id_gen.new_id();
                }
                _ => {}
            }
            visit_mut::walk_match_pattern(self, pattern);
        }

        fn visit_match_pattern_binding(
            &mut self,
            binding: &mut crate::core::ast::MatchPatternBinding,
        ) {
            if let crate::core::ast::MatchPatternBinding::Named { id, .. } = binding {
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
                .map(|arg| crate::core::ast::CallArg {
                    mode: crate::core::ast::CallArgMode::Default,
                    expr: arg,
                    init: crate::core::ast::InitInfo::default(),
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
            if fields.is_empty() {
                // Placeholder: synthesize the minimal handle field so `Self {}`
                // returns a valid struct before real machine allocation lands.
                fields.push(crate::core::ast::StructLitField {
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
