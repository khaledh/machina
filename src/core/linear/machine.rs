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
//!   surface for hosted `session.wait()`. These currently return a placeholder
//!   next state until runtime-backed waiter registration lands.
//!
//! - **Self-type rewriting**: `Self` references in machine constructor bodies are
//!   rewritten to the generated handle type, so `Self {}` returns the right struct.
//!
//! All generated functions are currently placeholders — they return minimal valid
//! values. Real runtime-backed implementations will replace these bodies when
//! machine execution lands.

use std::collections::HashMap;

use crate::core::ast::visit_mut::{self, VisitorMut};
use crate::core::ast::{
    BindPattern, BindPatternKind, BlockItem, CallArg, CallArgMode, EnumDefVariant, Expr, ExprKind,
    FuncDecl, FuncDef, FunctionSig, MachineDef, MachineItem, Module, NodeIdGen, Param, ParamMode,
    StmtExpr, StmtExprKind, TopLevelItem, TypeDef, TypeDefKind, TypeExpr, TypeExprKind,
};
use crate::core::diag::Span;

// ── Internal data structures ────────────────────────────────────────

#[derive(Clone, Debug)]
pub(super) struct MachineSpawnInfo {
    pub machine_name: String,
    pub hosted_type_name: String,
    pub initial_state: Option<String>,
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
pub(super) struct MachineTriggerHandlerInfo {
    pub hosted_type_name: String,
    pub handle_type_name: String,
    pub instance_param_name: String,
    pub params: Vec<Param>,
    pub body: Expr,
    pub fn_name: String,
}

#[derive(Clone, Debug)]
pub(super) struct MachineOnHandlerInfo {
    pub handle_type_name: String,
    pub params: Vec<Param>,
    pub body: Expr,
    pub fn_name: String,
}

#[derive(Clone, Debug)]
pub(super) struct MachineDeliverInfo {
    pub handle_type_name: String,
    pub key_ty: TypeExpr,
    pub event_type_name: String,
    pub fn_name: String,
}

#[derive(Clone, Debug)]
pub(super) struct MachineWaitInfo {
    pub hosted_type_name: String,
    pub handle_type_name: String,
    pub shared_field_tys: Vec<TypeExpr>,
    pub placeholder_target_state: String,
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

pub(crate) fn machine_trigger_handler_fn_name(machine_name: &str, trigger_name: &str) -> String {
    format!("__mc_machine_trigger_{machine_name}_{trigger_name}")
}

pub(crate) fn machine_on_handler_fn_name(machine_name: &str, handler_index: usize) -> String {
    format!("__mc_machine_on_{machine_name}_{handler_index}")
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
const HOSTED_LINEAR_SPAWN_FN: &str = "__mc_hosted_linear_spawn_u64";
const HOSTED_LINEAR_CREATE_FN: &str = "__mc_hosted_linear_create_u64";
const DEFAULT_MACHINE_MAILBOX_CAP: u64 = 64;

// ── Collection ──────────────────────────────────────────────────────

pub(super) fn collect_machine_spawn_infos(module: &Module) -> Vec<MachineSpawnInfo> {
    let type_defs = super::type_defs_by_name(module);
    module
        .machine_defs()
        .into_iter()
        .filter_map(|machine_def| {
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
                hosted_type_name: machine_def.host.type_name.clone(),
                initial_state: linear.states.first().map(|state| state.name.clone()),
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

pub(super) fn collect_machine_trigger_handler_infos(
    module: &Module,
) -> Vec<MachineTriggerHandlerInfo> {
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
            let triggers_by_name = linear
                .triggers
                .iter()
                .map(|trigger| (trigger.name.as_str(), trigger))
                .collect::<HashMap<_, _>>();
            let handle_type_name = machine_handle_type_name(&machine_def.name);
            machine_def
                .items
                .iter()
                .filter_map(|item| {
                    let MachineItem::Trigger(handler) = item else {
                        return None;
                    };
                    let _ = triggers_by_name.get(handler.name.as_str())?;
                    Some(MachineTriggerHandlerInfo {
                        hosted_type_name: machine_def.host.type_name.clone(),
                        handle_type_name: handle_type_name.clone(),
                        instance_param_name: handler.instance_param.clone(),
                        params: handler.params.clone(),
                        body: handler.body.clone(),
                        fn_name: machine_trigger_handler_fn_name(&machine_def.name, &handler.name),
                    })
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub(super) fn collect_machine_on_handler_infos(module: &Module) -> Vec<MachineOnHandlerInfo> {
    module
        .machine_defs()
        .into_iter()
        .flat_map(|machine_def| {
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
                    Some(MachineOnHandlerInfo {
                        handle_type_name: handle_type_name.clone(),
                        params,
                        body: handler.body.clone(),
                        fn_name: machine_on_handler_fn_name(&machine_def.name, index),
                    })
                })
                .collect::<Vec<_>>()
        })
        .collect()
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
            linear
                .triggers
                .iter()
                .map(|trigger| MachineDeliverInfo {
                    handle_type_name: handle_type_name.clone(),
                    key_ty: key_field.ty.clone(),
                    event_type_name: trigger.name.clone(),
                    fn_name: machine_deliver_fn_name(&machine_def.name, &trigger.name),
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
            let shared_field_tys = linear
                .fields
                .iter()
                .map(|field| field.ty.clone())
                .collect::<Vec<_>>();
            let mut waits = Vec::new();
            let mut seen_states = HashMap::<String, String>::new();
            for trigger in &linear.triggers {
                seen_states
                    .entry(trigger.source_state.clone())
                    .or_insert_with(|| trigger.target_state.clone());
            }
            for (source_state, fallback_target_state) in seen_states {
                waits.push(MachineWaitInfo {
                    hosted_type_name: machine_def.host.type_name.clone(),
                    handle_type_name: handle_type_name.clone(),
                    shared_field_tys: shared_field_tys.clone(),
                    placeholder_target_state: fallback_target_state,
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
                variants: vec![EnumDefVariant {
                    id: node_id_gen.new_id(),
                    name: "SpawnFailed".to_string(),
                    payload: Vec::new(),
                    span: Span::default(),
                }],
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
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_spawn_func(
                info,
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

    for trigger_info in trigger_handler_infos {
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_trigger_handler_func(
                trigger_info,
                node_id_gen,
            )));
    }

    for on_info in on_handler_infos {
        module
            .top_level_items
            .push(TopLevelItem::FuncDef(build_machine_on_handler_func(
                on_info,
                node_id_gen,
            )));
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

/// Generate: `fn __mc_machine_spawn_X() -> HandleType | MachineError { HandleType { _id: 1 } }`
fn build_machine_spawn_func(info: &MachineSpawnInfo, node_id_gen: &mut NodeIdGen) -> FuncDef {
    let span = Span::default();
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
                        "SpawnFailed",
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
                ],
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
    let fallback_state = info
        .initial_state
        .as_ref()
        .expect("hosted linear types must have an initial state");
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
        // Placeholder body: resume semantics are not runtime-backed yet, so we
        // return a deterministic state shape to keep the generated surface
        // valid. Shared fields use the same placeholder seeding as create.
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: Vec::new(),
                tail: Some(Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::EnumVariant {
                        enum_name: info.hosted_type_name.clone(),
                        variant: fallback_state.clone(),
                        type_args: Vec::new(),
                        payload: placeholder_shared_field_payload(
                            info.shared_fields.iter().map(|(_, ty)| ty),
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
        // Placeholder body: the typed surface exists now, but instance-backed
        // delivery semantics will land with the runtime work.
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: Vec::new(),
                tail: Some(Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::EnumVariant {
                        enum_name: "DeliverResult".to_string(),
                        variant: "Delivered".to_string(),
                        type_args: Vec::new(),
                        payload: Vec::new(),
                    },
                    span,
                })),
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
        // Placeholder body: waiter registration and wakeups are a later
        // runtime slice, so for now we return one valid trigger target shape.
        // Shared fields use placeholder seeding so the helper remains
        // well-typed for linear types with carried fields like `id`.
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: Vec::new(),
                tail: Some(Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::EnumVariant {
                        enum_name: info.hosted_type_name.clone(),
                        variant: info.placeholder_target_state.clone(),
                        type_args: Vec::new(),
                        payload: placeholder_shared_field_payload(
                            info.shared_field_tys.iter(),
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

fn placeholder_shared_field_payload<'a>(
    tys: impl IntoIterator<Item = &'a TypeExpr>,
    node_id_gen: &mut NodeIdGen,
) -> Vec<Expr> {
    tys.into_iter()
        .map(|ty| placeholder_expr_for_type(ty, node_id_gen))
        .collect()
}

fn placeholder_shared_field_payload_with_key(
    fields: &[(String, TypeExpr)],
    key_field_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> Vec<Expr> {
    let span = Span::default();
    fields
        .iter()
        .map(|(name, ty)| {
            if name == key_field_name {
                var_expr("__mc_key", node_id_gen, span)
            } else {
                placeholder_expr_for_type(ty, node_id_gen)
            }
        })
        .collect()
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
            widened.push(TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: "SessionError".to_string(),
                    type_args: Vec::new(),
                },
                span,
            });
            TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Union { variants: widened },
                span,
            }
        }
        _ => ret_ty_expr.clone(),
    }
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
