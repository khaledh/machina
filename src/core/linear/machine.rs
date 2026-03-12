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
//! - **Self-type rewriting**: `Self` references in machine constructor bodies are
//!   rewritten to the generated handle type, so `Self {}` returns the right struct.
//!
//! All generated functions are currently placeholders — they return minimal valid
//! values. Real runtime-backed implementations will replace these bodies when
//! machine execution lands.

use std::collections::HashMap;

use crate::core::ast::visit_mut::{self, VisitorMut};
use crate::core::ast::{
    EnumDefVariant, Expr, ExprKind, FuncDef, MachineDef, MachineItem, Module, NodeIdGen, Param,
    ParamMode, TopLevelItem, TypeDef, TypeDefKind, TypeExpr, TypeExprKind,
};
use crate::core::diag::Span;

// ── Internal data structures ────────────────────────────────────────

#[derive(Clone, Debug)]
pub(super) struct MachineSpawnInfo {
    pub machine_name: String,
    pub hosted_type_name: String,
    pub initial_state: Option<String>,
    pub role_names: Vec<String>,
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
                items: Vec::new(),
                tail: Some(Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::StructLit {
                        name: info.handle_type_name.clone(),
                        type_args: Vec::new(),
                        fields: vec![crate::core::ast::StructLitField {
                            id: node_id_gen.new_id(),
                            name: "_id".to_string(),
                            value: Expr {
                                id: node_id_gen.new_id(),
                                kind: ExprKind::IntLit(1),
                                span,
                            },
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
        // Placeholder body: returns the initial state variant directly.
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: Vec::new(),
                tail: Some(Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::EnumVariant {
                        enum_name: info.hosted_type_name.clone(),
                        variant: initial_state.clone(),
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
        // return a deterministic state shape to keep the generated surface valid.
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
