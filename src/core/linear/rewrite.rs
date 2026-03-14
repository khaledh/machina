//! Direct-mode lowering for `@linear type` declarations.
//!
//! Transforms linear types into forms the rest of the compiler already handles:
//!
//! - **Type defs**: `@linear type Door = { states { Closed, Open }, ... }`
//!   becomes `type Door = Closed | Open` (an ordinary enum).
//!
//! - **Method blocks**: action methods like `fn open(self) -> Open { ... }` are
//!   renamed to internal names (e.g., `__linear__Closed__open`), their receiver
//!   becomes a sink parameter, and their return type is rewritten to the parent
//!   enum type.
//!
//! - **Expressions**: action calls like `door.open()` are rewritten to use the
//!   internal method name, with state tracking through an environment that maps
//!   bindings to their current linear state. This enables:
//!   - Correct method dispatch based on the caller's known state
//!   - Use-after-consume detection (calling an action on an already-consumed binding)
//!   - Hosted provenance tracking (bindings from `create(...)` carry their
//!     originating machine, so hosted action calls can either become fallible
//!     base session actions or dispatch through machine override helpers)

use std::collections::{HashMap, HashSet};

use crate::core::ast::{
    ArrayLitInit, BlockItem, Expr, ExprKind, MethodItem, Module, NodeIdGen, ParamMode, StmtExpr,
    StmtExprKind, TopLevelItem, TypeDefKind, TypeExpr, TypeExprKind,
};
use crate::core::diag::Span;
use crate::core::resolve::{REK, ResolveError};

use super::index::{HostedActionExprInfo, LinearIndex};
use super::machine::{machine_lookup_fn_name, machine_spawn_fn_name};

// -- Internal data structures ----------------------------------------

/// Per-linear-type info collected before rewriting. Contains the data needed
/// to rewrite method names, track states, and dispatch actions.
#[derive(Clone, Debug)]
pub(super) struct DirectLinearInfo {
    pub type_name: String,
    /// Shared top-level fields carried by every state variant, in declaration order.
    pub shared_fields: Vec<DirectSharedFieldInfo>,
    pub state_names: HashSet<String>,
    pub initial_state: Option<String>,
    pub role_names: HashSet<String>,
    /// Maps (source_state, action_name) → internal method name + target state.
    pub action_by_source_and_name: HashMap<(String, String), DirectActionInfo>,
    /// Maps trigger name → source/target state names from the declaration.
    pub triggers_by_name: HashMap<String, DirectTriggerInfo>,
}

#[derive(Clone, Debug)]
pub(super) struct DirectActionInfo {
    pub internal_name: String,
    pub target_state: String,
}

#[derive(Clone, Debug)]
pub(super) struct DirectSharedFieldInfo {
    pub name: String,
    pub ty: TypeExpr,
}

#[derive(Clone, Debug)]
pub(super) struct DirectTriggerInfo {
    pub source_state: String,
    pub target_state: String,
}

/// Tracks the linear type and current state of a value flowing through
/// expression rewriting.
#[derive(Clone, Debug)]
struct LinearValueState {
    type_name: String,
    state_name: String,
}

/// Per-binding state in the rewriting environment. Tracks whether the binding
/// has been consumed (for use-after-consume detection) and whether it originated
/// from a hosted `create(...)` call (for hosted action fallibility).
#[derive(Clone, Debug)]
struct LinearBindingState {
    value_state: LinearValueState,
    consumed: bool,
    /// Hosted provenance for session values created from `machine.create(...)`.
    /// We keep the producing machine name and the local handle binding name so
    /// hosted action calls can lower to machine override helpers when one
    /// exists.
    hosted: Option<HostedProvenance>,
}

#[derive(Clone, Debug)]
struct HostedProvenance {
    machine_name: String,
    handle_binding: String,
    role_name: String,
}

#[derive(Clone, Debug)]
struct MachineBindingState {
    machine_name: String,
}

// -- Collection ------------------------------------------------------

/// Collect per-linear-type rewriting info from the module's type definitions.
pub(super) fn collect_direct_linear_infos(module: &Module) -> HashMap<String, DirectLinearInfo> {
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
        let shared_fields = linear
            .fields
            .iter()
            .map(|field| DirectSharedFieldInfo {
                name: field.name.clone(),
                ty: field.ty.clone(),
            })
            .collect::<Vec<_>>();
        let initial_state = linear.states.first().map(|state| state.name.clone());
        let role_names = linear
            .roles
            .iter()
            .map(|role| role.name.clone())
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
        let triggers_by_name = linear
            .triggers
            .iter()
            .map(|trigger| {
                (
                    trigger.name.clone(),
                    DirectTriggerInfo {
                        source_state: trigger.source_state.clone(),
                        target_state: trigger.target_state.clone(),
                    },
                )
            })
            .collect::<HashMap<_, _>>();

        infos.insert(
            type_def.name.clone(),
            DirectLinearInfo {
                type_name: type_def.name.clone(),
                shared_fields,
                state_names,
                initial_state,
                role_names,
                action_by_source_and_name,
                triggers_by_name,
            },
        );
    }
    infos
}

// -- Type def rewriting ----------------------------------------------

/// Rewrite `@linear type` definitions into ordinary enum definitions.
/// States become enum variants.
pub(super) fn rewrite_linear_type_defs(
    module: &mut Module,
    infos: &HashMap<String, DirectLinearInfo>,
) {
    for type_def in &mut module.top_level_items {
        let TopLevelItem::TypeDef(type_def) = type_def else {
            continue;
        };
        let Some(_info) = infos.get(&type_def.name) else {
            continue;
        };
        let TypeDefKind::Linear { linear } = &type_def.kind else {
            continue;
        };
        let shared_payload_tys = _info
            .shared_fields
            .iter()
            .map(|field| field.ty.clone())
            .collect::<Vec<_>>();
        type_def.kind = TypeDefKind::Enum {
            variants: linear
                .states
                .iter()
                .map(|state| crate::core::ast::EnumDefVariant {
                    id: state.id,
                    name: state.name.clone(),
                    payload: shared_payload_tys
                        .iter()
                        .cloned()
                        .chain(state.payload.clone())
                        .collect(),
                    span: state.span,
                })
                .collect(),
        };
    }
}

// -- Method block rewriting ------------------------------------------

/// Rewrite method blocks for linear types: rename action methods to their
/// internal names, change receiver to sink mode, and rewrite bodies.
pub(super) fn rewrite_linear_method_blocks(
    module: &mut Module,
    infos: &HashMap<String, DirectLinearInfo>,
    linear_index: &mut LinearIndex,
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
                let _ = rewrite_expr_with_linear_env(
                    &mut method.body,
                    infos,
                    linear_index,
                    Some(&info.type_name),
                    Some("self"),
                    Some(&source_state),
                    &[],
                    &[],
                    node_id_gen,
                );
            } else {
                let _ = rewrite_expr_with_linear_env(
                    &mut method.body,
                    infos,
                    linear_index,
                    Some(&info.type_name),
                    None,
                    None,
                    &[],
                    &[],
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

/// Determine which source state a method implements, either from an explicit
/// receiver annotation (`self: Draft`) or by finding a unique action match.
fn method_source_state(
    sig: &crate::core::ast::MethodSig,
    info: &DirectLinearInfo,
) -> Option<String> {
    if let Some(receiver_ty) = sig.self_param.receiver_ty_expr.as_ref()
        && let Some(name) = named_type_name(receiver_ty)
    {
        return Some(name);
    }

    // No explicit receiver — look for a unique action with this method name.
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

/// Rewrite bare state names in return types to the parent enum type.
/// e.g., `-> Open` becomes `-> Door` when `Open` is a state of `Door`.
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

// -- Expression rewriting --------------------------------------------

/// Rewrite expressions in function bodies and non-linear method blocks.
pub(super) fn rewrite_linear_exprs(
    module: &mut Module,
    infos: &HashMap<String, DirectLinearInfo>,
    linear_index: &mut LinearIndex,
    node_id_gen: &mut NodeIdGen,
) -> Vec<ResolveError> {
    let mut errors = Vec::new();
    for item in &mut module.top_level_items {
        match item {
            TopLevelItem::FuncDef(func) => {
                let override_info = linear_index
                    .action_override_fns
                    .get(&func.sig.name)
                    .cloned();
                let session_info = linear_index.action_session_fns.get(&func.sig.name).cloned();
                let trigger_info = linear_index
                    .trigger_handler_fns
                    .get(&func.sig.name)
                    .cloned();
                let seeded_bindings = if let Some(override_info) = &override_info {
                    vec![(
                        override_info.instance_param_name.clone(),
                        LinearBindingState {
                            value_state: LinearValueState {
                                type_name: override_info.hosted_type_name.clone(),
                                state_name: override_info.source_state.clone(),
                            },
                            consumed: false,
                            hosted: None,
                        },
                    )]
                } else if let Some(session_info) = &session_info {
                    vec![(
                        session_info.instance_param_name.clone(),
                        LinearBindingState {
                            value_state: LinearValueState {
                                type_name: session_info.hosted_type_name.clone(),
                                state_name: session_info.source_state.clone(),
                            },
                            consumed: false,
                            hosted: None,
                        },
                    )]
                } else if let Some(trigger_info) = &trigger_info {
                    vec![(
                        trigger_info.instance_param_name.clone(),
                        LinearBindingState {
                            value_state: LinearValueState {
                                type_name: trigger_info.hosted_type_name.clone(),
                                state_name: trigger_info.source_state.clone(),
                            },
                            consumed: false,
                            hosted: None,
                        },
                    )]
                } else {
                    Vec::new()
                };
                let seeded_machine_bindings =
                    collect_machine_param_bindings(&func.sig.params, linear_index);
                let current_linear_type = override_info
                    .as_ref()
                    .map(|override_info| override_info.hosted_type_name.as_str())
                    .or_else(|| {
                        session_info
                            .as_ref()
                            .map(|session_info| session_info.hosted_type_name.as_str())
                    })
                    .or_else(|| {
                        trigger_info
                            .as_ref()
                            .map(|trigger_info| trigger_info.hosted_type_name.as_str())
                    });
                let (rewrite_errors, result_state) = rewrite_expr_with_linear_env(
                    &mut func.body,
                    infos,
                    linear_index,
                    current_linear_type,
                    if override_info.is_some() {
                        override_info
                            .as_ref()
                            .map(|info| info.instance_param_name.as_str())
                    } else if session_info.is_some() {
                        session_info
                            .as_ref()
                            .map(|info| info.instance_param_name.as_str())
                    } else {
                        trigger_info
                            .as_ref()
                            .map(|info| info.instance_param_name.as_str())
                    },
                    None,
                    &seeded_bindings,
                    &seeded_machine_bindings,
                    node_id_gen,
                );
                errors.extend(rewrite_errors);

                if let (Some(trigger_info), Some(result_state)) = (&trigger_info, result_state) {
                    if result_state.value_state.state_name != trigger_info.target_state {
                        errors.push(
                            REK::MachineHandlerTypeMismatch(
                                trigger_info.machine_name.clone(),
                                "trigger",
                                func.sig.name.clone(),
                            )
                            .at(func.body.span),
                        );
                    }
                }
            }
            TopLevelItem::MethodBlock(method_block) => {
                if infos.contains_key(&method_block.type_name) {
                    continue;
                }
                for method_item in &mut method_block.method_items {
                    if let MethodItem::Def(method) = method_item {
                        let (rewrite_errors, _) = rewrite_expr_with_linear_env(
                            &mut method.body,
                            infos,
                            linear_index,
                            None,
                            None,
                            None,
                            &[],
                            &collect_machine_param_bindings(&method.sig.params, linear_index),
                            node_id_gen,
                        );
                        errors.extend(rewrite_errors);
                    }
                }
            }
            TopLevelItem::MachineDef(machine_def) => {
                let Some(info) = infos.get(&machine_def.host.type_name) else {
                    continue;
                };

                for machine_item in &mut machine_def.items {
                    match machine_item {
                        crate::core::ast::MachineItem::Action(handler) => {
                            let Some(source_state) =
                                unique_machine_action_source_state(info, &handler.name)
                            else {
                                continue;
                            };

                            let (rewrite_errors, _) = rewrite_expr_with_linear_env(
                                &mut handler.body,
                                infos,
                                linear_index,
                                Some(&info.type_name),
                                Some(&handler.instance_param),
                                None,
                                &[(
                                    handler.instance_param.clone(),
                                    LinearBindingState {
                                        value_state: LinearValueState {
                                            type_name: info.type_name.clone(),
                                            state_name: source_state,
                                        },
                                        consumed: false,
                                        hosted: None,
                                    },
                                )],
                                &[],
                                node_id_gen,
                            );
                            errors.extend(rewrite_errors);
                        }
                        crate::core::ast::MachineItem::Trigger(handler) => {
                            let Some(trigger) = info.triggers_by_name.get(&handler.name) else {
                                continue;
                            };

                            let (rewrite_errors, result_state) = rewrite_expr_with_linear_env(
                                &mut handler.body,
                                infos,
                                linear_index,
                                Some(&info.type_name),
                                Some(&handler.instance_param),
                                None,
                                &[(
                                    handler.instance_param.clone(),
                                    LinearBindingState {
                                        value_state: LinearValueState {
                                            type_name: info.type_name.clone(),
                                            state_name: trigger.source_state.clone(),
                                        },
                                        consumed: false,
                                        hosted: None,
                                    },
                                )],
                                &[],
                                node_id_gen,
                            );
                            errors.extend(rewrite_errors);

                            if let Some(result_state) = result_state
                                && result_state.value_state.state_name != trigger.target_state
                            {
                                errors.push(
                                    REK::MachineHandlerTypeMismatch(
                                        machine_def.name.clone(),
                                        "trigger",
                                        handler.name.clone(),
                                    )
                                    .at(handler.body.span),
                                );
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    errors
}

fn collect_machine_param_bindings(
    params: &[crate::core::ast::Param],
    linear_index: &LinearIndex,
) -> Vec<(String, MachineBindingState)> {
    params
        .iter()
        .filter_map(|param| {
            named_type_name(&param.typ).and_then(|ty_name| {
                linear_index
                    .machine_hosts
                    .iter()
                    .find_map(|(machine_name, host)| {
                        (host.handle_type_name == ty_name).then(|| {
                            (
                                param.ident.clone(),
                                MachineBindingState {
                                    machine_name: machine_name.clone(),
                                },
                            )
                        })
                    })
            })
        })
        .collect()
}

/// Set up the binding environment and rewrite a single expression tree.
fn rewrite_expr_with_linear_env(
    expr: &mut Expr,
    infos: &HashMap<String, DirectLinearInfo>,
    linear_index: &mut LinearIndex,
    current_linear_type: Option<&str>,
    carry_binding_name: Option<&str>,
    self_state: Option<&str>,
    seeded_bindings: &[(String, LinearBindingState)],
    seeded_machine_bindings: &[(String, MachineBindingState)],
    node_id_gen: &mut NodeIdGen,
) -> (Vec<ResolveError>, Option<LinearBindingState>) {
    let mut env = HashMap::new();
    let mut machine_env = HashMap::new();
    let mut errors = Vec::new();
    if let (Some(type_name), Some(binding_name), Some(state_name)) =
        (current_linear_type, carry_binding_name, self_state)
    {
        env.insert(
            binding_name.to_string(),
            LinearBindingState {
                value_state: LinearValueState {
                    type_name: type_name.to_string(),
                    state_name: state_name.to_string(),
                },
                consumed: false,
                hosted: None,
            },
        );
    }
    for (name, binding_state) in seeded_bindings {
        env.insert(name.clone(), binding_state.clone());
    }
    for (name, machine_state) in seeded_machine_bindings {
        machine_env.insert(name.clone(), machine_state.clone());
    }
    let result_state = rewrite_expr_in_scope(
        expr,
        infos,
        linear_index,
        current_linear_type,
        &mut env,
        &mut machine_env,
        &mut errors,
        node_id_gen,
    );
    (errors, result_state)
}

/// Recursively rewrite an expression, tracking linear binding states through
/// the environment. Returns the linear state of the expression's result (if any).
fn rewrite_expr_in_scope(
    expr: &mut Expr,
    infos: &HashMap<String, DirectLinearInfo>,
    linear_index: &mut LinearIndex,
    current_linear_type: Option<&str>,
    env: &mut HashMap<String, LinearBindingState>,
    machine_env: &mut HashMap<String, MachineBindingState>,
    errors: &mut Vec<ResolveError>,
    node_id_gen: &mut NodeIdGen,
) -> Option<LinearBindingState> {
    match &mut expr.kind {
        ExprKind::Block { items, tail } => {
            let mut scope_env = env.clone();
            let mut scope_machine_env = machine_env.clone();
            for item in items {
                match item {
                    BlockItem::Stmt(stmt) => rewrite_stmt_in_scope(
                        stmt,
                        infos,
                        linear_index,
                        current_linear_type,
                        &mut scope_env,
                        &mut scope_machine_env,
                        errors,
                        node_id_gen,
                    ),
                    BlockItem::Expr(expr) => {
                        let _ = rewrite_expr_in_scope(
                            expr,
                            infos,
                            linear_index,
                            current_linear_type,
                            &mut scope_env,
                            &mut scope_machine_env,
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
                    linear_index,
                    current_linear_type,
                    &mut scope_env,
                    &mut scope_machine_env,
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
            let callee_state = rewrite_expr_in_scope(
                callee,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            for arg in args.iter_mut() {
                let _ = rewrite_expr_in_scope(
                    &mut arg.expr,
                    infos,
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
                    errors,
                    node_id_gen,
                );
            }

            // Hosted `create(Type as Role)` — seed the binding as hosted with
            // the initial state so downstream action calls get hosted treatment.
            if method_name == "create"
                && args.len() == 1
                && let ExprKind::RoleProjection {
                    type_name,
                    role_name,
                } = &args[0].expr.kind
                && let Some(info) = infos.get(type_name)
                && info.role_names.contains(role_name)
                && let Some(initial_state) = info.initial_state.clone()
                && let Some(machine_ident) = callee_source_ident.clone()
                && let Some(machine_binding) = machine_env.get(&machine_ident)
            {
                return Some(LinearBindingState {
                    value_state: LinearValueState {
                        type_name: type_name.clone(),
                        state_name: initial_state,
                    },
                    consumed: false,
                    hosted: Some(HostedProvenance {
                        machine_name: machine_binding.machine_name.clone(),
                        handle_binding: machine_ident,
                        role_name: role_name.clone(),
                    }),
                });
            }

            // Hosted `resume(Type as Role, key)` seeds hosted provenance even
            // though the exact state is only known later at runtime. We use an
            // empty state name as an "unknown until refined" marker so direct
            // action dispatch still fails unless control flow narrows the value
            // (for example via a `match` arm).
            if method_name == "resume"
                && args.len() == 2
                && let ExprKind::RoleProjection {
                    type_name,
                    role_name,
                } = &args[0].expr.kind
                && let Some(info) = infos.get(type_name)
                && info.role_names.contains(role_name)
                && let Some(machine_ident) = callee_source_ident.clone()
                && let Some(machine_binding) = machine_env.get(&machine_ident)
            {
                return Some(LinearBindingState {
                    value_state: LinearValueState {
                        type_name: type_name.clone(),
                        state_name: String::new(),
                    },
                    consumed: false,
                    hosted: Some(HostedProvenance {
                        machine_name: machine_binding.machine_name.clone(),
                        handle_binding: machine_ident,
                        role_name: role_name.clone(),
                    }),
                });
            }

            // Hosted `wait()` lowers to a generated helper tied to the source
            // machine. We only expose it on hosted bindings in states that
            // actually have trigger-driven successors; all other cases should
            // keep reporting `wait` as unavailable.
            if method_name == "wait" && args.is_empty() {
                let Some(callee_state) = callee_state.as_ref() else {
                    return None;
                };
                if let Some(source_ident) = callee_source_ident.clone()
                    && let Some(binding) = env.get_mut(&source_ident)
                {
                    binding.consumed = true;
                }

                if let Some(hosted) = &callee_state.hosted
                    && let Some(host_info) = linear_index.machine_hosts.get(&hosted.machine_name)
                    && let Some(wait_fn_name) = host_info
                        .wait_helpers
                        .get(&callee_state.value_state.state_name)
                {
                    expr.kind = ExprKind::Call {
                        callee: Box::new(Expr {
                            id: node_id_gen.new_id(),
                            kind: ExprKind::Var {
                                ident: wait_fn_name.clone(),
                            },
                            span: expr.span,
                        }),
                        args: vec![
                            crate::core::ast::CallArg {
                                mode: crate::core::ast::CallArgMode::Default,
                                expr: Expr {
                                    id: node_id_gen.new_id(),
                                    kind: ExprKind::Var {
                                        ident: hosted.handle_binding.clone(),
                                    },
                                    span: expr.span,
                                },
                                init: crate::core::ast::InitInfo::default(),
                                span: expr.span,
                            },
                            crate::core::ast::CallArg {
                                mode: crate::core::ast::CallArgMode::Default,
                                expr: (**callee).clone(),
                                init: crate::core::ast::InitInfo::default(),
                                span: expr.span,
                            },
                        ],
                    };
                }
                // `wait()` resumes as one of several possible states, so we
                // intentionally stop tracking a single concrete linear state.
                return None;
            }

            if method_name == "lookup"
                && args.len() == 2
                && let ExprKind::Var { ident: type_name } = &args[0].expr.kind
                && let Some(machine_ident) = callee_source_ident.clone()
                && let Some(machine_binding) = machine_env.get(&machine_ident)
                && let Some(host_info) = linear_index
                    .machine_hosts
                    .get(&machine_binding.machine_name)
                && host_info.hosted_type_name == *type_name
            {
                let helper_name = machine_lookup_fn_name(&machine_binding.machine_name, type_name);
                let key_expr = args[1].expr.clone();
                expr.kind = ExprKind::Call {
                    callee: Box::new(Expr {
                        id: node_id_gen.new_id(),
                        kind: ExprKind::Var { ident: helper_name },
                        span: expr.span,
                    }),
                    args: vec![
                        crate::core::ast::CallArg {
                            mode: crate::core::ast::CallArgMode::Default,
                            expr: (**callee).clone(),
                            init: crate::core::ast::InitInfo::default(),
                            span: expr.span,
                        },
                        crate::core::ast::CallArg {
                            mode: crate::core::ast::CallArgMode::Default,
                            expr: key_expr,
                            init: crate::core::ast::InitInfo::default(),
                            span: expr.span,
                        },
                    ],
                };
                return None;
            }

            // Direct-mode action dispatch: look up the action by the callee's
            // current state and the method name.
            let Some(callee_state) = callee_state else {
                return None;
            };
            let Some(info) = infos.get(&callee_state.value_state.type_name) else {
                return None;
            };
            let action_name = method_name.clone();
            let Some(action) = info.action_by_source_and_name.get(&(
                callee_state.value_state.state_name.clone(),
                action_name.clone(),
            )) else {
                return None;
            };
            *method_name = action.internal_name.clone();
            if let Some(source_ident) = callee_source_ident
                && let Some(binding) = env.get_mut(&source_ident)
            {
                binding.consumed = true;
            }

            if let Some(hosted) = &callee_state.hosted
                && let Some(host_info) = linear_index.machine_hosts.get(&hosted.machine_name)
                && let Some(helper_fn_name) = host_info.action_helpers.get(&(
                    callee_state.value_state.state_name.clone(),
                    action_name.clone(),
                ))
            {
                let mut helper_args = Vec::with_capacity(args.len() + 2);
                helper_args.push(crate::core::ast::CallArg {
                    mode: crate::core::ast::CallArgMode::Default,
                    expr: Expr {
                        id: node_id_gen.new_id(),
                        kind: ExprKind::Var {
                            ident: hosted.handle_binding.clone(),
                        },
                        span: expr.span,
                    },
                    init: crate::core::ast::InitInfo::default(),
                    span: expr.span,
                });
                helper_args.push(crate::core::ast::CallArg {
                    mode: crate::core::ast::CallArgMode::Default,
                    expr: (**callee).clone(),
                    init: crate::core::ast::InitInfo::default(),
                    span: expr.span,
                });
                helper_args.extend(args.iter().cloned());
                expr.kind = ExprKind::Call {
                    callee: Box::new(Expr {
                        id: node_id_gen.new_id(),
                        kind: ExprKind::Var {
                            ident: helper_fn_name.clone(),
                        },
                        span: expr.span,
                    }),
                    args: helper_args,
                };
                linear_index.hosted_action_exprs.insert(
                    expr.id,
                    HostedActionExprInfo {
                        type_name: info.type_name.clone(),
                        role_name: hosted.role_name.clone(),
                        source_state: callee_state.value_state.state_name.clone(),
                        action_name,
                        runtime_arg_prefix: 2,
                    },
                );
            } else if let Some(hosted) = &callee_state.hosted {
                linear_index.hosted_action_exprs.insert(
                    expr.id,
                    HostedActionExprInfo {
                        type_name: info.type_name.clone(),
                        role_name: hosted.role_name.clone(),
                        source_state: callee_state.value_state.state_name.clone(),
                        action_name,
                        runtime_arg_prefix: 0,
                    },
                );
            }

            // Propagate hosted provenance through the action chain.
            Some(LinearBindingState {
                value_state: LinearValueState {
                    type_name: info.type_name.clone(),
                    state_name: action.target_state.clone(),
                },
                consumed: false,
                hosted: callee_state.hosted,
            })
        }
        ExprKind::StructLit { name, fields, .. } => {
            for field in fields.iter_mut() {
                let _ = rewrite_expr_in_scope(
                    &mut field.value,
                    infos,
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
                    errors,
                    node_id_gen,
                );
            }
            // Qualified state construction: `Door::Closed {}` → enum variant.
            if let Some((type_name, state_name)) = parse_qualified_linear_state_name(name, infos) {
                let Some(info) = infos.get(&type_name) else {
                    return None;
                };
                let payload = linear_state_payload_from_fields(
                    fields,
                    info,
                    carry_binding_name(env),
                    expr.span,
                    node_id_gen,
                )?;
                expr.kind = ExprKind::EnumVariant {
                    enum_name: type_name.clone(),
                    type_args: Vec::new(),
                    variant: state_name.clone(),
                    payload,
                };
                return Some(LinearBindingState {
                    value_state: LinearValueState {
                        type_name,
                        state_name,
                    },
                    consumed: false,
                    hosted: None,
                });
            }

            // Unqualified state construction inside a method body: `Closed {}`.
            let Some(type_name) = current_linear_type else {
                return None;
            };
            let Some(info) = infos.get(type_name) else {
                return None;
            };
            if info.state_names.contains(name) {
                let state_name = name.clone();
                let payload = linear_state_payload_from_fields(
                    fields,
                    info,
                    carry_binding_name(env),
                    expr.span,
                    node_id_gen,
                )?;
                expr.kind = ExprKind::EnumVariant {
                    enum_name: type_name.to_string(),
                    type_args: Vec::new(),
                    variant: state_name.clone(),
                    payload,
                };
                return Some(LinearBindingState {
                    value_state: LinearValueState {
                        type_name: type_name.to_string(),
                        state_name,
                    },
                    consumed: false,
                    hosted: None,
                });
            }
            None
        }
        ExprKind::Call { callee, args } => {
            let _ = rewrite_expr_in_scope(
                callee,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            for arg in args.iter_mut() {
                let _ = rewrite_expr_in_scope(
                    &mut arg.expr,
                    infos,
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
                    errors,
                    node_id_gen,
                );
            }

            // State construction with payload: `Locked(code)` inside a method body.
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
            if !info.shared_fields.is_empty() {
                // Shared-field-bearing states need struct-literal construction
                // so the carried field values can be supplied explicitly (and
                // later auto-propagated from `self`).
                return None;
            }

            let payload = args.iter().map(|arg| arg.expr.clone()).collect::<Vec<_>>();
            expr.kind = ExprKind::EnumVariant {
                enum_name: type_name.to_string(),
                type_args: Vec::new(),
                variant: state_name.clone(),
                payload,
            };
            Some(LinearBindingState {
                value_state: LinearValueState {
                    type_name: type_name.to_string(),
                    state_name,
                },
                consumed: false,
                hosted: None,
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
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
                    errors,
                    node_id_gen,
                );
            }
            if infos.contains_key(enum_name) {
                return Some(LinearBindingState {
                    value_state: LinearValueState {
                        type_name: enum_name.clone(),
                        state_name: variant.clone(),
                    },
                    consumed: false,
                    hosted: None,
                });
            }
            None
        }
        // `?` propagates the linear state of the inner expression, so
        // `service.create(...)? ` correctly seeds the binding.
        ExprKind::Try {
            fallible_expr,
            on_error,
        } => {
            let result_state = rewrite_expr_in_scope(
                fallible_expr,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            if let Some(on_error) = on_error {
                let _ = rewrite_expr_in_scope(
                    on_error,
                    infos,
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
                    errors,
                    node_id_gen,
                );
            }
            result_state
        }
        ExprKind::RoleProjection { .. } => None,
        ExprKind::Match { scrutinee, arms } => {
            let scrutinee_ident = match &scrutinee.kind {
                ExprKind::Var { ident } => Some(ident.clone()),
                _ => None,
            };
            let scrutinee_state = rewrite_expr_in_scope(
                scrutinee,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            for arm in arms {
                let mut arm_env = env.clone();
                let mut arm_machine_env = machine_env.clone();
                if let (Some(scrutinee_ident), Some(scrutinee_state)) =
                    (scrutinee_ident.as_ref(), scrutinee_state.as_ref())
                {
                    refine_match_scrutinee_binding(
                        &arm.pattern,
                        scrutinee_ident,
                        scrutinee_state,
                        infos,
                        &mut arm_env,
                    );
                }
                let _ = rewrite_expr_in_scope(
                    &mut arm.body,
                    infos,
                    linear_index,
                    current_linear_type,
                    &mut arm_env,
                    &mut arm_machine_env,
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
            let _ = rewrite_expr_in_scope(
                cond,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            let mut then_env = env.clone();
            let mut else_env = env.clone();
            let mut then_machine_env = machine_env.clone();
            let mut else_machine_env = machine_env.clone();
            let _ = rewrite_expr_in_scope(
                then_body,
                infos,
                linear_index,
                current_linear_type,
                &mut then_env,
                &mut then_machine_env,
                errors,
                node_id_gen,
            );
            let _ = rewrite_expr_in_scope(
                else_body,
                infos,
                linear_index,
                current_linear_type,
                &mut else_env,
                &mut else_machine_env,
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
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
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
                            linear_index,
                            current_linear_type,
                            env,
                            machine_env,
                            errors,
                            node_id_gen,
                        );
                    }
                }
                ArrayLitInit::Repeat(value, _) => {
                    let _ = rewrite_expr_in_scope(
                        value,
                        infos,
                        linear_index,
                        current_linear_type,
                        env,
                        machine_env,
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
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
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
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
                    errors,
                    node_id_gen,
                );
                let _ = rewrite_expr_in_scope(
                    &mut entry.value,
                    infos,
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
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
            if binding.consumed {
                errors.push(REK::LinearUseAfterConsume(source_ident).at(expr.span));
            }
            Some(binding.clone())
        }
        _ => {
            walk_child_exprs(
                expr,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            None
        }
    }
}

// -- Statement rewriting ---------------------------------------------

fn rewrite_stmt_in_scope(
    stmt: &mut StmtExpr,
    infos: &HashMap<String, DirectLinearInfo>,
    linear_index: &mut LinearIndex,
    current_linear_type: Option<&str>,
    env: &mut HashMap<String, LinearBindingState>,
    machine_env: &mut HashMap<String, MachineBindingState>,
    errors: &mut Vec<ResolveError>,
    node_id_gen: &mut NodeIdGen,
) {
    match &mut stmt.kind {
        StmtExprKind::LetBind { pattern, value, .. }
        | StmtExprKind::VarBind { pattern, value, .. } => {
            let result_state = rewrite_expr_in_scope(
                value,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            if let Some(ident) = bind_pattern_name_mut(pattern) {
                let source_ident = ident.clone();
                if let Some(result_state) = result_state {
                    env.insert(source_ident, result_state);
                    machine_env.remove(ident);
                } else if let Some(machine_name) = machine_spawn_machine_name(value, linear_index) {
                    machine_env.insert(source_ident.clone(), MachineBindingState { machine_name });
                    env.remove(ident);
                } else {
                    env.remove(&source_ident);
                    machine_env.remove(ident);
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
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            let _ = rewrite_expr_in_scope(
                value,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
        }
        StmtExprKind::While { cond, body } => {
            let _ = rewrite_expr_in_scope(
                cond,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            let mut body_env = env.clone();
            let mut body_machine_env = machine_env.clone();
            let _ = rewrite_expr_in_scope(
                body,
                infos,
                linear_index,
                current_linear_type,
                &mut body_env,
                &mut body_machine_env,
                errors,
                node_id_gen,
            );
        }
        StmtExprKind::For { iter, body, .. } => {
            let _ = rewrite_expr_in_scope(
                iter,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            let mut body_env = env.clone();
            let mut body_machine_env = machine_env.clone();
            let _ = rewrite_expr_in_scope(
                body,
                infos,
                linear_index,
                current_linear_type,
                &mut body_env,
                &mut body_machine_env,
                errors,
                node_id_gen,
            );
        }
        StmtExprKind::Defer { value } => {
            let _ = rewrite_expr_in_scope(
                value,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
        }
        StmtExprKind::Using {
            value,
            body,
            binding,
        } => {
            let result_state = rewrite_expr_in_scope(
                value,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            let mut body_env = env.clone();
            let mut body_machine_env = machine_env.clone();
            if let Some(result_state) = result_state {
                body_env.insert(binding.ident.clone(), result_state);
            }
            let _ = rewrite_expr_in_scope(
                body,
                infos,
                linear_index,
                current_linear_type,
                &mut body_env,
                &mut body_machine_env,
                errors,
                node_id_gen,
            );
        }
        StmtExprKind::Return { value } => {
            if let Some(value) = value {
                let _ = rewrite_expr_in_scope(
                    value,
                    infos,
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
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

// -- Child expression walking ----------------------------------------
//
// For expression kinds that don't produce linear state themselves but may
// contain sub-expressions that do. Delegates back to `rewrite_expr_in_scope`.

fn walk_child_exprs(
    expr: &mut Expr,
    infos: &HashMap<String, DirectLinearInfo>,
    linear_index: &mut LinearIndex,
    current_linear_type: Option<&str>,
    env: &mut HashMap<String, LinearBindingState>,
    machine_env: &mut HashMap<String, MachineBindingState>,
    errors: &mut Vec<ResolveError>,
    node_id_gen: &mut NodeIdGen,
) {
    match &mut expr.kind {
        ExprKind::BinOp { left, right, .. } => {
            let _ = rewrite_expr_in_scope(
                left,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            let _ = rewrite_expr_in_scope(
                right,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
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
            let _ = rewrite_expr_in_scope(
                expr,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
        }
        ExprKind::Try {
            fallible_expr,
            on_error,
        } => {
            let _ = rewrite_expr_in_scope(
                fallible_expr,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            if let Some(on_error) = on_error {
                let _ = rewrite_expr_in_scope(
                    on_error,
                    infos,
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
                    errors,
                    node_id_gen,
                );
            }
        }
        ExprKind::StructUpdate { target, fields } => {
            let _ = rewrite_expr_in_scope(
                target,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            for field in fields {
                let _ = rewrite_expr_in_scope(
                    &mut field.value,
                    infos,
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
                    errors,
                    node_id_gen,
                );
            }
        }
        ExprKind::Range { start, end } => {
            let _ = rewrite_expr_in_scope(
                start,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            let _ = rewrite_expr_in_scope(
                end,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
        }
        ExprKind::Slice { target, start, end } => {
            let _ = rewrite_expr_in_scope(
                target,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            if let Some(start) = start {
                let _ = rewrite_expr_in_scope(
                    start,
                    infos,
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
                    errors,
                    node_id_gen,
                );
            }
            if let Some(end) = end {
                let _ = rewrite_expr_in_scope(
                    end,
                    infos,
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
                    errors,
                    node_id_gen,
                );
            }
        }
        ExprKind::TupleField { target, .. } | ExprKind::StructField { target, .. } => {
            let _ = rewrite_expr_in_scope(
                target,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
        }
        ExprKind::ArrayIndex { target, indices } => {
            let _ = rewrite_expr_in_scope(
                target,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            for index in indices {
                let _ = rewrite_expr_in_scope(
                    index,
                    infos,
                    linear_index,
                    current_linear_type,
                    env,
                    machine_env,
                    errors,
                    node_id_gen,
                );
            }
        }
        ExprKind::Reply { cap, value } => {
            let _ = rewrite_expr_in_scope(
                cap,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            let _ = rewrite_expr_in_scope(
                value,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
        }
        ExprKind::Closure { body, .. } => {
            let mut closure_env = HashMap::new();
            let mut closure_machine_env = HashMap::new();
            let _ = rewrite_expr_in_scope(
                body,
                infos,
                linear_index,
                current_linear_type,
                &mut closure_env,
                &mut closure_machine_env,
                errors,
                node_id_gen,
            );
        }
        ExprKind::MapGet { target, key } => {
            let _ = rewrite_expr_in_scope(
                target,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
            let _ = rewrite_expr_in_scope(
                key,
                infos,
                linear_index,
                current_linear_type,
                env,
                machine_env,
                errors,
                node_id_gen,
            );
        }
        ExprKind::StringFmt { segments } => {
            for segment in segments {
                if let crate::core::ast::StringFmtSegment::Expr { expr, .. } = segment {
                    let _ = rewrite_expr_in_scope(
                        expr,
                        infos,
                        linear_index,
                        current_linear_type,
                        env,
                        machine_env,
                        errors,
                        node_id_gen,
                    );
                }
            }
        }
        // These are handled by dedicated arms in `rewrite_expr_in_scope`.
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
        | ExprKind::RoleProjection { .. }
        | ExprKind::UnitLit
        | ExprKind::IntLit(_)
        | ExprKind::BoolLit(_)
        | ExprKind::CharLit(_)
        | ExprKind::StringLit { .. }
        | ExprKind::Emit { .. }
        | ExprKind::ClosureRef { .. } => {}
    }
}

// -- Helpers ---------------------------------------------------------

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

fn linear_state_payload_from_fields(
    fields: &[crate::core::ast::StructLitField],
    info: &DirectLinearInfo,
    carry_binding_name: Option<&str>,
    span: Span,
    node_id_gen: &mut NodeIdGen,
) -> Option<Vec<Expr>> {
    if fields.is_empty() && info.shared_fields.is_empty() {
        return Some(Vec::new());
    }

    let mut payload = Vec::with_capacity(info.shared_fields.len());
    let mut used = HashSet::new();
    for shared_field in &info.shared_fields {
        if let Some(field) = fields.iter().find(|field| field.name == shared_field.name) {
            payload.push(field.value.clone());
            used.insert(shared_field.name.clone());
        } else if let Some(binding_name) = carry_binding_name {
            // Shared fields are preserved across transitions automatically.
            // If the user omits one in a returned state literal, carry it from
            // the current state value (`self` in methods, the instance param in
            // machine handlers) just like the old typestate lowering.
            payload.push(build_linear_carried_field_expr(
                binding_name,
                &shared_field.name,
                span,
                node_id_gen,
            ));
        } else {
            return None;
        }
    }

    // We only support shared-field struct construction in this slice. Any
    // extra fields would imply state-local named payloads, which linear states
    // do not support yet.
    if fields.iter().any(|field| !used.contains(&field.name)) {
        return None;
    }

    Some(payload)
}

/// Choose the binding that should implicitly carry shared fields across a
/// transition. We prefer `self` in ordinary methods; otherwise, use the unique
/// live linear binding in scope (e.g. the instance parameter in machine
/// action/trigger handlers). If there is no unique candidate, require the
/// caller to spell shared fields explicitly.
fn carry_binding_name(env: &HashMap<String, LinearBindingState>) -> Option<&str> {
    if env.contains_key("self") {
        return Some("self");
    }

    let mut candidates = env
        .iter()
        .filter(|(_, binding)| !binding.consumed)
        .map(|(name, _)| name.as_str());
    let first = candidates.next()?;
    if candidates.next().is_none() {
        Some(first)
    } else {
        None
    }
}

fn build_linear_carried_field_expr(
    binding_name: &str,
    field_name: &str,
    span: Span,
    node_id_gen: &mut NodeIdGen,
) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::StructField {
            target: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Var {
                    ident: binding_name.to_string(),
                },
                span,
            }),
            field: field_name.to_string(),
        },
        span,
    }
}

fn refine_match_scrutinee_binding(
    pattern: &crate::core::ast::MatchPattern,
    binding_name: &str,
    scrutinee_state: &LinearBindingState,
    infos: &HashMap<String, DirectLinearInfo>,
    env: &mut HashMap<String, LinearBindingState>,
) {
    let crate::core::ast::MatchPattern::EnumVariant {
        enum_name,
        variant_name,
        ..
    } = pattern
    else {
        return;
    };

    let type_name = enum_name
        .as_deref()
        .unwrap_or(scrutinee_state.value_state.type_name.as_str());
    let Some(info) = infos.get(type_name) else {
        return;
    };
    if !info.state_names.contains(variant_name) {
        return;
    }

    env.insert(
        binding_name.to_string(),
        LinearBindingState {
            value_state: LinearValueState {
                type_name: type_name.to_string(),
                state_name: variant_name.clone(),
            },
            consumed: false,
            hosted: scrutinee_state.hosted.clone(),
        },
    );
}

fn machine_spawn_machine_name(expr: &Expr, linear_index: &LinearIndex) -> Option<String> {
    match &expr.kind {
        ExprKind::Try { fallible_expr, .. } => {
            machine_spawn_machine_name(fallible_expr, linear_index)
        }
        // The linear rewriter may inspect bindings both before and after the
        // machine spawn sugar runs. Accept both `PRService::spawn()`'s parsed
        // `EnumVariant` form and the lowered `__mc_machine_spawn_PRService()`
        // call form so hosted provenance seeding stays robust across ordering.
        ExprKind::EnumVariant {
            enum_name, variant, ..
        } if variant == "spawn" => linear_index
            .machine_hosts
            .contains_key(enum_name)
            .then(|| enum_name.clone()),
        ExprKind::Call { callee, .. } => {
            let ExprKind::Var { ident } = &callee.kind else {
                return None;
            };
            linear_index.machine_hosts.keys().find_map(|machine_name| {
                if ident == &machine_spawn_fn_name(machine_name) {
                    Some(machine_name.clone())
                } else {
                    None
                }
            })
        }
        _ => None,
    }
}

fn bind_pattern_name_mut(pattern: &mut crate::core::ast::BindPattern) -> Option<&mut String> {
    match &mut pattern.kind {
        crate::core::ast::BindPatternKind::Name { ident } => Some(ident),
        _ => None,
    }
}

/// Find the unique source state for a machine action override name.
///
/// Same-named actions across multiple source states are intentionally treated
/// as ambiguous here. Validation already rejects missing receiver annotations
/// for that case, so the rewriter simply declines to seed a linear `self` state
/// until the declaration becomes unambiguous.
fn unique_machine_action_source_state(
    info: &DirectLinearInfo,
    action_name: &str,
) -> Option<String> {
    let mut matches =
        info.action_by_source_and_name
            .keys()
            .filter_map(|(source_state, candidate_name)| {
                (candidate_name == action_name).then(|| source_state.clone())
            });
    let first = matches.next()?;
    matches.next().is_none().then_some(first)
}

/// The internal method name for a direct-mode action. Encodes the source state
/// to avoid name collisions when multiple actions share the same name.
pub(crate) fn direct_action_method_name(source_state: &str, action_name: &str) -> String {
    format!("__linear__{source_state}__{action_name}")
}

fn named_type_name(ty_expr: &TypeExpr) -> Option<String> {
    match &ty_expr.kind {
        TypeExprKind::Named { ident, type_args } if type_args.is_empty() => Some(ident.clone()),
        _ => None,
    }
}
