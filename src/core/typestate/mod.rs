//! Typestate source-level desugaring.
//!
//! V1 strategy: lower parsed `typestate` declarations into ordinary type
//! definitions + method blocks + constructor functions before resolve.
//!
//! This pass is intentionally front-end only: it rewrites parsed AST into
//! existing language constructs so resolver/typechecker/back-end stay mostly
//! unchanged for the prototype.

use std::collections::{HashMap, HashSet};

use crate::core::diag::Span;
use crate::core::machine::naming::GENERATED_FINAL_STATE_MARKER;
use crate::core::resolve::ResolveError;
use crate::core::tree::NodeIdGen;
use crate::core::tree::parsed::{
    self, BindPattern, BindPatternKind, CallArg, Expr, ExprKind, FuncDecl, FuncDef, MatchPattern,
    MethodBlock, MethodDef, MethodItem, MethodSig, Module, SelfParam, StmtExpr, StmtExprKind,
    StructDefField, StructLitField, TopLevelItem, TypeDef, TypeDefKind, TypeExpr, TypeExprKind,
    TypestateDef, TypestateFields, TypestateItem, TypestateOnHandler, TypestateState,
    TypestateStateItem,
};
use crate::core::tree::visit::{self, Visitor};
use crate::core::tree::visit_mut::{self, VisitorMut};
use crate::core::tree::{CallArgMode, InitInfo, ParamMode};

const MACHINE_HANDLE_TYPE_NAME: &str = "Machine";
const MACHINE_SPAWN_FAILED_TYPE_NAME: &str = "MachineSpawnFailed";
const MACHINE_BIND_FAILED_TYPE_NAME: &str = "MachineBindFailed";
const MACHINE_START_FAILED_TYPE_NAME: &str = "MachineStartFailed";
const MACHINE_UNKNOWN_TYPE_NAME: &str = "MachineUnknown";
const MACHINE_NOT_RUNNING_TYPE_NAME: &str = "MachineNotRunning";
const MACHINE_MAILBOX_FULL_TYPE_NAME: &str = "MailboxFull";
const MACHINE_REQUEST_FAILED_TYPE_NAME: &str = "RequestFailed";
const MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME: &str = "ManagedRuntimeUnavailable";
const MANAGED_RUNTIME_DEFAULT_MAILBOX_CAP: u64 = 8;
const MANAGED_RUNTIME_BOOTSTRAP_FN: &str = "__mc_machine_runtime_managed_bootstrap_u64";
const MANAGED_RUNTIME_CURRENT_FN: &str = "__mc_machine_runtime_managed_current_u64";
const MANAGED_RUNTIME_SHUTDOWN_FN: &str = "__mc_machine_runtime_managed_shutdown_u64";
const MANAGED_RUNTIME_STEP_FN: &str = "__mc_machine_runtime_step_u64";

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypestateRoleImplRef {
    pub id: crate::core::tree::NodeId,
    pub typestate_name: String,
    pub path: Vec<String>,
    pub span: Span,
}

pub fn collect_role_impl_refs(module: &Module) -> Vec<TypestateRoleImplRef> {
    let mut out = Vec::new();
    for typestate in module.typestate_defs() {
        for role_impl in &typestate.role_impls {
            out.push(TypestateRoleImplRef {
                id: role_impl.id,
                typestate_name: typestate.name.clone(),
                path: role_impl.path.clone(),
                span: role_impl.span,
            });
        }
    }
    out
}

pub fn desugar_module(module: &mut Module, node_id_gen: &mut NodeIdGen) -> Vec<ResolveError> {
    let mut out = Vec::with_capacity(module.top_level_items.len());
    // Typestate `Type::new(...)` calls are rewritten after lowering, so we keep
    // a typestate-name -> generated-constructor-name map across the whole module.
    let mut ctor_by_typestate = HashMap::<String, String>::new();
    // Managed constructor sugar (`Type::spawn(...)`) rewrites similarly.
    let mut spawn_by_typestate = HashMap::<String, String>::new();
    // Source `Machine<Typestate>` annotations rewrite to these concrete handle
    // types before resolve/typecheck.
    let mut handle_by_typestate = HashMap::<String, String>::new();
    // Keep both source and generated state names so external-literal checks can
    // flag either form.
    let mut source_state_names = HashSet::new();
    let mut generated_state_names = HashSet::new();
    let mut errors = Vec::new();
    let mut saw_typestate = false;

    for item in module.top_level_items.drain(..) {
        match item {
            TopLevelItem::TypestateDef(typestate) => {
                saw_typestate = true;
                // One typestate can lower to multiple top-level items.
                let lowered = desugar_typestate(
                    typestate,
                    node_id_gen,
                    &mut ctor_by_typestate,
                    &mut spawn_by_typestate,
                    &mut handle_by_typestate,
                );
                out.extend(lowered.items);
                source_state_names.extend(lowered.source_state_names);
                generated_state_names.extend(lowered.generated_state_names);
                errors.extend(lowered.errors);
            }
            other => out.push(other),
        }
    }

    module.top_level_items = out;
    let machines_opted_in = rewrite_machines_entrypoint(module, node_id_gen);
    if saw_typestate {
        ensure_machine_support_types(module, node_id_gen);
    }
    if saw_typestate || machines_opted_in {
        ensure_machine_runtime_intrinsics(module, node_id_gen);
    }
    // Rewrite `Typestate::new(...)` enum-variant syntax into generated ctor calls.
    let first_spawn_call_span = rewrite_constructor_invocations(
        module,
        &ctor_by_typestate,
        &spawn_by_typestate,
        node_id_gen,
    );
    // Rewrite source typed handles into concrete typestate-specific handle
    // types so the distinction survives in core typing (non-erased model).
    rewrite_typed_machine_handle_refs(module, &handle_by_typestate);
    if let Some(span) = first_spawn_call_span
        && !machines_opted_in
    {
        errors.push(ResolveError::TypestateSpawnRequiresMachinesOptIn(span));
    }
    // Enforce constructor-only entry: reject state literals outside typestate
    // constructor/transition bodies.
    errors.extend(find_external_state_literal_errors(
        module,
        &source_state_names,
        &generated_state_names,
    ));
    errors
}

fn rewrite_typed_machine_handle_refs(
    module: &mut Module,
    handle_by_typestate: &HashMap<String, String>,
) {
    struct TypedMachineHandleRewriter<'a> {
        // Source typestate name -> concrete generated handle type name.
        handle_by_typestate: &'a HashMap<String, String>,
    }

    impl VisitorMut<()> for TypedMachineHandleRewriter<'_> {
        fn visit_stmt_expr(&mut self, stmt: &mut StmtExpr) {
            // `decl_ty` in let/var is not traversed by generic walk; handle
            // those explicitly so `Machine<Typestate>` annotations rewrite.
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

        fn visit_type_expr(&mut self, ty: &mut TypeExpr) {
            if let TypeExprKind::Named {
                ident, type_args, ..
            } = &mut ty.kind
                && ident == MACHINE_HANDLE_TYPE_NAME
                && type_args.len() == 1
                && let Some(type_arg) = type_args.first()
                && let TypeExprKind::Named {
                    ident: ts_name,
                    type_args: ts_args,
                    ..
                } = &type_arg.kind
                && ts_args.is_empty()
                && let Some(handle_name) = self.handle_by_typestate.get(ts_name)
            {
                *ident = handle_name.clone();
                type_args.clear();
            }
            visit_mut::walk_type_expr(self, ty);
        }

        fn visit_expr(&mut self, expr: &mut Expr) {
            if let ExprKind::StructLit {
                name, type_args, ..
            } = &mut expr.kind
                && name == MACHINE_HANDLE_TYPE_NAME
                && type_args.len() == 1
                && let Some(type_arg) = type_args.first()
                && let TypeExprKind::Named {
                    ident: ts_name,
                    type_args: ts_args,
                    ..
                } = &type_arg.kind
                && ts_args.is_empty()
                && let Some(handle_name) = self.handle_by_typestate.get(ts_name)
            {
                *name = handle_name.clone();
                type_args.clear();
            }
            visit_mut::walk_expr(self, expr);
        }
    }

    let mut rewriter = TypedMachineHandleRewriter {
        handle_by_typestate,
    };
    rewriter.visit_module(module);
}

fn desugar_typestate(
    typestate: TypestateDef,
    node_id_gen: &mut NodeIdGen,
    ctor_by_typestate: &mut HashMap<String, String>,
    spawn_by_typestate: &mut HashMap<String, String>,
    handle_by_typestate: &mut HashMap<String, String>,
) -> TypestateDesugarOutput {
    let ts_name = typestate.name.clone();
    let ctor_name = format!("__ts_ctor_{}", ts_name);
    let spawn_name = format!("__ts_spawn_{}", ts_name);
    let handle_type_name = format!("__mc_machine_handle_{}", ts_name);
    let descriptor_id_helper_name = format!("__mc_machine_descriptor_id_{}", ts_name);
    ctor_by_typestate.insert(ts_name.clone(), ctor_name.clone());
    spawn_by_typestate.insert(ts_name.clone(), spawn_name.clone());
    handle_by_typestate.insert(ts_name.clone(), handle_type_name.clone());

    let analysis = analyze_typestate(&typestate);
    // We preserve source state names for user-facing checks and to support the
    // carried-field rewrite before source names are rewritten to generated types.
    let source_state_names: HashSet<String> = analysis
        .states
        .iter()
        .map(|state| state.name.clone())
        .collect();
    let shared_fields = analysis.shared_fields;
    let states = analysis.states;
    let typestate_handlers = analysis.handlers;
    let final_state_names = analysis.final_state_names;
    let typed_send_specs = collect_typed_send_specs(&typestate_handlers, &states);
    let typed_request_specs = collect_typed_request_specs(&typestate_handlers, &states);
    let state_name_map = build_state_name_map(&ts_name, &states);
    let generated_state_names: HashSet<String> = state_name_map.values().cloned().collect();
    // Local fields are used by shorthand transition rewriting (`State`) to know
    // whether shorthand is legal (only when local field set is empty).
    let local_fields_by_state: HashMap<String, Vec<StructDefField>> = states
        .iter()
        .map(|state| {
            (
                state.name.clone(),
                collect_first_state_fields_block(&state.items),
            )
        })
        .collect();
    let carried_field_names: Vec<String> = shared_fields.iter().map(|f| f.name.clone()).collect();

    let mut lowered = Vec::new();
    lowered.push(machine_handle_named_type_def(
        &handle_type_name,
        node_id_gen,
    ));
    lowered.push(machine_handle_method_block(
        &handle_type_name,
        &typed_send_specs,
        &typed_request_specs,
        node_id_gen,
    ));

    for state in states {
        let is_final_state = final_state_names.contains(&state.name);
        // Each source state becomes a generated struct type that includes both
        // carried fields and local state fields.
        let state_ty_name = state_name_map
            .get(&state.name)
            .expect("state map should include state")
            .clone();

        let mut fields = shared_fields.clone();
        fields.extend(collect_first_state_fields_block(&state.items));
        // Generated struct state type.
        lowered.push(TopLevelItem::TypeDef(TypeDef {
            id: node_id_gen.new_id(),
            def_id: (),
            attrs: Vec::new(),
            name: state_ty_name.clone(),
            type_params: Vec::new(),
            kind: TypeDefKind::Struct { fields },
            span: state.span,
        }));

        // Generated inherent methods for this state. We inject implicit `sink self`
        // and apply transition-literal rewrites before state-name mangling.
        let mut method_items = Vec::new();
        let mut handler_index = 0usize;
        if !is_final_state {
            for handler in &typestate_handlers {
                let method_source = lower_handler_to_method_source(
                    handler,
                    &state.name,
                    &mut handler_index,
                    node_id_gen,
                );
                method_items.push(MethodItem::Def(lower_state_method(
                    method_source,
                    node_id_gen,
                    &source_state_names,
                    &local_fields_by_state,
                    &carried_field_names,
                    &state_name_map,
                    state.span,
                )));
            }
        }
        for item in state.items {
            match item {
                TypestateStateItem::Method(method) => {
                    if is_final_state {
                        continue;
                    }
                    method_items.push(MethodItem::Def(lower_state_method(
                        method,
                        node_id_gen,
                        &source_state_names,
                        &local_fields_by_state,
                        &carried_field_names,
                        &state_name_map,
                        state.span,
                    )));
                }
                TypestateStateItem::Handler(handler) => {
                    if is_final_state {
                        continue;
                    }
                    let method_source = lower_handler_to_method_source(
                        &handler,
                        &state.name,
                        &mut handler_index,
                        node_id_gen,
                    );
                    method_items.push(MethodItem::Def(lower_state_method(
                        method_source,
                        node_id_gen,
                        &source_state_names,
                        &local_fields_by_state,
                        &carried_field_names,
                        &state_name_map,
                        state.span,
                    )));
                }
                TypestateStateItem::Fields(_) => {}
            }
        }
        if is_final_state {
            method_items.push(MethodItem::Def(final_state_marker_method_def(
                state.span,
                node_id_gen,
            )));
        }
        if !method_items.is_empty() {
            lowered.push(TopLevelItem::MethodBlock(MethodBlock {
                id: node_id_gen.new_id(),
                type_name: state_ty_name,
                trait_name: None,
                method_items,
                span: state.span,
            }));
        }
    }

    if let Some(mut ctor) = analysis.constructor {
        // Constructor body/signature still refer to source state names here.
        // Rewrite them to generated names to make the lowered module consistent.
        ctor.sig.name = ctor_name;
        rewrite_state_refs_in_func(&mut ctor, &state_name_map);
        let spawn = lower_spawn_func(
            &spawn_name,
            &handle_type_name,
            &descriptor_id_helper_name,
            &ctor,
            &state_name_map,
            node_id_gen,
        );
        lowered.push(TopLevelItem::FuncDef(ctor));
        lowered.push(TopLevelItem::FuncDef(spawn));
    } else {
        // Keep parser/product permissive in #80/#81. Validation in #82 will
        // enforce required constructor.
        lowered.push(TopLevelItem::FuncDecl(FuncDecl {
            id: node_id_gen.new_id(),
            def_id: (),
            attrs: Vec::new(),
            sig: parsed::FunctionSig {
                name: ctor_name,
                type_params: Vec::new(),
                params: Vec::new(),
                ret_ty_expr: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Infer,
                    span: typestate.span,
                },
                span: typestate.span,
            },
            span: typestate.span,
        }));
    }
    lowered.push(TopLevelItem::FuncDecl(FuncDecl {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        sig: parsed::FunctionSig {
            name: descriptor_id_helper_name,
            type_params: Vec::new(),
            params: Vec::new(),
            ret_ty_expr: u64_type_expr(node_id_gen, typestate.span),
            span: typestate.span,
        },
        span: typestate.span,
    }));

    TypestateDesugarOutput {
        // Everything downstream consumes this lowered form only.
        items: lowered,
        source_state_names,
        generated_state_names,
        errors: analysis.errors,
    }
}

// ---------------------------------------------------------------------------
// Validation analysis (source typestate -> validated facts)
// ---------------------------------------------------------------------------

struct TypestateDesugarOutput {
    // Flattened top-level items emitted from one typestate declaration.
    items: Vec<TopLevelItem>,
    // Source-level `state Foo` names.
    source_state_names: HashSet<String>,
    // Lowered generated struct type names (`__ts_<Typestate>_<State>`).
    generated_state_names: HashSet<String>,
    // Validation diagnostics gathered while lowering this typestate.
    errors: Vec<ResolveError>,
}

#[derive(Clone)]
struct TypedSendSpec {
    selector_ty: TypeExpr,
    kind: u64,
}

#[derive(Clone)]
struct TypedRequestSpec {
    payload_ty: TypeExpr,
    kind: u64,
}

fn rewrite_machines_entrypoint(module: &mut Module, node_id_gen: &mut NodeIdGen) -> bool {
    let mut opted_in = false;
    for item in &mut module.top_level_items {
        let TopLevelItem::FuncDef(func) = item else {
            continue;
        };
        let has_machines_attr = func
            .attrs
            .iter()
            .any(|attr| attr.name == "machines" && attr.args.is_empty());
        if !has_machines_attr || func.sig.name != "main" {
            continue;
        }

        opted_in = true;
        wrap_main_with_managed_runtime(func, node_id_gen);
    }
    opted_in
}

fn wrap_main_with_managed_runtime(main: &mut FuncDef, node_id_gen: &mut NodeIdGen) {
    fn var_expr(name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Var {
                ident: name.to_string(),
                def_id: (),
            },
            ty: (),
            span,
        }
    }

    fn int_expr(value: u64, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::IntLit(value),
            ty: (),
            span,
        }
    }

    fn call_expr(
        callee_name: &str,
        args: Vec<Expr>,
        node_id_gen: &mut NodeIdGen,
        span: Span,
    ) -> Expr {
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Call {
                callee: Box::new(var_expr(callee_name, node_id_gen, span)),
                args: args
                    .into_iter()
                    .map(|expr| CallArg {
                        mode: CallArgMode::Default,
                        expr,
                        init: InitInfo::default(),
                        span,
                    })
                    .collect(),
            },
            ty: (),
            span,
        }
    }

    fn let_bind_stmt(
        ident: &str,
        value: Expr,
        node_id_gen: &mut NodeIdGen,
        span: Span,
    ) -> StmtExpr {
        StmtExpr {
            id: node_id_gen.new_id(),
            kind: StmtExprKind::LetBind {
                pattern: BindPattern {
                    id: node_id_gen.new_id(),
                    kind: BindPatternKind::Name {
                        ident: ident.to_string(),
                        def_id: (),
                    },
                    span,
                },
                decl_ty: None,
                value: Box::new(value),
            },
            ty: (),
            span,
        }
    }

    let span = main.body.span;
    let original_body = std::mem::replace(
        &mut main.body,
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::UnitLit,
            ty: (),
            span,
        },
    );
    let bootstrap_call = call_expr(MANAGED_RUNTIME_BOOTSTRAP_FN, Vec::new(), node_id_gen, span);
    let shutdown_call = call_expr(MANAGED_RUNTIME_SHUTDOWN_FN, Vec::new(), node_id_gen, span);
    let step_call = call_expr(
        MANAGED_RUNTIME_STEP_FN,
        vec![var_expr("__mc_rt", node_id_gen, span)],
        node_id_gen,
        span,
    );
    let step_status_is_did_work = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::BinOp {
            left: Box::new(var_expr("__mc_step_status", node_id_gen, span)),
            op: crate::core::tree::BinaryOp::Eq,
            right: Box::new(int_expr(1, node_id_gen, span)),
        },
        ty: (),
        span,
    };
    let runtime_available_cond = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::BinOp {
            left: Box::new(var_expr("__mc_rt", node_id_gen, span)),
            op: crate::core::tree::BinaryOp::Ne,
            right: Box::new(int_expr(0, node_id_gen, span)),
        },
        ty: (),
        span,
    };
    // Auto-drive policy for `@machines`:
    // keep stepping while runtime reports "did work", and stop once it reaches
    // idle or faulted. We still return the user's main result.
    let auto_drive_if = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::If {
            cond: Box::new(runtime_available_cond),
            then_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: vec![parsed::BlockItem::Stmt(StmtExpr {
                        id: node_id_gen.new_id(),
                        kind: StmtExprKind::While {
                            cond: Box::new(Expr {
                                id: node_id_gen.new_id(),
                                kind: ExprKind::BoolLit(true),
                                ty: (),
                                span,
                            }),
                            body: Box::new(Expr {
                                id: node_id_gen.new_id(),
                                kind: ExprKind::Block {
                                    items: vec![
                                        parsed::BlockItem::Stmt(let_bind_stmt(
                                            "__mc_step_status",
                                            step_call,
                                            node_id_gen,
                                            span,
                                        )),
                                        parsed::BlockItem::Expr(Expr {
                                            id: node_id_gen.new_id(),
                                            kind: ExprKind::If {
                                                cond: Box::new(step_status_is_did_work),
                                                then_body: Box::new(Expr {
                                                    id: node_id_gen.new_id(),
                                                    kind: ExprKind::Block {
                                                        items: vec![parsed::BlockItem::Stmt(
                                                            StmtExpr {
                                                                id: node_id_gen.new_id(),
                                                                kind: StmtExprKind::Continue,
                                                                ty: (),
                                                                span,
                                                            },
                                                        )],
                                                        tail: None,
                                                    },
                                                    ty: (),
                                                    span,
                                                }),
                                                else_body: Box::new(Expr {
                                                    id: node_id_gen.new_id(),
                                                    kind: ExprKind::Block {
                                                        items: vec![parsed::BlockItem::Stmt(
                                                            StmtExpr {
                                                                id: node_id_gen.new_id(),
                                                                kind: StmtExprKind::Break,
                                                                ty: (),
                                                                span,
                                                            },
                                                        )],
                                                        tail: None,
                                                    },
                                                    ty: (),
                                                    span,
                                                }),
                                            },
                                            ty: (),
                                            span,
                                        }),
                                    ],
                                    tail: None,
                                },
                                ty: (),
                                span,
                            }),
                        },
                        ty: (),
                        span,
                    })],
                    tail: None,
                },
                ty: (),
                span,
            }),
            else_body: Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Block {
                    items: Vec::new(),
                    tail: None,
                },
                ty: (),
                span,
            }),
        },
        ty: (),
        span,
    };

    main.body = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items: vec![
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_rt",
                    bootstrap_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_main_result",
                    original_body,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Expr(auto_drive_if),
                parsed::BlockItem::Expr(shutdown_call),
            ],
            tail: Some(Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::Var {
                    ident: "__mc_main_result".to_string(),
                    def_id: (),
                },
                ty: (),
                span,
            })),
        },
        ty: (),
        span,
    };
}

fn ensure_machine_support_types(module: &mut Module, node_id_gen: &mut NodeIdGen) {
    let existing: HashSet<String> = module
        .top_level_items
        .iter()
        .filter_map(|item| match item {
            TopLevelItem::TypeDef(def) => Some(def.name.clone()),
            _ => None,
        })
        .collect();

    let mut prepend = Vec::new();
    if !existing.contains(MACHINE_HANDLE_TYPE_NAME) {
        prepend.push(machine_handle_type_def(node_id_gen));
    }
    if !existing.contains(MACHINE_SPAWN_FAILED_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_SPAWN_FAILED_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_BIND_FAILED_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_BIND_FAILED_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_START_FAILED_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_START_FAILED_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_UNKNOWN_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_UNKNOWN_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_NOT_RUNNING_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_NOT_RUNNING_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_MAILBOX_FULL_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_MAILBOX_FULL_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_REQUEST_FAILED_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_REQUEST_FAILED_TYPE_NAME,
            node_id_gen,
        ));
    }
    if !existing.contains(MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME) {
        prepend.push(empty_struct_type_def(
            MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
            node_id_gen,
        ));
    }

    let has_machine_methods = module.top_level_items.iter().any(|item| {
        matches!(item, TopLevelItem::MethodBlock(block) if block.type_name == MACHINE_HANDLE_TYPE_NAME)
    });
    if !has_machine_methods {
        prepend.push(machine_handle_method_block(
            MACHINE_HANDLE_TYPE_NAME,
            &[],
            &[],
            node_id_gen,
        ));
    }

    if prepend.is_empty() {
        return;
    }

    let mut items = Vec::with_capacity(prepend.len() + module.top_level_items.len());
    items.extend(prepend);
    items.extend(std::mem::take(&mut module.top_level_items));
    module.top_level_items = items;
}

fn ensure_machine_runtime_intrinsics(module: &mut Module, node_id_gen: &mut NodeIdGen) {
    let existing_callables: HashSet<String> = module
        .top_level_items
        .iter()
        .filter_map(|item| match item {
            TopLevelItem::FuncDecl(decl) => Some(decl.sig.name.clone()),
            TopLevelItem::FuncDef(def) => Some(def.sig.name.clone()),
            _ => None,
        })
        .collect();

    let span = Span::default();
    let mut append = Vec::new();
    let mut push_decl = |name: &str, param_names: &[&str]| {
        if existing_callables.contains(name) {
            return;
        }
        // These declarations keep typestate-managed spawn lowering self-contained
        // even when prelude runtime declarations are not present in the source
        // module under test.
        append.push(TopLevelItem::FuncDecl(FuncDecl {
            id: node_id_gen.new_id(),
            def_id: (),
            attrs: Vec::new(),
            sig: parsed::FunctionSig {
                name: name.to_string(),
                type_params: Vec::new(),
                params: param_names
                    .iter()
                    .map(|param_name| parsed::Param {
                        id: node_id_gen.new_id(),
                        ident: (*param_name).to_string(),
                        def_id: (),
                        typ: u64_type_expr(node_id_gen, span),
                        mode: ParamMode::In,
                        span,
                    })
                    .collect(),
                ret_ty_expr: u64_type_expr(node_id_gen, span),
                span,
            },
            span,
        }));
    };

    push_decl(MANAGED_RUNTIME_BOOTSTRAP_FN, &[]);
    push_decl(MANAGED_RUNTIME_CURRENT_FN, &[]);
    push_decl(MANAGED_RUNTIME_SHUTDOWN_FN, &[]);
    push_decl(
        "__mc_machine_runtime_spawn_u64",
        &["runtime", "mailbox_cap"],
    );
    push_decl(
        "__mc_machine_runtime_bind_descriptor_u64",
        &[
            "runtime",
            "machine_id",
            "descriptor_id",
            "initial_state_tag",
        ],
    );
    push_decl("__mc_machine_runtime_start_u64", &["runtime", "machine_id"]);
    push_decl("__mc_machine_runtime_step_u64", &["runtime"]);
    push_decl(
        "__mc_machine_runtime_send_u64",
        &["runtime", "dst", "kind", "payload0", "payload1"],
    );
    push_decl(
        "__mc_machine_runtime_request_u64",
        &["runtime", "src", "dst", "kind", "payload0", "payload1"],
    );
    if !existing_callables.contains("__mc_machine_payload_pack") {
        append.push(TopLevelItem::FuncDecl(FuncDecl {
            id: node_id_gen.new_id(),
            def_id: (),
            attrs: Vec::new(),
            sig: parsed::FunctionSig {
                name: "__mc_machine_payload_pack".to_string(),
                type_params: vec![parsed::TypeParam {
                    id: node_id_gen.new_id(),
                    ident: "T".to_string(),
                    bound: None,
                    def_id: (),
                    span,
                }],
                params: vec![parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "payload".to_string(),
                    def_id: (),
                    typ: TypeExpr {
                        id: node_id_gen.new_id(),
                        kind: TypeExprKind::Named {
                            ident: "T".to_string(),
                            def_id: (),
                            type_args: Vec::new(),
                        },
                        span,
                    },
                    mode: ParamMode::In,
                    span,
                }],
                // Returns `(payload0_ptr_word, payload_layout_id)`.
                ret_ty_expr: TypeExpr {
                    id: node_id_gen.new_id(),
                    kind: TypeExprKind::Tuple {
                        field_ty_exprs: vec![
                            u64_type_expr(node_id_gen, span),
                            u64_type_expr(node_id_gen, span),
                        ],
                    },
                    span,
                },
                span,
            },
            span,
        }));
    }

    module.top_level_items.extend(append);
}

fn machine_handle_type_def(node_id_gen: &mut NodeIdGen) -> TopLevelItem {
    machine_handle_named_type_def(MACHINE_HANDLE_TYPE_NAME, node_id_gen)
}

fn machine_handle_named_type_def(type_name: &str, node_id_gen: &mut NodeIdGen) -> TopLevelItem {
    TopLevelItem::TypeDef(TypeDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        name: type_name.to_string(),
        type_params: Vec::new(),
        kind: TypeDefKind::Struct {
            fields: vec![StructDefField {
                id: node_id_gen.new_id(),
                name: "_id".to_string(),
                ty: u64_type_expr(node_id_gen, Span::default()),
                span: Span::default(),
            }],
        },
        span: Span::default(),
    })
}

fn empty_struct_type_def(name: &str, node_id_gen: &mut NodeIdGen) -> TopLevelItem {
    TopLevelItem::TypeDef(TypeDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        name: name.to_string(),
        type_params: Vec::new(),
        kind: TypeDefKind::Struct { fields: Vec::new() },
        span: Span::default(),
    })
}

fn machine_handle_method_block(
    handle_type_name: &str,
    typed_send_specs: &[TypedSendSpec],
    typed_request_specs: &[TypedRequestSpec],
    node_id_gen: &mut NodeIdGen,
) -> TopLevelItem {
    let span = Span::default();

    fn var_expr(name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Var {
                ident: name.to_string(),
                def_id: (),
            },
            ty: (),
            span,
        }
    }

    fn int_expr(value: u64, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::IntLit(value),
            ty: (),
            span,
        }
    }

    fn tuple_field_expr(
        target: Expr,
        index: usize,
        node_id_gen: &mut NodeIdGen,
        span: Span,
    ) -> Expr {
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::TupleField {
                target: Box::new(target),
                index,
            },
            ty: (),
            span,
        }
    }

    fn call_expr(
        callee_name: &str,
        args: Vec<Expr>,
        node_id_gen: &mut NodeIdGen,
        span: Span,
    ) -> Expr {
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Call {
                callee: Box::new(var_expr(callee_name, node_id_gen, span)),
                args: args
                    .into_iter()
                    .map(|expr| CallArg {
                        mode: CallArgMode::Default,
                        expr,
                        init: InitInfo::default(),
                        span,
                    })
                    .collect(),
            },
            ty: (),
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
            ty: (),
            span,
        }
    }

    fn let_bind_stmt(
        ident: &str,
        value: Expr,
        node_id_gen: &mut NodeIdGen,
        span: Span,
    ) -> StmtExpr {
        StmtExpr {
            id: node_id_gen.new_id(),
            kind: StmtExprKind::LetBind {
                pattern: BindPattern {
                    id: node_id_gen.new_id(),
                    kind: BindPatternKind::Name {
                        ident: ident.to_string(),
                        def_id: (),
                    },
                    span,
                },
                decl_ty: None,
                value: Box::new(value),
            },
            ty: (),
            span,
        }
    }

    fn return_stmt(value: Expr, node_id_gen: &mut NodeIdGen, span: Span) -> StmtExpr {
        StmtExpr {
            id: node_id_gen.new_id(),
            kind: StmtExprKind::Return {
                value: Some(Box::new(value)),
            },
            ty: (),
            span,
        }
    }

    fn return_if_eq_zero(
        var_name: &str,
        error_type_name: &str,
        node_id_gen: &mut NodeIdGen,
        span: Span,
    ) -> Expr {
        let cond = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::BinOp {
                left: Box::new(var_expr(var_name, node_id_gen, span)),
                op: crate::core::tree::BinaryOp::Eq,
                right: Box::new(int_expr(0, node_id_gen, span)),
            },
            ty: (),
            span,
        };
        let then_body = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![parsed::BlockItem::Stmt(return_stmt(
                    Expr {
                        id: node_id_gen.new_id(),
                        kind: ExprKind::StructLit {
                            name: error_type_name.to_string(),
                            type_args: Vec::new(),
                            fields: Vec::new(),
                        },
                        ty: (),
                        span,
                    },
                    node_id_gen,
                    span,
                ))],
                tail: None,
            },
            ty: (),
            span,
        };
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_body: Box::new(then_body),
                else_body: Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::Block {
                        items: Vec::new(),
                        tail: None,
                    },
                    ty: (),
                    span,
                }),
            },
            ty: (),
            span,
        }
    }

    fn return_if_eq(
        var_name: &str,
        value: u64,
        error_type_name: &str,
        node_id_gen: &mut NodeIdGen,
        span: Span,
    ) -> Expr {
        let cond = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::BinOp {
                left: Box::new(var_expr(var_name, node_id_gen, span)),
                op: crate::core::tree::BinaryOp::Eq,
                right: Box::new(int_expr(value, node_id_gen, span)),
            },
            ty: (),
            span,
        };
        let then_body = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![parsed::BlockItem::Stmt(return_stmt(
                    Expr {
                        id: node_id_gen.new_id(),
                        kind: ExprKind::StructLit {
                            name: error_type_name.to_string(),
                            type_args: Vec::new(),
                            fields: Vec::new(),
                        },
                        ty: (),
                        span,
                    },
                    node_id_gen,
                    span,
                ))],
                tail: None,
            },
            ty: (),
            span,
        };
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_body: Box::new(then_body),
                else_body: Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::Block {
                        items: Vec::new(),
                        tail: None,
                    },
                    ty: (),
                    span,
                }),
            },
            ty: (),
            span,
        }
    }

    fn machine_send_result_union_type(node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
        TypeExpr {
            id: node_id_gen.new_id(),
            kind: TypeExprKind::Union {
                variants: vec![
                    named_type_expr("()", node_id_gen, span),
                    named_type_expr(
                        MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
                        node_id_gen,
                        span,
                    ),
                    named_type_expr(MACHINE_UNKNOWN_TYPE_NAME, node_id_gen, span),
                    named_type_expr(MACHINE_NOT_RUNNING_TYPE_NAME, node_id_gen, span),
                    named_type_expr(MACHINE_MAILBOX_FULL_TYPE_NAME, node_id_gen, span),
                ],
            },
            span,
        }
    }

    fn machine_request_result_union_type(node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
        TypeExpr {
            id: node_id_gen.new_id(),
            kind: TypeExprKind::Union {
                variants: vec![
                    named_type_expr("u64", node_id_gen, span),
                    named_type_expr(
                        MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
                        node_id_gen,
                        span,
                    ),
                    named_type_expr(MACHINE_REQUEST_FAILED_TYPE_NAME, node_id_gen, span),
                ],
            },
            span,
        }
    }

    let send_method = MethodDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        sig: MethodSig {
            name: "send".to_string(),
            type_params: Vec::new(),
            self_param: SelfParam {
                id: node_id_gen.new_id(),
                def_id: (),
                mode: ParamMode::In,
                span,
            },
            params: vec![
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "kind".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "payload0".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "payload1".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
            ],
            ret_ty_expr: machine_send_result_union_type(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    parsed::BlockItem::Stmt(let_bind_stmt(
                        "__mc_rt",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_if_eq_zero(
                        "__mc_rt",
                        MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Stmt(let_bind_stmt(
                        "__mc_status",
                        call_expr(
                            "__mc_machine_runtime_send_u64",
                            vec![
                                var_expr("__mc_rt", node_id_gen, span),
                                self_field_expr("_id", node_id_gen, span),
                                var_expr("kind", node_id_gen, span),
                                var_expr("payload0", node_id_gen, span),
                                var_expr("payload1", node_id_gen, span),
                            ],
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_if_eq(
                        "__mc_status",
                        1,
                        MACHINE_UNKNOWN_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_if_eq(
                        "__mc_status",
                        2,
                        MACHINE_NOT_RUNNING_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_if_eq(
                        "__mc_status",
                        3,
                        MACHINE_MAILBOX_FULL_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                ],
                tail: Some(Box::new(unit_expr(node_id_gen, span))),
            },
            ty: (),
            span,
        },
        span,
    };

    let typed_send_methods: Vec<MethodItem> = typed_send_specs
        .iter()
        .map(|spec| {
            MethodItem::Def(MethodDef {
                id: node_id_gen.new_id(),
                def_id: (),
                attrs: Vec::new(),
                sig: MethodSig {
                    name: "send".to_string(),
                    type_params: Vec::new(),
                    self_param: SelfParam {
                        id: node_id_gen.new_id(),
                        def_id: (),
                        mode: ParamMode::In,
                        span,
                    },
                    params: vec![parsed::Param {
                        id: node_id_gen.new_id(),
                        ident: "payload".to_string(),
                        def_id: (),
                        typ: clone_type_expr_with_new_ids(&spec.selector_ty, node_id_gen),
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
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_rt",
                                call_expr(
                                    MANAGED_RUNTIME_CURRENT_FN,
                                    Vec::new(),
                                    node_id_gen,
                                    span,
                                ),
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Expr(return_if_eq_zero(
                                "__mc_rt",
                                MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                            // Pack typed payload into runtime ABI words:
                            // - payload0: heap box pointer
                            // - payload1: payload layout id
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_packed",
                                call_expr(
                                    "__mc_machine_payload_pack",
                                    vec![var_expr("payload", node_id_gen, span)],
                                    node_id_gen,
                                    span,
                                ),
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_status",
                                call_expr(
                                    "__mc_machine_runtime_send_u64",
                                    vec![
                                        var_expr("__mc_rt", node_id_gen, span),
                                        self_field_expr("_id", node_id_gen, span),
                                        int_expr(spec.kind, node_id_gen, span),
                                        tuple_field_expr(
                                            var_expr("__mc_packed", node_id_gen, span),
                                            0,
                                            node_id_gen,
                                            span,
                                        ),
                                        tuple_field_expr(
                                            var_expr("__mc_packed", node_id_gen, span),
                                            1,
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
                            parsed::BlockItem::Expr(return_if_eq(
                                "__mc_status",
                                1,
                                MACHINE_UNKNOWN_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Expr(return_if_eq(
                                "__mc_status",
                                2,
                                MACHINE_NOT_RUNNING_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Expr(return_if_eq(
                                "__mc_status",
                                3,
                                MACHINE_MAILBOX_FULL_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                        ],
                        tail: Some(Box::new(unit_expr(node_id_gen, span))),
                    },
                    ty: (),
                    span,
                },
                span,
            })
        })
        .collect();

    let typed_request_methods: Vec<MethodItem> = typed_request_specs
        .iter()
        .map(|spec| {
            MethodItem::Def(MethodDef {
                id: node_id_gen.new_id(),
                def_id: (),
                attrs: Vec::new(),
                sig: MethodSig {
                    name: "request".to_string(),
                    type_params: Vec::new(),
                    self_param: SelfParam {
                        id: node_id_gen.new_id(),
                        def_id: (),
                        mode: ParamMode::In,
                        span,
                    },
                    params: vec![
                        parsed::Param {
                            id: node_id_gen.new_id(),
                            ident: "dst".to_string(),
                            def_id: (),
                            typ: u64_type_expr(node_id_gen, span),
                            mode: ParamMode::In,
                            span,
                        },
                        parsed::Param {
                            id: node_id_gen.new_id(),
                            ident: "payload".to_string(),
                            def_id: (),
                            typ: clone_type_expr_with_new_ids(&spec.payload_ty, node_id_gen),
                            mode: ParamMode::In,
                            span,
                        },
                    ],
                    ret_ty_expr: machine_request_result_union_type(node_id_gen, span),
                    span,
                },
                body: Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::Block {
                        items: vec![
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_rt",
                                call_expr(
                                    MANAGED_RUNTIME_CURRENT_FN,
                                    Vec::new(),
                                    node_id_gen,
                                    span,
                                ),
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Expr(return_if_eq_zero(
                                "__mc_rt",
                                MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                            // Pack typed payload into runtime ABI words:
                            // - payload0: heap box pointer
                            // - payload1: payload layout id
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_packed",
                                call_expr(
                                    "__mc_machine_payload_pack",
                                    vec![var_expr("payload", node_id_gen, span)],
                                    node_id_gen,
                                    span,
                                ),
                                node_id_gen,
                                span,
                            )),
                            parsed::BlockItem::Stmt(let_bind_stmt(
                                "__mc_pending_id",
                                call_expr(
                                    "__mc_machine_runtime_request_u64",
                                    vec![
                                        var_expr("__mc_rt", node_id_gen, span),
                                        self_field_expr("_id", node_id_gen, span),
                                        var_expr("dst", node_id_gen, span),
                                        int_expr(spec.kind, node_id_gen, span),
                                        tuple_field_expr(
                                            var_expr("__mc_packed", node_id_gen, span),
                                            0,
                                            node_id_gen,
                                            span,
                                        ),
                                        tuple_field_expr(
                                            var_expr("__mc_packed", node_id_gen, span),
                                            1,
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
                            parsed::BlockItem::Expr(return_if_eq_zero(
                                "__mc_pending_id",
                                MACHINE_REQUEST_FAILED_TYPE_NAME,
                                node_id_gen,
                                span,
                            )),
                        ],
                        tail: Some(Box::new(var_expr("__mc_pending_id", node_id_gen, span))),
                    },
                    ty: (),
                    span,
                },
                span,
            })
        })
        .collect();

    let request_method = MethodDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        sig: MethodSig {
            name: "request".to_string(),
            type_params: Vec::new(),
            self_param: SelfParam {
                id: node_id_gen.new_id(),
                def_id: (),
                mode: ParamMode::In,
                span,
            },
            params: vec![
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "dst".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "kind".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "payload0".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
                parsed::Param {
                    id: node_id_gen.new_id(),
                    ident: "payload1".to_string(),
                    def_id: (),
                    typ: u64_type_expr(node_id_gen, span),
                    mode: ParamMode::In,
                    span,
                },
            ],
            ret_ty_expr: machine_request_result_union_type(node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![
                    parsed::BlockItem::Stmt(let_bind_stmt(
                        "__mc_rt",
                        call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span),
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_if_eq_zero(
                        "__mc_rt",
                        MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Stmt(let_bind_stmt(
                        "__mc_pending_id",
                        call_expr(
                            "__mc_machine_runtime_request_u64",
                            vec![
                                var_expr("__mc_rt", node_id_gen, span),
                                self_field_expr("_id", node_id_gen, span),
                                var_expr("dst", node_id_gen, span),
                                var_expr("kind", node_id_gen, span),
                                var_expr("payload0", node_id_gen, span),
                                var_expr("payload1", node_id_gen, span),
                            ],
                            node_id_gen,
                            span,
                        ),
                        node_id_gen,
                        span,
                    )),
                    parsed::BlockItem::Expr(return_if_eq_zero(
                        "__mc_pending_id",
                        MACHINE_REQUEST_FAILED_TYPE_NAME,
                        node_id_gen,
                        span,
                    )),
                ],
                tail: Some(Box::new(var_expr("__mc_pending_id", node_id_gen, span))),
            },
            ty: (),
            span,
        },
        span,
    };

    let mut method_items = vec![MethodItem::Def(send_method)];
    method_items.extend(typed_send_methods);
    method_items.push(MethodItem::Def(request_method));
    method_items.extend(typed_request_methods);

    TopLevelItem::MethodBlock(MethodBlock {
        id: node_id_gen.new_id(),
        type_name: handle_type_name.to_string(),
        trait_name: None,
        method_items,
        span,
    })
}

fn u64_type_expr(node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
    TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Named {
            ident: "u64".to_string(),
            def_id: (),
            type_args: Vec::new(),
        },
        span,
    }
}

fn unit_expr(node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
    Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::UnitLit,
        ty: (),
        span,
    }
}

fn named_type_expr(name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
    TypeExpr {
        id: node_id_gen.new_id(),
        kind: TypeExprKind::Named {
            ident: name.to_string(),
            def_id: (),
            type_args: Vec::new(),
        },
        span,
    }
}

/// Build a deterministic generated type name for every source state name.
fn build_state_name_map(ts_name: &str, states: &[TypestateState]) -> HashMap<String, String> {
    states
        .iter()
        .map(|state| {
            (
                state.name.clone(),
                format!("__ts_{}_{}", ts_name, state.name.clone()),
            )
        })
        .collect()
}

fn collect_typed_send_specs(
    typestate_handlers: &[TypestateOnHandler],
    states: &[TypestateState],
) -> Vec<TypedSendSpec> {
    use std::collections::BTreeMap;

    let mut selectors = BTreeMap::<String, TypeExpr>::new();
    let mut maybe_insert = |handler: &TypestateOnHandler| {
        // `send(payload)` targets ordinary payload handlers. Response handlers
        // (`for RequestType(...)`) keep request/reply routing semantics and are
        // intentionally excluded from this surface.
        if handler.provenance.is_some() {
            return;
        }
        let key = payload_selector_stable_key(&handler.selector_ty);
        selectors
            .entry(key)
            .or_insert_with(|| handler.selector_ty.clone());
    };

    for handler in typestate_handlers {
        maybe_insert(handler);
    }
    for state in states {
        for item in &state.items {
            if let TypestateStateItem::Handler(handler) = item {
                maybe_insert(handler);
            }
        }
    }

    selectors
        .into_values()
        .enumerate()
        .map(|(idx, selector_ty)| TypedSendSpec {
            selector_ty,
            kind: idx as u64 + 1,
        })
        .collect()
}

fn collect_typed_request_specs(
    typestate_handlers: &[TypestateOnHandler],
    states: &[TypestateState],
) -> Vec<TypedRequestSpec> {
    use std::collections::BTreeMap;

    let mut payloads = BTreeMap::<String, TypeExpr>::new();
    let mut maybe_insert = |handler: &TypestateOnHandler| {
        // `request(dst, payload)` accepts:
        // - direct request handlers: `on RequestTy(...)`
        // - provenance response handlers: `on ResponseTy(...) for RequestTy(origin)`
        let payload_ty = handler
            .provenance
            .as_ref()
            .map(|provenance| provenance.param.typ.clone())
            .unwrap_or_else(|| handler.selector_ty.clone());
        let key = payload_selector_stable_key(&payload_ty);
        payloads.entry(key).or_insert(payload_ty);
    };

    for handler in typestate_handlers {
        maybe_insert(handler);
    }
    for state in states {
        for item in &state.items {
            if let TypestateStateItem::Handler(handler) = item {
                maybe_insert(handler);
            }
        }
    }

    payloads
        .into_values()
        .enumerate()
        .map(|(idx, payload_ty)| TypedRequestSpec {
            payload_ty,
            kind: idx as u64 + 1,
        })
        .collect()
}

fn payload_selector_stable_key(ty: &TypeExpr) -> String {
    format!("payload:{}", type_expr_stable_name(ty))
}

fn type_expr_stable_name(ty: &TypeExpr) -> String {
    match &ty.kind {
        TypeExprKind::Infer => "_".to_string(),
        TypeExprKind::Union { variants } => variants
            .iter()
            .map(type_expr_stable_name)
            .collect::<Vec<_>>()
            .join(" | "),
        TypeExprKind::Named {
            ident, type_args, ..
        } => {
            if type_args.is_empty() {
                ident.clone()
            } else {
                format!(
                    "{}<{}>",
                    ident,
                    type_args
                        .iter()
                        .map(type_expr_stable_name)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
        TypeExprKind::Refined {
            base_ty_expr,
            refinements,
        } => format!(
            "{} where {}",
            type_expr_stable_name(base_ty_expr),
            refinements
                .iter()
                .map(|r| format!("{r:?}"))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        TypeExprKind::Array { elem_ty_expr, dims } => {
            let mut out = type_expr_stable_name(elem_ty_expr);
            for dim in dims {
                out.push('[');
                out.push_str(&dim.to_string());
                out.push(']');
            }
            out
        }
        TypeExprKind::DynArray { elem_ty_expr } => {
            format!("{}[*]", type_expr_stable_name(elem_ty_expr))
        }
        TypeExprKind::Tuple { field_ty_exprs } => format!(
            "({})",
            field_ty_exprs
                .iter()
                .map(type_expr_stable_name)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        TypeExprKind::Slice { elem_ty_expr } => {
            format!("{}[]", type_expr_stable_name(elem_ty_expr))
        }
        TypeExprKind::Heap { elem_ty_expr } => format!("{}^", type_expr_stable_name(elem_ty_expr)),
        TypeExprKind::Ref {
            mutable,
            elem_ty_expr,
        } => {
            if *mutable {
                format!("&mut {}", type_expr_stable_name(elem_ty_expr))
            } else {
                format!("&{}", type_expr_stable_name(elem_ty_expr))
            }
        }
        TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => format!(
            "fn({}) -> {}",
            params
                .iter()
                .map(|p| type_expr_stable_name(&p.ty_expr))
                .collect::<Vec<_>>()
                .join(", "),
            type_expr_stable_name(ret_ty_expr)
        ),
    }
}

struct TypestateAnalysis {
    // Shared `fields { ... }` on the typestate itself.
    shared_fields: Vec<StructDefField>,
    // Unique states only (duplicates are reported as diagnostics).
    states: Vec<TypestateState>,
    // Typestate-level handlers copied onto each lowered state.
    handlers: Vec<TypestateOnHandler>,
    // Source states marked with `@final`.
    final_state_names: HashSet<String>,
    // The selected `new` constructor (first one if multiple are present).
    constructor: Option<FuncDef>,
    // All validation diagnostics for this typestate block.
    errors: Vec<ResolveError>,
}

/// Validate typestate declaration shape and collect pre-lowering facts.
fn analyze_typestate(typestate: &TypestateDef) -> TypestateAnalysis {
    let ts_name = typestate.name.clone();
    let mut errors = Vec::new();

    // 1) Collect typestate-level carried fields.
    let fields_blocks: Vec<&TypestateFields> = typestate
        .items
        .iter()
        .filter_map(|item| match item {
            TypestateItem::Fields(fields) => Some(fields),
            _ => None,
        })
        .collect();

    // V1 allows at most one top-level carried-fields block.
    let shared_fields = fields_blocks
        .first()
        .map(|fields| fields.fields.clone())
        .unwrap_or_default();

    for extra in fields_blocks.iter().skip(1) {
        errors.push(ResolveError::TypestateDuplicateFieldsBlock(
            ts_name.clone(),
            extra.span,
        ));
    }

    // 2) Collect unique states and report duplicates.
    // Keep first state occurrence and report duplicates.
    let mut unique_state_names = HashSet::new();
    let mut final_state_names = HashSet::new();
    let mut states = Vec::new();
    let mut handlers = Vec::new();
    for item in &typestate.items {
        match item {
            TypestateItem::State(state) => {
                if parse_state_attrs(&ts_name, state, &mut errors) {
                    final_state_names.insert(state.name.clone());
                }
                if unique_state_names.insert(state.name.clone()) {
                    states.push(state.clone());
                } else {
                    errors.push(ResolveError::TypestateDuplicateState(
                        ts_name.clone(),
                        state.name.clone(),
                        state.span,
                    ));
                }
            }
            TypestateItem::Handler(handler) => handlers.push(handler.clone()),
            TypestateItem::Fields(_) | TypestateItem::Constructor(_) => {}
        }
    }

    // A typestate with no states is not meaningful.
    if states.is_empty() {
        errors.push(ResolveError::TypestateMissingState(
            ts_name.clone(),
            typestate.span,
        ));
    }

    // 3) Validate each state against typestate-level invariants.
    let state_names: HashSet<String> = states.iter().map(|state| state.name.clone()).collect();
    let shared_field_names: HashSet<String> = shared_fields
        .iter()
        .map(|field| field.name.clone())
        .collect();

    // Validate each state block against global typestate facts.
    for state in &states {
        validate_state_items(
            &ts_name,
            state,
            final_state_names.contains(&state.name),
            &state_names,
            &shared_field_names,
            &mut errors,
        );
    }
    // Typestate-level handlers apply in every state, so they follow the same
    // transition-return contract as state-local handlers.
    for handler in &handlers {
        if !is_valid_on_handler_return(&handler.ret_ty_expr, &state_names) {
            errors.push(ResolveError::TypestateInvalidOnHandlerReturn(
                ts_name.clone(),
                handler.ret_ty_expr.span,
            ));
        }
    }

    // 4) Validate constructor shape (`new`) and select one constructor body
    // for lowering so the pipeline can continue with diagnostics.
    // V1 constructor rule: exactly one `fn new(...)`.
    let new_ctors: Vec<&FuncDef> = typestate
        .items
        .iter()
        .filter_map(|item| match item {
            TypestateItem::Constructor(func) if func.sig.name == "new" => Some(func),
            _ => None,
        })
        .collect();
    let constructor = if let Some(first_new) = new_ctors.first() {
        Some((*first_new).clone())
    } else {
        None
    };

    match new_ctors.as_slice() {
        [] => errors.push(ResolveError::TypestateMissingNew(
            ts_name.clone(),
            typestate.span,
        )),
        [single] => {
            // `new` must return `State` or `State | Error...` (state first).
            if !is_valid_state_return(&single.sig.ret_ty_expr, &state_names) {
                errors.push(ResolveError::TypestateInvalidNewReturn(
                    ts_name.clone(),
                    single.sig.ret_ty_expr.span,
                ));
            }
        }
        [_, rest @ ..] => {
            // Keep first constructor for lowering so we can continue and report
            // additional diagnostics in the same pass.
            for duplicate in rest {
                errors.push(ResolveError::TypestateDuplicateNew(
                    ts_name.clone(),
                    duplicate.sig.span,
                ));
            }
            if !is_valid_state_return(&new_ctors[0].sig.ret_ty_expr, &state_names) {
                errors.push(ResolveError::TypestateInvalidNewReturn(
                    ts_name.clone(),
                    new_ctors[0].sig.ret_ty_expr.span,
                ));
            }
        }
    }

    TypestateAnalysis {
        shared_fields,
        states,
        handlers,
        final_state_names,
        constructor,
        errors,
    }
}

fn parse_state_attrs(
    ts_name: &str,
    state: &TypestateState,
    errors: &mut Vec<ResolveError>,
) -> bool {
    let mut seen = HashSet::new();
    let mut is_final = false;
    for attr in &state.attrs {
        if !seen.insert(attr.name.clone()) {
            errors.push(ResolveError::AttrDuplicate(attr.name.clone(), attr.span));
            continue;
        }
        match attr.name.as_str() {
            "final" => {
                if !attr.args.is_empty() {
                    errors.push(ResolveError::AttrWrongArgCount(
                        attr.name.clone(),
                        0,
                        attr.args.len(),
                        attr.span,
                    ));
                    continue;
                }
                is_final = true;
            }
            _ => errors.push(ResolveError::TypestateUnknownStateAttribute(
                ts_name.to_string(),
                state.name.clone(),
                attr.name.clone(),
                attr.span,
            )),
        }
    }
    is_final
}

fn validate_state_items(
    ts_name: &str,
    state: &TypestateState,
    is_final_state: bool,
    state_names: &HashSet<String>,
    shared_field_names: &HashSet<String>,
    errors: &mut Vec<ResolveError>,
) {
    // 1) Validate per-state fields blocks and shadowing rules.
    // V1 allows a single `fields { ... }` block per state.
    let fields_blocks: Vec<&TypestateFields> = state
        .items
        .iter()
        .filter_map(|item| match item {
            TypestateStateItem::Fields(fields) => Some(fields),
            _ => None,
        })
        .collect();
    for extra in fields_blocks.iter().skip(1) {
        errors.push(ResolveError::TypestateDuplicateStateFieldsBlock(
            ts_name.to_string(),
            state.name.clone(),
            extra.span,
        ));
    }
    if let Some(local_fields) = fields_blocks.first() {
        // Local state fields must not shadow carried fields in V1.
        for field in &local_fields.fields {
            if shared_field_names.contains(&field.name) {
                errors.push(ResolveError::TypestateStateFieldShadowsCarriedField(
                    ts_name.to_string(),
                    state.name.clone(),
                    field.name.clone(),
                    field.span,
                ));
            }
        }
    }

    if is_final_state {
        for item in &state.items {
            match item {
                TypestateStateItem::Method(method) => {
                    errors.push(ResolveError::TypestateFinalStateHasTransition(
                        ts_name.to_string(),
                        state.name.clone(),
                        method.sig.span,
                    ));
                }
                TypestateStateItem::Handler(handler) => {
                    errors.push(ResolveError::TypestateFinalStateHasHandler(
                        ts_name.to_string(),
                        state.name.clone(),
                        handler.span,
                    ));
                }
                TypestateStateItem::Fields(_) => {}
            }
        }
        return;
    }

    // 2) Validate transition signatures for this state.
    // Transition names must be unique per source state.
    let mut seen_methods = HashSet::new();
    for item in &state.items {
        match item {
            TypestateStateItem::Method(method) => {
                if !seen_methods.insert(method.sig.name.clone()) {
                    errors.push(ResolveError::TypestateDuplicateTransition(
                        ts_name.to_string(),
                        state.name.clone(),
                        method.sig.name.clone(),
                        method.sig.span,
                    ));
                }

                // `self` is implicit in typestate transitions.
                if let Some(self_param) =
                    method.sig.params.iter().find(|param| param.ident == "self")
                {
                    errors.push(ResolveError::TypestateExplicitSelfNotAllowed(
                        ts_name.to_string(),
                        state.name.clone(),
                        method.sig.name.clone(),
                        self_param.span,
                    ));
                }

                // Transition success return follows the same shape rule as `new`.
                if !is_valid_state_return(&method.sig.ret_ty_expr, state_names) {
                    errors.push(ResolveError::TypestateInvalidTransitionReturn(
                        ts_name.to_string(),
                        state.name.clone(),
                        method.sig.name.clone(),
                        method.sig.ret_ty_expr.span,
                    ));
                }
            }
            TypestateStateItem::Handler(handler) => {
                if !is_valid_on_handler_return(&handler.ret_ty_expr, state_names) {
                    errors.push(ResolveError::TypestateInvalidStateOnHandlerReturn(
                        ts_name.to_string(),
                        state.name.clone(),
                        handler.ret_ty_expr.span,
                    ));
                }
            }
            TypestateStateItem::Fields(_) => {}
        }
    }
}

fn final_state_marker_method_def(span: Span, node_id_gen: &mut NodeIdGen) -> MethodDef {
    // Marker used by machine planning to classify generated state types as
    // terminal without leaking source-only typestate attributes downstream.
    MethodDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        sig: MethodSig {
            name: GENERATED_FINAL_STATE_MARKER.to_string(),
            type_params: Vec::new(),
            self_param: SelfParam {
                id: node_id_gen.new_id(),
                def_id: (),
                mode: ParamMode::In,
                span,
            },
            params: Vec::new(),
            // Keep marker typing explicit to avoid introducing unresolved
            // inference variables in final-state-only method blocks.
            ret_ty_expr: named_type_expr("()", node_id_gen, span),
            span,
        },
        body: Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::UnitLit,
            ty: (),
            span,
        },
        span,
    }
}

fn is_valid_state_return(ret_ty: &TypeExpr, state_names: &HashSet<String>) -> bool {
    match &ret_ty.kind {
        TypeExprKind::Named { ident, .. } => state_names.contains(ident),
        TypeExprKind::Union { variants } => {
            // V1 convention: success state is always the first union variant.
            let Some(first) = variants.first() else {
                return false;
            };
            matches!(
                &first.kind,
                TypeExprKind::Named { ident, .. } if state_names.contains(ident)
            )
        }
        _ => false,
    }
}

fn is_valid_on_handler_return(ret_ty: &TypeExpr, state_names: &HashSet<String>) -> bool {
    match &ret_ty.kind {
        // `stay` is handler-only sugar for "current state".
        TypeExprKind::Named { ident, .. } if ident == "stay" => true,
        _ => is_valid_state_return_or_stay_union(ret_ty, state_names),
    }
}

fn is_valid_state_return_or_stay_union(ret_ty: &TypeExpr, state_names: &HashSet<String>) -> bool {
    match &ret_ty.kind {
        TypeExprKind::Named { ident, .. } => state_names.contains(ident),
        TypeExprKind::Union { variants } => {
            let Some(first) = variants.first() else {
                return false;
            };
            match &first.kind {
                TypeExprKind::Named { ident, .. } => ident == "stay" || state_names.contains(ident),
                _ => false,
            }
        }
        _ => false,
    }
}

/// Returns the state-local `fields` block if present, otherwise empty.
fn collect_first_state_fields_block(items: &[TypestateStateItem]) -> Vec<StructDefField> {
    items
        .iter()
        .find_map(|item| match item {
            TypestateStateItem::Fields(TypestateFields { fields, .. }) => Some(fields.clone()),
            _ => None,
        })
        .unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Lowering helpers (validated source typestate -> regular parsed tree)
// ---------------------------------------------------------------------------

fn lower_state_method(
    mut method: MethodDefSource,
    node_id_gen: &mut NodeIdGen,
    source_state_names: &HashSet<String>,
    local_fields_by_state: &HashMap<String, Vec<StructDefField>>,
    carried_field_names: &[String],
    state_name_map: &HashMap<String, String>,
    span: crate::core::diag::Span,
) -> MethodDef {
    // Run carried-field transition rewrites while state names are still in
    // source form (`Disconnected`, `Connected`, ...).
    rewrite_transition_literals_in_method(
        &mut method,
        node_id_gen,
        source_state_names,
        local_fields_by_state,
        carried_field_names,
    );
    // Then rewrite state names to generated nominal names.
    rewrite_state_refs_in_func(&mut method, state_name_map);
    MethodDef {
        id: method.id,
        def_id: method.def_id,
        attrs: method.attrs,
        sig: MethodSig {
            name: method.sig.name,
            type_params: method.sig.type_params,
            self_param: SelfParam {
                id: node_id_gen.new_id(),
                def_id: (),
                mode: ParamMode::Sink,
                span,
            },
            params: method.sig.params,
            ret_ty_expr: method.sig.ret_ty_expr,
            span: method.sig.span,
        },
        body: method.body,
        span: method.span,
    }
}

// Parsed typestate methods are represented as function defs before lowering.
type MethodDefSource = FuncDef;

fn clone_type_expr_with_new_ids(ty: &TypeExpr, node_id_gen: &mut NodeIdGen) -> TypeExpr {
    let kind = match &ty.kind {
        TypeExprKind::Infer => TypeExprKind::Infer,
        TypeExprKind::Union { variants } => TypeExprKind::Union {
            variants: variants
                .iter()
                .map(|variant| clone_type_expr_with_new_ids(variant, node_id_gen))
                .collect(),
        },
        TypeExprKind::Named {
            ident,
            def_id,
            type_args,
        } => TypeExprKind::Named {
            ident: ident.clone(),
            def_id: *def_id,
            type_args: type_args
                .iter()
                .map(|arg| clone_type_expr_with_new_ids(arg, node_id_gen))
                .collect(),
        },
        TypeExprKind::Refined {
            base_ty_expr,
            refinements,
        } => TypeExprKind::Refined {
            base_ty_expr: Box::new(clone_type_expr_with_new_ids(base_ty_expr, node_id_gen)),
            refinements: refinements.clone(),
        },
        TypeExprKind::Array { elem_ty_expr, dims } => TypeExprKind::Array {
            elem_ty_expr: Box::new(clone_type_expr_with_new_ids(elem_ty_expr, node_id_gen)),
            dims: dims.clone(),
        },
        TypeExprKind::DynArray { elem_ty_expr } => TypeExprKind::DynArray {
            elem_ty_expr: Box::new(clone_type_expr_with_new_ids(elem_ty_expr, node_id_gen)),
        },
        TypeExprKind::Tuple { field_ty_exprs } => TypeExprKind::Tuple {
            field_ty_exprs: field_ty_exprs
                .iter()
                .map(|expr| clone_type_expr_with_new_ids(expr, node_id_gen))
                .collect(),
        },
        TypeExprKind::Slice { elem_ty_expr } => TypeExprKind::Slice {
            elem_ty_expr: Box::new(clone_type_expr_with_new_ids(elem_ty_expr, node_id_gen)),
        },
        TypeExprKind::Heap { elem_ty_expr } => TypeExprKind::Heap {
            elem_ty_expr: Box::new(clone_type_expr_with_new_ids(elem_ty_expr, node_id_gen)),
        },
        TypeExprKind::Ref {
            mutable,
            elem_ty_expr,
        } => TypeExprKind::Ref {
            mutable: *mutable,
            elem_ty_expr: Box::new(clone_type_expr_with_new_ids(elem_ty_expr, node_id_gen)),
        },
        TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => TypeExprKind::Fn {
            params: params
                .iter()
                .map(|param| parsed::FnTypeParam {
                    mode: param.mode.clone(),
                    ty_expr: clone_type_expr_with_new_ids(&param.ty_expr, node_id_gen),
                })
                .collect(),
            ret_ty_expr: Box::new(clone_type_expr_with_new_ids(ret_ty_expr, node_id_gen)),
        },
    };

    TypeExpr {
        id: node_id_gen.new_id(),
        kind,
        span: ty.span,
    }
}

fn clone_param_with_new_ids(param: &parsed::Param, node_id_gen: &mut NodeIdGen) -> parsed::Param {
    parsed::Param {
        id: node_id_gen.new_id(),
        ident: param.ident.clone(),
        def_id: (),
        typ: clone_type_expr_with_new_ids(&param.typ, node_id_gen),
        mode: param.mode.clone(),
        span: param.span,
    }
}

fn lower_spawn_func(
    spawn_name: &str,
    handle_type_name: &str,
    descriptor_id_helper_name: &str,
    ctor: &FuncDef,
    state_name_map: &HashMap<String, String>,
    node_id_gen: &mut NodeIdGen,
) -> FuncDef {
    // V1 managed constructor flow:
    // 1) evaluate `new(...)` to enforce mirror-forward constructor contract,
    // 2) allocate/bind/start a runtime machine slot,
    // 3) return a machine handle wrapper on success.
    fn var_expr(name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> Expr {
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Var {
                ident: name.to_string(),
                def_id: (),
            },
            ty: (),
            span,
        }
    }

    fn call_expr(
        callee_name: &str,
        args: Vec<Expr>,
        node_id_gen: &mut NodeIdGen,
        span: Span,
    ) -> Expr {
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Call {
                callee: Box::new(var_expr(callee_name, node_id_gen, span)),
                args: args
                    .into_iter()
                    .map(|expr| CallArg {
                        mode: CallArgMode::Default,
                        expr,
                        init: InitInfo::default(),
                        span,
                    })
                    .collect(),
            },
            ty: (),
            span,
        }
    }

    fn let_bind_stmt(
        ident: &str,
        value: Expr,
        node_id_gen: &mut NodeIdGen,
        span: Span,
    ) -> StmtExpr {
        StmtExpr {
            id: node_id_gen.new_id(),
            kind: StmtExprKind::LetBind {
                pattern: BindPattern {
                    id: node_id_gen.new_id(),
                    kind: BindPatternKind::Name {
                        ident: ident.to_string(),
                        def_id: (),
                    },
                    span,
                },
                decl_ty: None,
                value: Box::new(value),
            },
            ty: (),
            span,
        }
    }

    fn return_stmt(value: Expr, node_id_gen: &mut NodeIdGen, span: Span) -> StmtExpr {
        StmtExpr {
            id: node_id_gen.new_id(),
            kind: StmtExprKind::Return {
                value: Some(Box::new(value)),
            },
            ty: (),
            span,
        }
    }

    fn make_error_return_if_zero(
        value_var: &str,
        error_type_name: &str,
        node_id_gen: &mut NodeIdGen,
        span: Span,
    ) -> Expr {
        let cond = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::BinOp {
                left: Box::new(var_expr(value_var, node_id_gen, span)),
                op: crate::core::tree::BinaryOp::Eq,
                right: Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::IntLit(0),
                    ty: (),
                    span,
                }),
            },
            ty: (),
            span,
        };
        let then_body = Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Block {
                items: vec![parsed::BlockItem::Stmt(return_stmt(
                    Expr {
                        id: node_id_gen.new_id(),
                        kind: ExprKind::StructLit {
                            name: error_type_name.to_string(),
                            type_args: Vec::new(),
                            fields: Vec::new(),
                        },
                        ty: (),
                        span,
                    },
                    node_id_gen,
                    span,
                ))],
                tail: None,
            },
            ty: (),
            span,
        };
        Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_body: Box::new(then_body),
                else_body: Box::new(Expr {
                    id: node_id_gen.new_id(),
                    kind: ExprKind::Block {
                        items: Vec::new(),
                        tail: None,
                    },
                    ty: (),
                    span,
                }),
            },
            ty: (),
            span,
        }
    }

    let span = ctor.span;
    let mut params = Vec::new();
    params.extend(
        ctor.sig
            .params
            .iter()
            .map(|param| clone_param_with_new_ids(param, node_id_gen)),
    );

    let mut state_names: Vec<String> = state_name_map.values().cloned().collect();
    state_names.sort();
    let initial_state_name = match &ctor.sig.ret_ty_expr.kind {
        TypeExprKind::Named { ident, .. } => ident.clone(),
        TypeExprKind::Union { variants } => variants
            .first()
            .and_then(|variant| match &variant.kind {
                TypeExprKind::Named { ident, .. } => Some(ident.clone()),
                _ => None,
            })
            .unwrap_or_else(|| state_names.first().cloned().unwrap_or_default()),
        _ => state_names.first().cloned().unwrap_or_default(),
    };
    let initial_state_tag = state_names
        .iter()
        .position(|name| name == &initial_state_name)
        .map(|idx| idx as u64 + 1)
        .unwrap_or(1);

    let ctor_call_args: Vec<Expr> = ctor
        .sig
        .params
        .iter()
        .map(|param| var_expr(&param.ident, node_id_gen, span))
        .collect();
    let ctor_call = call_expr(&ctor.sig.name, ctor_call_args, node_id_gen, span);
    let runtime_current_call = call_expr(MANAGED_RUNTIME_CURRENT_FN, Vec::new(), node_id_gen, span);
    let spawn_call = call_expr(
        "__mc_machine_runtime_spawn_u64",
        vec![
            var_expr("__mc_rt", node_id_gen, span),
            Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::IntLit(MANAGED_RUNTIME_DEFAULT_MAILBOX_CAP),
                ty: (),
                span,
            },
        ],
        node_id_gen,
        span,
    );
    let descriptor_id_call = call_expr(descriptor_id_helper_name, Vec::new(), node_id_gen, span);
    let bind_call = call_expr(
        "__mc_machine_runtime_bind_descriptor_u64",
        vec![
            var_expr("__mc_rt", node_id_gen, span),
            var_expr("__mc_machine_id", node_id_gen, span),
            var_expr("__mc_descriptor_id", node_id_gen, span),
            Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::IntLit(initial_state_tag),
                ty: (),
                span,
            },
        ],
        node_id_gen,
        span,
    );
    let start_call = call_expr(
        "__mc_machine_runtime_start_u64",
        vec![
            var_expr("__mc_rt", node_id_gen, span),
            var_expr("__mc_machine_id", node_id_gen, span),
        ],
        node_id_gen,
        span,
    );

    let body = Expr {
        id: node_id_gen.new_id(),
        kind: ExprKind::Block {
            items: vec![
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_rt",
                    runtime_current_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Expr(make_error_return_if_zero(
                    "__mc_rt",
                    MACHINE_SPAWN_FAILED_TYPE_NAME,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_initial_state",
                    ctor_call,
                    node_id_gen,
                    span,
                )),
                // Keep the constructor value live so side effects and argument
                // evaluation semantics match direct `new(...)` usage.
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_machine_id",
                    spawn_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Expr(make_error_return_if_zero(
                    "__mc_machine_id",
                    MACHINE_SPAWN_FAILED_TYPE_NAME,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_descriptor_id",
                    descriptor_id_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_bind_status",
                    bind_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Expr(make_error_return_if_zero(
                    "__mc_bind_status",
                    MACHINE_BIND_FAILED_TYPE_NAME,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Stmt(let_bind_stmt(
                    "__mc_start_status",
                    start_call,
                    node_id_gen,
                    span,
                )),
                parsed::BlockItem::Expr(make_error_return_if_zero(
                    "__mc_start_status",
                    MACHINE_START_FAILED_TYPE_NAME,
                    node_id_gen,
                    span,
                )),
            ],
            tail: Some(Box::new(Expr {
                id: node_id_gen.new_id(),
                kind: ExprKind::StructLit {
                    name: handle_type_name.to_string(),
                    type_args: Vec::new(),
                    fields: vec![StructLitField {
                        id: node_id_gen.new_id(),
                        name: "_id".to_string(),
                        value: var_expr("__mc_machine_id", node_id_gen, span),
                        span,
                    }],
                },
                ty: (),
                span,
            })),
        },
        ty: (),
        span,
    };

    FuncDef {
        id: node_id_gen.new_id(),
        def_id: (),
        attrs: Vec::new(),
        sig: parsed::FunctionSig {
            name: spawn_name.to_string(),
            type_params: Vec::new(),
            params,
            ret_ty_expr: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Union {
                    variants: vec![
                        named_type_expr(handle_type_name, node_id_gen, span),
                        named_type_expr(MACHINE_SPAWN_FAILED_TYPE_NAME, node_id_gen, span),
                        named_type_expr(MACHINE_BIND_FAILED_TYPE_NAME, node_id_gen, span),
                        named_type_expr(MACHINE_START_FAILED_TYPE_NAME, node_id_gen, span),
                    ],
                },
                span,
            },
            span,
        },
        body,
        span,
    }
}

fn lower_handler_to_method_source(
    handler: &TypestateOnHandler,
    state_name: &str,
    next_index: &mut usize,
    node_id_gen: &mut NodeIdGen,
) -> MethodDefSource {
    let provenance_site_suffix = handler
        .provenance
        .as_ref()
        .and_then(|provenance| provenance.request_site_label.as_deref())
        .map(|label| format!("__site_{label}"))
        .unwrap_or_default();
    // Preserve source-level provenance labels in generated handler symbols so
    // downstream machine-plan synthesis can recover deterministic site filters.
    let name = format!("__ts_on_{}{}", *next_index, provenance_site_suffix);
    *next_index += 1;
    let mut params = vec![parsed::Param {
        id: node_id_gen.new_id(),
        ident: "__event".to_string(),
        def_id: (),
        typ: handler.selector_ty.clone(),
        mode: ParamMode::In,
        span: handler.selector_ty.span,
    }];

    if handler.provenance.is_some() {
        // `for RequestType(binding)` handlers need response correlation id at
        // runtime so lowering can recover the originating request payload.
        // We model this via a hidden `Pending<Selector>` parameter.
        params.push(parsed::Param {
            id: node_id_gen.new_id(),
            ident: "__pending".to_string(),
            def_id: (),
            typ: TypeExpr {
                id: node_id_gen.new_id(),
                kind: TypeExprKind::Named {
                    ident: "Pending".to_string(),
                    def_id: (),
                    type_args: vec![handler.selector_ty.clone()],
                },
                span: handler.selector_ty.span,
            },
            mode: ParamMode::In,
            span: handler.selector_ty.span,
        });
        if let Some(provenance) = &handler.provenance {
            params.push(provenance.param.clone());
        }
    }
    params.extend(handler.params.clone());
    let mut body = handler.body.clone();
    rewrite_handler_command_sugar(&mut body);
    let mut ret_ty_expr =
        rewrite_handler_return_type(&handler.ret_ty_expr, state_name, node_id_gen);
    if handler_return_uses_stay(&handler.ret_ty_expr) {
        inject_self_tail_for_stay(&mut body, node_id_gen);
    }
    MethodDefSource {
        id: handler.id,
        def_id: (),
        attrs: Vec::new(),
        sig: parsed::FunctionSig {
            name,
            type_params: Vec::new(),
            params,
            ret_ty_expr: {
                // Keep node IDs unique after rewriting `stay` -> concrete state.
                ret_ty_expr.id = node_id_gen.new_id();
                ret_ty_expr
            },
            span: handler.span,
        },
        body,
        span: handler.span,
    }
}

fn type_expr_is_stay(ty: &TypeExpr) -> bool {
    matches!(
        &ty.kind,
        TypeExprKind::Named {
            ident,
            type_args,
            ..
        } if ident == "stay" && type_args.is_empty()
    )
}

fn rewrite_handler_return_type(
    ret_ty_expr: &TypeExpr,
    state_name: &str,
    node_id_gen: &mut NodeIdGen,
) -> TypeExpr {
    fn concrete_state_ty(state_name: &str, node_id_gen: &mut NodeIdGen, span: Span) -> TypeExpr {
        TypeExpr {
            id: node_id_gen.new_id(),
            kind: TypeExprKind::Named {
                ident: state_name.to_string(),
                def_id: (),
                type_args: Vec::new(),
            },
            span,
        }
    }

    if type_expr_is_stay(ret_ty_expr) {
        return concrete_state_ty(state_name, node_id_gen, ret_ty_expr.span);
    }

    let TypeExprKind::Union { variants } = &ret_ty_expr.kind else {
        return ret_ty_expr.clone();
    };
    let Some(first) = variants.first() else {
        return ret_ty_expr.clone();
    };
    if !type_expr_is_stay(first) {
        return ret_ty_expr.clone();
    }

    let mut rewritten = ret_ty_expr.clone();
    let TypeExprKind::Union { variants } = &mut rewritten.kind else {
        return rewritten;
    };
    variants[0] = concrete_state_ty(state_name, node_id_gen, first.span);
    rewritten
}

fn handler_return_uses_stay(ret_ty_expr: &TypeExpr) -> bool {
    if type_expr_is_stay(ret_ty_expr) {
        return true;
    }
    let TypeExprKind::Union { variants } = &ret_ty_expr.kind else {
        return false;
    };
    variants.first().is_some_and(type_expr_is_stay)
}

fn inject_self_tail_for_stay(body: &mut Expr, node_id_gen: &mut NodeIdGen) {
    let ExprKind::Block { tail, .. } = &mut body.kind else {
        return;
    };
    if tail.is_none() {
        *tail = Some(Box::new(Expr {
            id: node_id_gen.new_id(),
            kind: ExprKind::Var {
                ident: "self".to_string(),
                def_id: (),
            },
            ty: (),
            span: body.span,
        }));
    }
}

fn rewrite_handler_command_sugar(body: &mut Expr) {
    let mut rewriter = HandlerCommandSugarRewriter;
    rewriter.visit_expr(body);
}

struct HandlerCommandSugarRewriter;

impl VisitorMut<()> for HandlerCommandSugarRewriter {
    fn visit_expr(&mut self, expr: &mut Expr) {
        visit_mut::walk_expr(self, expr);

        let ExprKind::Call { callee, args } = &mut expr.kind else {
            return;
        };
        let ExprKind::Var { ident, .. } = &callee.kind else {
            return;
        };
        let Some((command, request_site_label)) = parse_handler_command_name(ident) else {
            return;
        };
        if args.len() != 2 || !args.iter().all(|arg| arg.mode == CallArgMode::Default) {
            return;
        }

        let mut call_args = std::mem::take(args);
        let payload = call_args
            .pop()
            .expect("call arg shape checked by typestate handler sugar rewrite")
            .expr;
        let to = call_args
            .pop()
            .expect("call arg shape checked by typestate handler sugar rewrite")
            .expr;
        let kind = if command == "send" {
            parsed::EmitKind::Send {
                to: Box::new(to),
                payload: Box::new(payload),
            }
        } else {
            parsed::EmitKind::Request {
                to: Box::new(to),
                payload: Box::new(payload),
                request_site_label,
            }
        };
        expr.kind = ExprKind::Emit { kind };
    }
}

fn parse_handler_command_name(ident: &str) -> Option<(&str, Option<String>)> {
    if ident == "send" {
        return Some(("send", None));
    }
    if ident == "request" {
        return Some(("request", None));
    }
    // Labeled request sugar is parsed as synthetic callee `request:label(...)`.
    let (command, label) = ident.split_once(':')?;
    if command != "request" || label.is_empty() {
        return None;
    }
    Some(("request", Some(label.to_string())))
}

fn rewrite_transition_literals_in_method(
    method: &mut FuncDef,
    node_id_gen: &mut NodeIdGen,
    source_state_names: &HashSet<String>,
    local_fields_by_state: &HashMap<String, Vec<StructDefField>>,
    carried_field_names: &[String],
) {
    // This rewrite is intentionally method-local; it should not affect
    // constructor bodies or unrelated free functions.
    let mut rewriter = TransitionLiteralRewriter {
        node_id_gen,
        source_state_names,
        local_fields_by_state,
        carried_field_names,
    };
    rewriter.visit_expr(&mut method.body);
}

struct TransitionLiteralRewriter<'a> {
    // Used to synthesize AST nodes while preserving node-id uniqueness.
    node_id_gen: &'a mut NodeIdGen,
    // Source state names for quick match on `State` / `State { ... }`.
    source_state_names: &'a HashSet<String>,
    // Local fields for each state. Needed for shorthand `State` legality.
    local_fields_by_state: &'a HashMap<String, Vec<StructDefField>>,
    // Carried field names copied from typestate-level `fields { ... }`.
    carried_field_names: &'a [String],
}

impl TransitionLiteralRewriter<'_> {
    // Builds `self.<field>` expression used for implicit carried-field transfer.
    fn build_self_field_expr(&mut self, field_name: &str, span: crate::core::diag::Span) -> Expr {
        Expr {
            id: self.node_id_gen.new_id(),
            kind: ExprKind::StructField {
                target: Box::new(Expr {
                    id: self.node_id_gen.new_id(),
                    kind: ExprKind::Var {
                        ident: "self".to_string(),
                        // Resolved later in the normal resolver pass.
                        def_id: (),
                    },
                    ty: (),
                    span,
                }),
                field: field_name.to_string(),
            },
            ty: (),
            span,
        }
    }

    fn carried_struct_fields(&mut self, span: crate::core::diag::Span) -> Vec<StructLitField> {
        // Materialize all carried fields as `name: self.name`.
        self.carried_field_names
            .iter()
            .cloned()
            .map(|name| StructLitField {
                id: self.node_id_gen.new_id(),
                name: name.clone(),
                value: self.build_self_field_expr(&name, span),
                span,
            })
            .collect()
    }
}

impl VisitorMut<()> for TransitionLiteralRewriter<'_> {
    fn visit_expr(&mut self, expr: &mut Expr) {
        // Visit children first so nested transition literals are rewritten
        // before parent transforms potentially replace this node.
        visit_mut::walk_expr(self, expr);

        match &mut expr.kind {
            ExprKind::StructLit { name, fields, .. } if self.source_state_names.contains(name) => {
                // `State { ... }`: inject only missing carried fields.
                let existing: HashSet<String> =
                    fields.iter().map(|field| field.name.clone()).collect();
                for field_name in self.carried_field_names {
                    if existing.contains(field_name) {
                        // User-provided value wins over implicit carry.
                        continue;
                    }
                    fields.push(StructLitField {
                        id: self.node_id_gen.new_id(),
                        name: field_name.clone(),
                        value: self.build_self_field_expr(field_name, expr.span),
                        span: expr.span,
                    });
                }
            }
            ExprKind::Var { ident, .. } if self.source_state_names.contains(ident) => {
                // `State` shorthand is only valid when the target state has no
                // local fields. We rewrite it to an explicit struct literal.
                if self
                    .local_fields_by_state
                    .get(ident)
                    .is_some_and(|fields| fields.is_empty())
                {
                    expr.kind = ExprKind::StructLit {
                        name: ident.clone(),
                        type_args: Vec::new(),
                        fields: self.carried_struct_fields(expr.span),
                    };
                }
            }
            _ => {}
        }
    }
}

// ---------------------------------------------------------------------------
// Name rewrites + call rewrites (source spellings -> generated spellings)
// ---------------------------------------------------------------------------

fn rewrite_state_refs_in_func(func: &mut FuncDef, state_name_map: &HashMap<String, String>) {
    // Generic state-name rewrite used by lowered constructors and methods.
    let mut rewriter = StateRefRewriter::new(state_name_map);
    rewriter.visit_func_def(func);
}

struct StateRefRewriter<'a> {
    state_name_map: &'a HashMap<String, String>,
}

impl<'a> StateRefRewriter<'a> {
    fn new(state_name_map: &'a HashMap<String, String>) -> Self {
        Self { state_name_map }
    }

    fn rewrite_name(&self, name: &mut String) {
        // Only rewrite known state names; non-state symbols are untouched.
        if let Some(new_name) = self.state_name_map.get(name) {
            *name = new_name.clone();
        }
    }
}

impl VisitorMut<()> for StateRefRewriter<'_> {
    fn visit_type_expr(&mut self, type_expr: &mut TypeExpr) {
        if let TypeExprKind::Named { ident, .. } = &mut type_expr.kind {
            self.rewrite_name(ident);
        }
        visit_mut::walk_type_expr(self, type_expr);
    }

    fn visit_bind_pattern(&mut self, pattern: &mut BindPattern) {
        if let BindPatternKind::Struct { name, .. } = &mut pattern.kind {
            self.rewrite_name(name);
        }
        visit_mut::walk_bind_pattern(self, pattern);
    }

    fn visit_match_pattern(&mut self, pattern: &mut MatchPattern) {
        if let MatchPattern::EnumVariant {
            enum_name: Some(enum_name),
            ..
        } = pattern
        {
            self.rewrite_name(enum_name);
        }
        visit_mut::walk_match_pattern(self, pattern);
    }

    fn visit_stmt_expr(&mut self, stmt: &mut StmtExpr) {
        // `decl_ty` in let/var is not traversed by generic walk; handle it here.
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

    fn visit_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::ArrayLit {
                elem_ty: Some(elem_ty),
                ..
            } => self.visit_type_expr(elem_ty),
            ExprKind::StructLit { name, .. } => self.rewrite_name(name),
            ExprKind::EnumVariant { enum_name, .. } => self.rewrite_name(enum_name),
            _ => {}
        }
        visit_mut::walk_expr(self, expr);
    }
}

// ---------------------------------------------------------------------------
// Constructor-only entry rule checks
// ---------------------------------------------------------------------------

fn rewrite_constructor_invocations(
    module: &mut Module,
    ctor_by_typestate: &HashMap<String, String>,
    spawn_by_typestate: &HashMap<String, String>,
    node_id_gen: &mut NodeIdGen,
) -> Option<Span> {
    // Global rewrite after all typestate declarations are lowered so call sites
    // can target the generated constructor symbol.
    let mut rewriter = CtorCallRewriter {
        ctor_by_typestate,
        spawn_by_typestate,
        node_id_gen,
        first_spawn_call_span: None,
    };
    rewriter.visit_module(module);
    rewriter.first_spawn_call_span
}

struct CtorCallRewriter<'a> {
    // Map source typestate name -> generated constructor function name.
    ctor_by_typestate: &'a HashMap<String, String>,
    // Map source typestate name -> generated managed spawn function name.
    spawn_by_typestate: &'a HashMap<String, String>,
    node_id_gen: &'a mut NodeIdGen,
    first_spawn_call_span: Option<Span>,
}

impl VisitorMut<()> for CtorCallRewriter<'_> {
    fn visit_expr(&mut self, expr: &mut Expr) {
        // Post-order traversal so nested ctor calls are rewritten first.
        visit_mut::walk_expr(self, expr);

        if let ExprKind::EnumVariant {
            enum_name,
            variant,
            payload,
            ..
        } = &mut expr.kind
            && let Some(callee_name) = if variant == "new" {
                self.ctor_by_typestate.get(enum_name)
            } else if variant == "spawn" {
                self.spawn_by_typestate.get(enum_name)
            } else {
                None
            }
        {
            // Parsed form of `Type::new(a, b)` arrives as enum-variant syntax in
            // the parsed tree. Lower it to a plain call expression.
            if variant == "spawn" && self.first_spawn_call_span.is_none() {
                self.first_spawn_call_span = Some(expr.span);
            }
            let payload = std::mem::take(payload);
            let args = payload
                .into_iter()
                .map(|arg| CallArg {
                    // `Type::new(a, b)` payload arguments map directly to call args.
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
                        ident: callee_name.clone(),
                        def_id: (),
                    },
                    ty: (),
                    span: expr.span,
                }),
                args,
            };
        }
    }
}

fn find_external_state_literal_errors(
    module: &Module,
    source_state_names: &HashSet<String>,
    generated_state_names: &HashSet<String>,
) -> Vec<ResolveError> {
    // Run after lowering+rewrites so we can check both source and generated
    // state names, independent of textual form.
    let mut checker = ExternalStateLiteralChecker {
        source_state_names,
        generated_state_names,
        errors: Vec::new(),
        allow_state_literals: false,
    };
    checker.visit_module(module);
    checker.errors
}

struct ExternalStateLiteralChecker<'a> {
    source_state_names: &'a HashSet<String>,
    generated_state_names: &'a HashSet<String>,
    errors: Vec<ResolveError>,
    // Scoped gate: enabled only while traversing typestate ctor/transition bodies.
    allow_state_literals: bool,
}

impl ExternalStateLiteralChecker<'_> {
    fn is_state_name(&self, name: &str) -> bool {
        // During migration, both source and generated names can appear.
        self.source_state_names.contains(name) || self.generated_state_names.contains(name)
    }
}

impl Visitor<()> for ExternalStateLiteralChecker<'_> {
    fn visit_func_def(&mut self, func_def: &FuncDef) {
        let prev = self.allow_state_literals;
        // Generated typestate constructor function body is allowed to construct states.
        self.allow_state_literals = func_def.sig.name.starts_with("__ts_ctor_");
        visit::walk_func_def(self, func_def);
        self.allow_state_literals = prev;
    }

    fn visit_method_block(&mut self, method_block: &MethodBlock) {
        let prev = self.allow_state_literals;
        // Method blocks on generated state types are transition bodies.
        self.allow_state_literals = self.generated_state_names.contains(&method_block.type_name);
        visit::walk_method_block(self, method_block);
        self.allow_state_literals = prev;
    }

    fn visit_expr(&mut self, expr: &Expr) {
        // Outside ctor/transition scopes, state struct literals are rejected.
        if !self.allow_state_literals
            && let ExprKind::StructLit { name, .. } = &expr.kind
            && self.is_state_name(name)
        {
            self.errors
                .push(ResolveError::TypestateStateLiteralOutsideTypestate(
                    name.clone(),
                    expr.span,
                ));
        }
        visit::walk_expr(self, expr);
    }
}
