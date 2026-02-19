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
    self, BindPattern, BindPatternKind, CallArg, EnumDefVariant, Expr, ExprKind, FuncDecl, FuncDef,
    MethodBlock, MethodDef, MethodItem, MethodSig, Module, SelfParam, StmtExpr, StmtExprKind,
    StructDefField, StructLitField, TopLevelItem, TypeDef, TypeDefKind, TypeExpr, TypeExprKind,
    TypestateDef, TypestateItem, TypestateOnHandler, TypestateState, TypestateStateItem,
};
use crate::core::tree::visit::{self, Visitor};
use crate::core::tree::visit_mut::{self, VisitorMut};
use crate::core::tree::{CallArgMode, InitInfo, ParamMode};

mod analysis;
mod ast_build;
mod handlers;
mod managed_api;
mod managed_entrypoint;
mod names;
mod rewrite_handles;
mod state_lowering;

use analysis::analyze_typestate;
use handlers::{
    clone_param_with_new_ids, clone_type_expr_with_new_ids, lower_handler_to_method_source,
};
use managed_api::{lower_spawn_func, machine_handle_method_block};
use state_lowering::{
    StateLoweringContext, collect_first_state_fields_block, lower_state_method,
    rewrite_state_refs_in_func,
};

use managed_entrypoint::rewrite_machines_entrypoint;
use names::*;
use rewrite_handles::{
    rewrite_machine_request_method_destinations, rewrite_typed_machine_handle_refs,
};

mod machine_error;
mod support_types;

use machine_error::{
    MachineErrorKind, machine_error_type_def, return_machine_error_if_zero, send_status_error_items,
};
use support_types::{
    ensure_machine_runtime_intrinsics, ensure_machine_support_types, machine_handle_named_type_def,
    machine_target_id_handle_helper_def,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypestateRoleImplRef {
    pub id: crate::core::tree::NodeId,
    pub typestate_name: String,
    pub path: Vec<String>,
    pub peer_role_bindings: Vec<TypestatePeerRoleBindingRef>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypestatePeerRoleBindingRef {
    pub id: crate::core::tree::NodeId,
    pub field_name: String,
    pub role_name: String,
    pub field_ty: TypeExpr,
    pub span: Span,
}

pub fn collect_role_impl_refs(module: &Module) -> Vec<TypestateRoleImplRef> {
    let mut out = Vec::new();
    for typestate in module.typestate_defs() {
        let peer_role_bindings = typestate
            .items
            .iter()
            .filter_map(|item| match item {
                TypestateItem::Fields(fields) => Some(fields),
                _ => None,
            })
            .flat_map(|fields| {
                fields.role_bindings.iter().filter_map(|binding| {
                    let field_ty = fields
                        .fields
                        .iter()
                        .find(|field| field.name == binding.field_name)
                        .map(|field| field.ty.clone())?;
                    Some(TypestatePeerRoleBindingRef {
                        id: binding.id,
                        field_name: binding.field_name.clone(),
                        role_name: binding.role_name.clone(),
                        field_ty,
                        span: binding.span,
                    })
                })
            })
            .collect::<Vec<_>>();
        for role_impl in &typestate.role_impls {
            out.push(TypestateRoleImplRef {
                id: role_impl.id,
                typestate_name: typestate.name.clone(),
                path: role_impl.path.clone(),
                peer_role_bindings: peer_role_bindings.clone(),
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
    for ts in module.typestate_defs() {
        handle_by_typestate.insert(ts.name.clone(), format!("__mc_machine_handle_{}", ts.name));
    }

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
    // Allow typed-handle destinations at request callsites by normalizing the
    // first request argument to a numeric machine id.
    rewrite_machine_request_method_destinations(module, node_id_gen);
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
    lowered.push(machine_target_id_handle_helper_def(
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
        let state_lowering_ctx = StateLoweringContext {
            source_state_names: &source_state_names,
            local_fields_by_state: &local_fields_by_state,
            carried_field_names: &carried_field_names,
            state_name_map: &state_name_map,
            span: state.span,
        };
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
                    &state_lowering_ctx,
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
                        &state_lowering_ctx,
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
                        &state_lowering_ctx,
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

/// Returns the state-local `fields` block if present, otherwise empty.
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
