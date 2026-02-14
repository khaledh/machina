//! Typestate source-level desugaring.
//!
//! V1 strategy: lower parsed `typestate` declarations into ordinary type
//! definitions + method blocks + constructor functions before resolve.
//!
//! This pass is intentionally front-end only: it rewrites parsed AST into
//! existing language constructs so resolver/typechecker/back-end stay mostly
//! unchanged for the prototype.

use std::collections::{HashMap, HashSet};

use crate::core::resolve::ResolveError;
use crate::core::tree::NodeIdGen;
use crate::core::tree::parsed::{
    self, BindPattern, BindPatternKind, CallArg, Expr, ExprKind, FuncDecl, FuncDef, MatchPattern,
    MethodBlock, MethodDef, MethodItem, MethodSig, Module, SelfParam, StmtExpr, StmtExprKind,
    StructDefField, StructLitField, TopLevelItem, TypeDef, TypeDefKind, TypeExpr, TypeExprKind,
    TypestateDef, TypestateFields, TypestateItem, TypestateState, TypestateStateItem,
};
use crate::core::tree::visit::{self, Visitor};
use crate::core::tree::visit_mut::{self, VisitorMut};
use crate::core::tree::{CallArgMode, InitInfo, ParamMode};

pub fn desugar_module(module: &mut Module, node_id_gen: &mut NodeIdGen) -> Vec<ResolveError> {
    let mut out = Vec::with_capacity(module.top_level_items.len());
    // Typestate `Type::new(...)` calls are rewritten after lowering, so we keep
    // a typestate-name -> generated-constructor-name map across the whole module.
    let mut ctor_by_typestate = HashMap::<String, String>::new();
    // Keep both source and generated state names so external-literal checks can
    // flag either form.
    let mut source_state_names = HashSet::new();
    let mut generated_state_names = HashSet::new();
    let mut errors = Vec::new();

    for item in module.top_level_items.drain(..) {
        match item {
            TopLevelItem::TypestateDef(typestate) => {
                // One typestate can lower to multiple top-level items.
                let lowered = desugar_typestate(typestate, node_id_gen, &mut ctor_by_typestate);
                out.extend(lowered.items);
                source_state_names.extend(lowered.source_state_names);
                generated_state_names.extend(lowered.generated_state_names);
                errors.extend(lowered.errors);
            }
            other => out.push(other),
        }
    }

    module.top_level_items = out;
    // Rewrite `Typestate::new(...)` enum-variant syntax into generated ctor calls.
    rewrite_constructor_invocations(module, &ctor_by_typestate, node_id_gen);
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
) -> TypestateDesugarOutput {
    let ts_name = typestate.name.clone();
    let ctor_name = format!("__ts_ctor_{}", ts_name);
    ctor_by_typestate.insert(ts_name.clone(), ctor_name.clone());

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

    for state in states {
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
        for item in state.items {
            if let TypestateStateItem::Method(method) = item {
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
        lowered.push(TopLevelItem::FuncDef(ctor));
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

struct TypestateAnalysis {
    // Shared `fields { ... }` on the typestate itself.
    shared_fields: Vec<StructDefField>,
    // Unique states only (duplicates are reported as diagnostics).
    states: Vec<TypestateState>,
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
    let mut states = Vec::new();
    for item in &typestate.items {
        if let TypestateItem::State(state) = item {
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
            &state_names,
            &shared_field_names,
            &mut errors,
        );
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
        constructor,
        errors,
    }
}

fn validate_state_items(
    ts_name: &str,
    state: &TypestateState,
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

    // 2) Validate transition signatures for this state.
    // Transition names must be unique per source state.
    let mut seen_methods = HashSet::new();
    for item in &state.items {
        let TypestateStateItem::Method(method) = item else {
            continue;
        };

        if !seen_methods.insert(method.sig.name.clone()) {
            errors.push(ResolveError::TypestateDuplicateTransition(
                ts_name.to_string(),
                state.name.clone(),
                method.sig.name.clone(),
                method.sig.span,
            ));
        }

        // `self` is implicit in typestate transitions.
        if let Some(self_param) = method.sig.params.iter().find(|param| param.ident == "self") {
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
    node_id_gen: &mut NodeIdGen,
) {
    // Global rewrite after all typestate declarations are lowered so call sites
    // can target the generated constructor symbol.
    let mut rewriter = CtorCallRewriter {
        ctor_by_typestate,
        node_id_gen,
    };
    rewriter.visit_module(module);
}

struct CtorCallRewriter<'a> {
    // Map source typestate name -> generated constructor function name.
    ctor_by_typestate: &'a HashMap<String, String>,
    node_id_gen: &'a mut NodeIdGen,
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
            && variant == "new"
            && let Some(ctor_name) = self.ctor_by_typestate.get(enum_name)
        {
            // Parsed form of `Type::new(a, b)` arrives as enum-variant syntax in
            // the parsed tree. Lower it to a plain call expression.
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
                        ident: ctor_name.clone(),
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
