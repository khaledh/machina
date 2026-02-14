//! Typestate source-level desugaring.
//!
//! V1 strategy: lower parsed `typestate` declarations into ordinary type
//! definitions + method blocks + constructor functions before resolve.

use std::collections::{HashMap, HashSet};

use crate::core::resolve::ResolveError;
use crate::core::tree::NodeIdGen;
use crate::core::tree::parsed::{
    self, BindPattern, BindPatternKind, CallArg, Expr, ExprKind, FuncDecl, FuncDef, MatchPattern,
    MethodBlock, MethodDef, MethodItem, MethodSig, Module, SelfParam, StmtExpr, StmtExprKind,
    StructDefField, TopLevelItem, TypeDef, TypeDefKind, TypeExpr, TypeExprKind, TypestateDef,
    TypestateFields, TypestateItem, TypestateState, TypestateStateItem,
};
use crate::core::tree::visit_mut::{self, VisitorMut};
use crate::core::tree::{CallArgMode, InitInfo, ParamMode};

pub fn desugar_module(module: &mut Module, node_id_gen: &mut NodeIdGen) -> Vec<ResolveError> {
    let mut out = Vec::with_capacity(module.top_level_items.len());
    let mut ctor_by_typestate = HashMap::<String, String>::new();
    let mut errors = Vec::new();

    for item in module.top_level_items.drain(..) {
        match item {
            TopLevelItem::TypestateDef(typestate) => {
                let (lowered, typestate_errors) =
                    desugar_typestate(typestate, node_id_gen, &mut ctor_by_typestate);
                out.extend(lowered);
                errors.extend(typestate_errors);
            }
            other => out.push(other),
        }
    }

    module.top_level_items = out;
    // Rewrite `Typestate::new(...)` enum-variant syntax into constructor calls.
    rewrite_constructor_invocations(module, &ctor_by_typestate, node_id_gen);
    errors
}

fn desugar_typestate(
    typestate: TypestateDef,
    node_id_gen: &mut NodeIdGen,
    ctor_by_typestate: &mut HashMap<String, String>,
) -> (Vec<TopLevelItem>, Vec<ResolveError>) {
    let ts_name = typestate.name.clone();
    let ctor_name = format!("__ts_ctor_{}", ts_name);
    ctor_by_typestate.insert(ts_name.clone(), ctor_name.clone());

    let analysis = analyze_typestate(&typestate);
    let shared_fields = analysis.shared_fields;
    let states = analysis.states;
    let state_name_map = build_state_name_map(&ts_name, &states);

    let mut lowered = Vec::new();

    for state in states {
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

        // Generated inherent methods for this state.
        let mut method_items = Vec::new();
        for item in state.items {
            if let TypestateStateItem::Method(method) = item {
                method_items.push(MethodItem::Def(lower_state_method(
                    method,
                    node_id_gen,
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

    (lowered, analysis.errors)
}

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
    shared_fields: Vec<StructDefField>,
    states: Vec<TypestateState>,
    constructor: Option<FuncDef>,
    errors: Vec<ResolveError>,
}

fn analyze_typestate(typestate: &TypestateDef) -> TypestateAnalysis {
    let ts_name = typestate.name.clone();
    let mut errors = Vec::new();

    let fields_blocks: Vec<&TypestateFields> = typestate
        .items
        .iter()
        .filter_map(|item| match item {
            TypestateItem::Fields(fields) => Some(fields),
            _ => None,
        })
        .collect();

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

    if states.is_empty() {
        errors.push(ResolveError::TypestateMissingState(
            ts_name.clone(),
            typestate.span,
        ));
    }

    let state_names: HashSet<String> = states.iter().map(|state| state.name.clone()).collect();
    let shared_field_names: HashSet<String> = shared_fields
        .iter()
        .map(|field| field.name.clone())
        .collect();

    for state in &states {
        validate_state_items(
            &ts_name,
            state,
            &state_names,
            &shared_field_names,
            &mut errors,
        );
    }

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
            if !is_valid_state_return(&single.sig.ret_ty_expr, &state_names) {
                errors.push(ResolveError::TypestateInvalidNewReturn(
                    ts_name.clone(),
                    single.sig.ret_ty_expr.span,
                ));
            }
        }
        [_, rest @ ..] => {
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

        if let Some(self_param) = method.sig.params.iter().find(|param| param.ident == "self") {
            errors.push(ResolveError::TypestateExplicitSelfNotAllowed(
                ts_name.to_string(),
                state.name.clone(),
                method.sig.name.clone(),
                self_param.span,
            ));
        }

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

fn collect_first_state_fields_block(items: &[TypestateStateItem]) -> Vec<StructDefField> {
    items
        .iter()
        .find_map(|item| match item {
            TypestateStateItem::Fields(TypestateFields { fields, .. }) => Some(fields.clone()),
            _ => None,
        })
        .unwrap_or_default()
}

fn lower_state_method(
    mut method: MethodDefSource,
    node_id_gen: &mut NodeIdGen,
    state_name_map: &HashMap<String, String>,
    span: crate::core::diag::Span,
) -> MethodDef {
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

type MethodDefSource = FuncDef;

fn rewrite_state_refs_in_func(func: &mut FuncDef, state_name_map: &HashMap<String, String>) {
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

fn rewrite_constructor_invocations(
    module: &mut Module,
    ctor_by_typestate: &HashMap<String, String>,
    node_id_gen: &mut NodeIdGen,
) {
    let mut rewriter = CtorCallRewriter {
        ctor_by_typestate,
        node_id_gen,
    };
    rewriter.visit_module(module);
}

struct CtorCallRewriter<'a> {
    ctor_by_typestate: &'a HashMap<String, String>,
    node_id_gen: &'a mut NodeIdGen,
}

impl VisitorMut<()> for CtorCallRewriter<'_> {
    fn visit_expr(&mut self, expr: &mut Expr) {
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
            let payload = std::mem::take(payload);
            let args = payload
                .into_iter()
                .map(|arg| CallArg {
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
