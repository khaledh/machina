//! Typestate declaration analysis and source-shape validation.
//!
//! This module validates source typestate declarations before lowering and
//! collects the facts needed by desugaring.

use std::collections::HashSet;

use crate::core::resolve::{REK, ResolveError};
use crate::core::tree::{
    FuncDef, StructDefField, TypeExpr, TypeExprKind, TypestateDef, TypestateFields, TypestateItem,
    TypestateOnHandler, TypestateState, TypestateStateItem,
};

#[derive(Debug)]
pub(super) struct TypestateAnalysis {
    // Shared `fields { ... }` on the typestate itself.
    pub(super) shared_fields: Vec<StructDefField>,
    // Unique states only (duplicates are reported as diagnostics).
    pub(super) states: Vec<TypestateState>,
    // Typestate-level handlers copied onto each lowered state.
    pub(super) handlers: Vec<TypestateOnHandler>,
    // Source states marked with `@final`.
    pub(super) final_state_names: HashSet<String>,
    // The selected `new` constructor (first one if multiple are present).
    pub(super) constructor: Option<FuncDef>,
    // All validation diagnostics for this typestate block.
    pub(super) errors: Vec<ResolveError>,
}

/// Validate typestate declaration shape and collect pre-lowering facts.
pub(super) fn analyze_typestate(typestate: &TypestateDef) -> TypestateAnalysis {
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
        errors.push(REK::TypestateDuplicateFieldsBlock(ts_name.clone()).at(extra.span));
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
                    errors.push(
                        REK::TypestateDuplicateState(ts_name.clone(), state.name.clone())
                            .at(state.span),
                    );
                }
            }
            TypestateItem::Handler(handler) => handlers.push(handler.clone()),
            TypestateItem::Fields(_) | TypestateItem::Constructor(_) => {}
        }
    }

    // A typestate with no states is not meaningful.
    if states.is_empty() {
        errors.push(REK::TypestateMissingState(ts_name.clone()).at(typestate.span));
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
            errors.push(
                REK::TypestateInvalidOnHandlerReturn(ts_name.clone()).at(handler.ret_ty_expr.span),
            );
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
    let constructor = new_ctors.first().map(|first_new| (*first_new).clone());

    match new_ctors.as_slice() {
        [] => errors.push(REK::TypestateMissingNew(ts_name.clone()).at(typestate.span)),
        [single] => {
            // `new` must return `State` or `State | Error...` (state first).
            if !is_valid_state_return(&single.sig.ret_ty_expr, &state_names) {
                errors.push(
                    REK::TypestateInvalidNewReturn(ts_name.clone()).at(single.sig.ret_ty_expr.span),
                );
            }
        }
        [_, rest @ ..] => {
            // Keep first constructor for lowering so we can continue and report
            // additional diagnostics in the same pass.
            for duplicate in rest {
                errors.push(REK::TypestateDuplicateNew(ts_name.clone()).at(duplicate.sig.span));
            }
            if !is_valid_state_return(&new_ctors[0].sig.ret_ty_expr, &state_names) {
                errors.push(
                    REK::TypestateInvalidNewReturn(ts_name.clone())
                        .at(new_ctors[0].sig.ret_ty_expr.span),
                );
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
            errors.push(REK::AttrDuplicate(attr.name.clone()).at(attr.span));
            continue;
        }
        match attr.name.as_str() {
            "final" => {
                if !attr.args.is_empty() {
                    errors.push(
                        REK::AttrWrongArgCount(attr.name.clone(), 0, attr.args.len()).at(attr.span),
                    );
                    continue;
                }
                is_final = true;
            }
            _ => errors.push(
                REK::TypestateUnknownStateAttribute(
                    ts_name.to_string(),
                    state.name.clone(),
                    attr.name.clone(),
                )
                .at(attr.span),
            ),
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
        errors.push(
            REK::TypestateDuplicateStateFieldsBlock(ts_name.to_string(), state.name.clone())
                .at(extra.span),
        );
    }
    if let Some(local_fields) = fields_blocks.first() {
        // Local state fields must not shadow carried fields in V1.
        for field in &local_fields.fields {
            if shared_field_names.contains(&field.name) {
                errors.push(
                    REK::TypestateStateFieldShadowsCarriedField(
                        ts_name.to_string(),
                        state.name.clone(),
                        field.name.clone(),
                    )
                    .at(field.span),
                );
            }
        }
    }

    if is_final_state {
        for item in &state.items {
            match item {
                TypestateStateItem::Method(method) => {
                    errors.push(
                        REK::TypestateFinalStateHasTransition(
                            ts_name.to_string(),
                            state.name.clone(),
                        )
                        .at(method.sig.span),
                    );
                }
                TypestateStateItem::Handler(handler) => {
                    errors.push(
                        REK::TypestateFinalStateHasHandler(ts_name.to_string(), state.name.clone())
                            .at(handler.span),
                    );
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
                    errors.push(
                        REK::TypestateDuplicateTransition(
                            ts_name.to_string(),
                            state.name.clone(),
                            method.sig.name.clone(),
                        )
                        .at(method.sig.span),
                    );
                }

                // `self` is implicit in typestate transitions.
                if let Some(self_param) =
                    method.sig.params.iter().find(|param| param.ident == "self")
                {
                    errors.push(
                        REK::TypestateExplicitSelfNotAllowed(
                            ts_name.to_string(),
                            state.name.clone(),
                            method.sig.name.clone(),
                        )
                        .at(self_param.span),
                    );
                }

                // Transition success return follows the same shape rule as `new`.
                if !is_valid_state_return(&method.sig.ret_ty_expr, state_names) {
                    errors.push(
                        REK::TypestateInvalidTransitionReturn(
                            ts_name.to_string(),
                            state.name.clone(),
                            method.sig.name.clone(),
                        )
                        .at(method.sig.ret_ty_expr.span),
                    );
                }
            }
            TypestateStateItem::Handler(handler) => {
                if !is_valid_on_handler_return(&handler.ret_ty_expr, state_names) {
                    errors.push(
                        REK::TypestateInvalidStateOnHandlerReturn(
                            ts_name.to_string(),
                            state.name.clone(),
                        )
                        .at(handler.ret_ty_expr.span),
                    );
                }
            }
            TypestateStateItem::Fields(_) => {}
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
