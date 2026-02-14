//! Pass 4 of the type checker: validate control-flow semantics.
//!
//! This pass handles semantic checks that are not pure type equalities or
//! assignability relations (e.g. `break`/`continue` scope and return rules).

use std::collections::{HashMap, HashSet};

use crate::core::resolve::DefId;
use crate::core::tree::NodeId;
use crate::core::tree::visit::{self, Visitor};
use crate::core::tree::{ExprKind, MethodItem};
use crate::core::typecheck::constraints::ControlFact;
use crate::core::typecheck::engine::TypecheckEngine;
use crate::core::typecheck::errors::{TypeCheckError, TypeCheckErrorKind};
use crate::core::typecheck::type_map::resolve_type_expr;
use crate::core::types::Type;

/// Pass 4: semantic checks that are not pure type equalities/assignability.
pub(crate) fn run(engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    let mut errors = Vec::new();
    for fact in &engine.state().constrain.control_facts {
        match fact {
            ControlFact::Break {
                loop_depth, span, ..
            } if *loop_depth == 0 => {
                errors.push(TypeCheckErrorKind::BreakOutsideLoop(*span).into())
            }
            ControlFact::Continue {
                loop_depth, span, ..
            } if *loop_depth == 0 => {
                errors.push(TypeCheckErrorKind::ContinueOutsideLoop(*span).into());
            }
            ControlFact::Return {
                has_value,
                expected_return_ty,
                span,
                ..
            } => match expected_return_ty {
                None => {
                    errors.push(TypeCheckErrorKind::ReturnOutsideFunction(*span).into());
                }
                Some(expected_ty) => {
                    let expected_ty = engine.type_vars().apply(expected_ty);
                    if *has_value && expected_ty == Type::Unit {
                        errors.push(TypeCheckErrorKind::ReturnValueUnexpected(*span).into());
                    } else if !*has_value && expected_ty != Type::Unit {
                        errors.push(
                            TypeCheckErrorKind::ReturnValueMissing(expected_ty, *span).into(),
                        );
                    }
                }
            },
            _ => {}
        }
    }
    errors.extend(check_protocol_shape_conformance(engine));

    if errors.is_empty() {
        Ok(())
    } else {
        engine.state_mut().diags.extend(errors.clone());
        Err(errors)
    }
}

fn check_protocol_shape_conformance(engine: &TypecheckEngine) -> Vec<TypeCheckError> {
    let resolved = engine.context();
    let role_impls = &resolved.typestate_role_impls;
    if role_impls.is_empty() {
        return Vec::new();
    }

    let mut protocol_by_name = HashMap::new();
    for protocol in resolved.module.protocol_defs() {
        protocol_by_name.insert(protocol.name.clone(), protocol);
    }

    let typestate_names: HashSet<String> = role_impls
        .iter()
        .map(|binding| binding.typestate_name.clone())
        .collect();
    let handler_payloads = collect_typestate_handler_payloads(engine, &typestate_names);
    let outgoing_payloads = collect_typestate_outgoing_payloads(engine, &typestate_names);

    let mut errors = Vec::new();
    for binding in role_impls {
        // Role path/def validity is resolver's responsibility. Skip malformed
        // entries to avoid duplicate/symptom diagnostics here.
        if binding.path.len() < 2 || binding.role_def_id.is_none() {
            continue;
        }

        let protocol_name = &binding.path[0];
        let role_name = &binding.path[1];
        let role_label = binding.path.join("::");
        let Some(protocol) = protocol_by_name.get(protocol_name) else {
            continue;
        };

        let mut required_incoming = HashSet::<Type>::new();
        let mut allowed_outgoing = HashSet::<Type>::new();
        for flow in &protocol.flows {
            let Ok(payload_ty) =
                resolve_type_expr(&resolved.def_table, &resolved.module, &flow.payload_ty)
            else {
                continue;
            };
            if &flow.to_role == role_name {
                required_incoming.insert(payload_ty.clone());
            }
            if &flow.from_role == role_name {
                allowed_outgoing.insert(payload_ty);
            }
        }

        let seen_handlers = handler_payloads
            .get(&binding.typestate_name)
            .cloned()
            .unwrap_or_default();
        for required in required_incoming {
            if !seen_handlers.contains(&required) {
                errors.push(
                    TypeCheckErrorKind::ProtocolFlowHandlerMissing(
                        binding.typestate_name.clone(),
                        role_label.clone(),
                        required,
                        binding.span,
                    )
                    .into(),
                );
            }
        }

        if let Some(emits) = outgoing_payloads.get(&binding.typestate_name) {
            for emit in emits {
                if !allowed_outgoing.contains(&emit.payload_ty) {
                    errors.push(
                        TypeCheckErrorKind::ProtocolOutgoingPayloadNotAllowed(
                            binding.typestate_name.clone(),
                            role_label.clone(),
                            emit.payload_ty.clone(),
                            emit.span,
                        )
                        .into(),
                    );
                }
            }
        }
    }

    errors
}

fn collect_typestate_handler_payloads(
    engine: &TypecheckEngine,
    typestate_names: &HashSet<String>,
) -> HashMap<String, HashSet<Type>> {
    let mut out = HashMap::<String, HashSet<Type>>::new();
    for method_block in engine.context().module.method_blocks() {
        let Some(typestate_name) =
            parse_typestate_from_generated_state(&method_block.type_name, typestate_names)
        else {
            continue;
        };
        for method_item in &method_block.method_items {
            let MethodItem::Def(method_def) = method_item else {
                continue;
            };
            if !method_def.sig.name.starts_with("__ts_on_") {
                continue;
            }
            let Some(event_param) = method_def.sig.params.first() else {
                continue;
            };
            let Ok(handler_ty) = resolve_type_expr(
                &engine.context().def_table,
                &engine.context().module,
                &event_param.typ,
            ) else {
                continue;
            };
            out.entry(typestate_name.clone())
                .or_default()
                .insert(handler_ty);
        }
    }
    out
}

#[derive(Clone, Debug)]
struct EmitPayload {
    payload_ty: Type,
    span: crate::core::diag::Span,
}

fn collect_typestate_outgoing_payloads(
    engine: &TypecheckEngine,
    typestate_names: &HashSet<String>,
) -> HashMap<String, Vec<EmitPayload>> {
    let mut collector = TypestateEmitCollector {
        typestate_names,
        node_types: &engine.state().solve.resolved_node_types,
        current_typestate: None,
        emits_by_typestate: HashMap::new(),
    };
    collector.visit_module(&engine.context().module);
    collector.emits_by_typestate
}

struct TypestateEmitCollector<'a> {
    typestate_names: &'a HashSet<String>,
    node_types: &'a HashMap<NodeId, Type>,
    current_typestate: Option<String>,
    emits_by_typestate: HashMap<String, Vec<EmitPayload>>,
}

impl Visitor<DefId, ()> for TypestateEmitCollector<'_> {
    fn visit_method_block(&mut self, method_block: &crate::core::tree::resolved::MethodBlock) {
        let prev = self.current_typestate.clone();
        self.current_typestate =
            parse_typestate_from_generated_state(&method_block.type_name, self.typestate_names);
        visit::walk_method_block(self, method_block);
        self.current_typestate = prev;
    }

    fn visit_expr(&mut self, expr: &crate::core::tree::resolved::Expr) {
        if let Some(typestate_name) = &self.current_typestate
            && let ExprKind::Emit { kind } = &expr.kind
        {
            let payload = match kind {
                crate::core::tree::EmitKind::Send { payload, .. }
                | crate::core::tree::EmitKind::Request { payload, .. } => payload,
            };
            if let Some(payload_ty) = self.node_types.get(&payload.id) {
                self.emits_by_typestate
                    .entry(typestate_name.clone())
                    .or_default()
                    .push(EmitPayload {
                        payload_ty: payload_ty.clone(),
                        span: payload.span,
                    });
            }
        }
        visit::walk_expr(self, expr);
    }
}

fn parse_typestate_from_generated_state(
    type_name: &str,
    typestate_names: &HashSet<String>,
) -> Option<String> {
    typestate_names
        .iter()
        .filter_map(|ts_name| {
            let prefix = format!("__ts_{ts_name}_");
            if type_name.starts_with(&prefix) {
                Some(ts_name.clone())
            } else {
                None
            }
        })
        .max_by_key(|ts_name| ts_name.len())
}

#[cfg(test)]
#[path = "../../tests/typecheck/t_validate.rs"]
mod tests;
