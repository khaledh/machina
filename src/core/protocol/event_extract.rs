//! Shared protocol event extraction helpers.
//!
//! This module centralizes the "what counts as protocol traffic" logic used by
//! both typecheck protocol conformance and semck progression extraction.

use crate::core::diag::Span;
use crate::core::tree::{CallArgMode, Expr, ExprKind, NodeId};
use crate::core::types::Type;

#[derive(Clone, Debug)]
pub struct ProtocolEmitExtract {
    pub payload_ty: Type,
    pub to_field_name: Option<String>,
    pub destination_implicit: bool,
    pub is_request: bool,
    pub request_response_tys: Vec<Type>,
    pub span: Span,
}

/// Extract protocol-visible emission facts from an expression.
///
/// Recognized forms:
/// - `emit Send(...)`
/// - `emit Request(...)`
/// - machine-handle method calls: `dst.send(payload)`, `src.request(dst, payload)`
/// - `reply(cap, payload)` / `cap.reply(payload)` lowered `ExprKind::Reply`
pub fn extract_emit_from_expr<F>(expr: &Expr, mut lookup_type: F) -> Option<ProtocolEmitExtract>
where
    F: FnMut(NodeId) -> Option<Type>,
{
    match &expr.kind {
        ExprKind::Emit { kind } => {
            let (to, payload, is_request) = match kind {
                crate::core::tree::EmitKind::Send { to, payload } => (to, payload, false),
                crate::core::tree::EmitKind::Request { to, payload, .. } => (to, payload, true),
            };
            let payload_ty = lookup_type(payload.id)?;
            let request_response_tys = if is_request {
                match lookup_type(expr.id) {
                    Some(Type::Pending { response_tys }) => response_tys,
                    _ => Vec::new(),
                }
            } else {
                Vec::new()
            };
            Some(ProtocolEmitExtract {
                payload_ty,
                to_field_name: destination_field_name(to),
                destination_implicit: false,
                is_request,
                request_response_tys,
                span: payload.span,
            })
        }
        ExprKind::MethodCall {
            callee,
            method_name,
            args,
        } => extract_emit_from_machine_method_call(
            callee,
            method_name,
            args,
            expr.span,
            &mut lookup_type,
        ),
        ExprKind::Reply { value, .. } => Some(ProtocolEmitExtract {
            payload_ty: lookup_type(value.id)?,
            to_field_name: None,
            destination_implicit: true,
            is_request: false,
            request_response_tys: Vec::new(),
            span: value.span,
        }),
        _ => None,
    }
}

fn extract_emit_from_machine_method_call<F>(
    callee: &Expr,
    method_name: &str,
    args: &[crate::core::tree::CallArg],
    _call_span: Span,
    lookup_type: &mut F,
) -> Option<ProtocolEmitExtract>
where
    F: FnMut(NodeId) -> Option<Type>,
{
    if !is_machine_handle_receiver(callee, lookup_type) {
        return None;
    }
    if !args.iter().all(|arg| arg.mode == CallArgMode::Default) {
        return None;
    }

    match method_name {
        "send" if args.len() == 1 => {
            let payload = &args[0].expr;
            Some(ProtocolEmitExtract {
                payload_ty: lookup_type(payload.id)?,
                to_field_name: destination_field_name(callee),
                destination_implicit: false,
                is_request: false,
                request_response_tys: Vec::new(),
                span: payload.span,
            })
        }
        "request" if args.len() == 2 => {
            let to = &args[0].expr;
            let payload = &args[1].expr;
            Some(ProtocolEmitExtract {
                payload_ty: lookup_type(payload.id)?,
                to_field_name: destination_field_name(to),
                destination_implicit: false,
                is_request: true,
                request_response_tys: Vec::new(),
                span: payload.span,
            })
        }
        // Raw ABI overloads (`send(kind,p0,p1)` / `request(dst,kind,p0,p1)`)
        // are intentionally ignored because payload type cannot be recovered.
        _ => None,
    }
}

fn is_machine_handle_receiver<F>(callee: &Expr, lookup_type: &mut F) -> bool
where
    F: FnMut(NodeId) -> Option<Type>,
{
    if let Some(receiver_ty) = lookup_type(callee.id)
        && type_is_machine_handle(&receiver_ty)
    {
        return true;
    }
    // Fallback for partially-resolved receiver nodes: treat `self.<peer>` as
    // machine-like so protocol extraction is still available.
    destination_field_name(callee).is_some()
}

fn type_is_machine_handle(ty: &Type) -> bool {
    match ty {
        Type::Struct { name, .. } => name.starts_with("__mc_machine_handle_"),
        Type::Ref { elem_ty, .. } | Type::Heap { elem_ty } => type_is_machine_handle(elem_ty),
        Type::ErrorUnion { ok_ty, .. } => type_is_machine_handle(ok_ty),
        _ => false,
    }
}

/// Extract destination role-binding field name from destination expression.
///
/// Recognized forms:
/// - `self.peer`
/// - `__mc_machine_target_id(self.peer)` (handler sugar lowered form)
pub fn destination_field_name(expr: &Expr) -> Option<String> {
    if let ExprKind::Call { callee, args } = &expr.kind
        && let ExprKind::Var { ident, .. } = &callee.kind
        && ident == "__mc_machine_target_id"
        && args.len() == 1
    {
        return destination_field_name(&args[0].expr);
    }

    if let ExprKind::StructField { target, field } = &expr.kind
        && let ExprKind::Var { ident, .. } = &target.kind
        && ident == "self"
    {
        Some(field.clone())
    } else {
        None
    }
}
