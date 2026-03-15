//! Remaining typestate-only semantic checks after protocol retirement.
//!
//! The old protocol conformance machinery has been removed, but a few
//! typestate-local checks still matter until typestate support itself is
//! deleted. In particular we still diagnose overlapping response handlers and
//! mismatched request/response shapes.

use std::collections::HashSet;

use crate::core::ast::visit::{self, Visitor};
use crate::core::ast::{EmitKind, Expr, ExprKind, MethodBlock, MethodItem};
use crate::core::context::SemCheckNormalizedContext;
use crate::core::machine::naming::{
    is_generated_handler_name, parse_generated_handler_site_label, parse_generated_state_name,
};
use crate::core::resolve::DefTable;
use crate::core::semck::generated_state_scan::collect_generated_state_handlers;
use crate::core::semck::{SEK, SemCheckError, push_error};
use crate::core::typecheck::type_map::resolve_type_expr;
use crate::core::types::Type;

#[derive(Clone, Debug)]
struct HandlerResponsePattern {
    selector_ty: Type,
    response_tys: Vec<Type>,
    request_site_label: Option<String>,
    span: crate::core::diag::Span,
}

pub(super) fn check_typestate_handler_overlap(
    ctx: &SemCheckNormalizedContext,
) -> Vec<SemCheckError> {
    let mut errors = Vec::new();

    for handler in collect_generated_state_handlers(&ctx.module) {
        let typestate_name = handler.typestate_name;
        let state_name = handler.state_name;
        let patterns =
            collect_handler_response_patterns(&ctx.def_table, &ctx.module, handler.method_block);
        for i in 0..patterns.len() {
            for j in (i + 1)..patterns.len() {
                let left = &patterns[i];
                let right = &patterns[j];
                if left.selector_ty != right.selector_ty {
                    continue;
                }

                let mut overlap = Vec::new();
                for ty in &left.response_tys {
                    if right.response_tys.contains(ty) && !overlap.contains(ty) {
                        overlap.push(ty.clone());
                    }
                }
                if overlap.is_empty() {
                    continue;
                }

                if let (Some(left_label), Some(right_label)) =
                    (&left.request_site_label, &right.request_site_label)
                    && left_label != right_label
                {
                    continue;
                }
                if left.request_site_label.is_none() && right.request_site_label.is_none() {
                    push_error(
                        &mut errors,
                        right.span,
                        SEK::TypestateOverlappingOnHandlers(
                            typestate_name.clone(),
                            state_name.clone(),
                            left.selector_ty.clone(),
                            overlap,
                        ),
                    );
                } else {
                    push_error(
                        &mut errors,
                        right.span,
                        SEK::TypestateAmbiguousResponseProvenance(
                            typestate_name.clone(),
                            state_name.clone(),
                            left.selector_ty.clone(),
                            overlap,
                        ),
                    );
                }
            }
        }
    }

    errors
}

fn collect_handler_response_patterns(
    def_table: &DefTable,
    module: &crate::core::ast::Module,
    method_block: &MethodBlock,
) -> Vec<HandlerResponsePattern> {
    let mut out = Vec::new();
    for method_item in &method_block.method_items {
        let MethodItem::Def(method_def) = method_item else {
            continue;
        };
        if !is_generated_handler_name(&method_def.sig.name) {
            continue;
        }
        if method_def.sig.params.len() < 3 {
            continue;
        }
        let event_param = &method_def.sig.params[0];
        let pending_param = &method_def.sig.params[1];

        let Ok(selector_ty) = resolve_type_expr(def_table, module, &event_param.typ) else {
            continue;
        };
        let Ok(pending_ty) = resolve_type_expr(def_table, module, &pending_param.typ) else {
            continue;
        };
        let Type::Pending { response_tys } = pending_ty else {
            continue;
        };
        if response_tys.is_empty() {
            continue;
        }

        out.push(HandlerResponsePattern {
            selector_ty,
            response_tys,
            request_site_label: parse_generated_handler_site_label(&method_def.sig.name)
                .map(ToString::to_string),
            span: method_def.sig.span,
        });
    }
    out
}

#[derive(Clone, Debug)]
struct RequestSiteShape {
    typestate_name: String,
    request_ty: Type,
    response_tys: Vec<Type>,
    request_site_label: Option<String>,
    span: crate::core::diag::Span,
}

#[derive(Clone, Debug)]
struct ProvenanceHandlerShape {
    typestate_name: String,
    request_ty: Type,
    response_ty: Type,
    request_site_label: Option<String>,
    span: crate::core::diag::Span,
}

pub(super) fn check_typestate_request_response_shape(
    ctx: &SemCheckNormalizedContext,
) -> Vec<SemCheckError> {
    let request_sites = collect_typestate_request_sites(ctx);
    let handler_shapes = collect_provenance_handler_shapes(ctx);
    if request_sites.is_empty() && handler_shapes.is_empty() {
        return Vec::new();
    }

    let mut errors = Vec::new();
    let typestates_with_requests: HashSet<String> = request_sites
        .iter()
        .map(|site| site.typestate_name.clone())
        .collect();

    for site in &request_sites {
        let label_suffix = site
            .request_site_label
            .as_ref()
            .map(|label| format!(":{label}"))
            .unwrap_or_default();

        for response_ty in &site.response_tys {
            let has_handler = handler_shapes.iter().any(|handler| {
                if handler.typestate_name != site.typestate_name
                    || handler.request_ty != site.request_ty
                    || handler.response_ty != *response_ty
                {
                    return false;
                }
                match (&site.request_site_label, &handler.request_site_label) {
                    (None, None) => true,
                    (Some(site_label), Some(handler_label)) => site_label == handler_label,
                    (Some(_), None) => true,
                    (None, Some(_)) => false,
                }
            });
            if !has_handler {
                push_error(
                    &mut errors,
                    site.span,
                    SEK::TypestateRequestMissingResponseHandler(
                        site.typestate_name.clone(),
                        site.request_ty.clone(),
                        label_suffix.clone(),
                        response_ty.clone(),
                    ),
                );
            }
        }
    }

    for handler in &handler_shapes {
        if !typestates_with_requests.contains(&handler.typestate_name) {
            continue;
        }
        let label_suffix = handler
            .request_site_label
            .as_ref()
            .map(|label| format!(":{label}"))
            .unwrap_or_default();
        let supported = request_sites.iter().any(|site| {
            if site.typestate_name != handler.typestate_name
                || site.request_ty != handler.request_ty
            {
                return false;
            }
            let label_matches = match (&site.request_site_label, &handler.request_site_label) {
                (Some(site_label), Some(handler_label)) => site_label == handler_label,
                (None, None) => true,
                (Some(_), None) => true,
                (None, Some(_)) => false,
            };
            label_matches && site.response_tys.contains(&handler.response_ty)
        });
        if !supported {
            push_error(
                &mut errors,
                handler.span,
                SEK::TypestateHandlerUnsupportedResponseVariant(
                    handler.typestate_name.clone(),
                    handler.request_ty.clone(),
                    label_suffix,
                    handler.response_ty.clone(),
                ),
            );
        }
    }

    errors
}

fn collect_typestate_request_sites(ctx: &SemCheckNormalizedContext) -> Vec<RequestSiteShape> {
    let mut collector = TypestateRequestCollector {
        type_map: &ctx.type_map,
        current_typestate: None,
        sites: Vec::new(),
    };
    collector.visit_module(&ctx.module);
    collector.sites
}

struct TypestateRequestCollector<'a> {
    type_map: &'a crate::core::typecheck::type_map::TypeMap,
    current_typestate: Option<String>,
    sites: Vec<RequestSiteShape>,
}

impl Visitor for TypestateRequestCollector<'_> {
    fn visit_method_block(&mut self, method_block: &MethodBlock) {
        let prev = self.current_typestate.clone();
        self.current_typestate = parse_generated_state_name(&method_block.type_name)
            .map(|(typestate_name, _)| typestate_name);
        visit::walk_method_block(self, method_block);
        self.current_typestate = prev;
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if let Some(typestate_name) = &self.current_typestate
            && let ExprKind::Emit {
                kind:
                    EmitKind::Request {
                        payload,
                        request_site_label,
                        ..
                    },
            } = &expr.kind
            && let Some(request_ty) = self.type_map.lookup_node_type(payload.id)
            && let Some(Type::Pending { response_tys }) = self.type_map.lookup_node_type(expr.id)
        {
            self.sites.push(RequestSiteShape {
                typestate_name: typestate_name.clone(),
                request_ty: request_ty.clone(),
                response_tys: response_tys.clone(),
                request_site_label: request_site_label.clone(),
                span: expr.span,
            });
        }
        visit::walk_expr(self, expr);
    }
}

fn collect_provenance_handler_shapes(
    ctx: &SemCheckNormalizedContext,
) -> Vec<ProvenanceHandlerShape> {
    let mut out = Vec::new();
    for handler in collect_generated_state_handlers(&ctx.module) {
        if handler.method_def.sig.params.len() < 3
            || handler.method_def.sig.params[1].ident != "__pending"
        {
            continue;
        }
        let Ok(pending_ty) = resolve_type_expr(
            &ctx.def_table,
            &ctx.module,
            &handler.method_def.sig.params[1].typ,
        ) else {
            continue;
        };
        let Type::Pending { .. } = pending_ty else {
            continue;
        };
        let Ok(response_ty) = resolve_type_expr(
            &ctx.def_table,
            &ctx.module,
            &handler.method_def.sig.params[0].typ,
        ) else {
            continue;
        };
        let Ok(request_ty) = resolve_type_expr(
            &ctx.def_table,
            &ctx.module,
            &handler.method_def.sig.params[2].typ,
        ) else {
            continue;
        };

        out.push(ProvenanceHandlerShape {
            typestate_name: handler.typestate_name,
            request_ty,
            response_ty,
            request_site_label: parse_generated_handler_site_label(&handler.method_def.sig.name)
                .map(ToString::to_string),
            span: handler.method_def.sig.span,
        });
    }
    out
}
