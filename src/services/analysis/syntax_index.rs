//! Syntax span indexing helpers for analysis queries.
//!
//! These utilities walk tree nodes once and provide reusable lookups used by
//! hover, go-to-definition, signature help, document symbols, and references.

use std::collections::HashMap;

use crate::core::diag::{Position, Span};
use crate::core::tree::visit::{self, Visitor};
use crate::core::tree::{Expr, MatchPattern, MatchPatternBinding, NodeId, StmtExpr};
use crate::services::analysis::results::DocumentSymbolKind;

#[derive(Clone, Debug)]
pub(crate) struct CallSite {
    pub node_id: NodeId,
    pub callee_node_id: NodeId,
    pub span: Span,
    pub arg_spans: Vec<Span>,
}

pub(crate) fn node_at_span<D, T>(
    module: &crate::core::tree::Module<D, T>,
    query_span: Span,
) -> Option<NodeId> {
    let mut collector = NodeSpanCollector::default();
    collector.visit_module(module);

    let mut best: Option<(NodeId, Span)> = None;
    for (node_id, span) in collector.nodes {
        if !span_contains_span(span, query_span) {
            continue;
        }
        let replace = best.as_ref().is_none_or(|(_, best_span)| {
            let width = span_width(span);
            let best_width = span_width(*best_span);
            width < best_width
                || (width == best_width && span.start.offset > best_span.start.offset)
        });
        if replace {
            best = Some((node_id, span));
        }
    }
    best.map(|(node_id, _)| node_id)
}

pub(crate) fn node_span_map<D, T>(
    module: &crate::core::tree::Module<D, T>,
) -> HashMap<NodeId, Span> {
    let mut collector = NodeSpanCollector::default();
    collector.visit_module(module);
    collector.nodes.into_iter().collect()
}

pub(crate) fn call_site_at_span<D, T>(
    module: &crate::core::tree::Module<D, T>,
    query_span: Span,
) -> Option<CallSite> {
    let mut collector = CallSiteCollector::default();
    collector.visit_module(module);
    let mut best: Option<CallSite> = None;
    for call in collector.calls {
        if !span_contains_span(call.span, query_span) {
            continue;
        }
        let replace = best
            .as_ref()
            .is_none_or(|best_call| span_width(call.span) <= span_width(best_call.span));
        if replace {
            best = Some(call);
        }
    }
    best
}

pub(crate) fn active_param_index(arg_spans: &[Span], pos: Position) -> usize {
    if arg_spans.is_empty() {
        return 0;
    }
    for (i, span) in arg_spans.iter().enumerate() {
        if position_leq(pos, span.end) {
            return i;
        }
    }
    arg_spans.len().saturating_sub(1)
}

pub(crate) fn document_symbol_nodes<D, T>(
    module: &crate::core::tree::Module<D, T>,
) -> Vec<(NodeId, DocumentSymbolKind)> {
    let mut collector = DocumentSymbolNodeCollector::default();
    collector.visit_module(module);
    collector.nodes
}

pub(crate) fn span_intersects_span(a: Span, b: Span) -> bool {
    position_leq(a.start, b.end) && position_leq(b.start, a.end)
}

pub(crate) fn span_contains_span(span: Span, query: Span) -> bool {
    position_leq(span.start, query.start) && position_leq(query.end, span.end)
}

pub(crate) fn position_leq(lhs: Position, rhs: Position) -> bool {
    (lhs.line, lhs.column) <= (rhs.line, rhs.column)
}

fn span_width(span: Span) -> usize {
    span.end.offset.saturating_sub(span.start.offset)
}

#[derive(Default)]
struct CallSiteCollector {
    calls: Vec<CallSite>,
}

impl<D, T> Visitor<D, T> for CallSiteCollector {
    fn visit_expr(&mut self, expr: &Expr<D, T>) {
        match &expr.kind {
            crate::core::tree::ExprKind::Call { args, .. }
            | crate::core::tree::ExprKind::MethodCall { args, .. } => {
                let callee_node_id = match &expr.kind {
                    crate::core::tree::ExprKind::Call { callee, .. }
                    | crate::core::tree::ExprKind::MethodCall { callee, .. } => callee.id,
                    _ => unreachable!(),
                };
                self.calls.push(CallSite {
                    node_id: expr.id,
                    callee_node_id,
                    span: expr.span,
                    arg_spans: args.iter().map(|arg| arg.span).collect(),
                });
            }
            _ => {}
        }
        visit::walk_expr(self, expr);
    }
}

#[derive(Default)]
struct DocumentSymbolNodeCollector {
    nodes: Vec<(NodeId, DocumentSymbolKind)>,
}

impl<D, T> Visitor<D, T> for DocumentSymbolNodeCollector {
    fn visit_type_def(&mut self, type_def: &crate::core::tree::TypeDef<D>) {
        self.nodes.push((type_def.id, DocumentSymbolKind::Type));
        visit::walk_type_def::<Self, D, T>(self, type_def);
    }

    fn visit_trait_def(&mut self, trait_def: &crate::core::tree::TraitDef<D>) {
        self.nodes.push((trait_def.id, DocumentSymbolKind::Trait));
        visit::walk_trait_def::<Self, D, T>(self, trait_def);
    }

    fn visit_func_decl(&mut self, func_decl: &crate::core::tree::FuncDecl<D>) {
        self.nodes
            .push((func_decl.id, DocumentSymbolKind::Function));
        visit::walk_func_decl::<Self, D, T>(self, func_decl);
    }

    fn visit_func_def(&mut self, func_def: &crate::core::tree::FuncDef<D, T>) {
        self.nodes.push((func_def.id, DocumentSymbolKind::Function));
        visit::walk_func_def(self, func_def);
    }

    fn visit_method_decl(&mut self, method_decl: &crate::core::tree::MethodDecl<D>) {
        self.nodes
            .push((method_decl.id, DocumentSymbolKind::Method));
        visit::walk_method_decl::<Self, D, T>(self, method_decl);
    }

    fn visit_method_def(&mut self, method_def: &crate::core::tree::MethodDef<D, T>) {
        self.nodes.push((method_def.id, DocumentSymbolKind::Method));
        visit::walk_method_def(self, method_def);
    }

    fn visit_trait_method(&mut self, method: &crate::core::tree::TraitMethod<D>) {
        self.nodes.push((method.id, DocumentSymbolKind::Method));
        visit::walk_trait_method::<Self, D, T>(self, method);
    }

    fn visit_trait_property(&mut self, property: &crate::core::tree::TraitProperty<D>) {
        self.nodes.push((property.id, DocumentSymbolKind::Property));
        visit::walk_trait_property::<Self, D, T>(self, property);
    }
}

#[derive(Default)]
struct NodeSpanCollector {
    nodes: Vec<(NodeId, Span)>,
}

impl NodeSpanCollector {
    fn record(&mut self, id: NodeId, span: Span) {
        self.nodes.push((id, span));
    }
}

impl<D, T> Visitor<D, T> for NodeSpanCollector {
    fn visit_type_def(&mut self, type_def: &crate::core::tree::TypeDef<D>) {
        self.record(type_def.id, type_def.span);
        visit::walk_type_def::<Self, D, T>(self, type_def);
    }

    fn visit_trait_def(&mut self, trait_def: &crate::core::tree::TraitDef<D>) {
        self.record(trait_def.id, trait_def.span);
        visit::walk_trait_def::<Self, D, T>(self, trait_def);
    }

    fn visit_trait_method(&mut self, method: &crate::core::tree::TraitMethod<D>) {
        self.record(method.id, method.span);
        visit::walk_trait_method::<Self, D, T>(self, method);
    }

    fn visit_trait_property(&mut self, property: &crate::core::tree::TraitProperty<D>) {
        self.record(property.id, property.span);
        visit::walk_trait_property::<Self, D, T>(self, property);
    }

    fn visit_type_expr(&mut self, type_expr: &crate::core::tree::TypeExpr<D>) {
        self.record(type_expr.id, type_expr.span);
        visit::walk_type_expr::<Self, D, T>(self, type_expr);
    }

    fn visit_func_decl(&mut self, func_decl: &crate::core::tree::FuncDecl<D>) {
        self.record(func_decl.id, func_decl.span);
        visit::walk_func_decl::<Self, D, T>(self, func_decl);
    }

    fn visit_func_def(&mut self, func_def: &crate::core::tree::FuncDef<D, T>) {
        self.record(func_def.id, func_def.span);
        visit::walk_func_def(self, func_def);
    }

    fn visit_type_param(&mut self, param: &crate::core::tree::TypeParam<D>) {
        self.record(param.id, param.span);
        if let Some(bound) = &param.bound {
            self.record(bound.id, bound.span);
        }
        visit::walk_type_param::<Self, D, T>(self, param);
    }

    fn visit_method_block(&mut self, method_block: &crate::core::tree::MethodBlock<D, T>) {
        self.record(method_block.id, method_block.span);
        visit::walk_method_block(self, method_block);
    }

    fn visit_method_decl(&mut self, method_decl: &crate::core::tree::MethodDecl<D>) {
        self.record(method_decl.id, method_decl.span);
        visit::walk_method_decl::<Self, D, T>(self, method_decl);
    }

    fn visit_method_def(&mut self, method_def: &crate::core::tree::MethodDef<D, T>) {
        self.record(method_def.id, method_def.span);
        visit::walk_method_def(self, method_def);
    }

    fn visit_closure_def(&mut self, closure_def: &crate::core::tree::ClosureDef<D, T>) {
        self.record(closure_def.id, closure_def.span);
        visit::walk_closure_def(self, closure_def);
    }

    fn visit_param(&mut self, param: &crate::core::tree::Param<D>) {
        self.record(param.id, param.span);
        visit::walk_param::<Self, D, T>(self, param);
    }

    fn visit_bind_pattern(&mut self, pattern: &crate::core::tree::BindPattern<D>) {
        self.record(pattern.id, pattern.span);
        visit::walk_bind_pattern::<Self, D, T>(self, pattern);
    }

    fn visit_match_pattern(&mut self, pattern: &MatchPattern<D>) {
        match pattern {
            MatchPattern::Binding { id, span, .. }
            | MatchPattern::TypedBinding { id, span, .. }
            | MatchPattern::EnumVariant { id, span, .. } => self.record(*id, *span),
            MatchPattern::Wildcard { .. }
            | MatchPattern::BoolLit { .. }
            | MatchPattern::IntLit { .. }
            | MatchPattern::Tuple { .. } => {}
        }
        visit::walk_match_pattern::<Self, D, T>(self, pattern);
        visit::walk_match_pattern_bindings::<Self, D, T>(self, pattern);
    }

    fn visit_match_pattern_binding(&mut self, binding: &MatchPatternBinding<D>) {
        if let MatchPatternBinding::Named { id, span, .. } = binding {
            self.record(*id, *span);
        }
        visit::walk_match_pattern_binding::<Self, D, T>(self, binding);
    }

    fn visit_match_arm(&mut self, arm: &crate::core::tree::MatchArm<D, T>) {
        self.record(arm.id, arm.span);
        visit::walk_match_arm(self, arm);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr<D, T>) {
        self.record(stmt.id, stmt.span);
        visit::walk_stmt_expr(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr<D, T>) {
        self.record(expr.id, expr.span);
        visit::walk_expr(self, expr);
    }
}
