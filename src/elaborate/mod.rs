use crate::ast::visit_mut;
use crate::ast::visit_mut::VisitorMut;
use crate::ast::{NodeId, NodeIdGen};
use crate::context::{ElaboratedContext, SemanticCheckedContext};
use crate::resolve::DefId;
use crate::sir::model as sir;
use crate::typeck::type_map::TypeMap;
use crate::types::TypeId;
use std::collections::HashSet;

/// Elaborate SIR using semantic analysis results.
///
/// Step 1: insert implicit move nodes based on semck results.
pub fn elaborate(ctx: SemanticCheckedContext) -> ElaboratedContext {
    let SemanticCheckedContext {
        module,
        sir_module,
        def_table,
        type_map,
        symbols,
        node_id_gen,
        implicit_moves,
        init_assigns,
        full_init_assigns,
    } = ctx;
    let mut sir_module = sir_module;
    let mut type_map = type_map;
    let mut node_id_gen = node_id_gen;
    let mut elaborator = Elaborator::new(&mut type_map, &mut node_id_gen, &implicit_moves);
    elaborator.visit_module(&mut sir_module);
    ElaboratedContext {
        module,
        sir_module,
        def_table,
        type_map,
        symbols,
        node_id_gen,
        init_assigns,
        full_init_assigns,
    }
}

struct Elaborator<'a> {
    type_map: &'a mut TypeMap,
    node_id_gen: &'a mut NodeIdGen,
    implicit_moves: &'a HashSet<NodeId>,
}

impl<'a> Elaborator<'a> {
    fn new(
        type_map: &'a mut TypeMap,
        node_id_gen: &'a mut NodeIdGen,
        implicit_moves: &'a HashSet<NodeId>,
    ) -> Self {
        Self {
            type_map,
            node_id_gen,
            implicit_moves,
        }
    }

    fn maybe_wrap_implicit_move(&mut self, expr: &mut sir::Expr) {
        if !self.implicit_moves.contains(&expr.id) {
            return;
        }
        if matches!(
            expr.kind,
            sir::ExprKind::ImplicitMove { .. } | sir::ExprKind::Move { .. }
        ) {
            return;
        }
        let Some(ty) = self.type_map.lookup_node_type(expr.id) else {
            return;
        };
        let span = expr.span;
        let inner = expr.clone();
        let id = self.node_id_gen.new_id();
        let ty_id = self.type_map.insert_node_type(id, ty);
        *expr = sir::Expr {
            id,
            kind: sir::ExprKind::ImplicitMove {
                expr: Box::new(inner),
            },
            ty: ty_id,
            span,
        };
    }
}

impl VisitorMut<DefId, TypeId> for Elaborator<'_> {
    fn visit_module(&mut self, module: &mut sir::Module) {
        visit_mut::walk_module(self, module);
    }

    fn visit_expr(&mut self, expr: &mut sir::Expr) {
        visit_mut::walk_expr(self, expr);
        self.maybe_wrap_implicit_move(expr);
    }
}
