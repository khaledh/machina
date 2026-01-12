use crate::ast::NodeIdGen;
use crate::ast::visit_mut;
use crate::ast::visit_mut::VisitorMut;
use crate::context::{ElaboratedContext, TypeCheckedContext};
use crate::resolve::DefId;
use crate::sir::builder::build_module as build_sir;
use crate::sir::model as sir;
use crate::typeck::type_map::{CallParam, TypeMap};
use crate::types::TypeId;
use crate::types::array_to_slice_assignable;

/// Elaborate TIR into SIR.
///
/// Step 1: 1:1 mapping of the typed tree into SIR, plus explicit array-to-slice
/// coercions on call arguments.
pub fn elaborate(ctx: TypeCheckedContext) -> ElaboratedContext {
    let mut type_map = ctx.type_map;
    let mut sir_module = build_sir(&ctx.module);
    let mut node_id_gen = ctx.node_id_gen;
    let mut elaborator = Elaborator::new(&mut type_map, &mut node_id_gen);
    elaborator.visit_module(&mut sir_module);
    ElaboratedContext {
        tir_module: ctx.module,
        sir_module,
        def_table: ctx.def_table,
        type_map,
        symbols: ctx.symbols,
        node_id_gen,
    }
}

struct Elaborator<'a> {
    type_map: &'a mut TypeMap,
    node_id_gen: &'a mut NodeIdGen,
}

impl<'a> Elaborator<'a> {
    fn new(type_map: &'a mut TypeMap, node_id_gen: &'a mut NodeIdGen) -> Self {
        Self {
            type_map,
            node_id_gen,
        }
    }

    fn coerce_call_args(&mut self, call_id: sir::NodeId, args: &mut [sir::CallArg]) {
        let Some(call_sig) = self.type_map.lookup_call_sig(call_id) else {
            return;
        };
        for (param, arg) in call_sig.params.iter().zip(args.iter_mut()) {
            self.coerce_array_to_slice(param, arg);
        }
    }

    fn coerce_array_to_slice(&mut self, param: &CallParam, arg: &mut sir::CallArg) {
        if !matches!(
            arg.mode,
            sir::CallArgMode::Default | sir::CallArgMode::InOut
        ) {
            return;
        }
        if matches!(
            arg.expr.kind,
            sir::ExprKind::Coerce {
                kind: sir::CoerceKind::ArrayToSlice,
                ..
            }
        ) {
            return;
        }
        let Some(arg_ty) = self.type_map.lookup_node_type(arg.expr.id) else {
            return;
        };
        if !array_to_slice_assignable(&arg_ty, &param.ty) {
            return;
        }

        let span = arg.expr.span;
        let inner = arg.expr.clone();
        let coerce_id = self.node_id_gen.new_id();
        let ty_id = self.type_map.insert_node_type(coerce_id, param.ty.clone());
        arg.expr = sir::Expr {
            id: coerce_id,
            kind: sir::ExprKind::Coerce {
                kind: sir::CoerceKind::ArrayToSlice,
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
        match &mut expr.kind {
            sir::ExprKind::Call { args, .. } | sir::ExprKind::MethodCall { args, .. } => {
                self.coerce_call_args(expr.id, args);
            }
            _ => {}
        }
    }
}
