use crate::context::{NormalizedContext, TypeCheckedContext};
use crate::resolve::DefId;
use crate::tree::NodeIdGen;
use crate::tree::normalized as norm;
use crate::tree::normalized::build_module;
use crate::tree::visit_mut;
use crate::tree::visit_mut::VisitorMut;
use crate::typeck::type_map::{CallParam, TypeMap};
use crate::types::TypeId;
use crate::types::array_to_slice_assignable;

/// Normalize a typed tree into a normalized tree.
///
/// Step 1: 1:1 mapping of the typed tree into a normalized tree, plus explicit
/// array-to-slice coercions on call arguments.
pub fn normalize(ctx: TypeCheckedContext) -> NormalizedContext {
    let TypeCheckedContext {
        module,
        def_table,
        type_map,
        symbols,
        node_id_gen,
    } = ctx;
    let mut module = build_module(&module);
    let mut type_map = type_map;
    let mut node_id_gen = node_id_gen;
    let mut normalizer = Normalizer::new(&mut type_map, &mut node_id_gen);
    normalizer.visit_module(&mut module);
    NormalizedContext {
        module,
        def_table,
        type_map,
        symbols,
        node_id_gen,
    }
}

struct Normalizer<'a> {
    type_map: &'a mut TypeMap,
    node_id_gen: &'a mut NodeIdGen,
}

impl<'a> Normalizer<'a> {
    fn new(type_map: &'a mut TypeMap, node_id_gen: &'a mut NodeIdGen) -> Self {
        Self {
            type_map,
            node_id_gen,
        }
    }

    fn coerce_call_args(&mut self, call_id: norm::NodeId, args: &mut [norm::CallArg]) {
        let Some(call_sig) = self.type_map.lookup_call_sig(call_id) else {
            return;
        };
        for (param, arg) in call_sig.params.iter().zip(args.iter_mut()) {
            self.coerce_array_to_slice(param, arg);
        }
    }

    fn coerce_array_to_slice(&mut self, param: &CallParam, arg: &mut norm::CallArg) {
        if !matches!(
            arg.mode,
            norm::CallArgMode::Default | norm::CallArgMode::InOut
        ) {
            return;
        }
        if matches!(
            arg.expr.kind,
            norm::ExprKind::Coerce {
                kind: norm::CoerceKind::ArrayToSlice,
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
        arg.expr = norm::Expr {
            id: coerce_id,
            kind: norm::ExprKind::Coerce {
                kind: norm::CoerceKind::ArrayToSlice,
                expr: Box::new(inner),
            },
            ty: ty_id,
            span,
        };
    }
}

impl VisitorMut<DefId, TypeId> for Normalizer<'_> {
    fn visit_module(&mut self, module: &mut norm::Module) {
        visit_mut::walk_module(self, module);
    }

    fn visit_expr(&mut self, expr: &mut norm::Expr) {
        visit_mut::walk_expr(self, expr);
        match &mut expr.kind {
            norm::ExprKind::Call { args, .. } | norm::ExprKind::MethodCall { args, .. } => {
                self.coerce_call_args(expr.id, args);
            }
            _ => {}
        }
    }
}
