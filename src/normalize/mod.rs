use crate::ast::NodeIdGen;
use crate::ast::visit_mut;
use crate::ast::visit_mut::VisitorMut;
use crate::context::{NormalizedContext, TypeCheckedContext};
use crate::nir::builder::ToNir;
use crate::nir::model as nir;
use crate::resolve::DefId;
use crate::typeck::type_map::{CallParam, TypeMap};
use crate::types::TypeId;
use crate::types::array_to_slice_assignable;

/// Normalize TIR into NIR.
///
/// Step 1: 1:1 mapping of the typed tree into NIR, plus explicit array-to-slice
/// coercions on call arguments.
pub fn normalize(ctx: TypeCheckedContext) -> NormalizedContext {
    let TypeCheckedContext {
        module,
        def_table,
        type_map,
        symbols,
        node_id_gen,
    } = ctx;
    let mut module = module.to_nir();
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

    fn coerce_call_args(&mut self, call_id: nir::NodeId, args: &mut [nir::CallArg]) {
        let Some(call_sig) = self.type_map.lookup_call_sig(call_id) else {
            return;
        };
        for (param, arg) in call_sig.params.iter().zip(args.iter_mut()) {
            self.coerce_array_to_slice(param, arg);
        }
    }

    fn coerce_array_to_slice(&mut self, param: &CallParam, arg: &mut nir::CallArg) {
        if !matches!(
            arg.mode,
            nir::CallArgMode::Default | nir::CallArgMode::InOut
        ) {
            return;
        }
        if matches!(
            arg.expr.kind,
            nir::ExprKind::Coerce {
                kind: nir::CoerceKind::ArrayToSlice,
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
        arg.expr = nir::Expr {
            id: coerce_id,
            kind: nir::ExprKind::Coerce {
                kind: nir::CoerceKind::ArrayToSlice,
                expr: Box::new(inner),
            },
            ty: ty_id,
            span,
        };
    }
}

impl VisitorMut<DefId, TypeId> for Normalizer<'_> {
    fn visit_module(&mut self, module: &mut nir::Module) {
        visit_mut::walk_module(self, module);
    }

    fn visit_expr(&mut self, expr: &mut nir::Expr) {
        visit_mut::walk_expr(self, expr);
        match &mut expr.kind {
            nir::ExprKind::Call { args, .. } | nir::ExprKind::MethodCall { args, .. } => {
                self.coerce_call_args(expr.id, args);
            }
            _ => {}
        }
    }
}
