use crate::context::{NormalizedContext, TypeCheckedContext};
use crate::resolve::DefId;
use crate::tree::NodeIdGen;
use crate::tree::normalized as norm;
use crate::tree::normalized::build_module;
use crate::tree::visit_mut;
use crate::tree::visit_mut::VisitorMut;
use crate::typeck::type_map::{CallParam, CallSigMap, TypeMap};
use crate::types::array_to_slice_assignable;
use crate::types::{Type, TypeId};

/// Normalize a typed tree into a normalized tree.
///
/// Step 1: 1:1 mapping of the typed tree into a normalized tree, plus explicit
/// array-to-slice coercions on call arguments.
pub fn normalize(ctx: TypeCheckedContext) -> NormalizedContext {
    let TypeCheckedContext {
        module,
        def_table,
        type_map,
        call_sigs,
        symbols,
        node_id_gen,
    } = ctx;
    let mut module = build_module(&module);
    let mut type_map = type_map;
    let mut node_id_gen = node_id_gen;
    let mut normalizer = Normalizer::new(&mut type_map, &call_sigs, &mut node_id_gen);
    normalizer.visit_module(&mut module);
    NormalizedContext {
        module,
        def_table,
        type_map,
        call_sigs,
        symbols,
        node_id_gen,
    }
}

struct Normalizer<'a> {
    type_map: &'a mut TypeMap,
    call_sigs: &'a CallSigMap,
    node_id_gen: &'a mut NodeIdGen,
}

impl<'a> Normalizer<'a> {
    fn new(
        type_map: &'a mut TypeMap,
        call_sigs: &'a CallSigMap,
        node_id_gen: &'a mut NodeIdGen,
    ) -> Self {
        Self {
            type_map,
            call_sigs,
            node_id_gen,
        }
    }

    fn coerce_call_args(&mut self, call_id: norm::NodeId, args: &mut [norm::CallArg]) {
        let Some(call_sig) = self.call_sigs.get(&call_id) else {
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

    fn visit_block_item(&mut self, item: &mut norm::BlockItem) {
        // Rewrite property assignments (`obj.prop = v`) into method calls
        // (`obj.prop(v)`) using the call signature recorded by type checking.
        let mut property_call = None;
        if let norm::BlockItem::Stmt(stmt) = item {
            if let norm::StmtExprKind::Assign {
                assignee, value, ..
            } = &stmt.kind
                && let norm::ExprKind::StructField { target, field } = &assignee.kind
                && self.call_sigs.contains_key(&assignee.id)
            {
                let call_expr = norm::Expr {
                    id: assignee.id,
                    kind: norm::ExprKind::MethodCall {
                        callee: target.clone(),
                        method_name: field.clone(),
                        args: vec![norm::CallArg {
                            mode: norm::CallArgMode::Default,
                            expr: *value.clone(),
                            init: norm::InitInfo::default(),
                            span: value.span,
                        }],
                    },
                    ty: self.type_map.insert_node_type(assignee.id, Type::Unit),
                    span: stmt.span,
                };
                property_call = Some(call_expr);
            }
        }

        if let Some(mut call_expr) = property_call {
            // Normalize the synthesized call and replace the statement with
            // an expression item so it follows the normal call lowering path.
            self.visit_expr(&mut call_expr);
            *item = norm::BlockItem::Expr(call_expr);
        } else {
            visit_mut::walk_block_item(self, item);
        }
    }

    fn visit_expr(&mut self, expr: &mut norm::Expr) {
        visit_mut::walk_expr(self, expr);
        // Rewrite property reads (`obj.prop`) into zero-arg method calls
        // (`obj.prop()`) when the type checker recorded a property getter.
        if let norm::ExprKind::StructField { target, field } = &expr.kind
            && self.call_sigs.contains_key(&expr.id)
        {
            expr.kind = norm::ExprKind::MethodCall {
                callee: target.clone(),
                method_name: field.clone(),
                args: Vec::new(),
            };
        }

        if let norm::ExprKind::Call { args, .. } | norm::ExprKind::MethodCall { args, .. } =
            &mut expr.kind
        {
            // Apply call-argument coercions (e.g., array-to-slice).
            self.coerce_call_args(expr.id, args);
        }
    }
}
