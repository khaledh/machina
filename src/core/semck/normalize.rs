use crate::core::analysis::facts::{SyntheticReason, TypeMapOverlay};
use crate::core::context::{SemCheckNormalizedContext, SemCheckStageInput};
use crate::core::resolve::DefId;
use crate::core::resolve::DefKind;
use crate::core::resolve::def_table::DefTable;
use crate::core::tree::NodeIdGen;
use crate::core::tree::normalized as norm;
use crate::core::tree::visit_mut;
use crate::core::tree::visit_mut::VisitorMut;
use crate::core::typecheck::type_map::{CallParam, CallSig, CallSigMap};
use crate::core::types::{
    Type, TypeId, array_to_dyn_array_assignable, array_to_slice_assignable,
    dyn_array_to_slice_assignable,
};

/// Normalize a typed tree into a normalized tree.
///
/// This is a semcheck-internal prepass that performs a 1:1 typed->normalized
/// mapping and inserts explicit call-argument coercions.
pub fn normalize(ctx: SemCheckStageInput) -> SemCheckNormalizedContext {
    let crate::core::context::TypeCheckedContext { module, payload } = ctx;
    let crate::core::context::TypedTables {
        resolved,
        type_map,
        call_sigs,
        generic_insts,
    } = payload;
    let crate::core::context::ResolvedTables {
        def_table,
        def_owners,
        symbols,
        node_id_gen,
        typestate_role_impls,
        protocol_index,
    } = resolved;
    // `typed::Module` and `normalized::Module` currently share the same
    // underlying representation (`tree::model::Module<DefId, TypeId>`), so
    // normalization can mutate the owned module in place.
    let mut module: norm::Module = module;
    let mut type_map = TypeMapOverlay::new(type_map);
    let mut node_id_gen = node_id_gen;
    let mut call_sigs = call_sigs;
    let mut normalizer =
        Normalizer::new(&def_table, &mut type_map, &mut call_sigs, &mut node_id_gen);
    normalizer.visit_module(&mut module);
    SemCheckNormalizedContext {
        module,
        payload: crate::core::context::TypedTables {
            resolved: crate::core::context::ResolvedTables {
                def_table,
                def_owners,
                symbols,
                node_id_gen,
                typestate_role_impls,
                protocol_index,
            },
            type_map: type_map.into_inner(),
            call_sigs,
            generic_insts,
        },
    }
}

struct Normalizer<'a> {
    def_table: &'a DefTable,
    type_map: &'a mut TypeMapOverlay,
    call_sigs: &'a mut CallSigMap,
    node_id_gen: &'a mut NodeIdGen,
}

/// Reseeds node IDs for a cloned expression subtree.
///
/// We only use this for normalize-time synthetic rewriting (e.g. compound
/// assignment desugaring) where cloned nodes must not alias original node ids.
struct ExprIdReseeder<'a> {
    node_id_gen: &'a mut NodeIdGen,
}

impl VisitorMut<DefId, TypeId> for ExprIdReseeder<'_> {
    fn visit_expr(&mut self, expr: &mut norm::Expr) {
        expr.id = self.node_id_gen.new_id();
        match &mut expr.kind {
            norm::ExprKind::StructLit { fields, .. } => {
                for field in fields {
                    field.id = self.node_id_gen.new_id();
                }
            }
            norm::ExprKind::MapLit { entries, .. } => {
                for entry in entries {
                    entry.id = self.node_id_gen.new_id();
                }
            }
            norm::ExprKind::StructUpdate { fields, .. } => {
                for field in fields {
                    field.id = self.node_id_gen.new_id();
                }
            }
            norm::ExprKind::Closure { captures, .. } => {
                for capture in captures {
                    let norm::CaptureSpec::Move { id, .. } = capture;
                    *id = self.node_id_gen.new_id();
                }
            }
            _ => {}
        }
        visit_mut::walk_expr(self, expr);
    }

    fn visit_stmt_expr(&mut self, stmt: &mut norm::StmtExpr) {
        stmt.id = self.node_id_gen.new_id();
        visit_mut::walk_stmt_expr(self, stmt);
    }

    fn visit_bind_pattern(&mut self, pattern: &mut norm::BindPattern) {
        pattern.id = self.node_id_gen.new_id();
        visit_mut::walk_bind_pattern(self, pattern);
    }

    fn visit_type_expr(&mut self, type_expr: &mut norm::TypeExpr) {
        type_expr.id = self.node_id_gen.new_id();
        visit_mut::walk_type_expr(self, type_expr);
    }

    fn visit_match_arm(&mut self, arm: &mut norm::MatchArm) {
        arm.id = self.node_id_gen.new_id();
        visit_mut::walk_match_arm(self, arm);
    }

    fn visit_match_pattern(&mut self, pattern: &mut norm::MatchPattern) {
        match pattern {
            norm::MatchPattern::Binding { id, .. } => {
                *id = self.node_id_gen.new_id();
            }
            norm::MatchPattern::TypedBinding { id, .. } => {
                *id = self.node_id_gen.new_id();
            }
            norm::MatchPattern::EnumVariant { id, .. } => {
                *id = self.node_id_gen.new_id();
            }
            _ => {}
        }
        visit_mut::walk_match_pattern(self, pattern);
    }

    fn visit_match_pattern_binding(&mut self, binding: &mut norm::MatchPatternBinding) {
        if let norm::MatchPatternBinding::Named { id, .. } = binding {
            *id = self.node_id_gen.new_id();
        }
        visit_mut::walk_match_pattern_binding(self, binding);
    }

    fn visit_param(&mut self, param: &mut norm::Param) {
        param.id = self.node_id_gen.new_id();
        visit_mut::walk_param(self, param);
    }
}

impl<'a> Normalizer<'a> {
    fn new(
        def_table: &'a DefTable,
        type_map: &'a mut TypeMapOverlay,
        call_sigs: &'a mut CallSigMap,
        node_id_gen: &'a mut NodeIdGen,
    ) -> Self {
        Self {
            def_table,
            type_map,
            call_sigs,
            node_id_gen,
        }
    }

    fn coerce_call_args(&mut self, call_id: norm::NodeId, args: &mut [norm::CallArg]) {
        let Some(params) = self.call_sigs.get(&call_id).map(|sig| sig.params.clone()) else {
            return;
        };
        for (param, arg) in params.iter().zip(args.iter_mut()) {
            self.coerce_to_param(param, arg);
        }
    }

    fn coerce_to_param(&mut self, param: &CallParam, arg: &mut norm::CallArg) {
        if !matches!(
            arg.mode,
            norm::CallArgMode::Default | norm::CallArgMode::InOut
        ) {
            return;
        }
        self.coerce_expr_to_expected(&param.ty, &mut arg.expr);
    }

    fn coerce_kind_for(&self, from: &Type, to: &Type) -> Option<norm::CoerceKind> {
        if array_to_slice_assignable(from, to) {
            return Some(norm::CoerceKind::ArrayToSlice);
        }
        if array_to_dyn_array_assignable(from, to) {
            return Some(norm::CoerceKind::ArrayToDynArray);
        }
        if dyn_array_to_slice_assignable(from, to) {
            return Some(norm::CoerceKind::DynArrayToSlice);
        }
        None
    }

    fn coerce_expr_to_expected(&mut self, expected_ty: &Type, expr: &mut norm::Expr) {
        let Some(from_ty) = self.type_map.lookup_node_type(expr.id) else {
            return;
        };
        let Some(kind) = self.coerce_kind_for(&from_ty, expected_ty) else {
            return;
        };
        if matches!(
            &expr.kind,
            norm::ExprKind::Coerce {
                kind: existing_kind,
                ..
            } if *existing_kind == kind
        ) {
            return;
        }

        let span = expr.span;
        let inner = expr.clone();
        let coerce_id = self.node_id_gen.new_id();
        let ty_id = self.type_map.insert_node_type(
            coerce_id,
            expected_ty.clone(),
            "normalize",
            SyntheticReason::NormalizeCoercion,
        );
        *expr = norm::Expr {
            id: coerce_id,
            kind: norm::ExprKind::Coerce {
                kind,
                expr: Box::new(inner),
            },
            ty: ty_id,
            span,
        };
    }

    fn desugar_compound_assign(
        &mut self,
        stmt_span: crate::core::diag::Span,
        assignee: &norm::Expr,
        op: norm::BinaryOp,
        rhs: &norm::Expr,
        init: norm::InitInfo,
    ) -> norm::StmtExprKind {
        let mut lhs = assignee.clone();
        // Ensure synthetic lhs read nodes don't alias assignee ids.
        ExprIdReseeder {
            node_id_gen: self.node_id_gen,
        }
        .visit_expr(&mut lhs);

        // Property writes are lowered later from Assign -> MethodCall when the
        // assignee has a setter call signature. For compound assignment, the
        // lhs read should use getter call semantics as well.
        if self.call_sigs.contains_key(&assignee.id)
            && let norm::ExprKind::StructField { target, field } = &assignee.kind
        {
            let lhs_call_id = self.node_id_gen.new_id();
            if let Some(setter_sig) = self.call_sigs.get(&assignee.id).cloned() {
                // Compound-assignment reads require a getter-style call site.
                // Reuse receiver typing and drop positional params.
                self.call_sigs.insert(
                    lhs_call_id,
                    CallSig {
                        def_id: setter_sig.def_id,
                        receiver: setter_sig.receiver,
                        params: Vec::new(),
                    },
                );
            }
            lhs = norm::Expr {
                id: lhs_call_id,
                kind: norm::ExprKind::MethodCall {
                    callee: target.clone(),
                    method_name: field.clone(),
                    args: Vec::new(),
                },
                ty: assignee.ty,
                span: assignee.span,
            };
        }

        let value = norm::Expr {
            id: self.node_id_gen.new_id(),
            kind: norm::ExprKind::BinOp {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs.clone()),
            },
            // Result type is solver-constrained to be assignable back to the
            // assignee. Reuse assignee type id for normalized checks.
            ty: assignee.ty,
            span: stmt_span,
        };

        norm::StmtExprKind::Assign {
            assignee: Box::new(assignee.clone()),
            value: Box::new(value),
            init,
        }
    }
}

impl VisitorMut<DefId, TypeId> for Normalizer<'_> {
    fn visit_module(&mut self, module: &mut norm::Module) {
        visit_mut::walk_module(self, module);
    }

    fn visit_block_item(&mut self, item: &mut norm::BlockItem) {
        if let norm::BlockItem::Stmt(stmt) = item
            && let norm::StmtExprKind::CompoundAssign {
                assignee,
                op,
                value,
                init,
            } = &stmt.kind
        {
            stmt.kind = self.desugar_compound_assign(stmt.span, assignee, *op, value, *init);
        }

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
                    ty: self.type_map.insert_node_type(
                        assignee.id,
                        Type::Unit,
                        "normalize",
                        SyntheticReason::NormalizeCoercion,
                    ),
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

            if let norm::BlockItem::Stmt(stmt) = item {
                match &mut stmt.kind {
                    norm::StmtExprKind::LetBind { pattern, value, .. }
                    | norm::StmtExprKind::VarBind { pattern, value, .. } => {
                        if let norm::BindPatternKind::Name { def_id, .. } = pattern.kind
                            && let Some(def) = self.def_table.lookup_def(def_id)
                            && let Some(expected_ty) = self.type_map.lookup_def_type(def)
                        {
                            self.coerce_expr_to_expected(&expected_ty, value);
                        }
                    }
                    norm::StmtExprKind::Assign {
                        assignee, value, ..
                    } => {
                        let expected_ty = self.type_map.type_table().get(assignee.ty).clone();
                        self.coerce_expr_to_expected(&expected_ty, value);
                    }
                    _ => {}
                }
            }
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

        if let norm::ExprKind::Call { callee, args } = &expr.kind {
            if let norm::ExprKind::Var { def_id, ident } = &callee.kind {
                if self
                    .def_table
                    .lookup_def(*def_id)
                    .is_some_and(|def| matches!(def.kind, DefKind::EnumVariantName))
                    && let Some(Type::Enum { name, .. }) = self.type_map.lookup_node_type(expr.id)
                {
                    let enum_name = name
                        .split_once('<')
                        .map(|(base, _)| base.to_string())
                        .unwrap_or(name);
                    let payload = args.iter().map(|arg| arg.expr.clone()).collect();
                    expr.kind = norm::ExprKind::EnumVariant {
                        enum_name,
                        type_args: Vec::new(),
                        variant: ident.clone(),
                        payload,
                    };
                    return;
                }
            }
        }

        if let norm::ExprKind::Call { args, .. } | norm::ExprKind::MethodCall { args, .. } =
            &mut expr.kind
        {
            // Apply call-argument coercions (e.g., array-to-slice).
            self.coerce_call_args(expr.id, args);
        }
    }
}
