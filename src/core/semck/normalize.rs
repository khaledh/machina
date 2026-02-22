use crate::core::analysis::facts::{SyntheticReason, TypeMapOverlay};
use crate::core::context::{SemCheckNormalizedContext, SemCheckStageInput};
use crate::core::resolve::DefKind;
use crate::core::resolve::def_table::DefTable;
use crate::core::tree::visit_mut::{self, VisitorMut};
use crate::core::tree::*;
use crate::core::typecheck::type_map::{CallParam, CallSig, CallSigMap};
use crate::core::types::{
    Type, array_to_dyn_array_assignable, array_to_slice_assignable, dyn_array_to_slice_assignable,
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
    // underlying representation (`tree::model::Module`), so
    // normalization can mutate the owned module in place.
    let mut module = module;
    let mut def_table = def_table;
    let mut type_map = TypeMapOverlay::new(type_map);
    let mut node_id_gen = node_id_gen;
    let mut call_sigs = call_sigs;
    let mut normalizer = Normalizer::new(
        &mut def_table,
        &mut type_map,
        &mut call_sigs,
        &mut node_id_gen,
    );
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
    def_table: &'a mut DefTable,
    type_map: &'a mut TypeMapOverlay,
    call_sigs: &'a mut CallSigMap,
    node_id_gen: &'a mut NodeIdGen,
}

/// Reseeds node IDs for a cloned expression subtree.
///
/// We only use this for normalize-time synthetic rewriting (e.g. compound
/// assignment desugaring) where cloned nodes must not alias original node ids.
struct ExprIdReseeder<'a> {
    def_table: &'a mut DefTable,
    type_map: &'a mut TypeMapOverlay,
    node_id_gen: &'a mut NodeIdGen,
}

impl VisitorMut for ExprIdReseeder<'_> {
    fn visit_expr(&mut self, expr: &mut Expr) {
        expr.id = self.reseed_id(expr.id);
        match &mut expr.kind {
            ExprKind::StructLit { fields, .. } => {
                for field in fields {
                    field.id = self.reseed_id(field.id);
                }
            }
            ExprKind::MapLit { entries, .. } => {
                for entry in entries {
                    entry.id = self.reseed_id(entry.id);
                }
            }
            ExprKind::StructUpdate { fields, .. } => {
                for field in fields {
                    field.id = self.reseed_id(field.id);
                }
            }
            ExprKind::Closure { captures, .. } => {
                for capture in captures {
                    let CaptureSpec::Move { id, .. } = capture;
                    *id = self.reseed_id(*id);
                }
            }
            _ => {}
        }
        visit_mut::walk_expr(self, expr);
    }

    fn visit_stmt_expr(&mut self, stmt: &mut StmtExpr) {
        stmt.id = self.reseed_id(stmt.id);
        visit_mut::walk_stmt_expr(self, stmt);
    }

    fn visit_bind_pattern(&mut self, pattern: &mut BindPattern) {
        pattern.id = self.reseed_id(pattern.id);
        visit_mut::walk_bind_pattern(self, pattern);
    }

    fn visit_type_expr(&mut self, type_expr: &mut TypeExpr) {
        type_expr.id = self.reseed_id(type_expr.id);
        visit_mut::walk_type_expr(self, type_expr);
    }

    fn visit_match_arm(&mut self, arm: &mut MatchArm) {
        arm.id = self.reseed_id(arm.id);
        visit_mut::walk_match_arm(self, arm);
    }

    fn visit_match_pattern(&mut self, pattern: &mut MatchPattern) {
        match pattern {
            MatchPattern::Binding { id, .. } => {
                *id = self.reseed_id(*id);
            }
            MatchPattern::TypedBinding { id, .. } => {
                *id = self.reseed_id(*id);
            }
            MatchPattern::EnumVariant { id, .. } => {
                *id = self.reseed_id(*id);
            }
            _ => {}
        }
        visit_mut::walk_match_pattern(self, pattern);
    }

    fn visit_match_pattern_binding(&mut self, binding: &mut MatchPatternBinding) {
        if let MatchPatternBinding::Named { id, .. } = binding {
            *id = self.reseed_id(*id);
        }
        visit_mut::walk_match_pattern_binding(self, binding);
    }

    fn visit_param(&mut self, param: &mut Param) {
        param.id = self.reseed_id(param.id);
        visit_mut::walk_param(self, param);
    }
}

impl ExprIdReseeder<'_> {
    fn reseed_id(&mut self, old_id: NodeId) -> NodeId {
        let new_id = self.node_id_gen.new_id();
        if let Some(def_id) = self.def_table.lookup_node_def_id(old_id) {
            self.def_table.record_use(new_id, def_id);
        }
        if let Some(ty) = self.type_map.lookup_node_type(old_id) {
            self.type_map.insert_node_type(
                new_id,
                ty,
                "normalize",
                SyntheticReason::NormalizeCoercion,
            );
        }
        new_id
    }
}

impl<'a> Normalizer<'a> {
    fn new(
        def_table: &'a mut DefTable,
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

    fn coerce_call_args(&mut self, call_id: NodeId, args: &mut [CallArg]) {
        let Some(params) = self.call_sigs.get(&call_id).map(|sig| sig.params.clone()) else {
            return;
        };
        for (param, arg) in params.iter().zip(args.iter_mut()) {
            self.coerce_to_param(param, arg);
        }
    }

    fn coerce_to_param(&mut self, param: &CallParam, arg: &mut CallArg) {
        if !matches!(arg.mode, CallArgMode::Default | CallArgMode::InOut) {
            return;
        }
        self.coerce_expr_to_expected(&param.ty, &mut arg.expr);
    }

    fn coerce_kind_for(&self, from: &Type, to: &Type) -> Option<CoerceKind> {
        if array_to_slice_assignable(from, to) {
            return Some(CoerceKind::ArrayToSlice);
        }
        if array_to_dyn_array_assignable(from, to) {
            return Some(CoerceKind::ArrayToDynArray);
        }
        if dyn_array_to_slice_assignable(from, to) {
            return Some(CoerceKind::DynArrayToSlice);
        }
        None
    }

    fn coerce_expr_to_expected(&mut self, expected_ty: &Type, expr: &mut Expr) {
        let Some(from_ty) = self.type_map.lookup_node_type(expr.id) else {
            return;
        };
        let Some(kind) = self.coerce_kind_for(&from_ty, expected_ty) else {
            return;
        };
        if matches!(
            &expr.kind,
            ExprKind::Coerce {
                kind: existing_kind,
                ..
            } if *existing_kind == kind
        ) {
            return;
        }

        let span = expr.span;
        let inner = expr.clone();
        let coerce_id = self.node_id_gen.new_id();
        self.type_map.insert_node_type(
            coerce_id,
            expected_ty.clone(),
            "normalize",
            SyntheticReason::NormalizeCoercion,
        );
        *expr = Expr {
            id: coerce_id,
            kind: ExprKind::Coerce {
                kind,
                expr: Box::new(inner),
            },
            span,
        };
    }

    fn desugar_compound_assign(
        &mut self,
        stmt_span: crate::core::diag::Span,
        assignee: &Expr,
        op: BinaryOp,
        rhs: &Expr,
        init: InitInfo,
    ) -> StmtExprKind {
        let mut lhs = assignee.clone();
        // Ensure synthetic lhs read nodes don't alias assignee ids.
        ExprIdReseeder {
            def_table: self.def_table,
            type_map: self.type_map,
            node_id_gen: self.node_id_gen,
        }
        .visit_expr(&mut lhs);

        // Property writes are lowered later from Assign -> MethodCall when the
        // assignee has a setter call signature. For compound assignment, the
        // lhs read should use getter call semantics as well.
        if self.call_sigs.contains_key(&assignee.id)
            && let ExprKind::StructField { target, field } = &assignee.kind
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
            lhs = Expr {
                id: lhs_call_id,
                kind: ExprKind::MethodCall {
                    callee: target.clone(),
                    method_name: field.clone(),
                    args: Vec::new(),
                },
                span: assignee.span,
            };
            if let Some(assignee_ty) = self.type_map.lookup_node_type(assignee.id) {
                self.type_map.insert_node_type(
                    lhs_call_id,
                    assignee_ty,
                    "normalize",
                    SyntheticReason::NormalizeCoercion,
                );
            }
        }

        let value_id = self.node_id_gen.new_id();
        if let Some(assignee_ty) = self.type_map.lookup_node_type(assignee.id) {
            self.type_map.insert_node_type(
                value_id,
                assignee_ty,
                "normalize",
                SyntheticReason::NormalizeCoercion,
            );
        }
        let value = Expr {
            id: value_id,
            kind: ExprKind::BinOp {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs.clone()),
            },
            span: stmt_span,
        };

        StmtExprKind::Assign {
            assignee: Box::new(assignee.clone()),
            value: Box::new(value),
            init,
        }
    }
}

impl VisitorMut for Normalizer<'_> {
    fn visit_module(&mut self, module: &mut Module) {
        visit_mut::walk_module(self, module);
    }

    fn visit_block_item(&mut self, item: &mut BlockItem) {
        if let BlockItem::Stmt(stmt) = item
            && let StmtExprKind::CompoundAssign {
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
        if let BlockItem::Stmt(stmt) = item
            && let StmtExprKind::Assign {
                assignee, value, ..
            } = &stmt.kind
            && let ExprKind::StructField { target, field } = &assignee.kind
            && self.call_sigs.contains_key(&assignee.id)
        {
            let call_expr = Expr {
                id: assignee.id,
                kind: ExprKind::MethodCall {
                    callee: target.clone(),
                    method_name: field.clone(),
                    args: vec![CallArg {
                        mode: CallArgMode::Default,
                        expr: *value.clone(),
                        init: InitInfo::default(),
                        span: value.span,
                    }],
                },
                span: stmt.span,
            };
            self.type_map.insert_node_type(
                assignee.id,
                Type::Unit,
                "normalize",
                SyntheticReason::NormalizeCoercion,
            );
            property_call = Some(call_expr);
        }

        if let Some(mut call_expr) = property_call {
            // Normalize the synthesized call and replace the statement with
            // an expression item so it follows the normal call lowering path.
            self.visit_expr(&mut call_expr);
            *item = BlockItem::Expr(call_expr);
        } else {
            visit_mut::walk_block_item(self, item);

            if let BlockItem::Stmt(stmt) = item {
                match &mut stmt.kind {
                    StmtExprKind::LetBind { pattern, value, .. }
                    | StmtExprKind::VarBind { pattern, value, .. } => {
                        if let BindPatternKind::Name { .. } = pattern.kind
                            && let Some(def) =
                                self.def_table.lookup_def(self.def_table.def_id(pattern.id))
                            && let Some(expected_ty) = self.type_map.lookup_def_type(def)
                        {
                            self.coerce_expr_to_expected(&expected_ty, value);
                        }
                    }
                    StmtExprKind::Assign {
                        assignee, value, ..
                    } => {
                        if let Some(expected_ty) = self.type_map.lookup_node_type(assignee.id) {
                            self.coerce_expr_to_expected(&expected_ty, value);
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        visit_mut::walk_expr(self, expr);
        // Rewrite property reads (`obj.prop`) into zero-arg method calls
        // (`obj.prop()`) when the type checker recorded a property getter.
        if let ExprKind::StructField { target, field } = &expr.kind
            && self.call_sigs.contains_key(&expr.id)
        {
            expr.kind = ExprKind::MethodCall {
                callee: target.clone(),
                method_name: field.clone(),
                args: Vec::new(),
            };
        }

        if let ExprKind::Call { callee, args } = &expr.kind
            && let ExprKind::Var { ident } = &callee.kind
        {
            let def_id = self.def_table.def_id(callee.id);
            if self
                .def_table
                .lookup_def(def_id)
                .is_some_and(|def| matches!(def.kind, DefKind::EnumVariantName))
                && let Some(Type::Enum { name, .. }) = self.type_map.lookup_node_type(expr.id)
            {
                let enum_name = name
                    .split_once('<')
                    .map(|(base, _)| base.to_string())
                    .unwrap_or(name);
                let payload = args.iter().map(|arg| arg.expr.clone()).collect();
                expr.kind = ExprKind::EnumVariant {
                    enum_name,
                    type_args: Vec::new(),
                    variant: ident.clone(),
                    payload,
                };
                return;
            }
        }

        if let ExprKind::Call { args, .. } | ExprKind::MethodCall { args, .. } = &mut expr.kind {
            // Apply call-argument coercions (e.g., array-to-slice).
            self.coerce_call_args(expr.id, args);
        }
    }
}
