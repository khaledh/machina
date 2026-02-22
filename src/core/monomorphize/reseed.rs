//! AST reseeding and local-def remap helpers for monomorphization.

use std::collections::{HashMap, HashSet};

use crate::core::resolve::{DefId, DefKind, DefTable};
use crate::core::tree::visit::{self, Visitor};
use crate::core::tree::visit_mut::{self, VisitorMut};
use crate::core::tree::*;

macro_rules! collect_id_and_walk {
    ($(fn $visit_fn:ident($ty:ty) => $walk_fn:ident;)+) => {
        $(
            fn $visit_fn(&mut self, node: &$ty) {
                self.ids.push(node.id);
                visit::$walk_fn(self, node);
            }
        )+
    };
}

macro_rules! reseed_id_and_walk {
    ($(fn $visit_fn:ident($ty:ty) => $walk_fn:ident;)+) => {
        $(
            fn $visit_fn(&mut self, node: &mut $ty) {
                node.id = self.next_id();
                visit_mut::$walk_fn(self, node);
            }
        )+
    };
}

struct CallInstRewriter<'a> {
    call_inst_map: &'a HashMap<NodeId, DefId>,
}

impl<'a> VisitorMut for CallInstRewriter<'a> {
    fn visit_expr(&mut self, expr: &mut Expr) {
        visit_mut::walk_expr(self, expr);
        let _ = self.call_inst_map.get(&expr.id);
    }
}

pub(super) fn register_item_def_id(def_table: &mut DefTable, item: &TopLevelItem, def_id: DefId) {
    let (node_id, span) = match item {
        TopLevelItem::FuncDef(func_def) => (func_def.id, func_def.span),
        TopLevelItem::FuncDecl(func_decl) => (func_decl.id, func_decl.span),
        _ => return,
    };
    def_table.record_use(node_id, def_id);
    def_table.record_def_node(def_id, node_id, span);
}

pub(super) fn register_method_item_def_id(
    def_table: &mut DefTable,
    item: &MethodItem,
    def_id: DefId,
) {
    let (node_id, span) = match item {
        MethodItem::Def(method_def) => (method_def.id, method_def.span),
        MethodItem::Decl(method_decl) => (method_decl.id, method_decl.span),
    };
    def_table.record_use(node_id, def_id);
    def_table.record_def_node(def_id, node_id, span);
}

pub(super) fn replay_node_def_mappings(
    def_table: &mut DefTable,
    old_ids: &[NodeId],
    new_ids: &[NodeId],
) {
    assert_eq!(
        old_ids.len(),
        new_ids.len(),
        "compiler bug: reseed changed node visitation shape ({} -> {})",
        old_ids.len(),
        new_ids.len()
    );
    for (old_id, new_id) in old_ids.iter().zip(new_ids) {
        if let Some(def_id) = def_table.lookup_node_def_id(*old_id) {
            def_table.record_use(*new_id, def_id);
        }
    }
}

pub(super) fn collect_node_ids_in_top_level_item(item: &TopLevelItem) -> Vec<NodeId> {
    let mut collector = NodeIdCollector { ids: Vec::new() };
    match item {
        TopLevelItem::ProtocolDef(protocol_def) => collector.visit_protocol_def(protocol_def),
        TopLevelItem::TraitDef(trait_def) => collector.visit_trait_def(trait_def),
        TopLevelItem::TypeDef(type_def) => collector.visit_type_def(type_def),
        TopLevelItem::TypestateDef(typestate_def) => collector.visit_typestate_def(typestate_def),
        TopLevelItem::FuncDecl(func_decl) => collector.visit_func_decl(func_decl),
        TopLevelItem::FuncDef(func_def) => collector.visit_func_def(func_def),
        TopLevelItem::MethodBlock(method_block) => collector.visit_method_block(method_block),
        TopLevelItem::ClosureDef(closure_def) => collector.visit_closure_def(closure_def),
    }
    collector.ids
}

pub(super) fn collect_node_ids_in_method_item(item: &MethodItem) -> Vec<NodeId> {
    let mut collector = NodeIdCollector { ids: Vec::new() };
    match item {
        MethodItem::Decl(method_decl) => collector.visit_method_decl(method_decl),
        MethodItem::Def(method_def) => collector.visit_method_def(method_def),
    }
    collector.ids
}

struct NodeIdCollector {
    ids: Vec<NodeId>,
}

impl Visitor for NodeIdCollector {
    collect_id_and_walk! {
        fn visit_protocol_def(ProtocolDef) => walk_protocol_def;
        fn visit_protocol_role(ProtocolRole) => walk_protocol_role;
        fn visit_protocol_message(ProtocolMessage) => walk_protocol_message;
        fn visit_protocol_request_contract(ProtocolRequestContract) => walk_protocol_request_contract;
        fn visit_protocol_state(ProtocolState) => walk_protocol_state;
        fn visit_protocol_transition(ProtocolTransition) => walk_protocol_transition;
        fn visit_trait_def(TraitDef) => walk_trait_def;
        fn visit_trait_method(TraitMethod) => walk_trait_method;
        fn visit_trait_property(TraitProperty) => walk_trait_property;
        fn visit_typestate_def(TypestateDef) => walk_typestate_def;
        fn visit_typestate_role_impl(TypestateRoleImpl) => walk_typestate_role_impl;
        fn visit_typestate_fields(TypestateFields) => walk_typestate_fields;
        fn visit_typestate_state(TypestateState) => walk_typestate_state;
        fn visit_typestate_on_handler(TypestateOnHandler) => walk_typestate_on_handler;
        fn visit_type_def(TypeDef) => walk_type_def;
        fn visit_struct_def_field(StructDefField) => walk_struct_def_field;
        fn visit_enum_def_variant(EnumDefVariant) => walk_enum_def_variant;
        fn visit_type_expr(TypeExpr) => walk_type_expr;
        fn visit_func_decl(FuncDecl) => walk_func_decl;
        fn visit_func_def(FuncDef) => walk_func_def;
        fn visit_type_param(TypeParam) => walk_type_param;
        fn visit_self_param(SelfParam) => walk_self_param;
        fn visit_param(Param) => walk_param;
        fn visit_method_block(MethodBlock) => walk_method_block;
        fn visit_method_decl(MethodDecl) => walk_method_decl;
        fn visit_method_def(MethodDef) => walk_method_def;
        fn visit_closure_def(ClosureDef) => walk_closure_def;
        fn visit_bind_pattern(BindPattern) => walk_bind_pattern;
        fn visit_match_arm(MatchArm) => walk_match_arm;
    }

    fn visit_protocol_trigger(&mut self, trigger: &ProtocolTrigger) {
        visit::walk_protocol_trigger(self, trigger);
    }

    fn visit_protocol_effect(&mut self, effect: &ProtocolEffect) {
        visit::walk_protocol_effect(self, effect);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        self.ids.push(stmt.id);
        if let StmtExprKind::VarDecl { .. } = &stmt.kind {
            // Preserve legacy id-shape for var-decl statements used by def remap replay.
            self.ids.push(stmt.id);
        }
        visit::walk_stmt_expr(self, stmt);
    }

    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        match pattern {
            MatchPattern::Binding { id, .. } | MatchPattern::TypedBinding { id, .. } => {
                self.ids.push(*id);
            }
            MatchPattern::EnumVariant { id, .. } => self.ids.push(*id),
            _ => {}
        }
        visit::walk_match_pattern(self, pattern);
    }

    fn visit_match_pattern_binding(&mut self, binding: &MatchPatternBinding) {
        if let MatchPatternBinding::Named { id, .. } = binding {
            self.ids.push(*id);
        }
        visit::walk_match_pattern_binding(self, binding);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        self.ids.push(expr.id);
        if let ExprKind::Closure { captures, .. } = &expr.kind {
            for capture in captures {
                let CaptureSpec::Move { id, .. } = capture;
                self.ids.push(*id);
            }
        }
        visit::walk_expr(self, expr);
    }
}

pub(super) fn rewrite_calls_in_item(
    item: &mut TopLevelItem,
    call_inst_map: &HashMap<NodeId, DefId>,
) {
    let mut rewriter = CallInstRewriter { call_inst_map };
    match item {
        TopLevelItem::FuncDef(func_def) => rewriter.visit_func_def(func_def),
        TopLevelItem::FuncDecl(func_decl) => rewriter.visit_func_decl(func_decl),
        TopLevelItem::MethodBlock(method_block) => rewriter.visit_method_block(method_block),
        TopLevelItem::ClosureDef(closure_def) => rewriter.visit_closure_def(closure_def),
        TopLevelItem::ProtocolDef(_)
        | TopLevelItem::TypeDef(_)
        | TopLevelItem::TraitDef(_)
        | TopLevelItem::TypestateDef(_) => {}
    }
}

pub(super) fn rewrite_calls_in_method_item(
    item: &mut MethodItem,
    call_inst_map: &HashMap<NodeId, DefId>,
) {
    let mut rewriter = CallInstRewriter { call_inst_map };
    match item {
        MethodItem::Def(method_def) => rewriter.visit_method_def(method_def),
        MethodItem::Decl(method_decl) => rewriter.visit_method_decl(method_decl),
    }
}

pub(super) fn remap_local_defs_in_item(
    item: TopLevelItem,
    def_table: &mut DefTable,
) -> TopLevelItem {
    let node_ids = collect_node_ids_in_top_level_item(&item);
    remap_local_defs_for_nodes(def_table, &node_ids);
    item
}

pub(super) fn remap_local_defs_in_method_item(
    item: MethodItem,
    def_table: &mut DefTable,
) -> MethodItem {
    let node_ids = collect_node_ids_in_method_item(&item);
    remap_local_defs_for_nodes(def_table, &node_ids);
    item
}

fn remap_local_defs_for_nodes(def_table: &mut DefTable, node_ids: &[NodeId]) {
    let node_set: HashSet<NodeId> = node_ids.iter().copied().collect();
    let mut old_to_new = HashMap::<DefId, DefId>::new();

    for node_id in node_ids {
        let Some(old_def_id) = def_table.lookup_node_def_id(*node_id) else {
            continue;
        };
        if old_to_new.contains_key(&old_def_id) {
            continue;
        }
        let Some(def) = def_table.lookup_def(old_def_id) else {
            continue;
        };
        if !matches!(def.kind, DefKind::Param { .. } | DefKind::LocalVar { .. }) {
            continue;
        }

        let name = def.name.clone();
        let kind = def.kind.clone();
        let new_def_id = def_table.add_def(name, kind);
        old_to_new.insert(old_def_id, new_def_id);

        if let Some(def_node) = def_table.lookup_def_node_id(old_def_id)
            && node_set.contains(&def_node)
        {
            let span = def_table.lookup_def_span(old_def_id).unwrap_or_default();
            def_table.record_def_node(new_def_id, def_node, span);
        }
    }

    if old_to_new.is_empty() {
        return;
    }

    for node_id in node_ids {
        let Some(old_def_id) = def_table.lookup_node_def_id(*node_id) else {
            continue;
        };
        if let Some(new_def_id) = old_to_new.get(&old_def_id) {
            def_table.record_use(*node_id, *new_def_id);
        }
    }
}

pub(super) fn reseed_ids_in_item(item: &mut TopLevelItem, node_id_gen: &mut NodeIdGen) {
    let mut reseeder = NodeIdReseeder { node_id_gen };
    match item {
        TopLevelItem::ProtocolDef(protocol_def) => reseeder.visit_protocol_def(protocol_def),
        TopLevelItem::TraitDef(trait_def) => reseeder.visit_trait_def(trait_def),
        TopLevelItem::TypeDef(type_def) => reseeder.visit_type_def(type_def),
        TopLevelItem::TypestateDef(typestate_def) => reseeder.visit_typestate_def(typestate_def),
        TopLevelItem::FuncDecl(func_decl) => reseeder.visit_func_decl(func_decl),
        TopLevelItem::FuncDef(func_def) => reseeder.visit_func_def(func_def),
        TopLevelItem::MethodBlock(method_block) => reseeder.visit_method_block(method_block),
        TopLevelItem::ClosureDef(closure_def) => reseeder.visit_closure_def(closure_def),
    }
}

pub(super) fn reseed_ids_in_method_item(item: &mut MethodItem, node_id_gen: &mut NodeIdGen) {
    let mut reseeder = NodeIdReseeder { node_id_gen };
    match item {
        MethodItem::Def(method_def) => reseeder.visit_method_def(method_def),
        MethodItem::Decl(method_decl) => reseeder.visit_method_decl(method_decl),
    }
}

struct NodeIdReseeder<'a> {
    node_id_gen: &'a mut NodeIdGen,
}

impl NodeIdReseeder<'_> {
    fn next_id(&mut self) -> NodeId {
        self.node_id_gen.new_id()
    }
}

impl VisitorMut for NodeIdReseeder<'_> {
    reseed_id_and_walk! {
        fn visit_protocol_def(ProtocolDef) => walk_protocol_def;
        fn visit_protocol_role(ProtocolRole) => walk_protocol_role;
        fn visit_protocol_message(ProtocolMessage) => walk_protocol_message;
        fn visit_protocol_request_contract(ProtocolRequestContract) => walk_protocol_request_contract;
        fn visit_protocol_state(ProtocolState) => walk_protocol_state;
        fn visit_protocol_transition(ProtocolTransition) => walk_protocol_transition;
        fn visit_trait_def(TraitDef) => walk_trait_def;
        fn visit_trait_method(TraitMethod) => walk_trait_method;
        fn visit_trait_property(TraitProperty) => walk_trait_property;
        fn visit_typestate_def(TypestateDef) => walk_typestate_def;
        fn visit_typestate_role_impl(TypestateRoleImpl) => walk_typestate_role_impl;
        fn visit_typestate_fields(TypestateFields) => walk_typestate_fields;
        fn visit_typestate_state(TypestateState) => walk_typestate_state;
        fn visit_typestate_on_handler(TypestateOnHandler) => walk_typestate_on_handler;
        fn visit_type_def(TypeDef) => walk_type_def;
        fn visit_struct_def_field(StructDefField) => walk_struct_def_field;
        fn visit_enum_def_variant(EnumDefVariant) => walk_enum_def_variant;
        fn visit_type_expr(TypeExpr) => walk_type_expr;
        fn visit_func_decl(FuncDecl) => walk_func_decl;
        fn visit_func_def(FuncDef) => walk_func_def;
        fn visit_self_param(SelfParam) => walk_self_param;
        fn visit_param(Param) => walk_param;
        fn visit_method_block(MethodBlock) => walk_method_block;
        fn visit_method_decl(MethodDecl) => walk_method_decl;
        fn visit_method_def(MethodDef) => walk_method_def;
        fn visit_closure_def(ClosureDef) => walk_closure_def;
        fn visit_match_arm(MatchArm) => walk_match_arm;
    }

    fn visit_type_param(&mut self, param: &mut TypeParam) {
        param.id = self.next_id();
        if let Some(bound) = &mut param.bound {
            bound.id = self.next_id();
        }
    }

    fn visit_bind_pattern(&mut self, pattern: &mut BindPattern) {
        pattern.id = self.next_id();
        if matches!(pattern.kind, BindPatternKind::Name { .. }) {
            // Preserve legacy behavior used during old/new node-id replay.
            pattern.id = self.next_id();
        }
        visit_mut::walk_bind_pattern(self, pattern);
    }

    fn visit_match_pattern(&mut self, pattern: &mut MatchPattern) {
        match pattern {
            MatchPattern::Binding { id, .. } => *id = self.next_id(),
            MatchPattern::TypedBinding { id, .. } => *id = self.next_id(),
            MatchPattern::EnumVariant { id, .. } => *id = self.next_id(),
            MatchPattern::Wildcard { .. }
            | MatchPattern::BoolLit { .. }
            | MatchPattern::IntLit { .. }
            | MatchPattern::Tuple { .. } => {}
        }
        visit_mut::walk_match_pattern(self, pattern);
    }

    fn visit_match_pattern_binding(&mut self, binding: &mut MatchPatternBinding) {
        if let MatchPatternBinding::Named { id, .. } = binding {
            *id = self.next_id();
        }
        visit_mut::walk_match_pattern_binding(self, binding);
    }

    fn visit_stmt_expr(&mut self, stmt: &mut StmtExpr) {
        stmt.id = self.next_id();
        match &mut stmt.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            }
            | StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                self.visit_bind_pattern(pattern);
                if let Some(ty) = decl_ty {
                    self.visit_type_expr(ty);
                }
                self.visit_expr(value);
            }
            StmtExprKind::VarDecl { decl_ty, .. } => self.visit_type_expr(decl_ty),
            StmtExprKind::Assign {
                assignee, value, ..
            }
            | StmtExprKind::CompoundAssign {
                assignee, value, ..
            } => {
                self.visit_expr(assignee);
                self.visit_expr(value);
            }
            StmtExprKind::While { cond, body } => {
                self.visit_expr(cond);
                self.visit_expr(body);
            }
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                self.visit_bind_pattern(pattern);
                self.visit_expr(iter);
                self.visit_expr(body);
            }
            StmtExprKind::Break | StmtExprKind::Continue => {}
            StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.visit_expr(value);
                }
            }
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        expr.id = self.next_id();
        match &mut expr.kind {
            ExprKind::Block { items, tail } => {
                for item in items {
                    self.visit_block_item(item);
                }
                if let Some(tail) = tail {
                    self.visit_expr(tail);
                }
            }
            ExprKind::StringFmt { segments } => {
                for segment in segments {
                    if let StringFmtSegment::Expr { expr, .. } = segment {
                        self.visit_expr(expr);
                    }
                }
            }
            ExprKind::ArrayLit { init, elem_ty } => {
                if let Some(elem_ty) = elem_ty {
                    self.visit_type_expr(elem_ty);
                }
                match init {
                    ArrayLitInit::Elems(elems) => {
                        for elem in elems {
                            self.visit_expr(elem);
                        }
                    }
                    ArrayLitInit::Repeat(expr, _) => self.visit_expr(expr),
                }
            }
            ExprKind::SetLit { elem_ty, elems } => {
                if let Some(elem_ty) = elem_ty {
                    self.visit_type_expr(elem_ty);
                }
                for elem in elems {
                    self.visit_expr(elem);
                }
            }
            ExprKind::MapLit {
                key_ty,
                value_ty,
                entries,
            } => {
                if let Some(key_ty) = key_ty {
                    self.visit_type_expr(key_ty);
                }
                if let Some(value_ty) = value_ty {
                    self.visit_type_expr(value_ty);
                }
                for entry in entries {
                    entry.id = self.next_id();
                    self.visit_expr(&mut entry.key);
                    self.visit_expr(&mut entry.value);
                }
            }
            ExprKind::TupleLit(fields) => {
                for field in fields {
                    self.visit_expr(field);
                }
            }
            ExprKind::StructLit {
                type_args, fields, ..
            } => {
                for arg in type_args {
                    self.visit_type_expr(arg);
                }
                for field in fields {
                    field.id = self.next_id();
                    self.visit_expr(&mut field.value);
                }
            }
            ExprKind::EnumVariant {
                type_args, payload, ..
            } => {
                for arg in type_args {
                    self.visit_type_expr(arg);
                }
                for expr in payload {
                    self.visit_expr(expr);
                }
            }
            ExprKind::StructUpdate { target, fields } => {
                self.visit_expr(target);
                for field in fields {
                    field.id = self.next_id();
                    self.visit_expr(&mut field.value);
                }
            }
            ExprKind::BinOp { left, right, .. } => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            ExprKind::UnaryOp { expr, .. } => self.visit_expr(expr),
            ExprKind::Try {
                fallible_expr,
                on_error,
            } => {
                self.visit_expr(fallible_expr);
                if let Some(handler) = on_error {
                    self.visit_expr(handler);
                }
            }
            ExprKind::HeapAlloc { expr }
            | ExprKind::Move { expr }
            | ExprKind::AddrOf { expr }
            | ExprKind::Deref { expr }
            | ExprKind::ImplicitMove { expr }
            | ExprKind::Coerce { expr, .. } => self.visit_expr(expr),
            ExprKind::ArrayIndex { target, indices } => {
                self.visit_expr(target);
                for index in indices {
                    self.visit_expr(index);
                }
            }
            ExprKind::TupleField { target, .. } | ExprKind::StructField { target, .. } => {
                self.visit_expr(target)
            }
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                self.visit_expr(cond);
                self.visit_expr(then_body);
                self.visit_expr(else_body);
            }
            ExprKind::Range { start, end } => {
                self.visit_expr(start);
                self.visit_expr(end);
            }
            ExprKind::Slice { target, start, end } => {
                self.visit_expr(target);
                if let Some(start) = start {
                    self.visit_expr(start);
                }
                if let Some(end) = end {
                    self.visit_expr(end);
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                self.visit_expr(scrutinee);
                for arm in arms {
                    self.visit_match_arm(arm);
                }
            }
            ExprKind::Call { callee, args } | ExprKind::MethodCall { callee, args, .. } => {
                self.visit_expr(callee);
                for arg in args {
                    self.visit_expr(&mut arg.expr);
                }
            }
            ExprKind::Emit { kind } => match kind {
                EmitKind::Send { to, payload }
                | EmitKind::Request {
                    to,
                    payload,
                    request_site_label: _,
                } => {
                    self.visit_expr(to);
                    self.visit_expr(payload);
                }
            },
            ExprKind::Reply { cap, value } => {
                self.visit_expr(cap);
                self.visit_expr(value);
            }
            ExprKind::Closure {
                params,
                return_ty,
                body,
                captures,
                ..
            } => {
                for param in params {
                    self.visit_param(param);
                }
                for capture in captures {
                    let CaptureSpec::Move { id, .. } = capture;
                    *id = self.next_id();
                }
                self.visit_type_expr(return_ty);
                self.visit_expr(body);
            }
            ExprKind::UnitLit
            | ExprKind::IntLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit { .. }
            | ExprKind::Var { .. } => {}
        }
    }
}
