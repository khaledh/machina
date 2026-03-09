//! Definition owner mapping for flattened multi-module programs.
//!
//! This pass attaches a module owner to every definition reachable from a
//! top-level item (including nested defs like method params, locals, and
//! pattern bindings). Type checking uses this map for cross-module visibility
//! checks.

use std::collections::HashMap;

use crate::core::ast::visit::{self, Visitor};
use crate::core::ast::*;
use crate::core::capsule::ModuleId;
use crate::core::context::ResolvedContext;
use crate::core::resolve::DefTable;

use super::DefId;

pub fn attach_def_owners(
    resolved_context: ResolvedContext,
    top_level_owners: &HashMap<NodeId, ModuleId>,
) -> ResolvedContext {
    if top_level_owners.is_empty() {
        return resolved_context;
    }

    let mut def_owners = HashMap::new();
    for item in &resolved_context.module.top_level_items {
        let item_id = top_level_item_id(item);
        let Some(owner) = top_level_owners.get(&item_id) else {
            continue;
        };
        let mut collector =
            DefOwnerCollector::new(*owner, &resolved_context.def_table, &mut def_owners);
        match item {
            TopLevelItem::ProtocolDef(protocol_def) => collector.visit_protocol_def(protocol_def),
            TopLevelItem::TraitDef(trait_def) => collector.visit_trait_def(trait_def),
            TopLevelItem::TypeDef(type_def) => collector.visit_type_def(type_def),
            TopLevelItem::TypestateDef(typestate_def) => {
                collector.visit_typestate_def(typestate_def)
            }
            TopLevelItem::MachineDef(machine_def) => collector.visit_machine_def(machine_def),
            TopLevelItem::FuncDecl(func_decl) => collector.visit_func_decl(func_decl),
            TopLevelItem::FuncDef(func_def) => collector.visit_func_def(func_def),
            TopLevelItem::MethodBlock(method_block) => collector.visit_method_block(method_block),
            TopLevelItem::ClosureDef(closure_def) => collector.visit_closure_def(closure_def),
        }
    }

    resolved_context.with_def_owners(def_owners)
}

fn top_level_item_id(item: &TopLevelItem) -> NodeId {
    match item {
        TopLevelItem::ProtocolDef(protocol_def) => protocol_def.id,
        TopLevelItem::TraitDef(trait_def) => trait_def.id,
        TopLevelItem::TypeDef(type_def) => type_def.id,
        TopLevelItem::TypestateDef(typestate_def) => typestate_def.id,
        TopLevelItem::MachineDef(machine_def) => machine_def.id,
        TopLevelItem::FuncDecl(func_decl) => func_decl.id,
        TopLevelItem::FuncDef(func_def) => func_def.id,
        TopLevelItem::MethodBlock(method_block) => method_block.id,
        TopLevelItem::ClosureDef(closure_def) => closure_def.id,
    }
}

struct DefOwnerCollector<'a> {
    owner: ModuleId,
    def_table: &'a DefTable,
    def_owners: &'a mut HashMap<DefId, ModuleId>,
}

impl<'a> DefOwnerCollector<'a> {
    fn new(
        owner: ModuleId,
        def_table: &'a DefTable,
        def_owners: &'a mut HashMap<DefId, ModuleId>,
    ) -> Self {
        Self {
            owner,
            def_table,
            def_owners,
        }
    }

    fn record(&mut self, def_id: DefId) {
        self.def_owners.insert(def_id, self.owner);
    }

    fn record_node(&mut self, node_id: NodeId) {
        if let Some(def_id) = self.def_table.lookup_node_def_id(node_id) {
            self.record(def_id);
        }
    }
}

impl Visitor for DefOwnerCollector<'_> {
    fn visit_protocol_def(&mut self, protocol_def: &ProtocolDef) {
        self.record_node(protocol_def.id);
        visit::walk_protocol_def(self, protocol_def);
    }

    fn visit_protocol_role(&mut self, role: &ProtocolRole) {
        self.record_node(role.id);
    }

    fn visit_trait_def(&mut self, trait_def: &TraitDef) {
        self.record_node(trait_def.id);
        visit::walk_trait_def(self, trait_def);
    }

    fn visit_type_def(&mut self, type_def: &TypeDef) {
        self.record_node(type_def.id);
        visit::walk_type_def(self, type_def);
    }

    fn visit_typestate_def(&mut self, typestate_def: &TypestateDef) {
        self.record_node(typestate_def.id);
        visit::walk_typestate_def(self, typestate_def);
    }

    fn visit_func_decl(&mut self, func_decl: &FuncDecl) {
        self.record_node(func_decl.id);
        visit::walk_func_decl(self, func_decl);
    }

    fn visit_func_def(&mut self, func_def: &FuncDef) {
        self.record_node(func_def.id);
        visit::walk_func_def(self, func_def);
    }

    fn visit_method_decl(&mut self, method_decl: &MethodDecl) {
        self.record_node(method_decl.id);
        visit::walk_method_decl(self, method_decl);
    }

    fn visit_method_def(&mut self, method_def: &MethodDef) {
        self.record_node(method_def.id);
        visit::walk_method_def(self, method_def);
    }

    fn visit_closure_def(&mut self, closure_def: &ClosureDef) {
        self.record_node(closure_def.id);
        visit::walk_closure_def(self, closure_def);
    }

    fn visit_type_param(&mut self, param: &TypeParam) {
        self.record_node(param.id);
        if let Some(bound) = &param.bound {
            self.record_node(bound.id);
        }
        visit::walk_type_param(self, param);
    }

    fn visit_method_sig(&mut self, method_sig: &MethodSig) {
        self.record_node(method_sig.self_param.id);
        visit::walk_method_sig(self, method_sig);
    }

    fn visit_param(&mut self, param: &Param) {
        self.record_node(param.id);
        visit::walk_param(self, param);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        if let StmtExprKind::VarDecl { .. } = &stmt.kind {
            self.record_node(stmt.id);
        }
        visit::walk_stmt_expr(self, stmt);
    }

    fn visit_bind_pattern(&mut self, pattern: &BindPattern) {
        if let BindPatternKind::Name { .. } = &pattern.kind {
            self.record_node(pattern.id);
        }
        visit::walk_bind_pattern(self, pattern);
    }

    fn visit_using_binding(&mut self, binding: &UsingBinding) {
        self.record_node(binding.id);
        visit::walk_using_binding(self, binding);
    }

    fn visit_match_pattern(&mut self, pattern: &MatchPattern) {
        match pattern {
            MatchPattern::Binding { id, .. } | MatchPattern::TypedBinding { id, .. } => {
                self.record_node(*id);
            }
            _ => {}
        }
        visit::walk_match_pattern(self, pattern);
    }

    fn visit_match_pattern_binding(&mut self, binding: &MatchPatternBinding) {
        if let MatchPatternBinding::Named { id, .. } = binding {
            self.record_node(*id);
        }
        visit::walk_match_pattern_binding(self, binding);
    }
}
