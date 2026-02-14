//! Definition owner mapping for flattened multi-module programs.
//!
//! This pass attaches a module owner to every definition reachable from a
//! top-level item (including nested defs like method params, locals, and
//! pattern bindings). Type checking uses this map for cross-module visibility
//! checks.

use std::collections::HashMap;

use crate::core::capsule::ModuleId;
use crate::core::context::ResolvedContext;
use crate::core::tree::NodeId;
use crate::core::tree::visit::{self, Visitor};

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
        let mut collector = DefOwnerCollector::new(*owner, &mut def_owners);
        match item {
            crate::core::tree::resolved::TopLevelItem::ProtocolDef(protocol_def) => {
                collector.visit_protocol_def(protocol_def)
            }
            crate::core::tree::resolved::TopLevelItem::TraitDef(trait_def) => {
                collector.visit_trait_def(trait_def)
            }
            crate::core::tree::resolved::TopLevelItem::TypeDef(type_def) => {
                collector.visit_type_def(type_def)
            }
            crate::core::tree::resolved::TopLevelItem::TypestateDef(typestate_def) => {
                collector.visit_typestate_def(typestate_def)
            }
            crate::core::tree::resolved::TopLevelItem::FuncDecl(func_decl) => {
                collector.visit_func_decl(func_decl)
            }
            crate::core::tree::resolved::TopLevelItem::FuncDef(func_def) => {
                collector.visit_func_def(func_def)
            }
            crate::core::tree::resolved::TopLevelItem::MethodBlock(method_block) => {
                collector.visit_method_block(method_block)
            }
            crate::core::tree::resolved::TopLevelItem::ClosureDef(closure_def) => {
                collector.visit_closure_def(closure_def)
            }
        }
    }

    resolved_context.with_def_owners(def_owners)
}

fn top_level_item_id(item: &crate::core::tree::resolved::TopLevelItem) -> NodeId {
    match item {
        crate::core::tree::resolved::TopLevelItem::ProtocolDef(protocol_def) => protocol_def.id,
        crate::core::tree::resolved::TopLevelItem::TraitDef(trait_def) => trait_def.id,
        crate::core::tree::resolved::TopLevelItem::TypeDef(type_def) => type_def.id,
        crate::core::tree::resolved::TopLevelItem::TypestateDef(typestate_def) => typestate_def.id,
        crate::core::tree::resolved::TopLevelItem::FuncDecl(func_decl) => func_decl.id,
        crate::core::tree::resolved::TopLevelItem::FuncDef(func_def) => func_def.id,
        crate::core::tree::resolved::TopLevelItem::MethodBlock(method_block) => method_block.id,
        crate::core::tree::resolved::TopLevelItem::ClosureDef(closure_def) => closure_def.id,
    }
}

struct DefOwnerCollector<'a> {
    owner: ModuleId,
    def_owners: &'a mut HashMap<DefId, ModuleId>,
}

impl<'a> DefOwnerCollector<'a> {
    fn new(owner: ModuleId, def_owners: &'a mut HashMap<DefId, ModuleId>) -> Self {
        Self { owner, def_owners }
    }

    fn record(&mut self, def_id: DefId) {
        self.def_owners.insert(def_id, self.owner);
    }
}

impl Visitor<DefId> for DefOwnerCollector<'_> {
    fn visit_protocol_def(&mut self, protocol_def: &crate::core::tree::resolved::ProtocolDef) {
        self.record(protocol_def.def_id);
        visit::walk_protocol_def(self, protocol_def);
    }

    fn visit_protocol_role(&mut self, role: &crate::core::tree::resolved::ProtocolRole) {
        self.record(role.def_id);
    }

    fn visit_trait_def(&mut self, trait_def: &crate::core::tree::resolved::TraitDef) {
        self.record(trait_def.def_id);
        visit::walk_trait_def(self, trait_def);
    }

    fn visit_type_def(&mut self, type_def: &crate::core::tree::resolved::TypeDef) {
        self.record(type_def.def_id);
        visit::walk_type_def(self, type_def);
    }

    fn visit_typestate_def(&mut self, typestate_def: &crate::core::tree::resolved::TypestateDef) {
        self.record(typestate_def.def_id);
        visit::walk_typestate_def(self, typestate_def);
    }

    fn visit_func_decl(&mut self, func_decl: &crate::core::tree::resolved::FuncDecl) {
        self.record(func_decl.def_id);
        visit::walk_func_decl(self, func_decl);
    }

    fn visit_func_def(&mut self, func_def: &crate::core::tree::resolved::FuncDef) {
        self.record(func_def.def_id);
        visit::walk_func_def(self, func_def);
    }

    fn visit_method_decl(&mut self, method_decl: &crate::core::tree::resolved::MethodDecl) {
        self.record(method_decl.def_id);
        visit::walk_method_decl(self, method_decl);
    }

    fn visit_method_def(&mut self, method_def: &crate::core::tree::resolved::MethodDef) {
        self.record(method_def.def_id);
        visit::walk_method_def(self, method_def);
    }

    fn visit_closure_def(&mut self, closure_def: &crate::core::tree::resolved::ClosureDef) {
        self.record(closure_def.def_id);
        visit::walk_closure_def(self, closure_def);
    }

    fn visit_type_param(&mut self, param: &crate::core::tree::resolved::TypeParam) {
        self.record(param.def_id);
        if let Some(bound) = &param.bound {
            self.record(bound.def_id);
        }
        visit::walk_type_param(self, param);
    }

    fn visit_method_sig(&mut self, method_sig: &crate::core::tree::resolved::MethodSig) {
        self.record(method_sig.self_param.def_id);
        visit::walk_method_sig(self, method_sig);
    }

    fn visit_param(&mut self, param: &crate::core::tree::resolved::Param) {
        self.record(param.def_id);
        visit::walk_param(self, param);
    }

    fn visit_stmt_expr(&mut self, stmt: &crate::core::tree::resolved::StmtExpr) {
        if let crate::core::tree::resolved::StmtExprKind::VarDecl { def_id, .. } = &stmt.kind {
            self.record(*def_id);
        }
        visit::walk_stmt_expr(self, stmt);
    }

    fn visit_bind_pattern(&mut self, pattern: &crate::core::tree::resolved::BindPattern) {
        if let crate::core::tree::resolved::BindPatternKind::Name { def_id, .. } = &pattern.kind {
            self.record(*def_id);
        }
        visit::walk_bind_pattern(self, pattern);
    }

    fn visit_match_pattern(&mut self, pattern: &crate::core::tree::resolved::MatchPattern) {
        match pattern {
            crate::core::tree::resolved::MatchPattern::Binding { def_id, .. }
            | crate::core::tree::resolved::MatchPattern::TypedBinding { def_id, .. } => {
                self.record(*def_id);
            }
            _ => {}
        }
        visit::walk_match_pattern(self, pattern);
    }

    fn visit_match_pattern_binding(
        &mut self,
        binding: &crate::core::tree::resolved::MatchPatternBinding,
    ) {
        if let crate::core::tree::resolved::MatchPatternBinding::Named { def_id, .. } = binding {
            self.record(*def_id);
        }
        visit::walk_match_pattern_binding(self, binding);
    }
}
