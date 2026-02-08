//! Program-level module preparation helpers.
//!
//! This module contains logic that is specific to composing a discovered
//! multi-module program into a single compile unit for the current pipeline.

use std::collections::{HashMap, HashSet};

use crate::context::ProgramParsedContext;
use crate::tree::parsed::{self, Module};
use crate::tree::visit_mut::{self, VisitorMut};

/// Merge prelude declarations ahead of user declarations.
pub(crate) fn merge_modules(prelude_module: &Module, user_module: &Module) -> Module {
    let mut requires =
        Vec::with_capacity(prelude_module.requires.len() + user_module.requires.len());
    requires.extend(prelude_module.requires.clone());
    requires.extend(user_module.requires.clone());

    let mut top_level_items = Vec::with_capacity(
        prelude_module.top_level_items.len() + user_module.top_level_items.len(),
    );
    top_level_items.extend(prelude_module.top_level_items.clone());
    top_level_items.extend(user_module.top_level_items.clone());

    Module {
        requires,
        top_level_items,
    }
}

/// Flatten discovered modules into one compile-unit module.
///
/// Dependency order is preserved, and known `alias.func(...)` patterns are
/// rewritten into plain function calls to fit the current single-module
/// resolve/typecheck/codegen pipeline.
pub(crate) fn flatten_program_module(program: &ProgramParsedContext) -> Module {
    let mut merged = Module {
        requires: Vec::new(),
        top_level_items: Vec::new(),
    };

    for module_id in program.dependency_order_from_entry() {
        let Some(parsed) = program.module(module_id) else {
            continue;
        };
        let mut module = parsed.module.clone();
        let alias_members = collect_alias_members(program, module_id);
        if !alias_members.is_empty() {
            let mut rewriter = ModuleAliasCallRewriter { alias_members };
            rewriter.visit_module(&mut module);
        }
        merged.top_level_items.extend(module.top_level_items);
    }

    merged
}

fn collect_alias_members(
    program: &ProgramParsedContext,
    module_id: crate::frontend::ModuleId,
) -> HashMap<String, HashSet<String>> {
    let mut alias_members = HashMap::new();
    let Some(parsed) = program.module(module_id) else {
        return alias_members;
    };
    for req in &parsed.requires {
        if let Some(dep_id) = program.program.by_path.get(&req.path)
            && let Some(dep_module) = program.module(*dep_id)
        {
            alias_members.insert(req.alias.clone(), callable_names(&dep_module.module));
        }
    }
    alias_members
}

fn callable_names(module: &Module) -> HashSet<String> {
    let mut names = HashSet::new();
    for item in &module.top_level_items {
        match item {
            parsed::TopLevelItem::FuncDecl(func_decl) => {
                names.insert(func_decl.sig.name.clone());
            }
            parsed::TopLevelItem::FuncDef(func_def) => {
                names.insert(func_def.sig.name.clone());
            }
            _ => {}
        }
    }
    names
}

struct ModuleAliasCallRewriter {
    alias_members: HashMap<String, HashSet<String>>,
}

impl VisitorMut<()> for ModuleAliasCallRewriter {
    fn visit_expr(&mut self, expr: &mut parsed::Expr) {
        visit_mut::walk_expr(self, expr);

        let parsed::ExprKind::MethodCall {
            callee,
            method_name,
            args,
        } = &expr.kind
        else {
            return;
        };
        let parsed::ExprKind::Var { ident: alias, .. } = &callee.kind else {
            return;
        };
        let Some(members) = self.alias_members.get(alias) else {
            return;
        };
        if !members.contains(method_name) {
            return;
        }

        let mut function_callee = (**callee).clone();
        if let parsed::ExprKind::Var { ident, .. } = &mut function_callee.kind {
            *ident = method_name.clone();
        }

        expr.kind = parsed::ExprKind::Call {
            callee: Box::new(function_callee),
            args: args.clone(),
        };
    }
}
