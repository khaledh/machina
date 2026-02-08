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
/// Dependency order is preserved, and known module-qualified references are
/// rewritten into unqualified names to fit the current single-module
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
        let alias_symbols = collect_alias_symbols(program, module_id);
        if !alias_symbols.is_empty() {
            let mut rewriter = ModuleAliasCallRewriter { alias_symbols };
            rewriter.visit_module(&mut module);
        }
        merged.top_level_items.extend(module.top_level_items);
    }

    merged
}

fn collect_alias_symbols(
    program: &ProgramParsedContext,
    module_id: crate::frontend::ModuleId,
) -> HashMap<String, AliasSymbols> {
    let mut alias_members = HashMap::new();
    let Some(parsed) = program.module(module_id) else {
        return alias_members;
    };
    for req in &parsed.requires {
        if let Some(dep_id) = program.program.by_path.get(&req.path)
            && let Some(dep_module) = program.module(*dep_id)
        {
            alias_members.insert(req.alias.clone(), module_symbols(&dep_module.module));
        }
    }
    alias_members
}

#[derive(Default)]
struct AliasSymbols {
    callables: HashSet<String>,
    types: HashSet<String>,
    traits: HashSet<String>,
}

fn module_symbols(module: &Module) -> AliasSymbols {
    let mut symbols = AliasSymbols::default();
    for item in &module.top_level_items {
        match item {
            parsed::TopLevelItem::FuncDecl(func_decl) => {
                symbols.callables.insert(func_decl.sig.name.clone());
            }
            parsed::TopLevelItem::FuncDef(func_def) => {
                symbols.callables.insert(func_def.sig.name.clone());
            }
            parsed::TopLevelItem::TypeDef(type_def) => {
                symbols.types.insert(type_def.name.clone());
            }
            parsed::TopLevelItem::TraitDef(trait_def) => {
                symbols.traits.insert(trait_def.name.clone());
            }
            _ => {}
        }
    }
    symbols
}

struct ModuleAliasCallRewriter {
    alias_symbols: HashMap<String, AliasSymbols>,
}

impl VisitorMut<()> for ModuleAliasCallRewriter {
    fn visit_method_block(&mut self, method_block: &mut parsed::MethodBlock) {
        if let Some(trait_name) = &mut method_block.trait_name
            && let Some((alias, member)) = trait_name.split_once('.')
            && let Some(symbols) = self.alias_symbols.get(alias)
            && symbols.traits.contains(member)
        {
            *trait_name = member.to_string();
        }
        visit_mut::walk_method_block(self, method_block);
    }

    fn visit_expr(&mut self, expr: &mut parsed::Expr) {
        visit_mut::walk_expr(self, expr);

        match &expr.kind {
            parsed::ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => {
                let parsed::ExprKind::Var { ident: alias, .. } = &callee.kind else {
                    return;
                };
                let Some(symbols) = self.alias_symbols.get(alias) else {
                    return;
                };
                if !symbols.callables.contains(method_name) {
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
            parsed::ExprKind::StructField { target, field } => {
                let parsed::ExprKind::Var { ident: alias, .. } = &target.kind else {
                    return;
                };
                let Some(symbols) = self.alias_symbols.get(alias) else {
                    return;
                };
                if !symbols.callables.contains(field) {
                    return;
                }

                let mut function_ref = (**target).clone();
                if let parsed::ExprKind::Var { ident, .. } = &mut function_ref.kind {
                    *ident = field.clone();
                }
                expr.kind = function_ref.kind;
            }
            _ => {}
        }
    }

    fn visit_type_expr(&mut self, type_expr: &mut parsed::TypeExpr) {
        visit_mut::walk_type_expr(self, type_expr);

        let parsed::TypeExprKind::Named { ident, .. } = &mut type_expr.kind else {
            return;
        };

        let Some((alias, member)) = ident.split_once('.') else {
            return;
        };

        let Some(symbols) = self.alias_symbols.get(alias) else {
            return;
        };

        if symbols.types.contains(member) {
            *ident = member.to_string();
        }
    }

    fn visit_type_param(&mut self, param: &mut parsed::TypeParam) {
        let Some(bound) = &mut param.bound else {
            return;
        };

        let Some((alias, member)) = bound.name.split_once('.') else {
            return;
        };

        let Some(symbols) = self.alias_symbols.get(alias) else {
            return;
        };

        if symbols.traits.contains(member) {
            bound.name = member.to_string();
        }
    }
}
