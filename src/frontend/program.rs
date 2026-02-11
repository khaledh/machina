//! Program-level module preparation helpers.
//!
//! This module contains logic that is specific to composing a discovered
//! multi-module program into a single compile unit for the current pipeline.

use std::collections::{HashMap, HashSet};

use crate::context::ProgramParsedContext;
use crate::diag::Span;
use crate::frontend::ModuleId;
use crate::frontend::bind::{AliasSymbols, ProgramBindings};
use crate::frontend::{FrontendError, ModulePath, RequireKind};
use crate::tree::NodeId;
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
#[derive(Clone)]
pub(crate) struct FlattenedProgram {
    pub module: Module,
    #[allow(dead_code)]
    pub top_level_owners: HashMap<NodeId, ModuleId>,
}

pub(crate) fn flatten_program(
    program: &ProgramParsedContext,
) -> Result<FlattenedProgram, Vec<FrontendError>> {
    let bindings = ProgramBindings::build(program);
    let conflicts = collect_conflicting_public_exports(program);

    let mut merged = Module {
        requires: Vec::new(),
        top_level_items: Vec::new(),
    };
    let mut errors = Vec::new();
    let mut top_level_owners = HashMap::new();

    errors.extend(validate_symbol_imports(program, &bindings));

    for module_id in program.dependency_order_from_entry() {
        let Some(parsed) = program.module(module_id) else {
            continue;
        };
        let mut module = parsed.module.clone();
        let alias_symbols = bindings.alias_symbols_for(module_id);
        let mut rewriter = ModuleAliasCallRewriter {
            alias_symbols,
            conflicts: &conflicts,
            errors: Vec::new(),
        };
        rewriter.visit_module(&mut module);
        errors.extend(rewriter.errors);

        if module_id != program.entry() {
            mangle_dependency_symbols(&mut module, &parsed.source.path, &conflicts);
        }

        for item in &module.top_level_items {
            top_level_owners.insert(top_level_item_id(item), module_id);
        }
        merged.top_level_items.extend(module.top_level_items);
    }

    if errors.is_empty() {
        Ok(FlattenedProgram {
            module: merged,
            top_level_owners,
        })
    } else {
        Err(errors)
    }
}

pub(crate) fn rewrite_program_module(
    program: &ProgramParsedContext,
    module_id: ModuleId,
) -> Result<Module, Vec<FrontendError>> {
    let bindings = ProgramBindings::build(program);
    let conflicts = collect_conflicting_public_exports(program);
    let Some(parsed) = program.module(module_id) else {
        return Ok(Module {
            requires: Vec::new(),
            top_level_items: Vec::new(),
        });
    };

    let mut module = parsed.module.clone();
    let alias_symbols = bindings.alias_symbols_for(module_id);
    let mut rewriter = ModuleAliasCallRewriter {
        alias_symbols,
        conflicts: &conflicts,
        errors: Vec::new(),
    };
    rewriter.visit_module(&mut module);
    if module_id != program.entry() {
        mangle_dependency_symbols(&mut module, &parsed.source.path, &conflicts);
    }
    module
        .top_level_items
        .extend(imported_symbol_stubs(program, module_id));

    if rewriter.errors.is_empty() {
        Ok(module)
    } else {
        Err(rewriter.errors)
    }
}

fn imported_symbol_stubs(
    program: &ProgramParsedContext,
    module_id: ModuleId,
) -> Vec<parsed::TopLevelItem> {
    let Some(parsed_module) = program.module(module_id) else {
        return Vec::new();
    };
    let mut stubs = Vec::new();
    for req in &parsed_module.requires {
        if req.kind != RequireKind::Symbol {
            continue;
        }
        let Some(member) = &req.member else {
            continue;
        };
        let Some(dep_id) = program.program.by_path.get(&req.module_path).copied() else {
            continue;
        };
        let Some(dep_module) = program.module(dep_id) else {
            continue;
        };
        let Some(export_item) = dep_module.module.top_level_items.iter().find(|item| {
            top_level_item_export_name(item) == Some(member.as_str())
                && top_level_item_is_public(item)
        }) else {
            continue;
        };
        if let Some(stub) = import_stub_from_top_level_item(export_item) {
            stubs.push(stub);
        }
    }
    stubs
}

fn top_level_item_export_name(item: &parsed::TopLevelItem) -> Option<&str> {
    match item {
        parsed::TopLevelItem::FuncDecl(func_decl) => Some(func_decl.sig.name.as_str()),
        parsed::TopLevelItem::FuncDef(func_def) => Some(func_def.sig.name.as_str()),
        parsed::TopLevelItem::TypeDef(type_def) => Some(type_def.name.as_str()),
        parsed::TopLevelItem::TraitDef(trait_def) => Some(trait_def.name.as_str()),
        parsed::TopLevelItem::MethodBlock(_) | parsed::TopLevelItem::ClosureDef(_) => None,
    }
}

fn top_level_item_is_public(item: &parsed::TopLevelItem) -> bool {
    match item {
        parsed::TopLevelItem::FuncDecl(func_decl) => has_public_attr(&func_decl.attrs),
        parsed::TopLevelItem::FuncDef(func_def) => has_public_attr(&func_def.attrs),
        parsed::TopLevelItem::TypeDef(type_def) => has_public_attr(&type_def.attrs),
        parsed::TopLevelItem::TraitDef(trait_def) => has_public_attr(&trait_def.attrs),
        parsed::TopLevelItem::MethodBlock(_) | parsed::TopLevelItem::ClosureDef(_) => false,
    }
}

fn import_stub_from_top_level_item(item: &parsed::TopLevelItem) -> Option<parsed::TopLevelItem> {
    match item {
        parsed::TopLevelItem::FuncDecl(func_decl) => {
            Some(parsed::TopLevelItem::FuncDecl(func_decl.clone()))
        }
        parsed::TopLevelItem::FuncDef(func_def) => {
            Some(parsed::TopLevelItem::FuncDecl(parsed::FuncDecl {
                id: func_def.id,
                def_id: func_def.def_id,
                attrs: func_def.attrs.clone(),
                sig: func_def.sig.clone(),
                span: func_def.span,
            }))
        }
        parsed::TopLevelItem::TypeDef(type_def) => {
            Some(parsed::TopLevelItem::TypeDef(type_def.clone()))
        }
        parsed::TopLevelItem::TraitDef(trait_def) => {
            Some(parsed::TopLevelItem::TraitDef(trait_def.clone()))
        }
        parsed::TopLevelItem::MethodBlock(_) | parsed::TopLevelItem::ClosureDef(_) => None,
    }
}

fn validate_symbol_imports(
    program: &ProgramParsedContext,
    bindings: &ProgramBindings,
) -> Vec<FrontendError> {
    let mut errors = Vec::new();
    for module_id in program.dependency_order_from_entry() {
        let Some(parsed) = program.module(module_id) else {
            continue;
        };
        for req in &parsed.requires {
            if req.kind != RequireKind::Symbol {
                continue;
            }
            let Some(member) = &req.member else {
                continue;
            };
            let Some(dep_id) = program.program.by_path.get(&req.module_path) else {
                continue;
            };
            let Some(exports) = bindings.exports_for(*dep_id) else {
                continue;
            };

            let callable = exports.callables.get(member);
            let type_def = exports.types.get(member);
            let trait_def = exports.traits.get(member);
            let has_any = callable.is_some() || type_def.is_some() || trait_def.is_some();
            let is_public = callable.is_some_and(|m| m.public)
                || type_def.is_some_and(|m| m.public)
                || trait_def.is_some_and(|m| m.public);
            if !has_any {
                errors.push(FrontendError::RequireMemberUndefined {
                    alias: req.alias.clone(),
                    module: req.module_path.clone(),
                    member: member.clone(),
                    expected_kind: "symbol",
                    span: req.span,
                });
                continue;
            }
            if !is_public {
                errors.push(FrontendError::RequireMemberPrivate {
                    alias: req.alias.clone(),
                    module: req.module_path.clone(),
                    member: member.clone(),
                    expected_kind: "symbol",
                    span: req.span,
                });
            }
        }
    }
    errors
}

fn has_public_attr(attrs: &[parsed::Attribute]) -> bool {
    attrs
        .iter()
        .any(|attr| attr.name == "public" || attr.name == "opaque")
}

#[derive(Default)]
struct ConflictingPublicExports {
    callables: HashSet<(ModulePath, String)>,
    types: HashSet<(ModulePath, String)>,
    traits: HashSet<(ModulePath, String)>,
}

impl ConflictingPublicExports {
    fn contains_callable(&self, module_path: &ModulePath, symbol: &str) -> bool {
        self.callables
            .contains(&(module_path.clone(), symbol.to_string()))
    }

    fn contains_type(&self, module_path: &ModulePath, symbol: &str) -> bool {
        self.types
            .contains(&(module_path.clone(), symbol.to_string()))
    }

    fn contains_trait(&self, module_path: &ModulePath, symbol: &str) -> bool {
        self.traits
            .contains(&(module_path.clone(), symbol.to_string()))
    }
}

fn collect_conflicting_public_exports(program: &ProgramParsedContext) -> ConflictingPublicExports {
    let mut callable_origins = HashMap::<String, HashSet<ModulePath>>::new();
    let mut type_origins = HashMap::<String, HashSet<ModulePath>>::new();
    let mut trait_origins = HashMap::<String, HashSet<ModulePath>>::new();

    for module_id in program.dependency_order_from_entry() {
        if module_id == program.entry() {
            continue;
        }
        let Some(parsed) = program.module(module_id) else {
            continue;
        };
        let module_path = parsed.source.path.clone();
        for item in &parsed.module.top_level_items {
            match item {
                parsed::TopLevelItem::FuncDecl(func_decl) if has_public_attr(&func_decl.attrs) => {
                    callable_origins
                        .entry(func_decl.sig.name.clone())
                        .or_default()
                        .insert(module_path.clone());
                }
                parsed::TopLevelItem::FuncDef(func_def) if has_public_attr(&func_def.attrs) => {
                    callable_origins
                        .entry(func_def.sig.name.clone())
                        .or_default()
                        .insert(module_path.clone());
                }
                parsed::TopLevelItem::TypeDef(type_def) if has_public_attr(&type_def.attrs) => {
                    type_origins
                        .entry(type_def.name.clone())
                        .or_default()
                        .insert(module_path.clone());
                }
                parsed::TopLevelItem::TraitDef(trait_def) if has_public_attr(&trait_def.attrs) => {
                    trait_origins
                        .entry(trait_def.name.clone())
                        .or_default()
                        .insert(module_path.clone());
                }
                _ => {}
            }
        }
    }

    let mut conflicts = ConflictingPublicExports::default();
    for (symbol, origins) in callable_origins {
        if origins.len() > 1 {
            for module in origins {
                conflicts.callables.insert((module, symbol.clone()));
            }
        }
    }
    for (symbol, origins) in type_origins {
        if origins.len() > 1 {
            for module in origins {
                conflicts.types.insert((module, symbol.clone()));
            }
        }
    }
    for (symbol, origins) in trait_origins {
        if origins.len() > 1 {
            for module in origins {
                conflicts.traits.insert((module, symbol.clone()));
            }
        }
    }
    conflicts
}

fn mangled_module_symbol(module_path: &ModulePath, symbol: &str) -> String {
    let module_tag = module_path.to_string().replace('.', "$");
    format!("__m${module_tag}${symbol}")
}

#[cfg(test)]
pub(crate) fn flatten_program_module(
    program: &ProgramParsedContext,
) -> Result<Module, Vec<FrontendError>> {
    flatten_program(program).map(|flattened| flattened.module)
}

fn top_level_item_id(item: &parsed::TopLevelItem) -> NodeId {
    match item {
        parsed::TopLevelItem::TraitDef(trait_def) => trait_def.id,
        parsed::TopLevelItem::TypeDef(type_def) => type_def.id,
        parsed::TopLevelItem::FuncDecl(func_decl) => func_decl.id,
        parsed::TopLevelItem::FuncDef(func_def) => func_def.id,
        parsed::TopLevelItem::MethodBlock(method_block) => method_block.id,
        parsed::TopLevelItem::ClosureDef(closure_def) => closure_def.id,
    }
}

struct ModuleAliasCallRewriter<'a> {
    alias_symbols: HashMap<String, AliasSymbols>,
    conflicts: &'a ConflictingPublicExports,
    errors: Vec<FrontendError>,
}

#[derive(Clone, Copy)]
enum ExpectedMemberKind {
    Callable,
    Type,
    Trait,
}

impl ExpectedMemberKind {
    fn as_str(self) -> &'static str {
        match self {
            ExpectedMemberKind::Callable => "function",
            ExpectedMemberKind::Type => "type",
            ExpectedMemberKind::Trait => "trait",
        }
    }
}

impl VisitorMut<()> for ModuleAliasCallRewriter<'_> {
    fn visit_method_block(&mut self, method_block: &mut parsed::MethodBlock) {
        if let Some(trait_name) = &mut method_block.trait_name {
            self.rewrite_qualified_name(trait_name, method_block.span, ExpectedMemberKind::Trait);
        }
        visit_mut::walk_method_block(self, method_block);
    }

    fn visit_expr(&mut self, expr: &mut parsed::Expr) {
        visit_mut::walk_expr(self, expr);

        if let parsed::ExprKind::Call { callee, .. } = &mut expr.kind {
            if let parsed::ExprKind::Var { ident, .. } = &mut callee.kind
                && let Some(new_name) = self.rewrite_qualified_callable_name(ident, expr.span)
            {
                *ident = new_name;
            }
        }

        if let parsed::ExprKind::Var { ident, .. } = &mut expr.kind
            && let Some(new_name) = self.rewrite_qualified_callable_name(ident, expr.span)
        {
            *ident = new_name;
        }

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
                let Some(member) = symbols.callables.get(method_name) else {
                    return;
                };
                if !member.public {
                    self.errors.push(FrontendError::RequireMemberPrivate {
                        alias: alias.clone(),
                        module: symbols.module_path.clone(),
                        member: method_name.clone(),
                        expected_kind: ExpectedMemberKind::Callable.as_str(),
                        span: expr.span,
                    });
                    return;
                }

                let mut function_callee = (**callee).clone();
                if let parsed::ExprKind::Var { ident, .. } = &mut function_callee.kind {
                    *ident = if self
                        .conflicts
                        .contains_callable(&symbols.module_path, method_name)
                    {
                        mangled_module_symbol(&symbols.module_path, method_name)
                    } else {
                        method_name.clone()
                    };
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
                let Some(member) = symbols.callables.get(field) else {
                    return;
                };
                if !member.public {
                    self.errors.push(FrontendError::RequireMemberPrivate {
                        alias: alias.clone(),
                        module: symbols.module_path.clone(),
                        member: field.clone(),
                        expected_kind: ExpectedMemberKind::Callable.as_str(),
                        span: expr.span,
                    });
                    return;
                }

                let mut function_ref = (**target).clone();
                if let parsed::ExprKind::Var { ident, .. } = &mut function_ref.kind {
                    *ident = if self
                        .conflicts
                        .contains_callable(&symbols.module_path, field)
                    {
                        mangled_module_symbol(&symbols.module_path, field)
                    } else {
                        field.clone()
                    };
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

        self.rewrite_qualified_name(ident, type_expr.span, ExpectedMemberKind::Type);
    }

    fn visit_type_param(&mut self, param: &mut parsed::TypeParam) {
        let Some(bound) = &mut param.bound else {
            return;
        };

        self.rewrite_qualified_name(&mut bound.name, bound.span, ExpectedMemberKind::Trait);
    }
}

impl ModuleAliasCallRewriter<'_> {
    fn rewrite_qualified_name(
        &mut self,
        ident: &mut String,
        span: Span,
        expected: ExpectedMemberKind,
    ) {
        let Some((alias, member)) = split_qualified_name(ident) else {
            return;
        };

        let Some(symbols) = self.alias_symbols.get(alias) else {
            self.errors.push(FrontendError::UnknownRequireAlias {
                alias: alias.to_string(),
                span,
            });
            return;
        };

        let found = match expected {
            ExpectedMemberKind::Callable => symbols.callables.contains_key(member),
            ExpectedMemberKind::Type => symbols.types.contains_key(member),
            ExpectedMemberKind::Trait => symbols.traits.contains_key(member),
        };

        if !found {
            self.errors.push(FrontendError::RequireMemberUndefined {
                alias: alias.to_string(),
                module: symbols.module_path.clone(),
                member: member.to_string(),
                expected_kind: expected.as_str(),
                span,
            });
            return;
        }

        let public = match expected {
            ExpectedMemberKind::Callable => symbols
                .callables
                .get(member)
                .is_some_and(|member_attrs| member_attrs.public),
            ExpectedMemberKind::Type => symbols
                .types
                .get(member)
                .is_some_and(|member_attrs| member_attrs.public),
            ExpectedMemberKind::Trait => symbols
                .traits
                .get(member)
                .is_some_and(|member_attrs| member_attrs.public),
        };

        if !public {
            self.errors.push(FrontendError::RequireMemberPrivate {
                alias: alias.to_string(),
                module: symbols.module_path.clone(),
                member: member.to_string(),
                expected_kind: expected.as_str(),
                span,
            });
            return;
        }

        let _opaque = symbols
            .types
            .get(member)
            .is_some_and(|member_attrs| member_attrs.opaque);
        *ident = match expected {
            ExpectedMemberKind::Callable
                if self
                    .conflicts
                    .contains_callable(&symbols.module_path, member) =>
            {
                mangled_module_symbol(&symbols.module_path, member)
            }
            ExpectedMemberKind::Type
                if self.conflicts.contains_type(&symbols.module_path, member) =>
            {
                mangled_module_symbol(&symbols.module_path, member)
            }
            ExpectedMemberKind::Trait
                if self.conflicts.contains_trait(&symbols.module_path, member) =>
            {
                mangled_module_symbol(&symbols.module_path, member)
            }
            _ => member.to_string(),
        };
    }

    fn rewrite_qualified_callable_name(&mut self, ident: &str, span: Span) -> Option<String> {
        let (alias, member) = split_qualified_name(ident)?;
        let symbols = self.alias_symbols.get(alias)?;
        let Some(member_attrs) = symbols.callables.get(member) else {
            self.errors.push(FrontendError::RequireMemberUndefined {
                alias: alias.to_string(),
                module: symbols.module_path.clone(),
                member: member.to_string(),
                expected_kind: ExpectedMemberKind::Callable.as_str(),
                span,
            });
            return None;
        };

        if !member_attrs.public {
            self.errors.push(FrontendError::RequireMemberPrivate {
                alias: alias.to_string(),
                module: symbols.module_path.clone(),
                member: member.to_string(),
                expected_kind: ExpectedMemberKind::Callable.as_str(),
                span,
            });
            return None;
        }

        Some(
            if self
                .conflicts
                .contains_callable(&symbols.module_path, member)
            {
                mangled_module_symbol(&symbols.module_path, member)
            } else {
                member.to_string()
            },
        )
    }
}

fn split_qualified_name(name: &str) -> Option<(&str, &str)> {
    name.split_once("::")
}

fn mangle_dependency_symbols(
    module: &mut Module,
    module_path: &ModulePath,
    conflicts: &ConflictingPublicExports,
) {
    let mut value_renames = HashMap::new();
    let mut type_renames = HashMap::new();
    let mut trait_renames = HashMap::new();

    for item in &mut module.top_level_items {
        match item {
            parsed::TopLevelItem::FuncDecl(func_decl) => {
                let old = func_decl.sig.name.clone();
                if !is_public_item(&func_decl.attrs)
                    || conflicts.contains_callable(module_path, &old)
                {
                    let new_name = mangled_module_symbol(module_path, &old);
                    func_decl.sig.name = new_name.clone();
                    value_renames.insert(old, new_name);
                }
            }
            parsed::TopLevelItem::FuncDef(func_def) => {
                let old = func_def.sig.name.clone();
                if !is_public_item(&func_def.attrs)
                    || conflicts.contains_callable(module_path, &old)
                {
                    let new_name = mangled_module_symbol(module_path, &old);
                    func_def.sig.name = new_name.clone();
                    value_renames.insert(old, new_name);
                }
            }
            parsed::TopLevelItem::TypeDef(type_def) => {
                let old = type_def.name.clone();
                if !is_public_item(&type_def.attrs) || conflicts.contains_type(module_path, &old) {
                    let new_name = mangled_module_symbol(module_path, &old);
                    type_def.name = new_name.clone();
                    type_renames.insert(old, new_name);
                }
            }
            parsed::TopLevelItem::TraitDef(trait_def) => {
                let old = trait_def.name.clone();
                if !is_public_item(&trait_def.attrs) || conflicts.contains_trait(module_path, &old)
                {
                    let new_name = mangled_module_symbol(module_path, &old);
                    trait_def.name = new_name.clone();
                    trait_renames.insert(old, new_name);
                }
            }
            _ => {}
        }
    }

    if value_renames.is_empty() && type_renames.is_empty() && trait_renames.is_empty() {
        return;
    }

    let mut renamer = PrivateSymbolRenamer {
        value_renames,
        type_renames,
        trait_renames,
        value_scopes: vec![HashMap::new()],
        type_param_scopes: vec![HashMap::new()],
    };
    renamer.visit_module(module);
}

fn is_public_item(attrs: &[parsed::Attribute]) -> bool {
    attrs
        .iter()
        .any(|attr| attr.name == "public" || attr.name == "opaque")
}

struct PrivateSymbolRenamer {
    value_renames: HashMap<String, String>,
    type_renames: HashMap<String, String>,
    trait_renames: HashMap<String, String>,
    value_scopes: Vec<HashMap<String, String>>,
    type_param_scopes: Vec<HashMap<String, String>>,
}

impl PrivateSymbolRenamer {
    fn push_value_scope(&mut self) {
        self.value_scopes.push(HashMap::new());
    }

    fn pop_value_scope(&mut self) {
        self.value_scopes.pop();
    }

    fn push_type_scope(&mut self) {
        self.type_param_scopes.push(HashMap::new());
    }

    fn pop_type_scope(&mut self) {
        self.type_param_scopes.pop();
    }

    fn bind_value_name(&mut self, name: &str) {
        if let Some(scope) = self.value_scopes.last_mut() {
            scope.insert(name.to_string(), name.to_string());
        }
    }

    fn bind_type_param(&mut self, name: &str) {
        if let Some(scope) = self.type_param_scopes.last_mut() {
            scope.insert(name.to_string(), name.to_string());
        }
    }

    fn value_rename_for_use(&self, name: &str) -> Option<String> {
        for scope in self.value_scopes.iter().rev() {
            if let Some(bound) = scope.get(name) {
                return Some(bound.clone());
            }
        }
        self.value_renames.get(name).cloned()
    }

    fn type_rename_for_use(&self, name: &str) -> Option<String> {
        for scope in self.type_param_scopes.iter().rev() {
            if scope.contains_key(name) {
                return None;
            }
        }
        self.type_renames.get(name).cloned()
    }

    fn trait_rename_for_use(&self, name: &str) -> Option<String> {
        self.trait_renames.get(name).cloned()
    }

    fn bind_pattern_names(&mut self, pattern: &parsed::BindPattern) {
        match &pattern.kind {
            parsed::BindPatternKind::Name { ident, .. } => self.bind_value_name(ident),
            parsed::BindPatternKind::Array { patterns }
            | parsed::BindPatternKind::Tuple { patterns } => {
                for child in patterns {
                    self.bind_pattern_names(child);
                }
            }
            parsed::BindPatternKind::Struct { fields, .. } => {
                for field in fields {
                    self.bind_pattern_names(&field.pattern);
                }
            }
        }
    }
}

impl VisitorMut<()> for PrivateSymbolRenamer {
    fn visit_func_sig(&mut self, func_sig: &mut parsed::FunctionSig) {
        self.push_type_scope();
        for type_param in &func_sig.type_params {
            self.bind_type_param(&type_param.ident);
        }
        for param in &mut func_sig.params {
            self.visit_type_expr(&mut param.typ);
        }
        self.visit_type_expr(&mut func_sig.ret_ty_expr);
        self.pop_type_scope();
    }

    fn visit_method_sig(&mut self, method_sig: &mut parsed::MethodSig) {
        self.push_type_scope();
        for type_param in &method_sig.type_params {
            self.bind_type_param(&type_param.ident);
        }
        for param in &mut method_sig.params {
            self.visit_type_expr(&mut param.typ);
        }
        self.visit_type_expr(&mut method_sig.ret_ty_expr);
        self.pop_type_scope();
    }

    fn visit_method_block(&mut self, method_block: &mut parsed::MethodBlock) {
        if let Some(new_name) = self.type_renames.get(&method_block.type_name) {
            method_block.type_name = new_name.clone();
        }
        if let Some(trait_name) = &mut method_block.trait_name
            && let Some(new_name) = self.trait_rename_for_use(trait_name)
        {
            *trait_name = new_name;
        }
        visit_mut::walk_method_block(self, method_block);
    }

    fn visit_type_expr(&mut self, type_expr: &mut parsed::TypeExpr) {
        visit_mut::walk_type_expr(self, type_expr);
        if let parsed::TypeExprKind::Named { ident, .. } = &mut type_expr.kind
            && let Some(new_name) = self.type_rename_for_use(ident)
        {
            *ident = new_name;
        }
    }

    fn visit_type_param(&mut self, param: &mut parsed::TypeParam) {
        if let Some(bound) = &mut param.bound
            && let Some(new_name) = self.trait_rename_for_use(&bound.name)
        {
            bound.name = new_name;
        }
    }

    fn visit_bind_pattern(&mut self, pattern: &mut parsed::BindPattern) {
        if let parsed::BindPatternKind::Struct { name, .. } = &mut pattern.kind
            && let Some(new_name) = self.type_renames.get(name)
        {
            *name = new_name.clone();
        }
        visit_mut::walk_bind_pattern(self, pattern);
    }

    fn visit_match_pattern(&mut self, pattern: &mut parsed::MatchPattern) {
        if let parsed::MatchPattern::EnumVariant {
            enum_name: Some(enum_name),
            ..
        } = pattern
            && let Some(new_name) = self.type_renames.get(enum_name)
        {
            *enum_name = new_name.clone();
        }
        visit_mut::walk_match_pattern(self, pattern);
    }

    fn visit_stmt_expr(&mut self, stmt: &mut parsed::StmtExpr) {
        match &mut stmt.kind {
            parsed::StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            }
            | parsed::StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                self.visit_expr(value);
                if let Some(decl_ty) = decl_ty {
                    self.visit_type_expr(decl_ty);
                }
                self.visit_bind_pattern(pattern);
                self.bind_pattern_names(pattern);
            }
            parsed::StmtExprKind::VarDecl { ident, decl_ty, .. } => {
                self.visit_type_expr(decl_ty);
                self.bind_value_name(ident);
            }
            parsed::StmtExprKind::Assign {
                assignee, value, ..
            } => {
                self.visit_expr(assignee);
                self.visit_expr(value);
            }
            parsed::StmtExprKind::While { cond, body } => {
                self.visit_expr(cond);
                self.push_value_scope();
                self.visit_expr(body);
                self.pop_value_scope();
            }
            parsed::StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                self.visit_expr(iter);
                self.push_value_scope();
                self.visit_bind_pattern(pattern);
                self.bind_pattern_names(pattern);
                self.visit_expr(body);
                self.pop_value_scope();
            }
            parsed::StmtExprKind::Break | parsed::StmtExprKind::Continue => {}
            parsed::StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.visit_expr(value);
                }
            }
        }
    }

    fn visit_expr(&mut self, expr: &mut parsed::Expr) {
        match &mut expr.kind {
            parsed::ExprKind::Block { items, tail } => {
                self.push_value_scope();
                for item in items {
                    self.visit_block_item(item);
                }
                if let Some(tail) = tail {
                    self.visit_expr(tail);
                }
                self.pop_value_scope();
            }
            parsed::ExprKind::Var { ident, .. } => {
                if let Some(new_name) = self.value_rename_for_use(ident) {
                    *ident = new_name;
                }
            }
            parsed::ExprKind::StructLit { name, .. } => {
                if let Some(new_name) = self.type_renames.get(name) {
                    *name = new_name.clone();
                }
                visit_mut::walk_expr(self, expr);
            }
            parsed::ExprKind::EnumVariant { enum_name, .. } => {
                if let Some(new_name) = self.type_renames.get(enum_name) {
                    *enum_name = new_name.clone();
                }
                visit_mut::walk_expr(self, expr);
            }
            parsed::ExprKind::Closure {
                captures,
                params,
                return_ty,
                body,
                ..
            } => {
                self.push_value_scope();
                self.push_type_scope();
                for capture in captures {
                    let parsed::CaptureSpec::Move { ident, .. } = capture;
                    self.bind_value_name(ident);
                }
                for param in params {
                    self.visit_type_expr(&mut param.typ);
                    self.bind_value_name(&param.ident);
                }
                self.visit_type_expr(return_ty);
                self.visit_expr(body);
                self.pop_type_scope();
                self.pop_value_scope();
            }
            _ => visit_mut::walk_expr(self, expr),
        }
    }

    fn visit_func_def(&mut self, func_def: &mut parsed::FuncDef) {
        self.visit_func_sig(&mut func_def.sig);
        self.push_value_scope();
        for param in &func_def.sig.params {
            self.bind_value_name(&param.ident);
        }
        self.visit_expr(&mut func_def.body);
        self.pop_value_scope();
    }

    fn visit_method_def(&mut self, method_def: &mut parsed::MethodDef) {
        self.visit_method_sig(&mut method_def.sig);
        self.push_value_scope();
        self.bind_value_name("self");
        for param in &method_def.sig.params {
            self.bind_value_name(&param.ident);
        }
        self.visit_expr(&mut method_def.body);
        self.pop_value_scope();
    }
}
