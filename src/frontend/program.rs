//! Program-level module preparation helpers.
//!
//! This module contains logic that is specific to composing a discovered
//! multi-module program into a single compile unit for the current pipeline.

use std::collections::HashMap;

use crate::context::ProgramParsedContext;
use crate::diag::Span;
use crate::frontend::ModuleId;
use crate::frontend::bind::{AliasSymbols, ProgramBindings};
use crate::frontend::{FrontendError, ModulePath};
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

    let mut merged = Module {
        requires: Vec::new(),
        top_level_items: Vec::new(),
    };
    let mut errors = Vec::new();
    let mut top_level_owners = HashMap::new();

    for module_id in program.dependency_order_from_entry() {
        let Some(parsed) = program.module(module_id) else {
            continue;
        };
        let mut module = parsed.module.clone();
        let alias_symbols = bindings.alias_symbols_for(module_id);
        let mut rewriter = ModuleAliasCallRewriter {
            alias_symbols,
            errors: Vec::new(),
        };
        rewriter.visit_module(&mut module);
        errors.extend(rewriter.errors);

        if module_id != program.entry() {
            mangle_private_symbols(&mut module, &parsed.source.path);
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

struct ModuleAliasCallRewriter {
    alias_symbols: HashMap<String, AliasSymbols>,
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

impl VisitorMut<()> for ModuleAliasCallRewriter {
    fn visit_method_block(&mut self, method_block: &mut parsed::MethodBlock) {
        if let Some(trait_name) = &mut method_block.trait_name {
            self.rewrite_qualified_name(trait_name, method_block.span, ExpectedMemberKind::Trait);
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

        self.rewrite_qualified_name(ident, type_expr.span, ExpectedMemberKind::Type);
    }

    fn visit_type_param(&mut self, param: &mut parsed::TypeParam) {
        let Some(bound) = &mut param.bound else {
            return;
        };

        self.rewrite_qualified_name(&mut bound.name, bound.span, ExpectedMemberKind::Trait);
    }
}

impl ModuleAliasCallRewriter {
    fn rewrite_qualified_name(
        &mut self,
        ident: &mut String,
        span: Span,
        expected: ExpectedMemberKind,
    ) {
        let Some((alias, member)) = ident.split_once('.') else {
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
        *ident = member.to_string();
    }
}

fn mangle_private_symbols(module: &mut Module, module_path: &ModulePath) {
    let mut value_renames = HashMap::new();
    let mut type_renames = HashMap::new();
    let mut trait_renames = HashMap::new();

    let module_tag = module_path.to_string().replace('.', "$");

    for item in &mut module.top_level_items {
        match item {
            parsed::TopLevelItem::FuncDecl(func_decl) => {
                if !is_public_item(&func_decl.attrs) {
                    let old = func_decl.sig.name.clone();
                    let new_name = format!("__m${module_tag}${old}");
                    func_decl.sig.name = new_name.clone();
                    value_renames.insert(old, new_name);
                }
            }
            parsed::TopLevelItem::FuncDef(func_def) => {
                if !is_public_item(&func_def.attrs) {
                    let old = func_def.sig.name.clone();
                    let new_name = format!("__m${module_tag}${old}");
                    func_def.sig.name = new_name.clone();
                    value_renames.insert(old, new_name);
                }
            }
            parsed::TopLevelItem::TypeDef(type_def) => {
                if !is_public_item(&type_def.attrs) {
                    let old = type_def.name.clone();
                    let new_name = format!("__m${module_tag}${old}");
                    type_def.name = new_name.clone();
                    type_renames.insert(old, new_name);
                }
            }
            parsed::TopLevelItem::TraitDef(trait_def) => {
                if !is_public_item(&trait_def.attrs) {
                    let old = trait_def.name.clone();
                    let new_name = format!("__m${module_tag}${old}");
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
