use std::collections::{HashMap, HashSet};

use crate::core::capsule::{ModuleId, RequireKind};
use crate::core::context::{
    CapsuleResolveStageInput, CapsuleResolveStageOutput, ImportEnv, ImportedSymbolBinding,
    ModuleExportFacts, ModuleImportBinding, ParsedContext, ResolveStageInput, ResolveStageOutput,
    ResolvedContext,
};
use crate::core::diag::Span;
use crate::core::resolve::def_table::{DefTable, DefTableBuilder, NodeDefLookup};
use crate::core::resolve::errors::ResolveError;
use crate::core::resolve::symbols::{Scope, Symbol, SymbolKind};
use crate::core::resolve::{
    Def, DefId, DefIdGen, DefKind, FuncAttrs, GlobalDefId, TraitAttrs, TypeAttrs, Visibility,
};
use crate::core::tree::ParamMode;
use crate::core::tree::parsed::*;
use crate::core::tree::resolved::builder::build_module;
use crate::core::tree::visit::*;
use crate::core::types::{BUILTIN_TYPES, Type};

#[derive(Clone, Debug)]
pub struct ImportedModule {
    pub path: String,
    pub members: HashSet<String>,
}

#[derive(Clone, Debug, Default)]
pub struct ImportedSymbol {
    pub has_callable: bool,
    pub callable_sigs: Vec<ImportedCallableSig>,
    pub has_type: bool,
    pub type_ty: Option<Type>,
    pub has_trait: bool,
    pub trait_sig: Option<ImportedTraitSig>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportedCallableSig {
    pub params: Vec<ImportedParamSig>,
    pub ret_ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportedParamSig {
    pub mode: ParamMode,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportedTraitMethodSig {
    pub name: String,
    pub params: Vec<ImportedParamSig>,
    pub ret_ty: Type,
    pub type_param_count: usize,
    pub type_param_bounds: Vec<Option<String>>,
    pub self_mode: ParamMode,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportedTraitPropertySig {
    pub name: String,
    pub ty: Type,
    pub has_get: bool,
    pub has_set: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportedTraitSig {
    pub methods: HashMap<String, ImportedTraitMethodSig>,
    pub properties: HashMap<String, ImportedTraitPropertySig>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ImportedFacts {
    pub callable_sigs_by_def: HashMap<DefId, Vec<ImportedCallableSig>>,
    pub type_defs_by_def: HashMap<DefId, Type>,
    pub trait_defs_by_def: HashMap<DefId, ImportedTraitSig>,
}

pub struct SymbolResolver {
    scopes: Vec<Scope>,
    errors: Vec<ResolveError>,
    def_id_gen: DefIdGen,
    def_table_builder: DefTableBuilder,
    func_decl_names: HashSet<String>,
    intrinsic_type_defs: HashSet<DefId>,
    callable_attrs: HashMap<NodeId, FuncAttrs>,
    variant_placeholders: HashMap<String, DefId>,
    require_aliases: HashSet<String>,
    imported_modules: HashMap<String, ImportedModule>,
    imported_symbols: HashMap<String, ImportedSymbol>,
    imported_callable_sigs: HashMap<DefId, Vec<ImportedCallableSig>>,
    imported_type_defs: HashMap<DefId, Type>,
    imported_trait_defs: HashMap<DefId, ImportedTraitSig>,
}

#[derive(Clone)]
pub struct ResolveOutput {
    pub context: ResolveStageOutput,
    pub imported_facts: ImportedFacts,
    pub errors: Vec<ResolveError>,
}

impl Default for SymbolResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolResolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope {
                defs: HashMap::new(),
            }],
            errors: Vec::new(),
            def_id_gen: DefIdGen::new(),
            def_table_builder: DefTableBuilder::new(),
            func_decl_names: HashSet::new(),
            intrinsic_type_defs: HashSet::new(),
            callable_attrs: HashMap::new(),
            variant_placeholders: HashMap::new(),
            require_aliases: HashSet::new(),
            imported_modules: HashMap::new(),
            imported_symbols: HashMap::new(),
            imported_callable_sigs: HashMap::new(),
            imported_type_defs: HashMap::new(),
            imported_trait_defs: HashMap::new(),
        }
    }

    fn is_enum_variant_name(&self, name: &str) -> bool {
        self.scopes.iter().rev().any(|scope| {
            scope.defs.values().any(|symbol| match &symbol.kind {
                SymbolKind::EnumDef { variants, .. } => {
                    variants.iter().any(|variant| variant.name == name)
                }
                _ => false,
            })
        })
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scope {
            defs: HashMap::new(),
        });
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn with_scope<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.enter_scope();
        f(self);
        self.exit_scope();
    }

    fn insert_symbol(&mut self, name: &str, symbol: Symbol, span: Span) {
        let scope = self.scopes.last_mut().unwrap();
        match scope.defs.get_mut(name) {
            None => {
                scope.defs.insert(name.to_string(), symbol);
            }
            Some(existing) => match (&mut existing.kind, symbol.kind) {
                (
                    SymbolKind::Func { overloads },
                    SymbolKind::Func {
                        overloads: new_overloads,
                    },
                ) => {
                    overloads.extend(new_overloads);
                }
                _ => {
                    self.errors
                        .push(ResolveError::SymbolAlreadyDefined(name.to_string(), span));
                }
            },
        }
    }

    fn register_param(&mut self, name: &str, mode: ParamMode, id: NodeId, span: Span, index: u32) {
        let is_mutable = matches!(mode, ParamMode::InOut | ParamMode::Out | ParamMode::Sink);
        let def_id = self.def_id_gen.new_id();
        let def = Def {
            id: def_id,
            name: name.to_string(),
            kind: DefKind::Param { index, is_mutable },
        };
        self.def_table_builder.record_def(def, id);
        self.insert_symbol(
            name,
            Symbol {
                name: name.to_string(),
                kind: SymbolKind::Var { def_id, is_mutable },
            },
            span,
        );
    }

    fn register_type_param(&mut self, param: &TypeParam) {
        let def_id = self.def_id_gen.new_id();
        let def = Def {
            id: def_id,
            name: param.ident.clone(),
            kind: DefKind::TypeParam,
        };
        self.def_table_builder.record_def(def, param.id);
        self.insert_symbol(
            &param.ident,
            Symbol {
                name: param.ident.clone(),
                kind: SymbolKind::TypeParam { def_id },
            },
            param.span,
        );

        if let Some(bound) = &param.bound {
            match self.lookup_symbol(&bound.name) {
                Some(symbol) => match &symbol.kind {
                    SymbolKind::TraitDef { .. } => {
                        self.def_table_builder.record_use(bound.id, symbol.def_id());
                    }
                    other => self.errors.push(ResolveError::ExpectedTrait(
                        bound.name.clone(),
                        other.clone(),
                        bound.span,
                    )),
                },
                None => self
                    .errors
                    .push(ResolveError::TraitUndefined(bound.name.clone(), bound.span)),
            }
        }
    }

    fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.defs.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    fn map_symbol_kind_to_def_kind(kind: &SymbolKind) -> DefKind {
        match kind {
            SymbolKind::TraitDef { .. } => DefKind::TraitDef {
                attrs: TraitAttrs::default(),
            },
            SymbolKind::TypeAlias { .. } => DefKind::TypeDef {
                attrs: TypeAttrs::default(),
            },
            SymbolKind::StructDef { .. } => DefKind::TypeDef {
                attrs: TypeAttrs::default(),
            },
            SymbolKind::TypeParam { .. } => DefKind::TypeParam,
            SymbolKind::Func { .. } => DefKind::FuncDef {
                attrs: FuncAttrs::default(),
            },
            SymbolKind::Var { is_mutable, .. } => DefKind::LocalVar {
                nrvo_eligible: false,
                is_mutable: *is_mutable,
            },
            SymbolKind::EnumDef { .. } => DefKind::TypeDef {
                attrs: TypeAttrs::default(),
            },
        }
    }

    fn resolve_type_attrs(&mut self, attrs: &[Attribute]) -> TypeAttrs {
        let mut resolved = TypeAttrs::default();
        let mut seen = HashSet::new();

        for attr in attrs {
            if !seen.insert(attr.name.clone()) {
                self.errors
                    .push(ResolveError::AttrDuplicate(attr.name.clone(), attr.span));
                continue;
            }
            match attr.name.as_str() {
                "public" => {
                    if !attr.args.is_empty() {
                        self.errors.push(ResolveError::AttrWrongArgCount(
                            attr.name.clone(),
                            0,
                            attr.args.len(),
                            attr.span,
                        ));
                    } else {
                        resolved.visibility = Visibility::Public;
                    }
                }
                "opaque" => {
                    if !attr.args.is_empty() {
                        self.errors.push(ResolveError::AttrWrongArgCount(
                            attr.name.clone(),
                            0,
                            attr.args.len(),
                            attr.span,
                        ));
                    } else {
                        resolved.visibility = Visibility::Public;
                        resolved.opaque = true;
                    }
                }
                "intrinsic" => {
                    if !attr.args.is_empty() {
                        self.errors.push(ResolveError::AttrWrongArgCount(
                            attr.name.clone(),
                            0,
                            attr.args.len(),
                            attr.span,
                        ));
                    } else {
                        resolved.intrinsic = true;
                    }
                }
                "link_name" => {
                    self.errors.push(ResolveError::AttrNotAllowed(
                        attr.name.clone(),
                        "type definition",
                        attr.span,
                    ));
                }
                _ => self
                    .errors
                    .push(ResolveError::UnknownAttribute(attr.name.clone(), attr.span)),
            }
        }

        resolved
    }

    fn resolve_trait_attrs(&mut self, attrs: &[Attribute]) -> TraitAttrs {
        let mut resolved = TraitAttrs::default();
        let mut seen = HashSet::new();

        for attr in attrs {
            if !seen.insert(attr.name.clone()) {
                self.errors
                    .push(ResolveError::AttrDuplicate(attr.name.clone(), attr.span));
                continue;
            }

            match attr.name.as_str() {
                "public" => {
                    if !attr.args.is_empty() {
                        self.errors.push(ResolveError::AttrWrongArgCount(
                            attr.name.clone(),
                            0,
                            attr.args.len(),
                            attr.span,
                        ));
                    } else {
                        resolved.visibility = Visibility::Public;
                    }
                }
                "intrinsic" | "runtime" | "link_name" | "opaque" => {
                    self.errors.push(ResolveError::AttrNotAllowed(
                        attr.name.clone(),
                        "trait definition",
                        attr.span,
                    ));
                }
                _ => self
                    .errors
                    .push(ResolveError::UnknownAttribute(attr.name.clone(), attr.span)),
            }
        }

        resolved
    }

    fn resolve_func_attrs(&mut self, attrs: &[Attribute]) -> FuncAttrs {
        let mut resolved = FuncAttrs::default();
        let mut seen = HashSet::new();

        for attr in attrs {
            if !seen.insert(attr.name.clone()) {
                self.errors
                    .push(ResolveError::AttrDuplicate(attr.name.clone(), attr.span));
                continue;
            }
            match attr.name.as_str() {
                "public" => {
                    if !attr.args.is_empty() {
                        self.errors.push(ResolveError::AttrWrongArgCount(
                            attr.name.clone(),
                            0,
                            attr.args.len(),
                            attr.span,
                        ));
                    } else {
                        resolved.visibility = Visibility::Public;
                    }
                }
                "intrinsic" => {
                    if !attr.args.is_empty() {
                        self.errors.push(ResolveError::AttrWrongArgCount(
                            attr.name.clone(),
                            0,
                            attr.args.len(),
                            attr.span,
                        ));
                    } else {
                        resolved.intrinsic = true;
                    }
                }
                "runtime" => {
                    if !attr.args.is_empty() {
                        self.errors.push(ResolveError::AttrWrongArgCount(
                            attr.name.clone(),
                            0,
                            attr.args.len(),
                            attr.span,
                        ));
                    } else {
                        resolved.runtime = true;
                    }
                }
                "link_name" => {
                    if attr.args.len() != 1 {
                        self.errors.push(ResolveError::AttrWrongArgCount(
                            attr.name.clone(),
                            1,
                            attr.args.len(),
                            attr.span,
                        ));
                        continue;
                    }
                    let Some(AttrArg::String(name)) = attr.args.first() else {
                        self.errors
                            .push(ResolveError::AttrWrongArgType(attr.name.clone(), attr.span));
                        continue;
                    };
                    resolved.link_name = Some(name.clone());
                }
                "__property_get" | "__property_set" => {
                    // Internal marker attributes emitted by the parser for properties.
                }
                "opaque" => {
                    self.errors.push(ResolveError::AttrNotAllowed(
                        attr.name.clone(),
                        "function",
                        attr.span,
                    ));
                }
                _ => self
                    .errors
                    .push(ResolveError::UnknownAttribute(attr.name.clone(), attr.span)),
            }
        }

        resolved
    }

    fn add_built_in_symbol<F>(&mut self, name: &str, intrinsic: bool, kind_fn: F) -> DefId
    where
        F: FnOnce(DefId) -> SymbolKind,
    {
        let def_id = self.def_id_gen.new_id();
        let kind = kind_fn(def_id);
        let mut def = Def {
            id: def_id,
            name: name.to_string(),
            kind: Self::map_symbol_kind_to_def_kind(&kind),
        };
        if intrinsic && let DefKind::TypeDef { attrs } = &mut def.kind {
            attrs.intrinsic = true;
            self.intrinsic_type_defs.insert(def_id);
        }
        self.def_table_builder.record_def(def, NodeId(0));
        self.insert_symbol(
            name,
            Symbol {
                name: name.to_string(),
                kind,
            },
            Span::default(),
        );
        def_id
    }

    fn check_requires(&mut self, module: &Module) {
        let mut seen = HashSet::new();
        for req in &module.requires {
            let alias = req
                .alias
                .clone()
                .or_else(|| req.path.last().cloned())
                .unwrap_or_default();
            if !seen.insert(alias.clone()) {
                self.errors
                    .push(ResolveError::DuplicateRequireAlias(alias, req.span));
            } else {
                self.require_aliases.insert(alias);
            }
        }
    }

    fn validate_module_alias_member(&mut self, alias: &str, member: &str, span: Span) -> bool {
        let Some(imported) = self.imported_modules.get(alias) else {
            return true;
        };
        if imported.members.contains(member) {
            true
        } else {
            self.errors.push(ResolveError::ModuleMemberUndefined(
                imported.path.clone(),
                member.to_string(),
                span,
            ));
            false
        }
    }

    fn populate_decls(&mut self, module: &Module) {
        // Populate trait definitions
        self.populate_trait_defs(&module.trait_defs());

        // Populate type definitions
        self.populate_type_defs(&module.type_defs());

        // Populate callable declarations
        self.populate_callables(&module.callables());

        // Populate imported symbol aliases so resolve can bind unqualified uses
        // (e.g. `requires { std::io::println }` then `println(...)`).
        self.populate_imported_symbol_aliases();
    }

    fn populate_imported_symbol_aliases(&mut self) {
        let aliases = self.imported_symbols.clone();
        for (alias, imported) in aliases {
            if imported.has_callable {
                let def_id = self.add_built_in_symbol(&alias, false, |def_id| SymbolKind::Func {
                    overloads: vec![def_id],
                });
                if !imported.callable_sigs.is_empty() {
                    self.imported_callable_sigs
                        .insert(def_id, imported.callable_sigs.clone());
                }
                continue;
            }

            if imported.has_type {
                let def_id =
                    self.add_built_in_symbol(&alias, false, |def_id| SymbolKind::TypeAlias {
                        def_id,
                        ty_expr: TypeExpr {
                            id: NodeId(0),
                            kind: TypeExprKind::Infer,
                            span: Span::default(),
                        },
                    });
                if let Some(type_ty) = imported.type_ty.clone() {
                    self.imported_type_defs.insert(def_id, type_ty);
                }
                continue;
            }

            if imported.has_trait {
                let def_id = self
                    .add_built_in_symbol(&alias, false, |def_id| SymbolKind::TraitDef { def_id });
                if let Some(trait_sig) = imported.trait_sig.clone() {
                    self.imported_trait_defs.insert(def_id, trait_sig);
                }
            }
        }
    }

    fn populate_trait_defs(&mut self, trait_defs: &[&TraitDef]) {
        for &trait_def in trait_defs {
            let def_id = self.def_id_gen.new_id();
            let trait_attrs = self.resolve_trait_attrs(&trait_def.attrs);
            let def = Def {
                id: def_id,
                name: trait_def.name.clone(),
                kind: DefKind::TraitDef { attrs: trait_attrs },
            };
            self.def_table_builder.record_def(def, trait_def.id);
            self.insert_symbol(
                &trait_def.name,
                Symbol {
                    name: trait_def.name.clone(),
                    kind: SymbolKind::TraitDef { def_id },
                },
                trait_def.span,
            );
        }
    }

    fn populate_type_defs(&mut self, type_defs: &[&TypeDef]) {
        for &type_def in type_defs {
            let def_id = self.def_id_gen.new_id();
            let type_attrs = self.resolve_type_attrs(&type_def.attrs);

            // Map type def kind to a (def kind, symbol kind) pair
            let (def_kind, symbol_kind) = match &type_def.kind {
                TypeDefKind::Alias { aliased_ty } => (
                    DefKind::TypeDef {
                        attrs: type_attrs.clone(),
                    },
                    SymbolKind::TypeAlias {
                        def_id,
                        ty_expr: aliased_ty.clone(),
                    },
                ),
                TypeDefKind::Struct { fields } => (
                    DefKind::TypeDef {
                        attrs: type_attrs.clone(),
                    },
                    SymbolKind::StructDef {
                        def_id,
                        fields: fields.clone(),
                    },
                ),
                TypeDefKind::Enum { variants } => (
                    DefKind::TypeDef {
                        attrs: type_attrs.clone(),
                    },
                    SymbolKind::EnumDef {
                        def_id,
                        variants: variants.clone(),
                    },
                ),
            };

            // Create a new Def
            let def = Def {
                id: def_id,
                name: type_def.name.clone(),
                kind: def_kind,
            };

            // Record the def
            self.def_table_builder.record_def(def, type_def.id);

            if type_attrs.intrinsic {
                self.intrinsic_type_defs.insert(def_id);
            }

            // Insert the symbol
            self.insert_symbol(
                &type_def.name,
                Symbol {
                    name: type_def.name.clone(),
                    kind: symbol_kind,
                },
                type_def.span,
            );
        }
    }

    fn populate_callables(&mut self, callables: &[CallableRef]) {
        for callable in callables {
            match callable {
                CallableRef::FuncDecl(func_decl) => {
                    let name = func_decl.sig.name.clone();
                    self.func_decl_names.insert(name);
                    self.populate_callable(callable);
                }
                CallableRef::FuncDef(func_def) => {
                    // Check if the function name is already defined as a function decl
                    if self.func_decl_names.contains(&func_def.sig.name) {
                        self.errors.push(ResolveError::SymbolAlreadyDefined(
                            func_def.sig.name.clone(),
                            func_def.span,
                        ));
                        continue;
                    }
                    self.populate_callable(callable);
                }
                CallableRef::MethodDecl { .. } => self.populate_callable(callable),
                CallableRef::MethodDef { .. } => self.populate_callable(callable),
                CallableRef::ClosureDef(_) => self.populate_callable(callable),
            }
        }
    }

    fn populate_callable(&mut self, callable: &CallableRef) {
        let def_id = self.def_id_gen.new_id();
        let func_attrs = match callable {
            CallableRef::FuncDecl(func_decl) => self.resolve_func_attrs(&func_decl.attrs),
            CallableRef::FuncDef(func_def) => self.resolve_func_attrs(&func_def.attrs),
            CallableRef::MethodDecl { method_decl, .. } => {
                self.resolve_func_attrs(&method_decl.attrs)
            }
            CallableRef::MethodDef { method_def, .. } => self.resolve_func_attrs(&method_def.attrs),
            CallableRef::ClosureDef(_) => FuncAttrs::default(),
        };
        self.callable_attrs
            .insert(callable.id(), func_attrs.clone());
        let def = Def {
            id: def_id,
            name: callable.name(),
            kind: match callable {
                CallableRef::FuncDecl(_) => DefKind::FuncDecl { attrs: func_attrs },
                CallableRef::MethodDecl { .. } => DefKind::FuncDecl { attrs: func_attrs },
                CallableRef::FuncDef(_)
                | CallableRef::MethodDef { .. }
                | CallableRef::ClosureDef(_) => DefKind::FuncDef { attrs: func_attrs },
            },
        };
        self.def_table_builder.record_def(def, callable.id());
        self.insert_symbol(
            &callable.name(),
            Symbol {
                name: callable.symbol_base_name(),
                kind: SymbolKind::Func {
                    overloads: vec![def_id],
                },
            },
            callable.span(),
        );
    }

    pub fn resolve_partial(
        &mut self,
        module: &Module,
    ) -> (DefTable, NodeDefLookup, Vec<ResolveError>) {
        self.with_scope(|resolver| {
            // global scope

            // add built-in types
            for ty in BUILTIN_TYPES {
                let ty_name = ty.to_string();
                let intrinsic = matches!(ty, Type::String);
                let _ = resolver.add_built_in_symbol(&ty_name, intrinsic, |def_id| {
                    SymbolKind::TypeAlias {
                        def_id,
                        ty_expr: TypeExpr {
                            id: NodeId(0),
                            kind: TypeExprKind::Named {
                                ident: ty_name.clone(),
                                def_id: (),
                                type_args: Vec::new(),
                            },
                            span: Span::default(),
                        },
                    }
                });
            }
            let _ = resolver.add_built_in_symbol("set", false, |def_id| SymbolKind::TypeAlias {
                def_id,
                ty_expr: TypeExpr {
                    id: NodeId(0),
                    kind: TypeExprKind::Named {
                        ident: "set".to_string(),
                        def_id: (),
                        type_args: Vec::new(),
                    },
                    span: Span::default(),
                },
            });
            let _ = resolver.add_built_in_symbol("map", false, |def_id| SymbolKind::TypeAlias {
                def_id,
                ty_expr: TypeExpr {
                    id: NodeId(0),
                    kind: TypeExprKind::Named {
                        ident: "map".to_string(),
                        def_id: (),
                        type_args: Vec::new(),
                    },
                    span: Span::default(),
                },
            });

            resolver.check_requires(module);
            resolver.populate_decls(module);

            resolver.visit_module(module);
        });

        let (def_table, node_def_lookup) = std::mem::take(&mut self.def_table_builder).finish();
        let errors = std::mem::take(&mut self.errors);
        (def_table, node_def_lookup, errors)
    }

    pub fn resolve(
        &mut self,
        module: &Module,
    ) -> Result<(DefTable, NodeDefLookup), Vec<ResolveError>> {
        let (def_table, node_def_lookup, errors) = self.resolve_partial(module);
        if errors.is_empty() {
            Ok((def_table, node_def_lookup))
        } else {
            Err(errors)
        }
    }

    fn check_lvalue_mutability(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Var { ident: name, .. } => {
                match self.lookup_symbol(name) {
                    Some(symbol) => match &symbol.kind {
                        SymbolKind::Var {
                            is_mutable: true, ..
                        } => {
                            // Mutable: ok
                            self.def_table_builder.record_use(expr.id, symbol.def_id());
                        }
                        SymbolKind::Var {
                            is_mutable: false, ..
                        } => {
                            // Immutable: error
                            self.def_table_builder.record_use(expr.id, symbol.def_id());
                            self.errors
                                .push(ResolveError::VarImmutable(name.clone(), expr.span));
                        }
                        _ => {
                            self.errors
                                .push(ResolveError::VarUndefined(name.clone(), expr.span));
                        }
                    },
                    None => {
                        self.errors
                            .push(ResolveError::VarUndefined(name.clone(), expr.span));
                    }
                }
            }
            ExprKind::ArrayIndex { target, indices } => {
                // Recursively check the target. If target is mutable, then target[index] is mutable.
                self.check_lvalue_mutability(target);
                for index in indices {
                    self.visit_expr(index);
                }
            }
            ExprKind::TupleField { target, .. } => {
                self.check_lvalue_mutability(target);
            }
            ExprKind::StructField { target, .. } => {
                self.check_lvalue_mutability(target);
            }
            _ => {
                self.errors.push(ResolveError::InvalidAssignmentTarget(
                    expr.kind.clone(),
                    expr.span,
                ));
            }
        }
    }

    fn check_bind_pattern(&mut self, pattern: &BindPattern, is_mutable: bool) {
        match &pattern.kind {
            BindPatternKind::Name {
                ident: var_name, ..
            } => {
                let def_id = self.def_id_gen.new_id();
                let def = Def {
                    id: def_id,
                    name: var_name.to_string(),
                    kind: DefKind::LocalVar {
                        nrvo_eligible: false,
                        is_mutable,
                    },
                };
                self.def_table_builder.record_def(def, pattern.id);
                self.insert_symbol(
                    var_name,
                    Symbol {
                        name: var_name.to_string(),
                        kind: SymbolKind::Var { def_id, is_mutable },
                    },
                    pattern.span,
                );
            }
            BindPatternKind::Array { patterns } => {
                // Recursively check each sub-pattern
                for pattern in patterns {
                    self.check_bind_pattern(pattern, is_mutable);
                }
            }
            BindPatternKind::Tuple { patterns } => {
                // Recursively check each sub-pattern
                for pattern in patterns {
                    self.check_bind_pattern(pattern, is_mutable);
                }
            }
            BindPatternKind::Struct {
                name: struct_name,
                fields,
            } => {
                // Resolve struct type name
                match self.lookup_symbol(struct_name) {
                    Some(Symbol {
                        kind: SymbolKind::StructDef { def_id, .. },
                        ..
                    }) => {
                        self.def_table_builder.record_use(pattern.id, *def_id);
                    }
                    Some(symbol) => {
                        self.errors.push(ResolveError::ExpectedType(
                            struct_name.clone(),
                            symbol.kind.clone(),
                            pattern.span,
                        ));
                    }
                    None => self.errors.push(ResolveError::StructUndefined(
                        struct_name.clone(),
                        pattern.span,
                    )),
                }

                // Bind each field's sub-pattern
                for field in fields {
                    self.check_bind_pattern(&field.pattern, is_mutable);
                }
            }
        }
    }

    fn check_match_pattern(&mut self, pattern: &MatchPattern) {
        match pattern {
            MatchPattern::Wildcard { .. } => {}
            MatchPattern::BoolLit { .. } => {}
            MatchPattern::IntLit { .. } => {}
            MatchPattern::Binding {
                id, ident, span, ..
            } => {
                self.bind_match_binding(*id, ident, *span);
            }
            MatchPattern::TypedBinding {
                id,
                ident,
                ty_expr,
                span,
                ..
            } => {
                self.visit_type_expr(ty_expr);
                self.bind_match_binding(*id, ident, *span);
            }
            MatchPattern::Tuple { patterns, .. } => {
                for pattern in patterns {
                    self.check_match_pattern(pattern);
                }
            }
            MatchPattern::EnumVariant {
                id,
                enum_name,
                type_args,
                bindings,
                span,
                ..
            } => {
                for arg in type_args {
                    self.visit_type_expr(arg);
                }
                // Resolve the enum name if present
                if let Some(enum_name) = enum_name {
                    let Some(Symbol {
                        kind: SymbolKind::EnumDef { def_id, .. },
                        ..
                    }) = self.lookup_symbol(enum_name)
                    else {
                        self.errors
                            .push(ResolveError::EnumUndefined(enum_name.clone(), *span));
                        return;
                    };
                    self.def_table_builder.record_use(*id, *def_id);
                }

                // Note: We delegate to the type checker to validate the variant.

                // Bind each binding's sub-pattern
                self.bind_match_bindings(bindings);
            }
        }
    }

    fn bind_match_bindings(&mut self, bindings: &[MatchPatternBinding]) {
        for binding in bindings {
            if let MatchPatternBinding::Named {
                id, ident, span, ..
            } = binding
            {
                self.bind_match_binding(*id, ident, *span);
            }
        }
    }

    fn bind_match_binding(&mut self, id: NodeId, name: &str, span: Span) {
        let def_id = self.def_id_gen.new_id();
        let def = Def {
            id: def_id,
            name: name.to_string(),
            kind: DefKind::LocalVar {
                nrvo_eligible: false,
                is_mutable: false,
            },
        };
        self.def_table_builder.record_def(def, id);
        self.insert_symbol(
            name,
            Symbol {
                name: name.to_string(),
                kind: SymbolKind::Var {
                    def_id,
                    is_mutable: false,
                },
            },
            span,
        );
    }

    fn check_method_block_type(&mut self, method_block: &MethodBlock) -> Option<DefId> {
        match self.lookup_symbol(&method_block.type_name) {
            Some(symbol) => match &symbol.kind {
                SymbolKind::TypeAlias { def_id, .. }
                | SymbolKind::StructDef { def_id, .. }
                | SymbolKind::EnumDef { def_id, .. } => Some(*def_id),
                other => {
                    self.errors.push(ResolveError::ExpectedType(
                        method_block.type_name.clone(),
                        other.clone(),
                        method_block.span,
                    ));
                    None
                }
            },
            None => {
                self.errors.push(ResolveError::TypeUndefined(
                    method_block.type_name.clone(),
                    method_block.span,
                ));
                None
            }
        }
    }

    fn check_method_block_trait(&mut self, method_block: &MethodBlock) {
        let Some(trait_name) = &method_block.trait_name else {
            return;
        };
        match self.lookup_symbol(trait_name) {
            Some(symbol) => match &symbol.kind {
                SymbolKind::TraitDef { .. } => {}
                other => self.errors.push(ResolveError::ExpectedTrait(
                    trait_name.clone(),
                    other.clone(),
                    method_block.span,
                )),
            },
            None => self.errors.push(ResolveError::TraitUndefined(
                trait_name.clone(),
                method_block.span,
            )),
        }
    }

    fn visit_method_decl_in_block(
        &mut self,
        method_block: &MethodBlock,
        method_decl: &MethodDecl,
        is_intrinsic_type: Option<bool>,
    ) {
        if matches!(is_intrinsic_type, Some(false)) {
            self.errors.push(ResolveError::MethodDeclOnNonIntrinsicType(
                method_block.type_name.clone(),
                method_decl.span,
            ));
        }

        let func_attrs = self
            .callable_attrs
            .get(&method_decl.id)
            .cloned()
            .unwrap_or_default();
        if !func_attrs.intrinsic && !func_attrs.runtime {
            self.errors.push(ResolveError::MethodDeclMissingIntrinsic(
                method_decl.sig.name.clone(),
                method_decl.span,
            ));
        }

        self.with_scope(|resolver| {
            for type_param in &method_decl.sig.type_params {
                resolver.register_type_param(type_param);
            }

            resolver.visit_type_expr(&method_decl.sig.ret_ty_expr);
            for param in &method_decl.sig.params {
                resolver.visit_type_expr(&param.typ);
            }

            resolver.register_param(
                "self",
                method_decl.sig.self_param.mode.clone(),
                method_decl.sig.self_param.id,
                method_decl.sig.self_param.span,
                0,
            );

            for (index, param) in method_decl.sig.params.iter().enumerate() {
                resolver.register_param(
                    &param.ident,
                    param.mode.clone(),
                    param.id,
                    param.span,
                    index as u32 + 1,
                );
            }
        });
    }
}

impl Visitor<()> for SymbolResolver {
    fn visit_type_def(&mut self, type_def: &TypeDef) {
        self.with_scope(|resolver| {
            for type_param in &type_def.type_params {
                resolver.register_type_param(type_param);
            }
            walk_type_def(resolver, type_def);
        });
    }

    fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
        match &type_expr.kind {
            TypeExprKind::Infer => {}
            TypeExprKind::Named {
                ident: name,
                type_args,
                ..
            } => {
                match self.lookup_symbol(name) {
                    Some(symbol) => match &symbol.kind {
                        SymbolKind::TypeAlias { .. }
                        | SymbolKind::StructDef { .. }
                        | SymbolKind::EnumDef { .. }
                        | SymbolKind::TypeParam { .. } => {
                            self.def_table_builder
                                .record_use(type_expr.id, symbol.def_id());
                        }
                        other => self.errors.push(ResolveError::ExpectedType(
                            name.clone(),
                            other.clone(),
                            type_expr.span,
                        )),
                    },
                    None => self
                        .errors
                        .push(ResolveError::TypeUndefined(name.clone(), type_expr.span)),
                }
                for arg in type_args {
                    self.visit_type_expr(arg);
                }
            }
            _ => walk_type_expr(self, type_expr),
        }
    }

    fn visit_func_sig(&mut self, func_sig: &FunctionSig) {
        self.visit_type_expr(&func_sig.ret_ty_expr);
        walk_func_sig(self, func_sig);
    }

    fn visit_func_decl(&mut self, func_decl: &FuncDecl) {
        // Ensure type params resolve in the signature and params have DefIds
        // even for declarations (no body/scope use).
        self.with_scope(|resolver| {
            for type_param in &func_decl.sig.type_params {
                resolver.register_type_param(type_param);
            }

            resolver.visit_type_expr(&func_decl.sig.ret_ty_expr);
            for param in &func_decl.sig.params {
                resolver.visit_type_expr(&param.typ);
            }

            for (index, param) in func_decl.sig.params.iter().enumerate() {
                resolver.register_param(
                    &param.ident,
                    param.mode.clone(),
                    param.id,
                    param.span,
                    index as u32,
                );
            }
        });
    }

    fn visit_func_def(&mut self, func_def: &FuncDef) {
        self.with_scope(|resolver| {
            for type_param in &func_def.sig.type_params {
                resolver.register_type_param(type_param);
            }

            resolver.visit_type_expr(&func_def.sig.ret_ty_expr);
            for param in &func_def.sig.params {
                resolver.visit_type_expr(&param.typ);
            }

            for (index, param) in func_def.sig.params.iter().enumerate() {
                resolver.register_param(
                    &param.ident,
                    param.mode.clone(),
                    param.id,
                    param.span,
                    index as u32,
                );
            }

            resolver.visit_expr(&func_def.body);
        });
    }

    fn visit_method_block(&mut self, method_block: &MethodBlock) {
        let type_def_id = self.check_method_block_type(method_block);
        let is_intrinsic_type =
            type_def_id.map(|def_id| self.intrinsic_type_defs.contains(&def_id));
        self.check_method_block_trait(method_block);

        for method_item in &method_block.method_items {
            match method_item {
                MethodItem::Decl(method_decl) => {
                    self.visit_method_decl_in_block(method_block, method_decl, is_intrinsic_type);
                }
                MethodItem::Def(method_def) => self.visit_method_def(method_def),
            }
        }
    }

    fn visit_trait_def(&mut self, trait_def: &TraitDef) {
        for method in &trait_def.methods {
            self.with_scope(|resolver| {
                for type_param in &method.sig.type_params {
                    resolver.register_type_param(type_param);
                }

                resolver.visit_type_expr(&method.sig.ret_ty_expr);
                for param in &method.sig.params {
                    resolver.visit_type_expr(&param.typ);
                }

                resolver.register_param(
                    "self",
                    method.sig.self_param.mode.clone(),
                    method.sig.self_param.id,
                    method.sig.self_param.span,
                    0,
                );

                for (index, param) in method.sig.params.iter().enumerate() {
                    resolver.register_param(
                        &param.ident,
                        param.mode.clone(),
                        param.id,
                        param.span,
                        index as u32 + 1,
                    );
                }
            });
        }

        for property in &trait_def.properties {
            self.visit_type_expr(&property.ty);
        }
    }

    fn visit_method_def(&mut self, method_def: &MethodDef) {
        // Enter a new scope for the method parameters and body.
        self.with_scope(|resolver| {
            for type_param in &method_def.sig.type_params {
                resolver.register_type_param(type_param);
            }

            resolver.visit_type_expr(&method_def.sig.ret_ty_expr);
            for param in &method_def.sig.params {
                resolver.visit_type_expr(&param.typ);
            }

            resolver.register_param(
                "self",
                method_def.sig.self_param.mode.clone(),
                method_def.sig.self_param.id,
                method_def.sig.self_param.span,
                0,
            );

            // Record defs for the method parameters.
            for (index, param) in method_def.sig.params.iter().enumerate() {
                resolver.register_param(
                    &param.ident,
                    param.mode.clone(),
                    param.id,
                    param.span,
                    index as u32 + 1,
                );
            }

            // Visit the method body.
            resolver.visit_expr(&method_def.body);
        });
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExpr) {
        match &stmt.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => {
                // Check the value first before introducing the lhs symbol(s) into the scope.
                self.visit_expr(value);
                if let Some(decl_ty) = decl_ty {
                    self.visit_type_expr(decl_ty);
                }
                self.check_bind_pattern(pattern, false);
            }

            StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                // Check the value first before introducing the lhs symbol(s) into the scope.
                self.visit_expr(value);
                if let Some(decl_ty) = decl_ty {
                    self.visit_type_expr(decl_ty);
                }
                self.check_bind_pattern(pattern, true);
            }

            StmtExprKind::VarDecl {
                ident,
                decl_ty,
                def_id: _,
            } => {
                let def_id = self.def_id_gen.new_id();
                let def = Def {
                    id: def_id,
                    name: ident.clone(),
                    kind: DefKind::LocalVar {
                        is_mutable: true,
                        nrvo_eligible: false,
                    },
                };
                self.def_table_builder.record_def(def, stmt.id);
                self.insert_symbol(
                    ident,
                    Symbol {
                        name: ident.clone(),
                        kind: SymbolKind::Var {
                            def_id,
                            is_mutable: true,
                        },
                    },
                    stmt.span,
                );
                self.visit_type_expr(decl_ty);
            }

            StmtExprKind::Assign {
                assignee, value, ..
            } => {
                self.check_lvalue_mutability(assignee);
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
                // Resolve iter first (pattern not in scope for it)
                self.visit_expr(iter);
                // Enter a new scope for the pattern + body
                self.with_scope(|resolver| {
                    resolver.check_bind_pattern(pattern, false);
                    resolver.visit_expr(body);
                });
            }
            StmtExprKind::Break | StmtExprKind::Continue => {}
            StmtExprKind::Return { value } => {
                if let Some(value) = value {
                    self.visit_expr(value);
                }
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Block { items, tail } => {
                self.with_scope(|resolver| {
                    for item in items {
                        resolver.visit_block_item(item);
                    }
                    if let Some(tail) = tail {
                        resolver.visit_expr(tail);
                    }
                });
            }

            ExprKind::IntLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit { .. }
            | ExprKind::UnitLit => {}

            ExprKind::Range { start, end } => {
                self.visit_expr(start);
                self.visit_expr(end);
            }

            ExprKind::ArrayLit { elem_ty, .. } => {
                if let Some(elem_ty) = elem_ty {
                    self.visit_type_expr(elem_ty);
                }
                walk_expr(self, expr);
            }

            ExprKind::StructLit { name, .. } => {
                match self.lookup_symbol(name) {
                    Some(Symbol {
                        kind: SymbolKind::StructDef { def_id, .. },
                        ..
                    }) => {
                        self.def_table_builder.record_use(expr.id, *def_id);
                    }
                    _ => self
                        .errors
                        .push(ResolveError::StructUndefined(name.clone(), expr.span)),
                }
                walk_expr(self, expr);
            }

            ExprKind::Var { ident: name, .. } => match self.lookup_symbol(name) {
                Some(symbol) => self.def_table_builder.record_use(expr.id, symbol.def_id()),
                None => {
                    if let Some((alias, member)) = name.split_once("::")
                        && self.require_aliases.contains(alias)
                    {
                        if self.validate_module_alias_member(alias, member, expr.span) {
                            self.errors
                                .push(ResolveError::ModuleQualifiedAccessUnsupported(
                                    alias.to_string(),
                                    member.to_string(),
                                    expr.span,
                                ));
                        }
                    } else if self.is_enum_variant_name(name) {
                        if let Some(def_id) = self.variant_placeholders.get(name).copied() {
                            self.def_table_builder.record_use(expr.id, def_id);
                        } else {
                            let def_id = self.def_id_gen.new_id();
                            let def = Def {
                                id: def_id,
                                name: name.to_string(),
                                kind: DefKind::EnumVariantName,
                            };
                            self.def_table_builder.record_def(def, expr.id);
                            self.variant_placeholders.insert(name.to_string(), def_id);
                        }
                    } else {
                        self.errors
                            .push(ResolveError::VarUndefined(name.to_string(), expr.span));
                    }
                }
            },

            ExprKind::EnumVariant {
                enum_name, variant, ..
            } => {
                // Resolve the enum name
                let Some(Symbol {
                    kind: SymbolKind::EnumDef { def_id, variants },
                    ..
                }) = self.lookup_symbol(enum_name)
                else {
                    self.errors
                        .push(ResolveError::EnumUndefined(enum_name.clone(), expr.span));
                    return;
                };

                // Ensure the variant is valid
                if !variants.iter().any(|v| v.name == *variant) {
                    self.errors.push(ResolveError::EnumVariantUndefined(
                        enum_name.clone(),
                        variant.clone(),
                        expr.span,
                    ));
                    return;
                }

                self.def_table_builder.record_use(expr.id, *def_id);
                walk_expr(self, expr);
            }

            // Resolve each payload expression
            ExprKind::Match { scrutinee, arms } => {
                self.visit_expr(scrutinee);
                for arm in arms {
                    // enter a new scope for the arm body
                    self.with_scope(|resolver| {
                        resolver.check_match_pattern(&arm.pattern);
                        resolver.visit_expr(&arm.body);
                    });
                }
            }

            ExprKind::Call { callee, args } => {
                self.visit_expr(callee);
                for arg in args {
                    self.visit_expr(&arg.expr);
                }
            }

            ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => {
                if let ExprKind::Var { ident: alias, .. } = &callee.kind
                    && self.require_aliases.contains(alias)
                {
                    if self.validate_module_alias_member(alias, method_name, expr.span) {
                        self.errors
                            .push(ResolveError::ModuleQualifiedAccessUnsupported(
                                alias.clone(),
                                method_name.clone(),
                                expr.span,
                            ));
                    }
                    for arg in args {
                        self.visit_expr(&arg.expr);
                    }
                    return;
                }

                self.visit_expr(callee);
                for arg in args {
                    self.visit_expr(&arg.expr);
                }
            }

            ExprKind::StructField { target, field } => {
                if let ExprKind::Var { ident: alias, .. } = &target.kind
                    && self.require_aliases.contains(alias)
                {
                    if self.validate_module_alias_member(alias, field, expr.span) {
                        self.errors
                            .push(ResolveError::ModuleQualifiedAccessUnsupported(
                                alias.clone(),
                                field.clone(),
                                expr.span,
                            ));
                    }
                    return;
                }
                walk_expr(self, expr);
            }

            ExprKind::Closure {
                ident,
                captures,
                params,
                return_ty,
                body,
                ..
            } => {
                // Closures are resolved at their expression site so they can
                // refer to the surrounding lexical scope.
                let def_id = self.def_id_gen.new_id();
                let def = Def {
                    id: def_id,
                    name: ident.clone(),
                    kind: DefKind::FuncDef {
                        attrs: FuncAttrs::default(),
                    },
                };
                self.def_table_builder.record_def(def, expr.id);

                if !captures.is_empty() {
                    for capture in captures {
                        let (cap_id, cap_name, cap_span) = match capture {
                            CaptureSpec::Move {
                                id,
                                ident: name,
                                span,
                                ..
                            } => (id, name, span),
                        };
                        match self.lookup_symbol(cap_name) {
                            Some(symbol) => {
                                self.def_table_builder.record_use(*cap_id, symbol.def_id());
                            }
                            None => self
                                .errors
                                .push(ResolveError::VarUndefined(cap_name.to_string(), *cap_span)),
                        }
                    }
                }

                for param in params {
                    self.visit_type_expr(&param.typ);
                }
                self.visit_type_expr(return_ty);

                // Enter a new scope for the closure parameters and body.
                self.with_scope(|resolver| {
                    for (index, param) in params.iter().enumerate() {
                        resolver.register_param(
                            &param.ident,
                            param.mode.clone(),
                            param.id,
                            param.span,
                            index as u32,
                        );
                    }
                    resolver.visit_expr(body);
                });
            }

            _ => walk_expr(self, expr),
        }
    }
}

pub fn resolve(ast_context: ResolveStageInput) -> Result<ResolveStageOutput, Vec<ResolveError>> {
    resolve_with_imports_and_symbols(ast_context, HashMap::new(), HashMap::new())
}

pub fn resolve_partial(ast_context: ResolveStageInput) -> ResolveOutput {
    resolve_with_imports_and_symbols_partial(ast_context, HashMap::new(), HashMap::new())
}

pub fn resolve_with_imports(
    ast_context: ResolveStageInput,
    imported_modules: HashMap<String, ImportedModule>,
) -> Result<ResolveStageOutput, Vec<ResolveError>> {
    resolve_with_imports_and_symbols(ast_context, imported_modules, HashMap::new())
}

pub fn resolve_with_imports_and_symbols(
    ast_context: ResolveStageInput,
    imported_modules: HashMap<String, ImportedModule>,
    imported_symbols: HashMap<String, ImportedSymbol>,
) -> Result<ResolveStageOutput, Vec<ResolveError>> {
    let output =
        resolve_with_imports_and_symbols_partial(ast_context, imported_modules, imported_symbols);
    if output.errors.is_empty() {
        Ok(output.context)
    } else {
        Err(output.errors)
    }
}

pub fn resolve_with_imports_partial(
    ast_context: ResolveStageInput,
    imported_modules: HashMap<String, ImportedModule>,
) -> ResolveOutput {
    resolve_with_imports_and_symbols_partial(ast_context, imported_modules, HashMap::new())
}

pub fn resolve_with_imports_and_symbols_partial(
    ast_context: ResolveStageInput,
    imported_modules: HashMap<String, ImportedModule>,
    imported_symbols: HashMap<String, ImportedSymbol>,
) -> ResolveOutput {
    let mut resolver = SymbolResolver::new();
    resolver.imported_modules = imported_modules;
    resolver.imported_symbols = imported_symbols;
    let (def_table, node_def_lookup, errors) = resolver.resolve_partial(&ast_context.module);

    // Build resolved tree from parsed tree + NodeDefLookup
    let resolved_module = build_module(&node_def_lookup, &ast_context.module);
    let imported_facts = ImportedFacts {
        callable_sigs_by_def: std::mem::take(&mut resolver.imported_callable_sigs),
        type_defs_by_def: std::mem::take(&mut resolver.imported_type_defs),
        trait_defs_by_def: std::mem::take(&mut resolver.imported_trait_defs),
    };

    ResolveOutput {
        context: ast_context.with_def_table(def_table, resolved_module),
        imported_facts,
        errors,
    }
}

pub fn resolve_program(
    program: CapsuleResolveStageInput,
) -> Result<CapsuleResolveStageOutput, Vec<ResolveError>> {
    let mut modules = HashMap::new();
    let mut errors = Vec::new();
    let mut top_level_owners = HashMap::new();
    let mut export_facts_by_module = HashMap::<ModuleId, ModuleExportFacts>::new();
    let mut import_env_by_module = HashMap::<ModuleId, ImportEnv>::new();

    for module_id in program.dependency_order_from_entry() {
        let Some(parsed_module) = program.module(module_id) else {
            continue;
        };
        for item in &parsed_module.module.top_level_items {
            top_level_owners.insert(top_level_item_id(item), module_id);
        }
        let mut imported_modules = HashMap::new();
        for req in &parsed_module.requires {
            if req.kind != RequireKind::Module {
                continue;
            }
            let alias = req.alias.clone();
            if let Some(dep_id) = program.capsule.by_path.get(&req.module_path)
                && let Some(dep_module) = program.module(*dep_id)
            {
                imported_modules.insert(
                    alias,
                    ImportedModule {
                        path: req.module_path.to_string(),
                        members: module_exported_members(&dep_module.module),
                    },
                );
            }
        }
        let mut imported_symbols = HashMap::<String, ImportedSymbol>::new();
        for req in &parsed_module.requires {
            if req.kind != RequireKind::Symbol {
                continue;
            }
            let Some(dep_id) = program.capsule.by_path.get(&req.module_path).copied() else {
                continue;
            };
            let Some(dep_exports) = export_facts_by_module.get(&dep_id) else {
                continue;
            };
            let Some(member) = &req.member else {
                continue;
            };
            let imported = ImportedSymbol {
                has_callable: dep_exports
                    .callables
                    .get(member)
                    .is_some_and(|overloads| !overloads.is_empty()),
                callable_sigs: Vec::new(),
                has_type: dep_exports.types.contains_key(member),
                type_ty: None,
                has_trait: dep_exports.traits.contains_key(member),
                trait_sig: None,
            };
            if imported.has_callable || imported.has_type || imported.has_trait {
                imported_symbols.insert(req.alias.clone(), imported);
            }
        }

        let parsed_context = ParsedContext::new(
            parsed_module.module.clone(),
            program.next_node_id_gen().clone(),
        );
        match resolve_with_imports_and_symbols(parsed_context, imported_modules, imported_symbols) {
            Ok(resolved_context) => {
                let module_exports = collect_module_export_facts(
                    &resolved_context,
                    module_id,
                    Some(parsed_module.source.path.clone()),
                );
                export_facts_by_module.insert(module_id, module_exports.clone());
                modules.insert(module_id, resolved_context);

                let mut import_env = ImportEnv::default();
                for req in &parsed_module.requires {
                    let Some(dep_id) = program.capsule.by_path.get(&req.module_path).copied()
                    else {
                        continue;
                    };
                    let Some(dep_exports) = export_facts_by_module.get(&dep_id).cloned() else {
                        continue;
                    };
                    match req.kind {
                        RequireKind::Module => {
                            import_env.module_aliases.insert(
                                req.alias.clone(),
                                ModuleImportBinding {
                                    module_id: dep_id,
                                    module_path: req.module_path.clone(),
                                    exports: dep_exports,
                                },
                            );
                        }
                        RequireKind::Symbol => {
                            let Some(member) = &req.member else {
                                continue;
                            };
                            let binding = collect_imported_symbol_binding_from_exports(
                                dep_id,
                                &req.module_path,
                                &dep_exports,
                                member,
                            );
                            if !binding.is_empty() {
                                import_env.symbol_aliases.insert(req.alias.clone(), binding);
                            }
                        }
                    }
                }
                if !import_env.module_aliases.is_empty() || !import_env.symbol_aliases.is_empty() {
                    import_env_by_module.insert(module_id, import_env);
                }
            }
            Err(mut module_errors) => {
                errors.append(&mut module_errors);
            }
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(CapsuleResolveStageOutput {
        entry: program.entry(),
        modules,
        by_path: program.capsule.by_path.clone(),
        edges: program.capsule.edges.clone(),
        top_level_owners,
        export_facts_by_module,
        import_env_by_module,
    })
}

fn collect_module_export_facts(
    resolved_context: &ResolvedContext,
    module_id: ModuleId,
    module_path: Option<crate::core::capsule::ModulePath>,
) -> ModuleExportFacts {
    let mut facts = ModuleExportFacts {
        module_id,
        module_path,
        callables: HashMap::new(),
        types: HashMap::new(),
        traits: HashMap::new(),
    };
    for def in resolved_context.def_table.clone() {
        if !def.is_public() {
            continue;
        }
        match def.kind {
            DefKind::FuncDef { .. } | DefKind::FuncDecl { .. } => {
                facts
                    .callables
                    .entry(def.name.clone())
                    .or_default()
                    .push(GlobalDefId::new(module_id, def.id));
            }
            DefKind::TypeDef { .. } => {
                facts
                    .types
                    .entry(def.name.clone())
                    .or_insert_with(|| GlobalDefId::new(module_id, def.id));
            }
            DefKind::TraitDef { .. } => {
                facts
                    .traits
                    .entry(def.name.clone())
                    .or_insert_with(|| GlobalDefId::new(module_id, def.id));
            }
            _ => {}
        }
    }
    for overloads in facts.callables.values_mut() {
        overloads.sort_by_key(|id| id.def_id);
        overloads.dedup();
    }
    facts
}

fn collect_imported_symbol_binding_from_exports(
    dep_module_id: ModuleId,
    dep_module_path: &crate::core::capsule::ModulePath,
    dep_exports: &ModuleExportFacts,
    member: &str,
) -> ImportedSymbolBinding {
    ImportedSymbolBinding {
        module_id: dep_module_id,
        module_path: dep_module_path.clone(),
        callables: dep_exports
            .callables
            .get(member)
            .cloned()
            .unwrap_or_default(),
        type_def: dep_exports.types.get(member).copied(),
        trait_def: dep_exports.traits.get(member).copied(),
    }
}

fn top_level_item_id(item: &TopLevelItem) -> crate::core::tree::NodeId {
    match item {
        TopLevelItem::TraitDef(trait_def) => trait_def.id,
        TopLevelItem::TypeDef(type_def) => type_def.id,
        TopLevelItem::FuncDecl(func_decl) => func_decl.id,
        TopLevelItem::FuncDef(func_def) => func_def.id,
        TopLevelItem::MethodBlock(method_block) => method_block.id,
        TopLevelItem::ClosureDef(closure_def) => closure_def.id,
    }
}

fn module_exported_members(module: &Module) -> HashSet<String> {
    let mut members = HashSet::new();
    for item in &module.top_level_items {
        match item {
            TopLevelItem::TraitDef(trait_def) => {
                members.insert(trait_def.name.clone());
            }
            TopLevelItem::TypeDef(type_def) => {
                members.insert(type_def.name.clone());
            }
            TopLevelItem::FuncDecl(func_decl) => {
                members.insert(func_decl.sig.name.clone());
            }
            TopLevelItem::FuncDef(func_def) => {
                members.insert(func_def.sig.name.clone());
            }
            TopLevelItem::MethodBlock(_) | TopLevelItem::ClosureDef(_) => {}
        }
    }
    members
}
