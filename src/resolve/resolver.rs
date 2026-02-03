use std::collections::{HashMap, HashSet};

use crate::context::{ParsedContext, ResolvedContext};
use crate::diag::Span;
use crate::resolve::def_table::{DefTable, DefTableBuilder, NodeDefLookup};
use crate::resolve::errors::ResolveError;
use crate::resolve::symbols::{Scope, Symbol, SymbolKind};
use crate::resolve::{Def, DefId, DefIdGen, DefKind, FuncAttrs, TypeAttrs};
use crate::tree::ParamMode;
use crate::tree::parsed::*;
use crate::tree::resolved::builder::build_module;
use crate::tree::visit::*;
use crate::types::{BUILTIN_TYPES, Type};

pub struct SymbolResolver {
    scopes: Vec<Scope>,
    errors: Vec<ResolveError>,
    def_id_gen: DefIdGen,
    def_table_builder: DefTableBuilder,
    func_decl_names: HashSet<String>,
    intrinsic_type_defs: HashSet<DefId>,
    callable_attrs: HashMap<NodeId, FuncAttrs>,
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
        }
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
            SymbolKind::TypeAlias { .. } => DefKind::TypeDef {
                attrs: TypeAttrs::default(),
            },
            SymbolKind::StructDef { .. } => DefKind::TypeDef {
                attrs: TypeAttrs::default(),
            },
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
                _ => self
                    .errors
                    .push(ResolveError::UnknownAttribute(attr.name.clone(), attr.span)),
            }
        }

        resolved
    }

    fn add_built_in_symbol<F>(&mut self, name: &str, intrinsic: bool, kind_fn: F)
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
    }

    fn populate_decls(&mut self, module: &Module) {
        // Populate type definitions
        self.populate_type_defs(&module.type_defs());

        // Populate callable declarations
        self.populate_callables(&module.callables());
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

    pub fn resolve(
        &mut self,
        module: &Module,
    ) -> Result<(DefTable, NodeDefLookup), Vec<ResolveError>> {
        self.with_scope(|resolver| {
            // global scope

            // add built-in types
            for ty in BUILTIN_TYPES {
                let ty_name = ty.to_string();
                let intrinsic = matches!(ty, Type::String);
                resolver.add_built_in_symbol(&ty_name, intrinsic, |def_id| SymbolKind::TypeAlias {
                    def_id,
                    ty_expr: TypeExpr {
                        id: NodeId(0),
                        kind: TypeExprKind::Named {
                            ident: ty_name.clone(),
                            def_id: (),
                        },
                        span: Span::default(),
                    },
                });
            }

            resolver.populate_decls(module);

            resolver.visit_module(module);
        });

        if self.errors.is_empty() {
            let (def_table, node_def_lookup) = std::mem::take(&mut self.def_table_builder).finish();
            Ok((def_table, node_def_lookup))
        } else {
            Err(self.errors.clone())
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

    fn check_match_pattern(&mut self, pattern: &MatchPattern, arm_id: NodeId) {
        match pattern {
            MatchPattern::Wildcard { .. } => {}
            MatchPattern::BoolLit { .. } => {}
            MatchPattern::IntLit { .. } => {}
            MatchPattern::Binding {
                id, ident, span, ..
            } => {
                self.bind_match_binding(*id, ident, *span);
            }
            MatchPattern::Tuple { patterns, .. } => {
                for pattern in patterns {
                    self.check_match_pattern(pattern, arm_id);
                }
            }
            MatchPattern::EnumVariant {
                enum_name,
                bindings,
                span,
                ..
            } => {
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
                    self.def_table_builder.record_use(arm_id, *def_id);
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

        self.visit_type_expr(&method_decl.sig.ret_ty_expr);
        for param in &method_decl.sig.params {
            self.visit_type_expr(&param.typ);
        }

        self.with_scope(|resolver| {
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
    fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
        match &type_expr.kind {
            TypeExprKind::Named { ident: name, .. } => match self.lookup_symbol(name) {
                Some(symbol) => match &symbol.kind {
                    SymbolKind::TypeAlias { .. }
                    | SymbolKind::StructDef { .. }
                    | SymbolKind::EnumDef { .. } => {
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
            },
            _ => walk_type_expr(self, type_expr),
        }
    }

    fn visit_func_sig(&mut self, func_sig: &FunctionSig) {
        self.visit_type_expr(&func_sig.ret_ty_expr);
        walk_func_sig(self, func_sig);
    }

    fn visit_func_decl(&mut self, func_decl: &FuncDecl) {
        self.visit_type_expr(&func_decl.sig.ret_ty_expr);
        for param in &func_decl.sig.params {
            self.visit_type_expr(&param.typ);
        }

        // Ensure params have DefIds even for declarations (no body/scope use).
        self.with_scope(|resolver| {
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
        self.visit_func_sig(&func_def.sig);

        self.with_scope(|resolver| {
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

        for method_item in &method_block.method_items {
            match method_item {
                MethodItem::Decl(method_decl) => {
                    self.visit_method_decl_in_block(method_block, method_decl, is_intrinsic_type);
                }
                MethodItem::Def(method_def) => self.visit_method_def(method_def),
            }
        }
    }

    fn visit_method_def(&mut self, method_def: &MethodDef) {
        self.visit_type_expr(&method_def.sig.ret_ty_expr);
        for param in &method_def.sig.params {
            self.visit_type_expr(&param.typ);
        }

        // Enter a new scope for the method parameters and body.
        self.with_scope(|resolver| {
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
                None => self
                    .errors
                    .push(ResolveError::VarUndefined(name.to_string(), expr.span)),
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
                        resolver.check_match_pattern(&arm.pattern, arm.id);
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

            ExprKind::MethodCall { callee, args, .. } => {
                self.visit_expr(callee);
                for arg in args {
                    self.visit_expr(&arg.expr);
                }
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

pub fn resolve(ast_context: ParsedContext) -> Result<ResolvedContext, Vec<ResolveError>> {
    let mut resolver = SymbolResolver::new();
    let (def_table, node_def_lookup) = resolver.resolve(&ast_context.module)?;

    // Build resolved tree from parsed tree + NodeDefLookup
    let resolved_module = build_module(&node_def_lookup, &ast_context.module);

    let resolved_context = ast_context.with_def_table(def_table, resolved_module);

    Ok(resolved_context)
}
