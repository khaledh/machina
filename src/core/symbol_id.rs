//! Canonical semantic symbol identity.
//!
//! This module defines the long-term, human-readable identity model for source
//! definitions. Unlike local `DefId`s or transitional `GlobalDefId`s,
//! `SymbolId` is intended to answer "what definition is this?" across module
//! boundaries and overloaded call sites.
//!
//! The compiler still uses local numeric ids internally for compact table
//! indexing. `SymbolId` is the semantic layer that tooling and cross-module
//! facts can converge on over time.

use std::collections::HashMap;
use std::fmt;

use serde::{Deserialize, Serialize};

use crate::core::ast::ParamMode;
use crate::core::ast::{
    FunctionSig, MethodItem, MethodSig, Module, RefinementKind, TopLevelItem, TypeExpr,
    TypeExprKind, TypeParam,
};
use crate::core::capsule::ModulePath;
use crate::core::resolve::{DefId, DefTable};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SymbolId {
    /// Owning source module, e.g. `std::io`.
    pub module: ModulePath,
    /// Nested definition path within the module, e.g. `ReadFile::text`.
    pub path: SymbolPath,
    /// Namespace that distinguishes values/types/methods/states/etc.
    pub ns: SymbolNs,
    /// Extra identity needed when a path is overloaded.
    pub disambiguator: Option<SymbolDisambiguator>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct SymbolPath {
    /// Source-level path segments below the module path.
    pub segments: Vec<SymbolSegment>,
}

impl SymbolPath {
    pub fn new(segments: Vec<SymbolSegment>) -> Self {
        Self { segments }
    }

    pub fn from_names<I, S>(segments: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        Self {
            segments: segments
                .into_iter()
                .map(|name| SymbolSegment { name: name.into() })
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SymbolSegment {
    /// User-facing segment text as written in source.
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SymbolNs {
    /// Functions, locals, globals, and other value-level defs.
    Value,
    /// Nominal type definitions.
    Type,
    /// Trait definitions.
    Trait,
    /// Enum variant names.
    Variant,
    /// Named struct/record fields.
    Field,
    /// Methods declared inside method blocks or traits.
    Method,
    /// Named properties (getter/setter surface).
    Property,
    /// Linear type states.
    State,
    /// Generated or source-level machine/event handlers.
    Handler,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SymbolDisambiguator {
    /// Callable overload identity derived from the declared signature shape.
    Callable(CallableSigKey),
}

/// Transitional selected-call identity.
///
/// The long-term goal is for call sites to carry a canonical `SymbolId`, but
/// while callable disambiguators are still being threaded through the frontend
/// we keep the migration state explicit instead of relying on loosely-related
/// optional ids.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SelectedCallable {
    /// A same-module callable selected by the type checker.
    Local(DefId),
    /// Final canonical callable identity once the frontend can provide it.
    Canonical(SymbolId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct CallableSigKey {
    /// Declared generic parameter count, normalized away from source names.
    pub type_param_count: u16,
    /// Receiver mode for methods, if any.
    pub self_mode: Option<CallableSelfKey>,
    /// Ordered callable parameters after generic-name normalization.
    pub params: Vec<ParamKey>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CallableSelfKey {
    In,
    InOut,
    Sink,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ParamKey {
    /// Parameter passing mode (`in`, `out`, `sink`, ...).
    pub mode: ParamMode,
    /// Normalized semantic type identity for the parameter.
    pub ty: TypeKey,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypeKey {
    /// Primitive/unit-like type identities.
    Unit,
    Bool,
    Char,
    Int {
        signed: bool,
        bits: u8,
        nonzero: bool,
    },
    String,
    /// Named nominal type, potentially with normalized generic arguments.
    Named {
        module: ModulePath,
        path: SymbolPath,
        args: Vec<TypeKey>,
    },
    Tuple(Vec<TypeKey>),
    Array {
        elem: Box<TypeKey>,
        dims: Vec<usize>,
    },
    Slice(Box<TypeKey>),
    DynArray(Box<TypeKey>),
    Heap(Box<TypeKey>),
    Ref {
        mutable: bool,
        elem: Box<TypeKey>,
    },
    Set(Box<TypeKey>),
    Map {
        key: Box<TypeKey>,
        value: Box<TypeKey>,
    },
    Fn {
        params: Vec<ParamKey>,
        ret: Box<TypeKey>,
    },
    /// Generic parameter position after normalization (`T0`, `T1`, ...).
    GenericParam(u32),
    ErrorUnion {
        ok: Box<TypeKey>,
        errs: Vec<TypeKey>,
    },
    Refined {
        base: Box<TypeKey>,
        refinements: Vec<RefinementKind>,
    },
}

/// Module-local view of currently-known semantic symbol ids.
///
/// This table is intentionally conservative in the first migration step:
/// overloaded callables still share a base `SymbolId` until callable
/// disambiguators are threaded through resolution/type checking. The reverse
/// lookup therefore remains one-to-many for now.
#[derive(Debug, Clone, Default)]
pub struct SymbolIdTable {
    by_def: HashMap<DefId, SymbolId>,
    defs_by_symbol: HashMap<SymbolId, Vec<DefId>>,
}

impl SymbolId {
    pub fn new(module: ModulePath, path: SymbolPath, ns: SymbolNs) -> Self {
        Self {
            module,
            path,
            ns,
            disambiguator: None,
        }
    }

    pub fn with_disambiguator(mut self, disambiguator: SymbolDisambiguator) -> Self {
        self.disambiguator = Some(disambiguator);
        self
    }
}

impl SymbolIdTable {
    pub fn from_module(module_path: &ModulePath, module: &Module, def_table: &DefTable) -> Self {
        let mut table = Self::default();

        for item in &module.top_level_items {
            match item {
                TopLevelItem::TraitDef(trait_def) => {
                    table.record(
                        def_table.lookup_node_def_id(trait_def.id),
                        SymbolId::new(
                            module_path.clone(),
                            SymbolPath::from_names([trait_def.name.as_str()]),
                            SymbolNs::Trait,
                        ),
                    );
                }
                TopLevelItem::TypeDef(type_def) => {
                    table.record(
                        def_table.lookup_node_def_id(type_def.id),
                        SymbolId::new(
                            module_path.clone(),
                            SymbolPath::from_names([type_def.name.as_str()]),
                            SymbolNs::Type,
                        ),
                    );
                    if let crate::core::ast::TypeDefKind::Enum { variants } = &type_def.kind {
                        for variant in variants {
                            table.record(
                                def_table.lookup_node_def_id(variant.id),
                                SymbolId::new(
                                    module_path.clone(),
                                    SymbolPath::from_names([
                                        type_def.name.as_str(),
                                        variant.name.as_str(),
                                    ]),
                                    SymbolNs::Variant,
                                ),
                            );
                        }
                    }
                }
                TopLevelItem::FuncDecl(func_decl) => {
                    table.record_callable(
                        def_table.lookup_node_def_id(func_decl.id),
                        module_path,
                        &[func_decl.sig.name.as_str()],
                        SymbolNs::Value,
                        callable_sig_key_for_function(&func_decl.sig, def_table, module_path),
                    );
                }
                TopLevelItem::FuncDef(func_def) => {
                    table.record_callable(
                        def_table.lookup_node_def_id(func_def.id),
                        module_path,
                        &[func_def.sig.name.as_str()],
                        SymbolNs::Value,
                        callable_sig_key_for_function(&func_def.sig, def_table, module_path),
                    );
                }
                TopLevelItem::MethodBlock(method_block) => {
                    for method_item in &method_block.method_items {
                        let (method_id, method_name) = match method_item {
                            MethodItem::Decl(method_decl) => {
                                (method_decl.id, method_decl.sig.name.as_str())
                            }
                            MethodItem::Def(method_def) => {
                                (method_def.id, method_def.sig.name.as_str())
                            }
                        };
                        table.record_callable(
                            def_table.lookup_node_def_id(method_id),
                            module_path,
                            &[method_block.type_name.as_str(), method_name],
                            SymbolNs::Method,
                            callable_sig_key_for_method(
                                match method_item {
                                    MethodItem::Decl(method_decl) => &method_decl.sig,
                                    MethodItem::Def(method_def) => &method_def.sig,
                                },
                                def_table,
                                module_path,
                            ),
                        );
                    }
                }
                _ => {}
            }
        }

        table
    }

    pub fn lookup_symbol_id(&self, def_id: DefId) -> Option<&SymbolId> {
        self.by_def.get(&def_id)
    }

    pub fn lookup_local_def_ids(&self, symbol_id: &SymbolId) -> Option<&[DefId]> {
        self.defs_by_symbol.get(symbol_id).map(Vec::as_slice)
    }

    /// Associate a synthetic local alias def with an existing canonical symbol
    /// id, without changing the source-owned symbol id itself.
    pub fn record_alias_symbol(&mut self, def_id: DefId, symbol_id: SymbolId) {
        self.record(Some(def_id), symbol_id);
    }

    fn record(&mut self, def_id: Option<DefId>, symbol_id: SymbolId) {
        let Some(def_id) = def_id else {
            return;
        };
        self.by_def.insert(def_id, symbol_id.clone());
        self.defs_by_symbol
            .entry(symbol_id)
            .or_default()
            .push(def_id);
    }

    fn record_callable(
        &mut self,
        def_id: Option<DefId>,
        module_path: &ModulePath,
        path_segments: &[&str],
        ns: SymbolNs,
        sig_key: CallableSigKey,
    ) {
        self.record(
            def_id,
            SymbolId::new(
                module_path.clone(),
                SymbolPath::from_names(path_segments.iter().copied()),
                ns,
            )
            .with_disambiguator(SymbolDisambiguator::Callable(sig_key)),
        );
    }
}

fn callable_sig_key_for_function(
    sig: &FunctionSig,
    def_table: &DefTable,
    module_path: &ModulePath,
) -> CallableSigKey {
    CallableSigKey {
        type_param_count: sig.type_params.len() as u16,
        self_mode: None,
        params: callable_param_keys(&sig.type_params, &sig.params, def_table, module_path),
    }
}

fn callable_sig_key_for_method(
    sig: &MethodSig,
    def_table: &DefTable,
    module_path: &ModulePath,
) -> CallableSigKey {
    CallableSigKey {
        type_param_count: sig.type_params.len() as u16,
        self_mode: Some(match sig.self_param.mode {
            ParamMode::In => CallableSelfKey::In,
            ParamMode::InOut => CallableSelfKey::InOut,
            ParamMode::Sink => CallableSelfKey::Sink,
            ParamMode::Out => CallableSelfKey::InOut,
        }),
        params: callable_param_keys(&sig.type_params, &sig.params, def_table, module_path),
    }
}

fn callable_param_keys(
    type_params: &[TypeParam],
    params: &[crate::core::ast::Param],
    def_table: &DefTable,
    module_path: &ModulePath,
) -> Vec<ParamKey> {
    let generic_param_indexes = generic_param_index_map(type_params, def_table);
    params
        .iter()
        .map(|param| ParamKey {
            mode: param.mode.clone(),
            ty: type_key_from_expr(&param.typ, def_table, module_path, &generic_param_indexes),
        })
        .collect()
}

pub(crate) fn type_key_for_type_expr(
    type_expr: &TypeExpr,
    type_params: &[TypeParam],
    def_table: &DefTable,
    module_path: &ModulePath,
) -> TypeKey {
    let generic_param_indexes = generic_param_index_map(type_params, def_table);
    type_key_from_expr(type_expr, def_table, module_path, &generic_param_indexes)
}

fn generic_param_index_map(type_params: &[TypeParam], def_table: &DefTable) -> HashMap<DefId, u32> {
    type_params
        .iter()
        .enumerate()
        .filter_map(|(idx, param)| {
            def_table
                .lookup_node_def_id(param.id)
                .map(|def_id| (def_id, idx as u32))
        })
        .collect()
}

fn type_key_from_expr(
    type_expr: &TypeExpr,
    def_table: &DefTable,
    module_path: &ModulePath,
    generic_param_indexes: &HashMap<DefId, u32>,
) -> TypeKey {
    match &type_expr.kind {
        TypeExprKind::Infer => TypeKey::GenericParam(u32::MAX),
        TypeExprKind::Union { variants } => {
            if let Some((ok, errs)) = variants.split_first() {
                TypeKey::ErrorUnion {
                    ok: Box::new(type_key_from_expr(
                        ok,
                        def_table,
                        module_path,
                        generic_param_indexes,
                    )),
                    errs: errs
                        .iter()
                        .map(|variant| {
                            type_key_from_expr(
                                variant,
                                def_table,
                                module_path,
                                generic_param_indexes,
                            )
                        })
                        .collect(),
                }
            } else {
                TypeKey::ErrorUnion {
                    ok: Box::new(TypeKey::Unit),
                    errs: Vec::new(),
                }
            }
        }
        TypeExprKind::Named { ident, type_args } => {
            if let Some(builtin) = builtin_type_key(ident) {
                return builtin;
            }
            if let Some(def_id) = def_table.lookup_node_def_id(type_expr.id)
                && let Some(index) = generic_param_indexes.get(&def_id)
            {
                return TypeKey::GenericParam(*index);
            }
            let args = type_args
                .iter()
                .map(|arg| type_key_from_expr(arg, def_table, module_path, generic_param_indexes))
                .collect();
            TypeKey::Named {
                module: module_path.clone(),
                path: SymbolPath::from_names([ident.as_str()]),
                args,
            }
        }
        TypeExprKind::Refined {
            base_ty_expr,
            refinements,
        } => TypeKey::Refined {
            base: Box::new(type_key_from_expr(
                base_ty_expr,
                def_table,
                module_path,
                generic_param_indexes,
            )),
            refinements: refinements.clone(),
        },
        TypeExprKind::Array { elem_ty_expr, dims } => TypeKey::Array {
            elem: Box::new(type_key_from_expr(
                elem_ty_expr,
                def_table,
                module_path,
                generic_param_indexes,
            )),
            dims: dims.clone(),
        },
        TypeExprKind::DynArray { elem_ty_expr } => TypeKey::DynArray(Box::new(type_key_from_expr(
            elem_ty_expr,
            def_table,
            module_path,
            generic_param_indexes,
        ))),
        TypeExprKind::Tuple { field_ty_exprs } => TypeKey::Tuple(
            field_ty_exprs
                .iter()
                .map(|field| {
                    type_key_from_expr(field, def_table, module_path, generic_param_indexes)
                })
                .collect(),
        ),
        TypeExprKind::Slice { elem_ty_expr } => TypeKey::Slice(Box::new(type_key_from_expr(
            elem_ty_expr,
            def_table,
            module_path,
            generic_param_indexes,
        ))),
        TypeExprKind::Heap { elem_ty_expr } => TypeKey::Heap(Box::new(type_key_from_expr(
            elem_ty_expr,
            def_table,
            module_path,
            generic_param_indexes,
        ))),
        TypeExprKind::Ref {
            mutable,
            elem_ty_expr,
        } => TypeKey::Ref {
            mutable: *mutable,
            elem: Box::new(type_key_from_expr(
                elem_ty_expr,
                def_table,
                module_path,
                generic_param_indexes,
            )),
        },
        TypeExprKind::Fn {
            params,
            ret_ty_expr,
        } => TypeKey::Fn {
            params: params
                .iter()
                .map(|param| ParamKey {
                    mode: param.mode.clone(),
                    ty: type_key_from_expr(
                        &param.ty_expr,
                        def_table,
                        module_path,
                        generic_param_indexes,
                    ),
                })
                .collect(),
            ret: Box::new(type_key_from_expr(
                ret_ty_expr,
                def_table,
                module_path,
                generic_param_indexes,
            )),
        },
    }
}

fn builtin_type_key(name: &str) -> Option<TypeKey> {
    match name {
        "()" => Some(TypeKey::Unit),
        "bool" => Some(TypeKey::Bool),
        "char" => Some(TypeKey::Char),
        "string" => Some(TypeKey::String),
        "u8" => Some(TypeKey::Int {
            signed: false,
            bits: 8,
            nonzero: false,
        }),
        "u16" => Some(TypeKey::Int {
            signed: false,
            bits: 16,
            nonzero: false,
        }),
        "u32" => Some(TypeKey::Int {
            signed: false,
            bits: 32,
            nonzero: false,
        }),
        "u64" => Some(TypeKey::Int {
            signed: false,
            bits: 64,
            nonzero: false,
        }),
        "i8" => Some(TypeKey::Int {
            signed: true,
            bits: 8,
            nonzero: false,
        }),
        "i16" => Some(TypeKey::Int {
            signed: true,
            bits: 16,
            nonzero: false,
        }),
        "i32" => Some(TypeKey::Int {
            signed: true,
            bits: 32,
            nonzero: false,
        }),
        "i64" => Some(TypeKey::Int {
            signed: true,
            bits: 64,
            nonzero: false,
        }),
        _ => None,
    }
}

impl fmt::Display for SymbolPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let parts = self.segments.iter().map(|segment| segment.name.as_str());
        write!(f, "{}", parts.collect::<Vec<_>>().join("::"))
    }
}

impl fmt::Display for SymbolId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let module = self.module.segments().join("::");
        if self.path.segments.is_empty() {
            write!(f, "{module}")
        } else {
            write!(f, "{module}::{}", self.path)
        }
    }
}

#[cfg(test)]
#[path = "../tests/core/t_symbol_id.rs"]
mod tests;
