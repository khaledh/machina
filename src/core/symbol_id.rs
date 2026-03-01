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

use crate::core::capsule::ModulePath;
use crate::core::resolve::{DefId, DefTable, GlobalDefId};
use crate::core::tree::ParamMode;
use crate::core::tree::{MethodItem, Module, TopLevelItem};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolSegment {
    /// User-facing segment text as written in source.
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolNs {
    /// Functions, locals, globals, and other value-level defs.
    Value,
    /// Protocol definitions.
    Protocol,
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
    /// Typestate states.
    State,
    /// Protocol roles.
    Role,
    /// Generated or source-level machine/event handlers.
    Handler,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    /// A cross-module callable selected via imported/exported facts.
    Global(GlobalDefId),
    /// Final canonical callable identity once the frontend can provide it.
    Canonical(SymbolId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct CallableSigKey {
    /// Declared generic parameter count, normalized away from source names.
    pub type_param_count: u16,
    /// Receiver mode for methods, if any.
    pub self_mode: Option<CallableSelfKey>,
    /// Ordered callable parameters after generic-name normalization.
    pub params: Vec<ParamKey>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallableSelfKey {
    In,
    InOut,
    Sink,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParamKey {
    /// Parameter passing mode (`in`, `out`, `sink`, ...).
    pub mode: ParamMode,
    /// Normalized semantic type identity for the parameter.
    pub ty: TypeKey,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
                TopLevelItem::ProtocolDef(protocol_def) => {
                    table.record(
                        def_table.lookup_node_def_id(protocol_def.id),
                        SymbolId::new(
                            module_path.clone(),
                            SymbolPath::from_names([protocol_def.name.as_str()]),
                            SymbolNs::Protocol,
                        ),
                    );
                    for role in &protocol_def.roles {
                        table.record(
                            def_table.lookup_node_def_id(role.id),
                            SymbolId::new(
                                module_path.clone(),
                                SymbolPath::from_names([
                                    protocol_def.name.as_str(),
                                    role.name.as_str(),
                                ]),
                                SymbolNs::Role,
                            ),
                        );
                    }
                }
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
                    if let crate::core::tree::TypeDefKind::Enum { variants } = &type_def.kind {
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
                    );
                }
                TopLevelItem::FuncDef(func_def) => {
                    table.record_callable(
                        def_table.lookup_node_def_id(func_def.id),
                        module_path,
                        &[func_def.sig.name.as_str()],
                        SymbolNs::Value,
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
    ) {
        // Callable overloads still share the same base `SymbolId` until we
        // thread canonical callable disambiguators through the frontend.
        self.record(
            def_id,
            SymbolId::new(
                module_path.clone(),
                SymbolPath::from_names(path_segments.iter().copied()),
                ns,
            ),
        );
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
        if self.path.segments.is_empty() {
            write!(f, "{}", self.module)
        } else {
            write!(f, "{}::{}", self.module, self.path)
        }
    }
}

#[cfg(test)]
#[path = "../tests/core/t_symbol_id.rs"]
mod tests;
