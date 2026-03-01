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

use std::fmt;

use crate::core::capsule::ModulePath;
use crate::core::tree::ParamMode;

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
