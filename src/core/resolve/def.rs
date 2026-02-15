use std::fmt;
use std::hash::{Hash, Hasher};

use crate::core::capsule::ModuleId;

// Defs store only identifiers and kinds; def kind structure lives in
// the resolved tree.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DefId(pub u32);

/// Sentinel def-id used in partial resolve paths when a node has no resolved
/// definition binding.
pub const UNKNOWN_DEF_ID: DefId = DefId(u32::MAX);

impl fmt::Display for DefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalDefId {
    pub module_id: ModuleId,
    pub def_id: DefId,
}

impl GlobalDefId {
    pub fn new(module_id: ModuleId, def_id: DefId) -> Self {
        Self { module_id, def_id }
    }
}

impl fmt::Display for GlobalDefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "m{}::{}", self.module_id.0, self.def_id)
    }
}

pub struct DefIdGen {
    next_id: u32,
}

impl Default for DefIdGen {
    fn default() -> Self {
        Self::new()
    }
}

impl DefIdGen {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    pub fn new_id(&mut self) -> DefId {
        let id = DefId(self.next_id);
        self.next_id += 1;
        id
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefKind {
    ProtocolDef,
    ProtocolRole,
    TraitDef {
        attrs: TraitAttrs,
    },
    TypeDef {
        attrs: TypeAttrs,
    },
    TypeParam,
    EnumVariantName,
    FuncDef {
        attrs: FuncAttrs,
    },
    FuncDecl {
        attrs: FuncAttrs,
    },
    LocalVar {
        nrvo_eligible: bool,
        is_mutable: bool,
    },
    Param {
        index: u32,
        is_mutable: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Visibility {
    #[default]
    Private,
    Public,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TraitAttrs {
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeAttrs {
    pub intrinsic: bool,
    pub visibility: Visibility,
    pub opaque: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FuncAttrs {
    pub intrinsic: bool,
    pub runtime: bool,
    pub machines: bool,
    pub link_name: Option<String>,
    pub visibility: Visibility,
}

impl fmt::Display for DefKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DefKind::ProtocolDef => write!(f, "ProtocolDef"),
            DefKind::ProtocolRole => write!(f, "ProtocolRole"),
            DefKind::TraitDef { .. } => write!(f, "TraitDef"),
            DefKind::TypeDef { .. } => write!(f, "TypeDef"),
            DefKind::TypeParam => write!(f, "TypeParam"),
            DefKind::EnumVariantName => write!(f, "EnumVariantName"),
            DefKind::FuncDef { .. } => write!(f, "FuncDef"),
            DefKind::FuncDecl { .. } => write!(f, "FuncDecl"),
            DefKind::LocalVar {
                nrvo_eligible,
                is_mutable,
            } => {
                if *nrvo_eligible {
                    write!(f, "LocalVar (NRVO eligible)")?;
                } else {
                    write!(f, "LocalVar")?;
                }
                if *is_mutable {
                    write!(f, " (mutable)")?;
                }
                Ok(())
            }
            DefKind::Param { index, is_mutable } => {
                write!(f, "Param[{}]", index)?;
                if *is_mutable {
                    write!(f, " (mutable)")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub struct Def {
    pub id: DefId,
    pub name: String,
    pub kind: DefKind,
}

impl Hash for Def {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for Def {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialOrd for Def {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

impl fmt::Display for Def {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Def [{}] {}: {}", self.id, self.name, self.kind)
    }
}

impl Def {
    pub fn is_intrinsic(&self) -> bool {
        match &self.kind {
            DefKind::TypeDef { attrs } => attrs.intrinsic,
            DefKind::FuncDef { attrs } | DefKind::FuncDecl { attrs } => attrs.intrinsic,
            _ => false,
        }
    }

    pub fn is_public(&self) -> bool {
        match &self.kind {
            DefKind::TraitDef { attrs } => attrs.visibility == Visibility::Public,
            DefKind::TypeDef { attrs } => attrs.visibility == Visibility::Public,
            DefKind::FuncDef { attrs } | DefKind::FuncDecl { attrs } => {
                attrs.visibility == Visibility::Public
            }
            _ => false,
        }
    }

    pub fn is_opaque(&self) -> bool {
        match &self.kind {
            DefKind::TypeDef { attrs } => attrs.opaque,
            _ => false,
        }
    }

    pub fn is_runtime(&self) -> bool {
        match &self.kind {
            DefKind::FuncDef { attrs } | DefKind::FuncDecl { attrs } => attrs.runtime,
            _ => false,
        }
    }

    pub fn link_name(&self) -> Option<&str> {
        match &self.kind {
            DefKind::FuncDef { attrs } | DefKind::FuncDecl { attrs } => attrs.link_name.as_deref(),
            _ => None,
        }
    }
}
