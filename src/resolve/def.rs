use std::fmt;
use std::hash::{Hash, Hasher};

// Defs store only identifiers and kinds; def kind structure lives in
// the resolved tree.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DefId(pub u32);

impl fmt::Display for DefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
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
    TypeDef,
    FuncDef,
    FuncDecl,
    LocalVar {
        nrvo_eligible: bool,
        is_mutable: bool,
    },
    Param {
        index: u32,
        is_mutable: bool,
    },
}

impl fmt::Display for DefKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DefKind::TypeDef => write!(f, "TypeDef"),
            DefKind::FuncDef => write!(f, "FuncDef"),
            DefKind::FuncDecl => write!(f, "FuncDecl"),
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
