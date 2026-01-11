use std::collections::HashMap;

use crate::ast::{EnumDefVariant, StructDefField, TypeExpr};
use crate::resolve::DefId;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    Var {
        def_id: DefId,
        is_mutable: bool,
    },
    Func {
        overloads: Vec<DefId>,
    },
    TypeAlias {
        def_id: DefId,
        ty_expr: TypeExpr,
    },
    StructDef {
        def_id: DefId,
        fields: Vec<StructDefField>,
    },
    EnumDef {
        def_id: DefId,
        variants: Vec<EnumDefVariant>,
    },
}

impl std::fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolKind::Var { .. } => write!(f, "var"),
            SymbolKind::Func { overloads } => write!(f, "func[{} overloads]", overloads.len()),
            SymbolKind::TypeAlias { ty_expr, .. } => write!(f, "type_alias[{}]", ty_expr),
            SymbolKind::StructDef { fields, .. } => {
                let field_names = fields
                    .iter()
                    .map(|field| field.name.as_str())
                    .collect::<Vec<_>>();
                write!(f, "struct_def[{}]", field_names.join(", "))
            }
            SymbolKind::EnumDef { variants, .. } => {
                let variant_names = variants
                    .iter()
                    .map(|variant| variant.name.as_str())
                    .collect::<Vec<_>>();
                write!(f, "enum_def[{}]", variant_names.join(", "))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
}

impl Symbol {
    pub fn def_id(&self) -> DefId {
        match &self.kind {
            SymbolKind::Var { def_id, .. } => *def_id,
            SymbolKind::Func { overloads } => overloads
                .first()
                .copied()
                .expect("Function symbol has no overloads"),
            SymbolKind::TypeAlias { def_id, .. } => *def_id,
            SymbolKind::StructDef { def_id, .. } => *def_id,
            SymbolKind::EnumDef { def_id, .. } => *def_id,
        }
    }

    pub fn func_overloads(&self) -> Option<&[DefId]> {
        match &self.kind {
            SymbolKind::Func { overloads } => Some(overloads.as_slice()),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Scope {
    pub defs: HashMap<String, Symbol>,
}
