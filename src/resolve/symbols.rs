use std::collections::HashMap;

use crate::ast::{EnumVariant, StructField, TypeExpr};
use crate::resolve::def_map::DefId;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    Var { is_mutable: bool },
    Func,
    TypeAlias { ty_expr: TypeExpr },
    StructDef { fields: Vec<StructField> },
    EnumDef { variants: Vec<EnumVariant> },
}

impl std::fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolKind::Var { .. } => write!(f, "var"),
            SymbolKind::Func => write!(f, "func"),
            SymbolKind::TypeAlias { ty_expr } => write!(f, "type_alias[{}]", ty_expr),
            SymbolKind::StructDef { fields } => {
                let field_names = fields
                    .iter()
                    .map(|field| field.name.as_str())
                    .collect::<Vec<_>>();
                write!(f, "struct_def[{}]", field_names.join(", "))
            }
            SymbolKind::EnumDef { variants } => {
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
    pub def_id: DefId,
    pub name: String,
    pub kind: SymbolKind,
}

#[derive(Clone, Debug)]
pub struct Scope {
    pub defs: HashMap<String, Symbol>,
}
