use std::collections::HashMap;

use crate::resolve::def_map::DefId;
use crate::resolve::errors::SymbolKind;

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
