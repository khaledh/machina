pub mod def;
pub mod def_table;
pub mod errors;
mod resolver;
pub mod symbols;

pub use def::{Def, DefId, DefIdGen, DefKind, FuncAttrs, TraitAttrs, TypeAttrs, Visibility};
pub use def_table::{DefTable, DefTableBuilder, NodeDefLookup};
pub use errors::ResolveError;
pub use resolver::{
    ImportedModule, SymbolResolver, resolve, resolve_program, resolve_with_imports,
};
pub use symbols::SymbolKind;

#[cfg(test)]
#[path = "../tests/resolve/t_resolve.rs"]
mod tests;
