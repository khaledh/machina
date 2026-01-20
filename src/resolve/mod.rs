pub mod def;
pub mod def_table;
pub mod errors;
mod resolver;
pub mod symbols;

pub use def::{Def, DefId, DefIdGen, DefKind, FuncAttrs, TypeAttrs};
pub use def_table::{DefTable, DefTableBuilder, NodeDefLookup};
pub use errors::ResolveError;
pub use resolver::{SymbolResolver, resolve};
pub use symbols::SymbolKind;

#[cfg(test)]
#[path = "../tests/resolve/t_resolve.rs"]
mod tests;
