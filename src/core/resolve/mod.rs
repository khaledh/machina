pub mod def;
pub mod def_table;
pub mod errors;
pub mod owners;
mod resolver;
pub mod symbols;

pub use def::{
    Def, DefId, DefIdGen, DefKind, FuncAttrs, GlobalDefId, TraitAttrs, TypeAttrs, UNKNOWN_DEF_ID,
    Visibility,
};
pub use def_table::{DefLocation, DefTable, DefTableBuilder, NodeDefLookup};
pub use errors::{ResolveError, ResolveErrorKind};
pub use owners::attach_def_owners;
pub use resolver::{
    ImportedCallableSig, ImportedFacts, ImportedModule, ImportedParamSig, ImportedSymbol,
    ImportedTraitMethodSig, ImportedTraitPropertySig, ImportedTraitSig, ResolveOutput,
    SymbolResolver, resolve, resolve_partial, resolve_program, resolve_with_imports,
    resolve_with_imports_and_symbols, resolve_with_imports_and_symbols_and_typestate_roles_partial,
    resolve_with_imports_and_symbols_partial, resolve_with_imports_partial,
};
pub use symbols::SymbolKind;

#[cfg(test)]
#[path = "../../tests/resolve/t_resolve.rs"]
mod tests;
