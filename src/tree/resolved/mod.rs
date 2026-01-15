//! Resolved tree
//!
//! The resolved tree is a resolved view of the parsed tree. It
//! reuses the generic parsed model with `DefId` identifiers.

pub mod builder;
pub mod model;

pub use builder::ResolvedTreeBuilder;
pub use model::*;
