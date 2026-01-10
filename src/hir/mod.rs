//! High-level IR (HIR)
//!
//! The HIR is a resolved, tree-shaped view of the AST. It reuses the generic
//! AST model with `DefId` identifiers.

pub mod builder;
pub mod fold;
pub mod format;
pub mod model;
pub mod visit;

pub use fold::AstFolder;
pub use model::*;
pub use visit::*;
