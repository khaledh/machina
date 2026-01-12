//! High-level IR (HIR)
//!
//! The HIR is a resolved, tree-shaped view of the AST. It reuses the generic
//! AST model with `DefId` identifiers.

pub mod builder;
pub mod model;

pub use model::*;
