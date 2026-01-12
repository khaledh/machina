//! Typed IR (TIR)
//!
//! The TIR is a typed, tree-shaped view of the HIR. It reuses the generic AST
//! model with `DefId` identifiers and `TypeId` annotations.

pub mod model;

pub use model::*;
