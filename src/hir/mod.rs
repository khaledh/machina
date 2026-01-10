//! High-level IR (HIR)
//!
//! This is a scaffold for now. The initial HIR is a thin wrapper around the
//! AST, but we'll gradually make it semantically explicit (moves, coercions,
//! call modes, places, etc.).

pub mod builder;
pub mod fold;
pub mod format;
pub mod model;
pub mod visit;

pub use crate::ast::*;
