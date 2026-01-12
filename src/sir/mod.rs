//! Semantic IR (SIR)
//!
//! The SIR is a tree-shaped, semantics-oriented view of the program.
//! Initially it mirrors TIR 1:1, then gains explicit semantic nodes.

pub mod builder;
pub mod model;

pub use model::*;
