//! Normalized tree
//!
//! The normalized tree is a 1:1 typed tree (DefIds + TypeIds) produced by
//! normalize.

pub mod builder;
pub mod model;

pub use builder::build_module;
pub use model::*;
