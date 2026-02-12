//! Resolved tree
//!
//! The resolved tree is a resolved view of the parsed tree. It
//! reuses the generic parsed ir with `DefId` identifiers.

pub mod builder;
pub mod model;

pub use builder::build_module;
pub use model::*;
