//! Typed tree
//!
//! The typed tree is a typed view of the resolved tree. It reuses the
//! generic model with `DefId` identifiers and `TypeId` annotations.

pub mod builder;
pub mod model;

pub use builder::build_module;
pub use model::*;
