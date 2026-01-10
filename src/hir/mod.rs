//! High-level IR (HIR)
//!
//! This is a scaffold for now. The initial HIR is a thin wrapper around the
//! AST, but we'll gradually make it semantically explicit (moves, coercions,
//! call modes, places, etc.).

pub type Module = crate::ast::Module;
pub type NodeId = crate::ast::NodeId;
