//! Core Machina compiler pipeline and language model.

pub mod analysis;
pub mod api;
pub mod backend;
pub mod capsule;
pub mod context;
pub mod diag;
pub mod elaborate;
pub mod ir;
pub mod lexer;
pub mod machine;
pub mod monomorphize;
pub mod nrvo;
pub mod parse;
pub mod protocol;
pub mod resolve;
pub mod semck;
pub mod symtab;
pub mod tree;
pub mod typecheck;
pub mod types;
pub mod typestate;
