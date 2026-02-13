//! Shared compiler analyses (CFG, dataflow, etc).

pub mod batch;
pub mod code_actions;
pub mod completion;
pub mod db;
pub mod diagnostics;
pub mod frontend_support;
pub mod lookups;
pub mod module_graph;
pub mod pipeline;
pub mod program_imports;
pub mod program_pipeline;
pub mod query;
pub mod rename;
pub mod results;
pub mod signature_help;
pub mod snapshot;
pub mod syntax_index;
