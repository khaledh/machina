//! Shared compiler analyses (CFG, dataflow, etc).

pub mod batch;
pub mod code_actions;
pub mod completion;
pub mod dataflow;
pub mod db;
pub mod diagnostics;
pub mod facts;
pub mod frontend_support;
pub mod lookups;
pub mod module_graph;
pub mod pipeline;
pub mod query;
pub mod results;
pub mod snapshot;
pub mod syntax_index;
