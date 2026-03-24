//! Elaboration plans consumed by SSA lowering.
//!
//! These plan types form the shared contract between elaboration (producer)
//! and lowering (consumer). Each plan pre-computes semantic decisions so
//! that lowering can focus on code generation.

pub mod call_plan;
pub mod drop_plan;
pub mod for_plan;
pub mod index_plan;
pub mod iterable_protocol;
pub mod lowering_plan;
pub mod match_plan;
pub mod string_fmt;

pub use call_plan::*;
pub use drop_plan::*;
pub use for_plan::*;
pub use index_plan::*;
pub use iterable_protocol::*;
pub use lowering_plan::*;
pub use match_plan::*;
pub use string_fmt::*;
