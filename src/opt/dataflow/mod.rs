pub mod copy_elide;

use crate::context::{LivenessContext, OptimizedMcirContext};

use copy_elide::elide_last_use_copies;

/// Run all dataflow-based MCIR optimizations.
pub fn run(ctx: LivenessContext) -> OptimizedMcirContext {
    elide_last_use_copies(ctx)
}

#[cfg(test)]
#[path = "../../tests/t_opt_dataflow.rs"]
mod tests;
