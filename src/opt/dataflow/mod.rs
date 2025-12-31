pub mod copy_elide;
pub mod memcpy_lower;

use crate::context::{LivenessContext, OptimizedMcirContext};

/// Run all dataflow-based MCIR optimizations.
pub fn run(ctx: LivenessContext) -> OptimizedMcirContext {
    // Elide last-use copies.
    let mut ctx = copy_elide::run(ctx);

    // Lower any remaining mem copies.
    memcpy_lower::run(&mut ctx.func_bodies);

    ctx
}

#[cfg(test)]
#[path = "../../tests/t_opt_dataflow.rs"]
mod tests;
