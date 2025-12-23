pub mod const_branch_elim;
pub mod local_simplify;
pub mod self_copy_elim;

use crate::context::{LoweredMcirContext, OptimizedMcirContext};
use crate::mcir::FuncBody;

use const_branch_elim::ConstBranchElim;
use local_simplify::LocalSimplify;
use self_copy_elim::RemoveSelfCopies;

/// Run all MCIR-level optimization passes.
pub fn optimize(ctx: LoweredMcirContext) -> OptimizedMcirContext {
    let LoweredMcirContext {
        func_bodies,
        symbols,
    } = ctx;
    let mut bodies = func_bodies;

    let mut manager = PassManager::new();
    manager.run(&mut bodies);

    OptimizedMcirContext {
        func_bodies: bodies,
        symbols,
    }
}

trait Pass {
    fn name(&self) -> &'static str;
    fn run(&mut self, body: &mut FuncBody) -> bool;
}

struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl PassManager {
    fn new() -> Self {
        Self {
            passes: vec![
                Box::new(LocalSimplify),
                Box::new(ConstBranchElim),
                Box::new(RemoveSelfCopies),
            ],
        }
    }

    fn run(&mut self, bodies: &mut [FuncBody]) {
        const MAX_ITERS: usize = 4;

        for _ in 0..MAX_ITERS {
            let mut changed = false;
            for pass in &mut self.passes {
                for body in bodies.iter_mut() {
                    changed |= pass.run(body);
                }
            }
            if !changed {
                break;
            }
        }
    }
}

#[cfg(test)]
#[path = "../tests/t_opt.rs"]
mod tests;
