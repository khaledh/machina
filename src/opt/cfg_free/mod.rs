pub mod const_branch_elim;
pub mod local_simplify;
pub mod memset_lower;
pub mod self_copy_elim;

use crate::context::{LoweredMcirContext, OptimizedMcirContext};
use crate::lower::LoweredFunc;
use crate::mcir::FuncBody;

use const_branch_elim::ConstBranchElim;
use local_simplify::LocalSimplify;
use memset_lower::MemSetLower;
use self_copy_elim::RemoveSelfCopies;

/// Run all CFG-free MCIR optimizations.
pub fn run(ctx: LoweredMcirContext) -> OptimizedMcirContext {
    let LoweredMcirContext {
        funcs,
        symbols,
        globals,
    } = ctx;
    let mut funcs = funcs;

    let mut manager = PassManager::new();
    manager.run(&mut funcs);

    OptimizedMcirContext {
        funcs,
        symbols,
        globals,
    }
}

pub trait Pass {
    #[allow(dead_code)]
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
                Box::new(MemSetLower),
            ],
        }
    }

    fn run(&mut self, funcs: &mut [LoweredFunc]) {
        const MAX_ITERS: usize = 4;

        for _ in 0..MAX_ITERS {
            let mut changed = false;
            for pass in &mut self.passes {
                for body in funcs.iter_mut() {
                    changed |= pass.run(&mut body.body);
                }
            }
            if !changed {
                break;
            }
        }
    }
}

#[cfg(test)]
#[path = "../../tests/opt/cfg_free/t_cfg_free.rs"]
mod tests;
