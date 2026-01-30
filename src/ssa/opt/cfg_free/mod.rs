//! CFG-free SSA optimizations.

use crate::ssa::model::ir::Function;

pub use crate::ssa::opt::Pass;

pub mod const_fold;
pub mod index_addr_simplify;

pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl PassManager {
    pub fn new() -> Self {
        Self {
            passes: vec![
                Box::new(const_fold::ConstFold),
                Box::new(index_addr_simplify::IndexAddrSimplify),
            ],
        }
    }

    pub fn run(&mut self, funcs: &mut [Function]) {
        const MAX_ITERS: usize = 4;

        // Iterate to a local fixpoint; CFG-free passes should be idempotent.
        for _ in 0..MAX_ITERS {
            let mut changed = false;
            for pass in &mut self.passes {
                for func in funcs.iter_mut() {
                    changed |= pass.run(func);
                }
            }
            if !changed {
                break;
            }
        }
    }
}

#[cfg(test)]
#[path = "../../../tests/ssa/opt/t_cfg_free.rs"]
mod tests;
