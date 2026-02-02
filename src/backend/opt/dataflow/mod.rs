//! Dataflow-based SSA optimizations.

use crate::backend::opt::Pass;
use crate::ir::Function;

pub mod byref_copy_elim;
pub mod dce;
pub mod local_memcpy_elim;
pub mod memops;
pub mod ptr_utils;
pub mod stack_temp_copy_elim;

pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl Default for PassManager {
    fn default() -> Self {
        Self::new()
    }
}

impl PassManager {
    pub fn new() -> Self {
        Self {
            passes: vec![
                Box::new(stack_temp_copy_elim::StackTempCopyElim),
                Box::new(local_memcpy_elim::LocalMemCopyElim),
                Box::new(byref_copy_elim::ByRefCopyElim),
                Box::new(dce::DeadCodeElim),
                Box::new(memops::MemOps),
            ],
        }
    }

    pub fn run(&mut self, funcs: &mut [Function]) {
        const MAX_ITERS: usize = 3;

        // Iterate to a small fixpoint; dataflow passes enable each other.
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
#[path = "../../../tests/backend/opt/dataflow/mod.rs"]
mod tests;
