//! Dataflow-based SSA optimizations.

use crate::ssa::model::ir::Function;
use crate::ssa::opt::Pass;

pub mod byref_copy_elim;
pub mod dce;
pub mod memops;

pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl PassManager {
    pub fn new() -> Self {
        Self {
            passes: vec![
                Box::new(byref_copy_elim::ByRefCopyElim),
                Box::new(dce::DeadCodeElim),
                Box::new(memops::MemOps),
            ],
        }
    }

    pub fn run(&mut self, funcs: &mut [Function]) {
        for pass in &mut self.passes {
            for func in funcs.iter_mut() {
                pass.run(func);
            }
        }
    }
}

#[cfg(test)]
#[path = "../../../tests/ssa/opt/t_dataflow.rs"]
mod tests;
