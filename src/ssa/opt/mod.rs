//! SSA optimization passes.

use crate::ssa::model::ir::Function;

pub mod cfg;
pub mod cfg_free;
pub mod dataflow;

/// SSA optimization pass trait.
pub trait Pass {
    fn name(&self) -> &'static str;
    fn run(&mut self, func: &mut Function) -> bool;
}

/// Combined SSA optimization pipeline.
///
/// This keeps the pass order in one place and provides a single entry point
/// for running SSA optimizations during early bring-up.
pub struct Pipeline {
    cfg: cfg::PassManager,
    cfg_free: cfg_free::PassManager,
    dataflow: dataflow::PassManager,
}

impl Pipeline {
    pub fn new() -> Self {
        Self {
            cfg: cfg::PassManager::new(),
            cfg_free: cfg_free::PassManager::new(),
            dataflow: dataflow::PassManager::new(),
        }
    }

    pub fn run(&mut self, funcs: &mut [Function]) {
        // Normalize the CFG before and after local simplification.
        self.cfg.run(funcs);
        self.cfg_free.run(funcs);
        self.dataflow.run(funcs);
        self.cfg.run(funcs);
    }
}
