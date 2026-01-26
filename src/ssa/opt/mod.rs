//! SSA optimization passes.

use crate::ssa::model::ir::Function;

pub mod cfg_free;
pub mod dataflow;

/// SSA optimization pass trait.
pub trait Pass {
    fn name(&self) -> &'static str;
    fn run(&mut self, func: &mut Function) -> bool;
}
