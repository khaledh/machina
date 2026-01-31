//! CFG-free SSA optimizations.

use crate::ssa::model::ir::Function;

pub use crate::ssa::opt::Pass;

pub mod const_fold;
pub mod empty_string_print;
pub mod field_addr_cse;
pub mod index_addr_simplify;
pub mod load_cse;
pub mod local_addr_copy_elim;
pub mod local_load_forward;
pub mod store_field_addr_simplify;

pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl PassManager {
    pub fn new() -> Self {
        Self {
            passes: vec![
                Box::new(const_fold::ConstFold),
                Box::new(empty_string_print::EmptyStringPrint),
                Box::new(local_addr_copy_elim::LocalAddrCopyElim),
                Box::new(local_load_forward::LocalLoadForward),
                Box::new(store_field_addr_simplify::StoreFieldAddrSimplify),
                Box::new(field_addr_cse::FieldAddrCse),
                Box::new(load_cse::LoadCse),
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
#[path = "../../../tests/ssa/opt/cfg_free/mod.rs"]
mod tests;
