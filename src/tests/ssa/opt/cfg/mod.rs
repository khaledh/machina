use crate::ssa::model::ir::Function;
use crate::ssa::opt::cfg::PassManager;

fn run_cleanup(func: &mut Function) {
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(func));
}

mod t_cleanup;
