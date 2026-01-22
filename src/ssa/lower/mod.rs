//! SSA lowering from the semantic tree into the SSA IR.
//!
//! This module currently supports a subset of the language while the SSA
//! pipeline is built out incrementally.

mod branching;
mod linear;
mod linearize;
mod lowerer;
mod mapping;
mod types;

use crate::resolve::DefTable;
use crate::ssa::IrTypeCache;
use crate::ssa::lower::lowerer::BranchResult;
use crate::ssa::model::ir::{Function, Terminator};
use crate::tree::semantic as sem;
use crate::typeck::type_map::TypeMap;
use lowerer::FuncLowerer;

pub struct LoweredFunction {
    pub func: Function,
    pub types: IrTypeCache,
}

/// Lowers a semantic function definition into SSA IR.
///
/// This is the main entry point for SSA lowering. The process:
/// 1. Creates a `FuncLowerer` with type context and function signature
/// 2. Seeds the entry block with function parameters as block params
/// 3. Maps parameters to local variables for SSA value tracking
/// 4. Lowers the function body using branching expression lowering
/// 5. Terminates with a return instruction if the body produces a value
pub fn lower_func(
    func: &sem::FuncDef,
    def_table: &DefTable,
    type_map: &TypeMap,
    block_expr_plans: &sem::BlockExprPlanMap,
) -> Result<LoweredFunction, sem::LinearizeError> {
    // Initialize the lowerer with function metadata and type information.
    // The builder starts with the cursor at the entry block (block 0).
    let mut lowerer = FuncLowerer::new(func, def_table, type_map, block_expr_plans);

    // Add function parameters as block parameters to the entry block,
    // then establish the initial locals mapping from parameters.
    let entry = lowerer.builder.current_block();
    let param_defs = lowerer.param_defs.clone();
    let param_tys = lowerer.param_tys.clone();
    let mut param_values = Vec::with_capacity(param_tys.len());
    for ty in &param_tys {
        param_values.push(lowerer.builder.add_block_param(entry, *ty));
    }
    lowerer.set_locals_from_params(&param_defs, &param_tys, &param_values);

    // Lower the function body. This may produce multiple basic blocks
    // for control flow (if/else, loops, etc.). The cursor ends at the
    // final block where execution continues.
    let result = lowerer.lower_branching_expr(&func.body)?;

    // If the body produces a value (not an early return), emit the final return.
    if let BranchResult::Value(value) = result {
        lowerer
            .builder
            .terminate(Terminator::Return { value: Some(value) });
    }

    Ok(lowerer.finish())
}

#[cfg(test)]
#[path = "../../tests/ssa/t_lower.rs"]
mod tests;
