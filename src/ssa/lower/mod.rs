//! SSA lowering from the semantic tree into the SSA IR.
//!
//! This module currently supports a subset of the language while the SSA
//! pipeline is built out incrementally.

mod branching;
mod ctx;
mod linear;
mod linearize;
mod lowerer;
mod mapping;

use crate::resolve::DefTable;
use crate::ssa::lower::lowerer::BranchResult;
use crate::ssa::model::ir::{Function, Terminator, TypeTable};
use crate::tree::semantic as sem;
use crate::typeck::type_map::TypeMap;
use lowerer::FuncLowerer;

pub struct LoweredFunction {
    pub func: Function,
    pub types: TypeTable,
}

pub fn lower_func(
    func: &sem::FuncDef,
    def_table: &DefTable,
    type_map: &TypeMap,
    block_expr_plans: &sem::BlockExprPlanMap,
) -> Result<LoweredFunction, sem::LinearizeError> {
    let mut lowerer = FuncLowerer::new(func, def_table, type_map, block_expr_plans);
    let entry = lowerer.builder.add_block();

    // Seed the entry block with SSA params and map them to locals.
    let param_defs = lowerer.param_defs.clone();
    let param_tys = lowerer.param_tys.clone();
    let mut param_values = Vec::with_capacity(param_tys.len());
    for ty in &param_tys {
        param_values.push(lowerer.builder.add_block_param(entry, *ty));
    }
    lowerer.set_locals_from_params(&param_defs, &param_tys, &param_values);

    let result = lowerer.lower_branching_expr(entry, &func.body)?;
    if let BranchResult::Value(result) = result {
        lowerer.builder.set_terminator(
            result.block,
            Terminator::Return {
                value: Some(result.value),
            },
        );
    }

    Ok(lowerer.finish())
}

#[cfg(test)]
#[path = "../../tests/ssa/t_lower.rs"]
mod tests;
