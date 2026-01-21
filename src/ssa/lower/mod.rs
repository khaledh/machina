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
) -> Result<LoweredFunction, sem::LinearizeError> {
    if !func.sig.params.is_empty() {
        panic!("ssa lower_func only supports functions without params");
    }

    let mut lowerer = FuncLowerer::new(func, def_table, type_map);
    let entry = lowerer.builder.add_block();
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
