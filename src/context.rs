use crate::analysis::{DefMap, TypeMap};
use crate::ast::Module;
use crate::ir::types::IrFunction;
use crate::regalloc::alloc::AllocationResult;

// -----------------------------------------------------------------------------
// AST Context
// -----------------------------------------------------------------------------
#[derive(Clone)]
pub struct AstContext {
    pub module: Module,
}

impl AstContext {
    pub fn new(module: Module) -> Self {
        Self { module }
    }

    pub fn with_def_map(self, def_map: DefMap) -> ResolvedContext {
        ResolvedContext {
            module: self.module,
            def_map,
        }
    }
}

// -----------------------------------------------------------------------------
// Resolved Context
// -----------------------------------------------------------------------------
#[derive(Clone)]
pub struct ResolvedContext {
    pub module: Module,
    pub def_map: DefMap,
}

impl ResolvedContext {
    pub fn with_type_map(self, type_map: TypeMap) -> TypeCheckedContext {
        TypeCheckedContext {
            module: self.module,
            def_map: self.def_map,
            type_map,
        }
    }
}

// -----------------------------------------------------------------------------
// Type Checked Context
// -----------------------------------------------------------------------------
#[derive(Clone)]
pub struct TypeCheckedContext {
    pub module: Module,
    pub def_map: DefMap,
    pub type_map: TypeMap,
}

// -----------------------------------------------------------------------------
// Analyzed Context
// -----------------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct AnalyzedContext {
    pub module: Module,
    pub def_map: DefMap,
    pub type_map: TypeMap,
}

impl AnalyzedContext {
    pub fn with_ir_funcs(self, ir_funcs: Vec<IrFunction>) -> LoweredContext {
        LoweredContext { ir_funcs }
    }
}

// -----------------------------------------------------------------------------
// Lowered Context
// -----------------------------------------------------------------------------
#[derive(Clone)]
pub struct LoweredContext {
    pub ir_funcs: Vec<IrFunction>,
}

impl LoweredContext {
    pub fn with_alloc_results(
        self,
        alloc_results: Vec<AllocationResult>,
    ) -> LoweredRegAllocContext {
        LoweredRegAllocContext {
            ir_funcs: self.ir_funcs,
            alloc_results,
        }
    }
}

// -----------------------------------------------------------------------------
// Lowered & Reg Alloc Context
// -----------------------------------------------------------------------------
pub struct LoweredRegAllocContext {
    pub ir_funcs: Vec<IrFunction>,
    pub alloc_results: Vec<AllocationResult>,
}
