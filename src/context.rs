use crate::ast::Module;
use crate::mcir::{FuncBody, GlobalItem};
use crate::regalloc::AllocationResult;
use crate::resolve::def_map::DefMap;
use crate::symtab::SymbolTable;
use crate::typeck::type_map::TypeMap;

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
        let symbols = SymbolTable::new(&self.module, &def_map);
        ResolvedContext {
            module: self.module,
            def_map,
            symbols,
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
    pub symbols: SymbolTable,
}

impl ResolvedContext {
    pub fn with_type_map(self, type_map: TypeMap) -> TypeCheckedContext {
        TypeCheckedContext {
            module: self.module,
            def_map: self.def_map,
            type_map,
            symbols: self.symbols,
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
    pub symbols: SymbolTable,
}

// -----------------------------------------------------------------------------
// Analyzed Context
// -----------------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct AnalyzedContext {
    pub module: Module,
    pub def_map: DefMap,
    pub type_map: TypeMap,
    pub symbols: SymbolTable,
}

impl AnalyzedContext {
    pub fn with_func_bodies(
        self,
        func_bodies: Vec<FuncBody>,
        globals: Vec<GlobalItem>,
    ) -> LoweredMcirContext {
        LoweredMcirContext {
            func_bodies,
            symbols: self.symbols,
            globals,
        }
    }
}

// -----------------------------------------------------------------------------
// Lowered MCIR Context
// -----------------------------------------------------------------------------
#[derive(Clone)]
pub struct LoweredMcirContext {
    pub func_bodies: Vec<FuncBody>,
    pub symbols: SymbolTable,
    pub globals: Vec<GlobalItem>,
}

impl LoweredMcirContext {
    pub fn with_optimized_bodies(
        self,
        func_bodies: Vec<FuncBody>,
        globals: Vec<GlobalItem>,
    ) -> OptimizedMcirContext {
        OptimizedMcirContext {
            func_bodies,
            symbols: self.symbols,
            globals,
        }
    }
}

// -----------------------------------------------------------------------------
// Optimized MCIR Context
// -----------------------------------------------------------------------------
#[derive(Clone)]
pub struct OptimizedMcirContext {
    pub func_bodies: Vec<FuncBody>,
    pub symbols: SymbolTable,
    pub globals: Vec<GlobalItem>,
}

impl OptimizedMcirContext {
    pub fn with_alloc_results(self, alloc_results: Vec<AllocationResult>) -> RegAllocatedContext {
        RegAllocatedContext {
            func_bodies: self.func_bodies,
            alloc_results,
            symbols: self.symbols,
            globals: self.globals,
        }
    }
}

// -----------------------------------------------------------------------------
// Optimized MCIR & Reg Alloc Context
// -----------------------------------------------------------------------------
pub struct RegAllocatedContext {
    pub func_bodies: Vec<FuncBody>,
    pub alloc_results: Vec<AllocationResult>,
    pub symbols: SymbolTable,
    pub globals: Vec<GlobalItem>,
}
