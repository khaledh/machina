use std::collections::HashSet;

use crate::ast::{Module as AstModule, NodeId};
use crate::hir::model::Module as HirModule;
use crate::liveness::LiveMap;
use crate::lower::LoweredFunc;
use crate::mcir::GlobalItem;
use crate::regalloc::AllocationResult;
use crate::resolve::def_map::DefMap;
use crate::symtab::SymbolTable;
use crate::typeck::type_map::TypeMap;

// -----------------------------------------------------------------------------
// Parsed Context
// -----------------------------------------------------------------------------
#[derive(Clone)]
pub struct ParsedContext {
    pub module: AstModule,
}

impl ParsedContext {
    pub fn new(module: AstModule) -> Self {
        Self { module }
    }

    pub fn with_def_map(self, def_map: DefMap, hir_module: HirModule) -> ResolvedContext {
        let symbols = SymbolTable::new(&hir_module);
        ResolvedContext {
            ast_module: self.module,
            module: hir_module,
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
    pub ast_module: AstModule,
    pub module: HirModule,
    pub def_map: DefMap,
    pub symbols: SymbolTable,
}

impl ResolvedContext {
    pub fn with_type_map(self, type_map: TypeMap) -> TypeCheckedContext {
        TypeCheckedContext {
            ast_module: self.ast_module,
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
    pub ast_module: AstModule,
    pub module: HirModule,
    pub def_map: DefMap,
    pub type_map: TypeMap,
    pub symbols: SymbolTable,
}

impl TypeCheckedContext {
    pub fn with_sem_results(
        self,
        implicit_moves: HashSet<NodeId>,
        init_assigns: HashSet<NodeId>,
        full_init_assigns: HashSet<NodeId>,
    ) -> SemanticCheckedContext {
        SemanticCheckedContext {
            ast_module: self.ast_module,
            module: self.module,
            def_map: self.def_map,
            type_map: self.type_map,
            symbols: self.symbols,
            implicit_moves,
            init_assigns,
            full_init_assigns,
        }
    }
}

// -----------------------------------------------------------------------------
// Semantic Checked Context
// -----------------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct SemanticCheckedContext {
    pub ast_module: AstModule,
    pub module: HirModule,
    pub def_map: DefMap,
    pub type_map: TypeMap,
    pub symbols: SymbolTable,
    pub implicit_moves: HashSet<NodeId>,
    pub init_assigns: HashSet<NodeId>,
    pub full_init_assigns: HashSet<NodeId>,
}

// -----------------------------------------------------------------------------
// Analyzed Context
// -----------------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct AnalyzedContext {
    pub ast_module: AstModule,
    pub module: HirModule,
    pub def_map: DefMap,
    pub type_map: TypeMap,
    pub symbols: SymbolTable,
    pub implicit_moves: HashSet<NodeId>,
    pub init_assigns: HashSet<NodeId>,
    pub full_init_assigns: HashSet<NodeId>,
}

impl AnalyzedContext {
    pub fn with_funcs(
        self,
        funcs: Vec<LoweredFunc>,
        globals: Vec<GlobalItem>,
    ) -> LoweredMcirContext {
        LoweredMcirContext {
            funcs,
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
    pub funcs: Vec<LoweredFunc>,
    pub symbols: SymbolTable,
    pub globals: Vec<GlobalItem>,
}

impl LoweredMcirContext {
    pub fn with_optimized_funcs(
        self,
        funcs: Vec<LoweredFunc>,
        globals: Vec<GlobalItem>,
    ) -> OptimizedMcirContext {
        OptimizedMcirContext {
            funcs,
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
    pub funcs: Vec<LoweredFunc>,
    pub symbols: SymbolTable,
    pub globals: Vec<GlobalItem>,
}

impl OptimizedMcirContext {
    pub fn with_liveness(self, live_maps: Vec<LiveMap>) -> LivenessContext {
        LivenessContext {
            funcs: self.funcs,
            live_maps,
            symbols: self.symbols,
            globals: self.globals,
        }
    }

    pub fn with_alloc_results(self, alloc_results: Vec<AllocationResult>) -> RegAllocatedContext {
        RegAllocatedContext {
            funcs: self.funcs,
            alloc_results,
            symbols: self.symbols,
            globals: self.globals,
        }
    }
}

// -----------------------------------------------------------------------------
// Liveness Context
// -----------------------------------------------------------------------------
#[derive(Clone)]
pub struct LivenessContext {
    pub funcs: Vec<LoweredFunc>,
    pub live_maps: Vec<LiveMap>,
    pub symbols: SymbolTable,
    pub globals: Vec<GlobalItem>,
}

impl LivenessContext {
    pub fn with_alloc_results(self, alloc_results: Vec<AllocationResult>) -> RegAllocatedContext {
        RegAllocatedContext {
            funcs: self.funcs,
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
    pub funcs: Vec<LoweredFunc>,
    pub alloc_results: Vec<AllocationResult>,
    pub symbols: SymbolTable,
    pub globals: Vec<GlobalItem>,
}
