use std::collections::HashSet;

use crate::liveness::LiveMap;
use crate::lower::LoweredFunc;
use crate::mcir::GlobalItem;
use crate::regalloc::AllocationResult;
use crate::resolve::DefTable;
use crate::symtab::SymbolTable;
use crate::tree::normalized::Module as NormalizedModule;
use crate::tree::parsed::Module as ParsedModule;
use crate::tree::resolved::Module as ResolvedModule;
use crate::tree::semantic::Module as SemanticModule;
use crate::tree::typed::Module as TypedModule;
use crate::tree::{NodeId, NodeIdGen};
use crate::typeck::type_map::TypeMap;

// -----------------------------------------------------------------------------
// Parsed Context
// -----------------------------------------------------------------------------

#[derive(Clone)]
pub struct ParsedContext {
    pub module: ParsedModule,
    pub node_id_gen: NodeIdGen,
}

impl ParsedContext {
    pub fn new(module: ParsedModule, node_id_gen: NodeIdGen) -> Self {
        Self {
            module,
            node_id_gen,
        }
    }

    pub fn with_def_table(self, def_table: DefTable, module: ResolvedModule) -> ResolvedContext {
        let symbols = SymbolTable::new(&module);
        ResolvedContext {
            module,
            def_table,
            symbols,
            node_id_gen: self.node_id_gen,
        }
    }
}

// -----------------------------------------------------------------------------
// Resolved Context
// -----------------------------------------------------------------------------

#[derive(Clone)]
pub struct ResolvedContext {
    pub module: ResolvedModule,
    pub def_table: DefTable,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
}

impl ResolvedContext {
    pub fn with_type_map(self, type_map: TypeMap, module: TypedModule) -> TypeCheckedContext {
        TypeCheckedContext {
            module,
            def_table: self.def_table,
            type_map,
            symbols: self.symbols,
            node_id_gen: self.node_id_gen,
        }
    }
}

// -----------------------------------------------------------------------------
// Type Checked Context
// -----------------------------------------------------------------------------

#[derive(Clone)]
pub struct TypeCheckedContext {
    pub module: TypedModule,
    pub def_table: DefTable,
    pub type_map: TypeMap,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
}

// -----------------------------------------------------------------------------
// Normalized Context
// -----------------------------------------------------------------------------

#[derive(Clone)]
pub struct NormalizedContext {
    pub module: NormalizedModule,
    pub def_table: DefTable,
    pub type_map: TypeMap,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
}

impl NormalizedContext {
    pub fn with_sem_results(
        self,
        implicit_moves: HashSet<NodeId>,
        init_assigns: HashSet<NodeId>,
        full_init_assigns: HashSet<NodeId>,
    ) -> SemanticCheckedContext {
        SemanticCheckedContext {
            module: self.module,
            def_table: self.def_table,
            type_map: self.type_map,
            symbols: self.symbols,
            node_id_gen: self.node_id_gen,
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
    pub module: NormalizedModule,
    pub def_table: DefTable,
    pub type_map: TypeMap,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
    pub implicit_moves: HashSet<NodeId>,
    pub init_assigns: HashSet<NodeId>,
    pub full_init_assigns: HashSet<NodeId>,
}

// -----------------------------------------------------------------------------
// Semantic Context
// -----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct SemanticContext {
    pub module: SemanticModule,
    pub def_table: DefTable,
    pub type_map: TypeMap,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
}

// -----------------------------------------------------------------------------
// Analyzed Context
// -----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct AnalyzedContext {
    pub module: SemanticModule,
    pub def_table: DefTable,
    pub type_map: TypeMap,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
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
