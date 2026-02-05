use std::collections::{HashMap, HashSet};

use crate::resolve::{DefId, DefTable};
use crate::semck::closure::capture::ClosureCapture;
use crate::symtab::SymbolTable;
use crate::tree::normalized::Module as NormalizedModule;
use crate::tree::parsed::Module as ParsedModule;
use crate::tree::resolved::Module as ResolvedModule;
use crate::tree::semantic::Module as SemanticModule;
use crate::tree::semantic::{DropPlanMap, LoweringPlanMap};
use crate::tree::typed::Module as TypedModule;
use crate::tree::{NodeId, NodeIdGen};
use crate::typeck::type_map::{CallSigMap, GenericInstMap, TypeMap};

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
        let symbols = SymbolTable::new(&module, &def_table);
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
    pub fn with_type_map(
        self,
        type_map: TypeMap,
        call_sigs: CallSigMap,
        generic_insts: GenericInstMap,
        module: TypedModule,
    ) -> TypeCheckedContext {
        TypeCheckedContext {
            module,
            def_table: self.def_table,
            type_map,
            call_sigs,
            generic_insts,
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
    pub call_sigs: CallSigMap,
    pub generic_insts: GenericInstMap,
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
    pub call_sigs: CallSigMap,
    pub generic_insts: GenericInstMap,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
}

impl NormalizedContext {
    pub fn with_sem_results(
        self,
        implicit_moves: HashSet<NodeId>,
        init_assigns: HashSet<NodeId>,
        full_init_assigns: HashSet<NodeId>,
        closure_captures: HashMap<DefId, Vec<ClosureCapture>>,
    ) -> SemanticCheckedContext {
        SemanticCheckedContext {
            module: self.module,
            def_table: self.def_table,
            type_map: self.type_map,
            call_sigs: self.call_sigs,
            generic_insts: self.generic_insts,
            symbols: self.symbols,
            node_id_gen: self.node_id_gen,
            implicit_moves,
            init_assigns,
            full_init_assigns,
            closure_captures,
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
    pub call_sigs: CallSigMap,
    pub generic_insts: GenericInstMap,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
    pub implicit_moves: HashSet<NodeId>,
    pub init_assigns: HashSet<NodeId>,
    pub full_init_assigns: HashSet<NodeId>,
    pub closure_captures: HashMap<DefId, Vec<ClosureCapture>>,
}

// -----------------------------------------------------------------------------
// Semantic Context
// -----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct SemanticContext {
    pub module: SemanticModule,
    pub def_table: DefTable,
    pub type_map: TypeMap,
    pub lowering_plans: LoweringPlanMap,
    pub drop_plans: DropPlanMap,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
    pub generic_insts: GenericInstMap,
}

// -----------------------------------------------------------------------------
// Analyzed Context
// -----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct AnalyzedContext {
    pub module: SemanticModule,
    pub def_table: DefTable,
    pub type_map: TypeMap,
    pub lowering_plans: LoweringPlanMap,
    pub drop_plans: DropPlanMap,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
    pub generic_insts: GenericInstMap,
}

impl AnalyzedContext {}
