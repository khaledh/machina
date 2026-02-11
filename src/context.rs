use std::collections::{HashMap, HashSet};

use crate::frontend::{ModuleId, ModulePath, ParsedModule as FrontendParsedModule, ProgramParsed};
use crate::resolve::{DefId, DefTable, GlobalDefId};
use crate::semck::closure::capture::ClosureCapture;
use crate::symtab::SymbolTable;
use crate::tree::normalized::Module as NormalizedModule;
use crate::tree::parsed::Module as ParsedModule;
use crate::tree::resolved::Module as ResolvedModule;
use crate::tree::semantic::Module as SemanticModule;
use crate::tree::semantic::{DropPlanMap, LoweringPlanMap};
use crate::tree::typed::Module as TypedModule;
use crate::tree::{NodeId, NodeIdGen};
use crate::typecheck::type_map::{CallSigMap, GenericInstMap, TypeMap};

// -----------------------------------------------------------------------------
// Program Parsed Context
// -----------------------------------------------------------------------------

/// Program-level parsed context produced by frontend module discovery/parsing.
///
/// This keeps module-graph data in one place so later resolve/typecheck work
/// can consume a stable program abstraction rather than ad-hoc maps.
#[derive(Clone)]
pub struct ProgramParsedContext {
    pub program: ProgramParsed,
}

impl ProgramParsedContext {
    pub fn new(program: ProgramParsed) -> Self {
        Self { program }
    }

    pub fn entry(&self) -> ModuleId {
        self.program.entry
    }

    pub fn entry_module(&self) -> &FrontendParsedModule {
        self.program.entry_module()
    }

    pub fn module(&self, id: ModuleId) -> Option<&FrontendParsedModule> {
        self.program.module(id)
    }

    pub fn dependency_order_from_entry(&self) -> Vec<ModuleId> {
        self.program.dependency_order_from_entry()
    }

    pub fn next_node_id_gen(&self) -> &NodeIdGen {
        &self.program.next_node_id_gen
    }
}

// -----------------------------------------------------------------------------
// Program Resolved Context
// -----------------------------------------------------------------------------

/// Program-level resolved context keyed by module id.
#[derive(Clone)]
pub struct ProgramResolvedContext {
    pub entry: ModuleId,
    pub modules: HashMap<ModuleId, ResolvedContext>,
    pub by_path: HashMap<ModulePath, ModuleId>,
    pub edges: HashMap<ModuleId, Vec<ModuleId>>,
    pub top_level_owners: HashMap<NodeId, ModuleId>,
    pub imported_symbol_bindings: HashMap<ModuleId, HashMap<String, ImportedSymbolBinding>>,
}

impl ProgramResolvedContext {
    pub fn entry_module(&self) -> &ResolvedContext {
        self.modules
            .get(&self.entry)
            .expect("resolved entry module should exist")
    }

    pub fn module(&self, id: ModuleId) -> Option<&ResolvedContext> {
        self.modules.get(&id)
    }

    pub fn global_def_id(&self, module_id: ModuleId, def_id: DefId) -> GlobalDefId {
        GlobalDefId::new(module_id, def_id)
    }

    pub fn imported_symbol_binding(
        &self,
        module_id: ModuleId,
        symbol: &str,
    ) -> Option<&ImportedSymbolBinding> {
        self.imported_symbol_bindings
            .get(&module_id)
            .and_then(|bindings| bindings.get(symbol))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportedSymbolBinding {
    pub module_id: ModuleId,
    pub module_path: ModulePath,
    pub callables: Vec<GlobalDefId>,
    pub type_def: Option<GlobalDefId>,
    pub trait_def: Option<GlobalDefId>,
}

impl ImportedSymbolBinding {
    pub fn is_empty(&self) -> bool {
        self.callables.is_empty() && self.type_def.is_none() && self.trait_def.is_none()
    }
}

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
            def_owners: HashMap::new(),
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
    pub def_owners: HashMap<DefId, ModuleId>,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
}

impl ResolvedContext {
    pub fn with_def_owners(mut self, def_owners: HashMap<DefId, ModuleId>) -> Self {
        self.def_owners = def_owners;
        self
    }

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
            def_owners: self.def_owners,
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
    pub def_owners: HashMap<DefId, ModuleId>,
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
    pub def_owners: HashMap<DefId, ModuleId>,
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
            def_owners: self.def_owners,
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
    pub def_owners: HashMap<DefId, ModuleId>,
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
    pub def_owners: HashMap<DefId, ModuleId>,
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
    pub def_owners: HashMap<DefId, ModuleId>,
    pub type_map: TypeMap,
    pub lowering_plans: LoweringPlanMap,
    pub drop_plans: DropPlanMap,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
    pub generic_insts: GenericInstMap,
}

impl AnalyzedContext {}
