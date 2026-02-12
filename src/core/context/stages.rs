use std::collections::{HashMap, HashSet};

use crate::core::capsule::ModuleId;
use crate::core::resolve::{DefId, DefTable};
use crate::core::semck::closure::capture::ClosureCapture;
use crate::core::symtab::SymbolTable;
use crate::core::tree::normalized::Module as NormalizedModule;
use crate::core::tree::parsed::Module as ParsedModule;
use crate::core::tree::resolved::Module as ResolvedModule;
use crate::core::tree::semantic::Module as SemanticModule;
use crate::core::tree::semantic::{DropPlanMap, LoweringPlanMap};
use crate::core::tree::typed::Module as TypedModule;
use crate::core::tree::{NodeId, NodeIdGen};
use crate::core::typecheck::type_map::{CallSigMap, GenericInstMap, TypeMap};

#[derive(Clone)]
pub struct ParsedContext {
    pub module: ParsedModule,
    pub node_id_gen: NodeIdGen,
}

/// Stage contract alias: resolve input.
pub type ResolveStageInput = ParsedContext;

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

#[derive(Clone)]
pub struct ResolvedContext {
    pub module: ResolvedModule,
    pub def_table: DefTable,
    pub def_owners: HashMap<DefId, ModuleId>,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
}

/// Stage contract alias: resolve output, typecheck input.
pub type ResolveStageOutput = ResolvedContext;
/// Stage contract alias: typecheck input.
pub type TypecheckStageInput = ResolvedContext;

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

/// Stage contract alias: typecheck output, normalize input.
pub type TypecheckStageOutput = TypeCheckedContext;
/// Stage contract alias: normalize input.
pub type NormalizeStageInput = TypeCheckedContext;

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

/// Stage contract alias: normalize output, semck input.
pub type NormalizeStageOutput = NormalizedContext;
/// Stage contract alias: semck input.
pub type SemCheckStageInput = NormalizedContext;

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

/// Stage contract alias: semck output, elaborate input.
pub type SemCheckStageOutput = SemanticCheckedContext;
/// Stage contract alias: elaborate input.
pub type ElaborateStageInput = SemanticCheckedContext;

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

/// Stage contract alias: elaborate output, NRVO input.
pub type ElaborateStageOutput = SemanticContext;
/// Stage contract alias: NRVO input.
pub type NrvoStageInput = SemanticContext;

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

/// Stage contract alias: NRVO output.
pub type NrvoStageOutput = AnalyzedContext;

impl AnalyzedContext {}
