use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;

use crate::core::ast::{Module, NodeId, NodeIdGen};
use crate::core::capsule::{ModuleId, ModulePath};
use crate::core::codegen_names::CodegenNameTable;
use crate::core::linear::LinearIndex;
use crate::core::plans::{DropPlanMap, LinearMachinePlanMap, LoweringPlanMap};
use crate::core::resolve::{DefId, DefTable};
use crate::core::semck::closure::capture::ClosureCapture;
use crate::core::symbol_id::SymbolIdTable;
use crate::core::typecheck::type_map::{CallSigMap, GenericInstMap, TypeMap};

#[derive(Debug, Clone)]
pub struct ResolvedTables {
    pub def_table: DefTable,
    pub module_path: Option<ModulePath>,
    pub def_owners: HashMap<DefId, ModuleId>,
    pub symbol_ids: SymbolIdTable,
    pub symbols: CodegenNameTable,
    pub node_id_gen: NodeIdGen,
    pub linear_index: LinearIndex,
}

#[derive(Debug, Clone)]
pub struct TypedTables {
    pub resolved: ResolvedTables,
    pub type_map: TypeMap,
    pub call_sigs: CallSigMap,
    pub generic_insts: GenericInstMap,
}

impl Deref for TypedTables {
    type Target = ResolvedTables;

    fn deref(&self) -> &Self::Target {
        &self.resolved
    }
}

impl DerefMut for TypedTables {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.resolved
    }
}

#[derive(Debug, Clone)]
pub struct SemFacts {
    pub implicit_moves: HashSet<NodeId>,
    pub init_assigns: HashSet<NodeId>,
    pub full_init_assigns: HashSet<NodeId>,
    pub closure_captures: HashMap<DefId, Vec<ClosureCapture>>,
}

#[derive(Debug, Clone)]
pub struct SemCheckedPayload {
    pub typed: TypedTables,
    pub implicit_moves: HashSet<NodeId>,
    pub init_assigns: HashSet<NodeId>,
    pub full_init_assigns: HashSet<NodeId>,
    pub closure_captures: HashMap<DefId, Vec<ClosureCapture>>,
}

impl SemCheckedPayload {
    pub fn sem_facts(&self) -> SemFacts {
        SemFacts {
            implicit_moves: self.implicit_moves.clone(),
            init_assigns: self.init_assigns.clone(),
            full_init_assigns: self.full_init_assigns.clone(),
            closure_captures: self.closure_captures.clone(),
        }
    }
}

impl Deref for SemCheckedPayload {
    type Target = TypedTables;

    fn deref(&self) -> &Self::Target {
        &self.typed
    }
}

impl DerefMut for SemCheckedPayload {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.typed
    }
}

#[derive(Debug, Clone)]
pub struct SemanticPlans {
    pub lowering_plans: LoweringPlanMap,
    pub drop_plans: DropPlanMap,
    pub linear_machine_plans: LinearMachinePlanMap,
}

#[derive(Debug, Clone)]
pub struct SemanticPayload {
    pub typed: TypedTables,
    pub lowering_plans: LoweringPlanMap,
    pub drop_plans: DropPlanMap,
    pub linear_machine_plans: LinearMachinePlanMap,
}

impl SemanticPayload {
    pub fn plans(&self) -> SemanticPlans {
        SemanticPlans {
            lowering_plans: self.lowering_plans.clone(),
            drop_plans: self.drop_plans.clone(),
            linear_machine_plans: self.linear_machine_plans.clone(),
        }
    }
}

impl Deref for SemanticPayload {
    type Target = TypedTables;

    fn deref(&self) -> &Self::Target {
        &self.typed
    }
}

impl DerefMut for SemanticPayload {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.typed
    }
}

#[derive(Clone)]
pub struct ParsedContext {
    pub module: Module,
    pub node_id_gen: NodeIdGen,
    pub source_path: Option<PathBuf>,
    pub module_path: Option<ModulePath>,
    pub linear_index: LinearIndex,
}

/// Stage contract alias: resolve input.
pub type ResolveStageInput = ParsedContext;

impl ParsedContext {
    pub fn new(module: Module, node_id_gen: NodeIdGen) -> Self {
        Self {
            module,
            node_id_gen,
            source_path: None,
            module_path: None,
            linear_index: LinearIndex::default(),
        }
    }

    pub fn with_source_path(mut self, source_path: PathBuf) -> Self {
        self.source_path = Some(source_path);
        self
    }

    pub fn with_module_path(mut self, module_path: ModulePath) -> Self {
        self.module_path = Some(module_path);
        self
    }

    pub fn with_def_table(self, mut def_table: DefTable) -> ResolvedContext {
        let ParsedContext {
            module,
            node_id_gen,
            source_path,
            module_path,
            linear_index,
        } = self;

        if def_table.source_path().is_none() {
            def_table.set_source_path(source_path);
        }

        let symbols = CodegenNameTable::new(&module, &def_table);
        let symbol_ids = module_path
            .as_ref()
            .map(|module_path| SymbolIdTable::from_module(module_path, &module, &def_table))
            .unwrap_or_default();

        ResolvedContext {
            module,
            payload: ResolvedTables {
                def_table,
                module_path,
                def_owners: HashMap::new(),
                symbol_ids,
                symbols,
                node_id_gen,
                linear_index,
            },
        }
    }
}

#[derive(Clone)]
pub struct ResolvedContext {
    pub module: Module,
    pub payload: ResolvedTables,
}

/// Stage contract alias: resolve output, typecheck input.
pub type ResolveStageOutput = ResolvedContext;
/// Stage contract alias: typecheck input.
pub type TypecheckStageInput = ResolvedContext;

impl ResolvedContext {
    pub fn with_def_owners(mut self, def_owners: HashMap<DefId, ModuleId>) -> Self {
        self.payload.def_owners = def_owners;
        self
    }

    pub fn with_type_map(
        self,
        type_map: TypeMap,
        call_sigs: CallSigMap,
        generic_insts: GenericInstMap,
    ) -> TypeCheckedContext {
        let ResolvedContext { module, payload } = self;
        TypeCheckedContext {
            module,
            payload: TypedTables {
                resolved: payload,
                type_map,
                call_sigs,
                generic_insts,
            },
        }
    }
}

impl Deref for ResolvedContext {
    type Target = ResolvedTables;

    fn deref(&self) -> &Self::Target {
        &self.payload
    }
}

impl DerefMut for ResolvedContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.payload
    }
}

#[derive(Clone)]
pub struct TypeCheckedContext {
    pub module: Module,
    pub payload: TypedTables,
}

/// Stage contract alias: typecheck output, semck input.
pub type TypecheckStageOutput = TypeCheckedContext;
/// Stage contract alias: semck input.
pub type SemCheckStageInput = TypeCheckedContext;

impl Deref for TypeCheckedContext {
    type Target = TypedTables;

    fn deref(&self) -> &Self::Target {
        &self.payload
    }
}

impl DerefMut for TypeCheckedContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.payload
    }
}

#[derive(Clone)]
/// Internal semcheck intermediate after typed->normalized lowering.
pub struct NormalizedContext {
    pub module: Module,
    pub payload: TypedTables,
}

/// Internal semck context after normalization.
pub type SemCheckNormalizedContext = NormalizedContext;

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
            payload: SemCheckedPayload {
                typed: self.payload,
                implicit_moves,
                init_assigns,
                full_init_assigns,
                closure_captures,
            },
        }
    }
}

impl Deref for NormalizedContext {
    type Target = TypedTables;

    fn deref(&self) -> &Self::Target {
        &self.payload
    }
}

impl DerefMut for NormalizedContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.payload
    }
}

#[derive(Debug, Clone)]
pub struct SemanticCheckedContext {
    pub module: Module,
    pub payload: SemCheckedPayload,
}

/// Stage contract alias: semck output, elaborate input.
pub type SemCheckStageOutput = SemanticCheckedContext;
/// Stage contract alias: elaborate input.
pub type ElaborateStageInput = SemanticCheckedContext;

impl Deref for SemanticCheckedContext {
    type Target = SemCheckedPayload;

    fn deref(&self) -> &Self::Target {
        &self.payload
    }
}

impl DerefMut for SemanticCheckedContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.payload
    }
}

#[derive(Debug, Clone)]
pub struct SemanticContext {
    pub module: Module,
    pub payload: SemanticPayload,
}

/// Stage contract alias: elaborate output, NRVO input.
pub type ElaborateStageOutput = SemanticContext;
/// Stage contract alias: NRVO input.
pub type NrvoStageInput = SemanticContext;

impl Deref for SemanticContext {
    type Target = SemanticPayload;

    fn deref(&self) -> &Self::Target {
        &self.payload
    }
}

impl DerefMut for SemanticContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.payload
    }
}

#[derive(Debug, Clone)]
pub struct AnalyzedContext {
    pub module: Module,
    pub payload: SemanticPayload,
}

/// Stage contract alias: NRVO output.
pub type NrvoStageOutput = AnalyzedContext;

impl Deref for AnalyzedContext {
    type Target = SemanticPayload;

    fn deref(&self) -> &Self::Target {
        &self.payload
    }
}

impl DerefMut for AnalyzedContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.payload
    }
}

impl AnalyzedContext {}
