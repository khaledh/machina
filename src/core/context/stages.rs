use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use std::path::PathBuf;

use crate::core::capsule::ModuleId;
use crate::core::diag::Span;
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

#[derive(Debug, Clone)]
pub struct ResolvedTables {
    pub def_table: DefTable,
    pub def_owners: HashMap<DefId, ModuleId>,
    pub symbols: SymbolTable,
    pub node_id_gen: NodeIdGen,
    pub typestate_role_impls: Vec<TypestateRoleImplBinding>,
}

#[derive(Debug, Clone)]
pub struct TypestateRoleImplBinding {
    pub node_id: NodeId,
    pub typestate_name: String,
    pub path: Vec<String>,
    pub role_def_id: Option<DefId>,
    pub span: Span,
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
}

#[derive(Debug, Clone)]
pub struct SemanticPayload {
    pub typed: TypedTables,
    pub lowering_plans: LoweringPlanMap,
    pub drop_plans: DropPlanMap,
}

impl SemanticPayload {
    pub fn plans(&self) -> SemanticPlans {
        SemanticPlans {
            lowering_plans: self.lowering_plans.clone(),
            drop_plans: self.drop_plans.clone(),
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
    pub module: ParsedModule,
    pub node_id_gen: NodeIdGen,
    pub source_path: Option<PathBuf>,
}

/// Stage contract alias: resolve input.
pub type ResolveStageInput = ParsedContext;

impl ParsedContext {
    pub fn new(module: ParsedModule, node_id_gen: NodeIdGen) -> Self {
        Self {
            module,
            node_id_gen,
            source_path: None,
        }
    }

    pub fn with_source_path(mut self, source_path: PathBuf) -> Self {
        self.source_path = Some(source_path);
        self
    }

    pub fn with_def_table(
        self,
        mut def_table: DefTable,
        module: ResolvedModule,
    ) -> ResolvedContext {
        if def_table.source_path().is_none() {
            def_table.set_source_path(self.source_path.clone());
        }
        let symbols = SymbolTable::new(&module, &def_table);
        ResolvedContext {
            module,
            payload: ResolvedTables {
                def_table,
                def_owners: HashMap::new(),
                symbols,
                node_id_gen: self.node_id_gen,
                typestate_role_impls: Vec::new(),
            },
        }
    }
}

#[derive(Clone)]
pub struct ResolvedContext {
    pub module: ResolvedModule,
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
        module: TypedModule,
    ) -> TypeCheckedContext {
        TypeCheckedContext {
            module,
            payload: TypedTables {
                resolved: self.payload,
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
    pub module: TypedModule,
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
    pub module: NormalizedModule,
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
    pub module: NormalizedModule,
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
    pub module: SemanticModule,
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
    pub module: SemanticModule,
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
