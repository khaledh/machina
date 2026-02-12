//! Type-check engine and shared phase state.
//!
//! `TypecheckEngine` wires pass execution order and owns:
//! - immutable collected environment (`TcEnv`),
//! - mutable per-phase outputs (`TcState`),
//! - solver-owned type-variable store.
//!
//! Each pass is kept as a small module and executed in fixed order by `run`.

use std::collections::HashMap;
use std::collections::HashSet;

use crate::core::context::{ResolvedContext, TypeCheckedContext};
use crate::core::diag::Span;
use crate::core::resolve::{DefId, ImportedFacts};
use crate::core::tree::ParamMode;
use crate::core::typecheck::errors::TypeCheckError;
use crate::core::types::{TyVarId, Type};

use super::{collect, constraints, finalize, solver, typesys, validate};

/// Immutable shared environment populated by early passes.
#[derive(Clone)]
pub(crate) struct TcEnv {
    pub(crate) context: ResolvedContext,
    pub(crate) imported_facts: ImportedFacts,
    pub(crate) type_symbols: HashMap<String, DefId>,
    pub(crate) type_defs: HashMap<String, Type>,
    pub(crate) trait_sigs: HashMap<String, CollectedTraitSig>,
    pub(crate) trait_impls: HashMap<String, HashSet<String>>,
    pub(crate) func_sigs: HashMap<String, Vec<CollectedCallableSig>>,
    pub(crate) method_sigs: HashMap<String, HashMap<String, Vec<CollectedCallableSig>>>,
    pub(crate) property_sigs: HashMap<String, HashMap<String, CollectedPropertySig>>,
    pub(crate) generic_envs: HashMap<DefId, HashMap<DefId, TyVarId>>,
}

/// Mutable state progressively filled by passes.
#[derive(Debug, Default)]
pub(crate) struct TcState {
    pub(crate) diags: Vec<TypeCheckError>,
    pub(crate) phase: Option<TcPhase>,
    pub(crate) constrain: constraints::ConstrainOutput,
    pub(crate) solve: solver::SolveOutput,
    pub(crate) finalize: Option<finalize::FinalizeOutput>,
    #[allow(dead_code)]
    pub(crate) diag_ctx: DiagCtx,
}

/// Lightweight context attached to diagnostics during collection/solving.
#[derive(Debug, Default)]
#[allow(dead_code)]
pub(crate) struct DiagCtx {
    pub(crate) current_span: Option<Span>,
    pub(crate) current_name: Option<String>,
}

/// Type-check pipeline phases.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TcPhase {
    Collect,
    Constrain,
    Solve,
    Validate,
    Finalize,
}

#[derive(Clone)]
pub struct TypecheckOutput {
    pub context: TypeCheckedContext,
    pub errors: Vec<TypeCheckError>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CollectedParamSig {
    pub(crate) name: String,
    pub(crate) ty: Type,
    pub(crate) mode: ParamMode,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CollectedCallableSig {
    pub(crate) def_id: DefId,
    pub(crate) params: Vec<CollectedParamSig>,
    pub(crate) ret_ty: Type,
    pub(crate) type_param_count: usize,
    pub(crate) type_param_bounds: Vec<Option<String>>,
    pub(crate) self_mode: Option<ParamMode>,
    pub(crate) impl_trait: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CollectedTraitMethodSig {
    pub(crate) name: String,
    pub(crate) params: Vec<CollectedParamSig>,
    pub(crate) ret_ty: Type,
    pub(crate) type_param_count: usize,
    pub(crate) type_param_bounds: Vec<Option<String>>,
    pub(crate) self_mode: ParamMode,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CollectedTraitPropertySig {
    pub(crate) name: String,
    pub(crate) ty: Type,
    pub(crate) has_get: bool,
    pub(crate) has_set: bool,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CollectedTraitSig {
    pub(crate) def_id: DefId,
    pub(crate) methods: HashMap<String, CollectedTraitMethodSig>,
    pub(crate) properties: HashMap<String, CollectedTraitPropertySig>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CollectedPropertySig {
    pub(crate) ty: Type,
    pub(crate) getter: Option<DefId>,
    pub(crate) setter: Option<DefId>,
}

pub(crate) fn property_owner_name(ty: &Type) -> Option<&str> {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => Some(name.as_str()),
        Type::String => Some("string"),
        _ => None,
    }
}

pub(crate) fn lookup_property<'a>(
    property_sigs: &'a HashMap<String, HashMap<String, CollectedPropertySig>>,
    owner_ty: &Type,
    field: &str,
) -> Option<&'a CollectedPropertySig> {
    let owner = property_owner_name(owner_ty)?;
    property_sigs.get(owner).and_then(|props| props.get(field))
}

pub(crate) struct TypecheckEngine {
    env: TcEnv,
    state: TcState,
    #[allow(dead_code)]
    type_vars: typesys::TypeVarStore,
}

impl TypecheckEngine {
    pub(crate) fn new(context: ResolvedContext, imported_facts: ImportedFacts) -> Self {
        Self {
            env: TcEnv {
                context,
                imported_facts,
                type_symbols: HashMap::new(),
                type_defs: HashMap::new(),
                trait_sigs: HashMap::new(),
                trait_impls: HashMap::new(),
                func_sigs: HashMap::new(),
                method_sigs: HashMap::new(),
                property_sigs: HashMap::new(),
                generic_envs: HashMap::new(),
            },
            state: TcState::default(),
            type_vars: typesys::TypeVarStore::default(),
        }
    }

    pub(crate) fn run(self) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
        let mut this = self;
        // Strict pipeline: stop at first failing phase.
        this.run_phase(TcPhase::Collect, collect::run)?;
        this.run_phase(TcPhase::Constrain, constraints::run)?;
        this.run_phase(TcPhase::Solve, solver::run)?;
        this.run_phase(TcPhase::Validate, validate::run)?;
        this.run_phase(TcPhase::Finalize, finalize::run)?;

        finalize::materialize(this)
    }

    pub(crate) fn run_partial(mut self) -> TypecheckOutput {
        // Keep strict phase order, but continue through failures so analysis
        // can still materialize a best-effort typed context.
        let _ = self.run_phase(TcPhase::Collect, collect::run);
        let _ = self.run_phase(TcPhase::Constrain, constraints::run);
        let _ = self.run_phase(TcPhase::Solve, solver::run);
        let _ = self.run_phase(TcPhase::Validate, validate::run);
        self.state.phase = Some(TcPhase::Finalize);
        finalize::run_partial(&mut self);

        let errors = self.state.diags.clone();
        let context = finalize::materialize(self)
            .expect("finalize::run_partial must prepare materialization output");
        TypecheckOutput { context, errors }
    }

    fn run_phase(
        &mut self,
        phase: TcPhase,
        run: fn(&mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>>,
    ) -> Result<(), Vec<TypeCheckError>> {
        self.state.phase = Some(phase);
        run(self)
    }

    #[allow(dead_code)]
    pub(crate) fn env(&self) -> &TcEnv {
        &self.env
    }

    pub(crate) fn env_mut(&mut self) -> &mut TcEnv {
        &mut self.env
    }

    pub(crate) fn state(&self) -> &TcState {
        &self.state
    }

    pub(crate) fn state_mut(&mut self) -> &mut TcState {
        &mut self.state
    }

    pub(crate) fn type_vars_mut(&mut self) -> &mut typesys::TypeVarStore {
        &mut self.type_vars
    }

    pub(crate) fn type_vars(&self) -> &typesys::TypeVarStore {
        &self.type_vars
    }

    pub(crate) fn context(&self) -> &ResolvedContext {
        &self.env.context
    }
}
