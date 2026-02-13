//! Shared compiler stage API.
//!
//! This facade provides a stable, typed entrypoint surface for stage execution
//! so batch compilation and IDE analysis can share the same stage contracts.

use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::core::capsule::ModuleId;
use crate::core::context::{
    ElaborateStageInput, ElaborateStageOutput, NormalizeStageInput, NormalizeStageOutput,
    ResolveStageInput, SemCheckStageInput, SemCheckStageOutput, TypecheckStageInput,
    TypecheckStageOutput,
};
use crate::core::elaborate;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::normalize;
use crate::core::parse::{ParseError, Parser};
use crate::core::resolve::{
    ImportedFacts, ImportedModule, ImportedSymbol, ResolveError, ResolveOutput, attach_def_owners,
    resolve_with_imports_and_symbols_partial,
};
use crate::core::semck::{self, SemCheckError};
use crate::core::tree::NodeId;
use crate::core::tree::NodeIdGen;
use crate::core::tree::parsed::Module as ParsedModule;
use crate::core::typecheck::{
    TypeCheckError, TypecheckOutput, type_check_partial_with_imported_facts,
    type_check_with_imported_facts,
};

#[derive(Debug, Error)]
pub enum ParseModuleError {
    #[error(transparent)]
    Lex(#[from] LexError),
    #[error(transparent)]
    Parse(#[from] ParseError),
}

/// Frontend semantic execution mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrontendPolicy {
    /// Fail-fast semantics used by batch compilation.
    Strict,
    /// Best-effort semantics used by IDE analysis.
    Partial,
}

/// Additional inputs for the resolve stage.
#[derive(Debug, Clone, Default)]
pub struct ResolveInputs {
    pub imported_modules: HashMap<String, ImportedModule>,
    pub imported_symbols: HashMap<String, ImportedSymbol>,
}

/// Unified resolve-stage result for strict/partial policies.
#[derive(Clone, Default)]
pub struct ResolveStageResult {
    pub context: Option<TypecheckStageInput>,
    pub imported_facts: ImportedFacts,
    pub errors: Vec<ResolveError>,
}

impl ResolveStageResult {
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

/// Unified typecheck-stage result for strict/partial policies.
#[derive(Clone, Default)]
pub struct TypecheckStageResult {
    pub context: Option<TypecheckStageOutput>,
    pub errors: Vec<TypeCheckError>,
}

impl TypecheckStageResult {
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

/// Unified semcheck-stage result for strict/partial policies.
#[derive(Clone, Default)]
pub struct SemcheckStageResult {
    pub context: Option<SemCheckStageOutput>,
    pub errors: Vec<SemCheckError>,
    pub poisoned_nodes: HashSet<NodeId>,
}

impl SemcheckStageResult {
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

/// Unified resolve+typecheck result for strict/partial orchestration.
#[derive(Clone, Default)]
pub struct ResolveTypecheckPipelineResult {
    pub resolved_context: Option<TypecheckStageInput>,
    pub imported_facts: ImportedFacts,
    pub resolve_errors: Vec<ResolveError>,
    pub typed_context: Option<TypecheckStageOutput>,
    pub type_errors: Vec<TypeCheckError>,
}

impl ResolveTypecheckPipelineResult {
    pub fn has_errors(&self) -> bool {
        !self.resolve_errors.is_empty() || !self.type_errors.is_empty()
    }
}

/// Shared frontend orchestrator used by both batch driver and analysis batch
/// paths. Runs resolve, applies optional owner metadata, then runs typecheck.
pub fn resolve_typecheck_pipeline_with_policy(
    input: ResolveStageInput,
    resolve_inputs: ResolveInputs,
    top_level_owners: Option<&HashMap<NodeId, ModuleId>>,
    policy: FrontendPolicy,
) -> ResolveTypecheckPipelineResult {
    let resolved = resolve_stage_with_policy(input, resolve_inputs, policy);
    let resolved_context = if let Some(owners) = top_level_owners {
        resolved.context.map(|ctx| attach_def_owners(ctx, owners))
    } else {
        resolved.context
    };

    let typechecked = if let Some(ctx) = resolved_context.clone() {
        typecheck_stage_with_policy(ctx, resolved.imported_facts.clone(), policy)
    } else {
        TypecheckStageResult::default()
    };

    ResolveTypecheckPipelineResult {
        resolved_context,
        imported_facts: resolved.imported_facts,
        resolve_errors: resolved.errors,
        typed_context: typechecked.context,
        type_errors: typechecked.errors,
    }
}

pub fn parse_module_with_id_gen(
    source: &str,
    id_gen: NodeIdGen,
) -> Result<(ParsedModule, NodeIdGen), ParseModuleError> {
    let lexer = Lexer::new(source);
    let tokens = lexer.tokenize().collect::<Result<Vec<Token>, LexError>>()?;
    let mut parser = Parser::new_with_id_gen(&tokens, id_gen);
    let module = parser.parse()?;
    Ok((module, parser.into_id_gen()))
}

pub fn resolve_stage(
    input: ResolveStageInput,
) -> Result<(TypecheckStageInput, ImportedFacts), Vec<ResolveError>> {
    let out = resolve_stage_with_policy(input, ResolveInputs::default(), FrontendPolicy::Strict);
    if out.errors.is_empty() {
        Ok((
            out.context
                .expect("strict resolve should yield context on success"),
            out.imported_facts,
        ))
    } else {
        Err(out.errors)
    }
}

pub fn resolve_stage_with_imports(
    input: ResolveStageInput,
    imported_modules: HashMap<String, ImportedModule>,
    imported_symbols: HashMap<String, ImportedSymbol>,
) -> Result<(TypecheckStageInput, ImportedFacts), Vec<ResolveError>> {
    let out = resolve_stage_with_policy(
        input,
        ResolveInputs {
            imported_modules,
            imported_symbols,
        },
        FrontendPolicy::Strict,
    );
    if out.errors.is_empty() {
        Ok((
            out.context
                .expect("strict resolve should yield context on success"),
            out.imported_facts,
        ))
    } else {
        Err(out.errors)
    }
}

pub fn resolve_stage_partial(
    input: ResolveStageInput,
    imported_modules: HashMap<String, ImportedModule>,
    imported_symbols: HashMap<String, ImportedSymbol>,
) -> ResolveOutput {
    let out = resolve_stage_with_policy(
        input,
        ResolveInputs {
            imported_modules,
            imported_symbols,
        },
        FrontendPolicy::Partial,
    );
    ResolveOutput {
        context: out
            .context
            .expect("partial resolve should always produce a context"),
        imported_facts: out.imported_facts,
        errors: out.errors,
    }
}

pub fn resolve_stage_with_policy(
    input: ResolveStageInput,
    inputs: ResolveInputs,
    policy: FrontendPolicy,
) -> ResolveStageResult {
    let output = resolve_with_imports_and_symbols_partial(
        input,
        inputs.imported_modules,
        inputs.imported_symbols,
    );
    match policy {
        FrontendPolicy::Strict if !output.errors.is_empty() => ResolveStageResult {
            context: None,
            imported_facts: ImportedFacts::default(),
            errors: output.errors,
        },
        FrontendPolicy::Strict | FrontendPolicy::Partial => ResolveStageResult {
            context: Some(output.context),
            imported_facts: output.imported_facts,
            errors: output.errors,
        },
    }
}

pub fn typecheck_stage(
    input: TypecheckStageInput,
    imported_facts: ImportedFacts,
) -> Result<TypecheckStageOutput, Vec<TypeCheckError>> {
    let out = typecheck_stage_with_policy(input, imported_facts, FrontendPolicy::Strict);
    if out.errors.is_empty() {
        Ok(out
            .context
            .expect("strict typecheck should yield context on success"))
    } else {
        Err(out.errors)
    }
}

pub fn typecheck_stage_partial(
    input: TypecheckStageInput,
    imported_facts: ImportedFacts,
) -> TypecheckOutput {
    let out = typecheck_stage_with_policy(input, imported_facts, FrontendPolicy::Partial);
    TypecheckOutput {
        context: out
            .context
            .expect("partial typecheck should always produce a context"),
        errors: out.errors,
    }
}

pub fn typecheck_stage_with_policy(
    input: TypecheckStageInput,
    imported_facts: ImportedFacts,
    policy: FrontendPolicy,
) -> TypecheckStageResult {
    match policy {
        FrontendPolicy::Strict => match type_check_with_imported_facts(input, imported_facts) {
            Ok(context) => TypecheckStageResult {
                context: Some(context),
                errors: Vec::new(),
            },
            Err(errors) => TypecheckStageResult {
                context: None,
                errors,
            },
        },
        FrontendPolicy::Partial => {
            let out = type_check_partial_with_imported_facts(input, imported_facts);
            TypecheckStageResult {
                context: Some(out.context),
                errors: out.errors,
            }
        }
    }
}

pub fn normalize_stage(input: NormalizeStageInput) -> NormalizeStageOutput {
    normalize::normalize(input)
}

pub fn semcheck_stage(
    input: SemCheckStageInput,
) -> Result<SemCheckStageOutput, Vec<SemCheckError>> {
    let out = semcheck_stage_with_policy(input, FrontendPolicy::Strict, &HashSet::new());
    if out.errors.is_empty() {
        Ok(out
            .context
            .expect("strict semcheck should yield context on success"))
    } else {
        Err(out.errors)
    }
}

pub fn semcheck_stage_partial(
    input: SemCheckStageInput,
    upstream_poisoned_nodes: &HashSet<NodeId>,
) -> semck::SemCheckOutput {
    let out = semcheck_stage_with_policy(input, FrontendPolicy::Partial, upstream_poisoned_nodes);
    semck::SemCheckOutput {
        context: out
            .context
            .expect("partial semcheck should always produce a context"),
        errors: out.errors,
        poisoned_nodes: out.poisoned_nodes,
    }
}

pub fn semcheck_stage_with_policy(
    input: SemCheckStageInput,
    policy: FrontendPolicy,
    upstream_poisoned_nodes: &HashSet<NodeId>,
) -> SemcheckStageResult {
    match policy {
        FrontendPolicy::Strict => match semck::sem_check(input) {
            Ok(context) => SemcheckStageResult {
                context: Some(context),
                errors: Vec::new(),
                poisoned_nodes: HashSet::new(),
            },
            Err(errors) => SemcheckStageResult {
                context: None,
                errors,
                poisoned_nodes: HashSet::new(),
            },
        },
        FrontendPolicy::Partial => {
            let out = semck::sem_check_partial(input, upstream_poisoned_nodes);
            SemcheckStageResult {
                context: Some(out.context),
                errors: out.errors,
                poisoned_nodes: out.poisoned_nodes,
            }
        }
    }
}

pub fn elaborate_stage(input: ElaborateStageInput) -> ElaborateStageOutput {
    elaborate::elaborate(input)
}

#[cfg(test)]
#[path = "../../tests/core/t_api.rs"]
mod tests;

#[cfg(test)]
#[path = "../../tests/core/t_frontend_parity.rs"]
mod tests_frontend_parity;
