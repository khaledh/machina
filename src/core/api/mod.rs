//! Shared compiler stage API.
//!
//! This facade provides a stable, typed entrypoint surface for stage execution
//! so batch compilation and IDE analysis can share the same stage contracts.

use std::collections::{HashMap, HashSet};

use thiserror::Error;

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
    ImportedFacts, ImportedModule, ImportedSymbol, ResolveError, ResolveOutput,
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
    let output = resolve_with_imports_and_symbols_partial(input, HashMap::new(), HashMap::new());
    if output.errors.is_empty() {
        Ok((output.context, output.imported_facts))
    } else {
        Err(output.errors)
    }
}

pub fn resolve_stage_with_imports(
    input: ResolveStageInput,
    imported_modules: HashMap<String, ImportedModule>,
    imported_symbols: HashMap<String, ImportedSymbol>,
) -> Result<(TypecheckStageInput, ImportedFacts), Vec<ResolveError>> {
    let output =
        resolve_with_imports_and_symbols_partial(input, imported_modules, imported_symbols);
    if output.errors.is_empty() {
        Ok((output.context, output.imported_facts))
    } else {
        Err(output.errors)
    }
}

pub fn resolve_stage_partial(
    input: ResolveStageInput,
    imported_modules: HashMap<String, ImportedModule>,
    imported_symbols: HashMap<String, ImportedSymbol>,
) -> ResolveOutput {
    resolve_with_imports_and_symbols_partial(input, imported_modules, imported_symbols)
}

pub fn typecheck_stage(
    input: TypecheckStageInput,
    imported_facts: ImportedFacts,
) -> Result<TypecheckStageOutput, Vec<TypeCheckError>> {
    type_check_with_imported_facts(input, imported_facts)
}

pub fn typecheck_stage_partial(
    input: TypecheckStageInput,
    imported_facts: ImportedFacts,
) -> TypecheckOutput {
    type_check_partial_with_imported_facts(input, imported_facts)
}

pub fn normalize_stage(input: NormalizeStageInput) -> NormalizeStageOutput {
    normalize::normalize(input)
}

pub fn semcheck_stage(
    input: SemCheckStageInput,
) -> Result<SemCheckStageOutput, Vec<SemCheckError>> {
    semck::sem_check(input)
}

pub fn semcheck_stage_partial(
    input: SemCheckStageInput,
    upstream_poisoned_nodes: &HashSet<NodeId>,
) -> semck::SemCheckOutput {
    semck::sem_check_partial(input, upstream_poisoned_nodes)
}

pub fn elaborate_stage(input: ElaborateStageInput) -> ElaborateStageOutput {
    elaborate::elaborate(input)
}
