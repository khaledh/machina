//! Batch compile compatibility facade for query-backed frontend analysis.
//!
//! This keeps backend/lowering linear while routing parse/resolve/typecheck
//! products through `AnalysisDb` query keys.

use std::collections::HashMap;

use crate::core::api::{
    FrontendPolicy, ResolveInputs, resolve_stage_with_policy, typecheck_stage_with_policy,
};
use crate::core::capsule::ModuleId;
use crate::core::context::ParsedContext;
use crate::core::resolve::{ImportedFacts, ResolveError, attach_def_owners};
use crate::core::tree::NodeId;
use crate::core::typecheck::TypeCheckError;
use crate::services::analysis::db::AnalysisDb;
use crate::services::analysis::query::{QueryCancelled, QueryKey, QueryKind};
use crate::services::analysis::results::{ResolvedModuleResult, TypedModuleResult};

#[derive(Debug)]
pub enum BatchQueryError {
    Cancelled,
    Resolve(Vec<ResolveError>),
    TypeCheck(Vec<TypeCheckError>),
}

impl From<QueryCancelled> for BatchQueryError {
    fn from(_: QueryCancelled) -> Self {
        BatchQueryError::Cancelled
    }
}

pub fn query_resolve(
    db: &mut AnalysisDb,
    module_id: ModuleId,
    revision: u64,
    parsed: ParsedContext,
    top_level_owners: HashMap<NodeId, ModuleId>,
) -> Result<ResolvedModuleResult, BatchQueryError> {
    let key = QueryKey::new(QueryKind::ResolveModule, module_id, revision);
    let resolved = db.execute_query(key, move |_rt| {
        let resolved =
            resolve_stage_with_policy(parsed, ResolveInputs::default(), FrontendPolicy::Strict);
        if resolved.has_errors() {
            return Ok(Err(resolved.errors));
        }
        let Some(ctx) = resolved.context else {
            return Ok(Err(Vec::new()));
        };
        let ctx = attach_def_owners(ctx, &top_level_owners);
        Ok(Ok(ResolvedModuleResult::from_context(module_id, ctx)))
    })?;
    resolved.map_err(BatchQueryError::Resolve)
}

pub fn query_typecheck(
    db: &mut AnalysisDb,
    module_id: ModuleId,
    revision: u64,
    resolved: ResolvedModuleResult,
) -> Result<TypedModuleResult, BatchQueryError> {
    query_typecheck_with_imported_facts(db, module_id, revision, resolved, ImportedFacts::default())
}

pub fn query_typecheck_with_imported_facts(
    db: &mut AnalysisDb,
    module_id: ModuleId,
    revision: u64,
    resolved: ResolvedModuleResult,
    imported_facts: ImportedFacts,
) -> Result<TypedModuleResult, BatchQueryError> {
    let key = QueryKey::new(QueryKind::TypecheckModule, module_id, revision);
    let typed = db.execute_query(key, move |_rt| {
        let typed = typecheck_stage_with_policy(
            resolved.into_context(),
            imported_facts,
            FrontendPolicy::Strict,
        );
        if typed.has_errors() {
            return Ok(Err(typed.errors));
        }
        let Some(ctx) = typed.context else {
            return Ok(Err(Vec::new()));
        };
        Ok(Ok(TypedModuleResult::from_context(module_id, ctx)))
    })?;
    typed.map_err(BatchQueryError::TypeCheck)
}

pub fn query_parse_resolve_typecheck(
    db: &mut AnalysisDb,
    module_id: ModuleId,
    revision: u64,
    parsed: ParsedContext,
    top_level_owners: HashMap<NodeId, ModuleId>,
) -> Result<(ResolvedModuleResult, TypedModuleResult), BatchQueryError> {
    // Parse query key is still useful for cache shape/ordering even though
    // parsed AST currently comes from the existing parser entrypoint.
    let parse_key = QueryKey::new(QueryKind::ParseModule, module_id, revision);
    let parsed = db.execute_query(parse_key, move |_rt| Ok(parsed))?;
    let pipeline = crate::core::api::resolve_typecheck_pipeline_with_policy(
        parsed,
        ResolveInputs::default(),
        Some(&top_level_owners),
        FrontendPolicy::Strict,
    );
    if !pipeline.resolve_errors.is_empty() {
        return Err(BatchQueryError::Resolve(pipeline.resolve_errors));
    }
    if !pipeline.type_errors.is_empty() {
        return Err(BatchQueryError::TypeCheck(pipeline.type_errors));
    }
    let resolved = ResolvedModuleResult::from_context(
        module_id,
        pipeline
            .resolved_context
            .expect("strict resolve should produce context on success"),
    );
    let typed = TypedModuleResult::from_context(
        module_id,
        pipeline
            .typed_context
            .expect("strict typecheck should produce context on success"),
    );
    Ok((resolved, typed))
}

#[cfg(test)]
#[path = "../../tests/analysis/t_batch.rs"]
mod tests;
