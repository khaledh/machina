//! Batch compile compatibility facade for query-backed frontend analysis.
//!
//! This keeps backend/lowering linear while routing parse/resolve/typecheck
//! products through `AnalysisDb` query keys.

use std::collections::HashMap;

use crate::analysis::db::AnalysisDb;
use crate::analysis::query::{QueryCancelled, QueryKey, QueryKind};
use crate::analysis::results::{ResolvedModuleResult, TypedModuleResult};
use crate::capsule::ModuleId;
use crate::context::ParsedContext;
use crate::resolve::{ResolveError, attach_def_owners, resolve};
use crate::tree::NodeId;
use crate::typecheck::{TypeCheckError, type_check};

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
        let resolved = resolve(parsed).map(|ctx| attach_def_owners(ctx, &top_level_owners));
        let result = resolved.map(|ctx| ResolvedModuleResult::from_context(module_id, ctx));
        Ok(result)
    })?;
    resolved.map_err(BatchQueryError::Resolve)
}

pub fn query_typecheck(
    db: &mut AnalysisDb,
    module_id: ModuleId,
    revision: u64,
    resolved: ResolvedModuleResult,
) -> Result<TypedModuleResult, BatchQueryError> {
    let key = QueryKey::new(QueryKind::TypecheckModule, module_id, revision);
    let typed = db.execute_query(key, move |_rt| {
        let typed = type_check(resolved.into_context())
            .map(|ctx| TypedModuleResult::from_context(module_id, ctx));
        Ok(typed)
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

    let resolved = query_resolve(db, module_id, revision, parsed, top_level_owners)?;
    let typed = query_typecheck(db, module_id, revision, resolved.clone())?;
    Ok((resolved, typed))
}

#[cfg(test)]
#[path = "../../tests/analysis/t_batch.rs"]
mod tests;
