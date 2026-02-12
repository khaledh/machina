//! Query runtime primitives for incremental analysis.
//!
//! This module provides a minimal in-memory query runtime used by future
//! `AnalysisDb` integration:
//! - stable query keys,
//! - memoized results,
//! - direct dependency/dependent tracking,
//! - cooperative cancellation checks.

use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::core::capsule::ModuleId;

/// Coarse query families used by the initial analysis runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum QueryKind {
    ParseModule,
    ResolveModule,
    TypecheckModule,
    SemcheckModule,
    Diagnostics,
    LookupState,
}

/// Query identity keyed by semantic stage, module, and revision snapshot.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct QueryKey {
    pub kind: QueryKind,
    pub module: ModuleId,
    pub revision: u64,
}

impl QueryKey {
    pub fn new(kind: QueryKind, module: ModuleId, revision: u64) -> Self {
        Self {
            kind,
            module,
            revision,
        }
    }
}

/// Cooperative cancellation signal shared across query evaluation.
#[derive(Debug, Clone, Default)]
pub struct CancellationToken {
    cancelled: Arc<AtomicBool>,
}

impl CancellationToken {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn cancel(&self) {
        self.cancelled.store(true, Ordering::Relaxed);
    }

    pub fn reset(&self) {
        self.cancelled.store(false, Ordering::Relaxed);
    }

    pub fn is_cancelled(&self) -> bool {
        self.cancelled.load(Ordering::Relaxed)
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CacheStats {
    pub hits: usize,
    pub misses: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct QueryCancelled;

pub type QueryResult<T> = Result<T, QueryCancelled>;

/// In-memory runtime for query memoization + dependency bookkeeping.
#[derive(Default)]
pub struct QueryRuntime {
    cache: HashMap<QueryKey, Arc<dyn Any + Send + Sync>>,
    deps: HashMap<QueryKey, HashSet<QueryKey>>,
    reverse_deps: HashMap<QueryKey, HashSet<QueryKey>>,
    active_stack: Vec<QueryKey>,
    stats: CacheStats,
    cancel: CancellationToken,
}

impl QueryRuntime {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_cancellation_token(cancel: CancellationToken) -> Self {
        Self {
            cancel,
            ..Self::default()
        }
    }

    pub fn cancellation_token(&self) -> CancellationToken {
        self.cancel.clone()
    }

    pub fn set_cancellation_token(&mut self, cancel: CancellationToken) {
        self.cancel = cancel;
    }

    pub fn check_cancelled(&self) -> QueryResult<()> {
        if self.cancel.is_cancelled() {
            Err(QueryCancelled)
        } else {
            Ok(())
        }
    }

    pub fn cache_stats(&self) -> CacheStats {
        self.stats
    }

    pub fn clear_stats(&mut self) {
        self.stats = CacheStats::default();
    }

    /// Returns direct dependencies (`parent -> children`) for a query key.
    pub fn dependencies_of(&self, key: QueryKey) -> HashSet<QueryKey> {
        self.deps.get(&key).cloned().unwrap_or_default()
    }

    /// Returns direct dependents (`child -> parents`) for a query key.
    pub fn dependents_of(&self, key: QueryKey) -> HashSet<QueryKey> {
        self.reverse_deps.get(&key).cloned().unwrap_or_default()
    }

    /// Invalidate a key and all direct/indirect dependents.
    pub fn invalidate(&mut self, root: QueryKey) {
        let mut stack = vec![root];
        let mut seen = HashSet::new();
        while let Some(key) = stack.pop() {
            if !seen.insert(key) {
                continue;
            }
            if let Some(parents) = self.reverse_deps.remove(&key) {
                for parent in parents {
                    stack.push(parent);
                }
            }
            if let Some(children) = self.deps.remove(&key) {
                for child in children {
                    if let Some(parents) = self.reverse_deps.get_mut(&child) {
                        parents.remove(&key);
                        if parents.is_empty() {
                            self.reverse_deps.remove(&child);
                        }
                    }
                }
            }
            self.cache.remove(&key);
        }
    }

    /// Invalidate all cached and dependency-tracked queries whose key belongs
    /// to any module in `modules`, including their transitive dependents.
    pub fn invalidate_modules(&mut self, modules: &HashSet<ModuleId>) {
        let mut roots = HashSet::new();
        for key in self.cache.keys() {
            if modules.contains(&key.module) {
                roots.insert(*key);
            }
        }
        for key in self.deps.keys() {
            if modules.contains(&key.module) {
                roots.insert(*key);
            }
        }
        for key in self.reverse_deps.keys() {
            if modules.contains(&key.module) {
                roots.insert(*key);
            }
        }
        for key in roots {
            self.invalidate(key);
        }
    }

    /// Execute query `key`, returning cached value when present.
    ///
    /// If called while another query is active, the active query is recorded
    /// as depending on `key`.
    pub fn execute<T, F>(&mut self, key: QueryKey, compute: F) -> QueryResult<T>
    where
        T: Clone + Send + Sync + 'static,
        F: FnOnce(&mut Self) -> QueryResult<T>,
    {
        self.check_cancelled()?;
        self.link_parent_dependency(key);
        if let Some(cached) = self.cache.get(&key) {
            self.stats.hits += 1;
            let value = cached
                .downcast_ref::<T>()
                .expect("query cache type mismatch for key");
            return Ok(value.clone());
        }

        self.stats.misses += 1;
        self.active_stack.push(key);
        let computed = compute(self);
        let popped = self.active_stack.pop();
        debug_assert_eq!(popped, Some(key));

        match computed {
            Ok(value) => {
                self.cache.insert(key, Arc::new(value.clone()));
                Ok(value)
            }
            Err(err) => Err(err),
        }
    }

    fn link_parent_dependency(&mut self, child: QueryKey) {
        let Some(parent) = self.active_stack.last().copied() else {
            return;
        };
        self.deps.entry(parent).or_default().insert(child);
        self.reverse_deps.entry(child).or_default().insert(parent);
    }
}

#[cfg(test)]
#[path = "../../../tests/analysis/t_query_runtime.rs"]
mod tests;
