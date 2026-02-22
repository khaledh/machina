//! Scope-oriented completion collection.
//!
//! This module composes global and local scope providers for identifier
//! completion and exposes callable-boundary lookup used by site classification.

mod global;
mod locals;

use std::collections::HashMap;

use crate::core::diag::Position;
use crate::core::resolve::DefId;
use crate::services::analysis::results::CompletionItem;

use global::global_scope;
use locals::collect_local_scopes;

pub(super) fn scope_completions(
    resolved: &crate::core::context::ResolvedContext,
    cursor: Position,
) -> Vec<CompletionItem> {
    let mut scopes = vec![global_scope(resolved)];
    scopes.extend(collect_local_scopes(
        &resolved.module,
        &resolved.def_table,
        cursor,
    ));
    merge_scope_frames(scopes)
}

pub(super) fn enclosing_callable_def_id(
    module: &crate::core::tree::Module,
    cursor: Position,
) -> Option<DefId> {
    locals::enclosing_callable_def_id(module, cursor)
}

fn merge_scope_frames(frames: Vec<HashMap<String, CompletionItem>>) -> Vec<CompletionItem> {
    let mut merged = HashMap::<String, CompletionItem>::new();
    for frame in frames {
        for (label, item) in frame {
            merged.insert(label, item);
        }
    }
    merged.into_values().collect()
}
