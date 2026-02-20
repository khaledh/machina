//! Completion logic for analysis queries.
//!
//! This module owns completion context detection, scope/member candidate
//! collection, and fallback source synthesis for incomplete member probes.

mod member;
mod scope;
mod site;
mod source_probe;

use std::collections::HashSet;

use crate::core::diag::{Position, Span};
use crate::services::analysis::results::CompletionItem;
use member::{member_completions, qualified_path_completions};
use scope::{enclosing_callable_def_id, scope_completions};
use site::{CompletionSite, classify_completion_site};

pub(crate) fn collect(
    source: &str,
    query_span: Span,
    resolved: &crate::core::context::ResolvedContext,
    typed: Option<&crate::core::context::TypeCheckedContext>,
) -> Vec<CompletionItem> {
    let site = classify_completion_site(
        source,
        query_span,
        resolved,
        typed,
        enclosing_callable_def_id,
    );
    let items = dispatch_site_completions(&site, query_span.start, resolved);
    completion_post_pass(items, site.prefix())
}

pub(crate) fn synthesize_member_completion_source(
    source: &str,
    cursor: Position,
) -> Option<String> {
    source_probe::synthesize_member_completion_source(source, cursor)
}

fn dispatch_site_completions(
    site: &CompletionSite,
    fallback_cursor: Position,
    resolved: &crate::core::context::ResolvedContext,
) -> Vec<CompletionItem> {
    let items = match site {
        CompletionSite::Scope { cursor, .. } => scope_completions(resolved, *cursor),
        CompletionSite::Member {
            receiver_ty,
            caller_def_id,
            ..
        } => member_completions(resolved, receiver_ty, *caller_def_id),
        CompletionSite::QualifiedPath { path_segments, .. } => {
            qualified_path_completions(resolved, path_segments)
        }
        CompletionSite::RequiresPath { .. }
        | CompletionSite::TypeExpr { .. }
        | CompletionSite::Pattern { .. } => Vec::new(),
    };
    if items.is_empty()
        && matches!(
            site,
            CompletionSite::RequiresPath { .. }
                | CompletionSite::TypeExpr { .. }
                | CompletionSite::Pattern { .. }
        )
    {
        scope_completions(resolved, fallback_cursor)
    } else {
        items
    }
}

fn completion_post_pass(mut items: Vec<CompletionItem>, prefix: &str) -> Vec<CompletionItem> {
    items.retain(|item| item.label.starts_with(prefix));
    let mut seen = HashSet::<String>::new();
    items.retain(|item| seen.insert(item.label.clone()));
    items.sort_unstable_by(|a, b| a.label.cmp(&b.label));
    items
}

#[cfg(test)]
mod tests;
