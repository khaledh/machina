mod ast_liveness;
pub(crate) mod closure;
mod def_init;
mod errors;
mod liveness_util;
mod lvalue_overlap;
mod match_check;
mod move_check;
mod slice_borrow;
mod slice_escape;
mod structural;
mod value;

use std::collections::HashSet;

pub use errors::SemCheckError;

use crate::context::{NormalizedContext, SemanticCheckedContext};
use crate::tree::NodeId;

pub fn sem_check(ctx: NormalizedContext) -> Result<SemanticCheckedContext, Vec<SemCheckError>> {
    let output = sem_check_partial(ctx, &HashSet::new());
    if output.errors.is_empty() {
        Ok(output.context)
    } else {
        Err(output.errors)
    }
}

/// Best-effort semantic checking used by analysis mode.
///
/// If the upstream pipeline is already root-poisoned, semantic checks are
/// skipped and a default semantic context is materialized.
pub fn sem_check_partial(
    ctx: NormalizedContext,
    upstream_poisoned_nodes: &HashSet<NodeId>,
) -> SemCheckOutput {
    const ROOT_POISON_NODE: NodeId = NodeId(0);

    if upstream_poisoned_nodes.contains(&ROOT_POISON_NODE) {
        return SemCheckOutput {
            context: ctx.with_sem_results(
                HashSet::new(),
                HashSet::new(),
                HashSet::new(),
                std::collections::HashMap::new(),
            ),
            errors: Vec::new(),
            poisoned_nodes: upstream_poisoned_nodes.clone(),
        };
    }

    let mut errors = Vec::new();

    let move_result = move_check::check(&ctx);
    let def_init_result = def_init::check(&ctx);
    let capture_result = closure::capture::check(&ctx);
    let closure_borrow_errors = closure::borrow::check(&ctx, &capture_result.captures);

    errors.extend(value::check(&ctx));
    errors.extend(structural::check(&ctx));
    errors.extend(lvalue_overlap::check(&ctx));
    errors.extend(slice_borrow::check(&ctx));
    errors.extend(def_init_result.errors);
    errors.extend(capture_result.errors);
    errors.extend(closure_borrow_errors);
    errors.extend(move_result.errors);
    errors.extend(slice_escape::check(&ctx));

    let mut poisoned_nodes = upstream_poisoned_nodes.clone();
    if !errors.is_empty() {
        poisoned_nodes.insert(ROOT_POISON_NODE);
    }

    SemCheckOutput {
        context: ctx.with_sem_results(
            move_result.implicit_moves,
            def_init_result.init_assigns,
            def_init_result.full_init_assigns,
            capture_result.captures,
        ),
        errors,
        poisoned_nodes,
    }
}

#[derive(Clone)]
pub struct SemCheckOutput {
    pub context: SemanticCheckedContext,
    pub errors: Vec<SemCheckError>,
    pub poisoned_nodes: HashSet<NodeId>,
}

#[cfg(test)]
#[path = "../../tests/semck/t_semck.rs"]
mod tests;

#[cfg(test)]
#[path = "../../tests/semck/t_move_check.rs"]
mod move_tests;

#[cfg(test)]
#[path = "../../tests/semck/t_partial.rs"]
mod partial_tests;
