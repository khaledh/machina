//! Closure borrow checking: prevents conflicting access to captured bases.
//!
//! Rule: if a closure captures a base by immutable borrow, the base cannot be
//! mutated or moved while the closure is live. If captured by mutable borrow,
//! the base cannot be accessed at all while the closure is live.
//!
//! This pass is flow-sensitive: it tracks which locals are bound to captured
//! closures and uses liveness to allow access after the last closure use.

mod bindings;
mod checks;
mod collect;
mod liveness;

use std::collections::HashMap;

use crate::core::context::NormalizedContext;
use crate::core::resolve::DefId;
use crate::core::semck::SemCheckError;
use crate::core::semck::closure::capture::{CaptureMode, ClosureCapture};

type CaptureMap = HashMap<DefId, CaptureMode>;
type ClosureBindings = HashMap<DefId, CaptureMap>;

pub(crate) fn check(
    ctx: &NormalizedContext,
    captures: &HashMap<DefId, Vec<ClosureCapture>>,
) -> Vec<SemCheckError> {
    let capture_map = bindings::build_capture_map(captures);
    let mut errors = Vec::new();

    for func_def in ctx.module.func_defs() {
        checks::check_func_def(ctx, func_def, &capture_map, &mut errors);
    }

    errors
}
