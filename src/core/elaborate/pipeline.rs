//! Explicit elaborate-stage pipeline scaffolding.
//!
//! This module introduces named pass boundaries for the long-horizon split of
//! elaborate responsibilities:
//! - syntax-level desugaring,
//! - closure conversion/materialization,
//! - place/value lowering and planning.
//!
//! For now these boundaries are orchestration wrappers over the existing
//! `Elaborator` implementation to preserve behavior while making subsequent
//! extraction work mechanical.

use crate::core::tree::normalized as norm;
use crate::core::tree::semantic as sem;

use super::elaborator::Elaborator;
use super::syntax_desugar;

/// Run explicit pass boundaries for elaboration.
pub(super) fn run(elaborator: &mut Elaborator<'_>, module: &norm::Module) -> sem::Module {
    elaborator.reset_module_state();

    // Pass 1: place/value lowering + plan capture.
    // This pass also discovers closure conversions while traversing items.
    let mut items = elaborator.run_place_value_planning_pass(module);

    // Pass 2: materialize closure conversion artifacts into top-level items.
    elaborator.append_lifted_closure_items(&mut items);

    // Pass 3: syntax-level desugaring over the semantic tree.
    let mut module = sem::Module {
        top_level_items: items,
    };
    syntax_desugar::run(elaborator, &mut module);

    module
}
