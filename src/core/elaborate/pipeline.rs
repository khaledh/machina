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

/// Run explicit pass boundaries for elaboration.
pub(super) fn run(elaborator: &mut Elaborator<'_>, module: &norm::Module) -> sem::Module {
    elaborator.reset_module_state();

    // Pass 1 (current combined core): syntax desugaring + place/value lowering.
    // This currently also discovers closure conversions while traversing items.
    let mut items = run_place_value_planning_pass(elaborator, module);

    // Pass 2: materialize closure conversion artifacts into top-level items.
    run_closure_materialization_pass(elaborator, &mut items);

    sem::Module {
        top_level_items: items,
    }
}

fn run_place_value_planning_pass(
    elaborator: &mut Elaborator<'_>,
    module: &norm::Module,
) -> Vec<sem::TopLevelItem> {
    elaborator.run_place_value_planning_pass(module)
}

fn run_closure_materialization_pass(
    elaborator: &mut Elaborator<'_>,
    items: &mut Vec<sem::TopLevelItem>,
) {
    elaborator.append_lifted_closure_items(items);
}
