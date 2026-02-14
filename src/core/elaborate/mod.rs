//! Elaboration pass: transform a normalized tree into a semantic tree.
//!
//! This pass sits between semantic checking and lowering. Its job is to
//! pre-compute semantic constructs so that lowering can focus on code
//! generation without making semantic decisions. Key responsibilities:
//!
//! - **Closure lifting**: Transform inline closures into struct types with
//!   captured fields and `invoke` methods. This makes closures first-class
//!   values that lowering can treat uniformly.
//!
//! - **Call planning**: Pre-compute how each call should be lowered, including
//!   argument passing modes, receiver handling, and intrinsic dispatch.
//!
//! - **Match planning**: Build decision trees for pattern matching that encode
//!   the exact sequence of tests and bindings needed at runtime.
//!
//! - **For loop desugaring**: Rewrite `for` loops into `while` loops with
//!   explicit index management, so lowering sees only `while`.
//!
//! - **String format planning**: Pre-compute string interpolation strategies
//!   (view vs owned formatting) and reserve length calculations.
//!
//! - **Place/value separation**: Distinguish between place expressions (lvalues)
//!   and value expressions, inserting explicit load/move nodes based on
//!   semantic analysis results.
//!
//! The output semantic tree contains all information needed for lowering to
//! proceed without further semantic reasoning.

use crate::core::analysis::facts::{DefTableOverlay, TypeMapOverlay};
use crate::core::context::{ElaborateStageInput, ElaborateStageOutput};
mod bind_pattern;
mod calls;
mod closure;
mod drop_plan;
mod elaborator;
mod index_plan;
mod lowering_plan;
mod match_plan;
mod pipeline;
mod place;
mod syntax_desugar;
mod types;
mod value;

use crate::core::elaborate::closure::register_lifted_method_symbols;
use crate::core::elaborate::drop_plan::build_drop_plans;
use crate::core::elaborate::elaborator::Elaborator;
use crate::core::elaborate::lowering_plan::build_lowering_plans;

/// Transform a normalized tree into a semantic tree using the results from
/// semantic analysis.
///
/// Internal stage entrypoint; prefer `crate::core::api::elaborate_stage` from
/// orchestration code.
pub fn elaborate(ctx: ElaborateStageInput) -> ElaborateStageOutput {
    let ElaborateStageInput { module, payload } = ctx;
    let crate::core::context::SemCheckedPayload {
        typed,
        implicit_moves,
        init_assigns,
        full_init_assigns,
        closure_captures,
    } = payload;
    let crate::core::context::TypedTables {
        resolved,
        type_map,
        call_sigs,
        generic_insts,
    } = typed;
    let crate::core::context::ResolvedTables {
        def_table,
        def_owners,
        symbols,
        node_id_gen,
        typestate_role_impls,
    } = resolved;
    let mut node_id_gen = node_id_gen;
    let mut def_table = DefTableOverlay::new(def_table);
    let mut type_map = TypeMapOverlay::new(type_map);
    let mut elaborator = Elaborator::new(
        &mut def_table,
        &mut type_map,
        &call_sigs,
        &mut node_id_gen,
        &implicit_moves,
        &init_assigns,
        &full_init_assigns,
        &closure_captures,
    );

    let module = pipeline::run(&mut elaborator, &module);
    let (call_plans, index_plans, match_plans, slice_plans) = elaborator.lowering_plan_tables();
    let lowering_plans =
        build_lowering_plans(&module, call_plans, index_plans, match_plans, slice_plans);
    let drop_plans = build_drop_plans(&module, &def_table, &type_map);

    let mut symbols = symbols;
    register_lifted_method_symbols(&module, &mut symbols);

    ElaborateStageOutput {
        module,
        payload: crate::core::context::SemanticPayload {
            typed: crate::core::context::TypedTables {
                resolved: crate::core::context::ResolvedTables {
                    def_table: def_table.into_inner(),
                    def_owners,
                    symbols,
                    node_id_gen,
                    typestate_role_impls,
                },
                type_map: type_map.into_inner(),
                call_sigs,
                generic_insts,
            },
            lowering_plans,
            drop_plans,
        },
    }
}
