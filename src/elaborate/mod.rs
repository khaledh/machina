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

use crate::context::{SemanticCheckedContext, SemanticContext};
use crate::tree::semantic as sem;
mod bind_pattern;
mod calls;
mod closure;
mod drop_plan;
mod elaborator;
mod index_plan;
mod lowering_plan;
mod match_plan;
mod place;
mod types;
mod value;

use crate::elaborate::drop_plan::build_drop_plans;
use crate::elaborate::elaborator::Elaborator;
use crate::elaborate::lowering_plan::build_lowering_plans;

/// Transform a normalized tree into a semantic tree using the results from
/// semantic analysis.
pub fn elaborate(ctx: SemanticCheckedContext) -> SemanticContext {
    let SemanticCheckedContext {
        module,
        def_table,
        type_map,
        call_sigs,
        generic_insts,
        symbols,
        node_id_gen,
        implicit_moves,
        init_assigns,
        full_init_assigns,
        closure_captures,
    } = ctx;
    let mut node_id_gen = node_id_gen;
    let mut def_table = def_table;
    let mut type_map = type_map;
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

    let module = elaborator.elaborate_module(&module);
    let lowering_plans = build_lowering_plans(&module, &type_map);
    let drop_plans = build_drop_plans(&module, &def_table, &type_map);

    // Generate method names for lifted closures and add them to the symbol table
    let mut symbols = symbols;
    let mut used_names: std::collections::HashSet<String> =
        symbols.def_names.values().cloned().collect();
    for method_block in module.method_blocks() {
        let type_name = method_block.type_name.as_str();
        for method_item in &method_block.method_items {
            let method_def = match method_item {
                sem::MethodItem::Def(method_def) => method_def,
                sem::MethodItem::Decl(_) => continue,
            };
            if symbols.def_names.contains_key(&method_def.def_id) {
                continue;
            }
            let base_name = format!("{type_name}${}", method_def.sig.name);
            let name = if used_names.contains(&base_name) {
                format!("{base_name}${}", method_def.def_id.0)
            } else {
                base_name
            };
            used_names.insert(name.clone());
            symbols.register_generated_def(method_def.def_id, name);
        }
    }

    SemanticContext {
        module,
        def_table,
        type_map,
        lowering_plans,
        drop_plans,
        symbols,
        node_id_gen,
        generic_insts,
    }
}
