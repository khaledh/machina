//! Shared helpers for scanning compiler-generated typestate handlers.
//!
//! Several semcheck passes need the same structural walk:
//! generated typestate method blocks -> generated handler defs. Keeping that
//! traversal here lets the individual passes focus on their own facts/checks.

use crate::core::machine::naming::{is_generated_handler_name, parse_generated_state_name};
use crate::core::tree::{MethodBlock, MethodDef, MethodItem, Module};

pub(super) struct GeneratedTypestateHandler<'a> {
    pub(super) typestate_name: String,
    pub(super) state_name: String,
    pub(super) method_block: &'a MethodBlock,
    pub(super) method_def: &'a MethodDef,
}

pub(super) fn collect_generated_typestate_handlers(
    module: &Module,
) -> Vec<GeneratedTypestateHandler<'_>> {
    let mut handlers = Vec::new();
    for method_block in module.method_blocks() {
        let Some((typestate_name, state_name)) =
            parse_generated_state_name(&method_block.type_name)
        else {
            continue;
        };
        for method_item in &method_block.method_items {
            let MethodItem::Def(method_def) = method_item else {
                continue;
            };
            if !is_generated_handler_name(&method_def.sig.name) {
                continue;
            }
            handlers.push(GeneratedTypestateHandler {
                typestate_name: typestate_name.clone(),
                state_name: state_name.clone(),
                method_block,
                method_def,
            });
        }
    }
    handlers
}
