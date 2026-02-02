// SSA-based code generation lives here.

pub mod arm64;
pub mod emitter;
pub mod graph;
pub mod moves;
pub mod traverse;

use std::collections::HashMap;

use crate::backend::analysis::liveness;
use crate::backend::codegen::emitter::CodegenEmitter;
use crate::backend::lower::{LoweredFunction, LoweredModule};
use crate::backend::regalloc::{TargetSpec, regalloc};
use crate::resolve::DefId;

/// Emit a full SSA module (globals + functions) using the provided emitter.
pub fn emit_module_with_emitter(
    module: &LoweredModule,
    def_names: &HashMap<DefId, String>,
    target: &dyn TargetSpec,
    emitter: &mut dyn CodegenEmitter,
) {
    for global in &module.globals {
        emitter.emit_global(global);
    }

    for func in &module.funcs {
        emit_function_with_emitter(func, def_names, target, emitter);
    }
}

/// Convenience entrypoint for emitting ARM64 assembly for a full module.
pub fn emit_module_arm64(
    module: &LoweredModule,
    def_names: &HashMap<DefId, String>,
    target: &dyn TargetSpec,
) -> String {
    let mut emitter = arm64::Arm64Emitter::new();
    emit_module_with_emitter(module, def_names, target, &mut emitter);
    emitter.finish()
}

fn emit_function_with_emitter(
    func: &LoweredFunction,
    def_names: &HashMap<DefId, String>,
    target: &dyn TargetSpec,
    emitter: &mut dyn CodegenEmitter,
) {
    let live_map = liveness::analyze(&func.func);
    let mut types = func.types.clone();
    let alloc = regalloc(&func.func, &mut types, &live_map, target);

    let schedule = moves::MoveSchedule::from_moves(
        &alloc.edge_moves,
        &alloc.call_moves,
        &alloc.entry_moves,
        &alloc.param_copies,
    );
    let plan = moves::EdgeMovePlan::new(&func.func, schedule);
    let graph = graph::CodegenGraph::new(&func.func, &plan);

    let func_label = def_names
        .get(&func.func.def_id)
        .map(|name| format!("_{}", name))
        .unwrap_or_else(|| format!("_fn{}", func.func.def_id.0));

    traverse::emit_graph_with_emitter(
        &graph,
        &func.func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        def_names,
        &func_label,
        emitter,
    );
}

#[cfg(test)]
#[path = "../../tests/backend/codegen/t_moves.rs"]
mod tests_moves;

#[cfg(test)]
#[path = "../../tests/backend/codegen/t_graph.rs"]
mod tests_graph;

#[cfg(test)]
#[path = "../../tests/backend/codegen/t_traverse.rs"]
mod tests_traverse;

#[cfg(test)]
#[path = "../../tests/backend/codegen/t_emitter.rs"]
mod tests_emitter;
