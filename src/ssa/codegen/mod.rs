// SSA-based code generation lives here.

pub mod arm64;
pub mod emitter;
pub mod graph;
pub mod moves;
pub mod traverse;

use crate::ssa::analysis::liveness;
use crate::ssa::codegen::emitter::CodegenEmitter;
use crate::ssa::lower::{LoweredFunction, LoweredModule};
use crate::ssa::regalloc::{TargetSpec, regalloc};

/// Emit a full SSA module (globals + functions) using the provided emitter.
pub fn emit_module_with_emitter(
    module: &LoweredModule,
    target: &dyn TargetSpec,
    emitter: &mut dyn CodegenEmitter,
) {
    for global in &module.globals {
        emitter.emit_global(global);
    }

    for func in &module.funcs {
        emit_function_with_emitter(func, target, emitter);
    }
}

/// Convenience entrypoint for emitting ARM64 assembly for a full module.
pub fn emit_module_arm64(module: &LoweredModule, target: &dyn TargetSpec) -> String {
    let mut emitter = arm64::Arm64Emitter::new();
    emit_module_with_emitter(module, target, &mut emitter);
    emitter.finish()
}

fn emit_function_with_emitter(
    func: &LoweredFunction,
    target: &dyn TargetSpec,
    emitter: &mut dyn CodegenEmitter,
) {
    let live_map = liveness::analyze(&func.func);
    let mut types = func.types.clone();
    let alloc = regalloc(&func.func, &mut types, &live_map, target);

    let schedule = moves::MoveSchedule::from_moves(&alloc.edge_moves, &alloc.call_moves);
    let plan = moves::EdgeMovePlan::new(&func.func, schedule);
    let graph = graph::CodegenGraph::new(&func.func, &plan);

    traverse::emit_graph_with_emitter(
        &graph,
        &func.func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        &mut types,
        emitter,
    );
}

#[cfg(test)]
#[path = "../../tests/ssa/codegen/t_moves.rs"]
mod tests_moves;

#[cfg(test)]
#[path = "../../tests/ssa/codegen/t_graph.rs"]
mod tests_graph;

#[cfg(test)]
#[path = "../../tests/ssa/codegen/t_traverse.rs"]
mod tests_traverse;

#[cfg(test)]
#[path = "../../tests/ssa/codegen/t_emitter.rs"]
mod tests_emitter;
