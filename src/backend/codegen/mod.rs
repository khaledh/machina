// SSA-based code generation lives here.

pub mod arm64;
pub mod emitter;
pub mod graph;
pub mod moves;
pub mod traverse;
pub mod x86_64;

use std::collections::HashMap;

use crate::backend::TargetKind;
use crate::backend::analysis::liveness;
use crate::backend::codegen::emitter::CodegenEmitter;
use crate::backend::lower::{LoweredFunction, LoweredModule};
use crate::backend::regalloc::arm64::Arm64Target;
use crate::backend::regalloc::x86_64::X86_64Target;
use crate::backend::regalloc::{TargetSpec, regalloc};
use crate::core::resolve::DefId;

/// Emit a full SSA module (globals + functions) using the provided emitter.
pub fn emit_module_with_emitter(
    module: &LoweredModule,
    def_names: &HashMap<DefId, String>,
    target: &dyn TargetSpec,
    target_kind: TargetKind,
    emitter: &mut dyn CodegenEmitter,
) {
    for global in &module.globals {
        emitter.emit_global(global);
    }

    for func in &module.funcs {
        emit_function_with_emitter(func, def_names, target, target_kind, emitter);
    }
}

/// Convenience entrypoint for emitting ARM64 assembly for a full module.
pub fn emit_module_arm64(
    module: &LoweredModule,
    def_names: &HashMap<DefId, String>,
    target: &dyn TargetSpec,
    target_kind: TargetKind,
) -> String {
    let mut emitter = arm64::Arm64Emitter::for_target(target_kind);
    emit_module_with_emitter(module, def_names, target, target_kind, &mut emitter);
    emitter.finish()
}

/// Convenience entrypoint for emitting x86-64 assembly for a full module.
pub fn emit_module_x86_64(
    module: &LoweredModule,
    def_names: &HashMap<DefId, String>,
    target: &dyn TargetSpec,
    target_kind: TargetKind,
) -> String {
    let mut emitter = x86_64::X86_64Emitter::for_target(target_kind);
    emit_module_with_emitter(module, def_names, target, target_kind, &mut emitter);
    emitter.finish()
}

/// Emit a full SSA module for the selected backend target.
pub fn emit_module(
    module: &LoweredModule,
    def_names: &HashMap<DefId, String>,
    target_kind: TargetKind,
) -> String {
    match target_kind {
        TargetKind::Arm64Macos => {
            let target = Arm64Target::new();
            emit_module_arm64(module, def_names, &target, target_kind)
        }
        TargetKind::X86_64Macos | TargetKind::X86_64Linux => {
            let target = X86_64Target::new();
            emit_module_x86_64(module, def_names, &target, target_kind)
        }
    }
}

fn emit_function_with_emitter(
    func: &LoweredFunction,
    def_names: &HashMap<DefId, String>,
    target: &dyn TargetSpec,
    target_kind: TargetKind,
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
        .map(|name| target_kind.mangle_symbol(name))
        .unwrap_or_else(|| format!("{}fn{}", target_kind.symbol_prefix(), func.func.def_id.0));

    traverse::emit_graph_with_emitter(
        &graph,
        &func.func,
        &alloc.alloc_map,
        alloc.frame_size,
        &alloc.used_callee_saved,
        target,
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
