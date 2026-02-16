//! SSA lowering from the semantic tree into the SSA IR.
//!
//! This module currently supports a subset of the language while the SSA
//! pipeline is built out incrementally.

mod bind;
mod branching;
mod calls;
mod drop_glue;
mod drops;
mod entry_wrapper;
mod equality;
mod error;
mod fstring;
mod globals;
mod join;
mod linear;
mod locals;
mod lowerer;
mod machine;
mod mapping;
mod r#match;
mod place;
mod proj;
mod slots;
mod types;
mod util;

use crate::core::backend::lower::drop_glue::DropGlueRegistry;
use crate::core::backend::lower::globals::GlobalArena;
use crate::core::backend::lower::lowerer::BranchResult;
use crate::core::ir::IrTypeCache;
use crate::core::ir::{Function, GlobalData};
use crate::core::resolve::DefTable;
use crate::core::tree::semantic as sem;
use crate::core::typecheck::type_map::TypeMap;
use crate::core::types::Type;
use lowerer::FuncLowerer;

pub struct LoweredFunction {
    pub func: Function,
    pub types: IrTypeCache,
    pub globals: Vec<GlobalData>,
}

pub struct LoweredModule {
    pub funcs: Vec<LoweredFunction>,
    pub globals: Vec<GlobalData>,
}

/// Options for module SSA lowering beyond the core semantic inputs.
#[derive(Clone, Default)]
pub struct LowerOpts<'a> {
    pub machine_plans: Option<&'a sem::MachinePlanMap>,
    pub trace_alloc: bool,
    pub trace_drops: bool,
    pub executable: bool,
}

pub use error::LowerToIrError;

/// Lowers a semantic function definition into SSA IR.
///
/// This is the main entry point for SSA lowering. The process:
/// 1. Creates a `FuncLowerer` with type context and function signature
/// 2. Seeds the entry block with function parameters as block params
/// 3. Maps parameters to local variables for SSA value tracking
/// 4. Lowers the function body using branching expression lowering
/// 5. Terminates with a return instruction if the body produces a value
pub fn lower_func(
    func: &sem::FuncDef,
    def_table: &DefTable,
    type_map: &TypeMap,
    lowering_plans: &sem::LoweringPlanMap,
    drop_plans: &sem::DropPlanMap,
) -> Result<LoweredFunction, LowerToIrError> {
    let mut globals = GlobalArena::new();
    let mut drop_glue = DropGlueRegistry::new(def_table);
    let empty_machine_plans = sem::MachinePlanMap::default();
    lower_func_with_globals(
        func,
        def_table,
        None,
        type_map,
        lowering_plans,
        &empty_machine_plans,
        drop_plans,
        false,
        false,
        &mut drop_glue,
        &mut globals,
    )
}

/// Lowers a semantic module with default options.
pub fn lower_module(
    module: &sem::Module,
    def_table: &DefTable,
    type_map: &TypeMap,
    lowering_plans: &sem::LoweringPlanMap,
    drop_plans: &sem::DropPlanMap,
) -> Result<LoweredModule, LowerToIrError> {
    lower_module_with_opts(
        module,
        def_table,
        type_map,
        lowering_plans,
        drop_plans,
        &LowerOpts::default(),
    )
}

/// Lowers a semantic module with configurable options.
pub fn lower_module_with_opts(
    module: &sem::Module,
    def_table: &DefTable,
    type_map: &TypeMap,
    lowering_plans: &sem::LoweringPlanMap,
    drop_plans: &sem::DropPlanMap,
    opts: &LowerOpts<'_>,
) -> Result<LoweredModule, LowerToIrError> {
    let default_machine_plans = sem::MachinePlanMap::default();
    let machine_plans = opts.machine_plans.unwrap_or(&default_machine_plans);
    lower_module_impl(
        module,
        def_table,
        type_map,
        lowering_plans,
        drop_plans,
        machine_plans,
        opts.trace_alloc,
        opts.trace_drops,
        opts.executable,
    )
}

#[allow(clippy::too_many_arguments)]
fn lower_module_impl(
    module: &sem::Module,
    def_table: &DefTable,
    type_map: &TypeMap,
    lowering_plans: &sem::LoweringPlanMap,
    drop_plans: &sem::DropPlanMap,
    machine_plans: &sem::MachinePlanMap,
    trace_alloc: bool,
    trace_drops: bool,
    inject_entry_wrapper: bool,
) -> Result<LoweredModule, LowerToIrError> {
    let mut globals = GlobalArena::new();
    let mut funcs = Vec::new();
    let mut drop_glue = DropGlueRegistry::from_module(def_table, module);

    for func_def in module.func_defs() {
        let lowered = lower_func_with_globals(
            func_def,
            def_table,
            Some(module),
            type_map,
            lowering_plans,
            machine_plans,
            drop_plans,
            trace_alloc,
            trace_drops,
            &mut drop_glue,
            &mut globals,
        )?;
        funcs.push(lowered);
    }

    for method_block in module.method_blocks() {
        for method_item in &method_block.method_items {
            let sem::MethodItem::Def(method_def) = method_item else {
                continue;
            };
            let lowered = lower_method_def_with_globals(
                module,
                method_block,
                method_def,
                def_table,
                type_map,
                lowering_plans,
                machine_plans,
                drop_plans,
                trace_alloc,
                trace_drops,
                &mut drop_glue,
                &mut globals,
            )?;
            funcs.push(lowered);
        }
    }

    let mut glue_funcs =
        drop_glue.take_glue_functions(def_table, type_map, &mut globals, trace_drops)?;
    funcs.append(&mut glue_funcs);

    // Materialize managed machine descriptors + thunk placeholders as backend
    // artifacts. Full runtime bootstrap wiring will consume these.
    machine::append_machine_runtime_artifacts(
        machine_plans,
        def_table,
        type_map,
        &mut funcs,
        &mut globals,
    );

    if inject_entry_wrapper {
        entry_wrapper::append_executable_entry_wrapper(
            module,
            def_table,
            type_map,
            &mut funcs,
            &mut globals,
        );
    }

    Ok(LoweredModule {
        funcs,
        globals: globals.into_globals(),
    })
}

#[allow(clippy::too_many_arguments)]
fn lower_func_with_globals(
    func: &sem::FuncDef,
    def_table: &DefTable,
    module: Option<&sem::Module>,
    type_map: &TypeMap,
    lowering_plans: &sem::LoweringPlanMap,
    machine_plans: &sem::MachinePlanMap,
    drop_plans: &sem::DropPlanMap,
    trace_alloc: bool,
    trace_drops: bool,
    drop_glue: &mut DropGlueRegistry,
    globals: &mut GlobalArena,
) -> Result<LoweredFunction, LowerToIrError> {
    let globals_start = globals.len();
    let ret_ty = {
        let def = def_table
            .lookup_def(func.def_id)
            .unwrap_or_else(|| panic!("backend lower_func missing def {:?}", func.def_id));
        let func_ty = type_map
            .lookup_def_type(def)
            .unwrap_or_else(|| panic!("backend lower_func missing def type {:?}", func.def_id));
        match func_ty {
            Type::Fn { ret_ty, .. } => ret_ty,
            other => panic!("backend lower_func expected fn type, found {:?}", other),
        }
    };
    let ret_is_unit = matches!(ret_ty.as_ref(), Type::Unit);

    // Initialize the lowerer with function metadata and type information.
    // The builder starts with the cursor at the entry block (block 0).
    let mut lowerer = FuncLowerer::new(
        func,
        def_table,
        module,
        type_map,
        lowering_plans,
        Some(machine_plans),
        drop_glue,
        globals,
        trace_drops,
    );
    lowerer.init_root_drop_scope(drop_plans, func.id);

    // Add function parameters as block parameters to the entry block,
    // then establish the initial locals mapping from parameters.
    let entry = lowerer.builder.current_block();
    let param_tys = lowerer.param_tys.clone();
    let mut param_values = Vec::with_capacity(param_tys.len());
    for ty in &param_tys {
        param_values.push(lowerer.builder.add_block_param(entry, *ty));
    }
    lowerer.init_param_locals(&param_values);

    if trace_alloc && func.sig.name == "main" {
        lowerer.emit_runtime_set_alloc_trace(true);
    }

    // Lower the function body. This may produce multiple basic blocks
    // for control flow (if/else, loops, etc.). The cursor ends at the
    // final block where execution continues.
    let result = lowerer.lower_branching_value_expr(&func.body)?;

    // If the body produces a value (not an early return), emit the final return.
    if let BranchResult::Value(value) = result {
        let body_ty = type_map.type_table().get(func.body.ty).clone();
        let value = lowerer.coerce_return_value(value, &body_ty);
        lowerer.emit_root_return(if ret_is_unit { None } else { Some(value) })?;
    }

    let (func, types) = lowerer.finish();
    let globals = globals.slice_from(globals_start);

    Ok(LoweredFunction {
        func,
        types,
        globals,
    })
}

#[allow(clippy::too_many_arguments)]
fn lower_method_def_with_globals(
    module: &sem::Module,
    method_block: &sem::MethodBlock,
    method_def: &sem::MethodDef,
    def_table: &DefTable,
    type_map: &TypeMap,
    lowering_plans: &sem::LoweringPlanMap,
    machine_plans: &sem::MachinePlanMap,
    drop_plans: &sem::DropPlanMap,
    trace_alloc: bool,
    trace_drops: bool,
    drop_glue: &mut DropGlueRegistry,
    globals: &mut GlobalArena,
) -> Result<LoweredFunction, LowerToIrError> {
    let globals_start = globals.len();
    let ret_ty = type_map.lookup_node_type(method_def.id).unwrap_or_else(|| {
        panic!(
            "backend lower_method missing return type for {:?}",
            method_def.id
        )
    });
    let ret_is_unit = matches!(ret_ty, Type::Unit);

    // Initialize the lowerer with method metadata and type information.
    // The builder starts with the cursor at the entry block (block 0).
    let mut lowerer = FuncLowerer::new_method(
        &method_block.type_name,
        method_def,
        def_table,
        module,
        type_map,
        lowering_plans,
        Some(machine_plans),
        drop_glue,
        globals,
        trace_drops,
    );
    lowerer.init_root_drop_scope(drop_plans, method_def.id);

    // Add method parameters (including `self`) as block parameters to the entry block,
    // then establish the initial locals mapping from parameters.
    let entry = lowerer.builder.current_block();
    let param_tys = lowerer.param_tys.clone();
    let mut param_values = Vec::with_capacity(param_tys.len());
    for ty in &param_tys {
        param_values.push(lowerer.builder.add_block_param(entry, *ty));
    }
    lowerer.init_param_locals(&param_values);

    if trace_alloc && method_def.sig.name == "main" {
        lowerer.emit_runtime_set_alloc_trace(true);
    }

    // Lower the method body and emit the final return if it yields a value.
    let result = lowerer.lower_branching_value_expr(&method_def.body)?;
    if let BranchResult::Value(value) = result {
        let body_ty = type_map.type_table().get(method_def.body.ty).clone();
        let value = lowerer.coerce_return_value(value, &body_ty);
        lowerer.emit_root_return(if ret_is_unit { None } else { Some(value) })?;
    }

    let (func, types) = lowerer.finish();
    let globals = globals.slice_from(globals_start);

    Ok(LoweredFunction {
        func,
        types,
        globals,
    })
}

#[cfg(test)]
#[path = "../../../tests/backend/lower/mod.rs"]
mod tests;
