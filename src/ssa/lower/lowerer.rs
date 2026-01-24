//! SSA lowering state and shared helpers.

use std::collections::HashMap;

use crate::resolve::{DefId, DefTable};
use crate::ssa::lower::LoweringError;
use crate::ssa::lower::globals::GlobalArena;
use crate::ssa::lower::locals::LocalMap;
use crate::ssa::lower::types::TypeLowerer;
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{BlockId, Function, FunctionSig, GlobalId, ValueId};
use crate::ssa::{IrTypeCache, IrTypeId};
use crate::tree::NodeId;
use crate::tree::ParamMode;
use crate::tree::semantic as sem;
use crate::typeck::type_map::TypeMap;
use crate::types::Type;

/// An SSA value produced by linear (single-block) expression lowering.
pub(super) type LinearValue = ValueId;

/// Result of lowering a branching expression.
///
/// After lowering, the builder's cursor is at the "ending block" where
/// execution continues.
pub(super) enum BranchResult {
    /// Expression produced a value. Cursor is at the ending block.
    Value(ValueId),
    /// Expression terminates with a return (no continuation).
    Return,
}

/// Outcome of lowering a statement.
pub(super) enum StmtOutcome {
    /// Continue execution in the current block.
    Continue,
    /// Statement terminates with a return.
    Return,
}

/// Loop context for break/continue lowering.
pub(super) struct LoopContext {
    pub(super) header_bb: BlockId,
    pub(super) exit_bb: BlockId,
    pub(super) defs: Vec<DefId>,
}

/// Main state for lowering a single function to SSA IR.
///
/// Tracks:
/// - Type lowering context for converting types
/// - SSA function builder for emitting instructions
/// - Current SSA values for local variables (updated on assignment/join)
/// - Expression plans from semantic analysis (linear vs branching)
pub(super) struct FuncLowerer<'a, 'g> {
    pub(super) type_lowerer: TypeLowerer<'a>,
    pub(crate) type_map: &'a TypeMap,
    pub(super) builder: FunctionBuilder,
    /// Maps definition IDs to their current SSA values (mutable during lowering).
    pub(super) locals: LocalMap,
    pub(super) lowering_plans: &'a HashMap<NodeId, sem::LoweringPlan>,
    pub(super) param_defs: Vec<DefId>,
    pub(super) param_tys: Vec<IrTypeId>,
    pub(super) loop_stack: Vec<LoopContext>,
    pub(super) globals: &'g mut GlobalArena,
}

/// Base pointer + length pair for slice-like lowering.
pub(super) struct BaseView {
    pub(super) ptr: ValueId,
    pub(super) len: ValueId,
}

/// An addressable slot for materializing aggregate values.
pub(super) struct ValueSlot {
    pub(super) addr: ValueId,
    pub(super) ty: IrTypeId,
}

impl<'a, 'g> FuncLowerer<'a, 'g> {
    /// Creates a new function lowerer for the given semantic function definition.
    ///
    /// Initializes the type context, extracts the function signature, and prepares
    /// parameter information for later mapping to SSA block parameters.
    pub(super) fn new(
        func: &sem::FuncDef,
        def_table: &DefTable,
        type_map: &'a TypeMap,
        lowering_plans: &'a HashMap<NodeId, sem::LoweringPlan>,
        globals: &'g mut GlobalArena,
    ) -> Self {
        let mut type_lowerer = TypeLowerer::new(type_map);

        // Look up the function's type to extract parameter and return types.
        let def = def_table
            .lookup_def(func.def_id)
            .unwrap_or_else(|| panic!("ssa lower_func missing def {:?}", func.def_id));
        let func_ty = type_map
            .lookup_def_type(def)
            .unwrap_or_else(|| panic!("ssa lower_func missing def type {:?}", func.def_id));
        let ret_ty = match func_ty {
            Type::Fn { ret_ty, .. } => ret_ty,
            other => panic!("ssa lower_func expected fn type, found {:?}", other),
        };

        // Convert each parameter to SSA types. Only `in` mode is supported for now.
        let mut param_defs = Vec::with_capacity(func.sig.params.len());
        let mut param_tys = Vec::with_capacity(func.sig.params.len());
        for param in &func.sig.params {
            if param.mode != ParamMode::In {
                panic!(
                    "ssa lower_func only supports in params, found {:?} for {:?}",
                    param.mode, param.ident
                );
            }
            let def = def_table
                .lookup_def(param.def_id)
                .unwrap_or_else(|| panic!("ssa lower_func missing param def {:?}", param.def_id));
            let param_ty = type_map
                .lookup_def_type(def)
                .unwrap_or_else(|| panic!("ssa lower_func missing param type {:?}", param.def_id));
            let param_ty_id = type_lowerer.lower_type(&param_ty);
            param_defs.push(param.def_id);
            param_tys.push(param_ty_id);
        }

        // Build the SSA function signature and initialize the builder.
        let ret_id = type_lowerer.lower_type(&ret_ty);
        let sig = FunctionSig {
            params: param_tys.clone(),
            ret: ret_id,
        };
        let builder = FunctionBuilder::new(func.def_id, func.sig.name.clone(), sig);
        Self {
            type_map,
            type_lowerer,
            builder,
            locals: LocalMap::new(),
            lowering_plans,
            param_defs,
            param_tys,
            loop_stack: Vec::new(),
            globals,
        }
    }

    /// Creates a new function lowerer for a method definition.
    ///
    /// Methods include an explicit `self` parameter that is lowered as the
    /// first SSA parameter before the user-declared parameters.
    pub(super) fn new_method(
        type_name: &str,
        method_def: &sem::MethodDef,
        def_table: &DefTable,
        type_map: &'a TypeMap,
        lowering_plans: &'a HashMap<NodeId, sem::LoweringPlan>,
        globals: &'g mut GlobalArena,
    ) -> Self {
        let mut type_lowerer = TypeLowerer::new(type_map);

        let ret_ty = type_map.lookup_node_type(method_def.id).unwrap_or_else(|| {
            panic!(
                "ssa lower_method missing return type for {:?}",
                method_def.id
            )
        });

        // Lower the explicit `self` parameter first.
        if method_def.sig.self_param.mode != ParamMode::In {
            panic!(
                "ssa lower_method only supports in self param, found {:?}",
                method_def.sig.self_param.mode
            );
        }
        let self_def = def_table
            .lookup_def(method_def.sig.self_param.def_id)
            .unwrap_or_else(|| {
                panic!(
                    "ssa lower_method missing self def {:?}",
                    method_def.sig.self_param.def_id
                )
            });
        let self_ty = type_map.lookup_def_type(self_def).unwrap_or_else(|| {
            panic!(
                "ssa lower_method missing self type {:?}",
                method_def.sig.self_param.def_id
            )
        });
        let self_ty_id = type_lowerer.lower_type(&self_ty);

        let mut param_defs = Vec::with_capacity(method_def.sig.params.len() + 1);
        let mut param_tys = Vec::with_capacity(method_def.sig.params.len() + 1);
        param_defs.push(method_def.sig.self_param.def_id);
        param_tys.push(self_ty_id);

        // Convert each method parameter to SSA types. Only `in` mode is supported for now.
        for param in &method_def.sig.params {
            if param.mode != ParamMode::In {
                panic!(
                    "ssa lower_method only supports in params, found {:?} for {:?}",
                    param.mode, param.ident
                );
            }
            let def = def_table
                .lookup_def(param.def_id)
                .unwrap_or_else(|| panic!("ssa lower_method missing param def {:?}", param.def_id));
            let param_ty = type_map.lookup_def_type(def).unwrap_or_else(|| {
                panic!("ssa lower_method missing param type {:?}", param.def_id)
            });
            let param_ty_id = type_lowerer.lower_type(&param_ty);
            param_defs.push(param.def_id);
            param_tys.push(param_ty_id);
        }

        // Build the SSA function signature and initialize the builder.
        let ret_id = type_lowerer.lower_type(&ret_ty);
        let sig = FunctionSig {
            params: param_tys.clone(),
            ret: ret_id,
        };
        let name = format!("{type_name}${}", method_def.sig.name);
        let builder = FunctionBuilder::new(method_def.def_id, name, sig);
        Self {
            type_map,
            type_lowerer,
            builder,
            locals: LocalMap::new(),
            lowering_plans,
            param_defs,
            param_tys,
            loop_stack: Vec::new(),
            globals,
        }
    }

    pub(super) fn finish(self) -> (Function, IrTypeCache) {
        (self.builder.finish(), self.type_lowerer.ir_type_cache)
    }

    /// Registers a byte blob as a global and returns its ID.
    pub(super) fn add_global_bytes(&mut self, bytes: Vec<u8>) -> GlobalId {
        self.globals.add_bytes(bytes)
    }

    /// Lowers a value expression by consulting its precomputed lowering plan.
    ///
    /// Linear expressions are lowered in the current block. Branching expressions
    /// delegate to the multi-block lowering path.
    pub(super) fn lower_value_expr(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<BranchResult, LoweringError> {
        let plan = self
            .lowering_plans
            .get(&expr.id)
            .unwrap_or_else(|| panic!("ssa lower_func missing lowering plan {:?}", expr.id));

        match plan {
            sem::LoweringPlan::Linear => {
                // The plan guarantees linearity; any failure here is a compiler bug.
                let value = self.lower_linear_value_expr(expr).unwrap_or_else(|err| {
                    panic!(
                        "ssa lower_func lowering plan mismatch {:?}: {:?}",
                        expr.id, err
                    )
                });
                Ok(BranchResult::Value(value))
            }
            sem::LoweringPlan::Branching => self.lower_branching_value_expr(expr),
        }
    }
}
