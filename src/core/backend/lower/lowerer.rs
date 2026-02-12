//! SSA lowering state and shared helpers.

use crate::backend::lower::LowerToIrError;
use crate::backend::lower::drop_glue::DropGlueRegistry;
use crate::backend::lower::drops::DropTracker;
use crate::backend::lower::globals::GlobalArena;
use crate::backend::lower::locals::{LocalMap, LocalValue};
use crate::backend::lower::types::TypeLowerer;
use crate::ir::builder::FunctionBuilder;
use crate::ir::{BlockId, Function, FunctionSig, GlobalId, IrTypeCache, IrTypeId, ValueId};
use crate::resolve::{Def, DefId, DefTable};
use crate::tree::NodeId;
use crate::tree::ParamMode;
use crate::tree::format_compact::{
    format_semantic_stmt_compact, format_semantic_value_expr_compact,
};
use crate::tree::semantic as sem;
use crate::typecheck::type_map::TypeMap;
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

impl From<ValueId> for BranchResult {
    fn from(value: ValueId) -> Self {
        BranchResult::Value(value)
    }
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
    pub(super) locals: Vec<LocalValue>,
}

/// Main state for lowering a single function to SSA IR.
///
/// Tracks:
/// - Type lowering context for converting types
/// - SSA function builder for emitting instructions
/// - Current SSA values for local variables (updated on assignment/join)
/// - Expression plans from semantic analysis (linear vs branching)
pub(super) struct FuncLowerer<'a, 'g> {
    pub(crate) def_table: &'a DefTable,
    pub(super) type_lowerer: TypeLowerer<'a>,
    pub(crate) type_map: &'a TypeMap,
    pub(super) ret_ty: Type,
    pub(super) builder: FunctionBuilder,
    /// Maps definition IDs to their current SSA values (mutable during lowering).
    pub(super) locals: LocalMap,
    pub(super) lowering_plans: &'a sem::LoweringPlanMap,
    pub(super) param_defs: Vec<DefId>,
    pub(super) param_tys: Vec<IrTypeId>,
    pub(super) param_modes: Vec<ParamMode>,
    pub(super) loop_stack: Vec<LoopContext>,
    pub(super) drop_tracker: DropTracker<'a>,
    pub(super) drop_glue: &'g mut DropGlueRegistry,
    pub(super) globals: &'g mut GlobalArena,
    pub(super) trace_drops: bool,
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
    /// Looks up a definition by id, panicking with SSA-specific context on failure.
    pub(super) fn def(&self, def_id: DefId) -> &Def {
        self.def_table
            .lookup_def(def_id)
            .unwrap_or_else(|| panic!("backend missing def {:?}", def_id))
    }

    /// Looks up a definition's semantic type, panicking on missing entries.
    pub(super) fn def_type(&self, def_id: DefId) -> Type {
        self.type_map
            .lookup_def_type(self.def(def_id))
            .unwrap_or_else(|| panic!("backend missing def type {:?}", def_id))
    }

    /// Fetches the lowering plan for a node.
    pub(super) fn lowering_plan(&self, node_id: NodeId) -> sem::LoweringPlan {
        self.lowering_plans
            .lookup_value_plan(node_id)
            .unwrap_or_else(|| panic!("backend missing lowering plan {:?}", node_id))
    }

    /// Fetches a call plan for a node.
    pub(super) fn call_plan(&self, node_id: NodeId) -> sem::CallPlan {
        self.lowering_plans
            .lookup_call_plan(node_id)
            .unwrap_or_else(|| panic!("backend missing call plan {:?}", node_id))
    }

    /// Fetches an index plan for a node.
    pub(super) fn index_plan(&self, node_id: NodeId) -> sem::IndexPlan {
        self.lowering_plans
            .lookup_index_plan(node_id)
            .unwrap_or_else(|| panic!("backend missing index plan {:?}", node_id))
    }

    /// Fetches a match plan for a node.
    pub(super) fn match_plan(&self, node_id: NodeId) -> sem::MatchPlan {
        self.lowering_plans
            .lookup_match_plan(node_id)
            .unwrap_or_else(|| panic!("backend missing match plan {:?}", node_id))
    }

    /// Fetches a slice plan for a node.
    pub(super) fn slice_plan(&self, node_id: NodeId) -> sem::SlicePlan {
        self.lowering_plans
            .lookup_slice_plan(node_id)
            .unwrap_or_else(|| panic!("backend missing slice plan {:?}", node_id))
    }

    /// Creates a new function lowerer for the given semantic function definition.
    ///
    /// Initializes the type context, extracts the function signature, and prepares
    /// parameter information for later mapping to SSA block parameters.
    pub(super) fn new(
        func: &sem::FuncDef,
        def_table: &'a DefTable,
        module: Option<&'a sem::Module>,
        type_map: &'a TypeMap,
        lowering_plans: &'a sem::LoweringPlanMap,
        drop_glue: &'g mut DropGlueRegistry,
        globals: &'g mut GlobalArena,
        trace_drops: bool,
    ) -> Self {
        let mut type_lowerer = TypeLowerer::new_with_type_defs(type_map, Some(def_table), module);

        // Look up the function's type to extract parameter and return types.
        let def = def_table
            .lookup_def(func.def_id)
            .unwrap_or_else(|| panic!("backend lower_func missing def {:?}", func.def_id));
        let func_ty = type_map
            .lookup_def_type(def)
            .unwrap_or_else(|| panic!("backend lower_func missing def type {:?}", func.def_id));
        let ret_ty = match func_ty {
            Type::Fn { ret_ty, .. } => ret_ty,
            other => panic!("backend lower_func expected fn type, found {:?}", other),
        };

        // Convert each parameter to SSA types. Only `in` mode is supported for now.
        let mut param_defs = Vec::with_capacity(func.sig.params.len());
        let mut param_tys = Vec::with_capacity(func.sig.params.len());
        let mut param_modes = Vec::with_capacity(func.sig.params.len());
        for param in &func.sig.params {
            let def = def_table.lookup_def(param.def_id).unwrap_or_else(|| {
                panic!("backend lower_func missing param def {:?}", param.def_id)
            });
            let param_ty = type_map.lookup_def_type(def).unwrap_or_else(|| {
                panic!("backend lower_func missing param type {:?}", param.def_id)
            });
            let param_ty_id = match param.mode {
                ParamMode::In | ParamMode::Sink => {
                    let value_ty = type_lowerer.lower_type(&param_ty);
                    if param_ty.is_scalar() {
                        value_ty
                    } else {
                        type_lowerer.ptr_to(value_ty)
                    }
                }
                ParamMode::Out | ParamMode::InOut => {
                    let value_ty = type_lowerer.lower_type(&param_ty);
                    type_lowerer.ptr_to(value_ty)
                }
            };
            param_defs.push(param.def_id);
            param_tys.push(param_ty_id);
            param_modes.push(param.mode.clone());
        }

        // Build the SSA function signature and initialize the builder.
        let ret_id = type_lowerer.lower_type(&ret_ty);
        let sig = FunctionSig {
            params: param_tys.clone(),
            ret: ret_id,
        };
        let builder = FunctionBuilder::new(func.def_id, func.sig.name.clone(), sig);
        Self {
            def_table,
            type_map,
            type_lowerer,
            ret_ty: (*ret_ty).clone(),
            builder,
            locals: LocalMap::new(),
            lowering_plans,
            param_defs,
            param_tys,
            param_modes,
            loop_stack: Vec::new(),
            drop_tracker: DropTracker::new(),
            drop_glue,
            globals,
            trace_drops,
        }
    }

    /// Creates a new function lowerer for a method definition.
    ///
    /// Methods include an explicit `self` parameter that is lowered as the
    /// first SSA parameter before the user-declared parameters.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn new_method(
        type_name: &str,
        method_def: &sem::MethodDef,
        def_table: &'a DefTable,
        module: &'a sem::Module,
        type_map: &'a TypeMap,
        lowering_plans: &'a sem::LoweringPlanMap,
        drop_glue: &'g mut DropGlueRegistry,
        globals: &'g mut GlobalArena,
        trace_drops: bool,
    ) -> Self {
        let mut type_lowerer =
            TypeLowerer::new_with_type_defs(type_map, Some(def_table), Some(module));

        let ret_ty = type_map.lookup_node_type(method_def.id).unwrap_or_else(|| {
            panic!(
                "backend lower_method missing return type for {:?}",
                method_def.id
            )
        });

        // Lower the explicit `self` parameter first.
        let self_def = def_table
            .lookup_def(method_def.sig.self_param.def_id)
            .unwrap_or_else(|| {
                panic!(
                    "backend lower_method missing self def {:?}",
                    method_def.sig.self_param.def_id
                )
            });
        let self_ty = type_map.lookup_def_type(self_def).unwrap_or_else(|| {
            panic!(
                "backend lower_method missing self type {:?}",
                method_def.sig.self_param.def_id
            )
        });
        let self_ty_id = match method_def.sig.self_param.mode {
            ParamMode::In | ParamMode::Sink => {
                let value_ty = type_lowerer.lower_type(&self_ty);
                if self_ty.is_scalar() {
                    value_ty
                } else {
                    type_lowerer.ptr_to(value_ty)
                }
            }
            ParamMode::Out | ParamMode::InOut => {
                let value_ty = type_lowerer.lower_type(&self_ty);
                type_lowerer.ptr_to(value_ty)
            }
        };

        let mut param_defs = Vec::with_capacity(method_def.sig.params.len() + 1);
        let mut param_tys = Vec::with_capacity(method_def.sig.params.len() + 1);
        let mut param_modes = Vec::with_capacity(method_def.sig.params.len() + 1);
        param_defs.push(method_def.sig.self_param.def_id);
        param_tys.push(self_ty_id);
        param_modes.push(method_def.sig.self_param.mode.clone());

        // Convert each method parameter to SSA types.
        for param in &method_def.sig.params {
            let def = def_table.lookup_def(param.def_id).unwrap_or_else(|| {
                panic!("backend lower_method missing param def {:?}", param.def_id)
            });
            let param_ty = type_map.lookup_def_type(def).unwrap_or_else(|| {
                panic!("backend lower_method missing param type {:?}", param.def_id)
            });
            let param_ty_id = match param.mode {
                ParamMode::In | ParamMode::Sink => {
                    let value_ty = type_lowerer.lower_type(&param_ty);
                    if param_ty.is_scalar() {
                        value_ty
                    } else {
                        type_lowerer.ptr_to(value_ty)
                    }
                }
                ParamMode::Out | ParamMode::InOut => {
                    let value_ty = type_lowerer.lower_type(&param_ty);
                    type_lowerer.ptr_to(value_ty)
                }
            };
            param_defs.push(param.def_id);
            param_tys.push(param_ty_id);
            param_modes.push(param.mode.clone());
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
            def_table,
            type_map,
            type_lowerer,
            ret_ty: ret_ty.clone(),
            builder,
            locals: LocalMap::new(),
            lowering_plans,
            param_defs,
            param_tys,
            param_modes,
            loop_stack: Vec::new(),
            drop_tracker: DropTracker::new(),
            drop_glue,
            globals,
            trace_drops,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn new_drop_glue(
        def_id: DefId,
        name: String,
        param_ty: Type,
        def_table: &'a DefTable,
        type_map: &'a TypeMap,
        lowering_plans: &'a sem::LoweringPlanMap,
        drop_glue: &'g mut DropGlueRegistry,
        globals: &'g mut GlobalArena,
        trace_drops: bool,
    ) -> Self {
        let mut type_lowerer = TypeLowerer::new(type_map);
        let param_ir = type_lowerer.lower_type(&param_ty);
        let param_ptr = type_lowerer.ptr_to(param_ir);
        let unit_ty = type_lowerer.lower_type(&Type::Unit);
        let sig = FunctionSig {
            params: vec![param_ptr],
            ret: unit_ty,
        };
        let builder = FunctionBuilder::new(def_id, name, sig);

        Self {
            def_table,
            type_map,
            type_lowerer,
            ret_ty: Type::Unit,
            builder,
            locals: LocalMap::new(),
            lowering_plans,
            param_defs: Vec::new(),
            param_tys: vec![param_ptr],
            param_modes: vec![ParamMode::In],
            loop_stack: Vec::new(),
            drop_tracker: DropTracker::new(),
            drop_glue,
            globals,
            trace_drops,
        }
    }

    /// Initializes locals for parameters using their declared modes.
    pub(super) fn init_param_locals(&mut self, params: &[ValueId]) {
        if params.len() != self.param_defs.len() {
            panic!(
                "backend param locals mismatch: {} defs, {} params",
                self.param_defs.len(),
                params.len()
            );
        }

        self.locals = LocalMap::new();
        for (index, value) in params.iter().enumerate() {
            let def_id = self.param_defs[index];
            let mode = self.param_modes[index].clone();
            let param_ty = self.def_type(def_id);
            let value_ty = self.type_lowerer.lower_type(&param_ty);
            let local = match mode {
                ParamMode::In | ParamMode::Sink => {
                    if param_ty.is_scalar() {
                        LocalValue::value(*value, value_ty)
                    } else {
                        let slot = self.alloc_value_slot(value_ty);
                        let layout = self.type_lowerer.ir_type_cache.layout(value_ty);
                        let len_ty = self.type_lowerer.lower_type(&Type::uint(64));
                        let len = self
                            .builder
                            .const_int(layout.size() as i128, false, 64, len_ty);
                        self.builder.memcopy(slot.addr, *value, len);
                        LocalValue::addr(slot.addr, value_ty)
                    }
                }
                ParamMode::Out | ParamMode::InOut => LocalValue::addr(*value, value_ty),
            };
            self.locals.insert(def_id, local);
            if matches!(mode, ParamMode::Sink) {
                self.set_drop_flag_for_def(def_id, true);
            }
        }
    }

    pub(super) fn finish(self) -> (Function, IrTypeCache) {
        (self.builder.finish(), self.type_lowerer.ir_type_cache)
    }

    pub(super) fn annotate_stmt(&mut self, stmt: &sem::StmtExpr) {
        self.builder
            .annotate_next_inst(format_semantic_stmt_compact(stmt));
    }

    pub(super) fn annotate_expr(&mut self, expr: &sem::ValueExpr) {
        self.builder
            .annotate_next_inst(format_semantic_value_expr_compact(expr));
    }

    pub(super) fn trace_drop(&mut self, message: impl Into<String>) {
        if self.trace_drops {
            self.builder.annotate_next_inst(message);
        }
    }

    pub(super) fn param_mode_for(&self, def_id: DefId) -> Option<ParamMode> {
        self.param_defs
            .iter()
            .position(|id| *id == def_id)
            .map(|idx| self.param_modes[idx].clone())
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
    ) -> Result<BranchResult, LowerToIrError> {
        match self.lowering_plan(expr.id) {
            sem::LoweringPlan::Linear => {
                // The plan guarantees linearity; any failure here is a compiler bug.
                let value = self.lower_linear_value_expr(expr).unwrap_or_else(|err| {
                    panic!(
                        "backend lower_func lowering plan mismatch {:?}: {:?}",
                        expr.id, err
                    )
                });
                Ok(BranchResult::Value(value))
            }
            sem::LoweringPlan::Branching => self.lower_branching_value_expr(expr),
        }
    }

    /// Lowers a value expression and returns `None` if it forces an early return.
    ///
    /// This is used by linear lowering to evaluate subexpressions that may contain
    /// branching control-flow (e.g. `if`), without duplicating the linear logic.
    pub(super) fn lower_value_expr_opt(
        &mut self,
        expr: &sem::ValueExpr,
    ) -> Result<Option<ValueId>, LowerToIrError> {
        match self.lower_value_expr(expr)? {
            BranchResult::Value(value) => Ok(Some(value)),
            BranchResult::Return => Ok(None),
        }
    }
}
