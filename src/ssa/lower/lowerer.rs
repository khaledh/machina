//! SSA lowering state and shared helpers.

use std::collections::HashMap;

use crate::diag::Span;
use crate::resolve::{DefId, DefTable};
use crate::ssa::lower::globals::GlobalArena;
use crate::ssa::lower::locals::{LocalMap, LocalStorage, LocalValue};
use crate::ssa::lower::types::TypeLowerer;
use crate::ssa::lower::{LoweringError, LoweringErrorKind};
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{
    BinOp, BlockId, Callee, CastKind, CmpOp, Function, FunctionSig, GlobalId, RuntimeFn,
    Terminator, ValueId,
};
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
                let value = self.lower_value_expr_linear(expr).unwrap_or_else(|err| {
                    panic!(
                        "ssa lower_func lowering plan mismatch {:?}: {:?}",
                        expr.id, err
                    )
                });
                Ok(BranchResult::Value(value))
            }
            sem::LoweringPlan::Branching => self.lower_branching_expr(expr),
        }
    }

    pub(super) fn lookup_local(&self, def_id: DefId) -> LocalValue {
        self.locals
            .get(def_id)
            .unwrap_or_else(|| panic!("ssa lower_func missing local {:?}", def_id))
    }

    /// Returns the innermost loop context, panicking if no loop is active.
    pub(super) fn current_loop(&self) -> &LoopContext {
        self.loop_stack
            .last()
            .unwrap_or_else(|| panic!("ssa break/continue outside of loop"))
    }

    /// Returns the IR type used to thread a local through control flow.
    ///
    /// Value locals use their value type; address locals use a pointer type.
    pub(super) fn local_storage_ty(&mut self, local: LocalValue) -> IrTypeId {
        match local.storage {
            LocalStorage::Value(_) => local.value_ty,
            LocalStorage::Addr(_) => self.type_lowerer.ptr_to(local.value_ty),
        }
    }

    /// Allocates a new local of the given value type and returns its address.
    pub(super) fn alloc_local_addr(&mut self, value_ty: IrTypeId) -> ValueId {
        let local_id = self.builder.add_local(value_ty, None);
        let ptr_ty = self.type_lowerer.ptr_to(value_ty);
        self.builder.addr_of_local(local_id, ptr_ty)
    }

    /// Computes a typed field address for a struct/tuple value.
    pub(super) fn field_addr_typed(
        &mut self,
        base: ValueId,
        index: usize,
        field_ty: IrTypeId,
    ) -> ValueId {
        let ptr_ty = self.type_lowerer.ptr_to(field_ty);
        self.builder.field_addr(base, index, ptr_ty)
    }

    /// Allocates a local slot for an aggregate value.
    pub(super) fn alloc_value_slot(&mut self, ty: IrTypeId) -> ValueSlot {
        let addr = self.alloc_local_addr(ty);
        ValueSlot { addr, ty }
    }

    /// Materializes a value into a local slot for address-based access.
    pub(super) fn materialize_value_slot(&mut self, value: ValueId, ty: IrTypeId) -> ValueSlot {
        let slot = self.alloc_value_slot(ty);
        self.builder.store(slot.addr, value);
        slot
    }

    /// Loads a value from a slot.
    pub(super) fn load_slot(&mut self, slot: &ValueSlot) -> ValueId {
        self.builder.load(slot.addr, slot.ty)
    }

    /// Loads a field value from a base aggregate.
    pub(super) fn load_field(
        &mut self,
        base: ValueId,
        index: usize,
        field_ty: IrTypeId,
    ) -> ValueId {
        let addr = self.field_addr_typed(base, index, field_ty);
        self.builder.load(addr, field_ty)
    }

    /// Stores a field value into a base aggregate.
    pub(super) fn store_field(
        &mut self,
        base: ValueId,
        index: usize,
        field_ty: IrTypeId,
        value: ValueId,
    ) {
        let addr = self.field_addr_typed(base, index, field_ty);
        self.builder.store(addr, value);
    }

    /// Loads ptr/len from a slice base stored at `base_addr`.
    pub(super) fn load_slice_view(
        &mut self,
        base_addr: ValueId,
        elem_ptr_ty: IrTypeId,
    ) -> BaseView {
        let len_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let ptr = self.load_field(base_addr, 0, elem_ptr_ty);
        let len = self.load_field(base_addr, 1, len_ty);
        BaseView { ptr, len }
    }

    /// Loads ptr/len from a string base stored at `base_addr`.
    pub(super) fn load_string_view(&mut self, base_addr: ValueId) -> BaseView {
        let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
        let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);
        let ptr = self.load_field(base_addr, 0, u8_ptr_ty);

        // String lengths are u32; widen to u64 for bounds checks and indexing.
        let len_u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
        let len_u32 = self.load_field(base_addr, 1, len_u32_ty);
        let len_u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let len = self
            .builder
            .cast(CastKind::IntExtend { signed: false }, len_u32, len_u64_ty);

        BaseView { ptr, len }
    }

    /// Builds ptr/len for array bases using a constant length.
    pub(super) fn load_array_view(
        &mut self,
        base_addr: ValueId,
        elem_ptr_ty: IrTypeId,
        len: u64,
    ) -> BaseView {
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let zero = self.builder.const_int(0, false, 64, u64_ty);
        let ptr = self.builder.index_addr(base_addr, zero, elem_ptr_ty);
        let len_val = self.builder.const_int(len as i128, false, 64, u64_ty);
        BaseView { ptr, len: len_val }
    }

    /// Emits an index address with bounds checking.
    pub(super) fn index_with_bounds(
        &mut self,
        view: BaseView,
        index: ValueId,
        elem_ptr_ty: IrTypeId,
    ) -> ValueId {
        self.emit_bounds_check(index, view.len);
        self.builder.index_addr(view.ptr, index, elem_ptr_ty)
    }

    /// Emits a bounds check guard that traps if `index >= len`.
    pub(super) fn emit_bounds_check(&mut self, index: ValueId, len: ValueId) {
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let cond = self.builder.cmp(CmpOp::Lt, index, len, bool_ty);

        let ok_bb = self.builder.add_block();
        let trap_bb = self.builder.add_block();

        // Split control flow on the bounds predicate.
        self.builder.terminate(Terminator::CondBr {
            cond,
            then_bb: ok_bb,
            then_args: Vec::new(),
            else_bb: trap_bb,
            else_args: Vec::new(),
        });

        // Trap on out-of-bounds.
        self.builder.select_block(trap_bb);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let kind = self.builder.const_int(1, false, 64, u64_ty);
        let zero = self.builder.const_int(0, false, 64, u64_ty);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::Trap),
            vec![kind, index, len, zero],
            unit_ty,
        );
        self.builder.terminate(Terminator::Unreachable);

        // Continue lowering in the in-bounds block.
        self.builder.select_block(ok_bb);
    }

    /// Emits a range check guard that traps if `value` is outside [min, max_excl).
    pub(super) fn emit_range_check(&mut self, value: ValueId, min: ValueId, max_excl: ValueId) {
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let ge_min = self.builder.cmp(CmpOp::Ge, value, min, bool_ty);
        let lt_max = self.builder.cmp(CmpOp::Lt, value, max_excl, bool_ty);
        let in_range = self.builder.binop(BinOp::And, ge_min, lt_max, bool_ty);

        let ok_bb = self.builder.add_block();
        let trap_bb = self.builder.add_block();

        // Split control flow on the range predicate.
        self.builder.terminate(Terminator::CondBr {
            cond: in_range,
            then_bb: ok_bb,
            then_args: Vec::new(),
            else_bb: trap_bb,
            else_args: Vec::new(),
        });

        // Trap on out-of-range values.
        self.builder.select_block(trap_bb);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let kind = self.builder.const_int(2, false, 64, u64_ty);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::Trap),
            vec![kind, value, min, max_excl],
            unit_ty,
        );
        self.builder.terminate(Terminator::Unreachable);

        // Continue lowering in the in-range block.
        self.builder.select_block(ok_bb);
    }

    /// Resolves a place to its base address after peeling heap/ref indirections.
    pub(super) fn resolve_deref_base(
        &mut self,
        target: &sem::PlaceExpr,
        deref_count: usize,
    ) -> Result<(ValueId, Type), LoweringError> {
        let mut base = self.lower_place_addr(target)?;
        let mut curr_ty = self.type_map.type_table().get(target.ty).clone();

        for _ in 0..deref_count {
            let elem_ty = match curr_ty {
                Type::Heap { elem_ty } | Type::Ref { elem_ty, .. } => elem_ty,
                other => panic!("ssa resolve_deref_base on non-heap/ref {:?}", other),
            };

            let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
            let ptr_ir_ty = self.type_lowerer.ptr_to(elem_ir_ty);
            base.addr = self.builder.load(base.addr, ptr_ir_ty);
            curr_ty = (*elem_ty).clone();
        }

        Ok((base.addr, curr_ty))
    }

    /// Resolves a value holding heap/ref pointers to its base address.
    pub(super) fn resolve_deref_base_value(
        &mut self,
        value: ValueId,
        ty: Type,
        deref_count: usize,
    ) -> (ValueId, Type) {
        let mut base_addr = value;
        let mut curr_ty = ty;

        for i in 0..deref_count {
            let elem_ty = match curr_ty {
                Type::Heap { elem_ty } | Type::Ref { elem_ty, .. } => elem_ty,
                other => panic!("ssa resolve_deref_base_value on non-heap/ref {:?}", other),
            };

            if i > 0 {
                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                let ptr_ir_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                base_addr = self.builder.load(base_addr, ptr_ir_ty);
            }

            curr_ty = (*elem_ty).clone();
        }

        (base_addr, curr_ty)
    }

    /// Ensures a local has an addressable slot and returns its address.
    pub(super) fn ensure_local_addr(&mut self, def_id: DefId, value_ty: IrTypeId) -> ValueId {
        let local = self.lookup_local(def_id);
        if local.value_ty != value_ty {
            panic!(
                "ssa ensure_local_addr type mismatch for {:?}: {:?} vs {:?}",
                def_id, local.value_ty, value_ty
            );
        }

        match local.storage {
            LocalStorage::Addr(addr) => addr,
            LocalStorage::Value(value) => {
                let addr = self.alloc_local_addr(value_ty);
                self.builder.store(addr, value);
                self.locals.insert(def_id, LocalValue::addr(addr, value_ty));
                addr
            }
        }
    }

    /// Loads a local's current value, emitting a load when stored in memory.
    pub(super) fn load_local_value(&mut self, def_id: DefId) -> ValueId {
        let local = self.lookup_local(def_id);
        match local.storage {
            LocalStorage::Value(value) => value,
            LocalStorage::Addr(addr) => self.builder.load(addr, local.value_ty),
        }
    }

    /// Assigns a new value to a local, storing when it is address-taken.
    pub(super) fn assign_local_value(&mut self, def_id: DefId, value: ValueId, value_ty: IrTypeId) {
        let local = self.lookup_local(def_id);
        if local.value_ty != value_ty {
            panic!(
                "ssa assign_local_value type mismatch for {:?}: {:?} vs {:?}",
                def_id, local.value_ty, value_ty
            );
        }

        match local.storage {
            LocalStorage::Value(_) => {
                self.locals
                    .insert(def_id, LocalValue::value(value, value_ty));
            }
            LocalStorage::Addr(addr) => {
                self.builder.store(addr, value);
            }
        }
    }

    /// Extracts ptr+len from a string/slice value for runtime argument lowering.
    pub(super) fn lower_ptr_len_from_value(
        &mut self,
        span: Span,
        value: ValueId,
        ty: &Type,
        len_bits: u8,
    ) -> Result<(ValueId, ValueId), LoweringError> {
        let (ptr_ty, len_ty) = match ty {
            Type::String => {
                if len_bits != 32 {
                    panic!("ssa ptr/len lowering invalid len_bits {len_bits} for string");
                }
                let byte_ty = self.type_lowerer.lower_type(&Type::uint(8));
                let ptr_ty = self.type_lowerer.ptr_to(byte_ty);
                let len_ty = self.type_lowerer.lower_type(&Type::uint(32));
                (ptr_ty, len_ty)
            }
            Type::Slice { elem_ty } => {
                if len_bits != 64 {
                    panic!("ssa ptr/len lowering invalid len_bits {len_bits} for slice");
                }
                let elem_ir = self.type_lowerer.lower_type(elem_ty);
                let ptr_ty = self.type_lowerer.ptr_to(elem_ir);
                let len_ty = self.type_lowerer.lower_type(&Type::uint(64));
                (ptr_ty, len_ty)
            }
            _ => return Err(self.err_span(span, LoweringErrorKind::UnsupportedExpr)),
        };

        let value_ty = self.type_lowerer.lower_type(ty);

        // Materialize the aggregate into a local to address its fields.
        let slot = self.materialize_value_slot(value, value_ty);

        let ptr_val = self.load_field(slot.addr, 0, ptr_ty);
        let mut len_val = self.load_field(slot.addr, 1, len_ty);
        if len_bits == 32 {
            let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
            len_val = self.builder.cast(
                crate::ssa::model::ir::CastKind::IntExtend { signed: false },
                len_val,
                u64_ty,
            );
        }

        Ok((ptr_val, len_val))
    }

    pub(super) fn byte_offset_addr(&mut self, base: ValueId, offset: u64) -> ValueId {
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let u8_ty = self.type_lowerer.lower_type(&Type::uint(8));
        let u8_ptr_ty = self.type_lowerer.ptr_to(u8_ty);

        // base is a ptr<blob>; we treat it as ptr<u8> for indexing
        // TODO: add proper pointer cast instruction in IR
        let base_u8 = base; // no bitcast in IR yet; assume ptr is opaque
        let offset_val = self.builder.const_int(offset as i128, false, 64, u64_ty);
        self.builder.index_addr(base_u8, offset_val, u8_ptr_ty)
    }

    pub(super) fn store_into_blob(
        &mut self,
        blob_ptr: ValueId,
        offset: u64,
        value: ValueId,
        value_ty: IrTypeId,
    ) {
        // Store the value into a temporary local
        let temp_ptr = self.alloc_local_addr(value_ty);
        self.builder.store(temp_ptr, value);

        // Copy the value into the blob at the given offset and length
        let dst = self.byte_offset_addr(blob_ptr, offset);

        let layout = self.type_lowerer.ir_type_cache.layout(value_ty);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let len = self
            .builder
            .const_int(layout.size() as i128, false, 64, u64_ty);

        self.builder.memcopy(dst, temp_ptr, len);
    }

    pub(super) fn err_span(&self, span: Span, kind: LoweringErrorKind) -> LoweringError {
        LoweringError { kind, span }
    }

    pub(super) fn err_stmt(&self, stmt: &sem::StmtExpr, kind: LoweringErrorKind) -> LoweringError {
        LoweringError {
            kind,
            span: stmt.span,
        }
    }
}
