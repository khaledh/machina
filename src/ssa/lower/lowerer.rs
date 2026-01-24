//! SSA lowering state and shared helpers.

use std::collections::HashMap;

use crate::diag::Span;
use crate::resolve::{DefId, DefTable};
use crate::ssa::IrTypeId;
use crate::ssa::lower::locals::{LocalMap, LocalStorage, LocalValue};
use crate::ssa::lower::types::TypeLowerer;
use crate::ssa::lower::{LoweredFunction, LoweringError, LoweringErrorKind};
use crate::ssa::model::builder::FunctionBuilder;
use crate::ssa::model::ir::{FunctionSig, ValueId};
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

/// Main state for lowering a single function to SSA IR.
///
/// Tracks:
/// - Type lowering context for converting types
/// - SSA function builder for emitting instructions
/// - Current SSA values for local variables (updated on assignment/join)
/// - Expression plans from semantic analysis (linear vs branching)
pub(super) struct FuncLowerer<'a> {
    pub(super) type_lowerer: TypeLowerer<'a>,
    pub(crate) type_map: &'a TypeMap,
    pub(super) builder: FunctionBuilder,
    /// Maps definition IDs to their current SSA values (mutable during lowering).
    pub(super) locals: LocalMap,
    pub(super) lowering_plans: &'a HashMap<NodeId, sem::LoweringPlan>,
    pub(super) param_defs: Vec<DefId>,
    pub(super) param_tys: Vec<IrTypeId>,
}

impl<'a> FuncLowerer<'a> {
    /// Creates a new function lowerer for the given semantic function definition.
    ///
    /// Initializes the type context, extracts the function signature, and prepares
    /// parameter information for later mapping to SSA block parameters.
    pub(super) fn new(
        func: &sem::FuncDef,
        def_table: &DefTable,
        type_map: &'a TypeMap,
        lowering_plans: &'a HashMap<NodeId, sem::LoweringPlan>,
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
        }
    }

    pub(super) fn finish(self) -> LoweredFunction {
        LoweredFunction {
            func: self.builder.finish(),
            types: self.type_lowerer.ir_type_cache,
        }
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
