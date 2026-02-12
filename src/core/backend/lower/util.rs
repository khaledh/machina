//! Shared lowering helpers for locals, views, bounds, and blob operations.

use crate::core::backend::lower::LowerToIrError;
use crate::core::backend::lower::locals::{LocalStorage, LocalValue};
use crate::core::backend::lower::lowerer::{BaseView, FuncLowerer, LoopContext};
use crate::core::diag::Span;
use crate::core::ir::{
    BinOp, Callee, CastKind, CmpOp, IrTypeId, IrTypeKind, RuntimeFn, Terminator, ValueId,
};
use crate::core::resolve::DefId;
use crate::core::tree::semantic as sem;
use crate::core::types::{Type, TypeAssignability, type_assignable};

impl<'a, 'g> FuncLowerer<'a, 'g> {
    pub(super) fn lookup_local(&self, def_id: DefId) -> LocalValue {
        self.locals
            .get(def_id)
            .unwrap_or_else(|| panic!("backend lower_func missing local {:?}", def_id))
    }

    /// Returns the innermost loop context, panicking if no loop is active.
    pub(super) fn current_loop(&self) -> &LoopContext {
        self.loop_stack
            .last()
            .unwrap_or_else(|| panic!("backend break/continue outside of loop"))
    }

    /// Coerces a value into the function's declared return type.
    ///
    /// This currently materializes `ErrorUnion` returns from plain payload
    /// expressions and supports widening a prefix-compatible error union.
    pub(super) fn coerce_return_value(&mut self, value: ValueId, value_ty: &Type) -> ValueId {
        let ret_ty = self.ret_ty.clone();
        self.coerce_value_to_type(value, value_ty, &ret_ty)
    }

    /// Coerces a value from one semantic type into another when assignable.
    ///
    /// This is primarily used at control-flow joins where arm/body values may
    /// be narrower than the join expression type (for example, `T` into `T | E`).
    pub(super) fn coerce_value(&mut self, value: ValueId, from_ty: &Type, to_ty: &Type) -> ValueId {
        self.coerce_value_to_type(value, from_ty, to_ty)
    }

    fn coerce_value_to_type(&mut self, value: ValueId, from_ty: &Type, to_ty: &Type) -> ValueId {
        if from_ty == to_ty {
            return value;
        }

        match to_ty {
            Type::ErrorUnion { ok_ty, err_tys } => {
                self.coerce_to_error_union(value, from_ty, to_ty, ok_ty, err_tys)
            }
            _ => value,
        }
    }

    fn coerce_to_error_union(
        &mut self,
        value: ValueId,
        from_ty: &Type,
        union_ty: &Type,
        ok_ty: &Type,
        err_tys: &[Type],
    ) -> ValueId {
        if from_ty == union_ty {
            return value;
        }

        if let Type::ErrorUnion {
            ok_ty: src_ok_ty,
            err_tys: src_err_tys,
        } = from_ty
        {
            return self.widen_error_union_prefix(
                src_ok_ty,
                src_err_tys,
                ok_ty,
                err_tys,
                from_ty,
                union_ty,
                value,
            );
        }

        let variant_index = self
            .error_union_variant_index(from_ty, ok_ty, err_tys)
            .unwrap_or_else(|| {
                panic!(
                    "backend cannot coerce return value {:?} into error union {:?}",
                    from_ty, union_ty
                )
            });

        self.build_error_union_value(union_ty, variant_index, value)
    }

    fn error_union_variant_index(
        &self,
        from_ty: &Type,
        ok_ty: &Type,
        err_tys: &[Type],
    ) -> Option<usize> {
        if type_assignable(from_ty, ok_ty) != TypeAssignability::Incompatible {
            return Some(0);
        }

        for (idx, err_ty) in err_tys.iter().enumerate() {
            if type_assignable(from_ty, err_ty) != TypeAssignability::Incompatible {
                return Some(idx + 1);
            }
        }

        None
    }

    fn build_error_union_value(
        &mut self,
        union_ty: &Type,
        variant_index: usize,
        payload_value: ValueId,
    ) -> ValueId {
        let union_ty_id = self
            .type_map
            .type_table()
            .lookup_id(union_ty)
            .unwrap_or_else(|| panic!("backend missing type id for error union {:?}", union_ty));
        let (tag_ty, blob_ty, variant_tag, payload_offset, payload_ty) = {
            let layout = self.type_lowerer.enum_layout(union_ty_id);
            let variant = layout.variants.get(variant_index).unwrap_or_else(|| {
                panic!(
                    "backend error union variant index out of range: {} of {}",
                    variant_index,
                    layout.variants.len()
                )
            });
            if variant.field_offsets.len() > 1 || variant.field_tys.len() > 1 {
                panic!(
                    "backend error union variant must carry at most one payload field, found {}",
                    variant.field_tys.len()
                );
            }
            (
                layout.tag_ty,
                layout.blob_ty,
                variant.tag,
                variant.field_offsets.first().copied(),
                variant.field_tys.first().copied(),
            )
        };

        let union_ir_ty = self.type_lowerer.lower_type(union_ty);
        let slot = self.alloc_value_slot(union_ir_ty);

        let tag_val = self
            .builder
            .const_int(variant_tag as i128, false, 32, tag_ty);
        self.store_field(slot.addr, 0, tag_ty, tag_val);

        if let (Some(offset), Some(payload_ty)) = (payload_offset, payload_ty) {
            let payload_ptr = self.field_addr_typed(slot.addr, 1, blob_ty);
            self.store_into_blob(payload_ptr, offset, payload_value, payload_ty);
        }

        self.load_slot(&slot)
    }

    #[allow(clippy::too_many_arguments)]
    fn widen_error_union_prefix(
        &mut self,
        src_ok_ty: &Type,
        src_err_tys: &[Type],
        dst_ok_ty: &Type,
        dst_err_tys: &[Type],
        src_union_ty: &Type,
        dst_union_ty: &Type,
        src_value: ValueId,
    ) -> ValueId {
        if src_ok_ty != dst_ok_ty
            || src_err_tys.len() > dst_err_tys.len()
            || !src_err_tys
                .iter()
                .zip(dst_err_tys.iter())
                .all(|(src, dst)| src == dst)
        {
            panic!(
                "backend unsupported error union widening: {:?} -> {:?}",
                src_union_ty, dst_union_ty
            );
        }

        let src_ir_ty = self.type_lowerer.lower_type(src_union_ty);
        let src_slot = self.materialize_value_slot(src_value, src_ir_ty);

        let src_ty_id = self
            .type_map
            .type_table()
            .lookup_id(src_union_ty)
            .unwrap_or_else(|| panic!("backend missing source type id for {:?}", src_union_ty));
        let dst_ty_id = self
            .type_map
            .type_table()
            .lookup_id(dst_union_ty)
            .unwrap_or_else(|| panic!("backend missing target type id for {:?}", dst_union_ty));

        let (src_tag_ty, src_blob_ty, src_blob_size, src_blob_align) = {
            let (tag_ty, blob_ty) = {
                let layout = self.type_lowerer.enum_layout(src_ty_id);
                (layout.tag_ty, layout.blob_ty)
            };
            let blob_layout = self.type_lowerer.ir_type_cache.layout(blob_ty);
            (tag_ty, blob_ty, blob_layout.size(), blob_layout.align())
        };
        let (dst_tag_ty, dst_blob_ty, dst_blob_size, dst_blob_align) = {
            let (tag_ty, blob_ty) = {
                let layout = self.type_lowerer.enum_layout(dst_ty_id);
                (layout.tag_ty, layout.blob_ty)
            };
            let blob_layout = self.type_lowerer.ir_type_cache.layout(blob_ty);
            (tag_ty, blob_ty, blob_layout.size(), blob_layout.align())
        };

        if src_tag_ty != dst_tag_ty {
            panic!(
                "backend error union widening tag mismatch: {:?} vs {:?}",
                src_union_ty, dst_union_ty
            );
        }

        // Fast path: if both unions have identical physical ABI (same tag and payload blob
        // shape), reinterpret the value directly instead of copying tag/payload bytes.
        if src_blob_size == dst_blob_size && src_blob_align == dst_blob_align {
            return self.reinterpret_value_between_types(src_value, src_union_ty, dst_union_ty);
        }

        let dst_ir_ty = self.type_lowerer.lower_type(dst_union_ty);
        let dst_slot = self.alloc_value_slot(dst_ir_ty);

        let src_tag_addr = self.field_addr_typed(src_slot.addr, 0, src_tag_ty);
        let src_tag = self.builder.load(src_tag_addr, src_tag_ty);
        self.store_field(dst_slot.addr, 0, dst_tag_ty, src_tag);

        let src_payload_ptr = self.field_addr_typed(src_slot.addr, 1, src_blob_ty);
        let dst_payload_ptr = self.field_addr_typed(dst_slot.addr, 1, dst_blob_ty);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let len = self
            .builder
            .const_int(src_blob_size as i128, false, 64, u64_ty);
        self.builder.memcopy(dst_payload_ptr, src_payload_ptr, len);

        self.load_slot(&dst_slot)
    }

    /// Reinterprets a value from one aggregate type to another via pointer cast.
    ///
    /// Caller must ensure both source and destination have identical ABI layout.
    fn reinterpret_value_between_types(
        &mut self,
        value: ValueId,
        src_ty: &Type,
        dst_ty: &Type,
    ) -> ValueId {
        let src_ir_ty = self.type_lowerer.lower_type(src_ty);
        let src_slot = self.materialize_value_slot(value, src_ir_ty);
        let dst_ir_ty = self.type_lowerer.lower_type(dst_ty);
        let dst_ptr_ty = self.type_lowerer.ptr_to(dst_ir_ty);
        let dst_addr = self
            .builder
            .cast(CastKind::PtrToPtr, src_slot.addr, dst_ptr_ty);
        self.builder.load(dst_addr, dst_ir_ty)
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
        let len = self.builder.int_extend(len_u32, len_u64_ty, false);

        BaseView { ptr, len }
    }

    /// Loads ptr/len from a dyn-array base stored at `base_addr`.
    pub(super) fn load_dyn_array_view(
        &mut self,
        base_addr: ValueId,
        elem_ptr_ty: IrTypeId,
    ) -> BaseView {
        let ptr = self.load_field(base_addr, 0, elem_ptr_ty);

        // Dyn-array lengths are stored as u32; widen for index arithmetic.
        let len_u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
        let len_u32 = self.load_field(base_addr, 1, len_u32_ty);
        let len_u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let len = self.builder.int_extend(len_u32, len_u64_ty, false);

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

    /// Emits a divide/modulo-by-zero check that traps if `rhs == 0`.
    pub(super) fn emit_div_by_zero_check(&mut self, rhs: ValueId, rhs_ty: &Type) {
        let Type::Int { signed, bits, .. } = rhs_ty else {
            panic!("backend div-by-zero check on non-int type {:?}", rhs_ty);
        };

        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let rhs_ir_ty = self.type_lowerer.lower_type(rhs_ty);
        let zero = self.builder.const_int(0, *signed, *bits, rhs_ir_ty);
        let cond = self.builder.cmp(CmpOp::Ne, rhs, zero, bool_ty);

        let ok_bb = self.builder.add_block();
        let trap_bb = self.builder.add_block();

        // Split control flow on the non-zero predicate.
        self.builder.terminate(Terminator::CondBr {
            cond,
            then_bb: ok_bb,
            then_args: Vec::new(),
            else_bb: trap_bb,
            else_args: Vec::new(),
        });

        // Trap on division/modulo by zero.
        self.builder.select_block(trap_bb);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let kind = self.builder.const_int(0, false, 64, u64_ty);
        let zero_u64 = self.builder.const_int(0, false, 64, u64_ty);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::Trap),
            vec![kind, zero_u64, zero_u64, zero_u64],
            unit_ty,
        );
        self.builder.terminate(Terminator::Unreachable);

        // Continue lowering in the non-zero block.
        self.builder.select_block(ok_bb);
    }

    /// Emits a nonzero refinement check that traps if `value == 0`.
    pub(super) fn emit_nonzero_check(&mut self, value: ValueId, value_ty: &Type) {
        let Type::Int { signed, bits, .. } = value_ty else {
            panic!("backend nonzero check on non-int type {:?}", value_ty);
        };

        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let value_ir_ty = self.type_lowerer.lower_type(value_ty);
        let zero = self.builder.const_int(0, *signed, *bits, value_ir_ty);
        let cond = self.builder.cmp(CmpOp::Ne, value, zero, bool_ty);

        let ok_bb = self.builder.add_block();
        let trap_bb = self.builder.add_block();

        // Split control flow on the non-zero predicate.
        self.builder.terminate(Terminator::CondBr {
            cond,
            then_bb: ok_bb,
            then_args: Vec::new(),
            else_bb: trap_bb,
            else_args: Vec::new(),
        });

        // Trap on zero values.
        self.builder.select_block(trap_bb);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let kind = self
            .builder
            .const_int(if *signed { 5 } else { 4 }, false, 64, u64_ty);
        let trap_ty = if *signed {
            Type::sint(64)
        } else {
            Type::uint(64)
        };
        let trap_ir_ty = self.type_lowerer.lower_type(&trap_ty);
        let trap_value = self.builder.int_extend(value, trap_ir_ty, *signed);
        let zero_u64 = self.builder.const_int(0, false, 64, u64_ty);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::Trap),
            vec![kind, trap_value, zero_u64, zero_u64],
            unit_ty,
        );
        self.builder.terminate(Terminator::Unreachable);

        // Continue lowering in the non-zero block.
        self.builder.select_block(ok_bb);
    }

    pub(super) fn emit_runtime_set_alloc_trace(&mut self, enabled: bool) {
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let flag = self.builder.const_int(enabled as i128, false, 64, u64_ty);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::SetAllocTrace),
            vec![flag],
            unit_ty,
        );
    }

    /// Emits a range check guard that traps if `value` is outside [min, max_excl).
    pub(super) fn emit_range_check(
        &mut self,
        value: ValueId,
        min: ValueId,
        max_excl: ValueId,
        signed: bool,
    ) {
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
        let kind = self
            .builder
            .const_int(if signed { 3 } else { 2 }, false, 64, u64_ty);
        let trap_ty = if signed {
            Type::sint(64)
        } else {
            Type::uint(64)
        };
        let trap_ir_ty = self.type_lowerer.lower_type(&trap_ty);
        let trap_value = self.builder.int_extend(value, trap_ir_ty, signed);
        let trap_min = self.builder.int_extend(min, trap_ir_ty, signed);
        let trap_max = self.builder.int_extend(max_excl, trap_ir_ty, signed);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self.builder.call(
            Callee::Runtime(RuntimeFn::Trap),
            vec![kind, trap_value, trap_min, trap_max],
            unit_ty,
        );
        self.builder.terminate(Terminator::Unreachable);

        // Continue lowering in the in-range block.
        self.builder.select_block(ok_bb);
    }

    pub(super) fn emit_conversion_check(&mut self, from_ty: &Type, to_ty: &Type, value: ValueId) {
        if let TypeAssignability::IntToRefined { min, max, nonzero } =
            type_assignable(from_ty, to_ty)
        {
            let (signed, bits) = match from_ty {
                Type::Int {
                    signed,
                    bits,
                    bounds: _,
                    nonzero: _,
                } => (*signed, *bits),
                other => panic!("backend refinement check on non-int type {:?}", other),
            };
            if let (Some(min), Some(max)) = (min, max) {
                let ir_ty = self.type_lowerer.lower_type(from_ty);
                let min_val = self.builder.const_int(min, signed, bits, ir_ty);
                let max_val = self.builder.const_int(max, signed, bits, ir_ty);
                self.emit_range_check(value, min_val, max_val, signed);
            }
            if nonzero {
                self.emit_nonzero_check(value, from_ty);
            }
        }
    }

    /// Resolves a place to its base address after peeling heap/ref indirections.
    pub(super) fn resolve_deref_base(
        &mut self,
        target: &sem::PlaceExpr,
        deref_count: usize,
    ) -> Result<(ValueId, Type), LowerToIrError> {
        let mut base = self.lower_place_addr(target)?;
        let mut curr_ty = self.type_map.type_table().get(target.ty).clone();

        for _ in 0..deref_count {
            let elem_ty = match curr_ty {
                Type::Heap { elem_ty } | Type::Ref { elem_ty, .. } => elem_ty,
                other => panic!("backend resolve_deref_base on non-heap/ref {:?}", other),
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
                other => panic!(
                    "backend resolve_deref_base_value on non-heap/ref {:?}",
                    other
                ),
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
                "backend ensure_local_addr type mismatch for {:?}: {:?} vs {:?}",
                def_id, local.value_ty, value_ty
            );
        }

        match local.storage {
            LocalStorage::Addr(addr) => addr,
            LocalStorage::Value(value) => {
                let addr = self.alloc_local_addr(value_ty);
                let ty = self.def_type(def_id);
                self.store_value_into_addr(addr, value, &ty, value_ty);
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
                "backend assign_local_value type mismatch for {:?}: {:?} vs {:?}",
                def_id, local.value_ty, value_ty
            );
        }

        match local.storage {
            LocalStorage::Value(_) => {
                self.locals
                    .insert(def_id, LocalValue::value(value, value_ty));
            }
            LocalStorage::Addr(addr) => {
                let ty = self.def_type(def_id);
                self.store_value_into_addr(addr, value, &ty, value_ty);
            }
        }
    }

    /// Builds block arguments for locals, coercing storage to match `expected`.
    ///
    /// This keeps join/loop arguments type-consistent even if a branch
    /// temporarily takes an address for a scalar local.
    pub(super) fn local_args_for_like(
        &mut self,
        defs: &[DefId],
        expected: &[LocalValue],
    ) -> Vec<ValueId> {
        let mut args = Vec::with_capacity(defs.len());
        for (def, expected_local) in defs.iter().zip(expected.iter()) {
            let current = self.lookup_local(*def);
            let value_ty = expected_local.value_ty;
            let arg = match expected_local.storage {
                LocalStorage::Value(_) => match current.storage {
                    LocalStorage::Value(value) => value,
                    LocalStorage::Addr(addr) => self.builder.load(addr, value_ty),
                },
                LocalStorage::Addr(_) => match current.storage {
                    LocalStorage::Addr(addr) => addr,
                    LocalStorage::Value(value) => {
                        let addr = self.alloc_local_addr(value_ty);
                        self.builder.store(addr, value);
                        addr
                    }
                },
            };
            args.push(arg);
        }
        args
    }

    fn is_aggregate(&self, ir_ty: IrTypeId) -> bool {
        matches!(
            self.type_lowerer.ir_type_cache.kind(ir_ty),
            IrTypeKind::Array { .. }
                | IrTypeKind::Tuple { .. }
                | IrTypeKind::Struct { .. }
                | IrTypeKind::Blob { .. }
        )
    }

    pub(super) fn store_value_into_addr(
        &mut self,
        dst: ValueId,
        value: ValueId,
        _ty: &Type,
        ir_ty: IrTypeId,
    ) {
        if !self.is_aggregate(ir_ty) {
            self.builder.store(dst, value);
            return;
        }

        // Aggregate values (struct/tuple/array/blob) are materialized into a slot
        // and copied byte-for-byte. Using scalar `store` here is incorrect for
        // multi-field values.
        let src_slot = self.materialize_value_slot(value, ir_ty);
        let layout = self.type_lowerer.ir_type_cache.layout(ir_ty);
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let len = self
            .builder
            .const_int(layout.size() as i128, false, 64, u64_ty);
        self.builder.memcopy(dst, src_slot.addr, len);
    }

    /// Extracts ptr+len from a string/slice value for runtime argument lowering.
    pub(super) fn lower_ptr_len_from_value(
        &mut self,
        _span: Span,
        value: ValueId,
        ty: &Type,
        len_bits: u8,
    ) -> Result<(ValueId, ValueId), LowerToIrError> {
        let (ptr_ty, len_ty) = match ty {
            Type::String => {
                if len_bits != 32 {
                    panic!("backend ptr/len lowering invalid len_bits {len_bits} for string");
                }
                let byte_ty = self.type_lowerer.lower_type(&Type::uint(8));
                let ptr_ty = self.type_lowerer.ptr_to(byte_ty);
                let len_ty = self.type_lowerer.lower_type(&Type::uint(32));
                (ptr_ty, len_ty)
            }
            Type::Slice { elem_ty } => {
                if len_bits != 64 {
                    panic!("backend ptr/len lowering invalid len_bits {len_bits} for slice");
                }
                let elem_ir = self.type_lowerer.lower_type(elem_ty);
                let ptr_ty = self.type_lowerer.ptr_to(elem_ir);
                let len_ty = self.type_lowerer.lower_type(&Type::uint(64));
                (ptr_ty, len_ty)
            }
            Type::DynArray { elem_ty } => {
                if len_bits != 64 {
                    panic!("backend ptr/len lowering invalid len_bits {len_bits} for dyn array");
                }
                let elem_ir = self.type_lowerer.lower_type(elem_ty);
                let ptr_ty = self.type_lowerer.ptr_to(elem_ir);
                let len_ty = self.type_lowerer.lower_type(&Type::uint(32));
                (ptr_ty, len_ty)
            }
            _ => {
                panic!(
                    "compiler bug: backend ptr/len lowering expects string, slice, or dyn array, got {ty:?}"
                )
            }
        };

        let value_ty = self.type_lowerer.lower_type(ty);

        // Materialize the aggregate into a local to address its fields.
        let slot = self.materialize_value_slot(value, value_ty);

        let ptr_val = self.load_field(slot.addr, 0, ptr_ty);
        let mut len_val = self.load_field(slot.addr, 1, len_ty);
        if len_bits == 32 || matches!(ty, Type::DynArray { .. }) {
            let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
            len_val = self.builder.int_extend(len_val, u64_ty, false);
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
        // Scalar payloads can be stored directly into the blob slot.
        if !self.is_aggregate(value_ty) {
            let dst_bytes = self.byte_offset_addr(blob_ptr, offset);
            let dst_ty = self.type_lowerer.ptr_to(value_ty);
            let dst = self.builder.cast(CastKind::PtrToPtr, dst_bytes, dst_ty);
            self.builder.store(dst, value);
            return;
        }

        // For POD-like tuple/struct payloads with scalar fields, store fields
        // directly into the blob instead of issuing a bulk memcpy call.
        if let Some(field_tys) = self.pod_blob_field_tys(value_ty) {
            let temp_ptr = self.alloc_local_addr(value_ty);
            self.builder.store(temp_ptr, value);

            let layout = self.type_lowerer.ir_type_cache.layout(value_ty);
            let offsets = layout.field_offsets().to_vec();
            for (index, field_ty) in field_tys.iter().copied().enumerate() {
                let src_field_ptr = self.field_addr_typed(temp_ptr, index, field_ty);
                let field_value = self.builder.load(src_field_ptr, field_ty);

                let dst_bytes = self.byte_offset_addr(blob_ptr, offset + offsets[index]);
                let dst_ty = self.type_lowerer.ptr_to(field_ty);
                let dst_ptr = self.builder.cast(CastKind::PtrToPtr, dst_bytes, dst_ty);
                self.builder.store(dst_ptr, field_value);
            }
            return;
        }

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

    fn pod_blob_field_tys(&self, ty: IrTypeId) -> Option<Vec<IrTypeId>> {
        let field_tys = match self.type_lowerer.ir_type_cache.kind(ty) {
            IrTypeKind::Tuple { fields } => fields.clone(),
            IrTypeKind::Struct { fields } => fields.iter().map(|field| field.ty).collect(),
            _ => return None,
        };
        if field_tys
            .iter()
            .any(|field_ty| self.is_aggregate(*field_ty))
        {
            return None;
        }
        Some(field_tys)
    }
}
