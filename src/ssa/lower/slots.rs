//! Helpers for materializing values into addressable slots.

use crate::ssa::IrTypeId;
use crate::ssa::lower::lowerer::{FuncLowerer, ValueSlot};
use crate::ssa::model::ir::ValueId;

impl<'a, 'g> FuncLowerer<'a, 'g> {
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
}
