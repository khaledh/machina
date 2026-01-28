//! Drop-plan helpers for SSA lowering.

use crate::resolve::DefId;
use crate::ssa::lower::LoweringError;
use crate::ssa::lower::lowerer::FuncLowerer;
use crate::ssa::model::ir::{Callee, ConstValue, RuntimeFn, SwitchCase, Terminator, ValueId};
use crate::tree::NodeId;
use crate::tree::semantic as sem;
use crate::types::Type;
use crate::types::TypeId;
use std::ptr::NonNull;

/// RAII guard that exits a drop scope when it falls out of scope.
#[must_use = "drop scope guard must be kept alive for the duration of the scope"]
pub(super) struct DropScopeGuard<'a, 'g> {
    lowerer: NonNull<FuncLowerer<'a, 'g>>,
    id: NodeId,
}

impl<'a, 'g> Drop for DropScopeGuard<'a, 'g> {
    fn drop(&mut self) {
        // SAFETY: the guard is created from a live FuncLowerer and dropped before it is moved.
        unsafe {
            self.lowerer.as_mut().exit_drop_scope_if_active(self.id);
        }
    }
}

impl<'a, 'g> FuncLowerer<'a, 'g> {
    pub(super) fn with_drop_scope<R>(
        &mut self,
        id: NodeId,
        f: impl FnOnce(&mut Self) -> Result<R, LoweringError>,
    ) -> Result<R, LoweringError> {
        let _guard = self.drop_scope(id);
        f(self)
    }

    pub(super) fn drop_scope(&mut self, id: NodeId) -> DropScopeGuard<'a, 'g> {
        self.enter_drop_scope(id);
        DropScopeGuard {
            lowerer: NonNull::from(self),
            id,
        }
    }

    pub(super) fn set_drop_plans(&mut self, drop_plans: &'a sem::DropPlanMap) {
        self.drop_plans = Some(drop_plans);
    }

    pub(super) fn drop_scopes_snapshot(&self) -> Vec<NodeId> {
        self.drop_scopes.clone()
    }

    pub(super) fn restore_drop_scopes(&mut self, snapshot: &[NodeId]) {
        self.drop_scopes = snapshot.to_vec();
    }

    pub(super) fn enter_drop_scope(&mut self, id: NodeId) {
        if self.drop_plans.is_some() {
            self.drop_scopes.push(id);
        }
    }

    fn exit_drop_scope_if_active(&mut self, id: NodeId) {
        if self.drop_plans.is_none() {
            return;
        }

        match self.drop_scopes.last().copied() {
            Some(top) if top == id => {
                if let Err(err) = self.exit_drop_scope(id) {
                    panic!("ssa drop scope exit failed: {err:?}");
                }
            }
            Some(_) => {
                if self.drop_scopes.iter().any(|scope_id| *scope_id == id) {
                    panic!("ssa drop scope mismatch while dropping {:?}", id);
                }
            }
            None => {}
        }
    }

    pub(super) fn exit_drop_scope(&mut self, id: NodeId) -> Result<(), LoweringError> {
        if self.drop_plans.is_none() {
            return Ok(());
        }

        let scope_id = self
            .drop_scopes
            .pop()
            .unwrap_or_else(|| panic!("ssa drop scope underflow for {:?}", id));
        if scope_id != id {
            panic!(
                "ssa drop scope mismatch: expected {:?}, got {:?}",
                id, scope_id
            );
        }

        self.emit_drop_scope(scope_id)
    }

    pub(super) fn emit_drops_to_depth(&mut self, depth: usize) -> Result<(), LoweringError> {
        if self.drop_plans.is_none() {
            return Ok(());
        }

        if depth > self.drop_scopes.len() {
            panic!(
                "ssa drop depth {} exceeds scope depth {}",
                depth,
                self.drop_scopes.len()
            );
        }

        while self.drop_scopes.len() > depth {
            let scope_id = self
                .drop_scopes
                .pop()
                .unwrap_or_else(|| panic!("ssa drop scope underflow"));
            self.emit_drop_scope(scope_id)?;
        }

        Ok(())
    }

    pub(super) fn emit_drops_for_stmt(&mut self, stmt_id: NodeId) -> Result<(), LoweringError> {
        let Some(drop_plans) = self.drop_plans.as_ref() else {
            return Ok(());
        };
        let depth = drop_plans
            .depth_for(stmt_id)
            .unwrap_or_else(|| panic!("ssa missing drop depth for {:?}", stmt_id));
        self.emit_drops_to_depth(depth)
    }

    pub(super) fn set_drop_flag_for_def(&mut self, def_id: DefId, value: bool) {
        if self.drop_plans.is_none() {
            return;
        }

        let def = self
            .def_table
            .lookup_def(def_id)
            .unwrap_or_else(|| panic!("ssa drop flag missing def {:?}", def_id));
        let ty_id = self
            .type_map
            .lookup_def_type_id(def)
            .unwrap_or_else(|| panic!("ssa drop flag missing type for {:?}", def_id));
        let ty = self.type_map.type_table().get(ty_id).clone();
        if !ty.needs_drop() {
            return;
        }

        let flag_addr = match self.drop_flags.get(&def_id).copied() {
            Some(addr) => addr,
            None => {
                let addr = self.create_drop_flag(def_id);
                self.drop_flags.insert(def_id, addr);
                addr
            }
        };
        self.store_drop_flag(flag_addr, value);
    }

    fn emit_drop_scope(&mut self, id: NodeId) -> Result<(), LoweringError> {
        let drop_plans = self
            .drop_plans
            .as_ref()
            .unwrap_or_else(|| panic!("ssa drop scope missing drop plans"));
        let scope = drop_plans
            .scope_for(id)
            .unwrap_or_else(|| panic!("ssa drop scope missing plan for {:?}", id));

        for item in scope.drops.iter().rev() {
            self.emit_drop_item(item)?;
        }

        Ok(())
    }

    fn emit_drop_item(&mut self, item: &sem::DropItem) -> Result<(), LoweringError> {
        let flag_addr = self
            .drop_flags
            .get(&item.def_id)
            .copied()
            .unwrap_or_else(|| {
                panic!("ssa drop missing flag for {:?}", item.def_id);
            });

        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let cond = self.builder.load(flag_addr, bool_ty);
        let drop_bb = self.builder.add_block();
        let cont_bb = self.builder.add_block();

        self.builder.terminate(Terminator::CondBr {
            cond,
            then_bb: drop_bb,
            then_args: Vec::new(),
            else_bb: cont_bb,
            else_args: Vec::new(),
        });

        self.builder.select_block(drop_bb);
        self.emit_drop_for_def(item.def_id, item.ty)?;
        self.builder.terminate(Terminator::Br {
            target: cont_bb,
            args: Vec::new(),
        });

        self.builder.select_block(cont_bb);
        Ok(())
    }

    fn emit_drop_for_def(
        &mut self,
        def_id: DefId,
        ty_id: crate::types::TypeId,
    ) -> Result<(), LoweringError> {
        let ty = self.type_map.type_table().get(ty_id).clone();
        let value_ty = self.type_lowerer.lower_type_id(ty_id);
        let addr = self.ensure_local_addr(def_id, value_ty);
        self.drop_value_at_addr(addr, &ty)
    }

    pub(super) fn emit_drop_for_value(
        &mut self,
        value: ValueId,
        ty: &Type,
        is_addr: bool,
    ) -> Result<(), LoweringError> {
        if !ty.needs_drop() {
            return Ok(());
        }

        if let Type::Heap { elem_ty } = ty {
            if is_addr {
                return self.drop_value_at_addr(value, ty);
            }

            if elem_ty.needs_drop() {
                self.drop_value_at_addr(value, elem_ty)?;
            }
            let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
            let _ = self
                .builder
                .call(Callee::Runtime(RuntimeFn::Free), vec![value], unit_ty);
            return Ok(());
        }

        if is_addr {
            return self.drop_value_at_addr(value, ty);
        }

        let ir_ty = self.type_lowerer.lower_type(ty);
        let slot = self.alloc_value_slot(ir_ty);
        self.store_value_into_addr(slot.addr, value, ty, ir_ty);
        self.drop_value_at_addr(slot.addr, ty)
    }

    fn drop_value_at_addr(&mut self, addr: ValueId, ty: &Type) -> Result<(), LoweringError> {
        if !ty.needs_drop() {
            return Ok(());
        }

        match ty {
            Type::String => {
                let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
                let _ =
                    self.builder
                        .call(Callee::Runtime(RuntimeFn::StringDrop), vec![addr], unit_ty);
                Ok(())
            }
            Type::Heap { elem_ty } => {
                let ptr_ty = self.type_lowerer.lower_type(ty);
                let ptr_val = self.builder.load(addr, ptr_ty);

                if elem_ty.needs_drop() {
                    self.drop_value_at_addr(ptr_val, elem_ty)?;
                }

                let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
                let _ = self
                    .builder
                    .call(Callee::Runtime(RuntimeFn::Free), vec![ptr_val], unit_ty);
                Ok(())
            }
            Type::Struct { fields, .. } => {
                for (index, field) in fields.iter().enumerate().rev() {
                    if !field.ty.needs_drop() {
                        continue;
                    }
                    let field_ty = self.type_lowerer.lower_type(&field.ty);
                    let field_addr = self.field_addr_typed(addr, index, field_ty);
                    self.drop_value_at_addr(field_addr, &field.ty)?;
                }
                Ok(())
            }
            Type::Tuple { field_tys } => {
                for (index, field_ty) in field_tys.iter().enumerate().rev() {
                    if !field_ty.needs_drop() {
                        continue;
                    }
                    let field_ir_ty = self.type_lowerer.lower_type(field_ty);
                    let field_addr = self.field_addr_typed(addr, index, field_ir_ty);
                    self.drop_value_at_addr(field_addr, field_ty)?;
                }
                Ok(())
            }
            Type::Array { dims, .. } => {
                let Some(elem_ty) = ty.array_item_type() else {
                    panic!("ssa drop array missing element type");
                };
                if !elem_ty.needs_drop() {
                    return Ok(());
                }

                let len = *dims
                    .first()
                    .unwrap_or_else(|| panic!("ssa drop array missing dims"));
                let elem_ir_ty = self.type_lowerer.lower_type(&elem_ty);
                let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                let idx_ty = self.type_lowerer.lower_type(&Type::uint(64));

                for i in (0..len).rev() {
                    let idx_val = self.builder.const_int(i as i128, false, 64, idx_ty);
                    let elem_addr = self.builder.index_addr(addr, idx_val, elem_ptr_ty);
                    self.drop_value_at_addr(elem_addr, &elem_ty)?;
                }
                Ok(())
            }
            Type::Enum { variants, .. } => {
                let ty_id = self.type_id_for_enum(ty);
                // Copy layout data out of the type lowerer to avoid holding a mutable borrow
                // while emitting IR instructions that need &mut self.
                let (tag_ty, blob_ty, layout_variants) = {
                    let layout = self.type_lowerer.enum_layout(ty_id);
                    let variants = layout
                        .variants
                        .iter()
                        .map(|variant| {
                            (
                                variant.tag,
                                variant.field_offsets.clone(),
                                variant.name.clone(),
                            )
                        })
                        .collect::<Vec<_>>();
                    (layout.tag_ty, layout.blob_ty, variants)
                };

                let mut needs_drop = false;
                for variant in variants {
                    if variant.payload.iter().any(Type::needs_drop) {
                        needs_drop = true;
                        break;
                    }
                }
                if !needs_drop {
                    return Ok(());
                }

                let switch_bb = self.builder.current_block();
                let tag_addr = self.field_addr_typed(addr, 0, tag_ty);
                let tag_val = self.builder.load(tag_addr, tag_ty);
                let ret_bb = self.builder.add_block();
                let mut cases = Vec::new();

                // Emit a tag-based switch that drops the active variant's payload.
                for (variant, (tag, field_offsets, name)) in
                    variants.iter().zip(layout_variants.iter())
                {
                    if !variant.payload.iter().any(Type::needs_drop) {
                        continue;
                    }

                    if variant.payload.len() != field_offsets.len() {
                        panic!(
                            "ssa enum drop payload mismatch for {}: {} offsets, {} tys",
                            name,
                            field_offsets.len(),
                            variant.payload.len()
                        );
                    }

                    let case_bb = self.builder.add_block();
                    cases.push(SwitchCase {
                        value: ConstValue::Int {
                            value: *tag as i128,
                            signed: false,
                            bits: 32,
                        },
                        target: case_bb,
                        args: Vec::new(),
                    });

                    self.builder.select_block(case_bb);

                    let payload_ptr = self.field_addr_typed(addr, 1, blob_ty);
                    // Drop payload fields in reverse order, using blob offsets.
                    for (payload_ty, offset) in
                        variant.payload.iter().zip(field_offsets.iter()).rev()
                    {
                        if !payload_ty.needs_drop() {
                            continue;
                        }
                        let field_addr = self.byte_offset_addr(payload_ptr, *offset);
                        self.drop_value_at_addr(field_addr, payload_ty)?;
                    }

                    self.builder.terminate(Terminator::Br {
                        target: ret_bb,
                        args: Vec::new(),
                    });
                }

                self.builder.select_block(switch_bb);
                self.builder.terminate(Terminator::Switch {
                    value: tag_val,
                    cases,
                    default: ret_bb,
                    default_args: Vec::new(),
                });

                self.builder.select_block(ret_bb);
                Ok(())
            }
            other => panic!("ssa drop not implemented for {:?}", other),
        }
    }

    fn type_id_for_enum(&self, ty: &Type) -> TypeId {
        self.type_map
            .type_table()
            .lookup_id(ty)
            .unwrap_or_else(|| panic!("ssa drop missing enum type id for {:?}", ty))
    }

    fn create_drop_flag(&mut self, def_id: DefId) -> ValueId {
        let flag_ty = self.type_lowerer.lower_type(&Type::Bool);
        let name = self
            .def_table
            .lookup_def(def_id)
            .map(|def| format!("{}$drop_live", def.name));
        let local = self.builder.add_local(flag_ty, name);
        let ptr_ty = self.type_lowerer.ptr_to(flag_ty);
        self.builder.addr_of_local(local, ptr_ty)
    }

    fn store_drop_flag(&mut self, flag_addr: ValueId, value: bool) {
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let flag_val = self.builder.const_bool(value, bool_ty);
        self.builder.store(flag_addr, flag_val);
    }
}
