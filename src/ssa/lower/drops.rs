//! Drop-plan helpers for SSA lowering.

use crate::resolve::DefId;
use crate::ssa::lower::LoweringError;
use crate::ssa::lower::lowerer::FuncLowerer;
use crate::ssa::model::ir::{Callee, ConstValue, RuntimeFn, SwitchCase, Terminator, ValueId};
use crate::tree::NodeId;
use crate::tree::semantic as sem;
use crate::types::Type;
use crate::types::TypeId;
use std::collections::HashMap;
use std::ptr::NonNull;

/// Tracks drop-scope state and per-def liveness flags during lowering.
///
/// This struct only manages bookkeeping (scope stack + liveness flags). The
/// caller is responsible for turning popped scopes into IR-level drop calls.
pub(super) struct DropTracker<'a> {
    plans: Option<&'a sem::DropPlanMap>,
    scopes: Vec<NodeId>,
    flags: HashMap<DefId, ValueId>,
}

impl<'a> DropTracker<'a> {
    pub(super) fn new() -> Self {
        Self {
            plans: None,
            scopes: Vec::new(),
            flags: HashMap::new(),
        }
    }

    pub(super) fn set_plans(&mut self, plans: &'a sem::DropPlanMap) {
        self.plans = Some(plans);
    }

    pub(super) fn plans(&self) -> Option<&'a sem::DropPlanMap> {
        self.plans
    }

    pub(super) fn is_active(&self) -> bool {
        self.plans.is_some()
    }

    /// Clones the current scope stack for later restoration (e.g., across branches).
    pub(super) fn snapshot(&self) -> Vec<NodeId> {
        self.scopes.clone()
    }

    /// Restores a previously captured scope stack snapshot.
    pub(super) fn restore(&mut self, snapshot: &[NodeId]) {
        self.scopes = snapshot.to_vec();
    }

    pub(super) fn enter_scope(&mut self, id: NodeId) {
        if self.is_active() {
            self.scopes.push(id);
        }
    }

    /// Pops the scope if it is the active scope and returns it for drop emission.
    pub(super) fn exit_scope_if_active(&mut self, id: NodeId) -> Option<NodeId> {
        if !self.is_active() {
            return None;
        }

        match self.scopes.last().copied() {
            Some(top) if top == id => Some(self.pop_scope(id)),
            Some(_) => {
                if self.scopes.iter().any(|scope_id| *scope_id == id) {
                    panic!("ssa drop scope mismatch while dropping {:?}", id);
                }
                None
            }
            None => None,
        }
    }

    /// Pops scopes until the requested depth and returns them in LIFO order.
    pub(super) fn pop_to_depth(&mut self, depth: usize) -> Vec<NodeId> {
        if !self.is_active() {
            return Vec::new();
        }

        if depth > self.scopes.len() {
            panic!(
                "ssa drop depth {} exceeds scope depth {}",
                depth,
                self.scopes.len()
            );
        }

        let mut scopes = Vec::new();
        while self.scopes.len() > depth {
            let scope_id = self
                .scopes
                .pop()
                .unwrap_or_else(|| panic!("ssa drop scope underflow"));
            scopes.push(scope_id);
        }

        scopes
    }

    /// Returns the target drop depth for a statement, if drop plans are active.
    pub(super) fn depth_for_stmt(&self, stmt_id: NodeId) -> Option<usize> {
        self.plans.map(|plans| {
            plans
                .depth_for(stmt_id)
                .unwrap_or_else(|| panic!("ssa missing drop depth for {:?}", stmt_id))
        })
    }

    /// Looks up the liveness flag for a definition.
    pub(super) fn flag(&self, def_id: DefId) -> Option<ValueId> {
        self.flags.get(&def_id).copied()
    }

    /// Records a liveness flag for a definition.
    pub(super) fn insert_flag(&mut self, def_id: DefId, addr: ValueId) {
        self.flags.insert(def_id, addr);
    }

    fn pop_scope(&mut self, id: NodeId) -> NodeId {
        let scope_id = self
            .scopes
            .pop()
            .unwrap_or_else(|| panic!("ssa drop scope underflow for {:?}", id));
        if scope_id != id {
            panic!(
                "ssa drop scope mismatch: expected {:?}, got {:?}",
                id, scope_id
            );
        }
        scope_id
    }
}

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
        self.drop_tracker.set_plans(drop_plans);
    }

    /// Captures the active drop scope stack for branch-local lowering.
    pub(super) fn drop_scopes_snapshot(&self) -> Vec<NodeId> {
        self.drop_tracker.snapshot()
    }

    /// Restores a previously captured drop scope stack snapshot.
    pub(super) fn restore_drop_scopes(&mut self, snapshot: &[NodeId]) {
        self.drop_tracker.restore(snapshot);
    }

    pub(super) fn enter_drop_scope(&mut self, id: NodeId) {
        self.drop_tracker.enter_scope(id);
    }

    /// Pops and emits a scope if it is active, used by the RAII guard.
    fn exit_drop_scope_if_active(&mut self, id: NodeId) {
        if let Some(scope_id) = self.drop_tracker.exit_scope_if_active(id) {
            if let Err(err) = self.emit_drop_scope(scope_id) {
                panic!("ssa drop scope exit failed: {err:?}");
            }
        }
    }

    /// Emits drops until the drop scope stack is at the requested depth.
    pub(super) fn emit_drops_to_depth(&mut self, depth: usize) -> Result<(), LoweringError> {
        let scopes = self.drop_tracker.pop_to_depth(depth);
        for scope_id in scopes {
            self.emit_drop_scope(scope_id)?;
        }
        Ok(())
    }

    /// Emits drops corresponding to a semantic statement's drop depth.
    pub(super) fn emit_drops_for_stmt(&mut self, stmt_id: NodeId) -> Result<(), LoweringError> {
        let Some(depth) = self.drop_tracker.depth_for_stmt(stmt_id) else {
            return Ok(());
        };
        self.emit_drops_to_depth(depth)
    }

    pub(super) fn emit_drop_for_def_if_live(
        &mut self,
        def_id: DefId,
        ty_id: TypeId,
    ) -> Result<(), LoweringError> {
        if !self.drop_tracker.is_active() {
            return Ok(());
        }

        let ty = self.type_map.type_table().get(ty_id).clone();
        if !ty.needs_drop() && shallow_named(&ty).is_none() {
            return Ok(());
        }

        let flag_addr = self
            .drop_tracker
            .flag(def_id)
            .unwrap_or_else(|| panic!("ssa drop missing flag for {:?}", def_id));
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
        let local = self.lookup_local(def_id);
        let (value, is_addr) = match local.storage {
            crate::ssa::lower::locals::LocalStorage::Value(value) => (value, false),
            crate::ssa::lower::locals::LocalStorage::Addr(addr) => (addr, true),
        };
        self.emit_drop_for_value(value, &ty, is_addr)?;
        self.builder.terminate(Terminator::Br {
            target: cont_bb,
            args: Vec::new(),
        });

        self.builder.select_block(cont_bb);
        Ok(())
    }

    pub(super) fn set_drop_flag_for_def(&mut self, def_id: DefId, value: bool) {
        if !self.drop_tracker.is_active() {
            return;
        }

        let ty = self.def_type(def_id);
        if !ty.needs_drop() && shallow_named(&ty).is_none() {
            return;
        }

        let flag_addr = match self.drop_tracker.flag(def_id) {
            Some(addr) => addr,
            None => {
                let addr = self.create_drop_flag(def_id);
                self.drop_tracker.insert_flag(def_id, addr);
                addr
            }
        };
        self.store_drop_flag(flag_addr, value);
    }

    fn emit_drop_scope(&mut self, id: NodeId) -> Result<(), LoweringError> {
        let drop_plans = self
            .drop_tracker
            .plans()
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
        match item.guard {
            sem::DropGuard::Always => {
                self.emit_drop_for_def(item.def_id, item.ty)?;
                Ok(())
            }
            sem::DropGuard::IfInitialized => {
                let flag_addr = match self.drop_tracker.flag(item.def_id) {
                    Some(addr) => addr,
                    None => {
                        let addr = self.create_drop_flag(item.def_id);
                        self.drop_tracker.insert_flag(item.def_id, addr);
                        self.store_drop_flag(addr, false);
                        addr
                    }
                };

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
        }
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
        if !ty.needs_drop() && shallow_named(ty).is_none() {
            return Ok(());
        }

        if let Type::Heap { elem_ty } = ty {
            if is_addr {
                return self.drop_value_at_addr(value, ty);
            }

            if let Some(name) = shallow_named(elem_ty) {
                let def_id = self.drop_glue.def_id_for(name, elem_ty);
                let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
                let _ = self
                    .builder
                    .call(Callee::Direct(def_id), vec![value], unit_ty);
            } else if elem_ty.needs_drop() {
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

    pub(super) fn drop_value_at_addr(
        &mut self,
        addr: ValueId,
        ty: &Type,
    ) -> Result<(), LoweringError> {
        if let Some(name) = shallow_named(ty) {
            let def_id = self.drop_glue.def_id_for(name, ty);
            let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
            let _ = self
                .builder
                .call(Callee::Direct(def_id), vec![addr], unit_ty);
            return Ok(());
        }

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

                if let Some(name) = shallow_named(elem_ty) {
                    let def_id = self.drop_glue.def_id_for(name, elem_ty);
                    let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
                    let _ = self
                        .builder
                        .call(Callee::Direct(def_id), vec![ptr_val], unit_ty);
                } else if elem_ty.needs_drop() {
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
                    if !field.ty.needs_drop() && shallow_named(&field.ty).is_none() {
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
                    if !field_ty.needs_drop() && shallow_named(field_ty).is_none() {
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
                if !elem_ty.needs_drop() && shallow_named(&elem_ty).is_none() {
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
                    if variant
                        .payload
                        .iter()
                        .any(|ty| ty.needs_drop() || shallow_named(ty).is_some())
                    {
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
                    if !variant
                        .payload
                        .iter()
                        .any(|ty| ty.needs_drop() || shallow_named(ty).is_some())
                    {
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
                        if !payload_ty.needs_drop() && shallow_named(payload_ty).is_none() {
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
        let name = Some(format!("{}$drop_live", self.def(def_id).name));
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

fn shallow_named(ty: &Type) -> Option<&str> {
    match ty {
        Type::Struct { name, fields } if fields.is_empty() => Some(name.as_str()),
        Type::Enum { name, variants } if variants.is_empty() => Some(name.as_str()),
        _ => None,
    }
}
