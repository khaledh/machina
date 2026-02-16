//! Drop-plan helpers for SSA lowering.

use crate::core::backend::lower::LowerToIrError;
use crate::core::backend::lower::locals::LocalStorage;
use crate::core::backend::lower::lowerer::{CallInputValue, FuncLowerer};
use crate::core::ir::{Callee, ConstValue, RuntimeFn, SwitchCase, Terminator, ValueId};
use crate::core::resolve::DefId;
use crate::core::tree::NodeId;
use crate::core::tree::semantic as sem;
use crate::core::tree::{InitInfo, ParamMode};
use crate::core::types::Type;
use crate::core::types::{EnumVariant, StructField, TypeId};
use std::collections::HashMap;

enum OutProj<'a> {
    Struct(&'a str),
    Tuple(usize),
}

/// Tracks drop-scope state and per-def liveness flags during lowering.
///
/// This struct only manages bookkeeping (scope stack + liveness flags). The
/// caller is responsible for turning popped scopes into IR-level drop calls.
pub(super) struct DropManager<'a> {
    plans: Option<&'a sem::DropPlanMap>,
    scopes: Vec<NodeId>,
    flags: HashMap<DefId, ValueId>,
    known_live: HashMap<DefId, bool>,
}

#[derive(Clone)]
pub(super) struct DropSnapshot {
    scopes: Vec<NodeId>,
    known_live: HashMap<DefId, bool>,
}

impl<'a> DropManager<'a> {
    pub(super) fn new() -> Self {
        Self {
            plans: None,
            scopes: Vec::new(),
            flags: HashMap::new(),
            known_live: HashMap::new(),
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
    pub(super) fn snapshot(&self) -> DropSnapshot {
        DropSnapshot {
            scopes: self.scopes.clone(),
            known_live: self.known_live.clone(),
        }
    }

    /// Restores a previously captured scope stack snapshot.
    pub(super) fn restore(&mut self, snapshot: &DropSnapshot) {
        self.scopes = snapshot.scopes.clone();
        self.known_live = snapshot.known_live.clone();
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
                if self.scopes.contains(&id) {
                    panic!("backend drop scope mismatch while dropping {:?}", id);
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
                "backend drop depth {} exceeds scope depth {}",
                depth,
                self.scopes.len()
            );
        }

        let mut scopes = Vec::new();
        while self.scopes.len() > depth {
            let scope_id = self
                .scopes
                .pop()
                .unwrap_or_else(|| panic!("backend drop scope underflow"));
            scopes.push(scope_id);
        }

        scopes
    }

    /// Returns the target drop depth for a statement, if drop plans are active.
    pub(super) fn depth_for_stmt(&self, stmt_id: NodeId) -> Option<usize> {
        self.plans.map(|plans| {
            plans
                .depth_for(stmt_id)
                .unwrap_or_else(|| panic!("backend missing drop depth for {:?}", stmt_id))
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

    pub(super) fn set_known_live(&mut self, def_id: DefId, value: bool) {
        self.known_live.insert(def_id, value);
    }

    pub(super) fn known_live(&self, def_id: DefId) -> Option<bool> {
        self.known_live.get(&def_id).copied()
    }

    pub(super) fn invalidate_known_live(&mut self) {
        self.known_live.clear();
    }

    fn pop_scope(&mut self, id: NodeId) -> NodeId {
        let scope_id = self
            .scopes
            .pop()
            .unwrap_or_else(|| panic!("backend drop scope underflow for {:?}", id));
        if scope_id != id {
            panic!(
                "backend drop scope mismatch: expected {:?}, got {:?}",
                id, scope_id
            );
        }
        scope_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DropKind {
    Trivial,
    Shallow,
    Deep,
}

fn drop_kind(ty: &Type) -> DropKind {
    if is_shallow_named(ty) {
        return DropKind::Shallow;
    }
    if has_nontrivial_drop(ty) {
        DropKind::Deep
    } else {
        DropKind::Trivial
    }
}

fn has_nontrivial_drop(ty: &Type) -> bool {
    if is_shallow_named(ty) {
        return true;
    }

    match ty {
        Type::String | Type::Heap { .. } => true,
        Type::Array { elem_ty, .. }
        | Type::Slice { elem_ty }
        | Type::DynArray { elem_ty }
        | Type::Set { elem_ty } => {
            has_nontrivial_drop(elem_ty) || matches!(ty, Type::DynArray { .. } | Type::Set { .. })
        }
        Type::Map { key_ty, value_ty } => {
            has_nontrivial_drop(key_ty)
                || has_nontrivial_drop(value_ty)
                || matches!(ty, Type::Map { .. })
        }
        Type::Tuple { field_tys } => field_tys.iter().any(has_nontrivial_drop),
        Type::Struct { fields, .. } => fields.iter().any(|field| has_nontrivial_drop(&field.ty)),
        Type::Enum { variants, .. } => variants
            .iter()
            .any(|variant| variant.payload.iter().any(has_nontrivial_drop)),
        Type::ErrorUnion { ok_ty, err_tys } => {
            has_nontrivial_drop(ok_ty) || err_tys.iter().any(has_nontrivial_drop)
        }
        _ => false,
    }
}

impl<'a, 'g> FuncLowerer<'a, 'g> {
    /// Initialize drop tracking for a function/method body root scope.
    pub(super) fn init_root_drop_scope(
        &mut self,
        drop_plans: &'a sem::DropPlanMap,
        root_scope: NodeId,
    ) {
        self.set_drop_plans(drop_plans);
        self.enter_drop_scope(root_scope);
    }

    pub(super) fn with_drop_scope<R>(
        &mut self,
        id: NodeId,
        f: impl FnOnce(&mut Self) -> Result<R, LowerToIrError>,
    ) -> Result<R, LowerToIrError> {
        self.enter_drop_scope(id);
        let result = f(self);
        self.exit_drop_scope_if_active(id);
        result
    }

    pub(super) fn set_drop_plans(&mut self, drop_plans: &'a sem::DropPlanMap) {
        self.drop_manager.set_plans(drop_plans);
    }

    /// Captures the active drop scope stack for branch-local lowering.
    pub(super) fn drop_scopes_snapshot(&self) -> DropSnapshot {
        self.drop_manager.snapshot()
    }

    /// Restores a previously captured drop scope stack snapshot.
    pub(super) fn restore_drop_scopes(&mut self, snapshot: &DropSnapshot) {
        self.drop_manager.restore(snapshot);
    }

    pub(super) fn invalidate_drop_liveness(&mut self) {
        self.drop_manager.invalidate_known_live();
    }

    pub(super) fn enter_drop_scope(&mut self, id: NodeId) {
        self.drop_manager.enter_scope(id);
    }

    /// Pops and emits a scope if it is active, used by the RAII guard.
    fn exit_drop_scope_if_active(&mut self, id: NodeId) {
        if let Some(scope_id) = self.drop_manager.exit_scope_if_active(id)
            && let Err(err) = self.emit_drop_scope(scope_id)
        {
            panic!("backend drop scope exit failed: {err:?}");
        }
    }

    /// Emits drops until the drop scope stack is at the requested depth.
    pub(super) fn emit_drops_to_depth(&mut self, depth: usize) -> Result<(), LowerToIrError> {
        let scopes = self.drop_manager.pop_to_depth(depth);
        for scope_id in scopes {
            self.emit_drop_scope(scope_id)?;
        }
        Ok(())
    }

    /// Emits drops corresponding to a semantic statement's drop depth.
    pub(super) fn emit_drops_for_stmt(&mut self, stmt_id: NodeId) -> Result<(), LowerToIrError> {
        let Some(depth) = self.drop_manager.depth_for_stmt(stmt_id) else {
            return Ok(());
        };
        self.emit_drops_to_depth(depth)
    }

    /// Emit all remaining drops and terminate with return.
    pub(super) fn emit_root_return(
        &mut self,
        value: Option<ValueId>,
    ) -> Result<(), LowerToIrError> {
        self.emit_drops_to_depth(0)?;
        self.builder.terminate(Terminator::Return { value });
        Ok(())
    }

    /// Apply all call-site drop side effects in one place:
    /// - drop temporaries selected by the call plan drop mask
    /// - clear sink source flags (ownership transferred to callee)
    /// - mark out/inout destinations as initialized when applicable
    pub(super) fn apply_call_drop_effects(
        &mut self,
        call_plan: &sem::CallPlan,
        args: &[sem::CallArg],
        receiver_value: Option<&CallInputValue>,
        arg_values: &[CallInputValue],
    ) -> Result<(), LowerToIrError> {
        self.emit_call_input_drops(call_plan, receiver_value, arg_values)?;
        self.clear_sink_input_drop_flags(call_plan, receiver_value, arg_values);
        self.mark_call_out_init_flags(args, arg_values);
        Ok(())
    }

    pub(super) fn emit_drop_for_def_if_live(
        &mut self,
        def_id: DefId,
        ty_id: TypeId,
    ) -> Result<(), LowerToIrError> {
        if !self.drop_manager.is_active() {
            return Ok(());
        }

        let ty = self.type_map.type_table().get(ty_id).clone();
        if matches!(drop_kind(&ty), DropKind::Trivial) {
            return Ok(());
        }

        let flag_addr = self
            .drop_manager
            .flag(def_id)
            .unwrap_or_else(|| panic!("backend drop missing flag for {:?}", def_id));
        self.emit_drop_if_flag(flag_addr, |lowerer| {
            lowerer.trace_drop(format!("drop-if-live {}", lowerer.def(def_id).name));
            let local = lowerer.lookup_local(def_id);
            let (value, is_addr) = match local.storage {
                LocalStorage::Value(value) => (value, false),
                LocalStorage::Addr(addr) => (addr, true),
            };
            lowerer.emit_drop_for_value(value, &ty, is_addr)
        })
    }

    pub(super) fn set_drop_flag_for_def(&mut self, def_id: DefId, value: bool) {
        if !self.drop_manager.is_active() {
            return;
        }

        let ty = self.def_type(def_id);
        if matches!(drop_kind(&ty), DropKind::Trivial) {
            return;
        }

        let flag_addr = match self.drop_manager.flag(def_id) {
            Some(addr) => addr,
            None => {
                let addr = self.create_drop_flag(def_id);
                self.drop_manager.insert_flag(def_id, addr);
                addr
            }
        };
        self.trace_drop(format!("drop-flag {} = {}", self.def(def_id).name, value));
        self.drop_manager.set_known_live(def_id, value);
        self.store_drop_flag(flag_addr, value);
    }

    fn emit_call_input_drops(
        &mut self,
        call_plan: &sem::CallPlan,
        receiver_value: Option<&CallInputValue>,
        arg_values: &[CallInputValue],
    ) -> Result<(), LowerToIrError> {
        let expected = (call_plan.has_receiver as usize) + arg_values.len();
        if call_plan.drop_mask.len() != expected {
            panic!(
                "backend call drop mask length mismatch: expected {}, got {}",
                expected,
                call_plan.drop_mask.len()
            );
        }

        let mut input_index = 0;
        if call_plan.has_receiver {
            let receiver = receiver_value.unwrap_or_else(|| {
                panic!("backend call drop mask missing receiver value for call plan");
            });
            if call_plan.drop_mask[input_index] && receiver.drop_def.is_none() {
                self.emit_drop_for_value(receiver.value, &receiver.ty, receiver.is_addr)?;
            }
            input_index += 1;
        }

        for arg in arg_values {
            if call_plan.drop_mask[input_index] && arg.drop_def.is_none() {
                self.emit_drop_for_value(arg.value, &arg.ty, arg.is_addr)?;
            }
            input_index += 1;
        }

        Ok(())
    }

    fn clear_sink_input_drop_flags(
        &mut self,
        call_plan: &sem::CallPlan,
        receiver_value: Option<&CallInputValue>,
        arg_values: &[CallInputValue],
    ) {
        let expected = (call_plan.has_receiver as usize) + arg_values.len();
        if call_plan.input_modes.len() != expected {
            panic!(
                "backend call input modes length mismatch: expected {}, got {}",
                expected,
                call_plan.input_modes.len()
            );
        }

        let mut input_index = 0;
        if call_plan.has_receiver {
            let receiver = receiver_value.unwrap_or_else(|| {
                panic!("backend call input modes missing receiver value for call plan");
            });
            if call_plan.input_modes[input_index] == ParamMode::Sink {
                self.clear_sink_flag_for_input(receiver);
            }
            input_index += 1;
        }

        for arg in arg_values {
            if call_plan.input_modes[input_index] == ParamMode::Sink {
                self.clear_sink_flag_for_input(arg);
            }
            input_index += 1;
        }
    }

    fn clear_sink_flag_for_input(&mut self, input: &CallInputValue) {
        let Some(def_id) = input.drop_def else {
            return;
        };
        if input.ty.needs_drop() {
            self.set_drop_flag_for_def(def_id, false);
        }
    }

    fn mark_call_out_init_flags(&mut self, args: &[sem::CallArg], arg_values: &[CallInputValue]) {
        for (arg, input) in args.iter().zip(arg_values.iter()) {
            match arg {
                sem::CallArg::Out { place, init, .. } => {
                    let Some(def_id) = input.drop_def else {
                        continue;
                    };
                    let should_set = match place.kind {
                        sem::PlaceExprKind::Var { .. } => init.is_init || init.promotes_full,
                        _ => init.promotes_full || self.out_place_promotes_full(place, init),
                    };
                    if should_set {
                        self.set_drop_flag_for_def(def_id, true);
                    }
                }
                sem::CallArg::InOut { .. } => {
                    if let Some(def_id) = input.drop_def {
                        self.set_drop_flag_for_def(def_id, true);
                    }
                }
                _ => {}
            }
        }
    }

    fn out_place_promotes_full(&self, place: &sem::PlaceExpr, init: &InitInfo) -> bool {
        if init.promotes_full || !init.is_init {
            return false;
        }

        let (base_def, proj) = match &place.kind {
            sem::PlaceExprKind::StructField { target, field } => match &target.kind {
                sem::PlaceExprKind::Var { def_id, .. } => {
                    (*def_id, OutProj::Struct(field.as_str()))
                }
                _ => return false,
            },
            sem::PlaceExprKind::TupleField { target, index } => match &target.kind {
                sem::PlaceExprKind::Var { def_id, .. } => (*def_id, OutProj::Tuple(*index)),
                _ => return false,
            },
            _ => return false,
        };

        let base_ty = self.def_type(base_def);
        match (proj, base_ty) {
            (OutProj::Struct(field), Type::Struct { fields, .. }) => {
                fields.len() == 1 && fields[0].name == *field
            }
            (OutProj::Tuple(index), Type::Tuple { field_tys }) => {
                field_tys.len() == 1 && index == 0
            }
            _ => false,
        }
    }

    fn emit_drop_scope(&mut self, id: NodeId) -> Result<(), LowerToIrError> {
        let drop_plans = self
            .drop_manager
            .plans()
            .unwrap_or_else(|| panic!("backend drop scope missing drop plans"));
        let scope = drop_plans
            .scope_for(id)
            .unwrap_or_else(|| panic!("backend drop scope missing plan for {:?}", id));

        for item in scope.drops.iter().rev() {
            self.emit_drop_item(item)?;
        }

        Ok(())
    }

    fn emit_drop_item(&mut self, item: &sem::DropItem) -> Result<(), LowerToIrError> {
        if self.drop_manager.known_live(item.def_id) == Some(false) {
            return Ok(());
        }
        // Skip drop for locals not yet registered in the lowerer. This happens when
        // an early return unwinds past a let-binding that hasn't been lowered yet
        // (e.g. generated spawn function bodies with guard-return patterns).
        if self.locals.get(item.def_id).is_none() {
            return Ok(());
        }

        match item.guard {
            sem::DropGuard::Always => {
                if let Some(flag_addr) = self.drop_manager.flag(item.def_id) {
                    self.emit_drop_if_flag(flag_addr, |lowerer| {
                        lowerer.emit_drop_for_def(item.def_id, item.ty)
                    })?;
                } else {
                    self.emit_drop_for_def(item.def_id, item.ty)?;
                }
                Ok(())
            }
            sem::DropGuard::IfInitialized => {
                let flag_addr = match self.drop_manager.flag(item.def_id) {
                    Some(addr) => addr,
                    None => {
                        let addr = self.create_drop_flag(item.def_id);
                        self.drop_manager.insert_flag(item.def_id, addr);
                        self.store_drop_flag(addr, false);
                        addr
                    }
                };

                self.emit_drop_if_flag(flag_addr, |lowerer| {
                    lowerer.emit_drop_for_def(item.def_id, item.ty)
                })
            }
        }
    }

    fn emit_drop_if_flag(
        &mut self,
        flag_addr: ValueId,
        f: impl FnOnce(&mut Self) -> Result<(), LowerToIrError>,
    ) -> Result<(), LowerToIrError> {
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
        f(self)?;
        self.builder.terminate(Terminator::Br {
            target: cont_bb,
            args: Vec::new(),
        });

        self.builder.select_block(cont_bb);
        Ok(())
    }

    fn emit_drop_for_def(&mut self, def_id: DefId, ty_id: TypeId) -> Result<(), LowerToIrError> {
        self.trace_drop(format!("drop {}", self.def(def_id).name));
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
    ) -> Result<(), LowerToIrError> {
        if matches!(drop_kind(ty), DropKind::Trivial) {
            return Ok(());
        }

        if let Type::Heap { elem_ty } = ty {
            if is_addr {
                return self.drop_value_at_addr(value, ty);
            }

            match drop_kind(elem_ty) {
                DropKind::Trivial => {}
                DropKind::Shallow => {
                    let def_id = self.drop_glue.def_id_for(elem_ty, self.type_map);
                    let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
                    let _ = self
                        .builder
                        .call(Callee::Direct(def_id), vec![value], unit_ty);
                }
                DropKind::Deep => {
                    self.drop_value_at_addr(value, elem_ty)?;
                }
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
    ) -> Result<(), LowerToIrError> {
        match drop_kind(ty) {
            DropKind::Trivial => return Ok(()),
            DropKind::Shallow => {
                let def_id = self.drop_glue.def_id_for(ty, self.type_map);
                let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
                let _ = self
                    .builder
                    .call(Callee::Direct(def_id), vec![addr], unit_ty);
                return Ok(());
            }
            DropKind::Deep => {}
        }

        match ty {
            Type::String => self.drop_string(addr),
            Type::Heap { elem_ty } => self.drop_heap(addr, elem_ty),
            Type::Struct { fields, .. } => self.drop_struct(addr, fields),
            Type::Tuple { field_tys } => self.drop_tuple(addr, field_tys),
            Type::Array { dims, .. } => self.drop_array(addr, ty, dims),
            Type::DynArray { elem_ty } => self.drop_dyn_array(addr, elem_ty),
            Type::Set { elem_ty } => self.drop_set(addr, elem_ty),
            Type::Map { .. } => self.drop_map(addr),
            Type::Enum { variants, .. } => self.drop_enum(addr, ty, variants),
            Type::ErrorUnion { ok_ty, err_tys } => {
                let variants = std::iter::once(EnumVariant {
                    name: "Ok".to_string(),
                    payload: vec![(*ok_ty.clone())],
                })
                .chain(
                    err_tys
                        .iter()
                        .enumerate()
                        .map(|(index, err_ty)| EnumVariant {
                            name: format!("Err{}", index),
                            payload: vec![err_ty.clone()],
                        }),
                )
                .collect::<Vec<_>>();
                self.drop_enum(addr, ty, &variants)
            }
            other => panic!("backend drop not implemented for {:?}", other),
        }
    }

    fn drop_string(&mut self, addr: ValueId) -> Result<(), LowerToIrError> {
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self
            .builder
            .call(Callee::Runtime(RuntimeFn::StringDrop), vec![addr], unit_ty);
        Ok(())
    }

    fn drop_dyn_array(&mut self, addr: ValueId, elem_ty: &Type) -> Result<(), LowerToIrError> {
        let u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
        let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);

        let cap_val = self.load_field(addr, 2, u32_ty);
        let owned_mask = self.builder.const_int(0x8000_0000, false, 32, u32_ty);
        let owned_bits =
            self.builder
                .binop(crate::core::ir::BinOp::And, cap_val, owned_mask, u32_ty);
        let zero_u32 = self.builder.const_int(0, false, 32, u32_ty);
        let is_owned = self
            .builder
            .cmp(crate::core::ir::CmpOp::Ne, owned_bits, zero_u32, bool_ty);

        let owned_bb = self.builder.add_block();
        let ret_bb = self.builder.add_block();
        self.builder.terminate(Terminator::CondBr {
            cond: is_owned,
            then_bb: owned_bb,
            then_args: Vec::new(),
            else_bb: ret_bb,
            else_args: Vec::new(),
        });

        self.builder.select_block(owned_bb);
        let elem_ir_ty = self.type_lowerer.lower_type(elem_ty);
        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
        let ptr_val = self.load_field(addr, 0, elem_ptr_ty);
        let len_u32 = self.load_field(addr, 1, u32_ty);
        let len_u64 = self.builder.int_extend(len_u32, u64_ty, false);

        if !matches!(drop_kind(elem_ty), DropKind::Trivial) {
            let idx_addr = self.alloc_local_addr(u64_ty);
            self.builder.store(idx_addr, len_u64);

            let loop_head = self.builder.add_block();
            let loop_body = self.builder.add_block();
            let loop_done = self.builder.add_block();
            self.builder.terminate(Terminator::Br {
                target: loop_head,
                args: Vec::new(),
            });

            self.builder.select_block(loop_head);
            let idx_val = self.builder.load(idx_addr, u64_ty);
            let zero_u64 = self.builder.const_int(0, false, 64, u64_ty);
            let has_items =
                self.builder
                    .cmp(crate::core::ir::CmpOp::Ne, idx_val, zero_u64, bool_ty);
            self.builder.terminate(Terminator::CondBr {
                cond: has_items,
                then_bb: loop_body,
                then_args: Vec::new(),
                else_bb: loop_done,
                else_args: Vec::new(),
            });

            self.builder.select_block(loop_body);
            let one_u64 = self.builder.const_int(1, false, 64, u64_ty);
            let next_idx =
                self.builder
                    .binop(crate::core::ir::BinOp::Sub, idx_val, one_u64, u64_ty);
            self.builder.store(idx_addr, next_idx);
            let elem_addr = self.builder.index_addr(ptr_val, next_idx, elem_ptr_ty);
            self.drop_value_at_addr(elem_addr, elem_ty)?;
            self.builder.terminate(Terminator::Br {
                target: loop_head,
                args: Vec::new(),
            });

            self.builder.select_block(loop_done);
        }

        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self
            .builder
            .call(Callee::Runtime(RuntimeFn::Free), vec![ptr_val], unit_ty);
        self.builder.terminate(Terminator::Br {
            target: ret_bb,
            args: Vec::new(),
        });

        self.builder.select_block(ret_bb);
        Ok(())
    }

    fn drop_set(&mut self, addr: ValueId, elem_ty: &Type) -> Result<(), LowerToIrError> {
        let u32_ty = self.type_lowerer.lower_type(&Type::uint(32));
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);

        let cap_val = self.load_field(addr, 2, u32_ty);
        let owned_mask = self.builder.const_int(0x8000_0000, false, 32, u32_ty);
        let owned_bits =
            self.builder
                .binop(crate::core::ir::BinOp::And, cap_val, owned_mask, u32_ty);
        let zero_u32 = self.builder.const_int(0, false, 32, u32_ty);
        let is_owned = self
            .builder
            .cmp(crate::core::ir::CmpOp::Ne, owned_bits, zero_u32, bool_ty);

        let owned_bb = self.builder.add_block();
        let ret_bb = self.builder.add_block();
        self.builder.terminate(Terminator::CondBr {
            cond: is_owned,
            then_bb: owned_bb,
            then_args: Vec::new(),
            else_bb: ret_bb,
            else_args: Vec::new(),
        });

        self.builder.select_block(owned_bb);
        let elem_ir_ty = self.type_lowerer.lower_type(elem_ty);
        let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
        let ptr_val = self.load_field(addr, 0, elem_ptr_ty);
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self
            .builder
            .call(Callee::Runtime(RuntimeFn::Free), vec![ptr_val], unit_ty);
        self.builder.terminate(Terminator::Br {
            target: ret_bb,
            args: Vec::new(),
        });

        self.builder.select_block(ret_bb);
        Ok(())
    }

    fn drop_map(&mut self, addr: ValueId) -> Result<(), LowerToIrError> {
        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self
            .builder
            .call(Callee::Runtime(RuntimeFn::MapDrop), vec![addr], unit_ty);
        Ok(())
    }

    fn drop_heap(&mut self, addr: ValueId, elem_ty: &Type) -> Result<(), LowerToIrError> {
        let ptr_ty = self.type_lowerer.lower_type(&Type::Heap {
            elem_ty: Box::new(elem_ty.clone()),
        });
        let ptr_val = self.builder.load(addr, ptr_ty);

        match drop_kind(elem_ty) {
            DropKind::Trivial => {}
            DropKind::Shallow => {
                let def_id = self.drop_glue.def_id_for(elem_ty, self.type_map);
                let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
                let _ = self
                    .builder
                    .call(Callee::Direct(def_id), vec![ptr_val], unit_ty);
            }
            DropKind::Deep => {
                self.drop_value_at_addr(ptr_val, elem_ty)?;
            }
        }

        let unit_ty = self.type_lowerer.lower_type(&Type::Unit);
        let _ = self
            .builder
            .call(Callee::Runtime(RuntimeFn::Free), vec![ptr_val], unit_ty);
        Ok(())
    }

    fn drop_struct(&mut self, addr: ValueId, fields: &[StructField]) -> Result<(), LowerToIrError> {
        for (index, field) in fields.iter().enumerate().rev() {
            if matches!(drop_kind(&field.ty), DropKind::Trivial) {
                continue;
            }
            let field_ty = self.type_lowerer.lower_type(&field.ty);
            let field_addr = self.field_addr_typed(addr, index, field_ty);
            self.drop_value_at_addr(field_addr, &field.ty)?;
        }
        Ok(())
    }

    fn drop_tuple(&mut self, addr: ValueId, field_tys: &[Type]) -> Result<(), LowerToIrError> {
        for (index, field_ty) in field_tys.iter().enumerate().rev() {
            if matches!(drop_kind(field_ty), DropKind::Trivial) {
                continue;
            }
            let field_ir_ty = self.type_lowerer.lower_type(field_ty);
            let field_addr = self.field_addr_typed(addr, index, field_ir_ty);
            self.drop_value_at_addr(field_addr, field_ty)?;
        }
        Ok(())
    }

    fn drop_array(
        &mut self,
        addr: ValueId,
        ty: &Type,
        dims: &[usize],
    ) -> Result<(), LowerToIrError> {
        let Some(elem_ty) = ty.array_item_type() else {
            panic!("backend drop array missing element type");
        };
        if matches!(drop_kind(&elem_ty), DropKind::Trivial) {
            return Ok(());
        }

        let len = *dims
            .first()
            .unwrap_or_else(|| panic!("backend drop array missing dims"));
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

    fn drop_enum(
        &mut self,
        addr: ValueId,
        ty: &Type,
        variants: &[EnumVariant],
    ) -> Result<(), LowerToIrError> {
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
                .any(|ty| !matches!(drop_kind(ty), DropKind::Trivial))
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
        for (variant, (tag, field_offsets, name)) in variants.iter().zip(layout_variants.iter()) {
            if !variant
                .payload
                .iter()
                .any(|ty| !matches!(drop_kind(ty), DropKind::Trivial))
            {
                continue;
            }

            if variant.payload.len() != field_offsets.len() {
                panic!(
                    "backend enum drop payload mismatch for {}: {} offsets, {} tys",
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
            for (payload_ty, offset) in variant.payload.iter().zip(field_offsets.iter()).rev() {
                if matches!(drop_kind(payload_ty), DropKind::Trivial) {
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

    fn type_id_for_enum(&self, ty: &Type) -> TypeId {
        self.type_map
            .type_table()
            .lookup_id(ty)
            .unwrap_or_else(|| panic!("backend drop missing enum type id for {:?}", ty))
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

fn is_shallow_named(ty: &Type) -> bool {
    match ty {
        Type::Struct { fields, .. } => fields.is_empty(),
        Type::Enum { variants, .. } => variants.is_empty(),
        Type::ErrorUnion { ok_ty, err_tys } => {
            matches!(ok_ty.as_ref(), Type::Unit)
                && err_tys.iter().all(|err_ty| err_ty == &Type::Unit)
        }
        _ => false,
    }
}
