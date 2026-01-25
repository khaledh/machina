//! Local SSA value tracking and snapshotting.

use std::collections::HashMap;

use crate::resolve::DefId;
use crate::ssa::IrTypeId;
use crate::ssa::model::ir::ValueId;

/// Storage kind for a local variable.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum LocalStorage {
    /// The local is tracked as a pure SSA value.
    Value(ValueId),
    /// The local lives in memory; this is the address of the slot.
    Addr(ValueId),
}

/// A local variable's current storage and value type.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) struct LocalValue {
    pub(super) storage: LocalStorage,
    pub(super) value_ty: IrTypeId,
}

impl LocalValue {
    pub(super) fn value(value: ValueId, value_ty: IrTypeId) -> Self {
        Self {
            storage: LocalStorage::Value(value),
            value_ty,
        }
    }

    pub(super) fn addr(addr: ValueId, value_ty: IrTypeId) -> Self {
        Self {
            storage: LocalStorage::Addr(addr),
            value_ty,
        }
    }

    pub(super) fn storage_value(self) -> ValueId {
        match self.storage {
            LocalStorage::Value(value) | LocalStorage::Addr(value) => value,
        }
    }

    pub(super) fn with_storage(self, storage: LocalStorage) -> Self {
        Self {
            storage,
            value_ty: self.value_ty,
        }
    }
}

#[derive(Clone)]
pub(super) struct LocalSnapshot {
    values: HashMap<DefId, LocalValue>,
}

pub(super) struct LocalMap {
    values: HashMap<DefId, LocalValue>,
}

impl LocalMap {
    pub(super) fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub(super) fn insert(&mut self, def_id: DefId, value: LocalValue) {
        self.values.insert(def_id, value);
    }

    pub(super) fn get(&self, def_id: DefId) -> Option<LocalValue> {
        self.values.get(&def_id).copied()
    }

    pub(super) fn snapshot(&self) -> LocalSnapshot {
        LocalSnapshot {
            values: self.values.clone(),
        }
    }

    pub(super) fn restore(&mut self, snapshot: &LocalSnapshot) {
        self.values = snapshot.values.clone();
    }

    /// Returns locals in a deterministic order (sorted by DefId).
    pub(super) fn ordered(&self) -> Vec<(DefId, LocalValue)> {
        let mut locals: Vec<_> = self
            .values
            .iter()
            .map(|(def, local)| (*def, *local))
            .collect();
        locals.sort_by_key(|(def, _)| def.0);
        locals
    }

    pub(super) fn args_for(&self, defs: &[DefId]) -> Option<Vec<ValueId>> {
        let mut args = Vec::with_capacity(defs.len());
        for def in defs {
            let local = self.values.get(def)?;
            args.push(local.storage_value());
        }
        Some(args)
    }

    /// Resets locals from block parameters, preserving storage kinds and value types.
    pub(super) fn set_from_params_like(
        &mut self,
        defs: &[DefId],
        locals: &[LocalValue],
        params: &[ValueId],
    ) {
        self.values.clear();
        for ((def, local), value) in defs.iter().zip(locals.iter()).zip(params.iter()) {
            let storage = match local.storage {
                LocalStorage::Value(_) => LocalStorage::Value(*value),
                LocalStorage::Addr(_) => LocalStorage::Addr(*value),
            };
            self.values.insert(*def, local.with_storage(storage));
        }
    }
}
