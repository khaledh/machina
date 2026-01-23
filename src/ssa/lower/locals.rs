//! Local SSA value tracking and snapshotting.

use std::collections::HashMap;

use crate::resolve::DefId;
use crate::ssa::IrTypeId;
use crate::ssa::model::ir::ValueId;

/// A local variable's current SSA value and type.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) struct LocalValue {
    pub(super) value: ValueId,
    pub(super) ty: IrTypeId,
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
            args.push(local.value);
        }
        Some(args)
    }

    /// Resets locals from block parameters.
    pub(super) fn set_from_params(&mut self, defs: &[DefId], tys: &[IrTypeId], params: &[ValueId]) {
        self.values.clear();
        for ((def, ty), value) in defs.iter().zip(tys.iter()).zip(params.iter()) {
            self.values.insert(
                *def,
                LocalValue {
                    value: *value,
                    ty: *ty,
                },
            );
        }
    }
}
