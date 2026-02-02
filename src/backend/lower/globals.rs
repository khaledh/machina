//! Shared global arena for SSA lowering.
//!
//! Provides stable, module-wide IDs for global data such as string literals.

use crate::ir::ir::{GlobalData, GlobalId};
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct GlobalArena {
    globals: Vec<GlobalData>,
    next_id: u32,
    by_bytes: HashMap<Vec<u8>, GlobalId>,
}

impl GlobalArena {
    pub fn new() -> Self {
        Self {
            globals: Vec::new(),
            next_id: 0,
            by_bytes: HashMap::new(),
        }
    }

    pub fn add_bytes(&mut self, bytes: Vec<u8>) -> GlobalId {
        if let Some(id) = self.by_bytes.get(&bytes).copied() {
            return id;
        }
        let id = GlobalId(self.next_id);
        self.next_id += 1;
        self.globals.push(GlobalData {
            id,
            bytes,
            align: 1,
        });
        if let Some(global) = self.globals.last() {
            self.by_bytes.insert(global.bytes.clone(), id);
        }
        id
    }

    pub fn len(&self) -> usize {
        self.globals.len()
    }

    pub fn slice_from(&self, start: usize) -> Vec<GlobalData> {
        self.globals.get(start..).unwrap_or(&[]).to_vec()
    }

    pub fn into_globals(self) -> Vec<GlobalData> {
        self.globals
    }
}
