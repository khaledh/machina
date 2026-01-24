//! Shared global arena for SSA lowering.
//!
//! Provides stable, module-wide IDs for global data such as string literals.

use crate::ssa::model::ir::{GlobalData, GlobalId};

#[derive(Debug, Default)]
pub struct GlobalArena {
    globals: Vec<GlobalData>,
    next_id: u32,
}

impl GlobalArena {
    pub fn new() -> Self {
        Self {
            globals: Vec::new(),
            next_id: 0,
        }
    }

    pub fn add_bytes(&mut self, bytes: Vec<u8>) -> GlobalId {
        let id = GlobalId(self.next_id);
        self.next_id += 1;
        self.globals.push(GlobalData {
            id,
            bytes,
            align: 1,
        });
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
