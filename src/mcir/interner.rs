use std::collections::HashMap;

use crate::mcir::types::{GlobalId, GlobalItem, GlobalPayload, GlobalSection};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlobalKey {
    pub kind: GlobalSection,
    pub payload: GlobalPayload,
}

#[derive(Debug)]
pub struct GlobalInterner {
    items: Vec<GlobalItem>,
    map: HashMap<GlobalKey, GlobalId>,
}

impl GlobalInterner {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub fn intern(&mut self, payload: GlobalPayload, kind: GlobalSection) -> GlobalId {
        let key = GlobalKey {
            kind: kind.clone(),
            payload: payload.clone(),
        };

        if let Some(&id) = self.map.get(&key) {
            return id;
        }

        let id = GlobalId(self.items.len() as u32);
        let item = GlobalItem { id, kind, payload };
        self.items.push(item);
        self.map.insert(key, id);

        id
    }

    pub fn take(self) -> Vec<GlobalItem> {
        self.items
    }
}
