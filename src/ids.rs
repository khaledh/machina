// AST Node IDs

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

pub struct NodeIdGen {
    next_id: u32,
}

impl NodeIdGen {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    pub fn new_id(&mut self) -> NodeId {
        let id = NodeId(self.next_id);
        self.next_id += 1;
        id
    }
}

// Definition IDs

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

pub struct DefIdGen {
    next_id: u32,
}

impl DefIdGen {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    pub fn new_id(&mut self) -> DefId {
        let id = DefId(self.next_id);
        self.next_id += 1;
        id
    }
}
