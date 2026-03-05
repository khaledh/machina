use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug)]
pub struct NodeIdGen {
    next_id: u32,
}

impl Default for NodeIdGen {
    fn default() -> Self {
        Self::new()
    }
}

impl NodeIdGen {
    pub fn new() -> Self {
        Self { next_id: 1 } // NodeId 0 is reserved
    }

    pub fn new_id(&mut self) -> NodeId {
        let id = NodeId(self.next_id);
        self.next_id += 1;
        id
    }
}
