use super::layout::{IrLayout, IrLayoutCache};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrTypeId(pub u32);

impl IrTypeId {
    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrTypeKind {
    Unit,
    Bool,
    Int {
        signed: bool,
        bits: u8,
    },
    Ptr {
        elem: IrTypeId,
    },
    Array {
        elem: IrTypeId,
        dims: Vec<u64>,
    },
    Tuple {
        fields: Vec<IrTypeId>,
    },
    Struct {
        fields: Vec<IrStructField>,
    },
    Blob {
        size: u64,
        align: u64,
    },
    Fn {
        params: Vec<IrTypeId>,
        ret: IrTypeId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrStructField {
    pub name: String,
    pub ty: IrTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrTypeInfo {
    pub kind: IrTypeKind,
    pub name: Option<String>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct IrTypeCache {
    types: Vec<IrTypeInfo>,
    layout_cache: IrLayoutCache,
}

impl IrTypeCache {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            layout_cache: IrLayoutCache::new(),
        }
    }

    /// Adds an anonymous type definition to the table.
    pub fn add(&mut self, kind: IrTypeKind) -> IrTypeId {
        let id = IrTypeId(self.types.len() as u32);
        self.types.push(IrTypeInfo { kind, name: None });
        id
    }

    /// Adds a named type definition to the table.
    pub fn add_named(&mut self, kind: IrTypeKind, name: String) -> IrTypeId {
        let id = IrTypeId(self.types.len() as u32);
        self.types.push(IrTypeInfo {
            kind,
            name: Some(name),
        });
        id
    }

    pub fn get(&self, id: IrTypeId) -> &IrTypeInfo {
        &self.types[id.index()]
    }

    pub fn kind(&self, id: IrTypeId) -> &IrTypeKind {
        &self.get(id).kind
    }

    /// Returns layout information for a type, computing it on demand.
    pub fn layout(&mut self, id: IrTypeId) -> IrLayout {
        self.layout_cache.layout(&self.types, id)
    }
}
