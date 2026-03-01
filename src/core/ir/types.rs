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

    pub fn add_placeholder_named(&mut self, name: String) -> IrTypeId {
        self.add_named(IrTypeKind::Struct { fields: Vec::new() }, name)
    }

    pub fn update_kind(&mut self, id: IrTypeId, kind: IrTypeKind) {
        self.types[id.index()].kind = kind;
        self.layout_cache.invalidate(id);
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

    /// Returns true if the type fits in a general-purpose register.
    ///
    /// Scalar types (unit, bool, integers, pointers, function pointers) fit in
    /// registers. Aggregate types (arrays, tuples, structs, blobs) do not.
    pub fn is_reg_type(&self, id: IrTypeId) -> bool {
        matches!(
            self.kind(id),
            IrTypeKind::Unit
                | IrTypeKind::Bool
                | IrTypeKind::Int { .. }
                | IrTypeKind::Ptr { .. }
                | IrTypeKind::Fn { .. }
        )
    }

    /// Returns the scalar register width used when loading/storing values of
    /// this type directly. Aggregate types fall back to their full layout size.
    pub fn scalar_size_for_layout(&self, id: IrTypeId, layout: &IrLayout) -> u32 {
        match self.kind(id) {
            IrTypeKind::Unit => 0,
            IrTypeKind::Bool => 1,
            IrTypeKind::Int { bits, .. } => (*bits as u32) / 8,
            IrTypeKind::Ptr { .. } | IrTypeKind::Fn { .. } => 8,
            _ => layout.size() as u32,
        }
    }

    /// Returns whether this type must be passed indirectly via sret on ARM64.
    pub fn needs_sret_for_layout(&self, id: IrTypeId, layout: &IrLayout) -> bool {
        !matches!(
            self.kind(id),
            IrTypeKind::Unit | IrTypeKind::Bool | IrTypeKind::Int { .. } | IrTypeKind::Ptr { .. }
        ) && layout.size() as u32 > 16
    }
}
