#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackSlotId(pub u32);

impl StackSlotId {
    /// Offset relative to the top of stack allocation area.
    pub fn offset_bytes(&self) -> u32 {
        self.0 * 8 + 8
    }
}

/// We allocate to an increasing slot number, but the stack is allocated in decreasing order.
/// So slot.offset_bytes() is the offset from the *top* address of the stack,
/// e.g. slot 0 is at offset 8 from the top of the stack.
/// Codegen will need to invert the offset relative to sp.
///
/// Higher address
///  ^
///  |  +--------------------+ <- offset 0              ---+
///  |  |     Slot 0         |                             |
///  |  +--------------------+ <- slot 0 offset = 8        |
///  |  |     Slot 1         |                             |
///  |  +--------------------+ <- slot 1 offset = 16       |
///  |  |     Slot 2  Arr[2] |                             +-- frame size = 40 bytes
///  |  +--                --+                             |
///  |  |     Slot 3  Arr[1] |  Compound (3 slots)         |
///  |  +--                --+                             |
///  |  |     Slot 4  Arr[0] |                             |
///  |  +--------------------+ <- slot 4 offset = 40     --+
///  |  | padding = 8 bytes  | (padding to align stack to 16 bytes)
///  |  +--------------------+ <- sp
///  v
/// Lower address
///
pub struct StackAllocator {
    next_slot: u32,
}

impl Default for StackAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl StackAllocator {
    pub fn new() -> Self {
        Self { next_slot: 0 }
    }

    pub fn alloc_slot(&mut self) -> StackSlotId {
        self.alloc_slots(1)
    }

    pub fn alloc_slots(&mut self, count: u32) -> StackSlotId {
        let start = self.next_slot + count - 1;
        self.next_slot += count;
        StackSlotId(start)
    }

    #[inline]
    pub fn total_slots(&self) -> u32 {
        self.next_slot
    }

    #[inline]
    pub fn frame_size_bytes(&self) -> u32 {
        self.total_slots() * 8
    }
}
