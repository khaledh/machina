#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackSlotId(pub u32);

impl StackSlotId {
    pub fn offset_bytes(&self) -> u32 {
        self.0 * 8 // 8-byte slots for 64-bit values
    }
}

pub struct SpillAllocator {
    next_slot: u32,
}

impl SpillAllocator {
    pub fn new() -> Self {
        Self { next_slot: 0 }
    }

    pub fn alloc_slot(&mut self) -> StackSlotId {
        let slot = StackSlotId(self.next_slot);
        self.next_slot += 1;
        slot
    }

    pub fn alloc_slots(&mut self, count: u32) -> StackSlotId {
        let start = self.next_slot;
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
