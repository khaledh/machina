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
    free: Vec<FreeRange>,
}

impl Default for StackAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl StackAllocator {
    pub fn new() -> Self {
        Self {
            next_slot: 0,
            free: Vec::new(),
        }
    }

    pub fn alloc_slot(&mut self) -> StackSlotId {
        self.alloc_slots(1)
    }

    pub fn alloc_slots(&mut self, count: u32) -> StackSlotId {
        if let Some((idx, range)) = self
            .free
            .iter_mut()
            .enumerate()
            .find(|(_, range)| range.count >= count)
        {
            let end = range.start + range.count - 1;
            if range.count == count {
                self.free.remove(idx);
            } else {
                range.count -= count;
            }
            return StackSlotId(end);
        }

        let start = self.next_slot + count - 1;
        self.next_slot += count;
        StackSlotId(start)
    }

    pub fn free_slots(&mut self, start_slot: StackSlotId, count: u32) {
        let end = start_slot.0;
        let start = end + 1 - count;
        let mut new_start = start;
        let mut new_end = end;

        let mut insert_idx = 0;
        while insert_idx < self.free.len() && self.free[insert_idx].start < new_start {
            insert_idx += 1;
        }

        if insert_idx > 0 {
            let prev = &self.free[insert_idx - 1];
            let prev_end = prev.start + prev.count - 1;
            if prev_end + 1 == new_start {
                new_start = prev.start;
                self.free.remove(insert_idx - 1);
                insert_idx -= 1;
            }
        }

        if insert_idx < self.free.len() {
            let next = &self.free[insert_idx];
            if new_end + 1 == next.start {
                new_end = next.start + next.count - 1;
                self.free.remove(insert_idx);
            }
        }

        self.free.insert(
            insert_idx,
            FreeRange {
                start: new_start,
                count: new_end - new_start + 1,
            },
        );
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

#[derive(Debug, Clone, Copy)]
struct FreeRange {
    start: u32,
    count: u32,
}
