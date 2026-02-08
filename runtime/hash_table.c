#include <stdint.h>

#include "hash_table.h"

void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2);
void __mc_memcpy(mc_slice_t *dst, const mc_slice_t *src);
void *__mc_alloc(size_t size, size_t align);
void __mc_free(void *ptr);

// CheckKind::Range = 2
#define MC_TRAP_RANGE 2

static uint8_t mc_bytes_eq(const uint8_t *a, const uint8_t *b, uint64_t len) {
    for (uint64_t i = 0; i < len; i++) {
        if (a[i] != b[i]) {
            return 0;
        }
    }
    return 1;
}

static void mc_copy_bytes(uint8_t *dst, const uint8_t *src, uint64_t len) {
    mc_slice_t src_slice = { .ptr = (uint64_t)src, .len = len };
    mc_slice_t dst_slice = { .ptr = (uint64_t)dst, .len = len };
    __mc_memcpy(&dst_slice, &src_slice);
}

uint64_t __mc_hash_bytes(const uint8_t *data, uint64_t len) {
    // FNV-1a 64-bit
    uint64_t h = 1469598103934665603ull;
    for (uint64_t i = 0; i < len; i++) {
        h ^= (uint64_t)data[i];
        h *= 1099511628211ull;
    }
    return h;
}

uint64_t __mc_hash_table_total_bytes(uint32_t cap, uint64_t slot_size) {
    uint64_t slot_bytes = (uint64_t)cap * slot_size;
    if (cap != 0 && slot_size != 0 && slot_bytes / slot_size != cap) {
        __mc_trap(MC_TRAP_RANGE, slot_bytes, 0, slot_bytes + 1);
    }
    uint64_t total = (uint64_t)cap + slot_bytes;
    if (total < slot_bytes) {
        __mc_trap(MC_TRAP_RANGE, total, 0, total + 1);
    }
    return total;
}

uint64_t __mc_hash_table_slot_offset(uint32_t index, uint64_t slot_size) {
    uint64_t offset = (uint64_t)index * slot_size;
    if (index != 0 && slot_size != 0 && offset / slot_size != index) {
        __mc_trap(MC_TRAP_RANGE, offset, 0, offset + 1);
    }
    return offset;
}

uint8_t *__mc_hash_table_ctrl(uint8_t *base) {
    return base;
}

uint8_t *__mc_hash_table_slots(uint8_t *base, uint32_t cap) {
    return base + cap;
}

uint8_t *__mc_hash_table_slot(uint8_t *slots, uint32_t index, uint64_t slot_size) {
    return slots + __mc_hash_table_slot_offset(index, slot_size);
}

uint32_t __mc_hash_table_probe_start(uint64_t hash, uint32_t cap) {
    return (uint32_t)(hash % (uint64_t)cap);
}

uint32_t __mc_hash_table_find_slot_bytes(
    uint8_t *ctrl,
    uint8_t *slots,
    uint32_t cap,
    const uint8_t *needle,
    uint64_t slot_size,
    uint64_t hash,
    uint8_t *found
) {
    uint32_t start = __mc_hash_table_probe_start(hash, cap);
    uint32_t first_tomb = cap;

    for (uint32_t step = 0; step < cap; step++) {
        uint32_t idx = (start + step) % cap;
        uint8_t state = ctrl[idx];
        if (state == MC_HASH_CTRL_EMPTY) {
            *found = 0;
            return first_tomb != cap ? first_tomb : idx;
        }
        if (state == MC_HASH_CTRL_TOMB) {
            if (first_tomb == cap) {
                first_tomb = idx;
            }
            continue;
        }

        uint8_t *slot = __mc_hash_table_slot(slots, idx, slot_size);
        if (mc_bytes_eq(slot, needle, slot_size)) {
            *found = 1;
            return idx;
        }
    }

    // Full table with only tombstones/full slots.
    *found = 0;
    return first_tomb != cap ? first_tomb : 0;
}

void __mc_hash_table_insert_unique_bytes(
    uint8_t *ctrl,
    uint8_t *slots,
    uint32_t cap,
    const uint8_t *elem,
    uint64_t slot_size
) {
    uint64_t hash = __mc_hash_bytes(elem, slot_size);
    uint32_t start = __mc_hash_table_probe_start(hash, cap);
    for (uint32_t step = 0; step < cap; step++) {
        uint32_t idx = (start + step) % cap;
        if (ctrl[idx] != MC_HASH_CTRL_FULL) {
            ctrl[idx] = MC_HASH_CTRL_FULL;
            mc_copy_bytes(__mc_hash_table_slot(slots, idx, slot_size), elem, slot_size);
            return;
        }
    }
    __mc_trap(MC_TRAP_RANGE, cap, 0, (uint64_t)cap + 1);
}

uint8_t *__mc_hash_table_alloc(uint32_t cap, uint64_t slot_size) {
    uint64_t total = __mc_hash_table_total_bytes(cap, slot_size);
    uint8_t *base = (uint8_t *)__mc_alloc((size_t)total, 1);
    if (!base && total != 0) {
        __mc_trap(MC_TRAP_RANGE, total, 0, total + 1);
    }
    __mc_hash_table_clear_ctrl(__mc_hash_table_ctrl(base), cap);
    return base;
}

void __mc_hash_table_clear_ctrl(uint8_t *ctrl, uint32_t cap) {
    for (uint32_t i = 0; i < cap; i++) {
        ctrl[i] = MC_HASH_CTRL_EMPTY;
    }
}

uint32_t __mc_hash_table_next_cap(uint32_t cap, uint32_t initial_cap) {
    if (cap == 0) {
        return initial_cap;
    }
    uint32_t next = cap << 1;
    if (next <= cap || next > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, next, 0, (uint64_t)MC_CAP_MASK + 1);
    }
    return next;
}

void __mc_hash_table_rehash_bytes(mc_dyn_array_t *table, uint32_t new_cap, uint64_t slot_size) {
    uint8_t *old_base = (uint8_t *)table->ptr;
    uint32_t old_cap = mc_cap_value(table->cap);
    uint8_t *new_base = __mc_hash_table_alloc(new_cap, slot_size);
    uint8_t *new_ctrl = __mc_hash_table_ctrl(new_base);
    uint8_t *new_slots = __mc_hash_table_slots(new_base, new_cap);

    if (old_base) {
        uint8_t *old_ctrl = __mc_hash_table_ctrl(old_base);
        uint8_t *old_slots = __mc_hash_table_slots(old_base, old_cap);
        for (uint32_t i = 0; i < old_cap; i++) {
            if (old_ctrl[i] != MC_HASH_CTRL_FULL) {
                continue;
            }
            uint8_t *elem = __mc_hash_table_slot(old_slots, i, slot_size);
            __mc_hash_table_insert_unique_bytes(new_ctrl, new_slots, new_cap, elem, slot_size);
        }
        __mc_free(old_base);
    }

    table->ptr = (uint64_t)new_base;
    table->cap = mc_cap_with_owned(new_cap);
}

void __mc_hash_table_ensure_for_insert_bytes(
    mc_dyn_array_t *table,
    uint64_t slot_size,
    uint32_t initial_cap,
    uint32_t max_load_num,
    uint32_t max_load_den
) {
    uint32_t cap = mc_cap_value(table->cap);
    uint32_t len = table->len;
    if (cap == 0) {
        __mc_hash_table_rehash_bytes(table, initial_cap, slot_size);
        return;
    }

    uint64_t lhs = (uint64_t)(len + 1) * max_load_den;
    uint64_t rhs = (uint64_t)cap * max_load_num;
    if (lhs > rhs) {
        __mc_hash_table_rehash_bytes(
            table,
            __mc_hash_table_next_cap(cap, initial_cap),
            slot_size
        );
    }
}
