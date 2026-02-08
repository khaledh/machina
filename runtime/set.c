#include <stdint.h>

#include "types.h"

void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2);
void __mc_memcpy(mc_slice_t *dst, const mc_slice_t *src);
void *__mc_alloc(size_t size, size_t align);
void __mc_free(void *ptr);

// CheckKind::Range = 2
#define MC_TRAP_RANGE 2

#define MC_SET_CTRL_EMPTY 0u
#define MC_SET_CTRL_FULL 1u
#define MC_SET_CTRL_TOMB 2u

#define MC_SET_INITIAL_CAP 8u
#define MC_SET_MAX_LOAD_NUM 7u
#define MC_SET_MAX_LOAD_DEN 10u

static uint8_t mc_bytes_eq(const uint8_t *a, const uint8_t *b, uint64_t len) {
    for (uint64_t i = 0; i < len; i++) {
        if (a[i] != b[i]) {
            return 0;
        }
    }
    return 1;
}

static uint64_t mc_hash_bytes(const uint8_t *data, uint64_t len) {
    // FNV-1a 64-bit
    uint64_t h = 1469598103934665603ull;
    for (uint64_t i = 0; i < len; i++) {
        h ^= (uint64_t)data[i];
        h *= 1099511628211ull;
    }
    return h;
}

static uint64_t mc_total_bytes(uint32_t cap, uint64_t elem_size) {
    uint64_t slot_bytes = (uint64_t)cap * elem_size;
    if (cap != 0 && elem_size != 0 && slot_bytes / elem_size != cap) {
        __mc_trap(MC_TRAP_RANGE, slot_bytes, 0, slot_bytes + 1);
    }
    uint64_t total = (uint64_t)cap + slot_bytes;
    if (total < slot_bytes) {
        __mc_trap(MC_TRAP_RANGE, total, 0, total + 1);
    }
    return total;
}

static uint64_t mc_slot_offset(uint32_t index, uint64_t elem_size) {
    uint64_t offset = (uint64_t)index * elem_size;
    if (index != 0 && elem_size != 0 && offset / elem_size != index) {
        __mc_trap(MC_TRAP_RANGE, offset, 0, offset + 1);
    }
    return offset;
}

static uint8_t *mc_set_ctrl(uint8_t *base) {
    return base;
}

static uint8_t *mc_set_slots(uint8_t *base, uint32_t cap) {
    return base + cap;
}

static uint8_t *mc_set_slot(uint8_t *slots, uint32_t index, uint64_t elem_size) {
    return slots + mc_slot_offset(index, elem_size);
}

static void mc_copy_elem(uint8_t *dst, const uint8_t *src, uint64_t elem_size) {
    mc_slice_t src_slice = { .ptr = (uint64_t)src, .len = elem_size };
    mc_slice_t dst_slice = { .ptr = (uint64_t)dst, .len = elem_size };
    __mc_memcpy(&dst_slice, &src_slice);
}

static uint32_t mc_probe_start(uint64_t hash, uint32_t cap) {
    return (uint32_t)(hash % (uint64_t)cap);
}

static uint32_t mc_set_find_slot(
    uint8_t *ctrl,
    uint8_t *slots,
    uint32_t cap,
    const uint8_t *needle,
    uint64_t elem_size,
    uint64_t hash,
    uint8_t *found
) {
    uint32_t start = mc_probe_start(hash, cap);
    uint32_t first_tomb = cap;

    for (uint32_t step = 0; step < cap; step++) {
        uint32_t idx = (start + step) % cap;
        uint8_t state = ctrl[idx];
        if (state == MC_SET_CTRL_EMPTY) {
            *found = 0;
            return first_tomb != cap ? first_tomb : idx;
        }
        if (state == MC_SET_CTRL_TOMB) {
            if (first_tomb == cap) {
                first_tomb = idx;
            }
            continue;
        }

        uint8_t *slot = mc_set_slot(slots, idx, elem_size);
        if (mc_bytes_eq(slot, needle, elem_size)) {
            *found = 1;
            return idx;
        }
    }

    // Full table with only tombstones/full slots.
    *found = 0;
    return first_tomb != cap ? first_tomb : 0;
}

static void mc_set_insert_unique(
    uint8_t *ctrl,
    uint8_t *slots,
    uint32_t cap,
    const uint8_t *elem,
    uint64_t elem_size
) {
    uint64_t hash = mc_hash_bytes(elem, elem_size);
    uint32_t start = mc_probe_start(hash, cap);
    for (uint32_t step = 0; step < cap; step++) {
        uint32_t idx = (start + step) % cap;
        if (ctrl[idx] != MC_SET_CTRL_FULL) {
            ctrl[idx] = MC_SET_CTRL_FULL;
            mc_copy_elem(mc_set_slot(slots, idx, elem_size), elem, elem_size);
            return;
        }
    }
    __mc_trap(MC_TRAP_RANGE, cap, 0, (uint64_t)cap + 1);
}

static uint8_t *mc_set_alloc_table(uint32_t cap, uint64_t elem_size) {
    uint64_t total = mc_total_bytes(cap, elem_size);
    uint8_t *base = (uint8_t *)__mc_alloc((size_t)total, 1);
    if (!base && total != 0) {
        __mc_trap(MC_TRAP_RANGE, total, 0, total + 1);
    }
    uint8_t *ctrl = mc_set_ctrl(base);
    for (uint32_t i = 0; i < cap; i++) {
        ctrl[i] = MC_SET_CTRL_EMPTY;
    }
    return base;
}

static uint32_t mc_next_cap(uint32_t cap) {
    if (cap == 0) {
        return MC_SET_INITIAL_CAP;
    }
    uint32_t next = cap << 1;
    if (next <= cap || next > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, next, 0, (uint64_t)MC_CAP_MASK + 1);
    }
    return next;
}

static void mc_set_rehash(mc_dyn_array_t *set, uint32_t new_cap, uint64_t elem_size) {
    uint8_t *old_base = (uint8_t *)set->ptr;
    uint32_t old_cap = mc_cap_value(set->cap);
    uint8_t *new_base = mc_set_alloc_table(new_cap, elem_size);
    uint8_t *new_ctrl = mc_set_ctrl(new_base);
    uint8_t *new_slots = mc_set_slots(new_base, new_cap);

    if (old_base) {
        uint8_t *old_ctrl = mc_set_ctrl(old_base);
        uint8_t *old_slots = mc_set_slots(old_base, old_cap);
        for (uint32_t i = 0; i < old_cap; i++) {
            if (old_ctrl[i] != MC_SET_CTRL_FULL) {
                continue;
            }
            uint8_t *elem = mc_set_slot(old_slots, i, elem_size);
            mc_set_insert_unique(new_ctrl, new_slots, new_cap, elem, elem_size);
        }
        __mc_free(old_base);
    }

    set->ptr = (uint64_t)new_base;
    set->cap = mc_cap_with_owned(new_cap);
}

static void mc_set_ensure_for_insert(mc_dyn_array_t *set, uint64_t elem_size) {
    uint32_t cap = mc_cap_value(set->cap);
    uint32_t len = set->len;
    if (cap == 0) {
        mc_set_rehash(set, MC_SET_INITIAL_CAP, elem_size);
        return;
    }

    uint64_t lhs = (uint64_t)(len + 1) * MC_SET_MAX_LOAD_DEN;
    uint64_t rhs = (uint64_t)cap * MC_SET_MAX_LOAD_NUM;
    if (lhs > rhs) {
        mc_set_rehash(set, mc_next_cap(cap), elem_size);
    }
}

uint8_t __rt_set_contains_elem(uint64_t set_ptr, uint64_t elem_ptr, uint64_t elem_size) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    if (elem_size == 0) {
        return set->len > 0;
    }

    uint32_t cap = mc_cap_value(set->cap);
    if (cap == 0 || set->ptr == 0) {
        return 0;
    }

    const uint8_t *needle = (const uint8_t *)elem_ptr;
    uint8_t *base = (uint8_t *)set->ptr;
    uint8_t *ctrl = mc_set_ctrl(base);
    uint8_t *slots = mc_set_slots(base, cap);
    uint64_t hash = mc_hash_bytes(needle, elem_size);

    uint8_t found = 0;
    (void)mc_set_find_slot(ctrl, slots, cap, needle, elem_size, hash, &found);
    return found;
}

uint8_t __rt_set_insert_elem(
    uint64_t set_ptr,
    uint64_t elem_ptr,
    uint64_t elem_size,
    uint64_t elem_align
) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;

    if (elem_size == 0) {
        if (set->len != 0) {
            return 0;
        }
        set->len = 1;
        return 1;
    }

    mc_set_ensure_for_insert(set, elem_size);
    uint32_t cap = mc_cap_value(set->cap);
    uint8_t *base = (uint8_t *)set->ptr;
    uint8_t *ctrl = mc_set_ctrl(base);
    uint8_t *slots = mc_set_slots(base, cap);
    const uint8_t *needle = (const uint8_t *)elem_ptr;
    uint64_t hash = mc_hash_bytes(needle, elem_size);

    uint8_t found = 0;
    uint32_t slot = mc_set_find_slot(ctrl, slots, cap, needle, elem_size, hash, &found);
    if (found) {
        return 0;
    }
    (void)elem_align; // Alignment is not required for byte-wise probing/copy.
    ctrl[slot] = MC_SET_CTRL_FULL;
    mc_copy_elem(mc_set_slot(slots, slot, elem_size), needle, elem_size);
    set->len += 1;
    return 1;
}

uint8_t __rt_set_remove_elem(uint64_t set_ptr, uint64_t elem_ptr, uint64_t elem_size) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    if (elem_size == 0) {
        if (set->len == 0) {
            return 0;
        }
        set->len = 0;
        return 1;
    }

    uint32_t cap = mc_cap_value(set->cap);
    if (cap == 0 || set->ptr == 0 || set->len == 0) {
        return 0;
    }

    uint8_t *base = (uint8_t *)set->ptr;
    uint8_t *ctrl = mc_set_ctrl(base);
    uint8_t *slots = mc_set_slots(base, cap);
    const uint8_t *needle = (const uint8_t *)elem_ptr;
    uint64_t hash = mc_hash_bytes(needle, elem_size);
    uint8_t found = 0;
    uint32_t slot = mc_set_find_slot(ctrl, slots, cap, needle, elem_size, hash, &found);
    if (!found) {
        return 0;
    }

    ctrl[slot] = MC_SET_CTRL_TOMB;
    set->len -= 1;
    return 1;
}

void __rt_set_clear(uint64_t set_ptr) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    uint32_t cap = mc_cap_value(set->cap);
    if (cap != 0 && set->ptr != 0) {
        uint8_t *ctrl = mc_set_ctrl((uint8_t *)set->ptr);
        for (uint32_t i = 0; i < cap; i++) {
            ctrl[i] = MC_SET_CTRL_EMPTY;
        }
    }
    set->len = 0;
}
