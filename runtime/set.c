#include <stdint.h>

#include "hash_table.h"

void __mc_memcpy(mc_slice_t *dst, const mc_slice_t *src);
uint8_t __mc_string_eq(const mc_string_t *lhs, const mc_string_t *rhs);
void __mc_string_retain(mc_string_t *s);
void __mc_string_drop(mc_string_t *s);
uint64_t __mc_hash_bytes(const uint8_t *data, uint64_t len);
void __mc_free(void *ptr);

#define MC_SET_INITIAL_CAP 8u
#define MC_SET_MAX_LOAD_NUM 7u
#define MC_SET_MAX_LOAD_DEN 10u

static void mc_copy_elem(uint8_t *dst, const uint8_t *src, uint64_t elem_size) {
    mc_slice_t src_slice = { .ptr = (uint64_t)src, .len = elem_size };
    mc_slice_t dst_slice = { .ptr = (uint64_t)dst, .len = elem_size };
    __mc_memcpy(&dst_slice, &src_slice);
}

static uint64_t mc_hash_string(const mc_string_t *s) {
    return __mc_hash_bytes((const uint8_t *)(uintptr_t)s->ptr, (uint64_t)s->len);
}

static uint32_t mc_find_slot_string(
    uint8_t *ctrl,
    uint8_t *slots,
    uint32_t cap,
    const mc_string_t *needle,
    uint8_t *found
) {
    uint64_t hash = mc_hash_string(needle);
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

        mc_string_t *entry = (mc_string_t *)__mc_hash_table_slot(slots, idx, sizeof(mc_string_t));
        if (__mc_string_eq(entry, needle)) {
            *found = 1;
            return idx;
        }
    }

    *found = 0;
    return first_tomb != cap ? first_tomb : 0;
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
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint64_t hash = __mc_hash_bytes(needle, elem_size);

    uint8_t found = 0;
    (void)__mc_hash_table_find_slot_bytes(
        ctrl,
        slots,
        cap,
        needle,
        elem_size,
        hash,
        &found
    );
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

    __mc_hash_table_ensure_for_insert_bytes(
        set,
        elem_size,
        MC_SET_INITIAL_CAP,
        MC_SET_MAX_LOAD_NUM,
        MC_SET_MAX_LOAD_DEN
    );

    uint32_t cap = mc_cap_value(set->cap);
    uint8_t *base = (uint8_t *)set->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    const uint8_t *needle = (const uint8_t *)elem_ptr;
    uint64_t hash = __mc_hash_bytes(needle, elem_size);

    uint8_t found = 0;
    uint32_t slot = __mc_hash_table_find_slot_bytes(
        ctrl,
        slots,
        cap,
        needle,
        elem_size,
        hash,
        &found
    );
    if (found) {
        return 0;
    }

    (void)elem_align; // Alignment is not required for byte-wise probing/copy.
    ctrl[slot] = MC_HASH_CTRL_FULL;
    mc_copy_elem(__mc_hash_table_slot(slots, slot, elem_size), needle, elem_size);
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
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    const uint8_t *needle = (const uint8_t *)elem_ptr;
    uint64_t hash = __mc_hash_bytes(needle, elem_size);
    uint8_t found = 0;
    uint32_t slot = __mc_hash_table_find_slot_bytes(
        ctrl,
        slots,
        cap,
        needle,
        elem_size,
        hash,
        &found
    );
    if (!found) {
        return 0;
    }

    ctrl[slot] = MC_HASH_CTRL_TOMB;
    set->len -= 1;
    return 1;
}

void __rt_set_clear(uint64_t set_ptr) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    uint32_t cap = mc_cap_value(set->cap);
    if (cap != 0 && set->ptr != 0) {
        __mc_hash_table_clear_ctrl(__mc_hash_table_ctrl((uint8_t *)set->ptr), cap);
    }
    set->len = 0;
}

uint8_t __rt_set_contains_string(uint64_t set_ptr, uint64_t elem_ptr) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    uint32_t cap = mc_cap_value(set->cap);
    if (cap == 0 || set->ptr == 0) {
        return 0;
    }

    const mc_string_t *needle = (const mc_string_t *)elem_ptr;
    uint8_t *base = (uint8_t *)set->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint8_t found = 0;
    (void)mc_find_slot_string(ctrl, slots, cap, needle, &found);
    return found;
}

uint8_t __rt_set_insert_string(uint64_t set_ptr, uint64_t elem_ptr) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    __mc_hash_table_ensure_for_insert_bytes(
        set,
        sizeof(mc_string_t),
        MC_SET_INITIAL_CAP,
        MC_SET_MAX_LOAD_NUM,
        MC_SET_MAX_LOAD_DEN
    );

    uint32_t cap = mc_cap_value(set->cap);
    uint8_t *base = (uint8_t *)set->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    const mc_string_t *needle = (const mc_string_t *)elem_ptr;
    uint8_t found = 0;
    uint32_t slot = mc_find_slot_string(ctrl, slots, cap, needle, &found);
    if (found) {
        return 0;
    }

    ctrl[slot] = MC_HASH_CTRL_FULL;
    mc_string_t *entry = (mc_string_t *)__mc_hash_table_slot(slots, slot, sizeof(mc_string_t));
    mc_copy_elem((uint8_t *)entry, (const uint8_t *)needle, sizeof(mc_string_t));
    __mc_string_retain(entry);
    set->len += 1;
    return 1;
}

uint8_t __rt_set_remove_string(uint64_t set_ptr, uint64_t elem_ptr) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    uint32_t cap = mc_cap_value(set->cap);
    if (cap == 0 || set->ptr == 0 || set->len == 0) {
        return 0;
    }

    const mc_string_t *needle = (const mc_string_t *)elem_ptr;
    uint8_t *base = (uint8_t *)set->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint8_t found = 0;
    uint32_t slot = mc_find_slot_string(ctrl, slots, cap, needle, &found);
    if (!found) {
        return 0;
    }

    mc_string_t *entry = (mc_string_t *)__mc_hash_table_slot(slots, slot, sizeof(mc_string_t));
    __mc_string_drop(entry);
    ctrl[slot] = MC_HASH_CTRL_TOMB;
    set->len -= 1;
    return 1;
}

void __rt_set_clear_string(uint64_t set_ptr) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    uint32_t cap = mc_cap_value(set->cap);
    if (cap != 0 && set->ptr != 0) {
        uint8_t *base = (uint8_t *)set->ptr;
        uint8_t *ctrl = __mc_hash_table_ctrl(base);
        uint8_t *slots = __mc_hash_table_slots(base, cap);
        for (uint32_t i = 0; i < cap; i++) {
            if (ctrl[i] != MC_HASH_CTRL_FULL) {
                continue;
            }
            mc_string_t *entry = (mc_string_t *)__mc_hash_table_slot(slots, i, sizeof(mc_string_t));
            __mc_string_drop(entry);
        }
        __mc_hash_table_clear_ctrl(ctrl, cap);
    }
    set->len = 0;
}

void __rt_set_drop_string(uint64_t set_ptr) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    uint32_t cap = mc_cap_value(set->cap);
    if (cap != 0 && set->ptr != 0 && mc_cap_is_owned(set->cap)) {
        uint8_t *base = (uint8_t *)set->ptr;
        uint8_t *ctrl = __mc_hash_table_ctrl(base);
        uint8_t *slots = __mc_hash_table_slots(base, cap);
        for (uint32_t i = 0; i < cap; i++) {
            if (ctrl[i] != MC_HASH_CTRL_FULL) {
                continue;
            }
            mc_string_t *entry = (mc_string_t *)__mc_hash_table_slot(slots, i, sizeof(mc_string_t));
            __mc_string_drop(entry);
        }
        __mc_free(base);
    }
    set->ptr = 0;
    set->len = 0;
    set->cap = 0;
}
