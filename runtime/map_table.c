#include <stdint.h>

#include "hash_table.h"
#include "map_table.h"

void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2);
void __mc_memcpy(mc_slice_t *dst, const mc_slice_t *src);
void __mc_free(void *ptr);
uint8_t __mc_string_eq(const mc_string_t *lhs, const mc_string_t *rhs);
void __mc_string_retain(mc_string_t *s);
void __mc_string_drop(mc_string_t *s);
uint64_t __mc_hash_bytes(const uint8_t *data, uint64_t len);

// CheckKind::Range = 2
#define MC_TRAP_RANGE 2

#define MC_MAP_INITIAL_CAP 8u
#define MC_MAP_MAX_LOAD_NUM 7u
#define MC_MAP_MAX_LOAD_DEN 10u

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

static uint64_t mc_hash_string_key(const mc_string_t *s) {
    return __mc_hash_bytes((const uint8_t *)(uintptr_t)s->ptr, (uint64_t)s->len);
}

static uint32_t mc_find_slot_by_string_key(
    uint8_t *ctrl,
    uint8_t *slots,
    uint32_t cap,
    const mc_string_t *needle_key,
    uint64_t entry_size,
    uint64_t key_hash,
    uint8_t *found
) {
    uint32_t start = __mc_hash_table_probe_start(key_hash, cap);
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

        mc_string_t *entry_key = (mc_string_t *)__mc_hash_table_slot(slots, idx, entry_size);
        if (__mc_string_eq(entry_key, needle_key)) {
            *found = 1;
            return idx;
        }
    }

    *found = 0;
    return first_tomb != cap ? first_tomb : 0;
}

static uint64_t mc_entry_size(uint64_t key_size, uint64_t value_size) {
    uint64_t total = key_size + value_size;
    if (total < key_size) {
        __mc_trap(MC_TRAP_RANGE, total, 0, total + 1);
    }
    return total;
}

static uint32_t mc_find_slot_by_key(
    uint8_t *ctrl,
    uint8_t *slots,
    uint32_t cap,
    const uint8_t *needle_key,
    uint64_t key_size,
    uint64_t entry_size,
    uint64_t key_hash,
    uint8_t *found
) {
    uint32_t start = __mc_hash_table_probe_start(key_hash, cap);
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

        uint8_t *entry = __mc_hash_table_slot(slots, idx, entry_size);
        if (mc_bytes_eq(entry, needle_key, key_size)) {
            *found = 1;
            return idx;
        }
    }

    *found = 0;
    return first_tomb != cap ? first_tomb : 0;
}

static void mc_insert_unique_entry(
    uint8_t *ctrl,
    uint8_t *slots,
    uint32_t cap,
    const uint8_t *entry,
    uint64_t key_size,
    uint64_t entry_size
) {
    uint64_t key_hash = __mc_hash_bytes(entry, key_size);
    uint32_t start = __mc_hash_table_probe_start(key_hash, cap);
    for (uint32_t step = 0; step < cap; step++) {
        uint32_t idx = (start + step) % cap;
        if (ctrl[idx] != MC_HASH_CTRL_FULL) {
            ctrl[idx] = MC_HASH_CTRL_FULL;
            mc_copy_bytes(__mc_hash_table_slot(slots, idx, entry_size), entry, entry_size);
            return;
        }
    }
    __mc_trap(MC_TRAP_RANGE, cap, 0, (uint64_t)cap + 1);
}

static void mc_rehash_map(mc_dyn_array_t *map, uint32_t new_cap, uint64_t key_size, uint64_t value_size) {
    uint64_t entry_size = mc_entry_size(key_size, value_size);
    uint8_t *old_base = (uint8_t *)map->ptr;
    uint32_t old_cap = mc_cap_value(map->cap);
    uint8_t *new_base = __mc_hash_table_alloc(new_cap, entry_size);
    uint8_t *new_ctrl = __mc_hash_table_ctrl(new_base);
    uint8_t *new_slots = __mc_hash_table_slots(new_base, new_cap);

    if (old_base) {
        uint8_t *old_ctrl = __mc_hash_table_ctrl(old_base);
        uint8_t *old_slots = __mc_hash_table_slots(old_base, old_cap);
        for (uint32_t i = 0; i < old_cap; i++) {
            if (old_ctrl[i] != MC_HASH_CTRL_FULL) {
                continue;
            }
            uint8_t *entry = __mc_hash_table_slot(old_slots, i, entry_size);
            mc_insert_unique_entry(new_ctrl, new_slots, new_cap, entry, key_size, entry_size);
        }
        __mc_free(old_base);
    }

    map->ptr = (uint64_t)new_base;
    map->cap = mc_cap_with_owned(new_cap);
}

static void mc_ensure_for_insert(mc_dyn_array_t *map, uint64_t key_size, uint64_t value_size) {
    uint32_t cap = mc_cap_value(map->cap);
    uint32_t len = map->len;
    if (cap == 0) {
        mc_rehash_map(map, MC_MAP_INITIAL_CAP, key_size, value_size);
        return;
    }

    uint64_t lhs = (uint64_t)(len + 1) * MC_MAP_MAX_LOAD_DEN;
    uint64_t rhs = (uint64_t)cap * MC_MAP_MAX_LOAD_NUM;
    if (lhs > rhs) {
        mc_rehash_map(
            map,
            __mc_hash_table_next_cap(cap, MC_MAP_INITIAL_CAP),
            key_size,
            value_size
        );
    }
}

static uint32_t mc_find_next_live_slot(const mc_dyn_array_t *map, uint32_t cursor) {
    uint32_t cap = mc_cap_value(map->cap);
    if (cap == 0 || map->ptr == 0) {
        return 0;
    }

    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    for (uint32_t i = cursor; i < cap; i++) {
        if (ctrl[i] == MC_HASH_CTRL_FULL) {
            return i;
        }
    }
    return cap;
}

uint8_t __mc_map_table_contains_key_bytes(
    const mc_dyn_array_t *map,
    const uint8_t *key,
    uint64_t key_size,
    uint64_t value_size
) {
    uint32_t cap = mc_cap_value(map->cap);
    if (cap == 0 || map->ptr == 0) {
        return 0;
    }

    uint64_t entry_size = mc_entry_size(key_size, value_size);
    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint64_t key_hash = __mc_hash_bytes(key, key_size);
    uint8_t found = 0;
    (void)mc_find_slot_by_key(ctrl, slots, cap, key, key_size, entry_size, key_hash, &found);
    return found;
}

uint8_t __mc_map_table_insert_or_assign_bytes(
    mc_dyn_array_t *map,
    const uint8_t *key,
    const uint8_t *value,
    uint64_t key_size,
    uint64_t value_size
) {
    uint64_t entry_size = mc_entry_size(key_size, value_size);
    mc_ensure_for_insert(map, key_size, value_size);

    uint32_t cap = mc_cap_value(map->cap);
    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint64_t key_hash = __mc_hash_bytes(key, key_size);
    uint8_t found = 0;
    uint32_t slot = mc_find_slot_by_key(ctrl, slots, cap, key, key_size, entry_size, key_hash, &found);
    uint8_t *entry = __mc_hash_table_slot(slots, slot, entry_size);
    if (found) {
        mc_copy_bytes(entry + key_size, value, value_size);
        return 0;
    }

    ctrl[slot] = MC_HASH_CTRL_FULL;
    mc_copy_bytes(entry, key, key_size);
    mc_copy_bytes(entry + key_size, value, value_size);
    map->len += 1;
    return 1;
}

uint8_t __mc_map_table_remove_key_bytes(
    mc_dyn_array_t *map,
    const uint8_t *key,
    uint64_t key_size,
    uint64_t value_size
) {
    uint32_t cap = mc_cap_value(map->cap);
    if (cap == 0 || map->ptr == 0 || map->len == 0) {
        return 0;
    }

    uint64_t entry_size = mc_entry_size(key_size, value_size);
    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint64_t key_hash = __mc_hash_bytes(key, key_size);
    uint8_t found = 0;
    uint32_t slot = mc_find_slot_by_key(ctrl, slots, cap, key, key_size, entry_size, key_hash, &found);
    if (!found) {
        return 0;
    }

    ctrl[slot] = MC_HASH_CTRL_TOMB;
    map->len -= 1;
    return 1;
}

uint8_t __mc_map_table_get_value_bytes(
    const mc_dyn_array_t *map,
    const uint8_t *key,
    uint64_t key_size,
    uint64_t value_size,
    uint8_t *out_value
) {
    uint32_t cap = mc_cap_value(map->cap);
    if (cap == 0 || map->ptr == 0) {
        return 0;
    }

    uint64_t entry_size = mc_entry_size(key_size, value_size);
    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint64_t key_hash = __mc_hash_bytes(key, key_size);
    uint8_t found = 0;
    uint32_t slot = mc_find_slot_by_key(ctrl, slots, cap, key, key_size, entry_size, key_hash, &found);
    if (!found) {
        return 0;
    }

    uint8_t *entry = __mc_hash_table_slot(slots, slot, entry_size);
    mc_copy_bytes(out_value, entry + key_size, value_size);
    return 1;
}

void __mc_map_table_clear(mc_dyn_array_t *map) {
    uint32_t cap = mc_cap_value(map->cap);
    if (cap != 0 && map->ptr != 0) {
        __mc_hash_table_clear_ctrl(__mc_hash_table_ctrl((uint8_t *)map->ptr), cap);
    }
    map->len = 0;
}

void __mc_map_table_drop(mc_dyn_array_t *map) {
    if (map->ptr != 0 && mc_cap_is_owned(map->cap)) {
        __mc_free((void *)map->ptr);
    }
    map->ptr = 0;
    map->len = 0;
    map->cap = 0;
}

uint32_t __mc_map_table_iter_init(const mc_dyn_array_t *map) {
    return mc_find_next_live_slot(map, 0);
}

uint8_t __mc_map_table_iter_is_done(const mc_dyn_array_t *map, uint32_t cursor) {
    return cursor >= mc_cap_value(map->cap);
}

void __mc_map_table_iter_load_bytes(
    const mc_dyn_array_t *map,
    uint32_t cursor,
    uint64_t key_size,
    uint64_t value_size,
    uint8_t *out_key,
    uint8_t *out_value
) {
    uint32_t cap = mc_cap_value(map->cap);
    if (cursor >= cap || map->ptr == 0) {
        __mc_trap(MC_TRAP_RANGE, cursor, 0, cap);
    }

    uint64_t entry_size = mc_entry_size(key_size, value_size);
    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    if (ctrl[cursor] != MC_HASH_CTRL_FULL) {
        __mc_trap(MC_TRAP_RANGE, cursor, 0, cap);
    }

    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint8_t *entry = __mc_hash_table_slot(slots, cursor, entry_size);
    mc_copy_bytes(out_key, entry, key_size);
    mc_copy_bytes(out_value, entry + key_size, value_size);
}

uint32_t __mc_map_table_iter_advance(const mc_dyn_array_t *map, uint32_t cursor) {
    uint32_t cap = mc_cap_value(map->cap);
    if (cursor >= cap) {
        return cap;
    }
    return mc_find_next_live_slot(map, cursor + 1);
}

uint8_t __rt_map_contains_string_key(uint64_t map_ptr, uint64_t key_ptr, uint64_t value_size) {
    const mc_dyn_array_t *map = (const mc_dyn_array_t *)map_ptr;
    const mc_string_t *key = (const mc_string_t *)key_ptr;
    uint32_t cap = mc_cap_value(map->cap);
    if (cap == 0 || map->ptr == 0) {
        return 0;
    }

    uint64_t entry_size = mc_entry_size(sizeof(mc_string_t), value_size);
    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint64_t key_hash = mc_hash_string_key(key);
    uint8_t found = 0;
    (void)mc_find_slot_by_string_key(ctrl, slots, cap, key, entry_size, key_hash, &found);
    return found;
}

uint8_t __rt_map_insert_or_assign_string_key(
    uint64_t map_ptr,
    uint64_t key_ptr,
    uint64_t value_ptr,
    uint64_t value_size
) {
    mc_dyn_array_t *map = (mc_dyn_array_t *)map_ptr;
    const mc_string_t *key = (const mc_string_t *)key_ptr;
    const uint8_t *value = (const uint8_t *)value_ptr;

    uint64_t key_size = sizeof(mc_string_t);
    uint64_t entry_size = mc_entry_size(key_size, value_size);
    mc_ensure_for_insert(map, key_size, value_size);

    uint32_t cap = mc_cap_value(map->cap);
    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint64_t key_hash = mc_hash_string_key(key);
    uint8_t found = 0;
    uint32_t slot = mc_find_slot_by_string_key(ctrl, slots, cap, key, entry_size, key_hash, &found);
    uint8_t *entry = __mc_hash_table_slot(slots, slot, entry_size);
    if (found) {
        mc_copy_bytes(entry + key_size, value, value_size);
        return 0;
    }

    ctrl[slot] = MC_HASH_CTRL_FULL;
    mc_copy_bytes(entry, (const uint8_t *)key, key_size);
    __mc_string_retain((mc_string_t *)entry);
    mc_copy_bytes(entry + key_size, value, value_size);
    map->len += 1;
    return 1;
}

uint8_t __rt_map_remove_string_key(uint64_t map_ptr, uint64_t key_ptr, uint64_t value_size) {
    mc_dyn_array_t *map = (mc_dyn_array_t *)map_ptr;
    const mc_string_t *key = (const mc_string_t *)key_ptr;
    uint32_t cap = mc_cap_value(map->cap);
    if (cap == 0 || map->ptr == 0 || map->len == 0) {
        return 0;
    }

    uint64_t entry_size = mc_entry_size(sizeof(mc_string_t), value_size);
    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint64_t key_hash = mc_hash_string_key(key);
    uint8_t found = 0;
    uint32_t slot = mc_find_slot_by_string_key(ctrl, slots, cap, key, entry_size, key_hash, &found);
    if (!found) {
        return 0;
    }

    uint8_t *entry = __mc_hash_table_slot(slots, slot, entry_size);
    __mc_string_drop((mc_string_t *)entry);
    ctrl[slot] = MC_HASH_CTRL_TOMB;
    map->len -= 1;
    return 1;
}

uint8_t __rt_map_get_value_string_key(
    uint64_t map_ptr,
    uint64_t key_ptr,
    uint64_t value_size,
    uint64_t out_value_ptr
) {
    const mc_dyn_array_t *map = (const mc_dyn_array_t *)map_ptr;
    const mc_string_t *key = (const mc_string_t *)key_ptr;
    uint8_t *out_value = (uint8_t *)out_value_ptr;
    uint32_t cap = mc_cap_value(map->cap);
    if (cap == 0 || map->ptr == 0) {
        return 0;
    }

    uint64_t entry_size = mc_entry_size(sizeof(mc_string_t), value_size);
    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint64_t key_hash = mc_hash_string_key(key);
    uint8_t found = 0;
    uint32_t slot = mc_find_slot_by_string_key(ctrl, slots, cap, key, entry_size, key_hash, &found);
    if (!found) {
        return 0;
    }

    uint8_t *entry = __mc_hash_table_slot(slots, slot, entry_size);
    mc_copy_bytes(out_value, entry + sizeof(mc_string_t), value_size);
    return 1;
}

void __rt_map_clear_string_keys(uint64_t map_ptr, uint64_t value_size) {
    mc_dyn_array_t *map = (mc_dyn_array_t *)map_ptr;
    uint32_t cap = mc_cap_value(map->cap);
    if (cap == 0 || map->ptr == 0) {
        map->len = 0;
        return;
    }

    uint64_t entry_size = mc_entry_size(sizeof(mc_string_t), value_size);
    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    uint8_t *slots = __mc_hash_table_slots(base, cap);
    for (uint32_t i = 0; i < cap; i++) {
        if (ctrl[i] != MC_HASH_CTRL_FULL) {
            continue;
        }
        mc_string_t *entry_key = (mc_string_t *)__mc_hash_table_slot(slots, i, entry_size);
        __mc_string_drop(entry_key);
    }
    __mc_hash_table_clear_ctrl(ctrl, cap);
    map->len = 0;
}

void __rt_map_drop_string_keys(uint64_t map_ptr, uint64_t value_size) {
    mc_dyn_array_t *map = (mc_dyn_array_t *)map_ptr;
    uint32_t cap = mc_cap_value(map->cap);
    if (cap != 0 && map->ptr != 0 && mc_cap_is_owned(map->cap)) {
        uint64_t entry_size = mc_entry_size(sizeof(mc_string_t), value_size);
        uint8_t *base = (uint8_t *)map->ptr;
        uint8_t *ctrl = __mc_hash_table_ctrl(base);
        uint8_t *slots = __mc_hash_table_slots(base, cap);
        for (uint32_t i = 0; i < cap; i++) {
            if (ctrl[i] != MC_HASH_CTRL_FULL) {
                continue;
            }
            mc_string_t *entry_key = (mc_string_t *)__mc_hash_table_slot(slots, i, entry_size);
            __mc_string_drop(entry_key);
        }
        __mc_free((void *)map->ptr);
    }
    map->ptr = 0;
    map->len = 0;
    map->cap = 0;
}

uint32_t __rt_map_iter_init(uint64_t map_ptr) {
    const mc_dyn_array_t *map = (const mc_dyn_array_t *)map_ptr;
    return __mc_map_table_iter_init(map);
}

uint8_t __rt_map_iter_is_done(uint64_t map_ptr, uint32_t cursor) {
    const mc_dyn_array_t *map = (const mc_dyn_array_t *)map_ptr;
    return __mc_map_table_iter_is_done(map, cursor);
}

void __rt_map_iter_load_string_key(
    uint64_t map_ptr,
    uint32_t cursor,
    uint64_t value_size,
    uint64_t out_key_ptr,
    uint64_t out_value_ptr
) {
    const mc_dyn_array_t *map = (const mc_dyn_array_t *)map_ptr;
    mc_string_t *out_key = (mc_string_t *)out_key_ptr;
    uint8_t *out_value = (uint8_t *)out_value_ptr;
    uint32_t cap = mc_cap_value(map->cap);
    if (cursor >= cap || map->ptr == 0) {
        __mc_trap(MC_TRAP_RANGE, cursor, 0, cap);
    }

    uint64_t entry_size = mc_entry_size(sizeof(mc_string_t), value_size);
    uint8_t *base = (uint8_t *)map->ptr;
    uint8_t *ctrl = __mc_hash_table_ctrl(base);
    if (ctrl[cursor] != MC_HASH_CTRL_FULL) {
        __mc_trap(MC_TRAP_RANGE, cursor, 0, cap);
    }

    uint8_t *slots = __mc_hash_table_slots(base, cap);
    uint8_t *entry = __mc_hash_table_slot(slots, cursor, entry_size);
    mc_copy_bytes((uint8_t *)out_key, entry, sizeof(mc_string_t));
    __mc_string_retain(out_key);
    mc_copy_bytes(out_value, entry + sizeof(mc_string_t), value_size);
}

uint32_t __rt_map_iter_advance(uint64_t map_ptr, uint32_t cursor) {
    const mc_dyn_array_t *map = (const mc_dyn_array_t *)map_ptr;
    return __mc_map_table_iter_advance(map, cursor);
}
