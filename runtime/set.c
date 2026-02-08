#include <stdint.h>

#include "types.h"

void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2);
void __mc_memcpy(mc_slice_t *dst, const mc_slice_t *src);
void __mc_dyn_array_ensure(
    mc_dyn_array_t *v,
    uint32_t min_cap,
    uint64_t elem_size,
    uint64_t elem_align
);

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

uint8_t __rt_set_contains_elem(uint64_t set_ptr, uint64_t elem_ptr, uint64_t elem_size) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    uint32_t len = set->len;

    if (elem_size == 0) {
        return len > 0;
    }

    const uint8_t *needle = (const uint8_t *)elem_ptr;
    uint8_t *base = (uint8_t *)set->ptr;
    for (uint32_t i = 0; i < len; i++) {
        uint8_t *slot = base + ((uint64_t)i * elem_size);
        if (mc_bytes_eq(slot, needle, elem_size)) {
            return 1;
        }
    }
    return 0;
}

uint8_t __rt_set_insert_elem(
    uint64_t set_ptr,
    uint64_t elem_ptr,
    uint64_t elem_size,
    uint64_t elem_align
) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;

    if (__rt_set_contains_elem(set_ptr, elem_ptr, elem_size)) {
        return 0;
    }

    uint32_t len = set->len;
    if (len > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, len, 0, (uint64_t)MC_CAP_MASK + 1);
    }

    uint64_t new_len = (uint64_t)len + 1;
    if (new_len > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, new_len, 0, (uint64_t)MC_CAP_MASK + 1);
    }

    __mc_dyn_array_ensure(set, (uint32_t)new_len, elem_size, elem_align);

    if (elem_size != 0) {
        uint64_t offset = (uint64_t)len * elem_size;
        if (len != 0 && offset / elem_size != len) {
            __mc_trap(MC_TRAP_RANGE, offset, 0, offset + 1);
        }
        mc_slice_t src = {
            .ptr = elem_ptr,
            .len = elem_size,
        };
        mc_slice_t dst = {
            .ptr = set->ptr + offset,
            .len = elem_size,
        };
        __mc_memcpy(&dst, &src);
    }

    set->len = (uint32_t)new_len;
    return 1;
}

uint8_t __rt_set_remove_elem(uint64_t set_ptr, uint64_t elem_ptr, uint64_t elem_size) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    uint32_t len = set->len;
    if (len == 0) {
        return 0;
    }

    if (elem_size == 0) {
        set->len = 0;
        return 1;
    }

    const uint8_t *needle = (const uint8_t *)elem_ptr;
    uint8_t *base = (uint8_t *)set->ptr;
    uint32_t found = MC_CAP_MASK;

    for (uint32_t i = 0; i < len; i++) {
        uint8_t *slot = base + ((uint64_t)i * elem_size);
        if (mc_bytes_eq(slot, needle, elem_size)) {
            found = i;
            break;
        }
    }

    if (found == MC_CAP_MASK) {
        return 0;
    }

    uint32_t last = len - 1;
    if (found != last) {
        uint8_t *dst_slot = base + ((uint64_t)found * elem_size);
        uint8_t *src_slot = base + ((uint64_t)last * elem_size);
        mc_slice_t src = {
            .ptr = (uint64_t)src_slot,
            .len = elem_size,
        };
        mc_slice_t dst = {
            .ptr = (uint64_t)dst_slot,
            .len = elem_size,
        };
        __mc_memcpy(&dst, &src);
    }

    set->len = last;
    return 1;
}

void __rt_set_clear(uint64_t set_ptr) {
    mc_dyn_array_t *set = (mc_dyn_array_t *)set_ptr;
    set->len = 0;
}
