#include <stddef.h>
#include <stdint.h>

#include "types.h"

void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2);
void __mc_memcpy(mc_slice_t *dst, const mc_slice_t *src);
void *__mc_alloc(size_t size, size_t align);
void *__mc_realloc(void *ptr, size_t size, size_t align);

// CheckKind::Range = 2
#define MC_TRAP_RANGE 2

static uint32_t mc_next_cap(uint32_t current, uint32_t min_cap) {
    uint32_t cap = current ? current : 1;
    while (cap < min_cap) {
        uint32_t doubled = cap << 1;
        if (doubled <= cap || doubled > MC_CAP_MASK) {
            cap = min_cap;
            break;
        }
        cap = doubled;
    }
    return cap;
}

void __mc_dyn_array_ensure(
    mc_dyn_array_t *v,
    uint32_t min_cap,
    uint64_t elem_size,
    uint64_t elem_align
) {
    uint32_t len = v->len;
    if (len > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, len, 0, (uint64_t)MC_CAP_MASK + 1);
    }
    if (min_cap < len) {
        min_cap = len;
    }
    if (min_cap > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, min_cap, 0, (uint64_t)MC_CAP_MASK + 1);
    }

    uint32_t cap = mc_cap_value(v->cap);
    uint8_t owned = mc_cap_is_owned(v->cap);
    if (owned && min_cap <= cap) {
        return;
    }

    if (!owned && min_cap == 0) {
        v->cap = mc_cap_with_owned(0);
        return;
    }

    uint32_t new_cap = cap;
    if (new_cap < min_cap) {
        new_cap = mc_next_cap(cap, min_cap);
    }

    if (elem_size == 0) {
        v->ptr = 0;
        v->cap = mc_cap_with_owned(new_cap);
        return;
    }

    if (elem_align == 0) {
        elem_align = 1;
    }

    uint64_t bytes = (uint64_t)new_cap * elem_size;
    if (new_cap != 0 && bytes / elem_size != new_cap) {
        __mc_trap(MC_TRAP_RANGE, bytes, 0, bytes + 1);
    }

    void *new_ptr = owned
        ? __mc_realloc((void *)v->ptr, bytes, (size_t)elem_align)
        : __mc_alloc(bytes, (size_t)elem_align);
    if (!new_ptr && bytes != 0) {
        __mc_trap(MC_TRAP_RANGE, bytes, 0, bytes + 1);
    }

    if (!owned && v->ptr && len > 0) {
        uint64_t copy_bytes = (uint64_t)len * elem_size;
        mc_slice_t src = {
            .ptr = v->ptr,
            .len = copy_bytes,
        };
        mc_slice_t dst = {
            .ptr = (uint64_t)new_ptr,
            .len = copy_bytes,
        };
        __mc_memcpy(&dst, &src);
    }

    v->ptr = (uint64_t)new_ptr;
    v->cap = mc_cap_with_owned(new_cap);
}

/**
 * Appends a single element to the dynamic array, promoting it to owned storage
 * if needed.
 */
void __mc_dyn_array_append_elem(
    mc_dyn_array_t *v,
    const void *elem,
    uint64_t elem_size,
    uint64_t elem_align
) {
    uint32_t len = v->len;
    if (len > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, len, 0, (uint64_t)MC_CAP_MASK + 1);
    }

    uint64_t new_len = (uint64_t)len + 1;
    if (new_len > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, new_len, 0, (uint64_t)MC_CAP_MASK + 1);
    }

    __mc_dyn_array_ensure(v, (uint32_t)new_len, elem_size, elem_align);

    if (elem_size == 0) {
        v->len = (uint32_t)new_len;
        return;
    }

    uint64_t offset = (uint64_t)len * elem_size;
    if (len != 0 && offset / elem_size != len) {
        __mc_trap(MC_TRAP_RANGE, offset, 0, offset + 1);
    }

    mc_slice_t src = {
        .ptr = (uint64_t)elem,
        .len = elem_size,
    };
    mc_slice_t dst = {
        .ptr = v->ptr + offset,
        .len = elem_size,
    };
    __mc_memcpy(&dst, &src);
    v->len = (uint32_t)new_len;
}

void __rt_dyn_array_ensure(
    uint64_t v_ptr,
    uint32_t min_cap,
    uint64_t elem_size,
    uint64_t elem_align
) {
    __mc_dyn_array_ensure((mc_dyn_array_t *)v_ptr, min_cap, elem_size, elem_align);
}

void __rt_dyn_array_append_elem(
    uint64_t v_ptr,
    uint64_t elem_ptr,
    uint64_t elem_size,
    uint64_t elem_align
) {
    __mc_dyn_array_append_elem(
        (mc_dyn_array_t *)v_ptr,
        (const void *)elem_ptr,
        elem_size,
        elem_align
    );
}
