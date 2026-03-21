#include <stddef.h>
#include <stdint.h>

#include "types.h"

void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2);
void __mc_memcpy(mc_slice_t *dst, const mc_slice_t *src);
void *__mc_alloc(size_t size, size_t align);
void *__mc_realloc(void *ptr, size_t size, size_t align);
void __mc_free(void *ptr);

// CheckKind::Range = 2
#define MC_TRAP_RANGE 2

typedef struct mc_dyn_array_backing {
    uint32_t refcount;
    uint32_t cap;
    uint8_t bytes[];
} mc_dyn_array_backing_t;

static mc_dyn_array_backing_t *mc_dyn_array_backing(mc_dyn_array_t *v) {
    if (!mc_cap_is_owned(v->cap) || v->ptr == 0) {
        return NULL;
    }
    return (mc_dyn_array_backing_t *)((uint8_t *)(uintptr_t)v->ptr
                                      - offsetof(mc_dyn_array_backing_t, bytes));
}

static mc_dyn_array_backing_t *mc_dyn_array_backing_alloc(
    uint32_t cap,
    uint64_t elem_size,
    uint64_t elem_align
) {
    if (elem_align == 0) {
        elem_align = 1;
    }

    uint64_t bytes = (uint64_t)cap * elem_size;
    if (cap != 0 && elem_size != 0 && bytes / elem_size != cap) {
        __mc_trap(MC_TRAP_RANGE, bytes, 0, bytes + 1);
    }

    size_t total = offsetof(mc_dyn_array_backing_t, bytes) + (size_t)bytes;
    size_t align = elem_align;
    if (align < _Alignof(mc_dyn_array_backing_t)) {
        align = _Alignof(mc_dyn_array_backing_t);
    }
    mc_dyn_array_backing_t *backing = (mc_dyn_array_backing_t *)__mc_alloc(total, align);
    if (!backing && total != 0) {
        __mc_trap(MC_TRAP_RANGE, (uint64_t)total, 0, (uint64_t)total + 1);
    }
    if (!backing) {
        return NULL;
    }
    backing->refcount = 1;
    backing->cap = cap;
    return backing;
}

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
    mc_dyn_array_backing_t *backing = mc_dyn_array_backing(v);
    if (owned && backing && backing->refcount == 1 && min_cap <= cap) {
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

    uint64_t bytes = (uint64_t)new_cap * elem_size;
    if (new_cap != 0 && bytes / elem_size != new_cap) {
        __mc_trap(MC_TRAP_RANGE, bytes, 0, bytes + 1);
    }

    if (owned && backing && backing->refcount == 1) {
        size_t total = offsetof(mc_dyn_array_backing_t, bytes) + (size_t)bytes;
        size_t align = elem_align ? (size_t)elem_align : 1;
        if (align < _Alignof(mc_dyn_array_backing_t)) {
            align = _Alignof(mc_dyn_array_backing_t);
        }
        mc_dyn_array_backing_t *new_backing =
            (mc_dyn_array_backing_t *)__mc_realloc(backing, total, align);
        if (!new_backing && total != 0) {
            __mc_trap(MC_TRAP_RANGE, bytes, 0, bytes + 1);
        }
        v->ptr = (uint64_t)new_backing->bytes;
        v->cap = mc_cap_with_owned(new_cap);
        return;
    }

    mc_dyn_array_backing_t *new_backing = mc_dyn_array_backing_alloc(new_cap, elem_size, elem_align);
    if (v->ptr && len > 0) {
        uint64_t copy_bytes = (uint64_t)len * elem_size;
        mc_slice_t src = {
            .ptr = v->ptr,
            .len = copy_bytes,
        };
        mc_slice_t dst = {
            .ptr = (uint64_t)new_backing->bytes,
            .len = copy_bytes,
        };
        __mc_memcpy(&dst, &src);
    }

    if (owned && backing) {
        if (backing->refcount > 1) {
            backing->refcount -= 1;
        } else {
            __mc_free(backing);
        }
    }

    v->ptr = (uint64_t)new_backing->bytes;
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

void __mc_dyn_array_retain(mc_dyn_array_t *v) {
    mc_dyn_array_backing_t *backing = mc_dyn_array_backing(v);
    if (!backing) {
        return;
    }
    if (backing->refcount == UINT32_MAX) {
        __mc_trap(MC_TRAP_RANGE, backing->refcount, 0, (uint64_t)UINT32_MAX + 1);
    }
    backing->refcount += 1;
}

uint8_t __mc_dyn_array_release(mc_dyn_array_t *v) {
    mc_dyn_array_backing_t *backing = mc_dyn_array_backing(v);
    if (!backing) {
        return 0;
    }
    if (backing->refcount > 1) {
        backing->refcount -= 1;
        v->ptr = 0;
        v->len = 0;
        v->cap = 0;
        return 0;
    }
    return 1;
}

void __mc_dyn_array_free_backing(mc_dyn_array_t *v) {
    mc_dyn_array_backing_t *backing = mc_dyn_array_backing(v);
    if (backing) {
        __mc_free(backing);
    }
    v->ptr = 0;
    v->len = 0;
    v->cap = 0;
}

void __rt_dyn_array_retain(uint64_t v_ptr) {
    __mc_dyn_array_retain((mc_dyn_array_t *)v_ptr);
}

uint8_t __rt_dyn_array_release(uint64_t v_ptr) {
    return __mc_dyn_array_release((mc_dyn_array_t *)v_ptr);
}

void __rt_dyn_array_free_backing(uint64_t v_ptr) {
    __mc_dyn_array_free_backing((mc_dyn_array_t *)v_ptr);
}
