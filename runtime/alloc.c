#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdalign.h>
#include <stdio.h>

#include "types.h"

void __mc_free(void *ptr);

typedef struct mc_alloc_header_t {
    mc_heap_t *heap;
    size_t size;
    void *raw;
} mc_alloc_header_t;

// Runtime toggle for emitting allocation trace messages.
static int mc_trace_allocs = 0;

void __mc_set_alloc_trace(uint8_t enabled) {
    mc_trace_allocs = enabled != 0;
}

static void mc_trace_log(
    const char *op,
    void *ptr,
    size_t size,
    size_t align,
    mc_heap_t *heap
) {
    if (!mc_trace_allocs) {
        return;
    }
    fprintf(
        stderr,
        "[mc] %s ptr=%p size=%zu align=%zu heap=%p\n",
        op,
        ptr,
        size,
        align,
        (void *)heap
    );
}

// Default heap uses the C runtime allocator for now.
static void *mc_sys_alloc(size_t size) {
    return malloc(size);
}

static void *mc_sys_realloc(void *ptr, size_t size) {
    return realloc(ptr, size);
}

static void mc_sys_free(void *ptr) {
    free(ptr);
}

// Global default heap used by v1 (single-heap model).
static mc_heap_vtable_t MC_SYS_VTABLE = {
    .alloc = mc_sys_alloc,
    .realloc = mc_sys_realloc,
    .free = mc_sys_free,
};

static mc_heap_t MC_DEFAULT_HEAP = {
    .vtable = &MC_SYS_VTABLE,
    .ctx = NULL,
};

// Clamp to a sensible minimum and power-of-two alignment.
static size_t mc_norm_align(size_t align) {
    size_t min_align = alignof(max_align_t);
    if (align < min_align) {
        return min_align;
    }
    if ((align & (align - 1)) != 0) {
        return min_align;
    }
    return align;
}

// Allocate with an embedded header and manual alignment.
static void *mc_alloc_with_heap(mc_heap_t *heap, size_t size, size_t align) {
    if (size == 0) {
        return NULL;
    }

    // Ensure we have a sane power-of-two alignment.
    align = mc_norm_align(align);

    // Over-allocate to make room for the header and alignment padding.
    size_t total = sizeof(mc_alloc_header_t) + size + (align - 1);
    void *raw = heap->vtable->alloc(total);
    if (!raw) {
        return NULL;
    }

    // Compute the aligned user pointer just after the header region.
    uintptr_t base = (uintptr_t)raw + sizeof(mc_alloc_header_t);
    uintptr_t aligned = (base + (align - 1)) & ~(uintptr_t)(align - 1);

    // Stash metadata right before the aligned payload.
    mc_alloc_header_t *header = (mc_alloc_header_t *)(aligned - sizeof(mc_alloc_header_t));
    header->heap = heap;
    header->size = size;
    header->raw = raw;

    return (void *)aligned;
}

// Header lives immediately before the aligned user pointer.
static mc_alloc_header_t *mc_header_from_ptr(void *ptr) {
    return (mc_alloc_header_t *)((uint8_t *)ptr - sizeof(mc_alloc_header_t));
}

void *__mc_alloc(size_t size, size_t align) {
    // v1: always allocate from the default heap.
    void *ptr = mc_alloc_with_heap(&MC_DEFAULT_HEAP, size, align);
    mc_trace_log("alloc", ptr, size, align, &MC_DEFAULT_HEAP);
    return ptr;
}

void *__mc_realloc(void *ptr, size_t size, size_t align) {
    // Simple realloc: allocate new, copy, free old.
    if (!ptr) {
        return __mc_alloc(size, align);
    }
    if (size == 0) {
        __mc_free(ptr);
        return NULL;
    }

    // Preserve the original heap and size metadata.
    mc_alloc_header_t *header = mc_header_from_ptr(ptr);
    size_t copy_size = header->size < size ? header->size : size;

    // Allocate a new block and copy the payload.
    void *new_ptr = mc_alloc_with_heap(header->heap, size, align);
    if (!new_ptr) {
        return NULL;
    }

    mc_trace_log("realloc", new_ptr, size, align, header->heap);

    memcpy(new_ptr, ptr, copy_size);
    __mc_free(ptr);
    return new_ptr;
}

void __mc_free(void *ptr) {
    if (!ptr) {
        return;
    }

    // Recover the heap and raw pointer from the allocation header.
    mc_alloc_header_t *header = mc_header_from_ptr(ptr);
    mc_heap_t *heap = header->heap ? header->heap : &MC_DEFAULT_HEAP;
    mc_trace_log("free", ptr, header->size, 0, heap);
    heap->vtable->free(header->raw);
}

void *__rt_alloc(uint64_t size, uint64_t align) {
    return __mc_alloc((size_t)size, (size_t)align);
}

void *__rt_realloc(void *ptr, uint64_t size, uint64_t align) {
    return __mc_realloc(ptr, (size_t)size, (size_t)align);
}

void __rt_free(void *ptr) {
    __mc_free(ptr);
}

void __rt_set_alloc_trace(uint8_t enabled) {
    __mc_set_alloc_trace(enabled);
}
