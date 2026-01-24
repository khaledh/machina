#include <stdint.h>

#include "types.h"

/**
 * Fills a slice with a single byte value.
 */
void __mc_memset(mc_slice_t *dst, uint8_t value) {
    uint8_t *buf = (uint8_t *)dst->ptr;
    uint64_t len = dst->len;
    for (uint64_t i = 0; i < len; i++) {
        buf[i] = value;
    }
}

/**
 * Copies a slice to another slice (lengths are expected to match by construction).
 */
void __mc_memcpy(mc_slice_t *dst, const mc_slice_t *src) {
    uint8_t *dst_buf = (uint8_t *)dst->ptr;
    const uint8_t *src_buf = (const uint8_t *)src->ptr;
    uint64_t len = dst->len;
    for (uint64_t i = 0; i < len; i++) {
        dst_buf[i] = src_buf[i];
    }
}

void __rt_memset(uint64_t ptr, uint64_t len, uint8_t value) {
    mc_slice_t dst = { .ptr = ptr, .len = len };
    __mc_memset(&dst, value);
}

void __rt_memcpy(uint64_t dst_ptr, uint64_t src_ptr, uint64_t len) {
    mc_slice_t dst = { .ptr = dst_ptr, .len = len };
    mc_slice_t src = { .ptr = src_ptr, .len = len };
    __mc_memcpy(&dst, &src);
}
