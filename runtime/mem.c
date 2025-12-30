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
