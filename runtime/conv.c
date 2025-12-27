#include <stdint.h>

#include "types.h"

/**
 * Converts a u64 to a decimal string and writes it to a slice.
 * Returns:
 * - 0 if the buffer is too small
 * - otherwise, the number of digits written
 */
uint64_t __mc_u64_to_dec(const mc_slice_t *s, uint64_t value) {
    char *buf = (char *)s->ptr;
    uint64_t len = s->len;

    if (value == 0) {
        if (len > 0) {
            buf[0] = '0';
            return 1;
        }
        return 0;
    }

    // Count digits first
    uint64_t tmp = value;
    uint64_t digits = 0;
    while (tmp > 0) {
        digits++;
        tmp /= 10;
    }

    // Return 0 if the buffer is too small
    if (digits > len) {
        return 0;
    }

    // Write digits from the end backwards
    uint64_t pos = digits;
    while (pos > 0) {
        buf[--pos] = (char)('0' + (value % 10));
        value /= 10;
    }

    return digits;
}
