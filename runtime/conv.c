#include <stdint.h>

#include "types.h"

void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2);

// CheckKind::Range = 2
#define MC_TRAP_RANGE 2

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

/**
 * Creates a string view over a byte slice.
 * The slice must point to valid UTF-8 data. No validation is performed.
 */
void __mc_string_from_bytes(mc_string_t *dst, const mc_slice_t *s) {
    if (s->len > UINT32_MAX) {
        __mc_trap(MC_TRAP_RANGE, s->len, 0, (uint64_t)UINT32_MAX + 1);
    }

    dst->ptr = s->ptr;
    dst->len = (uint32_t)s->len;
    dst->tag = 1;
    dst->_pad[0] = 0;
    dst->_pad[1] = 0;
    dst->_pad[2] = 0;
}
