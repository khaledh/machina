#include <stdint.h>

#include "types.h"

/**
 * Converts a u64 to a decimal string and writes it to a slice.
 * Returns:
 * - 0 if the buffer is too small
 * - otherwise, the number of digits written
 */
static uint64_t __mc_u64_to_base(
    const mc_slice_t *s,
    uint64_t value,
    uint64_t base,
    const char *digits
) {
    char *buf = (char *)s->ptr;
    uint64_t len = s->len;

    if (value == 0) {
        if (len > 0) {
            buf[0] = digits[0];
            return 1;
        }
        return 0;
    }

    // Count digits first
    uint64_t tmp = value;
    uint64_t count = 0;
    while (tmp > 0) {
        count++;
        tmp /= base;
    }

    // Return 0 if the buffer is too small
    if (count > len) {
        return 0;
    }

    // Write digits from the end backwards
    uint64_t pos = count;
    while (pos > 0) {
        buf[--pos] = digits[value % base];
        value /= base;
    }

    return count;
}

uint64_t __mc_u64_to_dec(const mc_slice_t *s, uint64_t value) {
    static const char digits[] = "0123456789";
    return __mc_u64_to_base(s, value, 10, digits);
}

/**
 * Converts a u64 to a binary string and writes it to a slice.
 * Returns:
 * - 0 if the buffer is too small
 * - otherwise, the number of digits written
 */
uint64_t __mc_u64_to_bin(const mc_slice_t *s, uint64_t value) {
    static const char digits[] = "01";
    return __mc_u64_to_base(s, value, 2, digits);
}

/**
 * Converts a u64 to an octal string and writes it to a slice.
 * Returns:
 * - 0 if the buffer is too small
 * - otherwise, the number of digits written
 */
uint64_t __mc_u64_to_oct(const mc_slice_t *s, uint64_t value) {
    static const char digits[] = "01234567";
    return __mc_u64_to_base(s, value, 8, digits);
}

/**
 * Converts a u64 to a hexadecimal string and writes it to a slice.
 * Returns:
 * - 0 if the buffer is too small
 * - otherwise, the number of digits written
 */
uint64_t __mc_u64_to_hex(const mc_slice_t *s, uint64_t value) {
    static const char digits[] = "0123456789abcdef";
    return __mc_u64_to_base(s, value, 16, digits);
}
