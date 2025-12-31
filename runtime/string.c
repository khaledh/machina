#include <stdint.h>

#include "types.h"

void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2);

// CheckKind::Range = 2
#define MC_TRAP_RANGE 2

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
