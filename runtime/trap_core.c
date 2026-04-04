#include <stdint.h>

/*
 * Minimal trap implementation for non-hosted runtime-core slices.
 * This deliberately avoids any hosted I/O or process assumptions.
 */
__attribute__((noreturn)) void __mc_trap(
    uint64_t kind,
    uint64_t arg0,
    uint64_t arg1,
    uint64_t arg2
) {
    (void)kind;
    (void)arg0;
    (void)arg1;
    (void)arg2;

    for (;;) {
        __builtin_trap();
    }
}

void __rt_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2) {
    __mc_trap(kind, arg0, arg1, arg2);
}
