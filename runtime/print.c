#include <stdint.h>
#include <unistd.h>

#include "types.h"

#define STDOUT_FILENO 1

void __mc_print(const mc_string_t *s, uint64_t newline) {
    uint64_t offset = 0;
    while (offset < s->len) {
        ssize_t n = write(
            STDOUT_FILENO,
            (const void *)(s->ptr + offset),
            (size_t)(s->len - offset)
        );
        if (n <= 0) {
            return;
        }
        offset += (uint64_t)n;
    }
    if (newline != 0) {
        (void)write(STDOUT_FILENO, "\n", 1);
    }
}
