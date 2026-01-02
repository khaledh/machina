#include <stddef.h>
#include <stdint.h>

void *__mc_alloc(size_t size, size_t align);
void *__mc_realloc(void *ptr, size_t size, size_t align);
void __mc_free(void *ptr);

int main(void) {
    uint8_t *p = (uint8_t *)__mc_alloc(32, 16);
    if (!p) {
        return 1;
    }
    if (((uintptr_t)p % 16) != 0) {
        __mc_free(p);
        return 2;
    }

    for (uint8_t i = 0; i < 32; i++) {
        p[i] = i;
    }

    uint8_t *q = (uint8_t *)__mc_realloc(p, 64, 16);
    if (!q) {
        return 3;
    }
    if (((uintptr_t)q % 16) != 0) {
        __mc_free(q);
        return 4;
    }

    for (uint8_t i = 0; i < 32; i++) {
        if (q[i] != i) {
            __mc_free(q);
            return 5;
        }
    }

    __mc_free(q);
    return 0;
}
