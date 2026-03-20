#include <stdint.h>
#include <string.h>

#include "types.h"

void __mc_string_from_bytes(mc_string_t *dst, const mc_slice_t *s);

static uint64_t g_argc = 0;
static const char *const *g_argv = 0;

void __rt_process_args_init(uint64_t argc, uint64_t argv_ptr) {
    g_argc = argc;
    g_argv = (const char *const *)(uintptr_t)argv_ptr;
}

uint64_t __rt_args_len(void) {
    return g_argc;
}

void __rt_arg_at(uint64_t out_ptr, uint64_t index) {
    mc_string_t *out = (mc_string_t *)(uintptr_t)out_ptr;
    if (!out || index >= g_argc || !g_argv) {
        if (out) {
            out->ptr = 0;
            out->len = 0;
            out->cap = 0;
        }
        return;
    }

    const char *arg = g_argv[index];
    mc_slice_t bytes = {
        .ptr = (uint64_t)(uintptr_t)arg,
        .len = arg ? (uint64_t)strlen(arg) : 0,
    };
    __mc_string_from_bytes(out, &bytes);
}
