#include <stdint.h>

#include "types.h"

void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2);
uint64_t __mc_u64_to_dec(const mc_slice_t *s, uint64_t value);
uint64_t __mc_i64_to_dec(const mc_slice_t *s, int64_t value);
void __mc_memcpy(mc_slice_t *dst, const mc_slice_t *src);

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
    dst->cap = 0;
}

/**
 * Initializes a formatting builder over a provided byte buffer.
 */
void __mc_fmt_init(mc_fmt_t *fmt, const mc_slice_t *buf) {
    fmt->ptr = buf->ptr;
    fmt->len = 0;
    fmt->cap = buf->len;
}

/**
 * Appends a byte range to the formatting builder.
 */
void __mc_fmt_append_bytes(mc_fmt_t *fmt, uint64_t ptr, uint64_t len) {
    uint64_t new_len = fmt->len + len;
    if (new_len > fmt->cap) {
        __mc_trap(MC_TRAP_RANGE, new_len, 0, fmt->cap + 1);
    }

    mc_slice_t src = {
        .ptr = ptr,
        .len = len,
    };
    mc_slice_t dst = {
        .ptr = fmt->ptr + fmt->len,
        .len = len,
    };
    __mc_memcpy(&dst, &src);
    fmt->len = new_len;
}

/**
 * Appends an unsigned integer in decimal to the formatting builder.
 */
void __mc_fmt_append_u64(mc_fmt_t *fmt, uint64_t value) {
    uint64_t avail = fmt->cap - fmt->len;
    mc_slice_t dst = {
        .ptr = fmt->ptr + fmt->len,
        .len = avail,
    };
    uint64_t written = __mc_u64_to_dec(&dst, value);
    if (written == 0) {
        __mc_trap(MC_TRAP_RANGE, fmt->cap, 0, fmt->cap + 1);
    }
    fmt->len += written;
}

/**
 * Appends a signed integer in decimal to the formatting builder.
 */
void __mc_fmt_append_i64(mc_fmt_t *fmt, int64_t value) {
    uint64_t avail = fmt->cap - fmt->len;
    mc_slice_t dst = {
        .ptr = fmt->ptr + fmt->len,
        .len = avail,
    };
    uint64_t written = __mc_i64_to_dec(&dst, value);
    if (written == 0) {
        __mc_trap(MC_TRAP_RANGE, fmt->cap, 0, fmt->cap + 1);
    }
    fmt->len += written;
}

/**
 * Appends a boolean as `true`/`false` to the formatting builder.
 */
void __mc_fmt_append_bool(mc_fmt_t *fmt, uint8_t value) {
    if (value) {
        __mc_fmt_append_bytes(fmt, (uint64_t)(uintptr_t)"true", 4);
    } else {
        __mc_fmt_append_bytes(fmt, (uint64_t)(uintptr_t)"false", 5);
    }
}

/**
 * Finalizes the formatting builder into a string value.
 */
void __mc_fmt_finish(mc_string_t *out, const mc_fmt_t *fmt) {
    mc_slice_t s = {
        .ptr = fmt->ptr,
        .len = fmt->len,
    };
    __mc_string_from_bytes(out, &s);
}

void __rt_fmt_init(uint64_t fmt_ptr, uint64_t buf_ptr, uint64_t buf_len) {
    mc_fmt_t *fmt = (mc_fmt_t *)fmt_ptr;
    mc_slice_t buf = { .ptr = buf_ptr, .len = buf_len };
    __mc_fmt_init(fmt, &buf);
}

void __rt_fmt_append_bytes(uint64_t fmt_ptr, uint64_t ptr, uint64_t len) {
    mc_fmt_t *fmt = (mc_fmt_t *)fmt_ptr;
    __mc_fmt_append_bytes(fmt, ptr, len);
}

void __rt_fmt_append_u64(uint64_t fmt_ptr, uint64_t value) {
    mc_fmt_t *fmt = (mc_fmt_t *)fmt_ptr;
    __mc_fmt_append_u64(fmt, value);
}

void __rt_fmt_append_i64(uint64_t fmt_ptr, int64_t value) {
    mc_fmt_t *fmt = (mc_fmt_t *)fmt_ptr;
    __mc_fmt_append_i64(fmt, value);
}

void __rt_fmt_append_bool(uint64_t fmt_ptr, uint8_t value) {
    mc_fmt_t *fmt = (mc_fmt_t *)fmt_ptr;
    __mc_fmt_append_bool(fmt, value);
}

void __rt_fmt_finish(uint64_t out_ptr, uint64_t fmt_ptr) {
    mc_string_t *out = (mc_string_t *)out_ptr;
    mc_fmt_t *fmt = (mc_fmt_t *)fmt_ptr;
    __mc_fmt_finish(out, fmt);
}

void __rt_string_from_bytes(uint64_t out_ptr, uint64_t ptr, uint64_t len) {
    mc_string_t *out = (mc_string_t *)out_ptr;
    mc_slice_t s = { .ptr = ptr, .len = len };
    __mc_string_from_bytes(out, &s);
}
