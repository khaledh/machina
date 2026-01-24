#include <stddef.h>
#include <stdint.h>

#include "types.h"

void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2);
uint64_t __mc_u64_to_dec(const mc_slice_t *s, uint64_t value);
uint64_t __mc_i64_to_dec(const mc_slice_t *s, int64_t value);
void __mc_memcpy(mc_slice_t *dst, const mc_slice_t *src);
void *__mc_alloc(size_t size, size_t align);
void *__mc_realloc(void *ptr, size_t size, size_t align);
void __mc_free(void *ptr);

// CheckKind::Range = 2
#define MC_TRAP_RANGE 2

static uint32_t mc_next_cap(uint32_t current, uint32_t min_cap) {
    uint32_t cap = current ? current : 1;
    while (cap < min_cap) {
        uint32_t doubled = cap << 1;
        if (doubled <= cap || doubled > MC_CAP_MASK) {
            cap = min_cap;
            break;
        }
        cap = doubled;
    }
    return cap;
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
    dst->cap = 0;
}

/**
 * Ensures the string has at least min_cap bytes of capacity and is owned.
 */
void __mc_string_ensure(mc_string_t *s, uint32_t min_cap) {
    uint32_t len = s->len;
    if (len > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, len, 0, (uint64_t)MC_CAP_MASK + 1);
    }
    if (min_cap < len) {
        min_cap = len;
    }
    if (min_cap > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, min_cap, 0, (uint64_t)MC_CAP_MASK + 1);
    }

    uint32_t cap = mc_cap_value(s->cap);
    uint8_t owned = mc_cap_is_owned(s->cap);
    if (owned && min_cap <= cap) {
        return;
    }

    if (!owned && min_cap == 0) {
        s->cap = mc_cap_with_owned(0);
        return;
    }

    uint32_t new_cap = cap;
    if (new_cap < min_cap) {
        new_cap = mc_next_cap(cap, min_cap);
    }

    uint64_t bytes = (uint64_t)new_cap;
    void *new_ptr = owned
        ? __mc_realloc((void *)s->ptr, bytes, 1)
        : __mc_alloc(bytes, 1);
    if (!new_ptr && bytes != 0) {
        __mc_trap(MC_TRAP_RANGE, bytes, 0, bytes + 1);
    }

    if (!owned && s->ptr && len > 0) {
        mc_slice_t src = {
            .ptr = s->ptr,
            .len = (uint64_t)len,
        };
        mc_slice_t dst = {
            .ptr = (uint64_t)new_ptr,
            .len = (uint64_t)len,
        };
        __mc_memcpy(&dst, &src);
    }

    s->ptr = (uint64_t)new_ptr;
    s->cap = mc_cap_with_owned(new_cap);
}

/**
 * Drops an owned string buffer (no-op for string views).
 */
void __mc_string_drop(mc_string_t *s) {
    if (!mc_cap_is_owned(s->cap)) {
        return;
    }
    if (s->ptr != 0) {
        __mc_free((void *)s->ptr);
    }
    s->ptr = 0;
    s->len = 0;
    s->cap = 0;
}

/**
 * Appends a byte slice to the string, promoting it to owned storage if needed.
 */
void __mc_string_append_bytes(mc_string_t *s, uint64_t ptr, uint64_t len) {
    uint32_t cur_len = s->len;
    if (cur_len > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, cur_len, 0, (uint64_t)MC_CAP_MASK + 1);
    }

    if (len == 0) {
        return;
    }

    uint64_t new_len = (uint64_t)cur_len + len;
    if (new_len > MC_CAP_MASK) {
        __mc_trap(MC_TRAP_RANGE, new_len, 0, (uint64_t)MC_CAP_MASK + 1);
    }

    __mc_string_ensure(s, (uint32_t)new_len);

    mc_slice_t src = {
        .ptr = ptr,
        .len = len,
    };
    mc_slice_t dst = {
        .ptr = s->ptr + cur_len,
        .len = len,
    };
    __mc_memcpy(&dst, &src);
    s->len = (uint32_t)new_len;
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

void __rt_string_append_bytes(uint64_t s_ptr, uint64_t ptr, uint64_t len) {
    mc_string_t *s = (mc_string_t *)s_ptr;
    __mc_string_append_bytes(s, ptr, len);
}

void __rt_string_ensure(uint64_t s_ptr, uint32_t min_cap) {
    mc_string_t *s = (mc_string_t *)s_ptr;
    __mc_string_ensure(s, min_cap);
}

void __rt_string_drop(uint64_t s_ptr) {
    mc_string_t *s = (mc_string_t *)s_ptr;
    __mc_string_drop(s);
}
