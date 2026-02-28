#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>

#include "types.h"

void __mc_string_append_bytes(mc_string_t *s, uint64_t ptr, uint64_t len);
void __mc_string_drop(mc_string_t *s);

// Minimal POSIX file-I/O bridge for std::io.
//
// ABI note:
// - String/slice arguments arrive as references to runtime view structs.
// - Return value 0 is reserved as a generic failure sentinel.
// - Successful open/read/write results are encoded as (value + 1) so zero can
//   unambiguously represent failure and the stdlib can read errno via
//   __rt_file_last_errno().

// Runtime-local last-errno slot for file I/O bridge calls.
// This is read by stdlib wrappers after a sentinel return indicates failure.
#if defined(__GNUC__) || defined(__clang__)
static __thread uint64_t g_file_last_errno = 0;
#else
static uint64_t g_file_last_errno = 0;
#endif

// Stores the latest errno value observed by file bridge calls.
// A zero value means the last bridge call succeeded.
static void mc_file_set_last_errno(int err) {
    if (err == 0) {
        g_file_last_errno = 0;
        return;
    }
    g_file_last_errno = (uint64_t)err;
}

// Shared open implementation used by open_read/open_write/open_rw.
// Converts Machina string view -> temporary NUL-terminated C string.
static uint64_t mc_file_open_impl(uint64_t path_ref, int flags, mode_t mode) {
    // ABI bridge: path_ref points to a Machina string view.
    mc_string_t *path_view = (mc_string_t *)path_ref;
    uint64_t path_ptr = path_view->ptr;
    uint32_t path_len = path_view->len;

    // Guard allocation size when adding trailing NUL.
    if ((uint64_t)path_len > (uint64_t)(SIZE_MAX - 1)) {
        mc_file_set_last_errno(ENAMETOOLONG);
        return 0;
    }

    size_t len = (size_t)path_len;
    // Convert from non-NUL Machina string bytes to NUL-terminated C path.
    char *path = (char *)malloc(len + 1);
    if (path == NULL) {
        mc_file_set_last_errno(ENOMEM);
        return 0;
    }

    uint8_t *src = (uint8_t *)path_ptr;
    for (size_t i = 0; i < len; i++) {
        path[i] = (char)src[i];
    }
    path[len] = '\0';

    int fd;
    // Retry interrupted syscalls so callers do not have to special-case EINTR.
    for (;;) {
        fd = open(path, flags, mode);
        if (fd < 0 && errno == EINTR) {
            continue;
        }
        break;
    }

    // Path buffer is no longer needed after open() returns.
    free(path);

    if (fd < 0) {
        mc_file_set_last_errno(errno);
        return 0;
    }

    mc_file_set_last_errno(0);
    // Encode success as fd+1 so zero can remain the failure sentinel.
    return (uint64_t)fd + 1;
}

// Exposes the last bridge errno to stdlib wrapper code.
uint64_t __rt_file_last_errno(void) {
    return g_file_last_errno;
}

// Opens a file for read-only access.
uint64_t __rt_file_open_read(uint64_t path_ref) {
    return mc_file_open_impl(path_ref, O_RDONLY, 0);
}

// Opens/creates a file for write access, truncating existing contents.
uint64_t __rt_file_open_write(uint64_t path_ref) {
    return mc_file_open_impl(path_ref, O_WRONLY | O_CREAT | O_TRUNC, 0666);
}

// Opens/creates a file for read-write access.
uint64_t __rt_file_open_rw(uint64_t path_ref) {
    return mc_file_open_impl(path_ref, O_RDWR | O_CREAT, 0666);
}

// Reads up to buf.len bytes from fd into buf.
// Returns (bytes_read + 1) on success, 0 on failure.
uint64_t __rt_file_read(uint64_t fd_raw, uint64_t buf_ref) {
    // ABI bridge: buf_ref points to a Machina slice view.
    mc_slice_t *buf_view = (mc_slice_t *)buf_ref;
    uint64_t ptr = buf_view->ptr;
    uint64_t len = buf_view->len;

    // Reject invalid fd values that cannot round-trip through POSIX int fd.
    if (fd_raw > INT_MAX) {
        mc_file_set_last_errno(EINVAL);
        return 0;
    }

    int fd = (int)fd_raw;
    uint8_t *buf = (uint8_t *)ptr;
    size_t cap = (size_t)len;
    // POSIX read/write use ssize_t lengths; cap the request at SSIZE_MAX.
    size_t chunk = cap;
    if (chunk > (size_t)SSIZE_MAX) {
        chunk = (size_t)SSIZE_MAX;
    }

    ssize_t n;
    // EINTR is retried so callers see a stable "success or failure" contract.
    for (;;) {
        n = read(fd, buf, chunk);
        if (n < 0 && errno == EINTR) {
            continue;
        }
        break;
    }

    if (n < 0) {
        mc_file_set_last_errno(errno);
        return 0;
    }

    // Success path clears errno slot for consumers of __rt_file_last_errno().
    mc_file_set_last_errno(0);
    // Encode success as n+1 so zero can remain the failure sentinel.
    return (uint64_t)n + 1;
}

// Reads the full file contents into an owned Machina string.
// The bytes are treated as text without UTF-8 validation; std::io owns the
// higher-level contract for when this is appropriate to call.
void __rt_file_read_all_text(uint64_t out_ptr, uint64_t fd_raw) {
    mc_string_t *out = (mc_string_t *)out_ptr;

    // Start with a well-defined empty owned string so append can promote it.
    out->ptr = 0;
    out->len = 0;
    out->cap = 0;

    // Reject invalid fd values that cannot round-trip through POSIX int fd.
    if (fd_raw > INT_MAX) {
        mc_file_set_last_errno(EINVAL);
        return;
    }

    int fd = (int)fd_raw;
    uint8_t buf[4096];

    for (;;) {
        ssize_t n;
        // EINTR is retried so callers see a stable "read until EOF or fail"
        // contract from the runtime helper.
        for (;;) {
            n = read(fd, buf, sizeof(buf));
            if (n < 0 && errno == EINTR) {
                continue;
            }
            break;
        }

        if (n < 0) {
            mc_file_set_last_errno(errno);
            __mc_string_drop(out);
            return;
        }

        if (n == 0) {
            break;
        }

        // Grow the owned output string incrementally as chunks arrive.
        __mc_string_append_bytes(out, (uint64_t)(uintptr_t)buf, (uint64_t)n);
    }

    mc_file_set_last_errno(0);
}

// Writes up to data.len bytes from binary payload to fd.
// Returns (bytes_written + 1) on success, 0 on failure.
uint64_t __rt_file_write(uint64_t fd_raw, uint64_t data_ref) {
    // ABI bridge: data_ref points to a Machina slice view.
    mc_slice_t *data_view = (mc_slice_t *)data_ref;
    uint64_t ptr = data_view->ptr;
    uint64_t len = data_view->len;

    // Reject invalid fd values that cannot round-trip through POSIX int fd.
    if (fd_raw > INT_MAX) {
        mc_file_set_last_errno(EINVAL);
        return 0;
    }

    int fd = (int)fd_raw;
    uint8_t *buf = (uint8_t *)ptr;
    size_t cap = (size_t)len;
    size_t chunk = cap;
    if (chunk > (size_t)SSIZE_MAX) {
        chunk = (size_t)SSIZE_MAX;
    }

    ssize_t n;
    // EINTR is retried so callers do not need retry loops at stdlib level.
    for (;;) {
        n = write(fd, buf, chunk);
        if (n < 0 && errno == EINTR) {
            continue;
        }
        break;
    }

    if (n < 0) {
        mc_file_set_last_errno(errno);
        return 0;
    }

    // Success path clears errno slot for consumers of __rt_file_last_errno().
    mc_file_set_last_errno(0);
    // Encode success as n+1 so zero can remain the failure sentinel.
    return (uint64_t)n + 1;
}

// Closes fd and returns 1 on success, 0 on failure.
uint64_t __rt_file_close(uint64_t fd_raw) {
    // Reject invalid fd values that cannot round-trip through POSIX int fd.
    if (fd_raw > INT_MAX) {
        mc_file_set_last_errno(EINVAL);
        return 0;
    }

    int fd = (int)fd_raw;
    int rc;
    // Do not retry close() on EINTR: the fd may already be closed/reused.
    // Retrying risks closing an unrelated descriptor.
    rc = close(fd);

    if (rc < 0) {
        mc_file_set_last_errno(errno);
        return 0;
    }

    // Success path clears errno slot for consumers of __rt_file_last_errno().
    mc_file_set_last_errno(0);
    return 1;
}
