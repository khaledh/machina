#include <stddef.h>
#include <stdint.h>
#include <unistd.h>

#define STDERR_FILENO 2

static void write_msg(const char *msg) {
    size_t len = 0;
    while (msg[len] != '\0') {
        len++;
    }
    (void)write(STDERR_FILENO, msg, len);
}

// helper that converts to decimal into a small buffer and writes it.
static void write_u64(uint64_t value) {
    char buf[21];
    size_t pos = sizeof(buf);

    if (value == 0) {
        buf[--pos] = '0';
    } else {
        while (value > 0 && pos > 0) {
            buf[--pos] = (char)('0' + (value % 10));
            value /= 10;
        }
    }

    (void)write(STDERR_FILENO, &buf[pos], sizeof(buf) - pos);
}

/*
 * Runtime trap handler called by generated code.
 * - kind: check kind discriminator
 * - arg0: payload slot 0
 * - arg1: payload slot 1
 */
void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1) {
    const char *fallback = "unknown trap";

    // common prefix and suffix
    const char *prefix = "Runtime error: ";
    const char *suffix = ")";
    const char *line_end = "\n";

    // bounds check
    const char *bounds_prefix = "Index out of bounds (index=";
    const char *bounds_mid = ", len=";

    // division by zero
    const char *div_prefix = "Division by zero";

    switch (kind) {
        case 0:
            write_msg(prefix);
            write_msg(bounds_prefix);
            write_u64(arg0);
            write_msg(bounds_mid);
            write_u64(arg1);
            write_msg(suffix);
            write_msg(line_end);
            break;
        case 1:
            write_msg(prefix);
            write_msg(div_prefix);
            write_msg(line_end);
            break;
        default:
            write_msg(prefix);
            write_msg(fallback);
            write_msg(line_end);
            break;
    }

    _exit((int)(100 + kind));
}
