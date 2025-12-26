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
 * - kind: check kind discriminator (0 = bounds)
 * - arg0: payload slot 0 (bounds: index)
 * - arg1: payload slot 1 (bounds: length)
 */
void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1) {
    const char *fallback = "Machina runtime error: unknown trap\n";

    const char *prefix = "Machina runtime error: bounds check failed (index=";
    const char *mid = ", len=";
    const char *suffix = ")\n";
    switch (kind) {
        case 0:
            write_msg(prefix);
            write_u64(arg0);
            write_msg(mid);
            write_u64(arg1);
            write_msg(suffix);
            break;
        default:
            write_msg(fallback);
            break;
    }

    _exit((int)(100 + kind));
}
