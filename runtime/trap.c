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
 * - arg2: payload slot 2
 */
void __mc_trap(uint64_t kind, uint64_t arg0, uint64_t arg1, uint64_t arg2) {
    // common prefix and suffix
    const char *prefix = "Runtime error: ";
    const char *line_end = "\n";

    // division by zero
    const char *divbyzero_msg = "Division by zero";

    // bounds check
    const char *bounds_msg_1 = "Index out of bounds: index=";
    const char *bounds_msg_2 = ", len=";

    // range check
    const char *range_msg_1 = "Value out of range: value=";
    const char *range_msg_2 = ", min(incl)=";
    const char *range_msg_3 = ", max(excl)=";

    // unknown trap
    const char *fallback_msg = "Unknown trap";

    write_msg(prefix);
    switch (kind) {
        case 0: // division by zero
            write_msg(divbyzero_msg);
            break;

        case 1: // bounds check
            write_msg(bounds_msg_1);
            write_u64(arg0);
            write_msg(bounds_msg_2);
            write_u64(arg1);
            break;

        case 2: // range check
            write_msg(range_msg_1);
            write_u64(arg0);
            write_msg(range_msg_2);
            write_u64(arg1);
            write_msg(range_msg_3);
            write_u64(arg2);
            break;

        default: // unknown trap
            write_msg(fallback_msg);
            break;
    }
    write_msg(line_end);

    _exit((int)(100 + kind));
}
