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

void __mc_trap(uint64_t kind) {
    const char *msg = "Machina runtime error: unknown trap\n";
    switch (kind) {
        case 0:
            msg = "Machina runtime error: bounds check failed\n";
            break;
        default:
            break;
    }

    write_msg(msg);
    _exit((int)(100 + kind));
}
