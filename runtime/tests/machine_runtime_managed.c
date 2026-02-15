#include "machine_runtime.h"

#include <stdint.h>

int main(void) {
    if (__mc_machine_runtime_managed_current_u64() != 0) {
        return 1;
    }

    uint64_t rt1 = __mc_machine_runtime_managed_bootstrap_u64();
    if (rt1 == 0) {
        return 2;
    }
    if (__mc_machine_runtime_managed_current_u64() != rt1) {
        return 3;
    }

    // Bootstrap is idempotent and should reuse the same process-managed handle.
    uint64_t rt2 = __mc_machine_runtime_managed_bootstrap_u64();
    if (rt2 != rt1) {
        return 4;
    }

    if (__mc_machine_runtime_managed_shutdown_u64() == 0) {
        return 5;
    }
    if (__mc_machine_runtime_managed_current_u64() != 0) {
        return 6;
    }

    // Double shutdown reports no active managed runtime.
    if (__mc_machine_runtime_managed_shutdown_u64() != 0) {
        return 7;
    }
    return 0;
}
