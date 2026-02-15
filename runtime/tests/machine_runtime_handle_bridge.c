#include <stdint.h>

#include "machine_runtime.h"

// Validates opaque-handle bridge helpers used by `std::machine`.
int main(void) {
    uint64_t rt = __mc_machine_runtime_new();
    if (rt == 0) {
        return 1;
    }

    uint64_t m = __mc_machine_runtime_spawn_u64(rt, 4);
    if (m == 0) {
        return 1;
    }
    if (__mc_machine_runtime_start_u64(rt, m) == 0) {
        return 1;
    }

    // Enqueue a raw message through handle bridge.
    uint64_t send = __mc_machine_runtime_send_u64(rt, m, 7, 11, 22);
    if (send != MC_MAILBOX_ENQUEUE_OK) {
        return 1;
    }

    // request_u64 returns minted pending id (non-zero) on success.
    uint64_t pending = __mc_machine_runtime_request_u64(rt, m, m, 8, 33, 44);
    if (pending == 0) {
        return 1;
    }

    // Reply is accepted when pending cap is active.
    uint64_t reply = __mc_machine_runtime_reply_u64(rt, m, pending, 9, 55, 66);
    if (reply != MC_REPLY_OK) {
        return 1;
    }

    // Step executes one queued envelope at a time (idle/work status).
    uint64_t step1 = __mc_machine_runtime_step_u64(rt);
    if (step1 != MC_STEP_DID_WORK) {
        return 1;
    }
    uint64_t step2 = __mc_machine_runtime_step_u64(rt);
    if (step2 != MC_STEP_DID_WORK) {
        return 1;
    }
    uint64_t step3 = __mc_machine_runtime_step_u64(rt);
    if (step3 != MC_STEP_DID_WORK) {
        return 1;
    }
    uint64_t step4 = __mc_machine_runtime_step_u64(rt);
    if (step4 != MC_STEP_IDLE) {
        return 1;
    }

    __mc_machine_runtime_free(rt);
    return 0;
}
