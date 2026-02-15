#include <stdint.h>
#include <stddef.h>

#include "machine_runtime.h"

// Minimal dispatch callback used to validate per-machine dispatch binding.
static mc_dispatch_result_t test_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    (void)ctx;
    (void)machine_id;
    (void)fault_code;

    // Deterministic state update: accumulate payload0 into state.
    txn->has_next_state = 1;
    txn->next_state = current_state + env->payload0;
    return MC_DISPATCH_OK;
}

int main(void) {
    mc_machine_runtime_t rt;
    __mc_machine_runtime_init(&rt);

    mc_machine_id_t m = 0;
    if (!__mc_machine_runtime_spawn(&rt, 4, &m)) {
        return 1;
    }

    __mc_machine_runtime_bind_dispatch(&rt, m, test_dispatch, NULL);
    __mc_machine_runtime_set_state(&rt, m, 10);
    if (!__mc_machine_runtime_start(&rt, m)) {
        return 1;
    }

    mc_machine_envelope_t env = {
        .kind = 1,
        .src = 0,
        .reply_cap_id = 0,
        .pending_id = 0,
        .payload0 = 5,
        .payload1 = 0,
    };
    if (__mc_machine_runtime_enqueue(&rt, m, &env) != MC_MAILBOX_ENQUEUE_OK) {
        return 1;
    }

    uint64_t step = __mc_machine_runtime_step_u64((uint64_t)(uintptr_t)&rt);
    if (step != MC_STEP_DID_WORK) {
        return 1;
    }
    if (__mc_machine_runtime_state(&rt, m) != 15) {
        return 1;
    }

    __mc_machine_runtime_drop(&rt);
    return 0;
}
