#include <stdint.h>

#include "machine/bridge.h"

static uint64_t g_bootstrap_calls = 0;

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

    txn->has_next_state = 1;
    txn->next_state = current_state + env->payload0;
    return MC_DISPATCH_OK;
}

// Strong definition that overrides weak symbol in runtime.
void __mc_machine_bootstrap(void) {
    g_bootstrap_calls += 1;
    __mc_machine_runtime_register_thunk_u64(99, (uint64_t)(uintptr_t)test_dispatch);
}

int main(void) {
    uint64_t rt1 = __mc_machine_runtime_new();
    uint64_t rt2 = __mc_machine_runtime_new();
    if (rt1 == 0 || rt2 == 0) {
        return 1;
    }
    if (g_bootstrap_calls != 1) {
        return 1;
    }

    uint64_t m = __mc_machine_runtime_spawn_u64(rt1, 4);
    if (m == 0) {
        return 1;
    }
    if (__mc_machine_runtime_bind_dispatch_thunk_u64(rt1, m, 99, 0) == 0) {
        return 1;
    }
    if (__mc_machine_runtime_start_u64(rt1, m) == 0) {
        return 1;
    }
    if (__mc_machine_runtime_send_u64(rt1, m, 1, 7, 0) != MC_MAILBOX_ENQUEUE_OK) {
        return 1;
    }
    if (__mc_machine_runtime_step_u64(rt1) != MC_STEP_DID_WORK) {
        return 1;
    }

    __mc_machine_runtime_free(rt1);
    __mc_machine_runtime_free(rt2);
    return 0;
}
