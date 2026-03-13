#include <stdint.h>

#include "machine/bridge.h"
#include "machine/hosted_instance.h"
#include "machine/internal.h"

typedef struct mc_test_hosted_linear_ctx {
    mc_hosted_instance_table_t instances;
} mc_test_hosted_linear_ctx_t;

// Validates the first hosted-linear runtime bridge surface:
// - spawn allocates a real machine slot with hosted instance storage
// - create allocates an instance with a monotonic key and initial state tag
int main(void) {
    uint64_t rt_handle = __mc_machine_runtime_new();
    if (rt_handle == 0) {
        return 1;
    }

    mc_machine_runtime_t *rt = (mc_machine_runtime_t *)(uintptr_t)rt_handle;

    uint64_t machine_id = __mc_hosted_linear_spawn_u64(rt_handle, 4);
    if (machine_id == 0) {
        __mc_machine_runtime_free(rt_handle);
        return 2;
    }

    mc_machine_slot_t *slot = mc_get_slot(rt, (mc_machine_id_t)machine_id);
    if (!slot || !slot->dispatch_ctx || !slot->dispatch_ctx_drop) {
        __mc_machine_runtime_free(rt_handle);
        return 3;
    }

    mc_test_hosted_linear_ctx_t *ctx =
        (mc_test_hosted_linear_ctx_t *)slot->dispatch_ctx;
    if (mc_hosted_instance_table_count(&ctx->instances) != 0) {
        __mc_machine_runtime_free(rt_handle);
        return 4;
    }

    uint64_t key = __mc_hosted_linear_create_u64(
        rt_handle,
        machine_id,
        1,
        (uintptr_t)0xBEEF
    );
    if (key == 0) {
        __mc_machine_runtime_free(rt_handle);
        return 5;
    }

    if (mc_hosted_instance_table_count(&ctx->instances) != 1) {
        __mc_machine_runtime_free(rt_handle);
        return 6;
    }

    uint64_t state_tag = 0;
    uintptr_t payload = 0;
    if (!mc_hosted_instance_table_lookup(
            &ctx->instances,
            key,
            &state_tag,
            &payload
        )) {
        __mc_machine_runtime_free(rt_handle);
        return 7;
    }

    if (state_tag != 1 || payload != (uintptr_t)0xBEEF) {
        __mc_machine_runtime_free(rt_handle);
        return 8;
    }

    uint64_t resumed_tag =
        __mc_hosted_linear_resume_state_u64(rt_handle, machine_id, key);
    if (resumed_tag != 1) {
        __mc_machine_runtime_free(rt_handle);
        return 9;
    }

    if (__mc_hosted_linear_resume_state_u64(rt_handle, machine_id, key + 1) != 0) {
        __mc_machine_runtime_free(rt_handle);
        return 10;
    }

    __mc_machine_runtime_free(rt_handle);
    return 0;
}
