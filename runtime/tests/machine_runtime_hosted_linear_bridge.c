#include <stdint.h>

#include "machine/bridge.h"
#include "machine/hosted_instance.h"
#include "machine/internal.h"

typedef struct mc_test_hosted_linear_ctx {
    mc_hosted_instance_table_t instances;
    uint64_t machine_kind;
} mc_test_hosted_linear_ctx_t;

uint64_t __mc_hosted_linear_trigger_dispatch_u64(
    uint64_t machine_kind,
    uint64_t machine_id,
    uint64_t kind,
    uint64_t current_state_tag,
    uint64_t key,
    uint64_t payload0,
    uint64_t payload1
) {
    (void)machine_id;
    (void)key;
    (void)payload0;
    (void)payload1;
    if (machine_kind == 1 && kind == MC_HOSTED_LINEAR_KIND_TRIGGER_BASE + 1 &&
        current_state_tag == 1) {
        return 2;
    }
    return 0;
}

// Validates the first hosted-linear runtime bridge surface:
// - spawn allocates a real machine slot with hosted instance storage
// - create allocates an instance with a monotonic key and initial state tag
int main(void) {
    uint64_t rt_handle = __mc_machine_runtime_new();
    if (rt_handle == 0) {
        return 1;
    }

    mc_machine_runtime_t *rt = (mc_machine_runtime_t *)(uintptr_t)rt_handle;

    uint64_t machine_id = __mc_hosted_linear_spawn_u64(rt_handle, 4, 1);
    if (machine_id == 0) {
        __mc_machine_runtime_free(rt_handle);
        return 2;
    }

    mc_machine_slot_t *slot = mc_get_slot(rt, (mc_machine_id_t)machine_id);
    if (!slot || !slot->dispatch_ctx || !slot->dispatch_ctx_drop || !slot->dispatch) {
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

    if (__mc_hosted_linear_deliver_u64(
            rt_handle,
            machine_id,
            key,
            1,
            2,
            MC_HOSTED_LINEAR_KIND_TRIGGER_BASE + 1,
            0,
            0
        ) !=
        MC_HOSTED_UPDATE_OK) {
        __mc_machine_runtime_free(rt_handle);
        return 10;
    }

    if (__mc_machine_runtime_mailbox_len(rt, (mc_machine_id_t)machine_id) != 0 ||
        __mc_machine_runtime_ready_len(rt) != 0) {
        __mc_machine_runtime_free(rt_handle);
        return 11;
    }

    resumed_tag = __mc_hosted_linear_resume_state_u64(rt_handle, machine_id, key);
    if (resumed_tag != 2) {
        __mc_machine_runtime_free(rt_handle);
        return 12;
    }

    if (__mc_hosted_linear_deliver_u64(
            rt_handle,
            machine_id,
            key,
            1,
            3,
            MC_HOSTED_LINEAR_KIND_TRIGGER_BASE + 1,
            0,
            0
        ) !=
        MC_HOSTED_UPDATE_STALE) {
        __mc_machine_runtime_free(rt_handle);
        return 13;
    }

    if (__mc_hosted_linear_deliver_u64(
            rt_handle,
            machine_id,
            key + 1,
            2,
            3,
            MC_HOSTED_LINEAR_KIND_TRIGGER_BASE + 1,
            0,
            0
        ) !=
        MC_HOSTED_UPDATE_NOT_FOUND) {
        __mc_machine_runtime_free(rt_handle);
        return 14;
    }

    if (__mc_hosted_linear_resume_state_u64(rt_handle, machine_id, key + 1) != 0) {
        __mc_machine_runtime_free(rt_handle);
        return 15;
    }

    uint64_t wait_key = __mc_hosted_linear_create_u64(
        rt_handle,
        machine_id,
        1,
        (uintptr_t)0xCAFE
    );
    if (wait_key == 0) {
        __mc_machine_runtime_free(rt_handle);
        return 16;
    }

    mc_machine_envelope_t env = {
        .kind = MC_HOSTED_LINEAR_KIND_DELIVER,
        .src = 0,
        .reply_cap_id = 777,
        .pending_id = 1,
        .payload0 = wait_key,
        .payload1 = 0,
        .origin_payload0 = 0,
        .origin_payload1 = MC_HOSTED_LINEAR_KIND_TRIGGER_BASE + 1,
        .origin_request_site_key = 2,
    };
    if (__mc_machine_runtime_enqueue(rt, (mc_machine_id_t)machine_id, &env) !=
        MC_MAILBOX_ENQUEUE_OK) {
        __mc_machine_runtime_free(rt_handle);
        return 17;
    }

    resumed_tag =
        __mc_hosted_linear_wait_state_u64(rt_handle, machine_id, wait_key, 1);
    if (resumed_tag != 2) {
        __mc_machine_runtime_free(rt_handle);
        return 18;
    }

    if (__mc_machine_runtime_mailbox_len(rt, (mc_machine_id_t)machine_id) != 0 ||
        __mc_machine_runtime_ready_len(rt) != 0) {
        __mc_machine_runtime_free(rt_handle);
        return 19;
    }

    __mc_machine_runtime_free(rt_handle);
    return 0;
}
