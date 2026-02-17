#include <stddef.h>
#include <stdint.h>

#include "machine/runtime.h"

// Verifies stop-path eager cleanup semantics:
// - MC_DISPATCH_STOP transitions lifecycle to STOPPED
// - remaining mailbox entries are drained/released
// - machine-owned subscriptions are removed
// - stopped machine rejects future enqueue attempts
static mc_dispatch_result_t stop_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    (void)ctx;
    (void)machine_id;
    (void)current_state;
    (void)fault_code;

    txn->has_next_state = 0;
    txn->next_state = 0;
    txn->next_state_tag = 0;
    txn->outbox = NULL;
    txn->outbox_len = 0;
    txn->requests = NULL;
    txn->requests_len = 0;
    txn->replies = NULL;
    txn->replies_len = 0;

    static mc_subscription_update_t sub_add = {
        .op = MC_SUBSCRIPTION_ADD,
        .machine_id = 0,
        .kind = 7,
        .routing = 1,
    };

    if (env->kind == 1) {
        sub_add.machine_id = machine_id;
        txn->subscriptions = &sub_add;
        txn->subscriptions_len = 1;
        return MC_DISPATCH_OK;
    }

    txn->subscriptions = NULL;
    txn->subscriptions_len = 0;

    if (env->kind == 99) {
        return MC_DISPATCH_STOP;
    }
    return MC_DISPATCH_OK;
}

int main(void) {
    mc_machine_runtime_t rt;
    __mc_machine_runtime_init(&rt);

    mc_machine_id_t m = 0;
    if (!__mc_machine_runtime_spawn(&rt, 4, &m)) {
        return 1;
    }
    if (!__mc_machine_runtime_start(&rt, m)) {
        return 2;
    }

    mc_machine_envelope_t first = {.kind = 1};
    mc_machine_envelope_t stop = {.kind = 99};
    mc_machine_envelope_t tail = {.kind = 55};
    if (__mc_machine_runtime_enqueue(&rt, m, &first) != MC_MAILBOX_ENQUEUE_OK) {
        return 3;
    }
    if (__mc_machine_runtime_enqueue(&rt, m, &stop) != MC_MAILBOX_ENQUEUE_OK) {
        return 4;
    }
    if (__mc_machine_runtime_enqueue(&rt, m, &tail) != MC_MAILBOX_ENQUEUE_OK) {
        return 5;
    }

    if (!__mc_machine_runtime_dispatch_one_txn(&rt, stop_dispatch, NULL)) {
        return 6;
    }
    if (!__mc_machine_runtime_subscription_contains(&rt, m, 7, 1)) {
        return 7;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, m) != 2) {
        return 8;
    }

    if (!__mc_machine_runtime_dispatch_one_txn(&rt, stop_dispatch, NULL)) {
        return 9;
    }
    if (__mc_machine_runtime_lifecycle(&rt, m) != MC_MACHINE_STOPPED) {
        return 10;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, m) != 0) {
        return 11;
    }
    if (__mc_machine_runtime_subscription_contains(&rt, m, 7, 1)) {
        return 12;
    }
    if (__mc_machine_runtime_stopped_cleanup_count(&rt) != 1) {
        return 13;
    }
    if (__mc_machine_runtime_enqueue(&rt, m, &tail) != MC_MAILBOX_ENQUEUE_MACHINE_NOT_RUNNING) {
        return 14;
    }

    __mc_machine_runtime_drop(&rt);
    return 0;
}
