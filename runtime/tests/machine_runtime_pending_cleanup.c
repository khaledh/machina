#include <stddef.h>
#include <stdint.h>

#include "machine/runtime.h"

// Covers pending lifecycle cleanup paths and metrics hooks:
// - completed reply consumption
// - requester faulted cleanup
// - timeout cleanup
typedef struct pending_cleanup_ctx {
    uint64_t hook_counts[5];
    uint64_t last_pending_id;
    uint64_t last_request_site_key;
    mc_pending_cleanup_reason_t last_reason;
    mc_machine_id_t fault_machine;
    uint64_t fault_kind;
} pending_cleanup_ctx_t;

static void pending_hook(
    void *ctx,
    mc_machine_id_t requester,
    uint64_t pending_id,
    uint64_t request_site_key,
    mc_pending_cleanup_reason_t reason
) {
    (void)requester;
    pending_cleanup_ctx_t *state = (pending_cleanup_ctx_t *)ctx;
    if (reason >= MC_PENDING_CLEANUP_COMPLETED && reason <= MC_PENDING_CLEANUP_TIMEOUT) {
        state->hook_counts[(uint32_t)reason] += 1;
    }
    state->last_pending_id = pending_id;
    state->last_request_site_key = request_site_key;
    state->last_reason = reason;
}

static mc_dispatch_result_t noop_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    pending_cleanup_ctx_t *state = (pending_cleanup_ctx_t *)ctx;
    (void)current_state;
    (void)fault_code;
    txn->has_next_state = 0;
    txn->next_state = 0;
    txn->next_state_tag = 0;
    txn->outbox = NULL;
    txn->outbox_len = 0;
    txn->subscriptions = NULL;
    txn->subscriptions_len = 0;
    txn->requests = NULL;
    txn->requests_len = 0;
    txn->replies = NULL;
    txn->replies_len = 0;
    if (machine_id == state->fault_machine && env->kind == state->fault_kind) {
        return MC_DISPATCH_FAULT;
    }
    return MC_DISPATCH_OK;
}

int main(void) {
    mc_machine_runtime_t rt;
    pending_cleanup_ctx_t state = {0};
    mc_machine_id_t client = 0;
    mc_machine_id_t client_fault_policy = 0;
    mc_machine_id_t client_timeout = 0;
    mc_machine_id_t server = 0;

    __mc_machine_runtime_init(&rt);
    __mc_machine_runtime_set_hooks(&rt, NULL, NULL, &state);
    __mc_machine_runtime_set_pending_hook(&rt, pending_hook);

    if (!__mc_machine_runtime_spawn(&rt, 4, &client)) {
        return 1;
    }
    if (!__mc_machine_runtime_spawn(&rt, 4, &server)) {
        return 2;
    }
    if (!__mc_machine_runtime_start(&rt, client)) {
        return 3;
    }
    if (!__mc_machine_runtime_start(&rt, server)) {
        return 4;
    }

    // Completed cleanup path.
    uint64_t pending_completed = 0;
    mc_machine_envelope_t req = {.kind = 10, .payload0 = 111, .payload1 = 0};
    mc_machine_envelope_t resp = {.kind = 20, .payload0 = 222, .payload1 = 0};
    if (__mc_machine_runtime_request(&rt, client, server, &req, &pending_completed)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 5;
    }
    if (__mc_machine_runtime_reply(&rt, server, pending_completed, &resp) != MC_REPLY_OK) {
        return 6;
    }
    if (__mc_machine_runtime_pending_contains(&rt, pending_completed)) {
        return 7;
    }
    if (__mc_machine_runtime_pending_cleanup_count(&rt, MC_PENDING_CLEANUP_COMPLETED) != 1) {
        return 8;
    }
    if (state.hook_counts[MC_PENDING_CLEANUP_COMPLETED] != 1
        || state.last_pending_id != pending_completed
        || state.last_reason != MC_PENDING_CLEANUP_COMPLETED) {
        return 9;
    }

    // Drain ready queue work to keep later dispatch ordering deterministic.
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, noop_dispatch, &state)) {
        return 10;
    }

    // Requester-faulted cleanup path.
    uint64_t pending_faulted = 0;
    if (__mc_machine_runtime_request(&rt, client, server, &req, &pending_faulted)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 11;
    }
    if (!__mc_machine_runtime_mark_faulted(&rt, client)) {
        return 12;
    }
    if (__mc_machine_runtime_pending_contains(&rt, pending_faulted)) {
        return 13;
    }
    if (__mc_machine_runtime_pending_cleanup_count(&rt, MC_PENDING_CLEANUP_REQUESTER_FAULTED)
        != 1) {
        return 14;
    }
    if (state.hook_counts[MC_PENDING_CLEANUP_REQUESTER_FAULTED] != 1
        || state.last_pending_id != pending_faulted
        || state.last_reason != MC_PENDING_CLEANUP_REQUESTER_FAULTED) {
        return 15;
    }

    // Fault-policy cleanup path (runtime-driven fault transition) on a fresh
    // requester machine.
    if (!__mc_machine_runtime_spawn(&rt, 4, &client_fault_policy)) {
        return 16;
    }
    if (!__mc_machine_runtime_start(&rt, client_fault_policy)) {
        return 17;
    }
    state.fault_machine = client_fault_policy;
    state.fault_kind = 99;
    uint64_t pending_fault_policy = 0;
    if (__mc_machine_runtime_request(
            &rt,
            client_fault_policy,
            server,
            &req,
            &pending_fault_policy
        )
        != MC_MAILBOX_ENQUEUE_OK) {
        return 18;
    }
    mc_machine_envelope_t fault_evt = {.kind = 99};
    if (__mc_machine_runtime_enqueue(&rt, client_fault_policy, &fault_evt)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 19;
    }
    for (uint32_t i = 0; i < 8; i++) {
        if (__mc_machine_runtime_lifecycle(&rt, client_fault_policy) == MC_MACHINE_FAULTED) {
            break;
        }
        if (!__mc_machine_runtime_dispatch_one_txn(&rt, noop_dispatch, &state)) {
            break;
        }
    }
    if (__mc_machine_runtime_lifecycle(&rt, client_fault_policy) != MC_MACHINE_FAULTED) {
        return 20;
    }
    if (__mc_machine_runtime_pending_contains(&rt, pending_fault_policy)) {
        return 21;
    }
    if (__mc_machine_runtime_pending_cleanup_count(&rt, MC_PENDING_CLEANUP_REQUESTER_FAULTED)
        != 2) {
        return 22;
    }
    if (state.hook_counts[MC_PENDING_CLEANUP_REQUESTER_FAULTED] != 2
        || state.last_pending_id != pending_fault_policy
        || state.last_reason != MC_PENDING_CLEANUP_REQUESTER_FAULTED) {
        return 23;
    }
    state.fault_machine = 0;
    state.fault_kind = 0;

    // Timeout cleanup path on a fresh requester machine.
    if (!__mc_machine_runtime_spawn(&rt, 4, &client_timeout)) {
        return 24;
    }
    if (!__mc_machine_runtime_start(&rt, client_timeout)) {
        return 25;
    }
    __mc_machine_runtime_set_pending_timeout_steps(&rt, 1);
    if (__mc_machine_runtime_pending_timeout_steps(&rt) != 1) {
        return 26;
    }

    uint64_t pending_timeout = 0;
    if (__mc_machine_runtime_request(&rt, client_timeout, server, &req, &pending_timeout)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 27;
    }
    // One dispatch step advances time and triggers timeout cleanup.
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, noop_dispatch, &state)) {
        return 28;
    }
    if (__mc_machine_runtime_pending_contains(&rt, pending_timeout)) {
        return 29;
    }
    if (__mc_machine_runtime_pending_cleanup_count(&rt, MC_PENDING_CLEANUP_TIMEOUT) != 1) {
        return 30;
    }
    if (state.hook_counts[MC_PENDING_CLEANUP_TIMEOUT] != 1
        || state.last_pending_id != pending_timeout
        || state.last_reason != MC_PENDING_CLEANUP_TIMEOUT) {
        return 31;
    }

    if (__mc_machine_runtime_pending_created_count(&rt) != 4) {
        return 32;
    }
    if (__mc_machine_runtime_pending_len(&rt) != 0) {
        return 33;
    }

    __mc_machine_runtime_drop(&rt);
    return 0;
}
