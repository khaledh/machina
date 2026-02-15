#include <stdint.h>
#include <stddef.h>

#include "machine_runtime.h"

// Transactional request/reply staging test.
//
// Verifies that staged request/reply effects are committed only on
// MC_DISPATCH_OK and rejected atomically on preflight failure.
typedef struct txn_reqreply_ctx {
    mc_machine_id_t src_req_ok;
    mc_machine_id_t src_req_fail;
    mc_machine_id_t src_reply_fail;

    mc_machine_id_t server;
    mc_machine_id_t server_full;
    mc_machine_id_t client;
    mc_machine_id_t client_full;

    uint64_t pending_for_reply_fail;

    mc_machine_request_effect_t req[1];
    mc_machine_reply_effect_t reply[1];

    uint32_t fault_count;
    uint64_t fault_code[8];
} txn_reqreply_ctx_t;

static void fault_hook(void *ctx, mc_machine_id_t machine_id, uint64_t fault_code) {
    (void)machine_id;
    txn_reqreply_ctx_t *state = (txn_reqreply_ctx_t *)ctx;
    state->fault_code[state->fault_count++] = fault_code;
}

static mc_dispatch_result_t txn_reqreply_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    (void)fault_code;
    txn_reqreply_ctx_t *state = (txn_reqreply_ctx_t *)ctx;

    txn->has_next_state = 0;
    txn->next_state = 0;
    txn->outbox = NULL;
    txn->outbox_len = 0;
    txn->subscriptions = NULL;
    txn->subscriptions_len = 0;
    txn->requests = NULL;
    txn->requests_len = 0;
    txn->replies = NULL;
    txn->replies_len = 0;

    // Successful staged request commit.
    if (machine_id == state->src_req_ok && env->kind == 1) {
        txn->has_next_state = 1;
        txn->next_state = current_state + 1;

        state->req[0].dst = state->server;
        state->req[0].pending_id = 9001;
        state->req[0].request_site_key = 7001;
        state->req[0].env.kind = 100;
        state->req[0].env.src = 0;
        state->req[0].env.reply_cap_id = 0;
        state->req[0].env.pending_id = 0;
        state->req[0].env.payload0 = 11;
        state->req[0].env.payload1 = 0;
        txn->requests = state->req;
        txn->requests_len = 1;
        return MC_DISPATCH_OK;
    }

    // Successful staged reply commit for pending id 9001.
    if (machine_id == state->src_req_ok && env->kind == 2) {
        txn->has_next_state = 1;
        txn->next_state = current_state + 1;

        state->reply[0].reply_cap_id = 9001;
        state->reply[0].env.kind = 200;
        state->reply[0].env.src = 0;
        state->reply[0].env.reply_cap_id = 0;
        state->reply[0].env.pending_id = 0;
        state->reply[0].env.payload0 = 22;
        state->reply[0].env.payload1 = 0;
        txn->replies = state->reply;
        txn->replies_len = 1;
        return MC_DISPATCH_OK;
    }

    // Staged request should fail preflight because server_full mailbox is full.
    if (machine_id == state->src_req_fail && env->kind == 3) {
        txn->has_next_state = 1;
        txn->next_state = current_state + 10;

        state->req[0].dst = state->server_full;
        state->req[0].pending_id = 9002;
        state->req[0].request_site_key = 7002;
        state->req[0].env.kind = 101;
        state->req[0].env.src = 0;
        state->req[0].env.reply_cap_id = 0;
        state->req[0].env.pending_id = 0;
        state->req[0].env.payload0 = 33;
        state->req[0].env.payload1 = 0;
        txn->requests = state->req;
        txn->requests_len = 1;
        return MC_DISPATCH_OK;
    }

    // Staged reply should fail preflight because requester mailbox is full.
    if (machine_id == state->src_reply_fail && env->kind == 4) {
        txn->has_next_state = 1;
        txn->next_state = current_state + 10;

        state->reply[0].reply_cap_id = state->pending_for_reply_fail;
        state->reply[0].env.kind = 201;
        state->reply[0].env.src = 0;
        state->reply[0].env.reply_cap_id = 0;
        state->reply[0].env.pending_id = 0;
        state->reply[0].env.payload0 = 44;
        state->reply[0].env.payload1 = 0;
        txn->replies = state->reply;
        txn->replies_len = 1;
        return MC_DISPATCH_OK;
    }

    return MC_DISPATCH_OK;
}

int main(void) {
    mc_machine_runtime_t rt;
    txn_reqreply_ctx_t state = {0};

    __mc_machine_runtime_init(&rt);
    __mc_machine_runtime_set_hooks(&rt, NULL, fault_hook, &state);

    if (!__mc_machine_runtime_spawn(&rt, 4, &state.src_req_ok)) {
        return 1;
    }
    if (!__mc_machine_runtime_spawn(&rt, 4, &state.src_req_fail)) {
        return 2;
    }
    if (!__mc_machine_runtime_spawn(&rt, 4, &state.src_reply_fail)) {
        return 3;
    }
    if (!__mc_machine_runtime_spawn(&rt, 2, &state.server)) {
        return 4;
    }
    if (!__mc_machine_runtime_spawn(&rt, 1, &state.server_full)) {
        return 5;
    }
    if (!__mc_machine_runtime_spawn(&rt, 2, &state.client)) {
        return 6;
    }
    if (!__mc_machine_runtime_spawn(&rt, 1, &state.client_full)) {
        return 7;
    }

    if (!__mc_machine_runtime_start(&rt, state.src_req_ok)) {
        return 8;
    }
    if (!__mc_machine_runtime_start(&rt, state.src_req_fail)) {
        return 9;
    }
    if (!__mc_machine_runtime_start(&rt, state.src_reply_fail)) {
        return 10;
    }

    __mc_machine_runtime_set_state(&rt, state.src_req_ok, 10);
    __mc_machine_runtime_set_state(&rt, state.src_req_fail, 20);
    __mc_machine_runtime_set_state(&rt, state.src_reply_fail, 30);

    // Fill server_full mailbox to force staged-request preflight failure.
    mc_machine_envelope_t fill_server_full = {.kind = 90};
    if (__mc_machine_runtime_enqueue(&rt, state.server_full, &fill_server_full)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 11;
    }

    // Seed a pending id whose requester is client_full.
    mc_machine_envelope_t req_seed = {.kind = 91};
    if (__mc_machine_runtime_request(
            &rt,
            state.client_full,
            state.server,
            &req_seed,
            &state.pending_for_reply_fail
        ) != MC_MAILBOX_ENQUEUE_OK) {
        return 12;
    }

    // Fill client_full mailbox so staged reply preflight to that requester fails.
    mc_machine_envelope_t fill_client_full = {.kind = 92};
    if (__mc_machine_runtime_enqueue(&rt, state.client_full, &fill_client_full)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 13;
    }

    // Success path: staged request commit.
    mc_machine_envelope_t step1 = {.kind = 1};
    if (__mc_machine_runtime_enqueue(&rt, state.src_req_ok, &step1) != MC_MAILBOX_ENQUEUE_OK) {
        return 14;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, txn_reqreply_dispatch, &state)) {
        return 15;
    }
    if (__mc_machine_runtime_state(&rt, state.src_req_ok) != 11) {
        return 16;
    }
    if (!__mc_machine_runtime_pending_contains(&rt, 9001)) {
        return 17;
    }
    if (!__mc_machine_runtime_pending_contains_identity(&rt, 9001, 7001)) {
        return 39;
    }
    if (__mc_machine_runtime_pending_request_site(&rt, 9001) != 7001) {
        return 40;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.server) != 2) {
        return 18;
    }

    // Success path: staged reply commit consumes pending 9001 and routes to client.
    mc_machine_envelope_t step2 = {.kind = 2};
    if (__mc_machine_runtime_enqueue(&rt, state.src_req_ok, &step2) != MC_MAILBOX_ENQUEUE_OK) {
        return 19;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, txn_reqreply_dispatch, &state)) {
        return 20;
    }
    if (__mc_machine_runtime_state(&rt, state.src_req_ok) != 12) {
        return 21;
    }
    if (__mc_machine_runtime_pending_contains(&rt, 9001)) {
        return 22;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.src_req_ok) != 1) {
        return 23;
    }
    // Drain the staged reply delivery to keep subsequent dispatch order deterministic.
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, txn_reqreply_dispatch, &state)) {
        return 37;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.src_req_ok) != 0) {
        return 38;
    }

    // Failure path: staged request preflight reject (server_full already full).
    mc_machine_envelope_t step3 = {.kind = 3};
    if (__mc_machine_runtime_enqueue(&rt, state.src_req_fail, &step3) != MC_MAILBOX_ENQUEUE_OK) {
        return 24;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, txn_reqreply_dispatch, &state)) {
        return 25;
    }
    if (__mc_machine_runtime_lifecycle(&rt, state.src_req_fail) != MC_MACHINE_FAULTED) {
        return 26;
    }
    if (__mc_machine_runtime_state(&rt, state.src_req_fail) != 20) {
        return 27;
    }
    if (__mc_machine_runtime_pending_contains(&rt, 9002)) {
        return 28;
    }
    if (__mc_machine_runtime_pending_contains_identity(&rt, 9002, 7002)) {
        return 41;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.server_full) != 1) {
        return 29;
    }

    // Failure path: staged reply preflight reject (client_full already full).
    mc_machine_envelope_t step4 = {.kind = 4};
    if (__mc_machine_runtime_enqueue(&rt, state.src_reply_fail, &step4)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 30;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, txn_reqreply_dispatch, &state)) {
        return 31;
    }
    if (__mc_machine_runtime_lifecycle(&rt, state.src_reply_fail) != MC_MACHINE_FAULTED) {
        return 32;
    }
    if (__mc_machine_runtime_state(&rt, state.src_reply_fail) != 30) {
        return 33;
    }
    if (!__mc_machine_runtime_pending_contains(&rt, state.pending_for_reply_fail)) {
        return 34;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.client_full) != 1) {
        return 35;
    }

    // Two preflight rejections should have produced two fault callbacks.
    if (state.fault_count != 2
        || state.fault_code[0] != MC_FAULT_CODE_TXN_COMMIT_REJECTED
        || state.fault_code[1] != MC_FAULT_CODE_TXN_COMMIT_REJECTED) {
        return 36;
    }

    __mc_machine_runtime_drop(&rt);
    return 0;
}
