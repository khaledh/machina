#include <stdint.h>
#include <stddef.h>

#include "machine/runtime.h"

typedef struct reqreply_ctx {
    mc_machine_runtime_t *rt;
    mc_machine_id_t client;
    mc_machine_id_t server;

    uint32_t server_dispatch_count;
    uint32_t client_dispatch_count;

    uint64_t last_reply_cap;
    uint64_t last_pending_id;
    uint64_t last_request_src;
    uint64_t last_response_src;
    uint64_t last_response_origin_payload0;
    uint64_t server_reply_caps[16];
    uint32_t server_reply_caps_len;
    uint64_t client_pending_seq[16];
    uint32_t client_pending_seq_len;
    uint64_t client_origin_payload_seq[16];
    uint32_t client_origin_payload_seq_len;

    uint32_t dead_count;
    mc_dead_letter_reason_t dead_reason[16];
} reqreply_ctx_t;

static void dead_letter_hook(
    void *ctx,
    mc_machine_id_t dst,
    mc_dead_letter_reason_t reason,
    const mc_machine_envelope_t *env
) {
    (void)dst;
    (void)env;
    reqreply_ctx_t *state = (reqreply_ctx_t *)ctx;
    state->dead_reason[state->dead_count++] = reason;
}

static mc_dispatch_result_t reqreply_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    (void)current_state;
    (void)fault_code;

    reqreply_ctx_t *state = (reqreply_ctx_t *)ctx;
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

    if (machine_id == state->server && env->kind == 100) {
        state->server_dispatch_count += 1;
        state->last_reply_cap = env->reply_cap_id;
        state->last_request_src = env->src;
        if (state->server_reply_caps_len < 16) {
            state->server_reply_caps[state->server_reply_caps_len++] = env->reply_cap_id;
        }
    } else if (machine_id == state->client && env->kind == 200) {
        state->client_dispatch_count += 1;
        state->last_pending_id = env->pending_id;
        state->last_response_src = env->src;
        state->last_response_origin_payload0 = env->origin_payload0;
        if (state->client_pending_seq_len < 16) {
            state->client_pending_seq[state->client_pending_seq_len++] = env->pending_id;
        }
        if (state->client_origin_payload_seq_len < 16) {
            state->client_origin_payload_seq[state->client_origin_payload_seq_len++] =
                env->origin_payload0;
        }
    }

    return MC_DISPATCH_OK;
}

int main(void) {
    mc_machine_runtime_t rt;
    reqreply_ctx_t state = {0};
    state.rt = &rt;

    __mc_machine_runtime_init(&rt);
    __mc_machine_runtime_set_hooks(&rt, dead_letter_hook, NULL, &state);

    if (!__mc_machine_runtime_spawn(&rt, 4, &state.client)) {
        return 1;
    }
    if (!__mc_machine_runtime_spawn(&rt, 4, &state.server)) {
        return 2;
    }
    if (!__mc_machine_runtime_start(&rt, state.client)) {
        return 3;
    }
    if (!__mc_machine_runtime_start(&rt, state.server)) {
        return 4;
    }

    // Happy path request: runtime mints pending/reply-cap id and enqueues request.
    uint64_t pending = 0;
    mc_machine_envelope_t req = {.kind = 100, .payload0 = 55};
    if (__mc_machine_runtime_request(&rt, state.client, state.server, &req, &pending)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 5;
    }
    if (__mc_machine_runtime_pending_len(&rt) != 1) {
        return 6;
    }
    if (pending == 0 || !__mc_machine_runtime_pending_contains(&rt, pending)) {
        return 7;
    }
    if (!__mc_machine_runtime_pending_contains_identity(&rt, pending, 0)) {
        return 8;
    }
    if (__mc_machine_runtime_pending_request_site(&rt, pending) != 0) {
        return 9;
    }

    // Dispatch server request handler and observe minted reply capability id.
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 10;
    }
    if (state.server_dispatch_count != 1) {
        return 11;
    }
    if (state.last_reply_cap != pending || state.last_request_src != state.client) {
        return 12;
    }

    // Happy path reply: route by reply-cap back to requester as Response(pending,...).
    mc_machine_envelope_t resp = {.kind = 200, .payload0 = 88};
    if (__mc_machine_runtime_reply(&rt, state.server, pending, &resp) != MC_REPLY_OK) {
        return 13;
    }
    if (__mc_machine_runtime_pending_len(&rt) != 0) {
        return 14;
    }
    if (__mc_machine_runtime_pending_contains(&rt, pending)) {
        return 15;
    }

    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 16;
    }
    if (state.client_dispatch_count != 1) {
        return 17;
    }
    if (state.last_pending_id != pending || state.last_response_src != state.server) {
        return 18;
    }
    if (state.last_response_origin_payload0 != req.payload0) {
        return 19;
    }

    // Invalid/consumed cap has deterministic reply failure and dead-letter reason.
    if (__mc_machine_runtime_reply(&rt, state.server, pending, &resp) != MC_REPLY_CAP_UNKNOWN) {
        return 20;
    }
    if (state.dead_count != 1 || state.dead_reason[0] != MC_DEAD_LETTER_REPLY_CAP_UNKNOWN) {
        return 21;
    }

    // Unknown and stopped request destination paths are deterministic.
    if (__mc_machine_runtime_request(&rt, state.client, 9999, &req, &pending)
        != MC_MAILBOX_ENQUEUE_MACHINE_UNKNOWN) {
        return 22;
    }
    if (state.dead_count != 2 || state.dead_reason[1] != MC_DEAD_LETTER_UNKNOWN_MACHINE) {
        return 23;
    }

    if (!__mc_machine_runtime_stop(&rt, state.server)) {
        return 24;
    }
    if (__mc_machine_runtime_request(&rt, state.client, state.server, &req, &pending)
        != MC_MAILBOX_ENQUEUE_MACHINE_NOT_RUNNING) {
        return 25;
    }
    if (state.dead_count != 3 || state.dead_reason[2] != MC_DEAD_LETTER_STOPPED_MACHINE) {
        return 26;
    }

    // Continue scenario with a fresh running server machine.
    if (!__mc_machine_runtime_spawn(&rt, 4, &state.server)) {
        return 27;
    }
    if (!__mc_machine_runtime_start(&rt, state.server)) {
        return 28;
    }

    // If requester is stopped, inflight pending ids are reclaimed immediately.
    uint64_t pending2 = 0;
    if (__mc_machine_runtime_request(&rt, state.client, state.server, &req, &pending2)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 29;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 30;
    }

    if (!__mc_machine_runtime_stop(&rt, state.client)) {
        return 31;
    }
    if (__mc_machine_runtime_pending_len(&rt) != 0) {
        return 32;
    }
    if (__mc_machine_runtime_pending_contains(&rt, pending2)) {
        return 33;
    }
    if (__mc_machine_runtime_reply(&rt, state.server, pending2, &resp) != MC_REPLY_CAP_UNKNOWN) {
        return 34;
    }
    if (state.dead_count != 4 || state.dead_reason[3] != MC_DEAD_LETTER_REPLY_CAP_UNKNOWN) {
        return 35;
    }

    // Continue scenario with a fresh running requester.
    if (!__mc_machine_runtime_spawn(&rt, 4, &state.client)) {
        return 36;
    }
    if (!__mc_machine_runtime_start(&rt, state.client)) {
        return 37;
    }
    if (__mc_machine_runtime_pending_len(&rt) != 0) {
        return 38;
    }
    if (__mc_machine_runtime_pending_contains(&rt, pending2)) {
        return 39;
    }

    // Out-of-order response routing:
    // issue two requests, then reply in reverse order and verify client sees
    // pending ids in the same reverse order.
    state.server_reply_caps_len = 0;
    state.client_pending_seq_len = 0;
    state.client_origin_payload_seq_len = 0;

    uint64_t pending_a = 0;
    uint64_t pending_b = 0;
    mc_machine_envelope_t req_a = {.kind = 100, .payload0 = 501};
    mc_machine_envelope_t req_b = {.kind = 100, .payload0 = 902};
    if (__mc_machine_runtime_request(&rt, state.client, state.server, &req_a, &pending_a)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 40;
    }
    if (__mc_machine_runtime_request(&rt, state.client, state.server, &req_b, &pending_b)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 41;
    }
    if (pending_a == 0 || pending_b == 0 || pending_a == pending_b) {
        return 42;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 43;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 44;
    }
    if (state.server_reply_caps_len != 2
        || state.server_reply_caps[0] != pending_a
        || state.server_reply_caps[1] != pending_b) {
        return 45;
    }

    if (__mc_machine_runtime_reply(&rt, state.server, pending_b, &resp) != MC_REPLY_OK) {
        return 46;
    }
    if (__mc_machine_runtime_reply(&rt, state.server, pending_a, &resp) != MC_REPLY_OK) {
        return 47;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 48;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 49;
    }
    if (state.client_pending_seq_len != 2
        || state.client_pending_seq[0] != pending_b
        || state.client_pending_seq[1] != pending_a) {
        return 50;
    }
    if (state.client_origin_payload_seq_len != 2
        || state.client_origin_payload_seq[0] != req_b.payload0
        || state.client_origin_payload_seq[1] != req_a.payload0) {
        return 51;
    }
    if (__mc_machine_runtime_pending_len(&rt) != 0) {
        return 47;
    }
    if (__mc_machine_runtime_pending_created_count(&rt) != 4) {
        return 48;
    }
    if (__mc_machine_runtime_pending_cleanup_count(&rt, MC_PENDING_CLEANUP_COMPLETED) != 3) {
        return 49;
    }
    if (__mc_machine_runtime_pending_cleanup_count(&rt, MC_PENDING_CLEANUP_REQUESTER_STOPPED)
        != 1) {
        return 50;
    }
    if (__mc_machine_runtime_pending_cleanup_count(&rt, MC_PENDING_CLEANUP_REQUESTER_FAULTED)
        != 0) {
        return 51;
    }
    if (__mc_machine_runtime_pending_cleanup_count(&rt, MC_PENDING_CLEANUP_TIMEOUT) != 0) {
        return 52;
    }

    __mc_machine_runtime_drop(&rt);
    return 0;
}
