#include <stdint.h>
#include <stddef.h>

#include "machine_runtime.h"

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
    uint64_t server_reply_caps[16];
    uint32_t server_reply_caps_len;
    uint64_t client_pending_seq[16];
    uint32_t client_pending_seq_len;

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
        if (state->client_pending_seq_len < 16) {
            state->client_pending_seq[state->client_pending_seq_len++] = env->pending_id;
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

    // Invalid/consumed cap has deterministic reply failure and dead-letter reason.
    if (__mc_machine_runtime_reply(&rt, state.server, pending, &resp) != MC_REPLY_CAP_UNKNOWN) {
        return 19;
    }
    if (state.dead_count != 1 || state.dead_reason[0] != MC_DEAD_LETTER_REPLY_CAP_UNKNOWN) {
        return 20;
    }

    // Unknown and stopped request destination paths are deterministic.
    if (__mc_machine_runtime_request(&rt, state.client, 9999, &req, &pending)
        != MC_MAILBOX_ENQUEUE_MACHINE_UNKNOWN) {
        return 21;
    }
    if (state.dead_count != 2 || state.dead_reason[1] != MC_DEAD_LETTER_UNKNOWN_MACHINE) {
        return 22;
    }

    __mc_machine_runtime_set_lifecycle(&rt, state.server, MC_MACHINE_STOPPED);
    if (__mc_machine_runtime_request(&rt, state.client, state.server, &req, &pending)
        != MC_MAILBOX_ENQUEUE_MACHINE_NOT_RUNNING) {
        return 23;
    }
    if (state.dead_count != 3 || state.dead_reason[2] != MC_DEAD_LETTER_STOPPED_MACHINE) {
        return 24;
    }

    // If requester is stopped, reply delivery fails and pending remains active.
    __mc_machine_runtime_set_lifecycle(&rt, state.server, MC_MACHINE_RUNNING);
    uint64_t pending2 = 0;
    if (__mc_machine_runtime_request(&rt, state.client, state.server, &req, &pending2)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 25;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 26;
    }

    __mc_machine_runtime_set_lifecycle(&rt, state.client, MC_MACHINE_STOPPED);
    if (__mc_machine_runtime_reply(&rt, state.server, pending2, &resp)
        != MC_REPLY_DEST_NOT_RUNNING) {
        return 27;
    }
    if (__mc_machine_runtime_pending_len(&rt) != 1) {
        return 28;
    }
    if (!__mc_machine_runtime_pending_contains(&rt, pending2)) {
        return 29;
    }

    // Restore requester and retry reply successfully.
    __mc_machine_runtime_set_lifecycle(&rt, state.client, MC_MACHINE_RUNNING);
    if (__mc_machine_runtime_reply(&rt, state.server, pending2, &resp) != MC_REPLY_OK) {
        return 30;
    }
    if (__mc_machine_runtime_pending_len(&rt) != 0) {
        return 31;
    }
    if (__mc_machine_runtime_pending_contains(&rt, pending2)) {
        return 32;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 33;
    }

    // Out-of-order response routing:
    // issue two requests, then reply in reverse order and verify client sees
    // pending ids in the same reverse order.
    state.server_reply_caps_len = 0;
    state.client_pending_seq_len = 0;

    uint64_t pending_a = 0;
    uint64_t pending_b = 0;
    if (__mc_machine_runtime_request(&rt, state.client, state.server, &req, &pending_a)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 34;
    }
    if (__mc_machine_runtime_request(&rt, state.client, state.server, &req, &pending_b)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 35;
    }
    if (pending_a == 0 || pending_b == 0 || pending_a == pending_b) {
        return 36;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 37;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 38;
    }
    if (state.server_reply_caps_len != 2
        || state.server_reply_caps[0] != pending_a
        || state.server_reply_caps[1] != pending_b) {
        return 39;
    }

    if (__mc_machine_runtime_reply(&rt, state.server, pending_b, &resp) != MC_REPLY_OK) {
        return 40;
    }
    if (__mc_machine_runtime_reply(&rt, state.server, pending_a, &resp) != MC_REPLY_OK) {
        return 41;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 42;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, reqreply_dispatch, &state)) {
        return 43;
    }
    if (state.client_pending_seq_len != 2
        || state.client_pending_seq[0] != pending_b
        || state.client_pending_seq[1] != pending_a) {
        return 44;
    }
    if (__mc_machine_runtime_pending_len(&rt) != 0) {
        return 45;
    }

    __mc_machine_runtime_drop(&rt);
    return 0;
}
