#include <stdint.h>
#include <stdio.h>

#include "machine_runtime.h"

// End-to-end test for compiler ABI shims:
// - __mc_machine_emit_send
// - __mc_machine_emit_request
// - __mc_machine_emit_reply
//
// The test drives these shims through transactional dispatch and validates:
// 1) successful send/request/reply effects are committed,
// 2) FAULT dispatch rolls back staged effects,
// 3) request/reply correlation ids flow through pending/reply-cap routing.
typedef struct emit_test_ctx {
    mc_machine_id_t src_send;
    mc_machine_id_t src_fail;
    mc_machine_id_t src_req;
    mc_machine_id_t dst_send;
    mc_machine_id_t dst_server;

    uint8_t saw_send;
    uint64_t send_src;
    uint64_t send_payload0;
    uint64_t send_payload1;

    uint8_t saw_request;
    uint64_t req_src;
    uint64_t req_payload0;
    uint64_t req_payload1;
    uint64_t req_reply_cap;

    uint8_t saw_response;
    uint64_t resp_src;
    uint64_t resp_payload0;
    uint64_t resp_payload1;
    uint64_t resp_pending_id;

    uint64_t minted_pending_id;

    uint32_t fault_count;
    mc_machine_id_t fault_machine;
    uint64_t fault_code;
} emit_test_ctx_t;

static void fault_hook(void *ctx, mc_machine_id_t machine_id, uint64_t fault_code) {
    emit_test_ctx_t *state = (emit_test_ctx_t *)ctx;
    state->fault_count += 1;
    state->fault_machine = machine_id;
    state->fault_code = fault_code;
}

static mc_dispatch_result_t emit_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    emit_test_ctx_t *state = (emit_test_ctx_t *)ctx;
    (void)current_state;
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

    if (machine_id == state->src_send && env->kind == 1) {
        if (!__mc_machine_emit_send(state->dst_send, 11, 22)) {
            *fault_code = 10;
            return MC_DISPATCH_FAULT;
        }
        txn->has_next_state = 1;
        txn->next_state = current_state + 1;
        return MC_DISPATCH_OK;
    }

    if (machine_id == state->src_fail && env->kind == 2) {
        if (!__mc_machine_emit_send(state->dst_send, 33, 44)) {
            *fault_code = 20;
            return MC_DISPATCH_FAULT;
        }
        // Fault after staging send: runtime must roll this effect back.
        *fault_code = 777;
        return MC_DISPATCH_FAULT;
    }

    if (machine_id == state->src_req && env->kind == 3) {
        uint64_t pending = __mc_machine_emit_request(state->dst_server, 55, 66);
        if (pending == 0) {
            *fault_code = 30;
            return MC_DISPATCH_FAULT;
        }
        state->minted_pending_id = pending;
        txn->has_next_state = 1;
        txn->next_state = current_state + 1;
        return MC_DISPATCH_OK;
    }

    if (machine_id == state->dst_send) {
        state->saw_send = 1;
        state->send_src = env->src;
        state->send_payload0 = env->payload0;
        state->send_payload1 = env->payload1;
        return MC_DISPATCH_OK;
    }

    if (machine_id == state->dst_server && env->reply_cap_id != 0) {
        state->saw_request = 1;
        state->req_src = env->src;
        state->req_payload0 = env->payload0;
        state->req_payload1 = env->payload1;
        state->req_reply_cap = env->reply_cap_id;
        if (!__mc_machine_emit_reply(env->reply_cap_id, 77, 88)) {
            *fault_code = 40;
            return MC_DISPATCH_FAULT;
        }
        return MC_DISPATCH_OK;
    }

    if (machine_id == state->src_req && env->pending_id != 0) {
        state->saw_response = 1;
        state->resp_src = env->src;
        state->resp_payload0 = env->payload0;
        state->resp_payload1 = env->payload1;
        state->resp_pending_id = env->pending_id;
        return MC_DISPATCH_OK;
    }

    return MC_DISPATCH_OK;
}

int main(void) {
    // ABI guardrail: emit shims must fail when no dispatch context is active.
    if (__mc_machine_emit_send(1, 0, 0) != 0) {
        return 1;
    }
    if (__mc_machine_emit_request(1, 0, 0) != 0) {
        return 1;
    }
    if (__mc_machine_emit_reply(1, 0, 0) != 0) {
        return 1;
    }

    mc_machine_runtime_t rt;
    emit_test_ctx_t state = {0};
    __mc_machine_runtime_init(&rt);
    __mc_machine_runtime_set_hooks(&rt, NULL, fault_hook, &state);

    if (!__mc_machine_runtime_spawn(&rt, 4, &state.src_send)) {
        return 1;
    }
    if (!__mc_machine_runtime_spawn(&rt, 4, &state.src_fail)) {
        return 1;
    }
    if (!__mc_machine_runtime_spawn(&rt, 4, &state.src_req)) {
        return 1;
    }
    if (!__mc_machine_runtime_spawn(&rt, 4, &state.dst_send)) {
        return 1;
    }
    if (!__mc_machine_runtime_spawn(&rt, 4, &state.dst_server)) {
        return 1;
    }

    if (!__mc_machine_runtime_start(&rt, state.src_send)) {
        return 1;
    }
    if (!__mc_machine_runtime_start(&rt, state.src_fail)) {
        return 1;
    }
    if (!__mc_machine_runtime_start(&rt, state.src_req)) {
        return 1;
    }
    if (!__mc_machine_runtime_start(&rt, state.dst_send)) {
        return 1;
    }
    if (!__mc_machine_runtime_start(&rt, state.dst_server)) {
        return 1;
    }

    __mc_machine_runtime_set_state(&rt, state.src_send, 10);
    __mc_machine_runtime_set_state(&rt, state.src_req, 20);
    __mc_machine_runtime_set_state(&rt, state.src_fail, 30);

    // Commit path: src_send emits one staged send to dst_send.
    mc_machine_envelope_t e_send = { .kind = 1 };
    if (__mc_machine_runtime_enqueue(&rt, state.src_send, &e_send) != MC_MAILBOX_ENQUEUE_OK) {
        return 1;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, emit_dispatch, &state)) {
        return 1;
    }
    if (__mc_machine_runtime_state(&rt, state.src_send) != 11) {
        return 1;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.dst_send) != 1) {
        return 1;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, emit_dispatch, &state)) {
        return 1;
    }
    if (!state.saw_send || state.send_src != state.src_send || state.send_payload0 != 11
        || state.send_payload1 != 22) {
        return 1;
    }

    // Rollback path: src_fail stages send then faults. No delivery should occur.
    mc_machine_envelope_t e_fail = { .kind = 2 };
    if (__mc_machine_runtime_enqueue(&rt, state.src_fail, &e_fail) != MC_MAILBOX_ENQUEUE_OK) {
        return 1;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, emit_dispatch, &state)) {
        return 1;
    }
    if (__mc_machine_runtime_lifecycle(&rt, state.src_fail) != MC_MACHINE_FAULTED) {
        return 1;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.dst_send) != 0) {
        return 1;
    }
    if (state.fault_count != 1 || state.fault_machine != state.src_fail || state.fault_code != 777) {
        return 1;
    }

    // Request/reply path: src_req stages request, dst_server replies by cap.
    mc_machine_envelope_t e_req = { .kind = 3 };
    if (__mc_machine_runtime_enqueue(&rt, state.src_req, &e_req) != MC_MAILBOX_ENQUEUE_OK) {
        return 1;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, emit_dispatch, &state)) {
        return 1;
    }
    if (state.minted_pending_id == 0 || !__mc_machine_runtime_pending_contains(&rt, state.minted_pending_id)) {
        return 1;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.dst_server) != 1) {
        return 1;
    }

    if (!__mc_machine_runtime_dispatch_one_txn(&rt, emit_dispatch, &state)) {
        return 1;
    }
    if (!state.saw_request || state.req_src != state.src_req || state.req_payload0 != 55
        || state.req_payload1 != 66 || state.req_reply_cap != state.minted_pending_id) {
        return 1;
    }
    if (__mc_machine_runtime_pending_contains(&rt, state.minted_pending_id)) {
        return 1;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.src_req) != 1) {
        return 1;
    }

    if (!__mc_machine_runtime_dispatch_one_txn(&rt, emit_dispatch, &state)) {
        return 1;
    }
    if (!state.saw_response || state.resp_src != state.dst_server || state.resp_payload0 != 77
        || state.resp_payload1 != 88 || state.resp_pending_id != state.minted_pending_id) {
        return 1;
    }

    __mc_machine_runtime_drop(&rt);
    return 0;
}
