#include <stdint.h>
#include <stddef.h>

#include "machine/runtime.h"
#include "machine/internal.h"
#include "types.h"

// Payload ownership regression test.
//
// Verifies that boxed payload ownership is released exactly once across:
// - normal request -> response dispatch lifecycle
// - dead-lettered unknown reply capability
// - dead-lettered unknown destination enqueue
// - pending cleanup when requester stops
//
// This test uses allocator live-block counters as a stable signal that runtime
// payload cleanup is balanced.
typedef struct payload_ctx {
    mc_machine_id_t client;
    mc_machine_id_t server;
    uint64_t seen_reply_cap_id;
    uint64_t seen_response_payload0;
    uint64_t seen_response_origin_payload0;
} payload_ctx_t;

static uint64_t g_drop_calls_layout1 = 0;
static uint64_t g_drop_calls_layout2 = 0;
static uint64_t g_drop_calls_layout3 = 0;
static uint64_t g_drop_calls_layout4 = 0;

static void drop_layout1(void *payload_addr) {
    (void)payload_addr;
    g_drop_calls_layout1 += 1;
}

static void drop_layout2(void *payload_addr) {
    (void)payload_addr;
    g_drop_calls_layout2 += 1;
}

static void drop_layout3(void *payload_addr) {
    (void)payload_addr;
    g_drop_calls_layout3 += 1;
}

static void drop_layout4(void *payload_addr) {
    (void)payload_addr;
    g_drop_calls_layout4 += 1;
}

static mc_dispatch_result_t payload_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    (void)current_state;
    (void)fault_code;

    payload_ctx_t *state = (payload_ctx_t *)ctx;
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

    if (machine_id == state->server && env->kind == 100) {
        state->seen_reply_cap_id = env->reply_cap_id;
    } else if (machine_id == state->client && env->kind == 200) {
        state->seen_response_payload0 = env->payload0;
        state->seen_response_origin_payload0 = env->origin_payload0;
    }
    return MC_DISPATCH_OK;
}

int main(void) {
    mc_machine_runtime_t rt;
    payload_ctx_t state = {0};
    uint64_t pending = 0;
    uint64_t pending_stopped = 0;
    uint64_t pending_warmup = 0;

    __mc_machine_runtime_init(&rt);

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
    __mc_machine_runtime_register_payload_drop(1, drop_layout1);
    __mc_machine_runtime_register_payload_drop(2, drop_layout2);
    __mc_machine_runtime_register_payload_drop(3, drop_layout3);
    __mc_machine_runtime_register_payload_drop(4, drop_layout4);

    // Warm up pending-correlation table allocations so later live-block checks
    // are not affected by first-use runtime growth.
    mc_machine_envelope_t warm_req = {.kind = 300, .payload0 = 0, .payload1 = 0};
    if (__mc_machine_runtime_request(
            &rt,
            state.client,
            state.server,
            &warm_req,
            &pending_warmup
        ) != MC_MAILBOX_ENQUEUE_OK) {
        return 5;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, payload_dispatch, &state)) {
        return 6;
    }
    mc_machine_envelope_t warm_resp = {.kind = 301, .payload0 = 0, .payload1 = 0};
    if (__mc_machine_runtime_reply(&rt, state.server, pending_warmup, &warm_resp)
        != MC_REPLY_OK) {
        return 7;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, payload_dispatch, &state)) {
        return 8;
    }

    const uint64_t base_live = __mc_alloc_live_blocks();

    // Happy-path request/reply should release both boxed payload and origin
    // request payload after the response dispatch executes.
    void *req_box = __mc_alloc(sizeof(uint64_t), _Alignof(uint64_t));
    void *resp_box = __mc_alloc(sizeof(uint64_t), _Alignof(uint64_t));
    if (!req_box || !resp_box) {
        return 9;
    }

    mc_machine_envelope_t req = {
        .kind = 100,
        .payload0 = (uint64_t)(uintptr_t)req_box,
        .payload1 = MC_PAYLOAD_LAYOUT_OWNED_MASK | 1,
    };
    if (__mc_machine_runtime_request(&rt, state.client, state.server, &req, &pending)
        != MC_MAILBOX_ENQUEUE_OK) {
        return 10;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, payload_dispatch, &state)) {
        return 11;
    }
    if (state.seen_reply_cap_id != pending) {
        return 12;
    }

    mc_machine_envelope_t resp = {
        .kind = 200,
        .payload0 = (uint64_t)(uintptr_t)resp_box,
        .payload1 = MC_PAYLOAD_LAYOUT_OWNED_MASK | 2,
    };
    if (__mc_machine_runtime_reply(&rt, state.server, pending, &resp) != MC_REPLY_OK) {
        return 13;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, payload_dispatch, &state)) {
        return 14;
    }
    if (state.seen_response_payload0 != (uint64_t)(uintptr_t)resp_box) {
        return 15;
    }
    if (state.seen_response_origin_payload0 != (uint64_t)(uintptr_t)req_box) {
        return 16;
    }
    if (__mc_alloc_live_blocks() != base_live) {
        return 17;
    }
    if (g_drop_calls_layout1 != 1 || g_drop_calls_layout2 != 1) {
        return 18;
    }

    // Reply with unknown cap should dead-letter and release payload box now.
    void *bad_reply_box = __mc_alloc(sizeof(uint64_t), _Alignof(uint64_t));
    if (!bad_reply_box) {
        return 18;
    }
    mc_machine_envelope_t bad_reply = {
        .kind = 200,
        .payload0 = (uint64_t)(uintptr_t)bad_reply_box,
        .payload1 = MC_PAYLOAD_LAYOUT_OWNED_MASK | 2,
    };
    if (__mc_machine_runtime_reply(&rt, state.server, 999999, &bad_reply)
        != MC_REPLY_CAP_UNKNOWN) {
        return 19;
    }
    if (__mc_alloc_live_blocks() != base_live) {
        return 20;
    }
    if (g_drop_calls_layout2 != 2) {
        return 21;
    }

    // Enqueue to unknown destination should dead-letter and release payload box.
    void *dead_send_box = __mc_alloc(sizeof(uint64_t), _Alignof(uint64_t));
    if (!dead_send_box) {
        return 21;
    }
    mc_machine_envelope_t dead_send = {
        .kind = 123,
        .payload0 = (uint64_t)(uintptr_t)dead_send_box,
        .payload1 = MC_PAYLOAD_LAYOUT_OWNED_MASK | 3,
    };
    if (__mc_machine_runtime_enqueue(&rt, 9999, &dead_send)
        != MC_MAILBOX_ENQUEUE_MACHINE_UNKNOWN) {
        return 22;
    }
    if (__mc_alloc_live_blocks() != base_live) {
        return 23;
    }
    if (g_drop_calls_layout3 != 1) {
        return 24;
    }

    // Pending cleanup on requester stop should release retained request payload.
    void *stop_req_box = __mc_alloc(sizeof(uint64_t), _Alignof(uint64_t));
    if (!stop_req_box) {
        return 24;
    }
    mc_machine_envelope_t stop_req = {
        .kind = 100,
        .payload0 = (uint64_t)(uintptr_t)stop_req_box,
        .payload1 = MC_PAYLOAD_LAYOUT_OWNED_MASK | 4,
    };
    if (__mc_machine_runtime_request(
            &rt,
            state.client,
            state.server,
            &stop_req,
            &pending_stopped
        ) != MC_MAILBOX_ENQUEUE_OK) {
        return 25;
    }
    __mc_machine_runtime_set_lifecycle(&rt, state.client, MC_MACHINE_STOPPED);
    if (__mc_machine_runtime_pending_contains(&rt, pending_stopped)) {
        return 26;
    }
    // Stopping a machine now eagerly reclaims its mailbox allocation.
    // So one runtime-owned block disappears in addition to pending payload
    // cleanup (which should return request payload ownership to baseline).
    if (__mc_alloc_live_blocks() != (base_live - 1)) {
        return 27;
    }
    if (g_drop_calls_layout4 != 1) {
        return 28;
    }

    __mc_machine_runtime_drop(&rt);
    return 0;
}
