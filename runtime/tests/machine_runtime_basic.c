#include <stdint.h>
#include <stddef.h>

#include "machine/runtime.h"
#include "machine_test_helpers.h"

// Baseline managed-runtime behavior test.
//
// This fixture exercises:
// - lifecycle progression (CREATED -> RUNNING)
// - enqueue + bounded mailbox behavior
// - deterministic ready-queue dispatch ordering
// - fault transition and dead-letter hooks
typedef struct test_ctx {
    // Dispatch trace captured in callback order.
    uint32_t dispatch_count;
    uint64_t seen_machine[8];
    uint64_t seen_kind[8];

    // Dead-letter trace captured from enqueue rejections.
    uint32_t dead_count;
    mc_dead_letter_reason_t dead_reason[8];

    // Fault trace captured from dispatch faults.
    uint32_t fault_count;
    uint64_t fault_code[8];
} test_ctx_t;

// Records dead-letter reasons so the test can validate reject behavior.
static void dead_letter_hook(
    void *ctx,
    mc_machine_id_t dst,
    mc_dead_letter_reason_t reason,
    const mc_machine_envelope_t *env
) {
    (void)dst;
    (void)env;
    test_ctx_t *state = (test_ctx_t *)ctx;
    state->dead_reason[state->dead_count++] = reason;
}

// Records emitted fault codes so the test can validate fault transitions.
static void fault_hook(void *ctx, mc_machine_id_t machine_id, uint64_t fault_code) {
    (void)machine_id;
    test_ctx_t *state = (test_ctx_t *)ctx;
    state->fault_code[state->fault_count++] = fault_code;
}

// Transactional dispatch callback used by this test.
//
// The callback intentionally does not stage any txn outputs. It only:
// - records observed dispatch order
// - returns FAULT for kind=77
// - returns STOP for kind=99
// - returns OK for all other envelopes
static mc_dispatch_result_t dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    (void)current_state;
    test_ctx_t *state = (test_ctx_t *)ctx;
    MC_TEST_TXN_INIT(txn);
    uint32_t i = state->dispatch_count++;
    state->seen_machine[i] = machine_id;
    state->seen_kind[i] = env->kind;

    if (env->kind == 77) {
        *fault_code = 700 + machine_id;
        return MC_DISPATCH_FAULT;
    }
    if (env->kind == 99) {
        return MC_DISPATCH_STOP;
    }
    return MC_DISPATCH_OK;
}

int main(void) {
    // 1) Bootstrap runtime with both hooks enabled.
    mc_machine_runtime_t rt;
    test_ctx_t state = {0};
    __mc_machine_runtime_init(&rt);
    __mc_machine_runtime_set_hooks(&rt, dead_letter_hook, fault_hook, &state);

    // 2) Spawn two machines. Both start in CREATED.
    mc_machine_id_t m1 = 0;
    mc_machine_id_t m2 = 0;
    if (!__mc_machine_runtime_spawn(&rt, 2, &m1)) {
        return 1;
    }
    if (!__mc_machine_runtime_spawn(&rt, 2, &m2)) {
        return 2;
    }
    if (__mc_machine_runtime_lifecycle(&rt, m1) != MC_MACHINE_CREATED) {
        return 3;
    }
    if (__mc_machine_runtime_lifecycle(&rt, m2) != MC_MACHINE_CREATED) {
        return 4;
    }

    // 3) Enqueue while CREATED: accepted into mailbox, but not scheduled.
    mc_machine_envelope_t e1 = {.kind = 1};
    mc_machine_envelope_t e2 = {.kind = 2};
    mc_machine_envelope_t e3 = {.kind = 3};
    if (__mc_machine_runtime_enqueue(&rt, m1, &e1) != MC_MAILBOX_ENQUEUE_OK) {
        return 5;
    }
    if (__mc_machine_runtime_enqueue(&rt, m1, &e2) != MC_MAILBOX_ENQUEUE_OK) {
        return 6;
    }
    if (__mc_machine_runtime_enqueue(&rt, m2, &e3) != MC_MAILBOX_ENQUEUE_OK) {
        return 7;
    }
    if (__mc_machine_runtime_ready_len(&rt) != 0) {
        return 8;
    }

    // 4) Start transitions CREATED -> RUNNING and schedules queued machines.
    if (!__mc_machine_runtime_start(&rt, m1)) {
        return 9;
    }
    if (!__mc_machine_runtime_start(&rt, m2)) {
        return 10;
    }
    if (__mc_machine_runtime_lifecycle(&rt, m1) != MC_MACHINE_RUNNING) {
        return 11;
    }
    if (__mc_machine_runtime_lifecycle(&rt, m2) != MC_MACHINE_RUNNING) {
        return 12;
    }
    if (__mc_machine_runtime_ready_len(&rt) != 2) {
        return 13;
    }

    // 5) Validate bounded mailbox + dead-letter hook.
    // m1 mailbox cap is 2 and already full with e1/e2.
    mc_machine_envelope_t overflow = {.kind = 4};
    if (__mc_machine_runtime_enqueue(&rt, m1, &overflow) != MC_MAILBOX_ENQUEUE_FULL) {
        return 14;
    }
    if (state.dead_count != 1 || state.dead_reason[0] != MC_DEAD_LETTER_MAILBOX_FULL) {
        return 15;
    }

    // 6) Validate deterministic scheduling:
    //    FIFO per mailbox + global ready queue => m1(1), m2(3), m1(2).
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, dispatch, &state)) {
        return 16;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, dispatch, &state)) {
        return 17;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, dispatch, &state)) {
        return 18;
    }
    if (__mc_machine_runtime_dispatch_one_txn(&rt, dispatch, &state)) {
        return 19;
    }

    if (state.dispatch_count != 3) {
        return 20;
    }
    if (state.seen_machine[0] != m1 || state.seen_kind[0] != 1) {
        return 21;
    }
    if (state.seen_machine[1] != m2 || state.seen_kind[1] != 3) {
        return 22;
    }
    if (state.seen_machine[2] != m1 || state.seen_kind[2] != 2) {
        return 23;
    }

    // 7) Fault path: dispatch fault transitions machine to FAULTED
    //    (default fault policy) and emits fault hook.
    mc_machine_envelope_t fault = {.kind = 77};
    if (__mc_machine_runtime_enqueue(&rt, m2, &fault) != MC_MAILBOX_ENQUEUE_OK) {
        return 24;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, dispatch, &state)) {
        return 25;
    }
    if (__mc_machine_runtime_lifecycle(&rt, m2) != MC_MACHINE_FAULTED) {
        return 26;
    }
    if (state.fault_count != 1 || state.fault_code[0] != 700 + m2) {
        return 27;
    }

    // 8) Enqueue to FAULTED machine is rejected and dead-lettered.
    mc_machine_envelope_t after_fault = {.kind = 88};
    if (__mc_machine_runtime_enqueue(&rt, m2, &after_fault) != MC_MAILBOX_ENQUEUE_MACHINE_NOT_RUNNING) {
        return 28;
    }
    if (state.dead_count != 2 || state.dead_reason[1] != MC_DEAD_LETTER_FAULTED_MACHINE) {
        return 29;
    }

    // 9) Tear down runtime allocations.
    __mc_machine_runtime_drop(&rt);
    return 0;
}
