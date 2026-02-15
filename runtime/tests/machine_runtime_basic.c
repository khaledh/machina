#include <stdint.h>

#include "machine_runtime.h"

typedef struct test_ctx {
    uint32_t dispatch_count;
    uint64_t seen_machine[8];
    uint64_t seen_kind[8];

    uint32_t dead_count;
    mc_dead_letter_reason_t dead_reason[8];

    uint32_t fault_count;
    uint64_t fault_code[8];
} test_ctx_t;

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

static void fault_hook(void *ctx, mc_machine_id_t machine_id, uint64_t fault_code) {
    (void)machine_id;
    test_ctx_t *state = (test_ctx_t *)ctx;
    state->fault_code[state->fault_count++] = fault_code;
}

static mc_dispatch_result_t dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    const mc_machine_envelope_t *env,
    uint64_t *fault_code
) {
    test_ctx_t *state = (test_ctx_t *)ctx;
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
    mc_machine_runtime_t rt;
    test_ctx_t state = {0};
    __mc_machine_runtime_init(&rt);
    __mc_machine_runtime_set_hooks(&rt, dead_letter_hook, fault_hook, &state);

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

    // Start transitions CREATED -> RUNNING and schedules machines with queued work.
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

    // m1 mailbox is bounded at 2 entries.
    mc_machine_envelope_t overflow = {.kind = 4};
    if (__mc_machine_runtime_enqueue(&rt, m1, &overflow) != MC_MAILBOX_ENQUEUE_FULL) {
        return 14;
    }
    if (state.dead_count != 1 || state.dead_reason[0] != MC_DEAD_LETTER_MAILBOX_FULL) {
        return 15;
    }

    // Deterministic single-dispatch behavior with FIFO mailbox + ready queue:
    // dispatch order should be m1(kind=1), m2(kind=3), m1(kind=2).
    if (!__mc_machine_runtime_dispatch_one(&rt, dispatch, &state)) {
        return 16;
    }
    if (!__mc_machine_runtime_dispatch_one(&rt, dispatch, &state)) {
        return 17;
    }
    if (!__mc_machine_runtime_dispatch_one(&rt, dispatch, &state)) {
        return 18;
    }
    if (__mc_machine_runtime_dispatch_one(&rt, dispatch, &state)) {
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

    // Fault path marks machine as faulted and emits hook callback.
    mc_machine_envelope_t fault = {.kind = 77};
    if (__mc_machine_runtime_enqueue(&rt, m2, &fault) != MC_MAILBOX_ENQUEUE_OK) {
        return 24;
    }
    if (!__mc_machine_runtime_dispatch_one(&rt, dispatch, &state)) {
        return 25;
    }
    if (__mc_machine_runtime_lifecycle(&rt, m2) != MC_MACHINE_FAULTED) {
        return 26;
    }
    if (state.fault_count != 1 || state.fault_code[0] != 700 + m2) {
        return 27;
    }

    // Enqueue to faulted machine should be rejected and dead-lettered.
    mc_machine_envelope_t after_fault = {.kind = 88};
    if (__mc_machine_runtime_enqueue(&rt, m2, &after_fault) != MC_MAILBOX_ENQUEUE_MACHINE_NOT_RUNNING) {
        return 28;
    }
    if (state.dead_count != 2 || state.dead_reason[1] != MC_DEAD_LETTER_FAULTED_MACHINE) {
        return 29;
    }

    __mc_machine_runtime_drop(&rt);
    return 0;
}
