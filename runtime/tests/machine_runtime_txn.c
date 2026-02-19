#include <stdint.h>
#include <stddef.h>

#include "machine/runtime.h"
#include "machine_test_helpers.h"

// Transactional dispatch behavior test.
//
// This fixture focuses on commit semantics:
// - successful commit applies state + outbox + subscriptions
// - failed commit preflight applies none of them (rollback-like behavior)
// - fault policy affects post-fault lifecycle state
typedef struct txn_test_ctx {
    // Source machine under test and helper destinations.
    mc_machine_id_t src;
    mc_machine_id_t dst_ok;
    mc_machine_id_t dst_full;

    // Fault hook trace.
    uint32_t fault_count;
    uint64_t fault_code[8];

    // Scratch arrays reused as staged txn payload outputs.
    mc_machine_outbox_effect_t outbox[2];
    mc_subscription_update_t subs[2];
} txn_test_ctx_t;

// Records all runtime-reported faults for assertions.
static void fault_hook(void *ctx, mc_machine_id_t machine_id, uint64_t fault_code) {
    (void)machine_id;
    txn_test_ctx_t *state = (txn_test_ctx_t *)ctx;
    state->fault_code[state->fault_count++] = fault_code;
}

// Dispatch callback that emits different staged transactions by envelope kind:
// - kind=1: valid commit (state + one outbox + one subscription add)
// - kind=2: commit preflight should fail (targets already-full destination)
// - kind=3: direct handler fault
static mc_dispatch_result_t txn_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    txn_test_ctx_t *state = (txn_test_ctx_t *)ctx;

    MC_TEST_TXN_INIT(txn);

    if (env->kind == 1) {
        // Successful staged transition.
        txn->has_next_state = 1;
        txn->next_state = current_state + 1;

        state->outbox[0].dst = state->dst_ok;
        state->outbox[0].env.kind = 101;
        state->outbox[0].env.src = machine_id;
        state->outbox[0].env.payload0 = 11;
        state->outbox[0].env.payload1 = 22;
        txn->outbox = state->outbox;
        txn->outbox_len = 1;

        state->subs[0].op = MC_SUBSCRIPTION_ADD;
        state->subs[0].machine_id = machine_id;
        state->subs[0].kind = 7;
        state->subs[0].routing = 70;
        txn->subscriptions = state->subs;
        txn->subscriptions_len = 1;
        return MC_DISPATCH_OK;
    }

    if (env->kind == 2) {
        // Stage multiple outputs. Commit should fail in preflight because
        // dst_full mailbox is already at capacity.
        txn->has_next_state = 1;
        txn->next_state = current_state + 100;

        state->outbox[0].dst = state->dst_ok;
        state->outbox[0].env.kind = 201;
        state->outbox[0].env.src = machine_id;
        state->outbox[0].env.payload0 = 1;
        state->outbox[0].env.payload1 = 2;

        state->outbox[1].dst = state->dst_full;
        state->outbox[1].env.kind = 202;
        state->outbox[1].env.src = machine_id;
        state->outbox[1].env.payload0 = 3;
        state->outbox[1].env.payload1 = 4;

        txn->outbox = state->outbox;
        txn->outbox_len = 2;

        state->subs[0].op = MC_SUBSCRIPTION_ADD;
        state->subs[0].machine_id = machine_id;
        state->subs[0].kind = 8;
        state->subs[0].routing = 80;
        txn->subscriptions = state->subs;
        txn->subscriptions_len = 1;
        return MC_DISPATCH_OK;
    }

    if (env->kind == 3) {
        // Force a dispatch-level fault to test policy wiring.
        *fault_code = 333;
        return MC_DISPATCH_FAULT;
    }

    return MC_DISPATCH_OK;
}

int main(void) {
    // 1) Initialize runtime with fault hook enabled.
    mc_machine_runtime_t rt;
    txn_test_ctx_t state = {0};

    __mc_machine_runtime_init(&rt);
    __mc_machine_runtime_set_hooks(&rt, NULL, fault_hook, &state);

    // 2) Spawn one source machine and two destinations.
    //    - dst_ok accepts outbox effects
    //    - dst_full will be deliberately filled to force preflight failure
    if (!__mc_machine_runtime_spawn(&rt, 4, &state.src)) {
        return 1;
    }
    if (!__mc_machine_runtime_spawn(&rt, 2, &state.dst_ok)) {
        return 2;
    }
    if (!__mc_machine_runtime_spawn(&rt, 1, &state.dst_full)) {
        return 3;
    }

    // Only source is started. Destinations remain CREATED and can buffer
    // deliveries without entering the ready queue.
    if (!__mc_machine_runtime_start(&rt, state.src)) {
        return 4;
    }
    // Keep dst_ok and dst_full in CREATED so they can buffer but are not
    // scheduled. This keeps dispatch order deterministic for commit/rollback
    // checks.

    // Seed initial source state to make state transitions observable.
    __mc_machine_runtime_set_state(&rt, state.src, 10);

    // 3) Happy path commit: state + outbox + subscription all apply.
    mc_machine_envelope_t step1 = {.kind = 1};
    if (__mc_machine_runtime_enqueue(&rt, state.src, &step1) != MC_MAILBOX_ENQUEUE_OK) {
        return 6;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, txn_dispatch, &state)) {
        return 7;
    }

    if (__mc_machine_runtime_state(&rt, state.src) != 11) {
        return 8;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.dst_ok) != 1) {
        return 9;
    }
    if (!__mc_machine_runtime_subscription_contains(&rt, state.src, 7, 70)) {
        return 10;
    }
    if (state.fault_count != 0) {
        return 11;
    }

    // 4) Fill dst_full mailbox while CREATED so preflight to it fails.
    mc_machine_envelope_t fill = {.kind = 99};
    if (__mc_machine_runtime_enqueue(&rt, state.dst_full, &fill) != MC_MAILBOX_ENQUEUE_OK) {
        return 12;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.dst_full) != 1) {
        return 13;
    }

    // 5) Commit-reject path:
    //    callback stages state+outbox+subscription, but preflight rejects
    //    because dst_full cannot accept another envelope.
    //
    // Expected behavior:
    // - source transitions to FAULTED (default policy)
    // - source state does not change
    // - outbox effects are not partially applied
    // - subscription add is not applied
    // - fault hook receives MC_FAULT_CODE_TXN_COMMIT_REJECTED
    mc_machine_envelope_t step2 = {.kind = 2};
    if (__mc_machine_runtime_enqueue(&rt, state.src, &step2) != MC_MAILBOX_ENQUEUE_OK) {
        return 14;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, txn_dispatch, &state)) {
        return 15;
    }

    if (__mc_machine_runtime_lifecycle(&rt, state.src) != MC_MACHINE_FAULTED) {
        return 16;
    }
    if (__mc_machine_runtime_state(&rt, state.src) != 11) {
        return 17;
    }
    if (__mc_machine_runtime_mailbox_len(&rt, state.dst_ok) != 1) {
        return 18;
    }
    if (__mc_machine_runtime_subscription_contains(&rt, state.src, 8, 80)) {
        return 19;
    }
    if (state.fault_count != 1 || state.fault_code[0] != MC_FAULT_CODE_TXN_COMMIT_REJECTED) {
        return 20;
    }

    // 6) Policy path: switch fault policy to STOPPED and verify lifecycle.
    mc_machine_id_t policy_machine = 0;
    if (!__mc_machine_runtime_spawn(&rt, 1, &policy_machine)) {
        return 21;
    }
    if (!__mc_machine_runtime_start(&rt, policy_machine)) {
        return 22;
    }
    __mc_machine_runtime_set_fault_policy(&rt, MC_FAULT_POLICY_MARK_STOPPED);

    mc_machine_envelope_t step3 = {.kind = 3};
    if (__mc_machine_runtime_enqueue(&rt, policy_machine, &step3) != MC_MAILBOX_ENQUEUE_OK) {
        return 23;
    }
    if (!__mc_machine_runtime_dispatch_one_txn(&rt, txn_dispatch, &state)) {
        return 24;
    }
    if (__mc_machine_runtime_lifecycle(&rt, policy_machine) != MC_MACHINE_STOPPED) {
        return 25;
    }
    if (__mc_machine_runtime_fault_policy(&rt) != MC_FAULT_POLICY_MARK_STOPPED) {
        return 26;
    }
    if (state.fault_count != 2 || state.fault_code[1] != 333) {
        return 27;
    }

    // 7) Tear down runtime allocations.
    __mc_machine_runtime_drop(&rt);
    return 0;
}
