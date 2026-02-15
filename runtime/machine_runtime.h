#ifndef MC_MACHINE_RUNTIME_H
#define MC_MACHINE_RUNTIME_H

#include <stdint.h>

// Managed typestate runtime core.
//
// V1 scope in this module:
// - machine table and lifecycle state
// - bounded per-machine FIFO mailboxes
// - global ready queue
// - deterministic single-dispatch entrypoints
// - transaction commit/rollback scaffolding for state/outbox/subscriptions
// - dead-letter and fault hook scaffolding

// Stable runtime identifier for a managed machine slot.
// IDs are 1-based (0 is reserved as invalid).
typedef uint32_t mc_machine_id_t;

typedef enum mc_machine_lifecycle {
    // Machine exists, can buffer mailbox messages, but cannot dispatch yet.
    MC_MACHINE_CREATED = 0,
    // Machine can receive and dispatch mailbox envelopes.
    MC_MACHINE_RUNNING = 1,
    // Machine encountered a fault during dispatch and is quarantined.
    MC_MACHINE_FAULTED = 2,
    // Machine is deliberately stopped and does not accept new work.
    MC_MACHINE_STOPPED = 3,
} mc_machine_lifecycle_t;

typedef enum mc_machine_fault_policy {
    // Fault transitions machine to FAULTED.
    MC_FAULT_POLICY_MARK_FAULTED = 0,
    // Fault transitions machine to STOPPED.
    MC_FAULT_POLICY_MARK_STOPPED = 1,
} mc_machine_fault_policy_t;

typedef enum mc_mailbox_enqueue_result {
    // Envelope was accepted.
    MC_MAILBOX_ENQUEUE_OK = 0,
    // Destination machine id does not exist.
    MC_MAILBOX_ENQUEUE_MACHINE_UNKNOWN = 1,
    // Destination exists but does not accept enqueues (FAULTED/STOPPED).
    MC_MAILBOX_ENQUEUE_MACHINE_NOT_RUNNING = 2,
    // Destination mailbox is full.
    MC_MAILBOX_ENQUEUE_FULL = 3,
} mc_mailbox_enqueue_result_t;

typedef enum mc_dead_letter_reason {
    // Enqueue target did not resolve to a machine.
    MC_DEAD_LETTER_UNKNOWN_MACHINE = 1,
    // Enqueue target machine is stopped.
    MC_DEAD_LETTER_STOPPED_MACHINE = 2,
    // Enqueue target machine is faulted.
    MC_DEAD_LETTER_FAULTED_MACHINE = 3,
    // Enqueue target mailbox was at capacity.
    MC_DEAD_LETTER_MAILBOX_FULL = 4,
} mc_dead_letter_reason_t;

typedef enum mc_dispatch_result {
    // Handler completed normally.
    MC_DISPATCH_OK = 0,
    // Handler failed; runtime applies fault policy.
    MC_DISPATCH_FAULT = 1,
    // Handler requested stop.
    MC_DISPATCH_STOP = 2,
} mc_dispatch_result_t;

// Internal fault code used when transactional commit preflight fails.
#define MC_FAULT_CODE_TXN_COMMIT_REJECTED 1u

// Minimal envelope used by v1 runtime tests and scheduler plumbing.
// Higher-level protocol routing metadata will extend this over time.
typedef struct mc_machine_envelope {
    // Logical payload kind/type tag.
    uint64_t kind;
    // Source machine id (0 when external/unknown).
    uint64_t src;
    // Payload slots for v1 scaffold.
    uint64_t payload0;
    uint64_t payload1;
} mc_machine_envelope_t;

// One staged outbox delivery emitted by a successful transition.
typedef struct mc_machine_outbox_effect {
    mc_machine_id_t dst;
    mc_machine_envelope_t env;
} mc_machine_outbox_effect_t;

typedef enum mc_subscription_op {
    MC_SUBSCRIPTION_ADD = 0,
    MC_SUBSCRIPTION_REMOVE = 1,
} mc_subscription_op_t;

// One staged subscription delta emitted by a successful transition.
typedef struct mc_subscription_update {
    mc_subscription_op_t op;
    mc_machine_id_t machine_id;
    uint64_t kind;
    uint64_t routing;
} mc_subscription_update_t;

// Staged transaction payload produced by transactional dispatch callback.
// Runtime commits this as one unit only when callback returns `MC_DISPATCH_OK`.
typedef struct mc_machine_dispatch_txn {
    // Whether the transition updates machine state.
    uint8_t has_next_state;
    // Next machine-local state value.
    uint64_t next_state;

    // Outbox effects to commit atomically with state/subscriptions.
    const mc_machine_outbox_effect_t *outbox;
    uint32_t outbox_len;

    // Subscription updates to commit atomically with state/outbox.
    const mc_subscription_update_t *subscriptions;
    uint32_t subscriptions_len;
} mc_machine_dispatch_txn_t;

// Called when an enqueue attempt cannot be delivered.
typedef void (*mc_dead_letter_hook_t)(
    void *ctx,
    mc_machine_id_t dst,
    mc_dead_letter_reason_t reason,
    const mc_machine_envelope_t *env
);

// Called when a dispatch transitions through fault policy.
typedef void (*mc_fault_hook_t)(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t fault_code
);

// Transactional dispatch callback executed by
// `__mc_machine_runtime_dispatch_one_txn`.
// Callback receives current machine state and can stage transaction outputs.
typedef mc_dispatch_result_t (*mc_machine_dispatch_txn_fn)(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
);

// Ring buffer mailbox for one machine.
typedef struct mc_machine_mailbox {
    // Ring buffer storage.
    mc_machine_envelope_t *items;
    // Ring buffer capacity.
    uint32_t cap;
    // Number of queued envelopes.
    uint32_t len;
    // Index of next dequeue.
    uint32_t head;
    // Guard bit to avoid duplicate ready-queue entries.
    uint8_t in_ready_queue;
} mc_machine_mailbox_t;

// One machine table slot.
typedef struct mc_machine_slot {
    // Dispatchability state.
    mc_machine_lifecycle_t lifecycle;
    // Machine-local runtime state (opaque to scheduler).
    uint64_t state_word;
    // Bounded FIFO for this machine.
    mc_machine_mailbox_t mailbox;
} mc_machine_slot_t;

// Global ready queue storing machine ids that currently have pending work.
typedef struct mc_ready_queue {
    // Ring buffer of machine ids with pending work.
    mc_machine_id_t *items;
    uint32_t cap;
    uint32_t len;
    uint32_t head;
} mc_ready_queue_t;

// One active subscription mapping.
typedef struct mc_subscription_entry {
    mc_machine_id_t machine_id;
    uint64_t kind;
    uint64_t routing;
} mc_subscription_entry_t;

// Flat subscription registry used by v1 tests/scaffold.
typedef struct mc_subscription_registry {
    mc_subscription_entry_t *entries;
    uint32_t len;
    uint32_t cap;
} mc_subscription_registry_t;

// Top-level managed runtime state.
typedef struct mc_machine_runtime {
    // Dense table of machine slots (id = index + 1).
    mc_machine_slot_t *machines;
    uint32_t machine_len;
    uint32_t machine_cap;

    // Global queue of runnable machines.
    mc_ready_queue_t ready;

    // Subscription registry.
    mc_subscription_registry_t subscriptions;

    // Fault lifecycle policy.
    mc_machine_fault_policy_t fault_policy;

    // Optional runtime hooks.
    mc_dead_letter_hook_t dead_letter_hook;
    mc_fault_hook_t fault_hook;
    void *hook_ctx;
} mc_machine_runtime_t;

// Initialize runtime state to empty.
void __mc_machine_runtime_init(mc_machine_runtime_t *rt);

// Release all runtime allocations and reset to empty.
void __mc_machine_runtime_drop(mc_machine_runtime_t *rt);

// Configure optional dead-letter and fault callbacks.
void __mc_machine_runtime_set_hooks(
    mc_machine_runtime_t *rt,
    mc_dead_letter_hook_t dead_letter_hook,
    mc_fault_hook_t fault_hook,
    void *hook_ctx
);

// Set/get runtime fault lifecycle policy.
void __mc_machine_runtime_set_fault_policy(
    mc_machine_runtime_t *rt,
    mc_machine_fault_policy_t policy
);
mc_machine_fault_policy_t __mc_machine_runtime_fault_policy(
    const mc_machine_runtime_t *rt
);

// Creates a new managed machine with the requested mailbox capacity.
// Returns 1 on success and writes the machine id to out_id.
uint8_t __mc_machine_runtime_spawn(
    mc_machine_runtime_t *rt,
    uint32_t mailbox_cap,
    mc_machine_id_t *out_id
);

// Query current lifecycle for machine id. Unknown id maps to STOPPED.
mc_machine_lifecycle_t __mc_machine_runtime_lifecycle(
    const mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
);

// Override lifecycle state for an existing machine.
void __mc_machine_runtime_set_lifecycle(
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id,
    mc_machine_lifecycle_t lifecycle
);

// Transition machine from CREATED to RUNNING.
// Returns 1 on success, 0 for unknown id or invalid lifecycle.
uint8_t __mc_machine_runtime_start(
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
);

// Set/get opaque machine-local state word.
void __mc_machine_runtime_set_state(
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id,
    uint64_t state_word
);
uint64_t __mc_machine_runtime_state(
    const mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
);

// Enqueue one envelope for destination machine.
//
// Behavior:
// - `CREATED`: accepted and buffered (not scheduled until start).
// - `RUNNING`: accepted and machine is queued once on ready queue.
// - `FAULTED`/`STOPPED`: rejected as not-running.
// - unknown machine id: rejected as unknown.
//
// Dead-letter hook is invoked for unknown/not-running/full cases when configured.
mc_mailbox_enqueue_result_t __mc_machine_runtime_enqueue(
    mc_machine_runtime_t *rt,
    mc_machine_id_t dst,
    const mc_machine_envelope_t *env
);

// Executes at most one envelope dispatch using transactional callback.
//
// Transaction model:
// - on `MC_DISPATCH_OK`: commit `(state update, outbox effects, subscription updates)` atomically.
// - on `MC_DISPATCH_FAULT`: rollback staged outputs and apply fault policy.
// - on `MC_DISPATCH_STOP`: rollback staged outputs and stop machine.
//
// Returns 1 if one envelope was dispatched, 0 if no runnable machine existed.
uint8_t __mc_machine_runtime_dispatch_one_txn(
    mc_machine_runtime_t *rt,
    mc_machine_dispatch_txn_fn dispatch,
    void *dispatch_ctx
);

// Introspection helper for tests/debugging.
uint32_t __mc_machine_runtime_ready_len(const mc_machine_runtime_t *rt);

// Introspection helper for tests/debugging.
uint32_t __mc_machine_runtime_mailbox_len(
    const mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
);

// Subscription introspection helpers for tests/debugging.
uint32_t __mc_machine_runtime_subscription_len(const mc_machine_runtime_t *rt);
uint8_t __mc_machine_runtime_subscription_contains(
    const mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id,
    uint64_t kind,
    uint64_t routing
);

#endif
