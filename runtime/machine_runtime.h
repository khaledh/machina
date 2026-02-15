#ifndef MC_MACHINE_RUNTIME_H
#define MC_MACHINE_RUNTIME_H

#include <stdint.h>

// Managed typestate runtime core.
//
// V1 scope in this module:
// - machine table and lifecycle state
// - bounded per-machine FIFO mailboxes
// - global ready queue
// - single-machine deterministic dispatch entrypoint
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

typedef enum mc_mailbox_enqueue_result {
    // Envelope was accepted.
    MC_MAILBOX_ENQUEUE_OK = 0,
    // Destination machine id does not exist.
    MC_MAILBOX_ENQUEUE_MACHINE_UNKNOWN = 1,
    // Destination exists but is not dispatchable (`FAULTED` or `STOPPED`).
    MC_MAILBOX_ENQUEUE_MACHINE_NOT_RUNNING = 2,
    // Destination mailbox is full.
    MC_MAILBOX_ENQUEUE_FULL = 3,
} mc_mailbox_enqueue_result_t;

typedef enum mc_dead_letter_reason {
    MC_DEAD_LETTER_UNKNOWN_MACHINE = 1,
    MC_DEAD_LETTER_STOPPED_MACHINE = 2,
    MC_DEAD_LETTER_FAULTED_MACHINE = 3,
    MC_DEAD_LETTER_MAILBOX_FULL = 4,
} mc_dead_letter_reason_t;

typedef enum mc_dispatch_result {
    // Handler completed normally.
    MC_DISPATCH_OK = 0,
    // Handler failed; runtime should mark machine faulted.
    MC_DISPATCH_FAULT = 1,
    // Handler requested stop; runtime should mark machine stopped.
    MC_DISPATCH_STOP = 2,
} mc_dispatch_result_t;

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

// Called when an enqueue attempt cannot be delivered.
typedef void (*mc_dead_letter_hook_t)(
    void *ctx,
    mc_machine_id_t dst,
    mc_dead_letter_reason_t reason,
    const mc_machine_envelope_t *env
);

// Called when a dispatch reports `MC_DISPATCH_FAULT`.
typedef void (*mc_fault_hook_t)(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t fault_code
);

// Machine dispatch callback executed by `__mc_machine_runtime_dispatch_one`.
// The callback processes exactly one envelope for one machine.
typedef mc_dispatch_result_t (*mc_machine_dispatch_fn)(
    void *ctx,
    mc_machine_id_t machine_id,
    const mc_machine_envelope_t *env,
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

// Top-level managed runtime state.
typedef struct mc_machine_runtime {
    // Dense table of machine slots (id = index + 1).
    mc_machine_slot_t *machines;
    uint32_t machine_len;
    uint32_t machine_cap;

    // Global queue of runnable machines.
    mc_ready_queue_t ready;

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

// Executes at most one envelope dispatch. Returns 1 if one envelope was
// dispatched, 0 if no runnable machine existed.
uint8_t __mc_machine_runtime_dispatch_one(
    mc_machine_runtime_t *rt,
    mc_machine_dispatch_fn dispatch,
    void *dispatch_ctx
);

// Introspection helper for tests/debugging.
uint32_t __mc_machine_runtime_ready_len(const mc_machine_runtime_t *rt);

// Introspection helper for tests/debugging.
uint32_t __mc_machine_runtime_mailbox_len(
    const mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
);

#endif
