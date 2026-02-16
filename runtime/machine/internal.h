#ifndef MC_MACHINE_INTERNAL_H
#define MC_MACHINE_INTERNAL_H

#include "runtime.h"

#include <stddef.h>

// Runtime allocator entrypoints from alloc.c.
void *__mc_alloc(size_t size, size_t align);
void *__mc_realloc(void *ptr, size_t size, size_t align);
void __mc_free(void *ptr);

// Default capacities tuned for simple v1 behavior.
#define MC_MACHINE_INITIAL_CAP 8u
#define MC_READY_INITIAL_CAP 16u
#define MC_SUBS_INITIAL_CAP 16u
#define MC_PENDING_INITIAL_CAP 16u
#define MC_MAILBOX_MIN_CAP 1u

// Slot resolution helpers.
mc_machine_slot_t *mc_get_slot(mc_machine_runtime_t *rt, mc_machine_id_t machine_id);
const mc_machine_slot_t *mc_get_slot_const(
    const mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
);

// Ready queue helpers.
uint8_t mc_ready_ensure_cap(mc_ready_queue_t *q, uint32_t min_cap);
uint8_t mc_ready_push(mc_ready_queue_t *q, mc_machine_id_t id);

// Mailbox helpers.
uint8_t mc_mailbox_init(mc_machine_mailbox_t *mailbox, uint32_t cap);
uint8_t mc_mailbox_push(mc_machine_mailbox_t *mailbox, const mc_machine_envelope_t *env);
uint8_t mc_mailbox_pop(mc_machine_mailbox_t *mailbox, mc_machine_envelope_t *out_env);

// Subscription helpers.
uint8_t mc_subscription_ensure_cap(mc_subscription_registry_t *subs, uint32_t min_cap);
uint8_t mc_subscription_contains(
    const mc_subscription_registry_t *subs,
    mc_machine_id_t machine_id,
    mc_machine_event_kind_t kind,
    uint64_t routing
);
void mc_subscription_remove(
    mc_subscription_registry_t *subs,
    mc_machine_id_t machine_id,
    mc_machine_event_kind_t kind,
    uint64_t routing
);
uint8_t mc_subscription_matches(
    const mc_subscription_entry_t *entry,
    mc_machine_id_t machine_id,
    mc_machine_event_kind_t kind,
    uint64_t routing
);

// Hook emitters.
void mc_emit_dead_letter(
    mc_machine_runtime_t *rt,
    mc_machine_id_t dst,
    mc_dead_letter_reason_t reason,
    const mc_machine_envelope_t *env
);
void mc_emit_fault(mc_machine_runtime_t *rt, mc_machine_id_t machine_id, uint64_t code);
void mc_apply_fault_policy(
    mc_machine_runtime_t *rt,
    mc_machine_slot_t *slot,
    mc_machine_id_t machine_id,
    uint64_t fault_code
);

// Internal dispatch implementation with fault metadata output.
uint8_t mc_machine_runtime_dispatch_one_txn_impl(
    mc_machine_runtime_t *rt,
    mc_machine_dispatch_txn_fn dispatch,
    void *dispatch_ctx,
    uint8_t *out_faulted,
    mc_machine_id_t *out_fault_machine,
    uint64_t *out_fault_code
);

#endif
