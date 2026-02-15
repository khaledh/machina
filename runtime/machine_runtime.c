#include "machine_runtime.h"

#include <stddef.h>

// Runtime allocator entrypoints from alloc.c.
void *__mc_alloc(size_t size, size_t align);
void *__mc_realloc(void *ptr, size_t size, size_t align);
void __mc_free(void *ptr);

// Default capacities tuned for simple v1 behavior.
#define MC_MACHINE_INITIAL_CAP 8u
#define MC_READY_INITIAL_CAP 16u
#define MC_MAILBOX_MIN_CAP 1u

// Resolve mutable machine slot from 1-based id.
static mc_machine_slot_t *mc_get_slot(mc_machine_runtime_t *rt, mc_machine_id_t machine_id) {
    if (!rt || machine_id == 0) {
        return NULL;
    }
    uint32_t index = machine_id - 1;
    if (index >= rt->machine_len) {
        return NULL;
    }
    return &rt->machines[index];
}

// Resolve immutable machine slot from 1-based id.
static const mc_machine_slot_t *mc_get_slot_const(
    const mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
) {
    if (!rt || machine_id == 0) {
        return NULL;
    }
    uint32_t index = machine_id - 1;
    if (index >= rt->machine_len) {
        return NULL;
    }
    return &rt->machines[index];
}

// Ensure ready queue backing storage can hold at least `min_cap` entries.
// Queue content is preserved in logical FIFO order.
static uint8_t mc_ready_ensure_cap(mc_ready_queue_t *q, uint32_t min_cap) {
    if (q->cap >= min_cap) {
        return 1;
    }

    uint32_t new_cap = q->cap == 0 ? MC_READY_INITIAL_CAP : q->cap;
    while (new_cap < min_cap) {
        uint32_t grown = new_cap * 2u;
        if (grown < new_cap) {
            return 0;
        }
        new_cap = grown;
    }

    mc_machine_id_t *new_items = (mc_machine_id_t *)__mc_alloc(
        (size_t)new_cap * sizeof(mc_machine_id_t),
        _Alignof(mc_machine_id_t)
    );
    if (!new_items) {
        return 0;
    }

    for (uint32_t i = 0; i < q->len; i++) {
        uint32_t src = (q->head + i) % (q->cap == 0 ? 1u : q->cap);
        new_items[i] = q->items[src];
    }

    __mc_free(q->items);
    q->items = new_items;
    q->cap = new_cap;
    q->head = 0;
    return 1;
}

// Push a machine id into the global ready queue.
static uint8_t mc_ready_push(mc_ready_queue_t *q, mc_machine_id_t id) {
    if (!mc_ready_ensure_cap(q, q->len + 1)) {
        return 0;
    }
    uint32_t tail = (q->head + q->len) % q->cap;
    q->items[tail] = id;
    q->len += 1;
    return 1;
}

// Pop one machine id from the global ready queue.
static uint8_t mc_ready_pop(mc_ready_queue_t *q, mc_machine_id_t *out_id) {
    if (q->len == 0) {
        return 0;
    }
    *out_id = q->items[q->head];
    q->head = (q->head + 1) % q->cap;
    q->len -= 1;
    return 1;
}

// Ensure machine table can hold at least `min_cap` machine slots.
// Newly exposed slots are initialized to STOPPED + empty mailbox.
static uint8_t mc_machine_ensure_cap(mc_machine_runtime_t *rt, uint32_t min_cap) {
    if (rt->machine_cap >= min_cap) {
        return 1;
    }

    uint32_t new_cap = rt->machine_cap == 0 ? MC_MACHINE_INITIAL_CAP : rt->machine_cap;
    while (new_cap < min_cap) {
        uint32_t grown = new_cap * 2u;
        if (grown < new_cap) {
            return 0;
        }
        new_cap = grown;
    }

    mc_machine_slot_t *new_slots = (mc_machine_slot_t *)__mc_realloc(
        rt->machines,
        (size_t)new_cap * sizeof(mc_machine_slot_t),
        _Alignof(mc_machine_slot_t)
    );
    if (!new_slots) {
        return 0;
    }

    for (uint32_t i = rt->machine_cap; i < new_cap; i++) {
        new_slots[i].lifecycle = MC_MACHINE_STOPPED;
        new_slots[i].mailbox.items = NULL;
        new_slots[i].mailbox.cap = 0;
        new_slots[i].mailbox.len = 0;
        new_slots[i].mailbox.head = 0;
        new_slots[i].mailbox.in_ready_queue = 0;
    }

    rt->machines = new_slots;
    rt->machine_cap = new_cap;
    return 1;
}

// Initialize one machine mailbox with bounded capacity.
static uint8_t mc_mailbox_init(mc_machine_mailbox_t *mailbox, uint32_t cap) {
    mailbox->cap = cap < MC_MAILBOX_MIN_CAP ? MC_MAILBOX_MIN_CAP : cap;
    mailbox->len = 0;
    mailbox->head = 0;
    mailbox->in_ready_queue = 0;
    mailbox->items = (mc_machine_envelope_t *)__mc_alloc(
        (size_t)mailbox->cap * sizeof(mc_machine_envelope_t),
        _Alignof(mc_machine_envelope_t)
    );
    return mailbox->items != NULL;
}

// Enqueue one envelope into mailbox ring buffer.
static uint8_t mc_mailbox_push(mc_machine_mailbox_t *mailbox, const mc_machine_envelope_t *env) {
    if (mailbox->len >= mailbox->cap) {
        return 0;
    }
    uint32_t tail = (mailbox->head + mailbox->len) % mailbox->cap;
    mailbox->items[tail] = *env;
    mailbox->len += 1;
    return 1;
}

// Dequeue one envelope from mailbox ring buffer.
static uint8_t mc_mailbox_pop(mc_machine_mailbox_t *mailbox, mc_machine_envelope_t *out_env) {
    if (mailbox->len == 0) {
        return 0;
    }
    *out_env = mailbox->items[mailbox->head];
    mailbox->head = (mailbox->head + 1) % mailbox->cap;
    mailbox->len -= 1;
    return 1;
}

// Best-effort dead-letter callback.
static void mc_emit_dead_letter(
    mc_machine_runtime_t *rt,
    mc_machine_id_t dst,
    mc_dead_letter_reason_t reason,
    const mc_machine_envelope_t *env
) {
    if (rt->dead_letter_hook) {
        rt->dead_letter_hook(rt->hook_ctx, dst, reason, env);
    }
}

// Best-effort fault callback.
static void mc_emit_fault(mc_machine_runtime_t *rt, mc_machine_id_t machine_id, uint64_t code) {
    if (rt->fault_hook) {
        rt->fault_hook(rt->hook_ctx, machine_id, code);
    }
}

// Public API: initialize empty runtime state.
void __mc_machine_runtime_init(mc_machine_runtime_t *rt) {
    rt->machines = NULL;
    rt->machine_len = 0;
    rt->machine_cap = 0;
    rt->ready.items = NULL;
    rt->ready.cap = 0;
    rt->ready.len = 0;
    rt->ready.head = 0;
    rt->dead_letter_hook = NULL;
    rt->fault_hook = NULL;
    rt->hook_ctx = NULL;
}

// Public API: free all runtime-owned buffers and reset state.
void __mc_machine_runtime_drop(mc_machine_runtime_t *rt) {
    if (!rt) {
        return;
    }
    for (uint32_t i = 0; i < rt->machine_len; i++) {
        __mc_free(rt->machines[i].mailbox.items);
        rt->machines[i].mailbox.items = NULL;
    }
    __mc_free(rt->machines);
    __mc_free(rt->ready.items);
    __mc_machine_runtime_init(rt);
}

// Public API: install hook callbacks + opaque hook context.
void __mc_machine_runtime_set_hooks(
    mc_machine_runtime_t *rt,
    mc_dead_letter_hook_t dead_letter_hook,
    mc_fault_hook_t fault_hook,
    void *hook_ctx
) {
    rt->dead_letter_hook = dead_letter_hook;
    rt->fault_hook = fault_hook;
    rt->hook_ctx = hook_ctx;
}

// Public API: create a managed machine slot in CREATED state.
// Returns new machine id via `out_id` when non-NULL.
uint8_t __mc_machine_runtime_spawn(
    mc_machine_runtime_t *rt,
    uint32_t mailbox_cap,
    mc_machine_id_t *out_id
) {
    if (!mc_machine_ensure_cap(rt, rt->machine_len + 1)) {
        return 0;
    }

    mc_machine_slot_t *slot = &rt->machines[rt->machine_len];
    if (!mc_mailbox_init(&slot->mailbox, mailbox_cap)) {
        return 0;
    }
    slot->lifecycle = MC_MACHINE_CREATED;

    rt->machine_len += 1;
    if (out_id) {
        *out_id = rt->machine_len;
    }
    return 1;
}

// Public API: lifecycle query for a machine id.
mc_machine_lifecycle_t __mc_machine_runtime_lifecycle(
    const mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
) {
    const mc_machine_slot_t *slot = mc_get_slot_const(rt, machine_id);
    if (!slot) {
        return MC_MACHINE_STOPPED;
    }
    return slot->lifecycle;
}

// Public API: lifecycle override for existing machine id.
void __mc_machine_runtime_set_lifecycle(
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id,
    mc_machine_lifecycle_t lifecycle
) {
    mc_machine_slot_t *slot = mc_get_slot(rt, machine_id);
    if (!slot) {
        return;
    }
    slot->lifecycle = lifecycle;
}

// Public API: start a CREATED machine.
// If buffered messages exist, schedule the machine once on ready queue.
uint8_t __mc_machine_runtime_start(
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
) {
    mc_machine_slot_t *slot = mc_get_slot(rt, machine_id);
    if (!slot || slot->lifecycle != MC_MACHINE_CREATED) {
        return 0;
    }
    slot->lifecycle = MC_MACHINE_RUNNING;
    if (slot->mailbox.len > 0 && !slot->mailbox.in_ready_queue) {
        if (!mc_ready_push(&rt->ready, machine_id)) {
            return 0;
        }
        slot->mailbox.in_ready_queue = 1;
    }
    return 1;
}

// Public API: enqueue envelope for destination machine.
//
// Key behavior:
// - Unknown/Stopped/Faulted destinations are rejected and optionally dead-lettered.
// - Mailbox overflow is rejected and optionally dead-lettered.
// - Successfully enqueued machines are inserted once into ready queue.
mc_mailbox_enqueue_result_t __mc_machine_runtime_enqueue(
    mc_machine_runtime_t *rt,
    mc_machine_id_t dst,
    const mc_machine_envelope_t *env
) {
    mc_machine_slot_t *slot = mc_get_slot(rt, dst);
    if (!slot) {
        mc_emit_dead_letter(rt, dst, MC_DEAD_LETTER_UNKNOWN_MACHINE, env);
        return MC_MAILBOX_ENQUEUE_MACHINE_UNKNOWN;
    }

    if (slot->lifecycle == MC_MACHINE_CREATED) {
        // CREATED machines are allowed to buffer inbound messages before start.
    } else if (slot->lifecycle != MC_MACHINE_RUNNING) {
        mc_dead_letter_reason_t reason = slot->lifecycle == MC_MACHINE_FAULTED
            ? MC_DEAD_LETTER_FAULTED_MACHINE
            : MC_DEAD_LETTER_STOPPED_MACHINE;
        mc_emit_dead_letter(rt, dst, reason, env);
        return MC_MAILBOX_ENQUEUE_MACHINE_NOT_RUNNING;
    }

    if (!mc_mailbox_push(&slot->mailbox, env)) {
        mc_emit_dead_letter(rt, dst, MC_DEAD_LETTER_MAILBOX_FULL, env);
        return MC_MAILBOX_ENQUEUE_FULL;
    }

    if (slot->lifecycle == MC_MACHINE_RUNNING && !slot->mailbox.in_ready_queue) {
        if (!mc_ready_push(&rt->ready, dst)) {
            // Ready queue push failure currently reuses FULL result.
            return MC_MAILBOX_ENQUEUE_FULL;
        }
        slot->mailbox.in_ready_queue = 1;
    }

    return MC_MAILBOX_ENQUEUE_OK;
}

// Public API: execute at most one dispatch from ready queue.
//
// Deterministic flow:
// 1. Pop next ready machine id.
// 2. Pop one envelope from that machine mailbox.
// 3. Run callback once.
// 4. Update lifecycle on callback result.
// 5. Requeue machine if it remains RUNNING and mailbox still has messages.
uint8_t __mc_machine_runtime_dispatch_one(
    mc_machine_runtime_t *rt,
    mc_machine_dispatch_fn dispatch,
    void *dispatch_ctx
) {
    mc_machine_id_t machine_id = 0;
    while (mc_ready_pop(&rt->ready, &machine_id)) {
        mc_machine_slot_t *slot = mc_get_slot(rt, machine_id);
        if (!slot) {
            continue;
        }

        slot->mailbox.in_ready_queue = 0;
        if (slot->lifecycle != MC_MACHINE_RUNNING) {
            continue;
        }

        mc_machine_envelope_t env = {0};
        if (!mc_mailbox_pop(&slot->mailbox, &env)) {
            // Stale ready-queue entry: nothing to process.
            continue;
        }

        uint64_t fault_code = 0;
        mc_dispatch_result_t result = dispatch
            ? dispatch(dispatch_ctx, machine_id, &env, &fault_code)
            : MC_DISPATCH_OK;

        if (result == MC_DISPATCH_FAULT) {
            slot->lifecycle = MC_MACHINE_FAULTED;
            mc_emit_fault(rt, machine_id, fault_code);
        } else if (result == MC_DISPATCH_STOP) {
            slot->lifecycle = MC_MACHINE_STOPPED;
        }

        if (slot->lifecycle == MC_MACHINE_RUNNING && slot->mailbox.len > 0) {
            if (mc_ready_push(&rt->ready, machine_id)) {
                slot->mailbox.in_ready_queue = 1;
            }
        }

        // Exactly one dispatch per call.
        return 1;
    }

    return 0;
}

// Public API: ready queue length (test/debug helper).
uint32_t __mc_machine_runtime_ready_len(const mc_machine_runtime_t *rt) {
    return rt ? rt->ready.len : 0;
}

// Public API: mailbox length for one machine (test/debug helper).
uint32_t __mc_machine_runtime_mailbox_len(
    const mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
) {
    const mc_machine_slot_t *slot = mc_get_slot_const(rt, machine_id);
    return slot ? slot->mailbox.len : 0;
}
