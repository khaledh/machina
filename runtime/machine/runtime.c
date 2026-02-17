#include "runtime.h"
#include "internal.h"
#include "pending.h"
#include "emit.h"
#include "descriptor.h"

#include <string.h>

static void mc_release_payload_word(uint64_t payload0, mc_payload_layout_id_t payload1) {
    if (payload0 == 0 || !mc_payload_layout_is_owned(payload1)) {
        return;
    }
    mc_payload_layout_id_t layout_id = mc_payload_layout_id(payload1);
    mc_payload_drop_fn drop_fn = __mc_machine_runtime_lookup_payload_drop(layout_id);
    if (drop_fn) {
        drop_fn((void *)(uintptr_t)payload0);
    }
    __mc_free((void *)(uintptr_t)payload0);
}

static void mc_release_envelope_payloads(
    const mc_machine_envelope_t *env,
    uint8_t release_payload,
    uint8_t release_origin
) {
    if (!env) {
        return;
    }

    if (release_payload) {
        mc_release_payload_word(env->payload0, env->payload1);
    }
    if (release_origin) {
        // If both words alias the same boxed payload, free exactly once.
        if (release_payload && env->origin_payload0 == env->payload0
            && env->origin_payload1 == env->payload1) {
            return;
        }
        mc_release_payload_word(env->origin_payload0, env->origin_payload1);
    }
}

static uint8_t mc_envelope_is_request(const mc_machine_envelope_t *env) {
    return env && env->reply_cap_id != 0 && env->pending_id == 0;
}

static uint8_t mc_envelope_is_response(const mc_machine_envelope_t *env) {
    return env && env->pending_id != 0;
}

// Resolve mutable machine slot from 1-based id.
mc_machine_slot_t *mc_get_slot(mc_machine_runtime_t *rt, mc_machine_id_t machine_id) {
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
const mc_machine_slot_t *mc_get_slot_const(
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
uint8_t mc_ready_ensure_cap(mc_ready_queue_t *q, uint32_t min_cap) {
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
uint8_t mc_ready_push(mc_ready_queue_t *q, mc_machine_id_t id) {
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
        new_slots[i].state_word = 0;
        new_slots[i].state_tag = 0;
        new_slots[i].descriptor = NULL;
        new_slots[i].dispatch = NULL;
        new_slots[i].dispatch_ctx = NULL;
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
uint8_t mc_mailbox_init(mc_machine_mailbox_t *mailbox, uint32_t cap) {
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
uint8_t mc_mailbox_push(mc_machine_mailbox_t *mailbox, const mc_machine_envelope_t *env) {
    if (mailbox->len >= mailbox->cap) {
        return 0;
    }
    uint32_t tail = (mailbox->head + mailbox->len) % mailbox->cap;
    mailbox->items[tail] = *env;
    mailbox->len += 1;
    return 1;
}

// Dequeue one envelope from mailbox ring buffer.
uint8_t mc_mailbox_pop(mc_machine_mailbox_t *mailbox, mc_machine_envelope_t *out_env) {
    if (mailbox->len == 0) {
        return 0;
    }
    *out_env = mailbox->items[mailbox->head];
    mailbox->head = (mailbox->head + 1) % mailbox->cap;
    mailbox->len -= 1;
    return 1;
}

// Subscription helpers ------------------------------------------------------

uint8_t mc_subscription_ensure_cap(mc_subscription_registry_t *subs, uint32_t min_cap) {
    if (subs->cap >= min_cap) {
        return 1;
    }

    uint32_t new_cap = subs->cap == 0 ? MC_SUBS_INITIAL_CAP : subs->cap;
    while (new_cap < min_cap) {
        uint32_t grown = new_cap * 2u;
        if (grown < new_cap) {
            return 0;
        }
        new_cap = grown;
    }

    mc_subscription_entry_t *new_entries = (mc_subscription_entry_t *)__mc_realloc(
        subs->entries,
        (size_t)new_cap * sizeof(mc_subscription_entry_t),
        _Alignof(mc_subscription_entry_t)
    );
    if (!new_entries) {
        return 0;
    }

    subs->entries = new_entries;
    subs->cap = new_cap;
    return 1;
}

uint8_t mc_subscription_matches(
    const mc_subscription_entry_t *entry,
    mc_machine_id_t machine_id,
    mc_machine_event_kind_t kind,
    uint64_t routing
) {
    return entry->machine_id == machine_id && entry->kind == kind && entry->routing == routing;
}

uint8_t mc_subscription_contains(
    const mc_subscription_registry_t *subs,
    mc_machine_id_t machine_id,
    mc_machine_event_kind_t kind,
    uint64_t routing
) {
    for (uint32_t i = 0; i < subs->len; i++) {
        if (mc_subscription_matches(&subs->entries[i], machine_id, kind, routing)) {
            return 1;
        }
    }
    return 0;
}

void mc_subscription_remove(
    mc_subscription_registry_t *subs,
    mc_machine_id_t machine_id,
    mc_machine_event_kind_t kind,
    uint64_t routing
) {
    uint32_t write = 0;
    for (uint32_t read = 0; read < subs->len; read++) {
        if (mc_subscription_matches(&subs->entries[read], machine_id, kind, routing)) {
            continue;
        }
        if (write != read) {
            subs->entries[write] = subs->entries[read];
        }
        write += 1;
    }
    subs->len = write;
}

// Remove all subscriptions registered by one machine.
static void mc_subscription_remove_machine(
    mc_subscription_registry_t *subs,
    mc_machine_id_t machine_id
) {
    uint32_t write = 0;
    for (uint32_t read = 0; read < subs->len; read++) {
        if (subs->entries[read].machine_id == machine_id) {
            continue;
        }
        if (write != read) {
            subs->entries[write] = subs->entries[read];
        }
        write += 1;
    }
    subs->len = write;
}

// Transition one machine into STOPPED and eagerly release per-machine runtime
// resources. The machine id remains reserved, so stale handles consistently
// observe NotRunning instead of Unknown.
static void mc_stop_machine_and_cleanup_slot(
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id,
    mc_machine_slot_t *slot
) {
    if (!rt || !slot) {
        return;
    }

    slot->lifecycle = MC_MACHINE_STOPPED;
    (void)mc_pending_cleanup_requester(rt, machine_id, MC_PENDING_CLEANUP_REQUESTER_STOPPED);
    mc_subscription_remove_machine(&rt->subscriptions, machine_id);

    mc_machine_mailbox_t *mailbox = &slot->mailbox;
    for (uint32_t j = 0; j < mailbox->len; j++) {
        uint32_t idx = (mailbox->head + j) % (mailbox->cap == 0 ? 1u : mailbox->cap);
        mc_machine_envelope_t *env = &mailbox->items[idx];
        const uint8_t release_payload = !mc_envelope_is_request(env);
        const uint8_t release_origin = mc_envelope_is_response(env);
        mc_release_envelope_payloads(env, release_payload, release_origin);
    }
    __mc_free(mailbox->items);
    mailbox->items = NULL;
    // Preserve configured capacity so explicit lifecycle overrides can
    // reactivate the slot for test/diagnostic scenarios.
    mailbox->len = 0;
    mailbox->head = 0;
    mailbox->in_ready_queue = 0;

    slot->state_word = 0;
    slot->state_tag = 0;
    slot->descriptor = NULL;
    slot->dispatch = NULL;
    slot->dispatch_ctx = NULL;

    rt->stopped_cleanup_count += 1;
}

// Re-initialize mailbox storage when a previously-stopped slot is explicitly
// transitioned back to CREATED/RUNNING via lifecycle override.
static uint8_t mc_restore_mailbox_if_needed(mc_machine_slot_t *slot) {
    if (!slot) {
        return 0;
    }
    if (slot->mailbox.items != NULL) {
        return 1;
    }
    return mc_mailbox_init(&slot->mailbox, slot->mailbox.cap);
}

// Hook emitters ---------------------------------------------------------------

// Best-effort dead-letter callback.
void mc_emit_dead_letter(
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
void mc_emit_fault(mc_machine_runtime_t *rt, mc_machine_id_t machine_id, uint64_t code) {
    if (rt->fault_hook) {
        rt->fault_hook(rt->hook_ctx, machine_id, code);
    }
}

// Apply fault policy to the machine lifecycle.
void mc_apply_fault_policy(
    mc_machine_runtime_t *rt,
    mc_machine_slot_t *slot,
    mc_machine_id_t machine_id,
    uint64_t fault_code
) {
    if (rt->fault_policy == MC_FAULT_POLICY_MARK_STOPPED) {
        // MARK_STOPPED faults follow the same cleanup path as explicit stop.
        mc_stop_machine_and_cleanup_slot(rt, machine_id, slot);
    } else {
        slot->lifecycle = MC_MACHINE_FAULTED;
        // Faulting requester invalidates all outstanding inflight correlations.
        (void)mc_pending_cleanup_requester(
            rt,
            machine_id,
            MC_PENDING_CLEANUP_REQUESTER_FAULTED
        );
    }
    mc_emit_fault(rt, machine_id, fault_code);
}

// Preflight / commit helpers -------------------------------------------------

// Validate that outbox commit can succeed fully before any state mutation.
// This gives all-or-nothing commit behavior for outbox delivery.
static uint8_t mc_preflight_outbox(
    mc_machine_runtime_t *rt,
    const mc_machine_outbox_effect_t *outbox,
    uint32_t outbox_len
) {
    // Validate destination existence/lifecycle first.
    for (uint32_t i = 0; i < outbox_len; i++) {
        const mc_machine_outbox_effect_t *eff = &outbox[i];
        mc_machine_slot_t *dst = mc_get_slot(rt, eff->dst);
        if (!dst) {
            return 0;
        }
        if (dst->lifecycle != MC_MACHINE_RUNNING && dst->lifecycle != MC_MACHINE_CREATED) {
            return 0;
        }
    }

    // For each unique destination, ensure cumulative mailbox capacity exists.
    uint32_t additional_ready = 0;
    for (uint32_t i = 0; i < outbox_len; i++) {
        mc_machine_id_t dst_id = outbox[i].dst;

        uint8_t seen = 0;
        for (uint32_t k = 0; k < i; k++) {
            if (outbox[k].dst == dst_id) {
                seen = 1;
                break;
            }
        }
        if (seen) {
            continue;
        }

        mc_machine_slot_t *dst = mc_get_slot(rt, dst_id);
        uint32_t needed = 0;
        for (uint32_t j = i; j < outbox_len; j++) {
            if (outbox[j].dst == dst_id) {
                needed += 1;
            }
        }

        uint32_t free_slots = dst->mailbox.cap - dst->mailbox.len;
        if (needed > free_slots) {
            return 0;
        }

        if (dst->lifecycle == MC_MACHINE_RUNNING
            && dst->mailbox.len == 0
            && !dst->mailbox.in_ready_queue) {
            additional_ready += 1;
        }
    }

    // Ensure ready queue can accommodate all newly-ready destinations.
    return mc_ready_ensure_cap(&rt->ready, rt->ready.len + additional_ready);
}

// Commit staged outbox after successful preflight.
static uint8_t mc_commit_outbox(
    mc_machine_runtime_t *rt,
    const mc_machine_outbox_effect_t *outbox,
    uint32_t outbox_len
) {
    for (uint32_t i = 0; i < outbox_len; i++) {
        const mc_machine_outbox_effect_t *eff = &outbox[i];
        mc_machine_slot_t *dst = mc_get_slot(rt, eff->dst);
        if (!dst) {
            return 0;
        }
        if (!mc_mailbox_push(&dst->mailbox, &eff->env)) {
            return 0;
        }

        if (dst->lifecycle == MC_MACHINE_RUNNING && !dst->mailbox.in_ready_queue) {
            if (!mc_ready_push(&rt->ready, eff->dst)) {
                return 0;
            }
            dst->mailbox.in_ready_queue = 1;
        }
    }
    return 1;
}

// Validate staged requests before commit.
static uint8_t mc_preflight_requests(
    mc_machine_runtime_t *rt,
    const mc_machine_request_effect_t *requests,
    uint32_t requests_len
) {
    // Validate pending ids are non-zero, unique in txn, and not already active.
    for (uint32_t i = 0; i < requests_len; i++) {
        uint64_t pending_id = requests[i].pending_id;
        if (pending_id == 0) {
            return 0;
        }
        if (mc_pending_find_active(&rt->pending, pending_id) >= 0) {
            return 0;
        }
        for (uint32_t k = 0; k < i; k++) {
            if (requests[k].pending_id == pending_id) {
                return 0;
            }
        }
    }

    // Ensure pending table has enough reusable/new slots for all staged requests.
    uint32_t inactive = mc_pending_inactive_len(&rt->pending);
    uint32_t need_new = requests_len > inactive ? (requests_len - inactive) : 0;
    if (need_new > 0 && !mc_pending_ensure_cap(&rt->pending, rt->pending.len + need_new)) {
        return 0;
    }

    // Validate destinations and cumulative mailbox capacity.
    for (uint32_t i = 0; i < requests_len; i++) {
        mc_machine_slot_t *dst = mc_get_slot(rt, requests[i].dst);
        if (!dst) {
            return 0;
        }
        if (dst->lifecycle != MC_MACHINE_RUNNING && dst->lifecycle != MC_MACHINE_CREATED) {
            return 0;
        }
    }

    uint32_t additional_ready = 0;
    for (uint32_t i = 0; i < requests_len; i++) {
        mc_machine_id_t dst_id = requests[i].dst;
        uint8_t seen = 0;
        for (uint32_t k = 0; k < i; k++) {
            if (requests[k].dst == dst_id) {
                seen = 1;
                break;
            }
        }
        if (seen) {
            continue;
        }

        mc_machine_slot_t *dst = mc_get_slot(rt, dst_id);
        uint32_t needed = 0;
        for (uint32_t j = i; j < requests_len; j++) {
            if (requests[j].dst == dst_id) {
                needed += 1;
            }
        }
        uint32_t free_slots = dst->mailbox.cap - dst->mailbox.len;
        if (needed > free_slots) {
            return 0;
        }
        if (dst->lifecycle == MC_MACHINE_RUNNING
            && dst->mailbox.len == 0
            && !dst->mailbox.in_ready_queue) {
            additional_ready += 1;
        }
    }

    return mc_ready_ensure_cap(&rt->ready, rt->ready.len + additional_ready);
}

// Commit staged requests after successful preflight.
static uint8_t mc_commit_requests(
    mc_machine_runtime_t *rt,
    mc_machine_id_t src,
    const mc_machine_request_effect_t *requests,
    uint32_t requests_len
) {
    for (uint32_t i = 0; i < requests_len; i++) {
        const mc_machine_request_effect_t *req = &requests[i];
        mc_machine_slot_t *dst = mc_get_slot(rt, req->dst);
        if (!dst) {
            return 0;
        }

        mc_machine_envelope_t request_env = req->env;
        request_env.src = src;
        request_env.reply_cap_id = req->pending_id;
        request_env.pending_id = 0;
        request_env.origin_payload0 = 0;
        request_env.origin_payload1 = 0;
        request_env.origin_request_site_key = 0;

        if (!mc_mailbox_push(&dst->mailbox, &request_env)) {
            return 0;
        }
        if (dst->lifecycle == MC_MACHINE_RUNNING && !dst->mailbox.in_ready_queue) {
            if (!mc_ready_push(&rt->ready, req->dst)) {
                return 0;
            }
            dst->mailbox.in_ready_queue = 1;
        }
        mc_pending_correlation_id_t correlation = {
            .pending_id = req->pending_id,
            .request_site_key = req->request_site_key,
        };
        if (!mc_pending_insert_active(
                &rt->pending,
                correlation,
                req->env.payload0,
                req->env.payload1,
                src,
                rt->dispatch_tick
            )) {
            return 0;
        }
        rt->pending_created_count += 1;
    }
    return 1;
}

// Validate staged replies before commit.
static uint8_t mc_preflight_replies(
    mc_machine_runtime_t *rt,
    const mc_machine_reply_effect_t *replies,
    uint32_t replies_len
) {
    // Validate reply caps are active and unique in this txn.
    for (uint32_t i = 0; i < replies_len; i++) {
        uint64_t cap = replies[i].reply_cap_id;
        if (cap == 0 || mc_pending_find_active(&rt->pending, cap) < 0) {
            return 0;
        }
        for (uint32_t k = 0; k < i; k++) {
            if (replies[k].reply_cap_id == cap) {
                return 0;
            }
        }
    }

    // Validate requester destinations and mailbox capacity.
    uint32_t additional_ready = 0;
    for (uint32_t i = 0; i < replies_len; i++) {
        int32_t idx = mc_pending_find_active(&rt->pending, replies[i].reply_cap_id);
        if (idx < 0) {
            return 0;
        }
        mc_machine_id_t requester = rt->pending.entries[(uint32_t)idx].requester;
        mc_machine_slot_t *dst = mc_get_slot(rt, requester);
        if (!dst) {
            return 0;
        }
        if (dst->lifecycle != MC_MACHINE_RUNNING && dst->lifecycle != MC_MACHINE_CREATED) {
            return 0;
        }

        uint8_t seen = 0;
        for (uint32_t k = 0; k < i; k++) {
            int32_t prev_idx = mc_pending_find_active(&rt->pending, replies[k].reply_cap_id);
            if (prev_idx >= 0
                && rt->pending.entries[(uint32_t)prev_idx].requester == requester) {
                seen = 1;
                break;
            }
        }
        if (seen) {
            continue;
        }

        uint32_t needed = 0;
        for (uint32_t j = i; j < replies_len; j++) {
            int32_t ridx = mc_pending_find_active(&rt->pending, replies[j].reply_cap_id);
            if (ridx >= 0 && rt->pending.entries[(uint32_t)ridx].requester == requester) {
                needed += 1;
            }
        }
        uint32_t free_slots = dst->mailbox.cap - dst->mailbox.len;
        if (needed > free_slots) {
            return 0;
        }
        if (dst->lifecycle == MC_MACHINE_RUNNING
            && dst->mailbox.len == 0
            && !dst->mailbox.in_ready_queue) {
            additional_ready += 1;
        }
    }

    return mc_ready_ensure_cap(&rt->ready, rt->ready.len + additional_ready);
}

// Commit staged replies after successful preflight.
static uint8_t mc_commit_replies(
    mc_machine_runtime_t *rt,
    mc_machine_id_t src,
    const mc_machine_reply_effect_t *replies,
    uint32_t replies_len
) {
    for (uint32_t i = 0; i < replies_len; i++) {
        const mc_machine_reply_effect_t *reply = &replies[i];
        int32_t idx = mc_pending_find_active(&rt->pending, reply->reply_cap_id);
        if (idx < 0) {
            return 0;
        }
        mc_machine_id_t requester = rt->pending.entries[(uint32_t)idx].requester;
        mc_machine_slot_t *dst = mc_get_slot(rt, requester);
        if (!dst) {
            return 0;
        }

        mc_machine_envelope_t response_env = reply->env;
        response_env.src = src;
        response_env.reply_cap_id = 0;
        response_env.pending_id = reply->reply_cap_id;
        response_env.origin_payload0 = rt->pending.entries[(uint32_t)idx].request_payload0;
        response_env.origin_payload1 = rt->pending.entries[(uint32_t)idx].request_payload1;
        response_env.origin_request_site_key =
            rt->pending.entries[(uint32_t)idx].correlation.request_site_key;

        if (!mc_mailbox_push(&dst->mailbox, &response_env)) {
            return 0;
        }
        if (dst->lifecycle == MC_MACHINE_RUNNING && !dst->mailbox.in_ready_queue) {
            if (!mc_ready_push(&rt->ready, requester)) {
                return 0;
            }
            dst->mailbox.in_ready_queue = 1;
        }

        // Consume capability only after successful delivery.
        rt->pending.entries[(uint32_t)idx].active = 0;
        mc_emit_pending_cleanup(
            rt,
            &rt->pending.entries[(uint32_t)idx],
            MC_PENDING_CLEANUP_COMPLETED
        );
    }
    return 1;
}

// Preflight subscription capacity for add operations.
static uint8_t mc_preflight_subscriptions(
    mc_machine_runtime_t *rt,
    const mc_subscription_update_t *updates,
    uint32_t updates_len
) {
    uint32_t add_ops = 0;
    for (uint32_t i = 0; i < updates_len; i++) {
        if (updates[i].op == MC_SUBSCRIPTION_ADD) {
            add_ops += 1;
        }
    }
    return mc_subscription_ensure_cap(&rt->subscriptions, rt->subscriptions.len + add_ops);
}

// Commit staged subscription updates.
static void mc_commit_subscriptions(
    mc_machine_runtime_t *rt,
    const mc_subscription_update_t *updates,
    uint32_t updates_len
) {
    for (uint32_t i = 0; i < updates_len; i++) {
        const mc_subscription_update_t *up = &updates[i];
        if (up->op == MC_SUBSCRIPTION_REMOVE) {
            mc_subscription_remove(&rt->subscriptions, up->machine_id, up->kind, up->routing);
            continue;
        }

        if (!mc_subscription_contains(&rt->subscriptions, up->machine_id, up->kind, up->routing)) {
            rt->subscriptions.entries[rt->subscriptions.len].machine_id = up->machine_id;
            rt->subscriptions.entries[rt->subscriptions.len].kind = up->kind;
            rt->subscriptions.entries[rt->subscriptions.len].routing = up->routing;
            rt->subscriptions.len += 1;
        }
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

    rt->subscriptions.entries = NULL;
    rt->subscriptions.len = 0;
    rt->subscriptions.cap = 0;

    rt->pending.entries = NULL;
    rt->pending.len = 0;
    rt->pending.cap = 0;
    rt->pending.next_cap_id = 1;

    rt->fault_policy = MC_FAULT_POLICY_MARK_FAULTED;
    rt->dead_letter_hook = NULL;
    rt->fault_hook = NULL;
    rt->pending_hook = NULL;
    rt->hook_ctx = NULL;

    rt->dispatch_tick = 0;
    rt->pending_timeout_steps = 0;
    rt->pending_created_count = 0;
    memset(rt->pending_cleanup_counts, 0, sizeof(rt->pending_cleanup_counts));
    rt->stopped_cleanup_count = 0;
}

// Public API: free all runtime-owned buffers and reset state.
void __mc_machine_runtime_drop(mc_machine_runtime_t *rt) {
    if (!rt) {
        return;
    }
    for (uint32_t i = 0; i < rt->machine_len; i++) {
        mc_machine_mailbox_t *mailbox = &rt->machines[i].mailbox;
        for (uint32_t j = 0; j < mailbox->len; j++) {
            uint32_t idx = (mailbox->head + j) % (mailbox->cap == 0 ? 1u : mailbox->cap);
            mc_machine_envelope_t *env = &mailbox->items[idx];
            const uint8_t release_payload = !mc_envelope_is_request(env);
            const uint8_t release_origin = mc_envelope_is_response(env);
            mc_release_envelope_payloads(env, release_payload, release_origin);
        }
        __mc_free(rt->machines[i].mailbox.items);
        rt->machines[i].mailbox.items = NULL;
    }
    for (uint32_t i = 0; i < rt->pending.len; i++) {
        mc_pending_release_request_payload(&rt->pending.entries[i]);
    }
    __mc_free(rt->machines);
    __mc_free(rt->ready.items);
    __mc_free(rt->subscriptions.entries);
    __mc_free(rt->pending.entries);
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

void __mc_machine_runtime_set_pending_hook(
    mc_machine_runtime_t *rt,
    mc_pending_lifecycle_hook_t pending_hook
) {
    if (!rt) {
        return;
    }
    rt->pending_hook = pending_hook;
}

void __mc_machine_runtime_set_fault_policy(
    mc_machine_runtime_t *rt,
    mc_machine_fault_policy_t policy
) {
    if (!rt) {
        return;
    }
    rt->fault_policy = policy;
}

mc_machine_fault_policy_t __mc_machine_runtime_fault_policy(
    const mc_machine_runtime_t *rt
) {
    if (!rt) {
        return MC_FAULT_POLICY_MARK_FAULTED;
    }
    return rt->fault_policy;
}

void __mc_machine_runtime_set_pending_timeout_steps(
    mc_machine_runtime_t *rt,
    uint64_t steps
) {
    if (!rt) {
        return;
    }
    rt->pending_timeout_steps = steps;
}

uint64_t __mc_machine_runtime_pending_timeout_steps(
    const mc_machine_runtime_t *rt
) {
    return rt ? rt->pending_timeout_steps : 0;
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
    slot->state_word = 0;
    slot->state_tag = 0;
    slot->descriptor = NULL;
    slot->dispatch = NULL;
    slot->dispatch_ctx = NULL;

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
    if (slot->lifecycle == lifecycle) {
        return;
    }
    if (lifecycle == MC_MACHINE_STOPPED) {
        mc_stop_machine_and_cleanup_slot(rt, machine_id, slot);
    } else if (lifecycle == MC_MACHINE_FAULTED) {
        slot->lifecycle = lifecycle;
        (void)mc_pending_cleanup_requester(rt, machine_id, MC_PENDING_CLEANUP_REQUESTER_FAULTED);
    } else {
        if (slot->lifecycle == MC_MACHINE_STOPPED && !mc_restore_mailbox_if_needed(slot)) {
            return;
        }
        slot->lifecycle = lifecycle;
    }
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

// Public API: bind one machine-local dispatch callback used by step/dispatch
// helpers when no explicit callback argument is supplied.
void __mc_machine_runtime_bind_dispatch(
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id,
    mc_machine_dispatch_txn_fn dispatch,
    void *dispatch_ctx
) {
    mc_machine_slot_t *slot = mc_get_slot(rt, machine_id);
    if (!slot) {
        return;
    }
    slot->dispatch = dispatch;
    slot->dispatch_ctx = dispatch_ctx;
}

void __mc_machine_runtime_set_state(
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id,
    mc_machine_state_token_t state_word
) {
    mc_machine_slot_t *slot = mc_get_slot(rt, machine_id);
    if (!slot) {
        return;
    }
    slot->state_word = state_word;
}

mc_machine_state_token_t __mc_machine_runtime_state(
    const mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
) {
    const mc_machine_slot_t *slot = mc_get_slot_const(rt, machine_id);
    if (!slot) {
        return 0;
    }
    return slot->state_word;
}

// Public API: enqueue envelope for destination machine.
//
// Key behavior:
// - Unknown/Faulted/Stopped destinations are rejected and optionally dead-lettered.
// - CREATED destination buffers messages but is not scheduled.
// - RUNNING destination buffers and is scheduled once.
mc_mailbox_enqueue_result_t __mc_machine_runtime_enqueue(
    mc_machine_runtime_t *rt,
    mc_machine_id_t dst,
    const mc_machine_envelope_t *env
) {
    mc_machine_slot_t *slot = mc_get_slot(rt, dst);
    if (!slot) {
        mc_emit_dead_letter(rt, dst, MC_DEAD_LETTER_UNKNOWN_MACHINE, env);
        mc_release_envelope_payloads(env, 1, 0);
        return MC_MAILBOX_ENQUEUE_MACHINE_UNKNOWN;
    }

    if (slot->lifecycle == MC_MACHINE_CREATED) {
        // CREATED machines are allowed to buffer inbound messages before start.
    } else if (slot->lifecycle != MC_MACHINE_RUNNING) {
        mc_dead_letter_reason_t reason = slot->lifecycle == MC_MACHINE_FAULTED
            ? MC_DEAD_LETTER_FAULTED_MACHINE
            : MC_DEAD_LETTER_STOPPED_MACHINE;
        mc_emit_dead_letter(rt, dst, reason, env);
        mc_release_envelope_payloads(env, 1, 0);
        return MC_MAILBOX_ENQUEUE_MACHINE_NOT_RUNNING;
    }

    if (!mc_mailbox_push(&slot->mailbox, env)) {
        mc_emit_dead_letter(rt, dst, MC_DEAD_LETTER_MAILBOX_FULL, env);
        mc_release_envelope_payloads(env, 1, 0);
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

mc_mailbox_enqueue_result_t __mc_machine_runtime_request(
    mc_machine_runtime_t *rt,
    mc_machine_id_t src,
    mc_machine_id_t dst,
    const mc_machine_envelope_t *env,
    uint64_t *out_pending_id
) {
    // Keep API behavior deterministic on invalid inputs.
    if (!rt || !env || !out_pending_id) {
        return MC_MAILBOX_ENQUEUE_MACHINE_UNKNOWN;
    }

    // Preflight local pending table capacity so request enqueue + pending insert
    // can complete as one unit.
    uint8_t has_inactive_slot = 0;
    for (uint32_t i = 0; i < rt->pending.len; i++) {
        if (!rt->pending.entries[i].active) {
            has_inactive_slot = 1;
            break;
        }
    }
    if (!has_inactive_slot && !mc_pending_ensure_cap(&rt->pending, rt->pending.len + 1)) {
        return MC_MAILBOX_ENQUEUE_FULL;
    }

    uint64_t pending_id = 0;
    if (!mc_pending_next_cap_id(&rt->pending, &pending_id)) {
        return MC_MAILBOX_ENQUEUE_FULL;
    }

    // Runtime-owned correlation fields override caller-provided values.
    mc_machine_envelope_t request_env = *env;
    request_env.src = src;
    request_env.reply_cap_id = pending_id;
    request_env.pending_id = 0;
    request_env.origin_payload0 = 0;
    request_env.origin_payload1 = 0;
    request_env.origin_request_site_key = 0;

    mc_mailbox_enqueue_result_t res = __mc_machine_runtime_enqueue(rt, dst, &request_env);
    if (res != MC_MAILBOX_ENQUEUE_OK) {
        return res;
    }

    // Record requester lookup keyed by minted pending correlation identity.
    mc_pending_correlation_id_t correlation = {
        .pending_id = pending_id,
        .request_site_key = 0,
    };
    (void)mc_pending_insert_active(
        &rt->pending,
        correlation,
        env->payload0,
        env->payload1,
        src,
        rt->dispatch_tick
    );
    rt->pending_created_count += 1;

    *out_pending_id = pending_id;
    return MC_MAILBOX_ENQUEUE_OK;
}

mc_machine_reply_result_t __mc_machine_runtime_reply(
    mc_machine_runtime_t *rt,
    mc_machine_id_t src,
    uint64_t reply_cap_id,
    const mc_machine_envelope_t *env
) {
    // Invalid inputs map to deterministic "unknown cap" behavior.
    if (!rt || !env) {
        return MC_REPLY_CAP_UNKNOWN;
    }

    // Reply is routed by reply capability id, not by explicit destination.
    int32_t idx = mc_pending_find_active(&rt->pending, reply_cap_id);
    if (idx < 0) {
        mc_emit_dead_letter(rt, 0, MC_DEAD_LETTER_REPLY_CAP_UNKNOWN, env);
        mc_release_envelope_payloads(env, 1, 1);
        return MC_REPLY_CAP_UNKNOWN;
    }

    mc_machine_id_t requester = rt->pending.entries[(uint32_t)idx].requester;
    // Runtime-owned correlation fields override caller-provided values.
    mc_machine_envelope_t response_env = *env;
    response_env.src = src;
    response_env.reply_cap_id = 0;
    response_env.pending_id = reply_cap_id;
    response_env.origin_payload0 = rt->pending.entries[(uint32_t)idx].request_payload0;
    response_env.origin_payload1 = rt->pending.entries[(uint32_t)idx].request_payload1;
    response_env.origin_request_site_key =
        rt->pending.entries[(uint32_t)idx].correlation.request_site_key;

    mc_mailbox_enqueue_result_t enqueue_res =
        __mc_machine_runtime_enqueue(rt, requester, &response_env);
    if (enqueue_res == MC_MAILBOX_ENQUEUE_OK) {
        // Transfer request payload ownership from pending table into response
        // provenance fields so response-dispatch cleanup can release it.
        rt->pending.entries[(uint32_t)idx].request_payload0 = 0;
        rt->pending.entries[(uint32_t)idx].request_payload1 = 0;
        // Consume capability on successful reply delivery.
        rt->pending.entries[(uint32_t)idx].active = 0;
        mc_emit_pending_cleanup(
            rt,
            &rt->pending.entries[(uint32_t)idx],
            MC_PENDING_CLEANUP_COMPLETED
        );
        return MC_REPLY_OK;
    }
    // Full mailbox keeps capability active so caller can retry.
    // For stopped/faulted requester, reclaim now to avoid stale inflight ids.
    if (enqueue_res == MC_MAILBOX_ENQUEUE_MACHINE_UNKNOWN) {
        return MC_REPLY_DEST_UNKNOWN;
    }
    if (enqueue_res == MC_MAILBOX_ENQUEUE_MACHINE_NOT_RUNNING) {
        mc_machine_lifecycle_t lifecycle = __mc_machine_runtime_lifecycle(rt, requester);
        if (lifecycle == MC_MACHINE_STOPPED) {
            mc_pending_release_request_payload(&rt->pending.entries[(uint32_t)idx]);
            rt->pending.entries[(uint32_t)idx].active = 0;
            mc_emit_pending_cleanup(
                rt,
                &rt->pending.entries[(uint32_t)idx],
                MC_PENDING_CLEANUP_REQUESTER_STOPPED
            );
        } else if (lifecycle == MC_MACHINE_FAULTED) {
            mc_pending_release_request_payload(&rt->pending.entries[(uint32_t)idx]);
            rt->pending.entries[(uint32_t)idx].active = 0;
            mc_emit_pending_cleanup(
                rt,
                &rt->pending.entries[(uint32_t)idx],
                MC_PENDING_CLEANUP_REQUESTER_FAULTED
            );
        }
        return MC_REPLY_DEST_NOT_RUNNING;
    }
    return MC_REPLY_FULL;
}

// Internal implementation used by both core dispatch API and opaque-handle
// bridge helpers that need per-step fault metadata.
uint8_t mc_machine_runtime_dispatch_one_txn_impl(
    mc_machine_runtime_t *rt,
    mc_machine_dispatch_txn_fn dispatch,
    void *dispatch_ctx,
    uint8_t *out_faulted,
    mc_machine_id_t *out_fault_machine,
    uint64_t *out_fault_code
) {
    if (out_faulted) {
        *out_faulted = 0;
    }
    if (out_fault_machine) {
        *out_fault_machine = 0;
    }
    if (out_fault_code) {
        *out_fault_code = 0;
    }

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
        // Dispatch-step clock advances once per consumed envelope.
        rt->dispatch_tick += 1;

        uint64_t fault_code = 0;
        mc_machine_dispatch_txn_t txn = {
            .has_next_state = 0,
            .next_state = 0,
            .next_state_tag = 0,
            .outbox = NULL,
            .outbox_len = 0,
            .subscriptions = NULL,
            .subscriptions_len = 0,
            .requests = NULL,
            .requests_len = 0,
            .replies = NULL,
            .replies_len = 0,
        };

        // Bind dispatch-local emit staging so compiler-lowered `emit` / `reply`
        // can populate txn effects without threading runtime pointers through
        // every handler ABI.
        mc_emit_staging_ctx_t emit_ctx;
        mc_emit_staging_begin(&emit_ctx, rt, machine_id);
        // Explicit callback argument takes precedence; otherwise fall back to
        // machine-local callback bound during spawn/bootstrap.
        mc_machine_dispatch_txn_fn effective_dispatch =
            dispatch ? dispatch : slot->dispatch;
        void *effective_dispatch_ctx = dispatch ? dispatch_ctx : slot->dispatch_ctx;
        mc_dispatch_result_t result = effective_dispatch
            ? effective_dispatch(
                  effective_dispatch_ctx,
                  machine_id,
                  slot->state_word,
                  &env,
                  &txn,
                  &fault_code
              )
            : MC_DISPATCH_OK;

        if (result == MC_DISPATCH_OK && !mc_emit_merge_into_txn(&emit_ctx, &txn)) {
            fault_code = MC_FAULT_CODE_TXN_COMMIT_REJECTED;
            result = MC_DISPATCH_FAULT;
        }

        if (result == MC_DISPATCH_OK) {
            uint8_t preflight_ok =
                mc_ready_ensure_cap(
                    &rt->ready,
                    rt->ready.len + txn.outbox_len + txn.requests_len + txn.replies_len
                )
                &&
                mc_preflight_subscriptions(rt, txn.subscriptions, txn.subscriptions_len)
                && mc_preflight_outbox(rt, txn.outbox, txn.outbox_len)
                && mc_preflight_requests(rt, txn.requests, txn.requests_len)
                && mc_preflight_replies(rt, txn.replies, txn.replies_len);
            if (!preflight_ok) {
                fault_code = MC_FAULT_CODE_TXN_COMMIT_REJECTED;
                result = MC_DISPATCH_FAULT;
            }
        }

        if (result == MC_DISPATCH_OK) {
            // Commit transaction after preflight has guaranteed capacity.
            mc_commit_subscriptions(rt, txn.subscriptions, txn.subscriptions_len);
            if (!mc_commit_outbox(rt, txn.outbox, txn.outbox_len)
                || !mc_commit_requests(rt, machine_id, txn.requests, txn.requests_len)
                || !mc_commit_replies(rt, machine_id, txn.replies, txn.replies_len)) {
                // Defensive guard: this should be unreachable after preflight.
                fault_code = MC_FAULT_CODE_TXN_COMMIT_REJECTED;
                result = MC_DISPATCH_FAULT;
            } else if (txn.has_next_state) {
                slot->state_word = txn.next_state;
                if (txn.next_state_tag != 0) {
                    slot->state_tag = txn.next_state_tag;
                }
            }
        }

        if (result == MC_DISPATCH_FAULT) {
            mc_apply_fault_policy(rt, slot, machine_id, fault_code);
            if (out_faulted) {
                *out_faulted = 1;
            }
            if (out_fault_machine) {
                *out_fault_machine = machine_id;
            }
            if (out_fault_code) {
                *out_fault_code = fault_code;
            }
        } else if (result == MC_DISPATCH_STOP) {
            mc_stop_machine_and_cleanup_slot(rt, machine_id, slot);
        }

        if (slot->lifecycle == MC_MACHINE_RUNNING && slot->mailbox.len > 0) {
            if (mc_ready_push(&rt->ready, machine_id)) {
                slot->mailbox.in_ready_queue = 1;
            }
        }

        // Always release dispatch-local staging buffers after commit/rollback.
        mc_emit_staging_end(&emit_ctx);

        // Timeout cleanup runs after each dispatch step and reclaims inflight
        // entries that exceeded configured dispatch-step horizon.
        (void)mc_pending_cleanup_timeouts(rt);

        // Envelope has been consumed; release boxed payload ownership.
        // Request envelopes keep `payload0` owned by pending-correlation table
        // until a correlated response is routed or pending cleanup reclaims it.
        const uint8_t release_payload = !mc_envelope_is_request(&env);
        const uint8_t release_origin = mc_envelope_is_response(&env);
        mc_release_envelope_payloads(&env, release_payload, release_origin);

        // Exactly one dispatch per call.
        return 1;
    }

    return 0;
}

// Public API: execute at most one dispatch using transactional callback.
//
// Commit semantics:
// - staged effects (outbox/subscriptions/request/reply) are committed only on
//   MC_DISPATCH_OK and successful preflight.
// - on faults/stops, staged effects are discarded.
uint8_t __mc_machine_runtime_dispatch_one_txn(
    mc_machine_runtime_t *rt,
    mc_machine_dispatch_txn_fn dispatch,
    void *dispatch_ctx
) {
    return mc_machine_runtime_dispatch_one_txn_impl(rt, dispatch, dispatch_ctx, NULL, NULL, NULL);
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

uint32_t __mc_machine_runtime_subscription_len(const mc_machine_runtime_t *rt) {
    return rt ? rt->subscriptions.len : 0;
}

uint8_t __mc_machine_runtime_subscription_contains(
    const mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id,
    mc_machine_event_kind_t kind,
    uint64_t routing
) {
    if (!rt) {
        return 0;
    }
    return mc_subscription_contains(&rt->subscriptions, machine_id, kind, routing);
}

uint32_t __mc_machine_runtime_pending_len(const mc_machine_runtime_t *rt) {
    if (!rt) {
        return 0;
    }
    return mc_pending_active_len(&rt->pending);
}

uint8_t __mc_machine_runtime_pending_contains(
    const mc_machine_runtime_t *rt,
    uint64_t pending_id
) {
    if (!rt) {
        return 0;
    }
    return mc_pending_contains_active(&rt->pending, pending_id);
}

uint8_t __mc_machine_runtime_pending_contains_identity(
    const mc_machine_runtime_t *rt,
    uint64_t pending_id,
    uint64_t request_site_key
) {
    if (!rt) {
        return 0;
    }
    mc_pending_correlation_id_t correlation = {
        .pending_id = pending_id,
        .request_site_key = request_site_key,
    };
    return mc_pending_contains_identity(&rt->pending, correlation);
}

uint64_t __mc_machine_runtime_pending_request_site(
    const mc_machine_runtime_t *rt,
    uint64_t pending_id
) {
    if (!rt) {
        return 0;
    }
    int32_t idx = mc_pending_find_active(&rt->pending, pending_id);
    if (idx < 0) {
        return 0;
    }
    return rt->pending.entries[(uint32_t)idx].correlation.request_site_key;
}

uint64_t __mc_machine_runtime_pending_created_count(
    const mc_machine_runtime_t *rt
) {
    return rt ? rt->pending_created_count : 0;
}

uint64_t __mc_machine_runtime_pending_cleanup_count(
    const mc_machine_runtime_t *rt,
    mc_pending_cleanup_reason_t reason
) {
    if (!rt || !mc_pending_cleanup_reason_valid(reason)) {
        return 0;
    }
    return rt->pending_cleanup_counts[(uint32_t)reason];
}

uint64_t __mc_machine_runtime_stopped_cleanup_count(
    const mc_machine_runtime_t *rt
) {
    return rt ? rt->stopped_cleanup_count : 0;
}
