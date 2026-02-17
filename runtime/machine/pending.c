#include "pending.h"
#include "internal.h"

uint8_t mc_pending_ensure_cap(mc_pending_reply_table_t *pending, uint32_t min_cap) {
    if (pending->cap >= min_cap) {
        return 1;
    }

    uint32_t new_cap = pending->cap == 0 ? MC_PENDING_INITIAL_CAP : pending->cap;
    while (new_cap < min_cap) {
        uint32_t grown = new_cap * 2u;
        if (grown < new_cap) {
            return 0;
        }
        new_cap = grown;
    }

    mc_pending_reply_entry_t *new_entries = (mc_pending_reply_entry_t *)__mc_realloc(
        pending->entries,
        (size_t)new_cap * sizeof(mc_pending_reply_entry_t),
        _Alignof(mc_pending_reply_entry_t)
    );
    if (!new_entries) {
        return 0;
    }

    pending->entries = new_entries;
    pending->cap = new_cap;
    return 1;
}

// Locate an active pending entry by runtime pending id.
// Returns -1 when the id is not currently active.
int32_t mc_pending_find_active(
    const mc_pending_reply_table_t *pending,
    uint64_t pending_id
) {
    for (uint32_t i = 0; i < pending->len; i++) {
        if (pending->entries[i].active
            && pending->entries[i].correlation.pending_id == pending_id) {
            return (int32_t)i;
        }
    }
    return -1;
}

// Locate an active pending entry by full correlation identity.
// Returns -1 when the identity is not currently active.
int32_t mc_pending_find_active_identity(
    const mc_pending_reply_table_t *pending,
    mc_pending_correlation_id_t correlation
) {
    for (uint32_t i = 0; i < pending->len; i++) {
        if (!pending->entries[i].active) {
            continue;
        }
        const mc_pending_correlation_id_t entry = pending->entries[i].correlation;
        if (entry.pending_id == correlation.pending_id
            && entry.request_site_key == correlation.request_site_key) {
            return (int32_t)i;
        }
    }
    return -1;
}

// Fast membership helper used by test/debug introspection.
uint8_t mc_pending_contains_active(
    const mc_pending_reply_table_t *pending,
    uint64_t pending_id
) {
    return mc_pending_find_active(pending, pending_id) >= 0;
}

// Identity membership helper used by test/debug introspection.
uint8_t mc_pending_contains_identity(
    const mc_pending_reply_table_t *pending,
    mc_pending_correlation_id_t correlation
) {
    return mc_pending_find_active_identity(pending, correlation) >= 0;
}

// Insert a newly-minted pending capability.
// Reuses inactive slots first to keep table growth bounded over time.
uint8_t mc_pending_insert_active(
    mc_pending_reply_table_t *pending,
    mc_pending_correlation_id_t correlation,
    uint64_t request_payload0,
    mc_payload_layout_id_t request_payload1,
    mc_machine_id_t requester,
    uint64_t created_tick
) {
    for (uint32_t i = 0; i < pending->len; i++) {
        if (!pending->entries[i].active) {
            pending->entries[i].correlation = correlation;
            pending->entries[i].requester = requester;
            pending->entries[i].request_payload0 = request_payload0;
            pending->entries[i].request_payload1 = request_payload1;
            pending->entries[i].created_tick = created_tick;
            pending->entries[i].active = 1;
            return 1;
        }
    }
    if (!mc_pending_ensure_cap(pending, pending->len + 1)) {
        return 0;
    }
    pending->entries[pending->len].correlation = correlation;
    pending->entries[pending->len].requester = requester;
    pending->entries[pending->len].request_payload0 = request_payload0;
    pending->entries[pending->len].request_payload1 = request_payload1;
    pending->entries[pending->len].created_tick = created_tick;
    pending->entries[pending->len].active = 1;
    pending->len += 1;
    return 1;
}

// Monotonic capability id allocator.
// Zero is reserved as "no capability id".
uint8_t mc_pending_next_cap_id(mc_pending_reply_table_t *pending, uint64_t *out_cap_id) {
    if (!out_cap_id) {
        return 0;
    }
    uint64_t id = pending->next_cap_id;
    if (id == 0) {
        return 0;
    }
    pending->next_cap_id += 1;
    if (pending->next_cap_id == 0) {
        // Exhausted the 64-bit id space.
        return 0;
    }
    *out_cap_id = id;
    return 1;
}

// Count active pending capabilities (used by tests/introspection APIs).
uint32_t mc_pending_active_len(const mc_pending_reply_table_t *pending) {
    uint32_t count = 0;
    for (uint32_t i = 0; i < pending->len; i++) {
        if (pending->entries[i].active) {
            count += 1;
        }
    }
    return count;
}

// Count inactive pending slots available for reuse.
uint32_t mc_pending_inactive_len(const mc_pending_reply_table_t *pending) {
    uint32_t count = 0;
    for (uint32_t i = 0; i < pending->len; i++) {
        if (!pending->entries[i].active) {
            count += 1;
        }
    }
    return count;
}

void mc_pending_release_request_payload(mc_pending_reply_entry_t *entry) {
    if (!entry) {
        return;
    }
    if (entry->request_payload0 != 0 && mc_payload_layout_is_owned(entry->request_payload1)) {
        __mc_free((void *)(uintptr_t)entry->request_payload0);
    }
    entry->request_payload0 = 0;
    entry->request_payload1 = 0;
}

uint8_t mc_pending_cleanup_reason_valid(mc_pending_cleanup_reason_t reason) {
    return reason >= MC_PENDING_CLEANUP_COMPLETED && reason <= MC_PENDING_CLEANUP_TIMEOUT;
}

// Record one pending lifecycle transition in counters and optional hook.
void mc_emit_pending_cleanup(
    mc_machine_runtime_t *rt,
    const mc_pending_reply_entry_t *entry,
    mc_pending_cleanup_reason_t reason
) {
    if (!rt || !entry || !mc_pending_cleanup_reason_valid(reason)) {
        return;
    }
    rt->pending_cleanup_counts[(uint32_t)reason] += 1;
    if (rt->pending_hook) {
        rt->pending_hook(
            rt->hook_ctx,
            entry->requester,
            entry->correlation.pending_id,
            entry->correlation.request_site_key,
            reason
        );
    }
}

// Drop all inflight request entries for one requester machine.
uint32_t mc_pending_cleanup_requester(
    mc_machine_runtime_t *rt,
    mc_machine_id_t requester,
    mc_pending_cleanup_reason_t reason
) {
    if (!rt || !mc_pending_cleanup_reason_valid(reason)) {
        return 0;
    }
    uint32_t dropped = 0;
    for (uint32_t i = 0; i < rt->pending.len; i++) {
        mc_pending_reply_entry_t *entry = &rt->pending.entries[i];
        if (!entry->active || entry->requester != requester) {
            continue;
        }
        mc_pending_release_request_payload(entry);
        entry->active = 0;
        mc_emit_pending_cleanup(rt, entry, reason);
        dropped += 1;
    }
    return dropped;
}

// Drop expired pending entries based on dispatch-step timeout horizon.
uint32_t mc_pending_cleanup_timeouts(mc_machine_runtime_t *rt) {
    if (!rt || rt->pending_timeout_steps == 0) {
        return 0;
    }
    uint32_t dropped = 0;
    for (uint32_t i = 0; i < rt->pending.len; i++) {
        mc_pending_reply_entry_t *entry = &rt->pending.entries[i];
        if (!entry->active) {
            continue;
        }
        if (rt->dispatch_tick < entry->created_tick) {
            // Counter wrapped; keep entry until timeline catches up.
            continue;
        }
        uint64_t age = rt->dispatch_tick - entry->created_tick;
        if (age < rt->pending_timeout_steps) {
            continue;
        }
        mc_pending_release_request_payload(entry);
        entry->active = 0;
        mc_emit_pending_cleanup(rt, entry, MC_PENDING_CLEANUP_TIMEOUT);
        dropped += 1;
    }
    return dropped;
}
