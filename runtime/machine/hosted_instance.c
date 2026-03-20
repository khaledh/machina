#include "hosted_instance.h"

#include "internal.h"

#ifndef MC_HOSTED_INSTANCE_INITIAL_CAP
#define MC_HOSTED_INSTANCE_INITIAL_CAP 8u
#endif

static uint8_t mc_hosted_instance_table_ensure_cap(
    mc_hosted_instance_table_t *table,
    uint32_t min_cap
) {
    if (table->cap >= min_cap) {
        return 1;
    }

    uint32_t new_cap = table->cap == 0 ? MC_HOSTED_INSTANCE_INITIAL_CAP : table->cap;
    while (new_cap < min_cap) {
        new_cap *= 2;
    }

    size_t new_size = sizeof(mc_hosted_instance_t) * (size_t)new_cap;
    size_t align = _Alignof(mc_hosted_instance_t);
    if (table->slots) {
        table->slots = (mc_hosted_instance_t *)__mc_realloc(table->slots, new_size, align);
    } else {
        table->slots = (mc_hosted_instance_t *)__mc_alloc(new_size, align);
    }
    if (!table->slots) {
        return 0;
    }

    for (uint32_t i = table->cap; i < new_cap; i++) {
        table->slots[i].key = 0;
        table->slots[i].state_tag = 0;
        table->slots[i].state_payload = (uintptr_t)0;
        table->slots[i].derived_interaction_id = 0;
        table->slots[i].derived_interaction_created_count = 0;
        table->slots[i].derived_interaction_resolved_count = 0;
        table->slots[i].active = 0;
    }

    table->cap = new_cap;
    return 1;
}

void mc_hosted_instance_table_init(mc_hosted_instance_table_t *table) {
    table->slots = NULL;
    table->len = 0;
    table->cap = 0;
    table->next_key = 1;
}

void mc_hosted_instance_table_drop(mc_hosted_instance_table_t *table) {
    if (table->slots) {
        __mc_free(table->slots);
    }
    table->slots = NULL;
    table->len = 0;
    table->cap = 0;
    table->next_key = 0;
}

uint64_t mc_hosted_instance_table_create(
    mc_hosted_instance_table_t *table,
    uint64_t initial_state_tag,
    uintptr_t initial_payload
) {
    if (!mc_hosted_instance_table_ensure_cap(table, table->len + 1)) {
        return 0;
    }

    uint32_t slot = table->cap;
    for (uint32_t i = 0; i < table->cap; i++) {
        if (!table->slots[i].active) {
            slot = i;
            break;
        }
    }

    if (slot == table->cap) {
        return 0;
    }

    // Keys are runtime-assigned identities. They remain stable even if the
    // instance moves to a different slot after future table growth.
    uint64_t key = table->next_key++;
    table->slots[slot].key = key;
    table->slots[slot].state_tag = initial_state_tag;
    table->slots[slot].state_payload = initial_payload;
    table->slots[slot].derived_interaction_id = 0;
    table->slots[slot].derived_interaction_created_count = 0;
    table->slots[slot].derived_interaction_resolved_count = 0;
    table->slots[slot].active = 1;
    table->len += 1;
    return key;
}

uint8_t mc_hosted_instance_table_lookup(
    const mc_hosted_instance_table_t *table,
    uint64_t key,
    uint64_t *out_state_tag,
    uintptr_t *out_payload
) {
    for (uint32_t i = 0; i < table->cap; i++) {
        const mc_hosted_instance_t *slot = &table->slots[i];
        if (!slot->active || slot->key != key) {
            continue;
        }
        if (out_state_tag) {
            *out_state_tag = slot->state_tag;
        }
        if (out_payload) {
            *out_payload = slot->state_payload;
        }
        return 1;
    }
    return 0;
}

uint8_t mc_hosted_instance_table_update(
    mc_hosted_instance_table_t *table,
    uint64_t key,
    uint64_t expected_tag,
    uint64_t new_tag,
    uintptr_t new_payload,
    uint64_t *out_actual_tag
) {
    for (uint32_t i = 0; i < table->cap; i++) {
        mc_hosted_instance_t *slot = &table->slots[i];
        if (!slot->active || slot->key != key) {
            continue;
        }
        // Conditional update is the primitive hosted sessions need for stale
        // checks: only advance the instance when the caller's expected state
        // still matches the machine's authoritative state.
        if (slot->state_tag != expected_tag) {
            if (out_actual_tag) {
                *out_actual_tag = slot->state_tag;
            }
            return MC_HOSTED_UPDATE_STALE;
        }
        slot->state_tag = new_tag;
        slot->state_payload = new_payload;
        return MC_HOSTED_UPDATE_OK;
    }
    return MC_HOSTED_UPDATE_NOT_FOUND;
}

uint8_t mc_hosted_instance_table_remove(mc_hosted_instance_table_t *table, uint64_t key) {
    for (uint32_t i = 0; i < table->cap; i++) {
        mc_hosted_instance_t *slot = &table->slots[i];
        if (!slot->active || slot->key != key) {
            continue;
        }
        slot->active = 0;
        slot->key = 0;
        slot->state_tag = 0;
        slot->state_payload = (uintptr_t)0;
        slot->derived_interaction_id = 0;
        slot->derived_interaction_created_count = 0;
        slot->derived_interaction_resolved_count = 0;
        table->len -= 1;
        return 1;
    }
    return 0;
}

uint32_t mc_hosted_instance_table_count(const mc_hosted_instance_table_t *table) {
    return table->len;
}

uint8_t mc_hosted_instance_table_begin_derived_interaction(
    mc_hosted_instance_table_t *table,
    uint64_t key,
    uint64_t interaction_id
) {
    if (interaction_id == 0) {
        return 0;
    }
    for (uint32_t i = 0; i < table->cap; i++) {
        mc_hosted_instance_t *slot = &table->slots[i];
        if (!slot->active || slot->key != key) {
            continue;
        }
        slot->derived_interaction_id = interaction_id;
        slot->derived_interaction_created_count += 1;
        return 1;
    }
    return 0;
}

uint8_t mc_hosted_instance_table_resolve_derived_interaction(
    mc_hosted_instance_table_t *table,
    uint64_t key,
    uint64_t *out_interaction_id
) {
    for (uint32_t i = 0; i < table->cap; i++) {
        mc_hosted_instance_t *slot = &table->slots[i];
        if (!slot->active || slot->key != key) {
            continue;
        }
        if (out_interaction_id) {
            *out_interaction_id = slot->derived_interaction_id;
        }
        if (slot->derived_interaction_id != 0) {
            slot->derived_interaction_id = 0;
            slot->derived_interaction_resolved_count += 1;
        }
        return 1;
    }
    return 0;
}

uint64_t mc_hosted_instance_table_active_derived_interaction(
    const mc_hosted_instance_table_t *table,
    uint64_t key
) {
    for (uint32_t i = 0; i < table->cap; i++) {
        const mc_hosted_instance_t *slot = &table->slots[i];
        if (!slot->active || slot->key != key) {
            continue;
        }
        return slot->derived_interaction_id;
    }
    return 0;
}

uint64_t mc_hosted_instance_table_derived_interaction_created_count(
    const mc_hosted_instance_table_t *table,
    uint64_t key
) {
    for (uint32_t i = 0; i < table->cap; i++) {
        const mc_hosted_instance_t *slot = &table->slots[i];
        if (!slot->active || slot->key != key) {
            continue;
        }
        return slot->derived_interaction_created_count;
    }
    return 0;
}

uint64_t mc_hosted_instance_table_derived_interaction_resolved_count(
    const mc_hosted_instance_table_t *table,
    uint64_t key
) {
    for (uint32_t i = 0; i < table->cap; i++) {
        const mc_hosted_instance_t *slot = &table->slots[i];
        if (!slot->active || slot->key != key) {
            continue;
        }
        return slot->derived_interaction_resolved_count;
    }
    return 0;
}
