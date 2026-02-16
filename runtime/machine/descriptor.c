#include "descriptor.h"
#include "internal.h"

#include <string.h>

// Process-global thunk registry used by bootstrap to map descriptor thunk ids
// to concrete dispatch callback pointers.
static mc_thunk_registry_entry_t *g_thunk_registry = NULL;
static uint32_t g_thunk_registry_len = 0;
static uint32_t g_thunk_registry_cap = 0;

// Parsed descriptor registry (global for process lifetime).
static mc_machine_descriptor_t *g_descriptor_registry = NULL;
static uint32_t g_descriptor_registry_len = 0;
static uint32_t g_descriptor_registry_cap = 0;
static uint64_t g_next_descriptor_id = 1;

uint8_t mc_thunk_registry_ensure_cap(uint32_t min_cap) {
    if (g_thunk_registry_cap >= min_cap) {
        return 1;
    }
    uint32_t new_cap = g_thunk_registry_cap == 0 ? 16u : g_thunk_registry_cap;
    while (new_cap < min_cap) {
        uint32_t grown = new_cap * 2u;
        if (grown < new_cap) {
            return 0;
        }
        new_cap = grown;
    }
    mc_thunk_registry_entry_t *new_entries = (mc_thunk_registry_entry_t *)__mc_realloc(
        g_thunk_registry,
        (size_t)new_cap * sizeof(mc_thunk_registry_entry_t),
        _Alignof(mc_thunk_registry_entry_t)
    );
    if (!new_entries) {
        return 0;
    }
    g_thunk_registry = new_entries;
    g_thunk_registry_cap = new_cap;
    return 1;
}

int32_t mc_thunk_registry_find(uint64_t thunk_id) {
    for (uint32_t i = 0; i < g_thunk_registry_len; i++) {
        if (g_thunk_registry[i].thunk_id == thunk_id) {
            return (int32_t)i;
        }
    }
    return -1;
}

uint8_t mc_descriptor_registry_ensure_cap(uint32_t min_cap) {
    if (g_descriptor_registry_cap >= min_cap) {
        return 1;
    }
    uint32_t new_cap = g_descriptor_registry_cap == 0 ? 8u : g_descriptor_registry_cap;
    while (new_cap < min_cap) {
        uint32_t grown = new_cap * 2u;
        if (grown < new_cap) {
            return 0;
        }
        new_cap = grown;
    }
    mc_machine_descriptor_t *new_entries = (mc_machine_descriptor_t *)__mc_realloc(
        g_descriptor_registry,
        (size_t)new_cap * sizeof(mc_machine_descriptor_t),
        _Alignof(mc_machine_descriptor_t)
    );
    if (!new_entries) {
        return 0;
    }
    g_descriptor_registry = new_entries;
    g_descriptor_registry_cap = new_cap;
    return 1;
}

mc_machine_descriptor_t *mc_descriptor_lookup(uint64_t descriptor_id) {
    for (uint32_t i = 0; i < g_descriptor_registry_len; i++) {
        if (g_descriptor_registry[i].descriptor_id == descriptor_id) {
            return &g_descriptor_registry[i];
        }
    }
    return NULL;
}

static uint8_t mc_desc_take(mc_desc_cursor_t *c, uint8_t *out, uint64_t n) {
    if (!c || !out || n > c->len - c->off) {
        return 0;
    }
    memcpy(out, c->bytes + c->off, (size_t)n);
    c->off += n;
    return 1;
}

static uint8_t mc_desc_read_u8(mc_desc_cursor_t *c, uint8_t *out) {
    return mc_desc_take(c, out, 1);
}

static uint8_t mc_desc_read_u32(mc_desc_cursor_t *c, uint32_t *out) {
    uint8_t tmp[4];
    if (!mc_desc_take(c, tmp, 4)) {
        return 0;
    }
    *out = (uint32_t)tmp[0] | ((uint32_t)tmp[1] << 8) | ((uint32_t)tmp[2] << 16)
        | ((uint32_t)tmp[3] << 24);
    return 1;
}

static uint8_t mc_desc_read_u64(mc_desc_cursor_t *c, uint64_t *out) {
    uint8_t tmp[8];
    if (!mc_desc_take(c, tmp, 8)) {
        return 0;
    }
    *out = (uint64_t)tmp[0] | ((uint64_t)tmp[1] << 8) | ((uint64_t)tmp[2] << 16)
        | ((uint64_t)tmp[3] << 24) | ((uint64_t)tmp[4] << 32) | ((uint64_t)tmp[5] << 40)
        | ((uint64_t)tmp[6] << 48) | ((uint64_t)tmp[7] << 56);
    return 1;
}

static uint8_t mc_desc_skip_string(mc_desc_cursor_t *c) {
    uint32_t n = 0;
    if (!mc_desc_read_u32(c, &n)) {
        return 0;
    }
    if ((uint64_t)n > c->len - c->off) {
        return 0;
    }
    c->off += (uint64_t)n;
    return 1;
}

static uint8_t mc_desc_read_string_alloc(mc_desc_cursor_t *c, char **out_str) {
    uint32_t n = 0;
    if (!mc_desc_read_u32(c, &n)) {
        return 0;
    }
    if ((uint64_t)n > c->len - c->off) {
        return 0;
    }
    char *buf = (char *)__mc_alloc((size_t)n + 1, 1);
    if (!buf) {
        return 0;
    }
    if (n > 0) {
        memcpy(buf, c->bytes + c->off, n);
    }
    buf[n] = '\0';
    c->off += (uint64_t)n;
    *out_str = buf;
    return 1;
}

const mc_thunk_registry_entry_t *mc_lookup_thunk_entry(uint64_t thunk_id) {
    int32_t idx = mc_thunk_registry_find(thunk_id);
    if (idx < 0) {
        return NULL;
    }
    return &g_thunk_registry[(uint32_t)idx];
}

const mc_machine_dispatch_row_t *mc_descriptor_find_row(
    const mc_machine_descriptor_t *descriptor,
    uint64_t state_tag,
    mc_machine_event_kind_t event_kind,
    uint64_t request_site_key
) {
    if (!descriptor) {
        return NULL;
    }
    // Dispatch rows can include site-specific response matches as well as a
    // wildcard row (`request_site_key == 0`). Prefer exact site match first.
    const mc_machine_dispatch_row_t *wildcard = NULL;
    for (uint32_t i = 0; i < descriptor->rows_len; i++) {
        const mc_machine_dispatch_row_t *row = &descriptor->rows[i];
        if (row->state_tag != state_tag || row->event_kind != event_kind) {
            continue;
        }
        if (row->request_site_key == request_site_key && request_site_key != 0) {
            // Exact site-specific match wins.
            return row;
        }
        if (row->request_site_key == 0) {
            wildcard = row;
        }
    }
    return wildcard;
}

mc_dispatch_result_t mc_descriptor_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    mc_machine_runtime_t *rt = (mc_machine_runtime_t *)ctx;
    mc_machine_slot_t *slot = mc_get_slot(rt, machine_id);
    if (!slot || !slot->descriptor) {
        if (fault_code) {
            *fault_code = MC_FAULT_CODE_DESCRIPTOR_ROW_MISSING;
        }
        return MC_DISPATCH_FAULT;
    }

    const mc_machine_dispatch_row_t *row =
        mc_descriptor_find_row(
            slot->descriptor,
            slot->state_tag,
            env->kind,
            env->origin_request_site_key
        );
    if (!row) {
        if (fault_code) {
            *fault_code = MC_FAULT_CODE_DESCRIPTOR_ROW_MISSING;
        }
        return MC_DISPATCH_FAULT;
    }

    uint64_t thunk_id = row->state_local_thunk_id != 0
        ? row->state_local_thunk_id
        : row->typestate_fallback_thunk_id;
    const mc_thunk_registry_entry_t *entry = mc_lookup_thunk_entry(thunk_id);
    if (!entry || !entry->dispatch) {
        if (fault_code) {
            *fault_code = MC_FAULT_CODE_DESCRIPTOR_THUNK_MISSING;
        }
        return MC_DISPATCH_FAULT;
    }

    mc_dispatch_result_t result = entry->dispatch(
        slot->dispatch_ctx,
        machine_id,
        current_state,
        env,
        txn,
        fault_code
    );
    if (result == MC_DISPATCH_OK && txn && txn->has_next_state && txn->next_state_tag == 0
        && entry->next_state_tag != 0) {
        txn->next_state_tag = entry->next_state_tag;
    }
    return result;
}

// Public API: register thunk id -> dispatch function.
void __mc_machine_runtime_register_thunk(
    uint64_t thunk_id,
    mc_machine_dispatch_txn_fn dispatch
) {
    __mc_machine_runtime_register_thunk_meta(thunk_id, dispatch, 0);
}

void __mc_machine_runtime_register_thunk_meta(
    uint64_t thunk_id,
    mc_machine_dispatch_txn_fn dispatch,
    uint64_t next_state_tag
) {
    if (thunk_id == 0) {
        return;
    }
    int32_t idx = mc_thunk_registry_find(thunk_id);
    if (idx >= 0) {
        g_thunk_registry[(uint32_t)idx].dispatch = dispatch;
        g_thunk_registry[(uint32_t)idx].next_state_tag = next_state_tag;
        return;
    }
    if (!mc_thunk_registry_ensure_cap(g_thunk_registry_len + 1)) {
        return;
    }
    g_thunk_registry[g_thunk_registry_len].thunk_id = thunk_id;
    g_thunk_registry[g_thunk_registry_len].dispatch = dispatch;
    g_thunk_registry[g_thunk_registry_len].next_state_tag = next_state_tag;
    g_thunk_registry_len += 1;
}

mc_machine_dispatch_txn_fn __mc_machine_runtime_lookup_thunk(uint64_t thunk_id) {
    const mc_thunk_registry_entry_t *entry = mc_lookup_thunk_entry(thunk_id);
    return entry ? entry->dispatch : NULL;
}

uint8_t __mc_machine_runtime_bind_dispatch_thunk(
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id,
    uint64_t thunk_id,
    void *dispatch_ctx
) {
    mc_machine_dispatch_txn_fn dispatch = __mc_machine_runtime_lookup_thunk(thunk_id);
    mc_machine_slot_t *slot = mc_get_slot(rt, machine_id);
    if (!slot || !dispatch) {
        return 0;
    }
    slot->dispatch = dispatch;
    slot->dispatch_ctx = dispatch_ctx;
    return 1;
}

uint64_t __mc_machine_runtime_register_descriptor(
    const uint8_t *descriptor_bytes,
    uint64_t descriptor_len
) {
    if (!descriptor_bytes || descriptor_len < 8) {
        return 0;
    }

    mc_desc_cursor_t c = {
        .bytes = descriptor_bytes,
        .len = descriptor_len,
        .off = 0,
    };
    uint8_t magic[4] = {0};
    if (!mc_desc_take(&c, magic, 4)) {
        return 0;
    }
    if (magic[0] != 'M' || magic[1] != 'C' || magic[2] != 'H' || magic[3] != 'D') {
        return 0;
    }

    uint32_t schema = 0;
    if (!mc_desc_read_u32(&c, &schema) || schema != 1u) {
        return 0;
    }

    char *typestate_name = NULL;
    mc_machine_dispatch_row_t *rows = NULL;
    if (!mc_desc_read_string_alloc(&c, &typestate_name)) {
        return 0;
    }

    uint32_t state_count = 0;
    uint32_t event_count = 0;
    uint32_t row_count = 0;
    uint32_t role_count = 0;
    if (!mc_desc_read_u32(&c, &state_count) || !mc_desc_read_u32(&c, &event_count)
        || !mc_desc_read_u32(&c, &row_count) || !mc_desc_read_u32(&c, &role_count)) {
        __mc_free(typestate_name);
        return 0;
    }

    for (uint32_t i = 0; i < state_count; i++) {
        uint64_t ignored = 0;
        if (!mc_desc_read_u64(&c, &ignored) || !mc_desc_read_u64(&c, &ignored)
            || !mc_desc_read_u64(&c, &ignored) || !mc_desc_skip_string(&c)) {
            __mc_free(typestate_name);
            return 0;
        }
    }

    for (uint32_t i = 0; i < event_count; i++) {
        uint64_t ignored = 0;
        uint8_t key_kind = 0;
        if (!mc_desc_read_u64(&c, &ignored) || !mc_desc_read_u64(&c, &ignored)
            || !mc_desc_read_u8(&c, &key_kind)) {
            __mc_free(typestate_name);
            return 0;
        }
        if (key_kind == 0) {
            if (!mc_desc_skip_string(&c)) {
                __mc_free(typestate_name);
                return 0;
            }
        } else if (key_kind == 1) {
            if (!mc_desc_skip_string(&c) || !mc_desc_skip_string(&c)) {
                __mc_free(typestate_name);
                return 0;
            }
        } else {
            __mc_free(typestate_name);
            return 0;
        }
    }

    if (row_count > 0) {
        rows = (mc_machine_dispatch_row_t *)__mc_alloc(
            (size_t)row_count * sizeof(mc_machine_dispatch_row_t),
            _Alignof(mc_machine_dispatch_row_t)
        );
        if (!rows) {
            __mc_free(typestate_name);
            return 0;
        }
    }

    for (uint32_t i = 0; i < row_count; i++) {
        if (!mc_desc_read_u64(&c, &rows[i].state_tag)
            || !mc_desc_read_u64(&c, &rows[i].event_kind)
            || !mc_desc_read_u64(&c, &rows[i].request_site_key)
            || !mc_desc_read_u64(&c, &rows[i].state_local_thunk_id)
            || !mc_desc_read_u64(&c, &rows[i].typestate_fallback_thunk_id)) {
            __mc_free(rows);
            __mc_free(typestate_name);
            return 0;
        }
    }

    for (uint32_t i = 0; i < role_count; i++) {
        uint64_t ignored = 0;
        if (!mc_desc_skip_string(&c) || !mc_desc_read_u64(&c, &ignored)) {
            __mc_free(rows);
            __mc_free(typestate_name);
            return 0;
        }
    }

    if (c.off != c.len || !mc_descriptor_registry_ensure_cap(g_descriptor_registry_len + 1)) {
        __mc_free(rows);
        __mc_free(typestate_name);
        return 0;
    }

    uint64_t descriptor_id = g_next_descriptor_id++;
    mc_machine_descriptor_t *dst = &g_descriptor_registry[g_descriptor_registry_len];
    dst->descriptor_id = descriptor_id;
    dst->typestate_name = typestate_name;
    dst->rows = rows;
    dst->rows_len = row_count;
    g_descriptor_registry_len += 1;
    return descriptor_id;
}

uint8_t __mc_machine_runtime_bind_descriptor(
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id,
    uint64_t descriptor_id,
    uint64_t initial_state_tag
) {
    mc_machine_slot_t *slot = mc_get_slot(rt, machine_id);
    mc_machine_descriptor_t *descriptor = mc_descriptor_lookup(descriptor_id);
    if (!slot || !descriptor) {
        return 0;
    }
    slot->descriptor = descriptor;
    slot->state_tag = initial_state_tag;
    slot->dispatch = mc_descriptor_dispatch;
    slot->dispatch_ctx = rt;
    return 1;
}
