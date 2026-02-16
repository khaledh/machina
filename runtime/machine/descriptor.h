#ifndef MC_MACHINE_DESCRIPTOR_H
#define MC_MACHINE_DESCRIPTOR_H

#include "runtime.h"

#define MC_FAULT_CODE_DESCRIPTOR_ROW_MISSING 2u
#define MC_FAULT_CODE_DESCRIPTOR_THUNK_MISSING 3u

// Thunk registry entry (process-global).
typedef struct mc_thunk_registry_entry {
    uint64_t thunk_id;
    mc_machine_dispatch_txn_fn dispatch;
    uint64_t next_state_tag;
} mc_thunk_registry_entry_t;

// Parsed machine descriptor (process-global).
struct mc_machine_descriptor {
    uint64_t descriptor_id;
    char *typestate_name;
    mc_machine_dispatch_row_t *rows;
    uint32_t rows_len;
};

// Descriptor cursor for binary parsing.
typedef struct mc_desc_cursor {
    const uint8_t *bytes;
    uint64_t len;
    uint64_t off;
} mc_desc_cursor_t;

// Thunk registry helpers.
uint8_t mc_thunk_registry_ensure_cap(uint32_t min_cap);
int32_t mc_thunk_registry_find(uint64_t thunk_id);
const mc_thunk_registry_entry_t *mc_lookup_thunk_entry(uint64_t thunk_id);

// Descriptor registry helpers.
uint8_t mc_descriptor_registry_ensure_cap(uint32_t min_cap);
mc_machine_descriptor_t *mc_descriptor_lookup(uint64_t descriptor_id);

// Descriptor dispatch row lookup.
const mc_machine_dispatch_row_t *mc_descriptor_find_row(
    const mc_machine_descriptor_t *descriptor,
    uint64_t state_tag,
    mc_machine_event_kind_t event_kind,
    uint64_t request_site_key
);

// Descriptor-driven dispatch callback.
mc_dispatch_result_t mc_descriptor_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
);

#endif
