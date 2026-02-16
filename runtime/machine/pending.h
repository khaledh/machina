#ifndef MC_MACHINE_PENDING_H
#define MC_MACHINE_PENDING_H

#include "runtime.h"

// Pending reply-cap table helpers.

uint8_t mc_pending_ensure_cap(mc_pending_reply_table_t *pending, uint32_t min_cap);

int32_t mc_pending_find_active(
    const mc_pending_reply_table_t *pending,
    uint64_t pending_id
);
int32_t mc_pending_find_active_identity(
    const mc_pending_reply_table_t *pending,
    mc_pending_correlation_id_t correlation
);

uint8_t mc_pending_contains_active(
    const mc_pending_reply_table_t *pending,
    uint64_t pending_id
);
uint8_t mc_pending_contains_identity(
    const mc_pending_reply_table_t *pending,
    mc_pending_correlation_id_t correlation
);

uint8_t mc_pending_insert_active(
    mc_pending_reply_table_t *pending,
    mc_pending_correlation_id_t correlation,
    uint64_t request_payload0,
    mc_payload_layout_id_t request_payload1,
    mc_machine_id_t requester,
    uint64_t created_tick
);

uint8_t mc_pending_next_cap_id(mc_pending_reply_table_t *pending, uint64_t *out_cap_id);

uint32_t mc_pending_active_len(const mc_pending_reply_table_t *pending);
uint32_t mc_pending_inactive_len(const mc_pending_reply_table_t *pending);

uint8_t mc_pending_cleanup_reason_valid(mc_pending_cleanup_reason_t reason);
void mc_emit_pending_cleanup(
    mc_machine_runtime_t *rt,
    const mc_pending_reply_entry_t *entry,
    mc_pending_cleanup_reason_t reason
);

uint32_t mc_pending_cleanup_requester(
    mc_machine_runtime_t *rt,
    mc_machine_id_t requester,
    mc_pending_cleanup_reason_t reason
);
uint32_t mc_pending_cleanup_timeouts(mc_machine_runtime_t *rt);

#endif
