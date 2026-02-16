#ifndef MC_MACHINE_EMIT_H
#define MC_MACHINE_EMIT_H

#include "runtime.h"

// Active dispatch-scoped staging context used by `__mc_machine_emit_*`.
//
// These ABI shims do not carry runtime/txn pointers explicitly, so the runtime
// binds them to the currently executing dispatch callback through this
// thread-local-like singleton. v1 runtime execution is single-threaded.
typedef struct mc_emit_staging_ctx {
    struct mc_emit_staging_ctx *prev;
    mc_machine_runtime_t *rt;
    mc_machine_id_t machine_id;

    // Effects staged via __mc_machine_emit_send/request/reply.
    mc_machine_outbox_effect_t *outbox;
    uint32_t outbox_len;
    uint32_t outbox_cap;

    mc_machine_request_effect_t *requests;
    uint32_t requests_len;
    uint32_t requests_cap;

    mc_machine_reply_effect_t *replies;
    uint32_t replies_len;
    uint32_t replies_cap;

    // Optional merged buffers used when callback sets txn arrays directly and
    // also uses __mc_machine_emit_* shims in the same dispatch.
    mc_machine_outbox_effect_t *merged_outbox;
    mc_machine_request_effect_t *merged_requests;
    mc_machine_reply_effect_t *merged_replies;
} mc_emit_staging_ctx_t;

// Staging lifecycle.
void mc_emit_staging_begin(
    mc_emit_staging_ctx_t *ctx,
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
);
void mc_emit_staging_end(mc_emit_staging_ctx_t *ctx);

// Merge staged effects into transaction.
uint8_t mc_emit_merge_into_txn(
    mc_emit_staging_ctx_t *ctx,
    mc_machine_dispatch_txn_t *txn
);

#endif
