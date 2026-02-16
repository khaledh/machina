#include "emit.h"
#include "internal.h"
#include "pending.h"

#include <string.h>

// Single active context pointer (acts like a tiny dispatch-local stack).
static mc_emit_staging_ctx_t *g_emit_staging_ctx = NULL;

void mc_emit_staging_begin(
    mc_emit_staging_ctx_t *ctx,
    mc_machine_runtime_t *rt,
    mc_machine_id_t machine_id
) {
    memset(ctx, 0, sizeof(*ctx));
    ctx->prev = g_emit_staging_ctx;
    ctx->rt = rt;
    ctx->machine_id = machine_id;
    g_emit_staging_ctx = ctx;
}

void mc_emit_staging_end(mc_emit_staging_ctx_t *ctx) {
    __mc_free(ctx->outbox);
    __mc_free(ctx->requests);
    __mc_free(ctx->replies);
    __mc_free(ctx->merged_outbox);
    __mc_free(ctx->merged_requests);
    __mc_free(ctx->merged_replies);
    g_emit_staging_ctx = ctx->prev;
}

static uint8_t mc_emit_ensure_outbox_cap(mc_emit_staging_ctx_t *ctx, uint32_t min_cap) {
    if (ctx->outbox_cap >= min_cap) {
        return 1;
    }
    uint32_t new_cap = ctx->outbox_cap == 0 ? 4u : ctx->outbox_cap;
    while (new_cap < min_cap) {
        uint32_t grown = new_cap * 2u;
        if (grown < new_cap) {
            return 0;
        }
        new_cap = grown;
    }
    mc_machine_outbox_effect_t *new_items = (mc_machine_outbox_effect_t *)__mc_realloc(
        ctx->outbox,
        (size_t)new_cap * sizeof(mc_machine_outbox_effect_t),
        _Alignof(mc_machine_outbox_effect_t)
    );
    if (!new_items) {
        return 0;
    }
    ctx->outbox = new_items;
    ctx->outbox_cap = new_cap;
    return 1;
}

static uint8_t mc_emit_ensure_requests_cap(mc_emit_staging_ctx_t *ctx, uint32_t min_cap) {
    if (ctx->requests_cap >= min_cap) {
        return 1;
    }
    uint32_t new_cap = ctx->requests_cap == 0 ? 4u : ctx->requests_cap;
    while (new_cap < min_cap) {
        uint32_t grown = new_cap * 2u;
        if (grown < new_cap) {
            return 0;
        }
        new_cap = grown;
    }
    mc_machine_request_effect_t *new_items = (mc_machine_request_effect_t *)__mc_realloc(
        ctx->requests,
        (size_t)new_cap * sizeof(mc_machine_request_effect_t),
        _Alignof(mc_machine_request_effect_t)
    );
    if (!new_items) {
        return 0;
    }
    ctx->requests = new_items;
    ctx->requests_cap = new_cap;
    return 1;
}

static uint8_t mc_emit_ensure_replies_cap(mc_emit_staging_ctx_t *ctx, uint32_t min_cap) {
    if (ctx->replies_cap >= min_cap) {
        return 1;
    }
    uint32_t new_cap = ctx->replies_cap == 0 ? 4u : ctx->replies_cap;
    while (new_cap < min_cap) {
        uint32_t grown = new_cap * 2u;
        if (grown < new_cap) {
            return 0;
        }
        new_cap = grown;
    }
    mc_machine_reply_effect_t *new_items = (mc_machine_reply_effect_t *)__mc_realloc(
        ctx->replies,
        (size_t)new_cap * sizeof(mc_machine_reply_effect_t),
        _Alignof(mc_machine_reply_effect_t)
    );
    if (!new_items) {
        return 0;
    }
    ctx->replies = new_items;
    ctx->replies_cap = new_cap;
    return 1;
}

static uint8_t mc_emit_stage_outbox(
    mc_emit_staging_ctx_t *ctx,
    const mc_machine_outbox_effect_t *effect
) {
    if (!mc_emit_ensure_outbox_cap(ctx, ctx->outbox_len + 1)) {
        return 0;
    }
    ctx->outbox[ctx->outbox_len] = *effect;
    ctx->outbox_len += 1;
    return 1;
}

static uint8_t mc_emit_stage_request(
    mc_emit_staging_ctx_t *ctx,
    const mc_machine_request_effect_t *effect
) {
    if (!mc_emit_ensure_requests_cap(ctx, ctx->requests_len + 1)) {
        return 0;
    }
    ctx->requests[ctx->requests_len] = *effect;
    ctx->requests_len += 1;
    return 1;
}

static uint8_t mc_emit_stage_reply(
    mc_emit_staging_ctx_t *ctx,
    const mc_machine_reply_effect_t *effect
) {
    if (!mc_emit_ensure_replies_cap(ctx, ctx->replies_len + 1)) {
        return 0;
    }
    ctx->replies[ctx->replies_len] = *effect;
    ctx->replies_len += 1;
    return 1;
}

static uint8_t mc_emit_merge_outbox(
    mc_emit_staging_ctx_t *ctx,
    mc_machine_dispatch_txn_t *txn
) {
    if (ctx->outbox_len == 0) {
        return 1;
    }
    if (txn->outbox_len == 0) {
        txn->outbox = ctx->outbox;
        txn->outbox_len = ctx->outbox_len;
        return 1;
    }

    uint32_t merged_len = txn->outbox_len + ctx->outbox_len;
    if (merged_len < txn->outbox_len) {
        return 0;
    }
    mc_machine_outbox_effect_t *merged = (mc_machine_outbox_effect_t *)__mc_alloc(
        (size_t)merged_len * sizeof(mc_machine_outbox_effect_t),
        _Alignof(mc_machine_outbox_effect_t)
    );
    if (!merged) {
        return 0;
    }

    memcpy(merged, txn->outbox, (size_t)txn->outbox_len * sizeof(mc_machine_outbox_effect_t));
    memcpy(
        merged + txn->outbox_len,
        ctx->outbox,
        (size_t)ctx->outbox_len * sizeof(mc_machine_outbox_effect_t)
    );
    ctx->merged_outbox = merged;
    txn->outbox = merged;
    txn->outbox_len = merged_len;
    return 1;
}

static uint8_t mc_emit_merge_requests(
    mc_emit_staging_ctx_t *ctx,
    mc_machine_dispatch_txn_t *txn
) {
    if (ctx->requests_len == 0) {
        return 1;
    }
    if (txn->requests_len == 0) {
        txn->requests = ctx->requests;
        txn->requests_len = ctx->requests_len;
        return 1;
    }

    uint32_t merged_len = txn->requests_len + ctx->requests_len;
    if (merged_len < txn->requests_len) {
        return 0;
    }
    mc_machine_request_effect_t *merged = (mc_machine_request_effect_t *)__mc_alloc(
        (size_t)merged_len * sizeof(mc_machine_request_effect_t),
        _Alignof(mc_machine_request_effect_t)
    );
    if (!merged) {
        return 0;
    }

    memcpy(merged, txn->requests, (size_t)txn->requests_len * sizeof(mc_machine_request_effect_t));
    memcpy(
        merged + txn->requests_len,
        ctx->requests,
        (size_t)ctx->requests_len * sizeof(mc_machine_request_effect_t)
    );
    ctx->merged_requests = merged;
    txn->requests = merged;
    txn->requests_len = merged_len;
    return 1;
}

static uint8_t mc_emit_merge_replies(
    mc_emit_staging_ctx_t *ctx,
    mc_machine_dispatch_txn_t *txn
) {
    if (ctx->replies_len == 0) {
        return 1;
    }
    if (txn->replies_len == 0) {
        txn->replies = ctx->replies;
        txn->replies_len = ctx->replies_len;
        return 1;
    }

    uint32_t merged_len = txn->replies_len + ctx->replies_len;
    if (merged_len < txn->replies_len) {
        return 0;
    }
    mc_machine_reply_effect_t *merged = (mc_machine_reply_effect_t *)__mc_alloc(
        (size_t)merged_len * sizeof(mc_machine_reply_effect_t),
        _Alignof(mc_machine_reply_effect_t)
    );
    if (!merged) {
        return 0;
    }

    memcpy(merged, txn->replies, (size_t)txn->replies_len * sizeof(mc_machine_reply_effect_t));
    memcpy(
        merged + txn->replies_len,
        ctx->replies,
        (size_t)ctx->replies_len * sizeof(mc_machine_reply_effect_t)
    );
    ctx->merged_replies = merged;
    txn->replies = merged;
    txn->replies_len = merged_len;
    return 1;
}

uint8_t mc_emit_merge_into_txn(
    mc_emit_staging_ctx_t *ctx,
    mc_machine_dispatch_txn_t *txn
) {
    return mc_emit_merge_outbox(ctx, txn) && mc_emit_merge_requests(ctx, txn)
        && mc_emit_merge_replies(ctx, txn);
}

// Managed typestate effect ABI shims.
//
// These shims stage effects into the active transactional dispatch context.
// They do not enqueue directly; commit still happens only through
// `__mc_machine_runtime_dispatch_one_txn`.
uint8_t __mc_machine_emit_send(
    uint64_t dst,
    uint64_t kind,
    uint64_t payload0,
    uint64_t payload1
) {
    mc_emit_staging_ctx_t *ctx = g_emit_staging_ctx;
    if (!ctx) {
        // Called outside managed dispatch.
        return 0;
    }

    mc_machine_outbox_effect_t effect = {
        .dst = (mc_machine_id_t)dst,
        .env = {
            .kind = kind,
            .src = ctx->machine_id,
            .reply_cap_id = 0,
            .pending_id = 0,
            .payload0 = payload0,
            .payload1 = payload1,
            .origin_payload0 = 0,
            .origin_payload1 = 0,
            .origin_request_site_key = 0,
        },
    };
    return mc_emit_stage_outbox(ctx, &effect);
}

uint64_t __mc_machine_emit_request(
    uint64_t dst,
    uint64_t kind,
    uint64_t payload0,
    uint64_t payload1,
    uint64_t request_site_key
) {
    mc_emit_staging_ctx_t *ctx = g_emit_staging_ctx;
    if (!ctx) {
        // Called outside managed dispatch.
        return 0;
    }

    uint64_t pending_id = 0;
    if (!mc_pending_next_cap_id(&ctx->rt->pending, &pending_id)) {
        return 0;
    }

    mc_machine_request_effect_t effect = {
        .dst = (mc_machine_id_t)dst,
        .pending_id = pending_id,
        .request_site_key = request_site_key,
        .env = {
            .kind = kind,
            .src = ctx->machine_id,
            .reply_cap_id = 0,
            .pending_id = 0,
            .payload0 = payload0,
            .payload1 = payload1,
            .origin_payload0 = 0,
            .origin_payload1 = 0,
            .origin_request_site_key = 0,
        },
    };
    if (!mc_emit_stage_request(ctx, &effect)) {
        return 0;
    }
    return pending_id;
}

uint8_t __mc_machine_emit_reply(
    uint64_t cap,
    uint64_t kind,
    uint64_t payload0,
    uint64_t payload1
) {
    mc_emit_staging_ctx_t *ctx = g_emit_staging_ctx;
    if (!ctx) {
        // Called outside managed dispatch.
        return 0;
    }

    mc_machine_reply_effect_t effect = {
        .reply_cap_id = cap,
        .env = {
            .kind = kind,
            .src = ctx->machine_id,
            .reply_cap_id = 0,
            .pending_id = 0,
            .payload0 = payload0,
            .payload1 = payload1,
            .origin_payload0 = 0,
            .origin_payload1 = 0,
            .origin_request_site_key = 0,
        },
    };
    return mc_emit_stage_reply(ctx, &effect);
}
