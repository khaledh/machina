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

// ---------------------------------------------------------------------------
// Generic effect-vector operations (ensure_cap / stage / merge).
//
// MC_DEFINE_EFFECT_VEC(name, type_t) generates three static helpers:
//   mc_emit_ensure_<name>_cap   — grow backing array to min_cap
//   mc_emit_stage_<name>        — append one effect
//   mc_emit_merge_<name>        — merge staged effects into txn
// ---------------------------------------------------------------------------
#define MC_DEFINE_EFFECT_VEC(name, type_t)                                     \
    static uint8_t mc_emit_ensure_##name##_cap(                                \
        mc_emit_staging_ctx_t *ctx, uint32_t min_cap                           \
    ) {                                                                        \
        if (ctx->name##_cap >= min_cap) {                                      \
            return 1;                                                          \
        }                                                                      \
        uint32_t new_cap = ctx->name##_cap == 0 ? 4u : ctx->name##_cap;       \
        while (new_cap < min_cap) {                                            \
            uint32_t grown = new_cap * 2u;                                     \
            if (grown < new_cap) {                                             \
                return 0;                                                      \
            }                                                                  \
            new_cap = grown;                                                   \
        }                                                                      \
        type_t *new_items = (type_t *)__mc_realloc(                            \
            ctx->name, (size_t)new_cap * sizeof(type_t), _Alignof(type_t)      \
        );                                                                     \
        if (!new_items) {                                                      \
            return 0;                                                          \
        }                                                                      \
        ctx->name = new_items;                                                 \
        ctx->name##_cap = new_cap;                                             \
        return 1;                                                              \
    }                                                                          \
                                                                               \
    static uint8_t mc_emit_stage_##name(                                       \
        mc_emit_staging_ctx_t *ctx, const type_t *effect                       \
    ) {                                                                        \
        if (!mc_emit_ensure_##name##_cap(ctx, ctx->name##_len + 1)) {          \
            return 0;                                                          \
        }                                                                      \
        ctx->name[ctx->name##_len] = *effect;                                  \
        ctx->name##_len += 1;                                                  \
        return 1;                                                              \
    }                                                                          \
                                                                               \
    static uint8_t mc_emit_merge_##name(                                       \
        mc_emit_staging_ctx_t *ctx, mc_machine_dispatch_txn_t *txn             \
    ) {                                                                        \
        if (ctx->name##_len == 0) {                                            \
            return 1;                                                          \
        }                                                                      \
        if (txn->name##_len == 0) {                                            \
            txn->name = ctx->name;                                             \
            txn->name##_len = ctx->name##_len;                                 \
            return 1;                                                          \
        }                                                                      \
        uint32_t merged_len = txn->name##_len + ctx->name##_len;               \
        if (merged_len < txn->name##_len) {                                    \
            return 0;                                                          \
        }                                                                      \
        type_t *merged = (type_t *)__mc_alloc(                                 \
            (size_t)merged_len * sizeof(type_t), _Alignof(type_t)              \
        );                                                                     \
        if (!merged) {                                                         \
            return 0;                                                          \
        }                                                                      \
        memcpy(merged, txn->name,                                              \
               (size_t)txn->name##_len * sizeof(type_t));                      \
        memcpy(merged + txn->name##_len, ctx->name,                            \
               (size_t)ctx->name##_len * sizeof(type_t));                      \
        ctx->merged_##name = merged;                                           \
        txn->name = merged;                                                    \
        txn->name##_len = merged_len;                                          \
        return 1;                                                              \
    }

MC_DEFINE_EFFECT_VEC(outbox, mc_machine_outbox_effect_t)
MC_DEFINE_EFFECT_VEC(requests, mc_machine_request_effect_t)
MC_DEFINE_EFFECT_VEC(replies, mc_machine_reply_effect_t)

#undef MC_DEFINE_EFFECT_VEC

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
    if (!mc_emit_stage_requests(ctx, &effect)) {
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
    return mc_emit_stage_replies(ctx, &effect);
}
