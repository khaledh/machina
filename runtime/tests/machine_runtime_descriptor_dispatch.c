#include <stdint.h>
#include <stddef.h>

#include "machine_runtime.h"

typedef struct byte_buf {
    uint8_t bytes[512];
    uint64_t len;
} byte_buf_t;

static uint64_t g_local_calls = 0;
static uint64_t g_fallback_calls = 0;

static void buf_u8(byte_buf_t *b, uint8_t v) {
    b->bytes[b->len++] = v;
}

static void buf_u32(byte_buf_t *b, uint32_t v) {
    buf_u8(b, (uint8_t)(v & 0xff));
    buf_u8(b, (uint8_t)((v >> 8) & 0xff));
    buf_u8(b, (uint8_t)((v >> 16) & 0xff));
    buf_u8(b, (uint8_t)((v >> 24) & 0xff));
}

static void buf_u64(byte_buf_t *b, uint64_t v) {
    buf_u8(b, (uint8_t)(v & 0xff));
    buf_u8(b, (uint8_t)((v >> 8) & 0xff));
    buf_u8(b, (uint8_t)((v >> 16) & 0xff));
    buf_u8(b, (uint8_t)((v >> 24) & 0xff));
    buf_u8(b, (uint8_t)((v >> 32) & 0xff));
    buf_u8(b, (uint8_t)((v >> 40) & 0xff));
    buf_u8(b, (uint8_t)((v >> 48) & 0xff));
    buf_u8(b, (uint8_t)((v >> 56) & 0xff));
}

static void buf_str(byte_buf_t *b, const char *s) {
    uint32_t n = 0;
    while (s[n] != '\0') {
        n += 1;
    }
    buf_u32(b, n);
    for (uint32_t i = 0; i < n; i++) {
        buf_u8(b, (uint8_t)s[i]);
    }
}

static mc_dispatch_result_t local_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    (void)ctx;
    (void)machine_id;
    (void)current_state;
    (void)env;
    (void)txn;
    (void)fault_code;
    g_local_calls += 1;
    return MC_DISPATCH_OK;
}

static mc_dispatch_result_t fallback_dispatch(
    void *ctx,
    mc_machine_id_t machine_id,
    uint64_t current_state,
    const mc_machine_envelope_t *env,
    mc_machine_dispatch_txn_t *txn,
    uint64_t *fault_code
) {
    (void)ctx;
    (void)machine_id;
    (void)current_state;
    (void)env;
    (void)txn;
    (void)fault_code;
    g_fallback_calls += 1;
    return MC_DISPATCH_OK;
}

static void build_test_descriptor(byte_buf_t *b) {
    // Magic + schema.
    buf_u8(b, 'M');
    buf_u8(b, 'C');
    buf_u8(b, 'H');
    buf_u8(b, 'D');
    buf_u32(b, 1);

    // typestate name
    buf_str(b, "M");

    // counts: states=1, events=2, rows=2, roles=0.
    buf_u32(b, 1);
    buf_u32(b, 2);
    buf_u32(b, 2);
    buf_u32(b, 0);

    // state tag row.
    buf_u64(b, 1);   // tag
    buf_u64(b, 0);   // state_type_def_id
    buf_u64(b, 0);   // state_layout_ty
    buf_str(b, "S");

    // event kind 10 payload key.
    buf_u64(b, 10);  // kind
    buf_u64(b, 0);   // payload_layout_ty
    buf_u8(b, 0);    // payload key
    buf_str(b, "Ping");

    // event kind 20 payload key.
    buf_u64(b, 20);
    buf_u64(b, 0);
    buf_u8(b, 0);
    buf_str(b, "Pong");

    // dispatch row: state 1 + kind 10 => local thunk 101.
    buf_u64(b, 1);
    buf_u64(b, 10);
    buf_u64(b, 101);
    buf_u64(b, 202);

    // dispatch row: state 1 + kind 20 => fallback thunk 202.
    buf_u64(b, 1);
    buf_u64(b, 20);
    buf_u64(b, 0);
    buf_u64(b, 202);
}

int main(void) {
    byte_buf_t desc = {0};
    build_test_descriptor(&desc);

    uint64_t rt = __mc_machine_runtime_new();
    if (rt == 0) {
        return 1;
    }

    __mc_machine_runtime_register_thunk_u64(101, (uint64_t)(uintptr_t)local_dispatch);
    __mc_machine_runtime_register_thunk_u64(202, (uint64_t)(uintptr_t)fallback_dispatch);

    uint64_t desc_id = __mc_machine_runtime_register_descriptor_u64(
        (uint64_t)(uintptr_t)desc.bytes,
        desc.len
    );
    if (desc_id == 0) {
        return 1;
    }

    uint64_t m = __mc_machine_runtime_spawn_u64(rt, 4);
    if (m == 0) {
        return 1;
    }
    if (__mc_machine_runtime_bind_descriptor_u64(rt, m, desc_id, 1) == 0) {
        return 1;
    }
    if (__mc_machine_runtime_start_u64(rt, m) == 0) {
        return 1;
    }

    if (__mc_machine_runtime_send_u64(rt, m, 10, 0, 0) != MC_MAILBOX_ENQUEUE_OK) {
        return 1;
    }
    if (__mc_machine_runtime_step_u64(rt) != MC_STEP_DID_WORK) {
        return 1;
    }
    if (g_local_calls != 1 || g_fallback_calls != 0) {
        return 1;
    }

    if (__mc_machine_runtime_send_u64(rt, m, 20, 0, 0) != MC_MAILBOX_ENQUEUE_OK) {
        return 1;
    }
    if (__mc_machine_runtime_step_u64(rt) != MC_STEP_DID_WORK) {
        return 1;
    }
    if (g_local_calls != 1 || g_fallback_calls != 1) {
        return 1;
    }

    // Kind 30 has no row in descriptor: machine should fault.
    if (__mc_machine_runtime_send_u64(rt, m, 30, 0, 0) != MC_MAILBOX_ENQUEUE_OK) {
        return 1;
    }
    if (__mc_machine_runtime_step_u64(rt) != MC_STEP_FAULTED) {
        return 1;
    }

    __mc_machine_runtime_free(rt);
    return 0;
}
