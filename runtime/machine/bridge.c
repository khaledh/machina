#include "internal.h"

#include <stdlib.h>

#if defined(__GNUC__) || defined(__clang__)
#define MC_WEAK __attribute__((weak))
#else
#define MC_WEAK
#endif

// Optional process-level bootstrap hook emitted by compiler artifacts.
// Runtime calls this once lazily on first `__mc_machine_runtime_new()`.
MC_WEAK void __mc_machine_bootstrap(void) {}

// Optional process-global managed runtime used by `@[machines]` entrypoint
// bootstrap. This keeps runtime ownership explicit at source level while
// removing runtime-handle plumbing from machine-centric app code.
static uint64_t g_managed_runtime_handle = 0;
static uint8_t g_managed_runtime_atexit_registered = 0;

static mc_machine_runtime_t *mc_runtime_from_handle(uint64_t runtime) {
    if (runtime == 0) {
        return NULL;
    }
    return (mc_machine_runtime_t *)(uintptr_t)runtime;
}

static void mc_machine_runtime_managed_atexit_cleanup(void) {
    if (g_managed_runtime_handle == 0) {
        return;
    }
    uint64_t runtime = g_managed_runtime_handle;
    g_managed_runtime_handle = 0;
    __mc_machine_runtime_free(runtime);
}

uint64_t __mc_machine_runtime_new(void) {
    static uint8_t bootstrap_done = 0;
    if (!bootstrap_done) {
        __mc_machine_bootstrap();
        bootstrap_done = 1;
    }

    mc_machine_runtime_t *rt = (mc_machine_runtime_t *)__mc_alloc(
        sizeof(mc_machine_runtime_t),
        _Alignof(mc_machine_runtime_t)
    );
    if (!rt) {
        return 0;
    }
    __mc_machine_runtime_init(rt);
    return (uint64_t)(uintptr_t)rt;
}

void __mc_machine_runtime_free(uint64_t runtime) {
    mc_machine_runtime_t *rt = mc_runtime_from_handle(runtime);
    if (!rt) {
        return;
    }
    __mc_machine_runtime_drop(rt);
    __mc_free(rt);
}

uint64_t __mc_machine_runtime_managed_bootstrap_u64(void) {
    if (g_managed_runtime_handle != 0) {
        return g_managed_runtime_handle;
    }
    uint64_t runtime = __mc_machine_runtime_new();
    if (runtime == 0) {
        return 0;
    }
    g_managed_runtime_handle = runtime;

    if (!g_managed_runtime_atexit_registered) {
        if (atexit(mc_machine_runtime_managed_atexit_cleanup) == 0) {
            g_managed_runtime_atexit_registered = 1;
        }
    }
    return g_managed_runtime_handle;
}

uint64_t __mc_machine_runtime_managed_current_u64(void) {
    return g_managed_runtime_handle;
}

uint64_t __mc_machine_runtime_managed_shutdown_u64(void) {
    if (g_managed_runtime_handle == 0) {
        return 0;
    }
    uint64_t runtime = g_managed_runtime_handle;
    g_managed_runtime_handle = 0;
    __mc_machine_runtime_free(runtime);
    return 1;
}

uint64_t __mc_machine_runtime_spawn_u64(uint64_t runtime, uint64_t mailbox_cap) {
    mc_machine_runtime_t *rt = mc_runtime_from_handle(runtime);
    if (!rt || mailbox_cap > UINT32_MAX) {
        return 0;
    }
    mc_machine_id_t id = 0;
    if (!__mc_machine_runtime_spawn(rt, (uint32_t)mailbox_cap, &id)) {
        return 0;
    }
    return (uint64_t)id;
}

uint64_t __mc_machine_runtime_start_u64(uint64_t runtime, uint64_t machine_id) {
    mc_machine_runtime_t *rt = mc_runtime_from_handle(runtime);
    if (!rt || machine_id > UINT32_MAX) {
        return 0;
    }
    return __mc_machine_runtime_start(rt, (mc_machine_id_t)machine_id) ? 1 : 0;
}

uint64_t __mc_machine_runtime_send_u64(
    uint64_t runtime,
    uint64_t dst,
    uint64_t kind,
    uint64_t payload0,
    uint64_t payload1
) {
    mc_machine_runtime_t *rt = mc_runtime_from_handle(runtime);
    if (!rt || dst > UINT32_MAX) {
        return (uint64_t)MC_MAILBOX_ENQUEUE_MACHINE_UNKNOWN;
    }
    mc_machine_envelope_t env = {
        .kind = kind,
        .src = 0,
        .reply_cap_id = 0,
        .pending_id = 0,
        .payload0 = payload0,
        .payload1 = payload1,
    };
    return (uint64_t)__mc_machine_runtime_enqueue(rt, (mc_machine_id_t)dst, &env);
}

uint64_t __mc_machine_runtime_request_u64(
    uint64_t runtime,
    uint64_t src,
    uint64_t dst,
    uint64_t kind,
    uint64_t payload0,
    uint64_t payload1
) {
    mc_machine_runtime_t *rt = mc_runtime_from_handle(runtime);
    if (!rt || src > UINT32_MAX || dst > UINT32_MAX) {
        return 0;
    }
    mc_machine_envelope_t env = {
        .kind = kind,
        .src = 0,
        .reply_cap_id = 0,
        .pending_id = 0,
        .payload0 = payload0,
        .payload1 = payload1,
    };
    uint64_t pending_id = 0;
    if (__mc_machine_runtime_request(
            rt,
            (mc_machine_id_t)src,
            (mc_machine_id_t)dst,
            &env,
            &pending_id
        ) != MC_MAILBOX_ENQUEUE_OK) {
        return 0;
    }
    return pending_id;
}

uint64_t __mc_machine_runtime_reply_u64(
    uint64_t runtime,
    uint64_t src,
    uint64_t reply_cap_id,
    uint64_t kind,
    uint64_t payload0,
    uint64_t payload1
) {
    mc_machine_runtime_t *rt = mc_runtime_from_handle(runtime);
    if (!rt || src > UINT32_MAX) {
        return (uint64_t)MC_REPLY_CAP_UNKNOWN;
    }
    mc_machine_envelope_t env = {
        .kind = kind,
        .src = 0,
        .reply_cap_id = 0,
        .pending_id = 0,
        .payload0 = payload0,
        .payload1 = payload1,
    };
    return (uint64_t)__mc_machine_runtime_reply(rt, (mc_machine_id_t)src, reply_cap_id, &env);
}

uint64_t __mc_machine_runtime_bind_dispatch_u64(
    uint64_t runtime,
    uint64_t machine_id,
    uint64_t dispatch_fn,
    uint64_t dispatch_ctx
) {
    mc_machine_runtime_t *rt = mc_runtime_from_handle(runtime);
    if (!rt || machine_id == 0 || machine_id > UINT32_MAX) {
        return 0;
    }
    mc_machine_slot_t *slot = mc_get_slot(rt, (mc_machine_id_t)machine_id);
    if (!slot) {
        return 0;
    }

    mc_machine_dispatch_txn_fn typed_dispatch =
        (mc_machine_dispatch_txn_fn)(uintptr_t)dispatch_fn;
    __mc_machine_runtime_bind_dispatch(
        rt,
        (mc_machine_id_t)machine_id,
        typed_dispatch,
        (void *)(uintptr_t)dispatch_ctx
    );
    return 1;
}

void __mc_machine_runtime_register_thunk_u64(uint64_t thunk_id, uint64_t dispatch_fn) {
    __mc_machine_runtime_register_thunk_meta(
        thunk_id,
        (mc_machine_dispatch_txn_fn)(uintptr_t)dispatch_fn,
        0
    );
}

void __mc_machine_runtime_register_thunk_meta_u64(
    uint64_t thunk_id,
    uint64_t dispatch_fn,
    uint64_t next_state_tag
) {
    __mc_machine_runtime_register_thunk_meta(
        thunk_id,
        (mc_machine_dispatch_txn_fn)(uintptr_t)dispatch_fn,
        next_state_tag
    );
}

uint64_t __mc_machine_runtime_register_descriptor_u64(
    uint64_t descriptor_ptr,
    uint64_t descriptor_len
) {
    if (descriptor_ptr == 0 || descriptor_len == 0) {
        return 0;
    }
    return __mc_machine_runtime_register_descriptor(
        (const uint8_t *)(uintptr_t)descriptor_ptr,
        descriptor_len
    );
}

uint64_t __mc_machine_runtime_bind_dispatch_thunk_u64(
    uint64_t runtime,
    uint64_t machine_id,
    uint64_t thunk_id,
    uint64_t dispatch_ctx
) {
    mc_machine_runtime_t *rt = mc_runtime_from_handle(runtime);
    if (!rt || machine_id == 0 || machine_id > UINT32_MAX) {
        return 0;
    }
    return __mc_machine_runtime_bind_dispatch_thunk(
               rt,
               (mc_machine_id_t)machine_id,
               thunk_id,
               (void *)(uintptr_t)dispatch_ctx
           )
        ? 1
        : 0;
}

uint64_t __mc_machine_runtime_bind_descriptor_u64(
    uint64_t runtime,
    uint64_t machine_id,
    uint64_t descriptor_id,
    uint64_t initial_state_tag
) {
    mc_machine_runtime_t *rt = mc_runtime_from_handle(runtime);
    if (!rt || machine_id == 0 || machine_id > UINT32_MAX) {
        return 0;
    }
    return __mc_machine_runtime_bind_descriptor(
               rt,
               (mc_machine_id_t)machine_id,
               descriptor_id,
               initial_state_tag
           )
        ? 1
        : 0;
}

uint64_t __mc_machine_runtime_step_u64(uint64_t runtime) {
    mc_machine_runtime_t *rt = mc_runtime_from_handle(runtime);
    if (!rt) {
        return (uint64_t)MC_STEP_IDLE;
    }

    uint8_t faulted = 0;
    mc_machine_id_t machine_id = 0;
    uint64_t fault_code = 0;
    uint8_t did_work = mc_machine_runtime_dispatch_one_txn_impl(
        rt,
        NULL,
        NULL,
        &faulted,
        &machine_id,
        &fault_code
    );
    if (!did_work) {
        return (uint64_t)MC_STEP_IDLE;
    }
    if (faulted) {
        (void)machine_id;
        (void)fault_code;
        return (uint64_t)MC_STEP_FAULTED;
    }
    return (uint64_t)MC_STEP_DID_WORK;
}
