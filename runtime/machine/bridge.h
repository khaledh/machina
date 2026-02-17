#ifndef MC_MACHINE_BRIDGE_H
#define MC_MACHINE_BRIDGE_H

#include "runtime.h"

#include <stdint.h>

// Integer-handle bridge wrappers for Machina stdlib/runtime intrinsics.
//
// These APIs expose managed machine runtime operations through plain `u64`
// handles so generated code and std wrappers do not need C struct layout
// knowledge. Handle value `0` is always invalid.
//
// Optional bootstrap hook:
// - if binary defines `void __mc_machine_bootstrap(void)`, runtime invokes it
//   once on first `__mc_machine_runtime_new()` call.
uint64_t __mc_machine_runtime_new(void);
void __mc_machine_runtime_free(uint64_t runtime);

// Process-global managed runtime used by `@machines` entrypoints.
// - bootstrap: lazily creates and stores process runtime handle, returns it.
// - current: returns active managed runtime handle, or 0 when not bootstrapped.
// - shutdown: drops active managed runtime handle and clears global slot.
uint64_t __mc_machine_runtime_managed_bootstrap_u64(void);
uint64_t __mc_machine_runtime_managed_current_u64(void);
uint64_t __mc_machine_runtime_managed_shutdown_u64(void);

uint64_t __mc_machine_runtime_spawn_u64(uint64_t runtime, uint64_t mailbox_cap);
uint64_t __mc_machine_runtime_start_u64(uint64_t runtime, uint64_t machine_id);

// Seed current machine state token for managed typestate runtime dispatch.
uint64_t __mc_machine_runtime_set_state_u64(
    uint64_t runtime,
    uint64_t machine_id,
    uint64_t state_word
);

uint64_t __mc_machine_runtime_send_u64(
    uint64_t runtime,
    uint64_t dst,
    uint64_t kind,
    uint64_t payload0,
    uint64_t payload1
);
uint64_t __mc_machine_runtime_request_u64(
    uint64_t runtime,
    uint64_t src,
    uint64_t dst,
    uint64_t kind,
    uint64_t payload0,
    uint64_t payload1
);
uint64_t __mc_machine_runtime_reply_u64(
    uint64_t runtime,
    uint64_t src,
    uint64_t reply_cap_id,
    uint64_t kind,
    uint64_t payload0,
    uint64_t payload1
);

// Bind machine-local dispatch callback through integer bridge.
// `dispatch_fn` is a `mc_machine_dispatch_txn_fn` pointer encoded as `u64`.
// `dispatch_ctx` is an opaque pointer value passed to callback.
// Returns 1 on success, 0 on invalid handle/id.
uint64_t __mc_machine_runtime_bind_dispatch_u64(
    uint64_t runtime,
    uint64_t machine_id,
    uint64_t dispatch_fn,
    uint64_t dispatch_ctx
);

// Register thunk id -> dispatch function pointer in process-global registry.
void __mc_machine_runtime_register_thunk_u64(uint64_t thunk_id, uint64_t dispatch_fn);
void __mc_machine_runtime_register_payload_drop_u64(
    uint64_t layout_id,
    uint64_t drop_fn
);
void __mc_machine_runtime_register_thunk_meta_u64(
    uint64_t thunk_id,
    uint64_t dispatch_fn,
    uint64_t next_state_tag
);
uint64_t __mc_machine_runtime_bind_dispatch_thunk_u64(
    uint64_t runtime,
    uint64_t machine_id,
    uint64_t thunk_id,
    uint64_t dispatch_ctx
);

// Bridge wrappers for descriptor registration/binding.
uint64_t __mc_machine_runtime_register_descriptor_u64(
    uint64_t descriptor_ptr,
    uint64_t descriptor_len
);
uint64_t __mc_machine_runtime_bind_descriptor_u64(
    uint64_t runtime,
    uint64_t machine_id,
    uint64_t descriptor_id,
    uint64_t initial_state_tag
);

// Runs one dispatch step through the managed runtime bridge.
// Returns `mc_machine_step_status_t` value.
uint64_t __mc_machine_runtime_step_u64(uint64_t runtime);

#endif
