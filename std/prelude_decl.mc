// Runtime externs: implemented in runtime/*
@runtime
fn __rt_print(s: string, newline: u64);

@runtime
fn __rt_u64_to_dec(inout buf: u8[], value: u64) -> u64;

@runtime
fn __rt_memset(inout buf: u8[], value: u8);

@runtime
fn __rt_string_from_bytes(out dst: string, bytes: u8[]);

@intrinsic
fn type_of<T>(value: T) -> string;

// Managed machine runtime bridge helpers.
@runtime
fn __mc_machine_runtime_new() -> u64;

@runtime
fn __mc_machine_runtime_free(runtime: u64);

@runtime
fn __mc_machine_runtime_managed_bootstrap_u64() -> u64;

@runtime
fn __mc_machine_runtime_managed_current_u64() -> u64;

@runtime
fn __mc_machine_runtime_managed_shutdown_u64() -> u64;

@runtime
fn __mc_machine_runtime_spawn_u64(runtime: u64, mailbox_cap: u64) -> u64;

@runtime
fn __mc_machine_runtime_start_u64(runtime: u64, machine_id: u64) -> u64;

@runtime
fn __mc_machine_runtime_send_u64(
  runtime: u64,
  dst: u64,
  kind: u64,
  payload0: u64,
  payload1: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_request_u64(
  runtime: u64,
  src: u64,
  dst: u64,
  kind: u64,
  payload0: u64,
  payload1: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_reply_u64(
  runtime: u64,
  src: u64,
  reply_cap_id: u64,
  kind: u64,
  payload0: u64,
  payload1: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_bind_dispatch_u64(
  runtime: u64,
  machine_id: u64,
  dispatch_fn: u64,
  dispatch_ctx: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_register_thunk_u64(thunk_id: u64, dispatch_fn: u64);

@runtime
fn __mc_machine_runtime_bind_dispatch_thunk_u64(
  runtime: u64,
  machine_id: u64,
  thunk_id: u64,
  dispatch_ctx: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_bind_descriptor_u64(
  runtime: u64,
  machine_id: u64,
  descriptor_id: u64,
  initial_state_tag: u64,
) -> u64;

@runtime
fn __mc_machine_runtime_step_u64(runtime: u64) -> u64;

type KeyNotFound = {}

string :: {
  @intrinsic
  prop len: u64 {
    get;
  }

  @runtime
  fn append(inout self, other: string);

  @runtime
  fn append_bytes(inout self, bytes: u8[]);
}
